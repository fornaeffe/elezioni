/** Worker-owned refit task. No UI, filesystem, default-policy rewriting or global caches. */
import {
  loadCandidate,
  loadCandidateSource,
  generationKeys,
  type FitState,
  type ProfileState,
  type ReleasedScenario,
} from './release';
import { encodeState } from './state-codec';
import { fittedStateShapes, profileStateShapes } from './state-shapes';
import { createCheckpoint, checkpointReader } from './checkpoint';
import { requireFastScenario } from './scenario';
import { candidatePlacements } from '../candidate-preview';
import {
  compileSource,
  validateEditedPoll,
  type SourceSelection,
} from './source-compiler';
import { compilePairedHistory } from './paired-source';
import { sourceKey, canonicalJson, type AssetBytes } from './source-codec';
import { fitReferenceSteps } from './information';
import { fitProfilesSteps } from './profiles';
import { prepareSenateAdapter } from './senate-adapter';
import { prepareFastGeneration } from './generation';
import {
  cooperativeSteps,
  throwIfCancelled,
  type FitProgress,
} from './execution';
import { fitMemoryPlan, ownedBuffers } from './fit-resources';
import type {
  AcceptedFit,
  FitCommand,
  FitTaskResult,
  FitConfiguration,
} from './fit-types';

const sorted = <T>(rows: T[]) =>
  [...rows].sort((a, b) => canonicalJson(a).localeCompare(canonicalJson(b)));
export function draftSelection(
  s: ReleasedScenario,
  previous: AcceptedFit,
): SourceSelection {
  return {
    referenceDate: s.referenceDate,
    electionIds: [...s.historySelection.electionIds],
    pollIds: [...s.polls.selected],
    components: s.components
      .filter((c) => c.kind === 'fitted')
      .map((c) => c.id)
      .sort(),
    sourceMemberships: s.sourceMemberships,
    componentMap: s.componentMap,
    targetContext: previous.selection.targetContext,
    targets: previous.assets.electorates.map((t) => ({
      code: t.code,
      electors: t.camera,
    })),
  };
}
/** Fast conservative evidence fingerprint; exact compiled keys decide numerical reuse. */
export async function evidenceKey(
  s: ReleasedScenario,
  selection: SourceSelection,
) {
  return sourceKey('browser-fit-evidence-v1', {
    referenceDate: s.referenceDate,
    fitSeed: s.fitSeed,
    source: s.data.sourceBundleId,
    components: [...selection.components].sort(),
    history: [...selection.electionIds].sort(),
    polls: [...selection.pollIds].sort(),
    paired: {
      camera: s.historySelection.pairedCamera,
      senato: s.historySelection.pairedSenato,
    },
    targetContext: selection.targetContext,
    targets: sorted(selection.targets),
    memberships: Object.fromEntries(
      Object.entries(s.sourceMemberships)
        .sort()
        .map(([id, e]) => [id, [...e.members].sort()]),
    ),
    componentMap: s.componentMap,
    userPolls: sorted(
      s.polls.added
        .filter((p) => selection.pollIds.includes(p.id))
        .map((p) => ({
          id: p.id,
          surveyId: p.surveyId,
          context: p.context,
          date: p.date,
          fieldworkStart: p.fieldworkStart,
          fieldworkEnd: p.fieldworkEnd,
          availableDate: p.availableDate,
          effectiveSample: p.effectiveSample,
          sampleSize: p.sampleSize,
          questionSampleSize: p.questionSampleSize,
          decidedFraction: p.decidedFraction,
          designEffect: p.designEffect,
          effectiveSamplePolicy: p.effectiveSamplePolicy,
          errorGroup: p.errorGroup,
          roundingWidth: p.roundingWidth,
          status: p.status,
          otherDefinition: p.otherDefinition,
          normalized: sorted(
            p.normalized.map((a) => ({
              members: [...a.members].sort(),
              share: a.share,
              other: a.other,
            })),
          ),
        })),
    ),
  });
}
function requireConfiguration(c: FitConfiguration) {
  if (
    (c.base && !/^\/[a-zA-Z0-9_-]+(?:\/[a-zA-Z0-9_-]+)*$/.test(c.base)) ||
    Object.values(c.implementations).some((v) => typeof v !== 'string' || !v) ||
    !c.implementations.generation ||
    !c.implementations.fitting ||
    !c.implementations.profiles
  )
    throw new Error('INVALID_FIT_CONFIGURATION');
}
function acceptedProvenance(
  s: ReleasedScenario,
  nationalInputKey: string,
  profileInputKey: string,
  pairedInputKey: string,
) {
  return sourceKey('browser-fit-provenance-v1', {
    source: s.data.sourceBundleId,
    nationalInputKey,
    profileInputKey,
    pairedInputKey,
    memberships: s.sourceMemberships,
    polls: s.polls.added,
  });
}
export async function runFitTask(
  command: FitCommand,
  read: (path: string) => Promise<AssetBytes>,
  options: {
    signal?: AbortSignal;
    checkpoint?: (p: FitProgress) => Promise<void>;
  } = {},
): Promise<FitTaskResult> {
  const { configuration: c } = command;
  requireConfiguration(c);
  const checkpoint = async (p: FitProgress) => {
    throwIfCancelled(options.signal);
    await options.checkpoint?.(p);
    throwIfCancelled(options.signal);
  };
  const audit = {
    nationalFits: 0,
    profileFits: 0,
    pairedFits: 0,
    sourceLoaded: false,
    numericBytes: 0,
  };
  const finish = (accepted: AcceptedFit, plan: FitTaskResult['plan']) => {
    throwIfCancelled(options.signal);
    audit.numericBytes = ownedBuffers(accepted).reduce(
      (n, b) => n + b.byteLength,
      0,
    );
    return { accepted, plan, audit };
  };
  if (command.action === 'source') {
    const source = await loadCandidateSource(command.catalog, read);
    return {
      source,
      plan: {
        national: false,
        profiles: false,
        paired: false,
        reasons: [],
        estimatedAggregateBytes: 0,
        reused: { national: true, cameraProfile: true, senateAdapter: true },
      },
      audit: { ...audit, sourceLoaded: true },
    };
  }
  if (command.action === 'load' || command.action === 'restore') {
    await checkpoint({ phase: 'load-default', completed: 0, total: 1 });
    const loaded = await loadCandidate(
      command.action === 'restore' ? command.file.pointer : c.pointer,
      command.action === 'restore' ? checkpointReader(command.file) : read,
      {
        expectedGenerationImplementationId: c.implementations.generation,
        allowEditedEvidence: command.action === 'restore',
      },
    );
    requireFastScenario(loaded.scenario);
    // Check the independently supplied build identities, not just the catalog's claims.
    if (
      loaded.fitParents.implementationId !== c.implementations.fitting ||
      loaded.profileParents.implementationId !== c.implementations.profiles
    )
      throw new Error('FIT_IMPLEMENTATION_MISMATCH');
    const accepted: AcceptedFit = {
      portableStates: loaded.portableStates,
      sourceCatalog: loaded.catalog,
      scenario: loaded.scenario,
      assets: loaded.assets,
      context: loaded.context,
      selection: loaded.selection,
      implementations: c.implementations,
      nationalInputKey: loaded.catalog.nationalInputKey,
      profileInputKey: loaded.catalog.profileInputKey,
      pairedInputKey: loaded.catalog.pairedInputKey,
      electorateKey: loaded.catalog.electorateKey,
      keys: {
        compositionKey: loaded.catalog.compositionKey,
        runKey: loaded.catalog.runKey,
      },
      evidenceKey: await evidenceKey(loaded.scenario, loaded.selection),
      provenanceId: await acceptedProvenance(
        loaded.scenario,
        loaded.catalog.nationalInputKey,
        loaded.catalog.profileInputKey,
        loaded.catalog.pairedInputKey,
      ),
    };
    const memory = fitMemoryPlan(accepted, undefined, accepted.selection, c);
    return finish(accepted, {
      national: false,
      profiles: false,
      paired: false,
      reasons: [],
      estimatedAggregateBytes: memory.estimatedAggregateBytes,
      reused: { national: true, cameraProfile: true, senateAdapter: true },
    });
  }
  if (command.action === 'checkpoint') {
    const file = await createCheckpoint(command.previous, command.scenario);
    return {
      file,
      plan: {
        national: false,
        profiles: false,
        paired: false,
        reasons: [],
        estimatedAggregateBytes: 0,
        reused: { national: true, cameraProfile: true, senateAdapter: true },
      },
      audit,
    };
  }
  if (command.action === 'preview') {
    const s = requireFastScenario(command.scenario),
      prepared = prepareFastGeneration(
        s,
        command.previous.assets,
        command.previous.context,
      ),
      { input, trace } = prepared.run(1);
    return {
      preview: {
        placements: candidatePlacements(input, command.previewSelection),
        bonusSelection: trace.bonusSelection,
        warnings: trace.warnings,
        normalization: trace.normalization,
        references: trace.references,
      },
      plan: {
        national: false,
        profiles: false,
        paired: false,
        reasons: [],
        estimatedAggregateBytes: 0,
        reused: { national: true, cameraProfile: true, senateAdapter: true },
      },
      audit,
    };
  }
  const previous = command.previous,
    scenario = structuredClone(command.scenario),
    selection = draftSelection(scenario, previous);
  requireFastScenario(scenario);
  const portableStates = { ...previous.portableStates };
  scenario.polls.added.forEach(validateEditedPoll);
  if (
    canonicalJson(previous.implementations) !==
      canonicalJson(c.implementations) ||
    scenario.models.generationImplementationId !==
      c.implementations.generation ||
    scenario.data.sourceBundleId !== previous.assets.sourceBundleId ||
    scenario.data.geographyVersion !== previous.assets.geography.version ||
    scenario.data.seatTableVersion !== previous.context.seats.version ||
    scenario.data.preferenceModelVersion !==
      previous.context.models.preferences.parameterVersion ||
    scenario.data.fittedStateId !== previous.assets.fittedStateId ||
    scenario.data.profileStateId !== previous.assets.profileStateId
  )
    throw new Error('FIT_DRAFT_BINDING_MISMATCH');
  const key = await evidenceKey(scenario, selection),
    sameEvidence = key === previous.evidenceKey;
  let memory = fitMemoryPlan(previous, undefined, selection, c);
  const plan: FitTaskResult['plan'] = {
    national: false,
    profiles: false,
    paired: false,
    reasons: [],
    estimatedAggregateBytes: memory.estimatedAggregateBytes,
    reused: { national: true, cameraProfile: true, senateAdapter: true },
  };
  let fit = previous.assets.fit,
    cameraProfile = previous.assets.cameraProfile,
    senateAdapter = previous.assets.senateAdapter!,
    fittedStateId = previous.assets.fittedStateId,
    profileStateId = previous.assets.profileStateId,
    nationalInputKey = previous.nationalInputKey,
    profileInputKey = previous.profileInputKey,
    pairedInputKey = previous.pairedInputKey,
    provenanceId = previous.provenanceId;
  if (!sameEvidence) {
    await checkpoint({ phase: 'load-refit-source', completed: 0, total: 1 });
    const source = await loadCandidateSource(previous.sourceCatalog, read);
    audit.sourceLoaded = true;
    memory = fitMemoryPlan(previous, source, selection, c);
    plan.estimatedAggregateBytes = memory.estimatedAggregateBytes;
    const compiled = await compileSource(source, read, selection, {
      polls: scenario.polls.added,
      checkpoint,
    });
    await checkpoint({ phase: 'compile-pair', completed: 0, total: 1 });
    const paired = await compilePairedHistory(
      source,
      read,
      selection,
      {
        camera: scenario.historySelection.pairedCamera,
        senato: scenario.historySelection.pairedSenato,
        electoratePolicy: 'equal-only',
      },
      checkpoint,
    );
    const pairKey = await sourceKey('paired-input-v1', {
      date: paired.history.date,
      availableDate: paired.history.availableDate,
      minimumAge: paired.history.minimumAge,
      rows: paired.history.rows,
    });
    plan.national =
      compiled.nationalInputKey !== nationalInputKey ||
      scenario.fitSeed !== previous.assets.fitSeed;
    plan.profiles = compiled.profileInputKey !== profileInputKey;
    plan.paired = plan.profiles || pairKey !== pairedInputKey;
    plan.reused = {
      national: !plan.national,
      cameraProfile: !plan.profiles,
      senateAdapter: !plan.paired,
    };
    if (plan.national)
      plan.reasons.push(
        scenario.referenceDate !== previous.scenario.referenceDate
          ? 'reference-date'
          : 'national-evidence-or-fit-seed',
      );
    if (plan.profiles)
      plan.reasons.push('municipal-evidence-basis-or-reference-date');
    if (plan.paired) plan.reasons.push('paired-baseline-or-camera-profile');
    if (
      command.action === 'validate' &&
      (plan.national || plan.profiles || plan.paired)
    )
      return { plan, audit };
    const metadata = {
      sourceVersions: [source.id, ...source.sourceVersions],
      methodVersions: [
        'fast-share-browser-fit-v1',
        c.implementations.fitting,
        c.implementations.profiles,
      ],
      assumptions: [
        ...source.assumptions,
        'Interactive evidence edits are scenario-owned; no immutable source count is overwritten.',
      ],
    };
    if (plan.national) {
      fit = await cooperativeSteps(
        fitReferenceSteps(compiled.national, scenario.fitSeed, {
          solver: memory.solver,
          maximumWorkspaceBytes: memory.maximumWorkspaceBytes,
        }),
        checkpoint,
      );
      audit.nationalFits++;
      const state: FitState = {
        parents: {
          nationalInputKey: compiled.nationalInputKey,
          implementationId: c.implementations.fitting,
          fitSeed: scenario.fitSeed,
          solver: memory.solver,
        },
        fit,
        provenanceId: compiled.provenanceId,
        warnings: compiled.warnings,
        sampler: {
          chains: 4,
          burn: 1000,
          thin: 2,
          retention: 'all-accepted-joint-context-draws',
        },
      };
      fittedStateId = (
        await encodeState(
          'fast-share-fit',
          state,
          metadata,
          fittedStateShapes(fit),
        )
      ).envelope.id;
      portableStates.fit = { state, metadata };
    }
    if (plan.profiles) {
      cameraProfile = await cooperativeSteps(
        fitProfilesSteps(compiled.profiles, memory.maximumWorkspaceBytes),
        checkpoint,
      );
      audit.profileFits++;
    }
    if (plan.paired) {
      await checkpoint({ phase: 'senate-adapter', completed: 0, total: 1 });
      senateAdapter = prepareSenateAdapter(
        cameraProfile,
        scenario.referenceDate,
        paired.history,
      );
      audit.pairedFits++;
    }
    if (plan.profiles || plan.paired) {
      const state: ProfileState = {
        parents: {
          profileInputKey: compiled.profileInputKey,
          pairedInputKey: pairKey,
          implementationId: c.implementations.profiles,
          referenceDate: scenario.referenceDate,
        },
        cameraProfile,
        senateAdapter,
        pairedAudit: paired.audit,
      };
      profileStateId = (
        await encodeState(
          'fast-share-profiles',
          state,
          metadata,
          profileStateShapes(cameraProfile),
        )
      ).envelope.id;
      portableStates.profiles = { state, metadata };
    }
    nationalInputKey = compiled.nationalInputKey;
    profileInputKey = compiled.profileInputKey;
    pairedInputKey = pairKey;
    provenanceId = compiled.provenanceId;
  }
  await checkpoint({ phase: 'validate-generation', completed: 0, total: 1 });
  scenario.data = { ...scenario.data, fittedStateId, profileStateId };
  const assets = {
    ...previous.assets,
    fit,
    cameraProfile,
    senateAdapter,
    fitSeed: scenario.fitSeed,
    profileReferenceDate: scenario.referenceDate,
    fittedStateId,
    profileStateId,
  };
  prepareFastGeneration(scenario, assets, previous.context);
  const keys = await generationKeys(
    scenario,
    previous.context,
    previous.electorateKey,
  );
  provenanceId = await acceptedProvenance(
    scenario,
    nationalInputKey,
    profileInputKey,
    pairedInputKey,
  );
  return finish(
    {
      ...previous,
      portableStates,
      scenario,
      selection,
      assets,
      keys,
      nationalInputKey,
      profileInputKey,
      pairedInputKey,
      provenanceId,
      evidenceKey: key,
    },
    plan,
  );
}
