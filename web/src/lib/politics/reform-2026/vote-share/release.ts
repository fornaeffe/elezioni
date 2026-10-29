/** Candidate-release contract, usable by Node preparation and browser workers. */
import type {
  FastGenerationScenario,
  FastVoteAssets,
  ReferenceFit,
  GeographicProfile,
  SenateAdapter,
} from './forecast-types';
import type { SourceSelection } from './source-compiler';
import type { ReformAssemblyContext } from '../generation-types';
import type { CompactSource } from './source-types';
import type { SourcePoll } from './source-types';
import {
  sourceKey,
  canonicalJson,
  type ByteAsset,
  type AssetBytes,
} from './source-codec';
import { decodeState, type StateEnvelope } from './state-codec';
import { validateSourceEnvelope, decodeAsset } from './source-codec';
import { prepareFastGeneration } from './generation';
import { FAST_SHARE_MODEL, FAST_SHARE_NUMERICS } from './types';
export interface ReleasedScenario extends FastGenerationScenario {
  models: FastGenerationScenario['models'] & {
    generationImplementationId: string;
  };
  data: FastGenerationScenario['data'] & {
    seatTableVersion: string;
    preferenceModelVersion: string;
  };
  historySelection: {
    electionIds: string[];
    pairedCamera: string;
    pairedSenato: string;
  };
  sourceMemberships: NonNullable<SourceSelection['sourceMemberships']>;
  componentMap: NonNullable<SourceSelection['componentMap']>;
  polls: {
    selected: string[];
    added: SourcePoll[];
    excluded: Record<string, string>;
  };
  presentation: {
    name?: string;
    coalitions?: {
      id: string;
      name: string;
      color: string;
      chambers?: ('camera' | 'senato')[];
    }[];
    lists: {
      chamber: 'camera' | 'senato';
      id: string;
      name: string;
      color?: string;
    }[];
    components: { id: string; name: string; support: 'fitted' | 'reference' }[];
  };
}
export interface FitState {
  parents: {
    nationalInputKey: string;
    implementationId: string;
    fitSeed: string;
    solver: 'dense' | 'matrix-free';
  };
  fit: ReferenceFit;
  provenanceId: string;
  warnings: string[];
  sampler: {
    chains: 4;
    burn: 1000;
    thin: 2;
    retention: 'all-accepted-joint-context-draws';
  };
}
export interface ProfileState {
  parents: {
    profileInputKey: string;
    pairedInputKey: string;
    implementationId: string;
    referenceDate: string;
  };
  cameraProfile: GeographicProfile;
  senateAdapter: SenateAdapter;
  pairedAudit: unknown;
}
export interface RuntimeState {
  context: ReformAssemblyContext;
  geography: FastVoteAssets['geography'];
  electorate: {
    codes: string[];
    date: string;
    camera: Uint32Array;
    senato: Uint32Array;
    provenance: string;
  };
}
export interface ScenarioState {
  scenario: ReleasedScenario;
  selection: SourceSelection;
  defaultPolicy: unknown;
  keys: { compositionKey: string; runKey: string };
  electorateKey: string;
}
export interface ReleaseCatalog {
  generationImplementationId: string;
  status: 'candidate';
  policyId: string;
  referenceDate: string;
  electionDate: string;
  sourceId: string;
  fittedStateId: string;
  profileStateId: string;
  runtimeId: string;
  scenarioId: string;
  nationalInputKey: string;
  profileInputKey: string;
  pairedInputKey: string;
  compositionKey: string;
  runKey: string;
  electorateKey: string;
  assets: {
    source: ByteAsset;
    fit: ByteAsset;
    profiles: ByteAsset;
    runtime: ByteAsset;
    scenario: ByteAsset;
  };
  sourceChunks: ByteAsset[];
  byteBudget: {
    generationPayloadCompressedBytes: number;
    refitCompressedBytes: number;
  };
}
export interface CandidatePointer {
  schemaVersion: 1;
  kind: 'fast-share-candidate';
  catalogId: string;
  catalog: ByteAsset;
  assetRoot: 'data/reform-2026/fast-share/';
}
const sorted = <T>(xs: readonly T[]) =>
  [...xs].sort((a, b) => canonicalJson(a).localeCompare(canonicalJson(b)));
export async function generationKeys(
  s: FastGenerationScenario,
  context: ReformAssemblyContext,
  electorateKey: string,
) {
  const list = (ch: 'camera' | 'senato') =>
    s.lists[ch]
      .map((l) => ({
        id: l.id,
        componentIds: [...l.componentIds].sort(),
        nationalUncertaintyMultiplier: l.nationalUncertaintyMultiplier ?? 1,
        localUncertaintyMultiplier: l.localUncertaintyMultiplier ?? 1,
      }))
      .sort((a, b) => a.id.localeCompare(b.id));
  const compositionKey = await sourceKey('fast-share-composition-v1', {
    data: {
      sourceBundleId: s.data.sourceBundleId,
      fittedStateId: s.data.fittedStateId,
      profileStateId: s.data.profileStateId,
      geographyVersion: s.data.geographyVersion,
    },
    models: s.models,
    referenceDate: s.referenceDate,
    electionDate: s.electionDate,
    renewal: s.renewal,
    electorateKey,
    components: s.components
      .map(({ id, kind, fallback }) => ({
        id,
        kind,
        ...(fallback ? { fallback } : {}),
      }))
      .sort((a, b) => a.id.localeCompare(b.id)),
    lists: { camera: list('camera'), senato: list('senato') },
    coverage: sorted(
      s.coverage.map((c) => ({ ...c, listIds: [...c.listIds].sort() })),
    ),
    references: sorted(s.references),
    nonValidReferences: sorted(s.nonValidReferences),
    chamberProfile:
      context.models.chambers.profiles.find(
        (p) => p.id === s.chamberProfileId,
      ) ?? null,
  });
  const runKey = await sourceKey('fast-share-run-v1', {
    compositionKey,
    seed: s.seed,
    legalAlgorithm: s.algorithmId,
    seatTable: context.seats.version,
    preferenceVersion: context.models.preferences.parameterVersion,
    preferenceParameters: {
      baseline: context.models.preferences.baseline,
      pools: context.models.preferences.pools,
      profile:
        context.models.preferences.profiles.find(
          (p) => p.id === s.preferenceProfileId,
        ) ?? null,
    },
    candidateGeneration: s.candidateGeneration,
    candidateTemplates: sorted(s.candidateTemplates),
    suppliedVotes: sorted(s.suppliedVotes),
    suppliedPreferences: sorted(s.suppliedPreferences),
    bonusLinks: sorted(s.bonusLinks),
    legalLists: {
      camera: sorted(
        s.lists.camera.map(({ id, coalitionId, protectedMinority, kind }) => ({
          id,
          coalitionId,
          protectedMinority,
          ...(kind ? { kind } : {}),
        })),
      ),
      senato: sorted(
        s.lists.senato.map(({ id, coalitionId, protectedMinority, kind }) => ({
          id,
          coalitionId,
          protectedMinority,
          ...(kind ? { kind } : {}),
        })),
      ),
    },
  });
  return { compositionKey, runKey };
}
export async function loadCandidate(
  pointer: CandidatePointer,
  read: (path: string) => Promise<AssetBytes>,
  options: {
    validateGeneration?: boolean;
    expectedGenerationImplementationId?: string;
    allowEditedEvidence?: boolean;
  } = {},
) {
  if (
    pointer.schemaVersion !== 1 ||
    pointer.kind !== 'fast-share-candidate' ||
    pointer.assetRoot !== 'data/reform-2026/fast-share/'
  )
    throw new Error('CANDIDATE_POINTER');
  const catalog = await decodeState<ReleaseCatalog>(
    pointer.catalog,
    read,
    'fast-share-catalog',
  );
  if (
    catalog.envelope.id !== pointer.catalogId ||
    catalog.state.status !== 'candidate'
  )
    throw new Error('CANDIDATE_CATALOG');
  const c = catalog.state;
  if (
    options.expectedGenerationImplementationId !== undefined &&
    options.expectedGenerationImplementationId !== c.generationImplementationId
  )
    throw new Error('RELEASE_IMPLEMENTATION_MISMATCH');
  // Sequential decoding bounds simultaneous temporary inflation buffers.
  const f = await decodeState<FitState>(c.assets.fit, read, 'fast-share-fit'),
    p = await decodeState<ProfileState>(
      c.assets.profiles,
      read,
      'fast-share-profiles',
    ),
    r = await decodeState<RuntimeState>(
      c.assets.runtime,
      read,
      'fast-share-runtime',
    ),
    s = await decodeState<ScenarioState>(
      c.assets.scenario,
      read,
      'fast-share-scenario',
    );
  if (
    f.envelope.id !== c.fittedStateId ||
    p.envelope.id !== c.profileStateId ||
    r.envelope.id !== c.runtimeId ||
    s.envelope.id !== c.scenarioId ||
    f.state.parents.nationalInputKey !== c.nationalInputKey ||
    p.state.parents.profileInputKey !== c.profileInputKey ||
    p.state.parents.pairedInputKey !== c.pairedInputKey ||
    s.state.scenario.referenceDate !== c.referenceDate ||
    s.state.scenario.electionDate !== c.electionDate
  )
    throw new Error('RELEASE_STATE_MISMATCH');
  const shape = (envelope: StateEnvelope, name: string, expected: number[]) => {
    if (
      canonicalJson(
        envelope.payload.columns.find((c) => c.name === name)?.shape ?? null,
      ) !== canonicalJson(expected)
    )
      throw new Error('RELEASE_NUMERIC_SHAPE:' + name);
  };
  for (const name of ['political', 'participation'] as const) {
    const sample = f.state.fit[name];
    shape(f.envelope, '/fit/' + name + '/values', [
      sample.chains,
      sample.drawsPerChain,
      sample.dimensions,
    ]);
  }
  for (const [path, profile] of [
    ['/cameraProfile', p.state.cameraProfile],
    ['/senateAdapter/profile', p.state.senateAdapter.profile],
  ] as const)
    for (const field of ['means', 'profileSd', 'fallback'])
      shape(p.envelope, path + '/' + field, [
        profile.municipalityIds.length,
        profile.componentIds.length + 1,
      ]);
  if (
    f.state.sampler.chains !== 4 ||
    f.state.sampler.burn !== 1000 ||
    f.state.sampler.thin !== 2 ||
    f.state.sampler.retention !== 'all-accepted-joint-context-draws' ||
    !f.envelope.methodVersions.includes(f.state.parents.implementationId) ||
    !p.envelope.methodVersions.includes(p.state.parents.implementationId) ||
    !catalog.envelope.methodVersions.includes(c.generationImplementationId)
  )
    throw new Error('RELEASE_NUMERICAL_POLICY');
  const { scenario } = s.state,
    e = r.state.electorate;
  if (
    !(e.camera instanceof Uint32Array) ||
    !(e.senato instanceof Uint32Array) ||
    e.codes.length !== e.camera.length ||
    e.codes.length !== e.senato.length ||
    new Set(e.codes).size !== e.codes.length
  )
    throw new Error('RELEASE_ELECTORATES');
  const assets: FastVoteAssets = {
    fit: f.state.fit,
    cameraProfile: p.state.cameraProfile,
    senateAdapter: p.state.senateAdapter,
    modelId: FAST_SHARE_MODEL,
    numericalVersion: FAST_SHARE_NUMERICS,
    fitSeed: f.state.parents.fitSeed,
    profileReferenceDate: p.state.parents.referenceDate,
    geography: r.state.geography,
    electorates: e.codes.map((code, i) => ({
      code,
      date: e.date,
      camera: e.camera[i],
      senato: e.senato[i],
      provenance: e.provenance,
    })),
    sourceBundleId: c.sourceId,
    fittedStateId: f.envelope.id,
    profileStateId: p.envelope.id,
  };
  if (
    scenario.data.sourceBundleId !== c.sourceId ||
    scenario.data.fittedStateId !== c.fittedStateId ||
    scenario.data.profileStateId !== c.profileStateId ||
    scenario.data.seatTableVersion !== r.state.context.seats.version ||
    scenario.data.preferenceModelVersion !==
      r.state.context.models.preferences.parameterVersion ||
    scenario.models.generationImplementationId !==
      c.generationImplementationId ||
    (await sourceKey('conditional-electorates-v1', r.state.electorate)) !==
      c.electorateKey
  )
    throw new Error('RELEASE_SCENARIO_BINDING');
  const selection = s.state.selection;
  if (
    selection.referenceDate !== scenario.referenceDate ||
    canonicalJson([...selection.electionIds].sort()) !==
      canonicalJson([...scenario.historySelection.electionIds].sort()) ||
    canonicalJson([...selection.pollIds].sort()) !==
      canonicalJson([...scenario.polls.selected].sort()) ||
    (!options.allowEditedEvidence && scenario.polls.added.length > 0) ||
    canonicalJson(selection.sourceMemberships ?? {}) !==
      canonicalJson(scenario.sourceMemberships) ||
    canonicalJson(selection.componentMap ?? {}) !==
      canonicalJson(scenario.componentMap) ||
    canonicalJson(sorted(selection.targets)) !==
      canonicalJson(
        sorted(
          assets.electorates.map((t) => ({ code: t.code, electors: t.camera })),
        ),
      )
  )
    throw new Error('RELEASE_SOURCE_SELECTION_BINDING');
  const keys = await generationKeys(scenario, r.state.context, c.electorateKey);
  if (
    keys.compositionKey !== c.compositionKey ||
    keys.runKey !== c.runKey ||
    JSON.stringify(keys) !== JSON.stringify(s.state.keys)
  )
    throw new Error('RELEASE_SEMANTIC_KEYS');
  const prepared =
    options.validateGeneration === false
      ? undefined
      : prepareFastGeneration(scenario, assets, r.state.context);
  return {
    catalog: c,
    catalogEnvelope: catalog.envelope,
    scenario,
    selection: s.state.selection,
    assets,
    context: r.state.context,
    prepared,
    fitParents: f.state.parents,
    profileParents: p.state.parents,
    provenanceId: f.state.provenanceId,
    portableStates: {
      fit: {
        state: f.state,
        metadata: {
          sourceVersions: f.envelope.sourceVersions,
          methodVersions: f.envelope.methodVersions,
          assumptions: f.envelope.assumptions,
        },
      },
      profiles: {
        state: p.state,
        metadata: {
          sourceVersions: p.envelope.sourceVersions,
          methodVersions: p.envelope.methodVersions,
          assumptions: p.envelope.assumptions,
        },
      },
      runtime: {
        state: r.state,
        metadata: {
          sourceVersions: r.envelope.sourceVersions,
          methodVersions: r.envelope.methodVersions,
          assumptions: r.envelope.assumptions,
        },
      },
    },
  };
}
/** Source data is optional at ordinary startup, mandatory for release/refit checks. */
export async function loadCandidateSource(
  catalog: ReleaseCatalog,
  read: (path: string) => Promise<AssetBytes>,
) {
  const bytes = await decodeAsset(
      catalog.assets.source,
      await read(catalog.assets.source.path),
    ),
    source: CompactSource = JSON.parse(
      new TextDecoder('utf-8', { fatal: true }).decode(bytes),
    );
  await validateSourceEnvelope(source);
  if (
    source.id !== catalog.sourceId ||
    canonicalJson(source.payload.events.map((e) => e.chunk)) !==
      canonicalJson(catalog.sourceChunks)
  )
    throw new Error('RELEASE_SOURCE_BINDING');
  return source;
}
