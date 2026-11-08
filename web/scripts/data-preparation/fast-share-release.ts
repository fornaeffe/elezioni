import { resolve } from "node:path";
import { readAuthoredPolitics, reformDefaultInputs, releasedDefault, validateAuthoredPolitics } from '../workspaces/politics-default';
import { buildPoliticsAssets, validateBuiltPoliticsAssets } from '../workspaces/politics-assets';
import type { releaseInputs } from "./fast-share-release-inputs";
import {
  implementationKey,
  immutableJson,
  readCachedState,
  writeAssets,
} from "./fast-share-release-io";
import {
  canonicalJson,
  sourceKey,
  type ByteAsset,
} from "../../src/lib/politics/reform-2026/vote-share/source-codec";
import {
  encodeState,
  type StateMetadata,
} from "../../src/lib/politics/reform-2026/vote-share/state-codec";
import { compileSource } from "../../src/lib/politics/reform-2026/vote-share/source-compiler";
import {
  fittedStateShapes,
  profileStateShapes,
} from "../../src/lib/politics/reform-2026/vote-share/state-shapes";
import { compilePairedHistory } from "../../src/lib/politics/reform-2026/vote-share/paired-source";
import { fitReference } from "../../src/lib/politics/reform-2026/vote-share/information";
import { fitProfiles } from "../../src/lib/politics/reform-2026/vote-share/profiles";
import { prepareSenateAdapter } from "../../src/lib/politics/reform-2026/vote-share/senate-adapter";
import {
  generationKeys,
  type CandidatePointer,
  type FitState,
  type ProfileState,
  type RuntimeState,
  type ScenarioState,
  type ReleaseCatalog,
} from "../../src/lib/politics/reform-2026/vote-share/release";
export async function prepareShareRelease(
  root: string,
  output: string,
  cache: string,
  inputs: Awaited<ReturnType<typeof releaseInputs>>,
) {
  const authored = await readAuthoredPolitics(root);
  const built = await buildPoliticsAssets(root, inputs.sources, inputs);
  await validateBuiltPoliticsAssets(authored, built);
  const readers = { implementationId: 'packaging', sourceChunk: inputs.read,
    asset: async (pin: import('../../src/lib/politics/scenario/schema').Asset) => {
      const asset = built.assets.get(pin.id);
      if (!asset) throw new Error(`Missing authored asset ${pin.id}`);
      return asset.bytes;
    } };
  await validateAuthoredPolitics(authored, readers);
  const projected = await reformDefaultInputs(authored, readers, inputs.legacy.context);
  const source = inputs.source, read = inputs.read;
  const policy = { id: authored.id, fitSeed: authored.execution.fittingSeed, referenceDate: authored.referenceDate,
    electionDate: authored.electionDate, electoratePolicy: built.electorate.targets[0].provenance,
    paired: { camera: authored.history.pairedCamera!, senato: authored.history.pairedSenato!, electoratePolicy: 'equal-only' as const } };
  const plan = { selection: projected.inputs.selection, electorates: built.electorate.targets, assumptions: [...authored.provenance.assumptions] };
  const compiled = await compileSource(source, read, plan.selection, { polls: projected.inputs.common.polls.added });
  const paired = await compilePairedHistory(source, read, plan.selection, policy.paired);
  const fitImplementation = await implementationKey(
      root,
      "web/src/lib/politics/reform-2026/vote-share/information.ts",
    ),
    profileImplementation = await implementationKey(
      root,
      "web/src/lib/politics/reform-2026/vote-share/senate-adapter.ts",
    );
  const fitParents = {
      nationalInputKey: compiled.nationalInputKey,
      implementationId: fitImplementation,
      fitSeed: policy.fitSeed,
      solver: "dense" as const,
    },
    fitCacheKey = await sourceKey("fit-cache-v1", fitParents),
    oldFit = await readCachedState<FitState>(
      cache,
      fitCacheKey,
      "fast-share-fit",
    );
  if (
    oldFit &&
    canonicalJson(oldFit.state.parents) !== canonicalJson(fitParents)
  )
    throw new Error("Fitted cache semantic mismatch");
  const fit =
    oldFit?.state.fit ??
    fitReference(compiled.national, policy.fitSeed, { solver: "dense" });
  const fitState: FitState = {
    parents: fitParents,
    fit,
    provenanceId: compiled.provenanceId,
    warnings: compiled.warnings,
    sampler: {
      chains: 4,
      burn: 1000,
      thin: 2,
      retention: "all-accepted-joint-context-draws",
    },
  };
  const metadata: StateMetadata = {
    sourceVersions: [
      source.id,
      ...source.sourceVersions,
      await sourceKey('authored-politics-scenario', authored),
    ],
    methodVersions: [policy.id, fitImplementation, profileImplementation],
    assumptions: [...source.assumptions, ...plan.assumptions],
  };
  const generationImplementation = await implementationKey(
    root,
    "web/src/lib/politics/reform-2026/vote-share/unified-generation.ts",
  );
  const generationMetadata: StateMetadata = {
    ...metadata,
    methodVersions: [...metadata.methodVersions, generationImplementation],
  };
  const fitted = await encodeState(
    "fast-share-fit",
    fitState,
    { ...metadata, methodVersions: [policy.id, fitImplementation] },
    fittedStateShapes(fit),
  );
  await writeAssets(cache, fitted.assets);
  if (!oldFit)
    await immutableJson(resolve(cache, fitCacheKey + ".json"), {
      id: fitted.envelope.id,
      descriptor: fitted.descriptor,
    });
  const pairedInputKey = await sourceKey("paired-input-v1", {
    date: paired.history.date,
    availableDate: paired.history.availableDate,
    minimumAge: paired.history.minimumAge,
    rows: paired.history.rows,
  });
  const profileParents = {
      profileInputKey: compiled.profileInputKey,
      pairedInputKey,
      implementationId: profileImplementation,
      referenceDate: policy.referenceDate,
    },
    profileCacheKey = await sourceKey("profiles-cache-v1", profileParents),
    oldProfile = await readCachedState<ProfileState>(
      cache,
      profileCacheKey,
      "fast-share-profiles",
    );
  if (
    oldProfile &&
    canonicalJson(oldProfile.state.parents) !== canonicalJson(profileParents)
  )
    throw new Error("Profile cache semantic mismatch");
  const cameraProfile =
      oldProfile?.state.cameraProfile ?? fitProfiles(compiled.profiles),
    senateAdapter =
      oldProfile?.state.senateAdapter ??
      prepareSenateAdapter(cameraProfile, policy.referenceDate, paired.history);
  senateAdapter.sourceVersions = [...paired.history.sourceVersions];
  const profiles = await encodeState(
    "fast-share-profiles",
    {
      parents: profileParents,
      cameraProfile,
      senateAdapter,
      pairedAudit: paired.audit,
    } satisfies ProfileState,
    metadata,
    profileStateShapes(cameraProfile),
  );
  await writeAssets(cache, profiles.assets);
  if (!oldProfile)
    await immutableJson(resolve(cache, profileCacheKey + ".json"), {
      id: profiles.envelope.id,
      descriptor: profiles.descriptor,
    });
  const context = structuredClone(inputs.legacy.context);
  context.assumptions = [
    ...context.assumptions,
    ...plan.assumptions,
    "The legacy model bundle is reused for preference parameters, seat tables and declared dependence profiles. Its old chamber sigmaPerSqrtDay fields are not used by the fast vote-share engine.",
  ];
  const electorate = {
    codes: plan.electorates.map((e) => e.code),
    date: plan.electorates[0].date,
    camera: Uint32Array.from(plan.electorates, (e) => e.camera),
    senato: Uint32Array.from(plan.electorates, (e) => e.senato),
    provenance: policy.electoratePolicy,
  };
  const runtimeState: RuntimeState = {
      context,
      geography: source.payload.geography,
      electorate,
    },
    runtime = await encodeState("fast-share-runtime", runtimeState, {
      ...generationMetadata,
      sourceVersions: [
        ...metadata.sourceVersions,
        inputs.legacy.modelsVersion,
        context.seats.version,
      ],
      methodVersions: [
        ...generationMetadata.methodVersions,
        inputs.legalImplementation,
      ],
    }),
    electorateKey = await sourceKey("conditional-electorates-v1", electorate);
  const scenario = releasedDefault(authored, projected, {
    sourceBundleId: source.id, fittedStateId: fitted.envelope.id, profileStateId: profiles.envelope.id,
    geographyVersion: source.payload.geography.version, seatTableVersion: context.seats.version,
    preferenceModelVersion: context.models.preferences.parameterVersion,
  }, generationImplementation);
  const keys = await generationKeys(scenario, context, electorateKey),
    defaults = await encodeState(
      "fast-share-scenario",
      {
        scenario,
        selection: plan.selection,
        defaultPolicy: { authoredScenarioId: authored.id, authoredScenarioHash: await sourceKey('authored-politics-scenario', authored) },
        keys,
        electorateKey,
      } satisfies ScenarioState,
      generationMetadata,
    );
  const sets = [fitted, profiles, runtime, defaults],
    sourcePaths = [
      inputs.descriptor.path,
      ...source.payload.events.map((e) => e.chunk.path),
    ];
  for (const path of sourcePaths)
    await writeAssets(output, new Map([[path, await read(path)]]));
  for (const set of sets) await writeAssets(output, set.assets);
  const generationPayloadCompressedBytes = sets.reduce(
      (n, s) => n + [...s.assets.values()].reduce((a, b) => a + b.length, 0),
      0,
    ),
    refitCompressedBytes =
      inputs.descriptor.compressedBytes +
      source.payload.events.reduce((n, e) => n + e.chunk.compressedBytes, 0);
  const catalogState: ReleaseCatalog = {
    generationImplementationId: generationImplementation,
    status: "candidate",
    policyId: policy.id,
    referenceDate: policy.referenceDate,
    electionDate: policy.electionDate,
    sourceId: source.id,
    fittedStateId: fitted.envelope.id,
    profileStateId: profiles.envelope.id,
    runtimeId: runtime.envelope.id,
    scenarioId: defaults.envelope.id,
    nationalInputKey: compiled.nationalInputKey,
    profileInputKey: compiled.profileInputKey,
    pairedInputKey,
    ...keys,
    electorateKey,
    assets: {
      source: inputs.descriptor,
      fit: fitted.descriptor,
      profiles: profiles.descriptor,
      runtime: runtime.descriptor,
      scenario: defaults.descriptor,
    },
    sourceChunks: source.payload.events.map((e) => e.chunk),
    byteBudget: { generationPayloadCompressedBytes, refitCompressedBytes },
  };
  const catalog = await encodeState(
    "fast-share-catalog",
    catalogState,
    generationMetadata,
  );
  await writeAssets(output, catalog.assets);
  const pointer: CandidatePointer = {
    schemaVersion: 1,
    kind: "fast-share-candidate",
    catalogId: catalog.envelope.id,
    catalog: catalog.descriptor,
    assetRoot: "data/reform-2026/fast-share/",
  };
  return {
    byteBudget: {
      generationPayloadCompressedBytes,
      catalogCompressedBytes: catalog.descriptor.compressedBytes,
      pointerBytes: new TextEncoder().encode(canonicalJson(pointer) + "\n")
        .length,
      generationCompressedBytes:
        generationPayloadCompressedBytes +
        catalog.descriptor.compressedBytes +
        new TextEncoder().encode(canonicalJson(pointer) + "\n").length,
      refitCompressedBytes,
    },
    pointer,
    catalog: catalogState,
    files: [
      ...sourcePaths,
      ...sets.flatMap((s) => [...s.assets.keys()]),
      ...catalog.assets.keys(),
    ],
    cache: {
      ...inputs.hits,
      fit: Boolean(oldFit),
      profiles: Boolean(oldProfile),
    },
    audit: {
      fittedComponents: fit.componentIds.length,
      futureComponents: scenario.components.length,
      lists: {
        camera: scenario.lists.camera.length,
        senato: scenario.lists.senato.length,
      },
      polls: { included: [...authored.history.polls.selected], excluded: { ...authored.history.polls.excluded } },
      pair: paired.audit,
      preferenceFit: inputs.legacy.preferenceAudit,
      sourceKey: inputs.sourceKey,
      legalKey: inputs.legalKey,
      fitCacheKey,
      profileCacheKey,
    },
  };
}
