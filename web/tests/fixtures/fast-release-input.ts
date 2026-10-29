/** Accepted reduced FV-04 states in a constructed FV-05 catalog. Original refit
 * files are deliberately absent: ordinary startup must not read them. */
import { fastGenerationFixture } from './fast-generation-input';
import { encodeState } from '../../src/lib/politics/reform-2026/vote-share/state-codec';
import { sourceKey } from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import {
  generationKeys,
  type FitState,
  type ProfileState,
  type RuntimeState,
  type ScenarioState,
  type ReleasedScenario,
  type ReleaseCatalog,
  type CandidatePointer,
} from '../../src/lib/politics/reform-2026/vote-share/release';
export async function fastReleaseFixture() {
  const f = fastGenerationFixture(),
    implementation = 'fixture-implementation-v1',
    metadata = {
      sourceVersions: [f.assets.sourceBundleId],
      methodVersions: [implementation],
      assumptions: ['Constructed loader boundary only'],
    };
  const fitState: FitState = {
    parents: {
      nationalInputKey: 'national-key',
      implementationId: implementation,
      fitSeed: f.scenario.fitSeed,
      solver: 'dense',
    },
    fit: f.assets.fit,
    provenanceId: 'fixture-provenance',
    warnings: [],
    sampler: {
      chains: 4,
      burn: 1000,
      thin: 2,
      retention: 'all-accepted-joint-context-draws',
    },
  };
  const profilesState: ProfileState = {
    parents: {
      profileInputKey: 'profile-key',
      pairedInputKey: 'paired-key',
      implementationId: implementation,
      referenceDate: f.scenario.referenceDate,
    },
    cameraProfile: f.assets.cameraProfile,
    senateAdapter: f.assets.senateAdapter!,
    pairedAudit: { constructed: true },
  };
  const fit = await encodeState('fast-share-fit', fitState, metadata, {
    '/fit/political/values': [
      4,
      f.assets.fit.political.drawsPerChain,
      f.assets.fit.political.dimensions,
    ],
    '/fit/participation/values': [
      4,
      f.assets.fit.participation.drawsPerChain,
      4,
    ],
  });
  const shapes: Record<string, number[]> = {};
  for (const path of ['/cameraProfile', '/senateAdapter/profile'])
    for (const name of ['means', 'profileSd', 'fallback'])
      shapes[path + '/' + name] = [
        f.assets.cameraProfile.municipalityIds.length,
        5,
      ];
  const profiles = await encodeState(
      'fast-share-profiles',
      profilesState,
      metadata,
      shapes,
    ),
    e = f.assets.electorates;
  const runtimeState: RuntimeState = {
    context: f.context,
    geography: f.assets.geography,
    electorate: {
      codes: e.map((r) => r.code),
      date: e[0].date,
      camera: Uint32Array.from(e, (r) => r.camera),
      senato: Uint32Array.from(e, (r) => r.senato),
      provenance: e[0].provenance,
    },
  };
  const runtime = await encodeState(
      'fast-share-runtime',
      runtimeState,
      metadata,
    ),
    electorateKey = await sourceKey(
      'conditional-electorates-v1',
      runtimeState.electorate,
    );
  const scenario: ReleasedScenario = {
    ...f.scenario,
    data: {
      ...f.scenario.data,
      fittedStateId: fit.envelope.id,
      profileStateId: profiles.envelope.id,
      seatTableVersion: f.context.seats.version,
      preferenceModelVersion: f.context.models.preferences.parameterVersion,
    },
    models: {
      ...f.scenario.models,
      generationImplementationId: implementation,
    },
    historySelection: {
      electionIds: [],
      pairedCamera: 'camera',
      pairedSenato: 'senato',
    },
    sourceMemberships: {},
    componentMap: {},
    polls: { selected: [], added: [], excluded: {} },
    presentation: { lists: [], components: [] },
  };
  const selection = {
    referenceDate: scenario.referenceDate,
    electionIds: [],
    pollIds: [],
    components: f.assets.fit.componentIds,
    targets: e.map((r) => ({ code: r.code, electors: r.camera })),
    targetContext: 'camera' as const,
  };
  const keys = await generationKeys(scenario, f.context, electorateKey),
    scenarioState: ScenarioState = {
      scenario,
      selection,
      defaultPolicy: { constructed: true },
      keys,
      electorateKey,
    },
    defaults = await encodeState(
      'fast-share-scenario',
      scenarioState,
      metadata,
    );
  const catalogState: ReleaseCatalog = {
    status: 'candidate',
    policyId: 'fixture',
    referenceDate: scenario.referenceDate,
    electionDate: scenario.electionDate,
    sourceId: f.assets.sourceBundleId,
    fittedStateId: fit.envelope.id,
    profileStateId: profiles.envelope.id,
    runtimeId: runtime.envelope.id,
    scenarioId: defaults.envelope.id,
    nationalInputKey: 'national-key',
    profileInputKey: 'profile-key',
    pairedInputKey: 'paired-key',
    ...keys,
    electorateKey,
    generationImplementationId: implementation,
    assets: {
      source: {
        path: 'unrequested-source.gz',
        compressedBytes: 100,
        decodedBytes: 100,
        sha256: '0'.repeat(64),
        decodedSha256: '0'.repeat(64),
      },
      fit: fit.descriptor,
      profiles: profiles.descriptor,
      runtime: runtime.descriptor,
      scenario: defaults.descriptor,
    },
    sourceChunks: [],
    byteBudget: {
      generationPayloadCompressedBytes: 0,
      refitCompressedBytes: 0,
    },
  };
  const catalog = await encodeState(
      'fast-share-catalog',
      catalogState,
      metadata,
    ),
    assets = new Map(
      [fit, profiles, runtime, defaults, catalog].flatMap((s) => [...s.assets]),
    );
  const pointer: CandidatePointer = {
    schemaVersion: 1,
    kind: 'fast-share-candidate',
    catalogId: catalog.envelope.id,
    catalog: catalog.descriptor,
    assetRoot: 'data/reform-2026/fast-share/',
  };
  const read = async (path: string) => {
    const bytes = assets.get(path);
    if (!bytes) throw new Error('Missing required asset ' + path);
    return bytes;
  };
  return {
    pointer,
    assets,
    read,
    catalogState,
    scenarioState,
    fitState,
    metadata,
    implementation,
    fit,
  };
}
