import { generationFixture } from '../../src/lib/politics/reform-2026/generation.fixture';
import { fitReference } from '../../src/lib/politics/reform-2026/vote-share/information';
import { fitProfiles } from '../../src/lib/politics/reform-2026/vote-share/profiles';
import { prepareSenateAdapter } from '../../src/lib/politics/reform-2026/vote-share/senate-adapter';
import { FAST_SHARE_MODEL, FAST_SHARE_NUMERICS } from '../../src/lib/politics/reform-2026/vote-share/types';
import { FORECAST_STREAM_VERSION } from '../../src/lib/politics/reform-2026/vote-share/forecast-random';
import { REFERENCE_POLICY } from '../../src/lib/politics/reform-2026/vote-share/reference-family';
import type {
  FastGenerationScenario,
  FastVoteAssets
} from '../../src/lib/politics/reform-2026/vote-share/forecast-types';
import { reducedShareInput } from './fast-share-input';
let cached: ReturnType<typeof build> | undefined;
function build() {
  const legacy = generationFixture(),
    context = legacy.snapshot,
    input = reducedShareInput(),
    referenceDate = '2026-10-19';
  input.national.referenceDate = referenceDate;
  const fit = fitReference(input.national, 'fv04-fixture-fit'),
    components = input.national.components;
  const targets = context.municipalities.map((m) => ({ id: m.code, region: m.regionId, electors: 5000 }));
  const cameraProfile = fitProfiles({
    components,
    referenceDate,
    targetContext: 'camera',
    nationalElections: input.national.elections,
    targets,
    elections: input.national.elections.map((e) => ({
      id: e.id,
      date: e.date,
      context: e.context,
      rows: targets.map((t) => ({
        id: t.id,
        region: t.region,
        electors: 1000000,
        nonValid: 300000,
        groups: components.map((id) => [id]),
        counts: [175000, 175000, 175000, 175000]
      }))
    }))
  });
  const pairedRows = targets.map((t) => ({
    municipality: t.id,
    camera: {
      id: t.id,
      region: t.region,
      electors: 100000,
      nonValid: 30000,
      groups: components.map((id) => [id]),
      counts: [17500, 17500, 17500, 17500]
    },
    senato: {
      id: t.id,
      region: t.region,
      electors: 100000,
      nonValid: 30000,
      groups: components.map((id) => [id]),
      counts: [18000, 18000, 17000, 17000]
    }
  }));
  const senateAdapter = prepareSenateAdapter(cameraProfile, referenceDate, {
    date: '2022-09-25',
    availableDate: '2022-09-26',
    minimumAge: 18,
    sourceVersions: ['constructed-18plus-pair'],
    rows: pairedRows
  });
  const colleges = context.seats.geography.colleges.filter((c) => c.kind !== 'taa-proportional');
  const cells = context.memberships
    .filter((m) => colleges.some((c) => c.id === m.collegeId))
    .map((m) => ({
      chamber: m.chamber,
      collegeId: m.collegeId,
      unitId: m.municipalityCode + ':' + m.collegeId,
      municipality: m.municipalityCode,
      population: m.population
    }));
  const geography = {
    version: context.seats.geography.version,
    censusDate: context.seats.geography.censusDate,
    targets: context.municipalities.map((m) => ({
      code: m.code,
      name: m.name,
      region: m.regionId,
      province: m.provinceId,
      population: cells
        .filter((c) => c.chamber === 'camera' && c.municipality === m.code)
        .reduce((s, c) => s + c.population, 0)
    })),
    cells,
    colleges: colleges.map((c) => ({
      id: c.id,
      chamber: c.chamber,
      kind: c.kind,
      territoryId: c.territoryId
    }))
  };
  const assets: FastVoteAssets = {
    fit,
    cameraProfile,
    senateAdapter,
    geography,
    electorates: targets.map((t) => ({
      code: t.id,
      date: '2027-09-25',
      camera: t.electors,
      senato: t.electors,
      provenance: 'Conditional constructed electorate'
    })),
    modelId: FAST_SHARE_MODEL,
    numericalVersion: FAST_SHARE_NUMERICS,
    fitSeed: 'fv04-fixture-fit',
    profileReferenceDate: referenceDate,
    sourceBundleId: 'constructed-sources',
    fittedStateId: 'constructed-fit',
    profileStateId: 'constructed-profiles'
  };
  const s = legacy.scenario;
  const scenario: FastGenerationScenario = {
    schemaVersion: 4,
    kind: s.kind,
    algorithmId: s.algorithmId,
    referenceDate,
    electionDate: '2027-09-25',
    seed: 'fv04-generated-fixture',
    fitSeed: assets.fitSeed,
    data: {
      sourceBundleId: assets.sourceBundleId,
      fittedStateId: assets.fittedStateId,
      profileStateId: assets.profileStateId,
      geographyVersion: geography.version
    },
    models: {
      statistical: FAST_SHARE_MODEL,
      numerical: FAST_SHARE_NUMERICS,
      references: REFERENCE_POLICY,
      chambers: 'fast-share-paired-v1',
      rng: FORECAST_STREAM_VERSION
    },
    components: components.map((id) => ({ id, kind: 'fitted', provenance: 'Constructed fixture component' })),
    lists: {
      camera: s.lists.camera.map((l, i) => ({ ...l, componentIds: i === 0 ? ['a', 'b'] : ['c', 'd'] })),
      senato: s.lists.senato.map((l, i) => ({ ...l, componentIds: i === 0 ? ['a', 'b'] : ['c', 'd'] }))
    },
    renewal: s.renewal,
    coverage: s.coverage,
    candidateGeneration: { ...s.candidateGeneration, bonusPolicy: 'reference-proportional-v1' },
    candidateTemplates: s.candidateTemplates,
    suppliedVotes: s.suppliedVotes,
    suppliedPreferences: s.suppliedPreferences,
    bonusLinks: s.bonusLinks,
    preferenceProfileId: s.preferenceProfileId,
    chamberProfileId: s.chamberProfileId,
    references: [],
    nonValidReferences: []
  };
  return { scenario, assets, context, pairedRows };
}
export function fastGenerationFixture() {
  cached ??= build();
  return structuredClone(cached);
}
