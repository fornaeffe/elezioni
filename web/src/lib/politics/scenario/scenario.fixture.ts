import { chambers, laws, REFORM, ROSATELLUM, type Asset, type LawId, type Location, type PoliticsScenario } from './schema';
import { requiredAssets } from './dependencies';
import type { DomainContext } from './validation';

export function asset(id: string): Asset {
  return { id, version: `${id}-v1`, schemaVersion: 1, bytes: 100, sha256: 'a'.repeat(64), uri: `/data/${id}.json` };
}
export function location(law: LawId, chamber: 'camera' | 'senato', level: Location['level'] = law === REFORM ? 'college' : 'plurinominal'): Location {
  return { law, geographyVersion: `${law}-geography-v1`, chamber, level, code: '01' };
}
export function fixture(): { scenario: PoliticsScenario; context: DomainContext } {
  const modelAssets = (law: LawId) => ({ geography: asset(`${law}-geography`), seats: asset(`${law}-seats`),
    models: { M01: [asset(`${law}-m01`)], M02: [asset(`${law}-m02`)] } });
  const fallback = { M01: { policy: 'regional-or-equal-list-v1' as const, provenance: 'M01 existing fallback' },
    M02: { geographicLogProfile: -2, profileSd: 0.1, localInnovationSd: 0.2, nationalInnovationSd: 0.3, provenance: 'Explicit test assumption' } };
  const policies = { numerical: 'fast-share-float64-v1', references: 'fixed-turnout-reference-v1', chambers: 'fast-share-paired-v1',
    rng: 'fast-share-forecast-streams-v1', fittingRng: 'fast-share-fit-streams-v1' } as const;
  const filings = (law: LawId) => [
    { listId: 'a', locations: [location(law, 'camera')] },
    { listId: 'b', locations: [location(law, 'senato')] } ];
  const scenario: PoliticsScenario = {
    kind: 'politics-scenario', schemaVersion: 1, id: 'test', name: 'Politiche — prova', electionDate: '2027-09-25', referenceDate: '2026-10-19', renewal: 'joint',
    selection: { scrutiny: { id: REFORM, version: '1.0.0' }, generatorsByLaw: {
      [ROSATELLUM]: { id: 'M01', version: '2.0.0' }, [REFORM]: { id: 'M02', version: '2.0.0' } } },
    data: { historical: asset('history'), catalogue: asset('catalogue'), referenceGeography: asset('reference-geo'), electorate: asset('electorate'),
      lawAssets: { [ROSATELLUM]: modelAssets(ROSATELLUM), [REFORM]: modelAssets(REFORM) } },
    history: { electionIds: ['e-camera'], pairedCamera: 'e-camera', pairedSenato: 'e-senato', polls: { selected: [], added: [], excluded: {} },
      sourceMemberships: [{ sourceId: 'e-camera', questionId: 'q', choiceId: 'a', members: ['ca'], provenance: 'Observed association' }], pooling: [] },
    subjects: { components: [], coalitions: [{ id: 'A', name: 'Coalizione A', color: '#2563EB', chambers: ['camera'] }],
      lists: [{ id: 'a', name: 'Lista A', color: '#003366', kind: 'list', coalitionId: 'A', minority: null },
        { id: 'b', name: 'Lista B', color: '#D71920', kind: 'list', coalitionId: null, minority: null }] },
    memberships: [{ listId: 'a', camera: ['ca'], senato: ['ca'], locked: true, newComponentId: 'na' },
      { listId: 'b', camera: ['cb'], senato: ['cb'], locked: true, newComponentId: 'nb' }],
    newComponents: [{ id: 'na', name: 'Nuova A', support: 'Assumed', provenance: 'Scenario', fallback },
      { id: 'nb', name: 'Nuova B', support: 'Assumed', provenance: 'Scenario', fallback }],
    references: [{ id: 'r-a', scope: { kind: 'national' }, channel: 'common', subject: { kind: 'list', id: 'a' }, mean: 0.4,
      denominator: 'valid-votes', appliesTo: ['M01', 'M02'], uncertainty: { sdPercentagePoints: 1, origin: 'explicit', policyVersion: null } }],
    nonValidReferences: [], candidates: [],
    lawSettings: {
      [ROSATELLUM]: { schemaVersion: 1, filings: filings(ROSATELLUM), placements: [], externalCandidacies: [],
        candidateGeneration: { uninominalToPlurinominalShare: 0.35, plurinominalCandidacyCountShares: [0.875, 0.0625, 0.025, 0.0125, 0.025] } },
      [REFORM]: { schemaVersion: 1, filings: filings(REFORM), placements: [], externalCandidacies: [],
        candidateGeneration: { reuseProbability: 0.2, maximumOrdinaryCandidacies: 5, bonusPolicy: 'reference-proportional-v1' },
        bonusLinks: [{ camera: { kind: 'coalition', id: 'A' }, senato: { kind: 'list', id: 'b' } }], preferenceProfileId: 'baseline', chamberProfileId: 'baseline',
        suppliedVotes: [], suppliedPreferences: [] } },
    modelSettings: {
      M01: { schemaVersion: 1, probabilityLawVersion: '2.0.0', parameterDate: '2026-10-19', policies,
        compositions: [{ sourceId: 'e-camera', questionId: 'q', choiceId: 'a', fractions: { ca: 1, cb: 0 }, provenance: 'Explicit composition', proxy: false }], supplementalPolls: [] },
      M02: { schemaVersion: 1, probabilityLawVersion: '2.0.0', policies, statistical: 'fast-share-kind-common-conservative-v1',
        forecast: 'bounded-mass-pair-field-v2', forecastScale: 1.5, profileRegionCorrelation: 0.25, forecastReference: 'conditional',
        uncertaintyMultipliers: [{ listId: 'a', chamber: 'camera', national: null, local: 0 }] } },
    execution: { simulations: 10, fittingSeed: 'fit', simulationSeed: 'sim', workers: { kind: 'auto', policy: 'model-aware-v1' } },
    provenance: { origin: 'Constructed contract test, not a default', sourceHashes: [], assumptions: [], warnings: [], referenceDefaultPolicy: 'reference-defaults-v1' }
  };
  return { scenario, context: {
    componentIds: ['ca', 'cb', 'cc'], pollIds: [], sources: [
      { id: 'e-camera', questionId: 'q', choiceIds: ['a', 'b'], availableDate: '2022-09-26' },
      { id: 'e-senato', questionId: 'q', choiceIds: ['a', 'b'], availableDate: '2022-09-26' } ],
    referenceGeography: [{ kind: 'region', code: '01' }, { kind: 'province', code: '001' }, { kind: 'municipality', code: '001001' }],
    locations: laws.flatMap(law => chambers.flatMap(ch => law === REFORM ? [location(law, ch), location(law, ch, 'territory')] :
      [location(law, ch), location(law, ch, 'uninominal')])),
    geographyRelations: laws.flatMap(law => chambers.map(ch => ({ parent: location(law, ch, law === REFORM ? 'territory' : 'plurinominal'),
      child: location(law, ch, law === REFORM ? 'college' : 'uninominal') }))),
    availableAssets: [...requiredAssets(scenario), asset(`${ROSATELLUM}-geography`), asset(`${ROSATELLUM}-seats`),
      asset(`${ROSATELLUM}-m01`), asset(`${REFORM}-m01`)]
  } };
}
