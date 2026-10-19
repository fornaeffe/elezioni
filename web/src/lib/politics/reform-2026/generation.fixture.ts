import { getBundledReformSeatTables } from './bundled-data';
import { REFORM_MODEL_IDS, type ReformGenerationSnapshot } from './generation-types';
import { createDefaultReformScenario } from './generation-scenario';

/** Twenty constructed municipalities, one per region, spanning the real tested
 * seat capacities. Memberships are artificial population partitions. No source
 * acquisition or ignored production data are needed for generator mechanics. */
export function generationFixture() {
  const seats = getBundledReformSeatTables();
  (seats.geography as { municipalityCount: number }).municipalityCount = 20;
  const snapshot: ReformGenerationSnapshot = { schemaVersion: 1, kind: 'politics-reform-generation', version: 'constructed-generation-v1', historicalDate: '2022-09-25',
    sourceVersions: ['constructed-history-v1'], assumptions: ['Constructed equal regional electorate and population partitions; not historical data.'], seats,
    models: { schemaVersion: 1, version: 'constructed-models-v1', modelIds: REFORM_MODEL_IDS, sourceVersions: ['constructed-source-v1'], assumptions: ['Constructed test parameters, no empirical calibration.'],
      preferences: { parameterVersion: 'test-preferences-v1', baseline: { intensity: 0.5, strengthSigma: 1, localSigma: 0.3 }, pools: [], profiles: [
        { id: 'central-v1', intensityMultiplier: 1, concentrationMultiplier: 1, persistence: 0.7, tripleWeight: 1 },
        { id: 'dispersed-v1', intensityMultiplier: 1, concentrationMultiplier: 0.7, persistence: 0.7, tripleWeight: 1 },
        { id: 'no-triples-v1', intensityMultiplier: 1, concentrationMultiplier: 1, persistence: 0.7, tripleWeight: 0 }] },
      chambers: { parameterVersion: 'test-paired-v1', sigmaPerSqrtDay: { national: 0.02, region: 0.01, local: 0.01 }, profiles: [
        { id: 'central-v1', sharedVariance: 0.5, residualCorrelation: 0.7, gapMultiplier: 1 },
        { id: 'weaker-v1', sharedVariance: 0.5, residualCorrelation: 0, gapMultiplier: 1 },
        { id: 'stronger-v1', sharedVariance: 0.5, residualCorrelation: 0.96, gapMultiplier: 1 }] } },
    historicalLists: ['A', 'B'].map(id => ({ id, name: id, kind: 'list' })), historicalCoalitions: [],
    municipalities: seats.geography.regions.map((r, i) => ({ code: String(990001 + i), name: `Constructed ${r.name}`, regionId: r.id, provinceId: String(101 + i),
      camera: { electors: 500000, votes: [{ listId: 'A', votes: 135000 }, { listId: 'B', votes: 165000 }] },
      senato: { electors: 500000, votes: [{ listId: 'A', votes: 138000 }, { listId: 'B', votes: 162000 }] } })),
    memberships: seats.geography.colleges.map(c => ({ chamber: c.chamber, collegeId: c.id, population: c.population,
      municipalityCode: String(990000 + Number(seats.geography.territories.find(t => t.id === c.territoryId)!.regionId)) })) };
  const scenario = createDefaultReformScenario(snapshot, '2026-10-19', '2027-09-25', 'generation-test');
  return { snapshot, scenario };
}
