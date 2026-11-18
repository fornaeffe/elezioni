import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { hasGeneratedFixture, loadGeneratedJsonFixture } from '$lib/test/generated-fixtures';
import { politicsScrutinyAlgorithms, resolvePoliticsSimulationAlgorithm, runPoliticsSimulation, type PoliticsSimulationRequest } from './simulation-scrutiny';
import { resolvePoliticsScrutinyAlgorithm } from './scrutiny-algorithms';
import { completeScrutinyFixture } from './reform-2026/scrutiny.fixture';
import type { PoliticsGoldenFixture } from './types';
import { runPoliticsScrutiny } from './scrutiny';

describe('simulation-level scrutiny registry', () => {
  it('registers both real algorithms with explicit production availability', () => {
    expect(politicsScrutinyAlgorithms.map(a => [a.id, a.productionAvailable])).toEqual([
      ['politiche-r-parity-v1', true], ['politiche-reform-2026-v1', true]
    ]);
    expect(resolvePoliticsSimulationAlgorithm('politiche-reform-2026-v1').id).toBe('politiche-reform-2026-v1');
    expect(() => resolvePoliticsSimulationAlgorithm('unknown')).toThrow(/unknown-algorithm/);
    expect(resolvePoliticsSimulationAlgorithm('politiche-reform-2026-v1', 'production').productionAvailable).toBe(true);
    expect(() => resolvePoliticsScrutinyAlgorithm('politiche-reform-2026-v1')).toThrow(/production-unavailable/);
    expect(() => runPoliticsSimulation({ algorithmId: 'unknown', simulationId: 1 } as unknown as PoliticsSimulationRequest)).toThrow(/unknown-algorithm/);
  });
  it('executes paired reform input under exactly the requested simulation and algorithm', () => {
    const input = completeScrutinyFixture();
    const request = { algorithmId: input.algorithmId, simulationId: input.simulationId, input };
    const r = runPoliticsSimulation(request);
    expect(r).toMatchObject({ algorithmId: input.algorithmId, simulationId: input.simulationId,
      result: { algorithmId: input.algorithmId, simulationId: input.simulationId, status: 'complete', renewedChambers: ['camera', 'senato'] } });
    expect(() => runPoliticsSimulation({ ...request, simulationId: 99 })).toThrow(/same algorithm and simulation/);
    expect(() => runPoliticsSimulation({ ...request, simulationId: NaN })).toThrow(/simulation ID/);
    expect(() => politicsScrutinyAlgorithms[0].execute(request)).toThrow(/requires legacy input/);
  });
  const path = fileURLToPath(new URL('../../../../test/fixtures/politiche/debug_scrutinio.json', import.meta.url));
  const golden = hasGeneratedFixture(path) ? it : it.skip;
  golden('preserves all ten paired old-law golden outputs through the common adapter', () => {
    const fixture = loadGeneratedJsonFixture<PoliticsGoldenFixture>(path);
    for (let i = 0; i < 10; i++) {
      const request: PoliticsSimulationRequest = { algorithmId: 'politiche-r-parity-v1', simulationId: fixture.rami.camera.simulations[i].sim,
        chambers: (['camera', 'senato'] as const).map(ramo => {
          const side = fixture.rami[ramo];
          return { input: side.simulations[i].input, context: { ramo, liste_naz: side.liste_naz, totali_pluri: side.totali_pluri, totale_seggi: side.totale_seggi } };
        }) };
      const r = runPoliticsSimulation(request);
      if (r.algorithmId !== 'politiche-r-parity-v1') throw new Error();
      for (const side of r.chambers) {
        const source = request.chambers.find(c => c.context.ramo === side.chamber)!;
        expect(side.output).toEqual(runPoliticsScrutiny(source.input, source.context));
        // The R fixture tables use keyed row order; compare the same unordered
        // result-table boundary as the established old-law golden checks.
        const expected = fixture.rami[side.chamber].simulations[i].expected;
        for (const table of ['liste_pluri', 'candidati_uni', 'candidati_pluri'] as const) {
          const canonical = (rows: readonly object[]) => rows.map(row => JSON.stringify(Object.fromEntries(Object.entries(row).sort()))).sort();
          expect(canonical(side.output[table])).toEqual(canonical(expected[table]));
        }
      }
      const other = structuredClone(request);
      other.chambers[0].input.liste_uni[0].SIM = request.simulationId + 1;
      expect(() => runPoliticsSimulation(other)).toThrow(/different simulation/);
    }
  }, 30000);
});
