import { expect, it } from 'vitest';
import { generationFixture } from './generation.fixture';
import { completeScrutinyFixture } from './scrutiny.fixture';
import { runPoliticsReformScrutiny } from './scrutiny';
import { createReformPresentation, buildReformSeatChart } from './result-presentation';
import { createSimulationResultExport, parseSimulationResultExport } from '../../core/result-export';

it('keeps failed/partial runs in the requested denominator and exports distinct legal and conditional outcomes', () => {
  const { scenario, snapshot } = generationFixture(), input = completeScrutinyFixture(60);
  input.simulationId = 3;
  const legal = runPoliticsReformScrutiny(input);
  const presentation = createReformPresentation(scenario, snapshot, 3);
  const trace = { warnings: [], references: [], normalization: [], streamVersion: 'test-v1', snapshotVersion: snapshot.version, modelVersion: snapshot.models.version };
  presentation.add(input, legal, trace);
  presentation.add({ ...input, simulationId: 4 }, { ...legal, simulationId: 4, status: 'non-computable', bonus: { ...legal.bonus, status: 'non-computable', awards: [], blockers: [{ chamber: 'camera', reason: 'missing-chamber-data' }] },
    stages: [{ norm: 'N18', chamber: 'camera', status: 'non-computable', reason: 'cap-group-capacity-mismatch', affectedIds: ['test'] }] }, trace);
  presentation.failure(5, new Error('No legal progress'));
  const output = presentation.finish(), find = (name: string) => output.tables.find(t => t.name === name)!;
  expect(find('Reform overview').rows[0]).toMatchObject({ Requested: 3, Complete: 1, 'Non-computable': 2 });
  expect(find('Reform overview').rows[0]['Joint bonus probability / requested']).toBe(1 / 3);
  expect(find('Reform runs').rows.map(r => r.Simulation)).toEqual([3, 4, 5]);
  expect(output.warnings).toContainEqual(expect.objectContaining({ code: 'R26_N18_NON_COMPUTABLE', severity: 'error', message: expect.stringContaining('cap-group-capacity-mismatch') }));
  expect(find('Vote-based entitlements').rows).toHaveLength(4);
  expect(find('Candidate outcomes (conditional)').rows.length).toBeGreaterThan(0);
  expect(find('Models, references and conditions').rows).toContainEqual(expect.objectContaining({ Kind: 'Candidate/vote condition', Value: expect.stringContaining('unknown-ballot-composition') }));
  expect(find('I11 zero-entitlement exclusions').rows.length).toBeGreaterThan(0);
  expect(find('Models, references and conditions').rows).toContainEqual(expect.objectContaining({ Kind: 'Applied interpretation', Value: expect.stringContaining('I11-v1') }));
  const bars = buildReformSeatChart(find('Seat entitlement and election locations'), 'camera', 3);
  expect(bars.reduce((n, b) => n + b.seats, 0)).toBe(392);
  expect(bars.some(b => b.label.startsWith('Premio /'))).toBe(true);
  const result = { type: 'result' as const, status: 'completed' as const, ...output,
    benchmark: { startedAt: '2026-10-20T00:00:00Z', elapsedMs: 10, simulations: 3, dataVersion: scenario.snapshotVersion, scrutinyAlgorithmId: scenario.algorithmId, seed: scenario.seed } };
  const exported = createSimulationResultExport({ scenario, result, exportedAt: '2026-10-20T00:00:00Z' });
  expect(parseSimulationResultExport(JSON.stringify(exported))).toMatchObject({ scenario, result });
  exported.result.benchmark.seed = 'wrong-seed';
  expect(() => parseSimulationResultExport(JSON.stringify(exported))).toThrow(/identity mismatch/);
});

it('preserves vote-based entitlements when preference-dependent candidate identities cannot be filled', () => {
  const { scenario, snapshot } = generationFixture(), input = completeScrutinyFixture(60);
  const supplied = runPoliticsReformScrutiny(input);
  if (input.renewal.kind !== 'joint') throw new Error();
  for (const c of [input.renewal.camera, input.renewal.senato]) for (const slate of c.ordinarySlates) slate.preferences = { kind: 'missing', reason: 'No supplied candidate preference data' };
  const missing = runPoliticsReformScrutiny(input);
  expect(missing.status).toBe('partial');
  expect(missing.chambers[0].national).toEqual(supplied.chambers[0].national);
  const presentation = createReformPresentation(scenario, snapshot, 1);
  presentation.add(input, missing, { warnings: [], references: [], normalization: [], streamVersion: 'test', snapshotVersion: snapshot.version, modelVersion: snapshot.models.version });
  const result = presentation.finish();
  const ledger = result.tables.find(t => t.name === 'Seat entitlement and election locations')!;
  expect(ledger.rows).toHaveLength(588);
  expect(ledger.rows.some(r => r.Status !== 'filled' && r.Candidate === null)).toBe(true);
  expect(result.tables.find(t => t.name === 'Models, references and conditions')!.rows).toContainEqual(expect.objectContaining({ Kind: 'Candidate/vote condition', Value: expect.stringContaining('missing-preferences') }));
  expect(result.tables.find(t => t.name === 'Reform overview')!.rows[0]).toMatchObject({ Requested: 1, Partial: 1 });
});

it('releases drained detail while keeping detached batches and cumulative requested-run summaries', () => {
  const { scenario, snapshot } = generationFixture(), input = completeScrutinyFixture(60);
  const legal = runPoliticsReformScrutiny(input);
  const presentation = createReformPresentation(scenario, snapshot, 3);
  presentation.add(input, legal, { warnings: [], references: [], normalization: [], streamVersion: 'test', snapshotVersion: snapshot.version, modelVersion: snapshot.models.version });
  const first = presentation.drain();
  const firstLedger = first.tables.find(t => t.name === 'Seat entitlement and election locations')!;
  expect(firstLedger.rows).toHaveLength(588);
  expect(presentation.drain().tables.every(t => t.rows.length === 0)).toBe(true);
  presentation.failure(2, new Error('No legal progress'));
  const failed = presentation.drain();
  expect(failed.tables.find(t => t.name === 'Reform runs')!.rows).toEqual([
    { Simulation: 2, Status: 'non-computable', Bonus: 'unknown', Reason: 'No legal progress' }
  ]);
  expect(failed.warnings).toContainEqual(expect.objectContaining({ code: 'R26_RUN_ERROR', simulationId: 2 }));
  const final = presentation.finish();
  expect(firstLedger.rows).toHaveLength(588);
  expect(final.tables.find(t => t.name === 'Seat entitlement and election locations')!.rows).toEqual([]);
  expect(final.tables.find(t => t.name === 'Reform overview')!.rows[0]).toMatchObject({
    Requested: 3, Complete: 1, 'Non-computable': 1, 'Joint bonus probability / requested': 1 / 3
  });
});
