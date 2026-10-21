import { readFile, access } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
import { gunzipSync } from 'fflate';
import { expect, it } from 'vitest';
import catalog from './data/generation-catalog.generated.json';
import { generateReformSimulation, prepareReformGeneration } from './generation';
import { runPoliticsReformScrutiny } from './scrutiny';
import type { ReformGenerationSnapshot } from './generation-types';
import { createReformPresentation } from './result-presentation';
import { createSimulationResultExport, parseSimulationResultExport } from '../../core/result-export';

const path = fileURLToPath(new URL(`../../../../static/${catalog.path}`, import.meta.url));
const present = await access(path).then(() => true, () => false);
it.skipIf(!present)('R26-19 required production bundle: acquire/generate with data:reform commands when absent (release check never skips)', async () => {
  const compressed = await readFile(path), hash = (b: Uint8Array) => createHash('sha256').update(b).digest('hex');
  expect(hash(compressed)).toBe(catalog.sha256);
  const bytes = gunzipSync(compressed); expect(hash(bytes)).toBe(catalog.uncompressedSha256);
  const snapshot: ReformGenerationSnapshot = JSON.parse(new TextDecoder().decode(bytes));
  const defaults = JSON.parse(await readFile(new URL(`./data/${catalog.defaultsPath}`, import.meta.url), 'utf8'));
  const prepared = prepareReformGeneration(defaults, snapshot);
  const generated = generateReformSimulation(prepared, 1);
  expect(generated.input.chamberModel.kind).toBe('generated');
  const result = runPoliticsReformScrutiny(generated.input);
  expect(result.seats).toHaveLength(588);
  expect(result.stages.filter(s => s.status === 'non-computable')).toEqual([]);
  // Recorded benchmark simulation 4: ordinary Basilicata integer-seat donor.
  const replay = runPoliticsReformScrutiny(generateReformSimulation(prepared, 4).input);
  expect(replay.status).toBe('complete');
  expect(replay.seats).toHaveLength(588);
  expect(new Set(replay.seats.flatMap(s => s.status === 'filled' ? [s.candidateId] : [])).size).toBe(588);
  expect(replay.warnings).toContainEqual(expect.objectContaining({ code: 'I13_INTEGER_SEAT_REMOVAL', chamber: 'camera', severity: 'warning' }));
  expect(replay.interpretations).toContainEqual({ id: 'I13', version: 'I13-v1' });
  expect(replay.transfers).toContainEqual(expect.objectContaining({ interpretationId: 'I13',
    remainders: [expect.objectContaining({ before: 'unused', after: 'unused' })] }));
  // Recorded benchmark 165: Veneto 1's +Europa target needs a second pass.
  const repeated = runPoliticsReformScrutiny(generateReformSimulation(prepared, 165).input);
  expect(repeated.status).toBe('complete');
  expect(repeated.seats).toHaveLength(588);
  expect(repeated.interpretations).toContainEqual({ id: 'I14', version: 'I14-v1' });
  expect(repeated.warnings).toContainEqual(expect.objectContaining({ code: 'I14_REPEATED_REMAINDER_PASS', severity: 'warning', chamber: 'camera' }));
  const matrix = repeated.chambers[0].cameraTerritorial!.colleges.matrices.find(m => m.contextId.endsWith('camera:territory:07/lists'))!;
  const europa = matrix.cells.find(c => c.targetId === 'list-4737a15e2c762fa0')!;
  expect(europa).toMatchObject({ integerSeats: 1, provisionalSeats: 3, seats: 3, remainderState: 'used' });
  expect(repeated.chambers[0].recovery!.remainders).toContainEqual(expect.objectContaining({
    collegeId: europa.areaId, subject: { kind: 'list', id: europa.targetId }, value: europa.decimalPart, state: 'used' }));
  expect(new Set(repeated.seats.flatMap(s => s.status === 'filled' ? [s.candidateId] : [])).size).toBe(588);
  expect(JSON.parse(JSON.stringify(repeated)).interpretations).toContainEqual({ id: 'I14', version: 'I14-v1' });
  // Recorded benchmark 31: Campania's M5S integer surplus in Senate N21.
  const senateReplay = runPoliticsReformScrutiny(generateReformSimulation(prepared, 31).input);
  expect(senateReplay.status).toBe('complete');
  expect(senateReplay.seats).toHaveLength(588);
  expect(new Set(senateReplay.seats.flatMap(s => s.status === 'filled' ? [s.candidateId] : [])).size).toBe(588);
  expect(senateReplay.warnings).toContainEqual(expect.objectContaining({ code: 'I13_INTEGER_SEAT_REMOVAL', chamber: 'senato', severity: 'warning' }));
  expect(senateReplay.interpretations).toContainEqual({ id: 'I13', version: 'I13-v1' });
  expect(senateReplay.transfers).toContainEqual(expect.objectContaining({ chamber: 'senato', norm: 'N21', interpretationId: 'I13',
    remainders: [expect.objectContaining({ before: 'unused', after: 'unused' }), expect.anything()] }));
  // Recorded benchmark 242: active N22 minority-list correction fails in region 08.
  const failedInput = generateReformSimulation(prepared, 242), failed = runPoliticsReformScrutiny(failedInput.input);
  const senate = failed.chambers.find(c => c.chamber === 'senato')!;
  expect(failed.status).toBe('non-computable');
  expect(senate.regional).toMatchObject({ status: 'complete', path: 'bonus-with-cap' });
  expect(senate.senateTerritorial!.lists.status).toBe('complete');
  expect(senate.senateTerritorial!.colleges).toMatchObject({ status: 'non-computable',
    reason: 'no-eligible-recipient-remainder', affectedRegionId: '08' });
  expect(failed.stages).toContainEqual({ norm: 'N22', chamber: 'senato', status: 'non-computable',
    reason: 'no-eligible-recipient-remainder', affectedIds: ['08'] });
  const presentation = createReformPresentation(defaults, snapshot, 1);
  presentation.add(failedInput.input, failed, failedInput.trace);
  const output = presentation.finish();
  const expectedRow = { Simulation: 242, Chamber: 'senato', Norm: 'N22', Status: 'non-computable',
    Reason: 'no-eligible-recipient-remainder', 'Affected IDs': '["08"]' };
  expect(output.tables.find(t => t.name === 'Legal stages and blockers')!.rows).toContainEqual(expectedRow);
  const error = output.warnings.find(w => w.code === 'R26_N22_NON_COMPUTABLE')!;
  expect(error).toMatchObject({ severity: 'error', simulationId: 242 });
  expect(error.message).toContain('no-eligible-recipient-remainder');
  expect(error.message).not.toContain('alternative-allocation-path');
  const exported = createSimulationResultExport({ scenario: defaults, exportedAt: '2026-10-21T00:00:00Z',
    result: { type: 'result', status: 'completed', ...output, benchmark: { startedAt: '2026-10-21T00:00:00Z',
      elapsedMs: 0, simulations: 1, dataVersion: defaults.snapshotVersion, scrutinyAlgorithmId: defaults.algorithmId, seed: defaults.seed } } });
  const imported = parseSimulationResultExport(JSON.stringify(exported));
  expect(imported.result.tables.find(t => t.name === 'Legal stages and blockers')!.rows).toContainEqual(expectedRow);
}, 30000);
