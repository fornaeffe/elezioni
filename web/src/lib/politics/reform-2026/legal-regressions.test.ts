import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { createHash } from 'node:crypto';
import { expect, it } from 'vitest';
import manifest from '../../../../../test/fixtures/reform-legal-inputs-v1.manifest.json';
import { runPoliticsReformScrutiny } from './scrutiny';
import { createReformPresentation } from './result-presentation';
import { createSimulationResultExport } from '../../core/result-export';
import type { ReformAssemblyContext, ReformGenerationScenario, GenerationTrace } from './generation-types';
import type { ReformScrutinyInput } from './input';

// These exact legal inputs remain independent of generated defaults, source
// geography updates and generator seeds. Never skip or refresh them implicitly.
it('preserves the frozen production I13/I14/I16 regressions and presentation', () => {
  const compressed = readFileSync(new URL(`../../../../../test/fixtures/${manifest.path}`, import.meta.url));
  const hash = (bytes: Uint8Array) => createHash('sha256').update(bytes).digest('hex');
  expect(compressed.length).toBe(manifest.bytes);
  expect(hash(compressed)).toBe(manifest.sha256);
  const bytes = gunzipSync(compressed);
  expect(hash(bytes)).toBe(manifest.uncompressedSha256);
  const fixture: { defaults: ReformGenerationScenario; context: ReformAssemblyContext;
    runs: Record<number, { input: ReformScrutinyInput; trace: GenerationTrace }> } = JSON.parse(bytes.toString('utf8'));
  expect(Object.keys(fixture.runs).map(Number)).toEqual(manifest.runs.map(r => r.id).sort((a,b) => a-b));
  for (const run of manifest.runs) expect(hash(Buffer.from(JSON.stringify(fixture.runs[run.id].input)))).toBe(run.inputSha256);
  const defaults = fixture.defaults, snapshot = fixture.context;
  const result = runPoliticsReformScrutiny(fixture.runs[1].input);
  expect(result.seats).toHaveLength(588);
  expect(result.stages.filter(s => s.status === 'non-computable')).toEqual([]);
  // Recorded benchmark simulation 4: ordinary Basilicata integer-seat donor.
  const replay = runPoliticsReformScrutiny(fixture.runs[4].input);
  expect(replay.status).toBe('complete');
  expect(replay.seats).toHaveLength(588);
  expect(new Set(replay.seats.flatMap(s => s.status === 'filled' ? [s.candidateId] : [])).size).toBe(588);
  expect(replay.warnings).toContainEqual(expect.objectContaining({ code: 'I13_INTEGER_SEAT_REMOVAL', chamber: 'camera', severity: 'warning' }));
  expect(replay.interpretations).toContainEqual({ id: 'I13', version: 'I13-v1' });
  expect(replay.transfers).toContainEqual(expect.objectContaining({ interpretationId: 'I13',
    remainders: [expect.objectContaining({ before: 'unused', after: 'unused' })] }));
  // Recorded benchmark 165: Veneto 1's +Europa target needs a second pass.
  const repeated = runPoliticsReformScrutiny(fixture.runs[165].input);
  expect(repeated.status).toBe('complete');
  expect(repeated.seats).toHaveLength(588);
  expect(repeated.interpretations).toContainEqual({ id: 'I14', version: 'I14-v2' });
  expect(repeated.warnings).toContainEqual(expect.objectContaining({ code: 'I14_REPEATED_REMAINDER_PASS', severity: 'warning', chamber: 'camera' }));
  const matrix = repeated.chambers[0].cameraTerritorial!.colleges.matrices.find(m => m.contextId.endsWith('camera:territory:07/lists'))!;
  const europa = matrix.cells.find(c => c.targetId === 'list-4737a15e2c762fa0')!;
  expect(europa).toMatchObject({ integerSeats: 1, provisionalSeats: 3, seats: 3, remainderState: 'used' });
  expect(repeated.chambers[0].recovery!.remainders).toContainEqual(expect.objectContaining({
    collegeId: europa.areaId, subject: { kind: 'list', id: europa.targetId }, value: europa.decimalPart, state: 'used' }));
  expect(new Set(repeated.seats.flatMap(s => s.status === 'filled' ? [s.candidateId] : [])).size).toBe(588);
  expect(JSON.parse(JSON.stringify(repeated)).interpretations).toContainEqual({ id: 'I14', version: 'I14-v2' });
  // Recorded benchmark 31: Campania's M5S integer surplus in Senate N21.
  const senateReplay = runPoliticsReformScrutiny(fixture.runs[31].input);
  expect(senateReplay.status).toBe('complete');
  expect(senateReplay.seats).toHaveLength(588);
  expect(new Set(senateReplay.seats.flatMap(s => s.status === 'filled' ? [s.candidateId] : [])).size).toBe(588);
  expect(senateReplay.warnings).toContainEqual(expect.objectContaining({ code: 'I13_INTEGER_SEAT_REMOVAL', chamber: 'senato', severity: 'warning' }));
  expect(senateReplay.interpretations).toContainEqual({ id: 'I13', version: 'I13-v1' });
  expect(senateReplay.transfers).toContainEqual(expect.objectContaining({ chamber: 'senato', norm: 'N21', interpretationId: 'I13',
    remainders: [expect.objectContaining({ before: 'unused', after: 'unused' }), expect.anything()] }));
  // Former LG04 blockers: I16 completes all six with original territorial targets.
  for (const id of [42, 80, 598, 792, 49]) {
    const input = fixture.runs[id].input, result = runPoliticsReformScrutiny(input);
    expect(result.status).toBe('complete');
    expect(result.seats).toHaveLength(588);
    expect(new Set(result.seats.flatMap(s => s.status === 'filled' ? [s.candidateId] : [])).size).toBe(588);
    expect(result.interpretations).toContainEqual({ id: 'I16', version: 'I16-v1' });
    expect(result.warnings).toContainEqual(expect.objectContaining({ code: 'I16_REUSED_RECIPIENT_REMAINDER', severity: 'warning' }));
  }
  // Recorded benchmark 242: N22 uses I13's donor and I16's used recipient.
  const completedInput = fixture.runs[242], completed = runPoliticsReformScrutiny(completedInput.input);
  const senate = completed.chambers.find(c => c.chamber === 'senato')!;
  expect(completed.status).toBe('complete');
  expect(completed.seats).toHaveLength(588);
  expect(senate.regional).toMatchObject({ status: 'complete', path: 'bonus-with-cap' });
  expect(senate.senateTerritorial!.lists.status).toBe('complete');
  expect(senate.senateTerritorial!.colleges.status).toBe('complete');
  expect(completed.stages).toContainEqual({ norm: 'N22', chamber: 'senato', status: 'complete' });
  expect(completed.interpretations).toContainEqual({ id: 'I16', version: 'I16-v1' });
  const reused = completed.transfers.find(t => t.interpretationIds?.includes('I16'))!;
  expect(reused).toMatchObject({ norm: 'N22', interpretationId: 'I13', interpretationIds: ['I13', 'I16'],
    remainders: [{ before: 'unused', after: 'unused' }, { before: 'used', after: 'used' }] });
  expect(senate.recovery!.remainders).toContainEqual(expect.objectContaining({
    collegeId: reused.to!.collegeId, subject: { kind: 'list', id: reused.to!.listId }, value: reused.remainders[1].value, state: 'used' }));
  const presentation = createReformPresentation(defaults, snapshot, 1);
  presentation.add(completedInput.input, completed, completedInput.trace);
  const output = presentation.finish();
  const expectedRow = { Simulation: 242, Chamber: 'senato', Norm: 'N22', Status: 'complete',
    Reason: null, 'Affected IDs': null };
  expect(output.tables.find(t => t.name === 'Legal stages and blockers')!.rows).toContainEqual(expectedRow);
  const warning = output.warnings.find(w => w.code === 'I16_REUSED_RECIPIENT_REMAINDER')!;
  expect(warning).toMatchObject({ severity: 'warning', simulationId: 242 });
  expect(output.warnings.some(w => w.code === 'R26_N22_NON_COMPUTABLE')).toBe(false);
  expect(()=>createSimulationResultExport({scenario:defaults,exportedAt:'2026-10-21T00:00:00Z',result:{type:'result',status:'completed',...output,benchmark:{startedAt:'2026-10-21T00:00:00Z',elapsedMs:0,simulations:1,dataVersion:defaults.snapshotVersion,scrutinyAlgorithmId:defaults.algorithmId,seed:defaults.seed}}})).toThrow('Only latest');
}, 30000);
