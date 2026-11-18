import { describe, expect, it } from 'vitest';
import receipt from '../../../../../test/fixtures/benchmarks/senate_sicilia_n21_review.json';
import { exactFraction as f } from './exact';
import { scrutinyMetadata } from './scrutiny-metadata';
import { completeScrutinyFixture } from './scrutiny.fixture';
import { allocateTerritorialMatrix, type TerritorialMatrixInput } from './territorial-matrix';
import { createSeededDrawContext } from './ties';
import type { ScrutinyExecutionContext } from './result';

const noDraw: ScrutinyExecutionContext = { draw() { throw Error('Unexpected draw'); } };
const input = receipt.matrixInput as TerritorialMatrixInput;
function matrix(targets: number[], votes: number[][], capacities: number[], norm: 'N21' | 'N22'): TerritorialMatrixInput {
  const ids = targets.map((_, i) => String.fromCharCode(65 + i));
  return { ...input, norm, contextId: 'senate-I14', simulationId: 14,
    targets: ids.map((id, j) => ({ id, seats: targets[j], votes: votes.reduce((n, row) => n + row[j], 0) })),
    areas: votes.map((row, i) => ({ id: `P${i}`, seats: capacities[i], entries: ids.map((id, j) => ({ id, votes: row[j], tieVotes: row[j] })) })) };
}
function complete(value: TerritorialMatrixInput, context = noDraw) {
  const r = allocateTerritorialMatrix(value, context);
  expect(r.status).toBe('complete');
  return r;
}

describe('I14-v2 Senate initial list remainder passes', () => {
  it('replays the frozen Sicilia 135 matrix with exact quotas, targets and one second pass', () => {
    const r = complete(input);
    expect(r.areas.map(a => [a.quota, a.finalSeats])).toEqual([[f(134276), 6], [f(143017), 6]]);
    expect(r.cells.map(c => [c.areaId, c.targetId, c.seats])).toEqual(receipt.proposal.matrix.cells.map(c => [c.areaId, c.targetId, c.seats]));
    expect(r.cells.find(c => c.areaId === input.areas[0].id && c.targetId === 'fi')).toMatchObject({
      integerSeats: 1, provisionalSeats: 3, seats: 3, remainderState: 'used', decimalPart: f(69539, 134276) });
    expect(r.warnings).toEqual([expect.objectContaining({ code: 'I14_REPEATED_REMAINDER_PASS', chamber: 'senato',
      interpretationId: 'I14', severity: 'warning', message: expect.stringContaining('Provisional I14-v2:') })]);
    expect(r.warnings[0].message).toContain('Reached regional targets excluded');
    expect(r.transfers).toEqual([]); expect(r.draws).toEqual([]);
    const permuted = { ...input, targets: [...input.targets].reverse(), areas: [...input.areas].reverse().map(a => ({ ...a, entries: [...a.entries].reverse() })) };
    expect(complete(permuted)).toEqual(r);
  });

  it.each(['N21', 'N22'] as const)('%s supports several passes and excludes newly reached regional targets', norm => {
    const r = complete(matrix([1, 3, 1, 1, 1], [[21, 19, 20, 20, 20], [0, 0, 100, 100, 100]], [4, 3], norm));
    expect(r.cells.filter(c => c.areaId === 'P0').map(c => c.seats)).toEqual([1, 3, 0, 0, 0]);
    expect(r.warnings.filter(w => w.interpretationId === 'I14')).toHaveLength(2);
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
  });

  it.each(['N21', 'N22'] as const)('%s resolves initial ties with college votes and never redraws a repeated pass', norm => {
    const value = matrix([2, 1, 1, 1, 1], [[20, 20, 20, 20, 20], [0, 0, 100, 100, 100]], [3, 3], norm);
    const seeded = () => createSeededDrawContext('senate-I14', 'legal');
    const r = complete(value, seeded());
    expect(r.draws).toHaveLength(1);
    expect(r.draws[0]).toMatchObject({ chamber: 'senato', norm, rule: 'statutory' });
    expect(r.resolutions[0].steps.map(s => s.criterion)).toEqual(['decimal-part', 'college-electoral-figure']);
    expect(r.cells.filter(c => c.areaId === 'P0').map(c => c.seats)).toEqual([2, 1, 0, 0, 0]);
    expect(complete({ ...value, areas: [...value.areas].reverse().map(a => ({ ...a, entries: [...a.entries].reverse() })) }, seeded())).toEqual(r);
  });

  it.each(['N21', 'N22'] as const)('%s consumes multiply-awarded donor remainders only after the last removal', norm => {
    const value = matrix([4, 5, 1, 1, 1, 1], [
      [20, 19, 16, 15, 15, 15], [20, 19, 16, 15, 15, 15], [20, 19, 16, 15, 15, 15], [0, 0, 100, 100, 100, 100]
    ], [3, 3, 3, 4], norm);
    const r = complete(value, { draw(request) { return { ...request, chosenId: request.alternativeIds[0], streamId: 'controlled' }; } });
    const removals = r.transfers.filter(t => t.interpretationId === 'I14');
    expect(removals).toHaveLength(2);
    expect(removals.map(t => t.remainders[0])).toMatchObject([{ before: 'used', after: 'used' }, { before: 'used', after: 'consumed' }]);
    const donor = r.cells.find(c => c.id === removals[0].fromCellId)!;
    expect(donor).toMatchObject({ integerSeats: 0, provisionalSeats: 2, seats: 0, remainderState: 'consumed' });
    expect(r.transfers.some(t => t.interpretationId === 'I13')).toBe(false);
  });

  it('retains an impossible residual for an empty eligible pool without consuming draws', () => {
    const value = matrix([2, 1, 1], [[40, 35, 0], [100, 100, 0]], [2, 2], 'N21');
    const absent = { ...value, areas: value.areas.map((a, i) => ({ ...a,
      entries: a.entries.map((e, j) => i === 0 && j === 2 ? { ...e, eligible: false } : e) })) };
    expect(allocateTerritorialMatrix(absent, noDraw)).toMatchObject({ status: 'non-computable', phase: 'remainders',
      reason: 'impossible-residual', draws: [], warnings: [] });
  });

  it('does not emit I14 or change a matrix when one initial pass suffices', () => {
    const value = matrix([3, 2], [[210, 90], [90, 110]], [3, 2], 'N21');
    const r = complete(value);
    expect(r.warnings).toEqual([]);
    expect({ ...r, norm: 'N22' }).toEqual(complete({ ...value, norm: 'N22' }));
  });

  it('records I14-v2 from Senate warnings regardless of chamber warning order', () => {
    const scrutiny = completeScrutinyFixture();
    const warnings = [...complete(input).warnings, { ...complete(input).warnings[0], chamber: 'camera' as const }];
    const metadata = scrutinyMetadata(scrutiny, [], [], warnings);
    expect(metadata.interpretations).toContainEqual({ id: 'I14', version: 'I14-v2' });
    expect(scrutinyMetadata(scrutiny, [], [], [...warnings].reverse()).interpretations).toEqual(metadata.interpretations);
  });
});
