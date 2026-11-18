import { expect, it } from 'vitest';
import { admissionFixture } from './admission.fixture';
import { allocateTerritorialMatrix, type TerritorialMatrixInput } from './territorial-matrix';
import { territorialEligibility } from './territorial-eligibility';
import type { ScrutinyExecutionContext } from './result';

const first: ScrutinyExecutionContext = { draw(r) { return { ...r, chosenId: r.alternativeIds[0], streamId: 'first' }; } };

it('uses filed territory coverage independently of votes, preferences and candidate exhaustion', () => {
  const { chamber } = admissionFixture('senato', { X: { A: 0, B: 100 }, Y: { B: 100 } }, { G: ['A', 'B'] });
  const eligible = territorialEligibility(chamber);
  expect(eligible.list('X', 'A')).toBe(true);
  expect(eligible.list('Y', 'A')).toBe(false);
  expect(eligible.subject('Y', { kind: 'coalition', id: 'G' })).toBe(true);
  expect(eligible.subject('missing', { kind: 'coalition', id: 'G' })).toBe(false);
});

function matrix(filed: boolean): TerritorialMatrixInput {
  return { chamber: 'camera', simulationId: 1, norm: 'N15', contextId: 'I12-coverage', lawReference: 'constructed I12 fixture',
    targets: [{ id: 'A', votes: 198, seats: 0 }, { id: 'B', votes: 0, seats: 1 }, { id: 'C', votes: 2, seats: 1 }],
    areas: ['X', 'Y'].map(id => ({ id, seats: 1, entries: [
      { id: 'A', votes: 99, tieVotes: 198 }, { id: 'B', votes: 0, tieVotes: 0, eligible: filed && id === 'X' },
      { id: 'C', votes: 1, tieVotes: 2 }
    ] })), correction: 'constituencies', initialTieFigure: 'national-electoral-figure' };
}

it('allows a filed zero-vote recipient while excluding an absent constituency from correction', () => {
  const result = allocateTerritorialMatrix(matrix(true), first);
  expect(result.status).toBe('complete');
  expect(result.cells.find(c => c.targetId === 'B' && c.areaId === 'X')).toMatchObject({ seats: 1, decimalPart: { numerator: '0', denominator: '1' } });
  expect(result.cells.find(c => c.targetId === 'B' && c.areaId === 'Y')).toMatchObject({ seats: 0, allocationEligible: false, decimalPart: null });
});

it('returns an explicit blocker when no filed constituency can receive a communicated entitlement', () => {
  const result = allocateTerritorialMatrix(matrix(false), first);
  expect(result).toMatchObject({ status: 'non-computable', reason: 'no-eligible-recipient-remainder' });
  expect(result.cells.filter(c => c.targetId === 'B').every(c => c.seats === 0)).toBe(true);
});
