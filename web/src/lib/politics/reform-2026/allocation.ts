import { addExact, compareExact, divideExact, exactFraction, floorExact, fractionalPart, multiplyExact, subtractExact } from './exact';
import type { DrawTrace, ExactFraction, ReformWarning, ScrutinyExecutionContext } from './result';
import { resolveTie, type TieContext, type TieCriterion, type TieResolution } from './ties';

export interface QuotaSubject {
  readonly id: string;
  /** Integer votes, or exact active cap indices; never a binary floating-point share. */
  readonly figure: ExactFraction;
  readonly tieValues: readonly ExactFraction[];
  /** Exclusion from residual seats only (e.g. a target already reached). */
  readonly remainderEligible?: boolean;
}
export interface QuotaAllocationInput {
  readonly subjects: readonly QuotaSubject[];
  readonly seats: number;
  readonly quotaMode: 'integer' | 'exact';
  /** Scope-specific criteria after the remainder: no implicit national/local votes. */
  readonly tieCriteria: readonly TieCriterion[];
  readonly context: TieContext;
}
export interface QuotaAssignment {
  readonly id: string;
  readonly quotient: ExactFraction | null;
  readonly integerSeats: number;
  readonly remainder: ExactFraction | null;
  readonly decimalPart: ExactFraction | null;
  readonly remainderState: 'unused' | 'used';
  readonly remainderSeats: 0 | 1;
  readonly seats: number;
}
interface AllocationTrace {
  readonly rawQuota: ExactFraction | null;
  readonly quota: ExactFraction | null;
  readonly initialSeatTotal: string | null;
  readonly assignments: readonly QuotaAssignment[];
  readonly resolutions: readonly TieResolution[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}
export type QuotaAllocationResult = AllocationTrace & (
  | { readonly status: 'complete' }
  | { readonly status: 'non-computable'; readonly reason: 'no-eligible-subjects' | 'zero-vote-total' | 'zero-quota' | 'initial-overallocation' | 'impossible-residual' }
);

/**
 * Integer quotas: C83(1)(f)-(i), C83-bis, S16-bis(1)(f), S17:
 * "non tiene conto dell'eventuale parte frazionaria del quoziente".
 * Exact quotas: C77(1)(c), C93-septies(2)(a)-(b), S16(1)(c) and
 * special candidate-only attribution retain the quota's fraction instead.
 * The stage selects its statutory secondary criteria and final draw rule.
 * I04 rejects undefined/impossible arithmetic rather than inventing shares.
 */
export function allocateByQuotaAndRemainders(
  input: QuotaAllocationInput,
  execution: ScrutinyExecutionContext
): QuotaAllocationResult {
  if (!Number.isSafeInteger(input.seats) || input.seats < 0) throw new RangeError('Invalid seat count');
  if (!['integer', 'exact'].includes(input.quotaMode)) throw new RangeError('Invalid quota mode');
  const ids = input.subjects.map((subject) => subject.id);
  if (ids.some((id) => !id) || new Set(ids).size !== ids.length) throw new RangeError('Subjects need unique nonempty IDs');
  const subjects = input.subjects.map((subject) => {
    if (subject.tieValues.length !== input.tieCriteria.length) throw new RangeError('Missing or extra tie values');
    return { ...subject, figure: exactFraction(subject.figure.numerator, subject.figure.denominator),
      tieValues: subject.tieValues.map((value) => exactFraction(value.numerator, value.denominator)) };
  });
  const assignments: QuotaAssignment[] = subjects.map(({ id }) => ({
    id, quotient: null, integerSeats: 0, remainder: null, decimalPart: null,
    remainderState: 'unused', remainderSeats: 0, seats: 0
  }));
  const trace: AllocationTrace = { rawQuota: null, quota: null, initialSeatTotal: null, assignments: [], resolutions: [], draws: [], warnings: [] };
  // I04: zero requested seats is a complete empty assignment, without division.
  if (input.seats === 0) return { ...trace, assignments, status: 'complete' };
  if (!subjects.length) return { ...trace, status: 'non-computable', reason: 'no-eligible-subjects' };
  const total = subjects.reduce((sum, subject) => addExact(sum, subject.figure), exactFraction(0));
  if (compareExact(total, exactFraction(0)) === 0) return { ...trace, status: 'non-computable', reason: 'zero-vote-total' };
  const rawQuota = divideExact(total, exactFraction(input.seats));
  const quota = input.quotaMode === 'integer' ? exactFraction(floorExact(rawQuota)) : rawQuota;
  const quotients = compareExact(quota, exactFraction(0)) === 0 ? [] : subjects.map((subject) => divideExact(subject.figure, quota));
  const integerParts = quotients.map((quotient) => BigInt(floorExact(quotient)));
  const assigned = integerParts.reduce((sum, whole) => sum + whole, 0n);
  const failure = (reason: Extract<QuotaAllocationResult, { status: 'non-computable' }>['reason']): QuotaAllocationResult =>
    ({ ...trace, rawQuota, quota, initialSeatTotal: quotients.length ? assigned.toString() : null,
      assignments: [], status: 'non-computable', reason });
  if (compareExact(quota, exactFraction(0)) === 0) return failure('zero-quota');
  // Check the entire exact total before converting any initial integer part to
  // number. Failure must not expose a partial, source-order-dependent allocation.
  if (assigned > BigInt(input.seats)) return failure('initial-overallocation');

  for (const [index, subject] of subjects.entries()) {
    const quotient = quotients[index];
    const whole = integerParts[index];
    const integerSeats = Number(whole);
    assignments[index] = { id: subject.id, quotient, integerSeats,
      remainder: subtractExact(subject.figure, multiplyExact(exactFraction(whole), quota)),
      decimalPart: fractionalPart(quotient), remainderState: 'unused', remainderSeats: 0, seats: integerSeats };
  }
  const residual = input.seats - Number(assigned);
  let eligible = subjects.filter((subject) => subject.remainderEligible !== false);
  // Each remainder is usable once. Check feasibility before consuming any draws.
  if (residual > eligible.length) return failure('impossible-residual');
  const resolutions: TieResolution[] = [];
  for (let seat = 0; seat < residual; seat += 1) {
    const resolution = resolveTie(eligible.map((subject) => ({ id: subject.id,
      values: [assignments[ids.indexOf(subject.id)].decimalPart!, ...subject.tieValues] })),
    [{ name: 'remainder', direction: 'descending' }, ...input.tieCriteria],
    { ...input.context, request: { ...input.context.request, contextId: `${input.context.request.contextId}/remainder-${seat + 1}` } }, execution);
    resolutions.push(resolution);
    const index = ids.indexOf(resolution.chosenId);
    assignments[index] = { ...assignments[index], remainderSeats: 1, remainderState: 'used', seats: assignments[index].seats + 1 };
    eligible = eligible.filter((subject) => subject.id !== resolution.chosenId);
  }
  return { status: 'complete', rawQuota, quota, initialSeatTotal: assigned.toString(), assignments, resolutions,
    draws: resolutions.flatMap((resolution) => resolution.draws),
    warnings: resolutions.flatMap((resolution) => resolution.warnings) };
}
