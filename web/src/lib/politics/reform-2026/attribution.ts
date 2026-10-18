import { allocateByQuotaAndRemainders, type QuotaAllocationResult } from './allocation';
import { exactFraction } from './exact';
import { sumVoteCounts, type CollegeListFigure } from './figures';
import type { Chamber, Coalition, ListVotes, OrdinaryVotes, Subject } from './input';
import type { DrawTrace, ReformWarning, ScrutinyExecutionContext } from './result';

export interface BonusAttributionInput {
  readonly chamber: Chamber;
  readonly simulationId: number;
  readonly collegeId: string;
  readonly coalitions: readonly Coalition[];
  /** Validated college input; direct rows include every contesting list. */
  readonly votes: OrdinaryVotes;
}
export interface BonusAttributionTrace {
  readonly subject: Subject;
  readonly onlyVotes: number;
  readonly method: 'no-additions' | 'single-list' | 'quota-remainders';
  readonly allocation: QuotaAllocationResult | null;
  readonly additions: readonly ListVotes[];
}
interface AttributionBase {
  readonly collegeId: string;
  readonly traces: readonly BonusAttributionTrace[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}
export type BonusAttributionResult = AttributionBase & (
  | (CollegeListFigure & { readonly status: 'complete'; readonly validVotes: number; readonly representation: OrdinaryVotes['kind'] })
  | { readonly status: 'non-computable'; readonly reason: 'zero-direct-support'; readonly affectedSubject: Subject }
);

/**
 * C58 third paragraph, C77(1)(c); S14(2), S16(1)(c):
 * "divide il totale dei voti validi ... per il numero dei voti espressi"
 * only for bonus candidates; "La parte intera ... i maggiori resti".
 * Exact q=directTotal/B, at this plurinominal college before any aggregation.
 * R26-02 final totals bypass this operation. I04's single-list and zero-only
 * cases avoid division; positive B without coalition support is non-computable.
 */
export function distributeBonusOnlyVotes(input: BonusAttributionInput, execution: ScrutinyExecutionContext): BonusAttributionResult {
  const { votes } = input;
  // Also protect direct JS callers from accidentally mixing the two variants.
  if (votes.kind === 'attributed' ? ('directListVotes' in votes || 'bonusOnlyVotes' in votes) : 'listVotes' in votes) {
    throw new RangeError('Mixed vote representations would cause double attribution');
  }
  const rows = votes.kind === 'attributed' ? votes.listVotes : votes.directListVotes;
  if (new Set(rows.map(row => row.listId)).size !== rows.length) throw new RangeError('Duplicate list votes');
  const directTotal = sumVoteCounts(rows.map(row => row.votes));
  if (votes.kind === 'attributed') return { status: 'complete', collegeId: input.collegeId,
    listVotes: rows.map(row => ({ ...row })), validVotes: directTotal, representation: votes.kind,
    traces: [], draws: [], warnings: [] };

  const validVotes = sumVoteCounts([directTotal, ...votes.bonusOnlyVotes.map(row => row.votes)]);
  const totals = new Map(rows.map(row => [row.listId, row.votes]));
  const traces: BonusAttributionTrace[] = [];
  const seen = new Set<string>();
  // Canonical subject order makes injected draw consumption independent of arrays.
  const subjectKey = (s: Subject) => JSON.stringify([s.kind, s.id]);
  const subjects = [...votes.bonusOnlyVotes].sort((a, b) => subjectKey(a.subject) < subjectKey(b.subject) ? -1 : 1);
  const groups = subjects.map(only => {
    const key = subjectKey(only.subject);
    if (seen.has(key)) throw new RangeError('Duplicate bonus-only subject');
    seen.add(key);
    const coalition = input.coalitions.find(c => c.id === only.subject.id);
    if (only.subject.kind === 'coalition' && !coalition) throw new RangeError('Unknown coalition');
    const ids = only.subject.kind === 'list' ? [only.subject.id] : coalition!.listIds.filter(id => totals.has(id));
    if (!ids.length || ids.some(id => !totals.has(id))) throw new RangeError('Bonus subject must contest this college');
    return { only, ids, support: sumVoteCounts(rows.filter(row => ids.includes(row.listId)).map(row => row.votes)) };
  });
  // Preflight every group before consuming draws or exposing partial attribution.
  const failed = groups.find(g => g.only.votes > 0 && g.ids.length > 1 && g.support === 0);
  if (failed) return { status: 'non-computable', collegeId: input.collegeId, reason: 'zero-direct-support',
    affectedSubject: { ...failed.only.subject }, traces: [], draws: [], warnings: [] };

  for (const { only, ids } of groups) {
    let allocation: QuotaAllocationResult | null = null;
    let additions: ListVotes[];
    if (only.votes === 0 || ids.length === 1) {
      additions = ids.map(id => ({ listId: id, votes: only.votes === 0 ? 0 : only.votes }));
    } else {
      allocation = allocateByQuotaAndRemainders({
        subjects: ids.slice().sort().map(id => ({ id, figure: exactFraction(rows.find(row => row.listId === id)!.votes), tieValues: [] })),
        seats: only.votes, quotaMode: 'exact', tieCriteria: [],
        context: { simulationId: input.simulationId, request: {
          contextId: JSON.stringify(['N04', input.chamber, input.collegeId, only.subject.kind, only.subject.id]),
          chamber: input.chamber, norm: 'N04', rule: 'I03',
          lawReference: input.chamber === 'camera' ? 'C77(1)(c)' : 'S16(1)(c)'
        } }
      }, execution);
      // For positive integer B and positive support the exact Hare allocation
      // always completes; no integer-quota fallback is permitted here.
      if (allocation.status !== 'complete') throw new Error(`Unexpected attribution failure: ${allocation.reason}`);
      additions = allocation.assignments.map(a => ({ listId: a.id, votes: a.seats }));
    }
    for (const addition of additions) totals.set(addition.listId, sumVoteCounts([totals.get(addition.listId)!, addition.votes]));
    traces.push({ subject: { ...only.subject }, onlyVotes: only.votes,
      method: only.votes === 0 ? 'no-additions' : ids.length === 1 ? 'single-list' : 'quota-remainders', allocation, additions });
  }
  return { status: 'complete', collegeId: input.collegeId, representation: votes.kind, validVotes,
    listVotes: rows.map(row => ({ listId: row.listId, votes: totals.get(row.listId)! })), traces,
    draws: traces.flatMap(t => t.allocation?.draws ?? []), warnings: traces.flatMap(t => t.allocation?.warnings ?? []) };
}
