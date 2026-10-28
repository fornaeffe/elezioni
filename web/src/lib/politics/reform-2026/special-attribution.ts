import { otherListsInCollege } from './other-list';
import { allocateByQuotaAndRemainders, type QuotaAllocationResult } from './allocation';
import { exactFraction } from './exact';
import { sumVoteCounts, type CollegeListFigure } from './figures';
import type { ChamberInput, ListVotes, SpecialVotes } from './input';
import type { DrawTrace, ReformWarning, ScrutinyExecutionContext } from './result';

export interface SpecialAttributionTrace {
  readonly candidateId: string;
  readonly linkedListIds: readonly string[];
  readonly onlyVotes: number;
  readonly method: 'no-additions' | 'single-list' | 'quota-remainders';
  readonly allocation: QuotaAllocationResult | null;
  readonly additions: readonly ListVotes[];
}
interface SpecialAttributionBase {
  readonly collegeId: string;
  readonly lawReference: string;
  readonly traces: readonly SpecialAttributionTrace[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}
export type SpecialAttributionResult = SpecialAttributionBase & (
  | (CollegeListFigure & { readonly status: 'complete'; readonly validVotes: number;
      readonly representation: SpecialVotes['kind']; readonly candidateTotals: SpecialVotes['candidateTotals'] })
  | { readonly status: 'non-computable'; readonly reason: 'zero-direct-support'; readonly affectedCandidateId: string }
);

/**
 * C92, C93(3-bis)(a), C93-septies(1)(c); S20(1)(c-ter), S21(2-bis)(a):
 * "in proporzione ai voti ottenuti da ciascuna nel collegio uninominale".
 * Exact q=directSupport/candidateOnly, then integer additions and remainders.
 * I02 reads S21's a-bis link reference together with c-ter's ballot rule.
 * Attribution is per candidate, per UNINOMINAL college, before aggregation.
 * Preliminary winners depend on candidateTotals, never on an attribution draw.
 */
export function attributeSpecialVotes(
  input: { readonly chamber: ChamberInput; readonly collegeId: string; readonly simulationId: number },
  execution: ScrutinyExecutionContext
): SpecialAttributionResult {
  const { chamber, collegeId, simulationId } = input;
  const college = chamber.colleges.find(c => c.id === collegeId);
  const territory = chamber.territories.find(t => t.id === college?.territoryId);
  const rows = chamber.specialVotes.filter(r => r.collegeId === collegeId);
  if (college?.kind !== 'special-uninominal' || !territory || rows.length !== 1) {
    throw new RangeError('Supply one special vote row for a special uninominal college');
  }
  const lawReference = chamber.chamber === 'senato' ? 'S21(2-bis)(a)' :
    territory.kind === 'vda' ? 'C93(3-bis)(a)' : 'C93-septies(1)(c)';
  const base: SpecialAttributionBase = { collegeId, lawReference, traces: [], draws: [], warnings: [] };
  const votes = rows[0].votes;
  if (votes.kind === 'attributed' ? ('directListVotes' in votes || 'candidateOnlyVotes' in votes) : 'listVotes' in votes) {
    throw new RangeError('Mixed special vote representations would cause double attribution');
  }
  const listRows = votes.kind === 'attributed' ? votes.listVotes : votes.directListVotes;
  const totals = new Map(listRows.map(r => [r.listId, r.votes]));
  const filed = chamber.specialCandidatures.filter(c => c.collegeId === collegeId).slice()
    .sort((a, b) => a.candidateId < b.candidateId ? -1 : 1);
  const linked = [...filed.flatMap(c => c.linkedListIds), ...otherListsInCollege(chamber, collegeId)];
  if (!filed.length || new Set(filed.map(c => c.candidateId)).size !== filed.length ||
      new Set(linked).size !== linked.length || linked.length !== listRows.length ||
      totals.size !== listRows.length || linked.some(id => !totals.has(id) || !chamber.lists.some(l => l.id === id))) {
    throw new RangeError('Supply exactly the linked contesting list figures, including zeros');
  }
  const candidateTotals = new Map(votes.candidateTotals.map(c => [c.candidateId, c.votes]));
  const only = votes.kind === 'components' ? new Map(votes.candidateOnlyVotes.map(c => [c.candidateId, c.votes])) : null;
  if (candidateTotals.size !== votes.candidateTotals.length || candidateTotals.size !== filed.length ||
      (votes.kind === 'components' && (only!.size !== votes.candidateOnlyVotes.length || only!.size !== filed.length))) {
    throw new RangeError('Supply all special candidate counts exactly once');
  }
  const validVotes = sumVoteCounts([...votes.candidateTotals.map(c => c.votes), ...listRows.filter(l => otherListsInCollege(chamber, collegeId).includes(l.listId)).map(l => l.votes)]);
  sumVoteCounts(listRows.map(r => r.votes));
  // Preflight the entire college, including a later zero-support group, before draws.
  const groups = filed.map(c => {
    if (!c.linkedListIds.length || !candidateTotals.has(c.candidateId) || (only && !only.has(c.candidateId))) {
      throw new RangeError('Missing special candidate links or counts');
    }
    const support = sumVoteCounts(c.linkedListIds.map(id => totals.get(id)!));
    const onlyVotes = only?.get(c.candidateId) ?? 0;
    if (sumVoteCounts([support, onlyVotes]) !== candidateTotals.get(c.candidateId)) {
      throw new RangeError('Special candidate total disagrees with linked list votes and components');
    }
    return { c, support, onlyVotes };
  });
  if (votes.kind === 'attributed') return { ...base, status: 'complete', validVotes,
    representation: votes.kind, listVotes: listRows.map(r => ({ ...r })),
    candidateTotals: votes.candidateTotals.map(r => ({ ...r })) };
  const failed = groups.find(g => g.onlyVotes > 0 && g.c.linkedListIds.length > 1 && g.support === 0);
  if (failed) return { ...base, status: 'non-computable', reason: 'zero-direct-support', affectedCandidateId: failed.c.candidateId };
  const traces: SpecialAttributionTrace[] = groups.map(({ c, onlyVotes }) => {
    const ids = c.linkedListIds.slice().sort();
    let allocation: QuotaAllocationResult | null = null;
    let additions: ListVotes[];
    if (onlyVotes === 0 || ids.length === 1) additions = ids.map(listId => ({ listId, votes: onlyVotes }));
    else {
      allocation = allocateByQuotaAndRemainders({ subjects: ids.map(id => ({ id,
        figure: exactFraction(totals.get(id)!), tieValues: [] })), seats: onlyVotes, quotaMode: 'exact', tieCriteria: [],
        context: { simulationId, request: { contextId: JSON.stringify(['N10', chamber.chamber, collegeId, c.candidateId]),
          chamber: chamber.chamber, norm: 'N10', rule: 'I03', lawReference } } }, execution);
      if (allocation.status !== 'complete') throw new Error(`Unexpected special attribution failure: ${allocation.reason}`);
      additions = allocation.assignments.map(a => ({ listId: a.id, votes: a.seats }));
    }
    for (const a of additions) totals.set(a.listId, sumVoteCounts([totals.get(a.listId)!, a.votes]));
    return { candidateId: c.candidateId, linkedListIds: ids, onlyVotes,
      method: onlyVotes === 0 ? 'no-additions' : ids.length === 1 ? 'single-list' : 'quota-remainders', allocation, additions };
  });
  return { ...base, status: 'complete', validVotes, representation: votes.kind,
    listVotes: listRows.map(r => ({ listId: r.listId, votes: totals.get(r.listId)! })),
    candidateTotals: votes.candidateTotals.map(r => ({ ...r })), traces,
    draws: traces.flatMap(t => t.allocation?.draws ?? []), warnings: traces.flatMap(t => t.allocation?.warnings ?? []) };
}
