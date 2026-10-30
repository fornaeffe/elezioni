import type { AdmissionResult } from './admission';
import { allocateByQuotaAndRemainders, type QuotaAllocationResult } from './allocation';
import { exactFraction } from './exact';
import type { AggregatedElectoralFigures } from './figures';
import type { Candidate, ChamberInput, Subject } from './input';
import { rankPreferenceCandidates, type PreferenceRanking } from './rankings';
import type { DrawTrace, ReformWarning, ScrutinyExecutionContext, SeatLedgerEntry, SeatLocation } from './result';

export interface TaaListEntitlement {
  readonly subject: Subject;
  readonly listId: string;
  readonly votes: number;
  readonly seats: number;
  readonly ranking: PreferenceRanking;
}
interface TaaAllocationBase {
  readonly chamber: 'camera';
  readonly simulationId: number;
  readonly collegeId: string;
  readonly territoryId: string;
  readonly proportionalEndowment: number;
  readonly subjectAllocation: QuotaAllocationResult | null;
  readonly coalitionAllocations: readonly { readonly coalitionId: string; readonly allocation: QuotaAllocationResult }[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}
export type CameraTaaAllocationResult = TaaAllocationBase & (
  | { readonly status: 'non-computable'; readonly reason: string }
  | {
      /** Proclamations remain provisional until R26-16/17 precedence/recovery. */
      readonly status: 'complete' | 'partial';
      readonly entitlementStatus: 'complete';
      readonly listEntitlements: readonly TaaListEntitlement[];
      readonly seats: readonly SeatLedgerEntry[];
      readonly localExceptionListIds: readonly string[];
    }
);

/**
 * C93-septies(2)(a)-(c),(3): exact constituency quota, then exact coalition
 * quota; remainder ties use local figures, then statutory draw. No truncation
 * or subtraction of uninominal winners from votes or proportional seats.
 * "a partire dal candidato capolista e successivamente seguendo la graduatoria".
 * The communication C83(1)(e-bis) and the LOCAL I01 exception control subjects.
 */
export function allocateCameraTaaSeats(
  input: { readonly chamber: ChamberInput; readonly candidates: readonly Candidate[];
    readonly figures: AggregatedElectoralFigures; readonly admission: AdmissionResult;
    readonly collegeId: string; readonly simulationId: number },
  execution: ScrutinyExecutionContext
): CameraTaaAllocationResult {
  const { chamber, figures, admission, collegeId, simulationId } = input;
  const college = chamber.colleges.find(c => c.id === collegeId);
  const territory = chamber.territories.find(t => t.id === college?.territoryId);
  if (chamber.chamber !== 'camera' || admission.chamber !== 'camera' ||
      college?.kind !== 'taa-proportional' || territory?.kind !== 'taa' ||
      college.seatsWithBonus !== college.seatsWithoutBonus) {
    throw new RangeError('Camera TAA allocation requires its invariant proportional endowment and Camera admission');
  }
  const local = figures.taaProportional.find(r => r.collegeId === collegeId)?.figures;
  if (!local) throw new RangeError('Supply the derived TAA constituency figures');
  const base: TaaAllocationBase = { chamber: 'camera', simulationId, collegeId, territoryId: territory.id,
    proportionalEndowment: college.seatsWithoutBonus, subjectAllocation: null, coalitionAllocations: [], draws: [], warnings: [] };
  if (admission.status !== 'complete') return { ...base, status: 'non-computable', reason: 'admission-non-computable' };
  const filed = chamber.ordinarySlates.filter(s => s.collegeId === collegeId);
  const people = new Map(input.candidates.map(c => [c.id, c]));
  // Ranks and references are preflighted even for a list receiving no seat.
  const ranks = new Map(filed.map(s => {
    if (s.candidateIds.length !== 7 || s.candidateIds.some(id => !people.has(id))) {
      throw new RangeError('Supply validated seven-slot TAA slates and candidate references');
    }
    return [s.listId, rankPreferenceCandidates(s, local)];
  }));
  if (ranks.size !== filed.length) throw new RangeError('Duplicate TAA slate');
  const localExceptions = admission.taaLocalExceptions.filter(e => e.territoryId === territory.id).map(e => e.listId);
  const subjectKey = (s: Subject) => JSON.stringify([s.kind, s.id]);
  const subjects = new Map(admission.admittedSubjects.map(s => [subjectKey(s), s]));
  // I01: preserve the anomalous statutory (2-bis) reference; a failed-coalition
  // minority is standalone LOCALLY, without changing its national admission.
  for (const id of localExceptions) subjects.set(subjectKey({ kind: 'list', id }), { kind: 'list', id });
  const listVotes = (id: string) => {
    const row = local.lists.find(l => l.listId === id);
    if (!row) throw new RangeError('Missing TAA list figure');
    return row.votes;
  };
  const prepared = [...subjects.values()].sort((a, b) => subjectKey(a) < subjectKey(b) ? -1 : 1).map(subject => {
    const coalition = chamber.coalitions.find(c => c.id === subject.id);
    if (subject.kind === 'coalition' && !coalition) throw new RangeError('Unknown admitted coalition');
    // C93-septies(2)(b): "tra le liste coalizzate", read with (2)'s admission
    // communication, implicitly means admitted members under C83(1)(e)(2-bis)/
    // (2-ter). External legal review, adopted by Luca on 30 October 2026.
    // (2)(a) still uses the whole locally filed coalition's electoral figure;
    // only the internal quota/recipients exclude unadmitted member lists.
    const ids = subject.kind === 'list' ? [subject.id] : coalition!.listIds;
    const filedMembers = ids.filter(id => ranks.has(id)).slice().sort();
    const members = subject.kind === 'list' ? filedMembers :
      filedMembers.filter(id => admission.lists.some(l => l.listId === id && l.admitted));
    const missing = ids.filter(id => !ranks.has(id) && listVotes(id) > 0);
    if (missing.length) throw new RangeError('Positive TAA votes require a filed proportional slate');
    return { subject: { ...subject }, id: subjectKey(subject), members, filedMembers,
      votes: filedMembers.reduce((sum, id) => sum + BigInt(listVotes(id)), 0n) };
  }).filter(s => s.filedMembers.length > 0);
  if (new Set(prepared.flatMap(s => s.filedMembers)).size !== prepared.flatMap(s => s.filedMembers).length) {
    throw new RangeError('TAA subjects overlap and would count a list twice');
  }
  const allocate = (rows: readonly { id: string; votes: bigint }[], seats: number, suffix: string, lawReference: string) =>
    allocateByQuotaAndRemainders({ subjects: rows.map(r => ({ id: r.id, figure: exactFraction(r.votes), tieValues: [exactFraction(r.votes)] })),
      seats, quotaMode: 'exact', tieCriteria: [{ name: 'local-electoral-figure', direction: 'descending' }],
      context: { simulationId, request: { contextId: JSON.stringify(['N11', 'camera', collegeId, suffix]),
        chamber: 'camera', norm: 'N11', rule: 'statutory', lawReference } } }, execution);
  const subjectAllocation = allocate(prepared, college.seatsWithoutBonus, 'subjects', 'C93-septies(2)(a)');
  if (subjectAllocation.status !== 'complete') return { ...base, subjectAllocation,
    status: 'non-computable', reason: subjectAllocation.reason };
  const coalitionAllocations: { coalitionId: string; allocation: QuotaAllocationResult }[] = [];
  const entitlements: TaaListEntitlement[] = [];
  for (const s of prepared) {
    const seats = subjectAllocation.assignments.find(a => a.id === s.id)!.seats;
    if (s.subject.kind === 'list') {
      const listId = s.members[0];
      entitlements.push({ subject: s.subject, listId, votes: listVotes(listId), seats, ranking: ranks.get(listId)! });
    } else {
      const allocation = allocate(s.members.map(id => ({ id, votes: BigInt(listVotes(id)) })), seats,
        `coalition:${s.subject.id}`, 'C93-septies(2)(b)');
      coalitionAllocations.push({ coalitionId: s.subject.id, allocation });
      if (allocation.status !== 'complete') return { ...base, subjectAllocation, coalitionAllocations,
        draws: [...subjectAllocation.draws, ...coalitionAllocations.flatMap(a => a.allocation.draws)],
        warnings: [...subjectAllocation.warnings, ...coalitionAllocations.flatMap(a => a.allocation.warnings)],
        status: 'non-computable', reason: allocation.reason };
      entitlements.push(...allocation.assignments.map(a => ({ subject: s.subject, listId: a.id,
        votes: listVotes(a.id), seats: a.seats, ranking: ranks.get(a.id)! })));
    }
  }
  const seats: SeatLedgerEntry[] = entitlements.flatMap(e => {
    const ranking = e.ranking;
    const order = [ranking.capolistaId, ...(ranking.status === 'complete' ? ranking.candidates.map(c => c.candidateId) : [])];
    return Array.from({ length: e.seats }, (_, i): SeatLedgerEntry => {
      const entitlement: SeatLocation = { chamber: 'camera', territoryId: territory.id, collegeId, subject: { ...e.subject }, listId: e.listId };
      const seat = { seatId: JSON.stringify(['camera', 'taa-proportional', collegeId, e.listId, i + 1]),
        category: 'taa-proportional' as const, entitlement };
      const id = order[i];
      if (!id) return { ...seat, status: 'unresolved', reason: ranking.status === 'missing-preferences' ? 'missing-preferences' : 'candidate-exhaustion-requires-recovery' };
      if (people.get(id)!.availability.kind !== 'available') return { ...seat, status: 'unresolved', reason: 'candidate-unavailable-requires-recovery' };
      return { ...seat, status: 'filled', electedAt: structuredClone(entitlement), candidateId: id };
    });
  });
  return structuredClone({ ...base, status: seats.some(s => s.status !== 'filled') ? 'partial' : 'complete',
    entitlementStatus: 'complete', subjectAllocation, coalitionAllocations, listEntitlements: entitlements, seats,
    localExceptionListIds: [...new Set(localExceptions)].sort(),
    draws: [...subjectAllocation.draws, ...coalitionAllocations.flatMap(a => a.allocation.draws)],
    warnings: [...subjectAllocation.warnings, ...coalitionAllocations.flatMap(a => a.allocation.warnings)] });
}
