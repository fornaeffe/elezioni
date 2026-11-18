import type { Candidate, Chamber, ChamberInput, Subject } from './input';
import type { SeatLedgerEntry, SeatLocation } from './result';
import type { SpecialWinnersResult } from './special-winners';
import type { CameraTaaAllocationResult } from './taa-allocation';

export interface SpecialUninominalResult {
  readonly chamber: Chamber;
  readonly simulationId: number;
  readonly status: 'complete' | 'partial';
  readonly seats: readonly SeatLedgerEntry[];
  /** This is the reused N09 communication, including all list links/affiliations. */
  readonly preliminary: SpecialWinnersResult;
}

/**
 * C93(2), C93-septies(1)(b),(3), S21(2),(2-bis)(c): proclaim the SAME N09
 * winners regardless of admission. No execution/draw dependency is accepted.
 * Availability may block an identity, never erase the linked seat entitlement;
 * initial recovery and any later supplementary elections are separate work.
 */
export function proclaimSpecialUninominalWinners(input: {
  readonly chamber: ChamberInput; readonly candidates: readonly Candidate[];
  readonly simulationId: number; readonly preliminary: SpecialWinnersResult;
}): SpecialUninominalResult {
  const { chamber, simulationId, preliminary } = input;
  const colleges = chamber.colleges.filter(c => c.kind === 'special-uninominal');
  if (preliminary.chamber !== chamber.chamber || preliminary.simulationId !== simulationId ||
      preliminary.winners.length !== colleges.length ||
      new Set(preliminary.winners.map(w => w.collegeId)).size !== colleges.length) {
    throw new RangeError('Reuse the complete preliminary special winners from this chamber and simulation');
  }
  const seats = preliminary.winners.map((w): SeatLedgerEntry => {
    const college = colleges.find(c => c.id === w.collegeId);
    const filed = chamber.specialCandidatures.find(c => c.collegeId === w.collegeId && c.candidateId === w.candidateId);
    const territory = chamber.territories.find(t => t.id === college?.territoryId);
    const total = chamber.specialVotes.find(r => r.collegeId === w.collegeId)?.votes.candidateTotals.find(c => c.candidateId === w.candidateId)?.votes;
    const person = input.candidates.find(c => c.id === w.candidateId);
    if (!filed || !territory || territory.id !== w.territoryId || territory.regionId !== w.regionId ||
        !person || total !== w.votes || filed.affiliationListId !== w.affiliationListId ||
        JSON.stringify([...filed.linkedListIds].sort()) !== JSON.stringify([...w.linkedListIds].sort())) {
      throw new RangeError('Preliminary winner does not match the validated special contest');
    }
    const list = chamber.lists.find(l => l.id === filed.linkedListIds[0])!;
    const subject: Subject = list.coalitionId === null ? { kind: 'list', id: list.id } : { kind: 'coalition', id: list.coalitionId };
    if (subject.kind !== w.subject.kind || subject.id !== w.subject.id) throw new RangeError('Preliminary winner subject does not match its links');
    // A coalition seat belongs to the linked subject. A declared personal
    // affiliation is metadata, not a proportional division among linked lists.
    const entitlement: SeatLocation = { chamber: chamber.chamber, territoryId: w.territoryId,
      collegeId: w.collegeId, subject, listId: subject.kind === 'list' ? subject.id : null };
    const seat = { seatId: JSON.stringify([chamber.chamber, 'special-uninominal', w.collegeId]),
      category: 'special-uninominal' as const, entitlement };
    return person.availability.kind === 'available'
      ? { ...seat, status: 'filled', electedAt: structuredClone(entitlement), candidateId: w.candidateId }
      : { ...seat, status: 'unresolved', reason: 'candidate-unavailable-requires-recovery' };
  });
  return structuredClone({ chamber: chamber.chamber, simulationId,
    status: seats.some(s => s.status !== 'filled') ? 'partial' : 'complete', seats, preliminary });
}

export interface SpecialSeatTotals {
  readonly chamber: Chamber;
  /** Entitlements, independently of missing preferences/availability. */
  readonly status: 'complete' | 'partial';
  readonly uninominalSeats: number;
  readonly taaProportionalSeats: number | null;
  readonly totalSeats: number | null;
  readonly subjects: readonly {
    readonly subject: Subject; readonly uninominalSeats: number;
    readonly taaProportionalSeats: number | null; readonly totalSeats: number | null;
  }[];
}

/** C2/C83(1)(f-bis), S1(4-bis)/S16-bis(1)(f-bis): special seats enter
 * the cap once and never enter ordinary territorial capacities. No cap decision
 * is made here. Unknown TAA entitlements remain null, not a fabricated zero.
 */
export function summarizeSpecialSeats(
  uninominal: SpecialUninominalResult, taa?: CameraTaaAllocationResult
): SpecialSeatTotals {
  if (taa && (uninominal.chamber !== 'camera' || taa.simulationId !== uninominal.simulationId)) {
    throw new RangeError('Special seat totals require the same chamber and simulation');
  }
  const known = uninominal.chamber === 'senato' || (taa !== undefined && taa.status !== 'non-computable');
  const proportional = taa && taa.status !== 'non-computable' ? taa.listEntitlements : [];
  const rows = new Map<string, { subject: Subject; uninominalSeats: number; taaProportionalSeats: number }>();
  const row = (s: Subject) => {
    const key = JSON.stringify([s.kind, s.id]);
    if (!rows.has(key)) rows.set(key, { subject: { ...s }, uninominalSeats: 0, taaProportionalSeats: 0 });
    return rows.get(key)!;
  };
  for (const seat of uninominal.seats) row(seat.entitlement.subject).uninominalSeats++;
  for (const list of proportional) row(list.subject).taaProportionalSeats += list.seats;
  const taaSeats = proportional.reduce((sum, l) => sum + l.seats, 0);
  return { chamber: uninominal.chamber, status: known ? 'complete' : 'partial',
    uninominalSeats: uninominal.seats.length, taaProportionalSeats: known ? taaSeats : null,
    totalSeats: known ? uninominal.seats.length + taaSeats : null,
    subjects: [...rows.entries()].sort(([a], [b]) => a < b ? -1 : 1).map(([, r]) => ({ ...r,
      taaProportionalSeats: known ? r.taaProportionalSeats : null,
      totalSeats: known ? r.uninominalSeats + r.taaProportionalSeats : null })) };
}
