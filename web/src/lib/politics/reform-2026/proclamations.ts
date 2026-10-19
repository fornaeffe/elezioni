import type { GovernabilityBonusResult } from './bonus';
import type { CameraCollegeResult } from './camera-territorial';
import { rankPreferenceCandidates } from './rankings';
import type { AggregatedElectoralFigures, Percentage } from './figures';
import type { Candidate, Chamber, ChamberInput, Subject } from './input';
import type { SeatLedgerEntry, SeatLocation } from './result';
import type { SenateCollegeResult } from './senate-territorial';
import type { SpecialUninominalResult } from './special-seats';
import type { CameraTaaAllocationResult } from './taa-allocation';

export type FilledSeat = Extract<SeatLedgerEntry, { readonly status: 'filled' }>;
export interface CandidateElection {
  readonly seat: FilledSeat;
  readonly role: 'bonus' | 'special-uninominal' | 'capolista' | 'preference';
  /** N25-N27 may elect an ordinary-slate candidate into a bonus entitlement. */
  readonly source?: 'ordinary-recovery';
  /** Personal affiliation, separate from the entitlement of a coalition seat. */
  readonly affiliationListId: string | null;
  readonly listPercentage: Percentage | null;
  readonly individualPercentage: Percentage | null;
}
export interface ProclamationInput {
  readonly simulationId: number;
  readonly chamber: ChamberInput;
  readonly candidates: readonly Candidate[];
  readonly figures: AggregatedElectoralFigures;
  readonly ordinary: CameraCollegeResult | SenateCollegeResult;
  readonly bonus: GovernabilityBonusResult;
  readonly special: SpecialUninominalResult;
  readonly taa?: CameraTaaAllocationResult;
}
export interface CandidateProclamations {
  readonly chamber: Chamber;
  readonly simulationId: number;
  /** Complete only for this initial batch, before precedence and recovery. */
  readonly status: 'complete' | 'partial' | 'non-computable';
  readonly blockers: readonly string[];
  readonly seats: readonly SeatLedgerEntry[];
  readonly elections: readonly CandidateElection[];
}
const key = (...parts: unknown[]) => JSON.stringify(parts);
const same = (a: Subject, b: Subject) => a.kind === b.kind && a.id === b.id;
const ordered = <T>(rows: readonly T[], id: (r: T) => string) => [...rows].sort((a, b) => id(a) < id(b) ? -1 : id(a) > id(b) ? 1 : 0);

/**
 * N23: C83-bis(1)(a), S17(1)(a): "proclamazione dei candidati della lista
 * circoscrizionale". C84(1), S17-bis(1), C93-septies(2)(c): "a partire dal
 * candidato capolista e successivamente seguendo la graduatoria".
 * Uses the validated filed slates and final territorial targets. Availability
 * never changes an entitlement or silently promotes another filed candidate:
 * all exhaustion, shortfalls and replacement operations are left to N25-N27.
 */
export function proclaimCandidates(input: ProclamationInput): CandidateProclamations {
  const { chamber: c, simulationId, ordinary, bonus, special, taa } = input;
  if (!Number.isSafeInteger(simulationId) || simulationId < 0 ||
      ordinary.chamber !== c.chamber || ordinary.simulationId !== simulationId ||
      JSON.stringify(ordinary.seatTable) !== JSON.stringify(c.seatTable) ||
      bonus.simulationId !== simulationId || !bonus.renewedChambers.includes(c.chamber) ||
      special.chamber !== c.chamber || special.simulationId !== simulationId ||
      (taa && (c.chamber !== 'camera' || taa.simulationId !== simulationId))) {
    throw new RangeError('Supply matching proclamation stages, chamber, seat table and simulation');
  }
  const taaColleges = c.colleges.filter(r => r.kind === 'taa-proportional');
  if (taaColleges.length !== (taa ? 1 : 0) || (taa &&
      (taaColleges[0].id !== taa.collegeId || taaColleges[0].territoryId !== taa.territoryId ||
        taaColleges[0].seatsWithoutBonus !== taa.proportionalEndowment ||
        taaColleges[0].seatsWithBonus !== taa.proportionalEndowment))) {
    throw new RangeError('Supply the matching Camera TAA communication');
  }
  const people = new Map(input.candidates.map(p => [p.id, p]));
  if (people.size !== input.candidates.length) throw new RangeError('Duplicate candidate identity');
  // C93-octies(2), S20-bis(01): special candidates cannot also be bonus
  // candidates. Check filed candidatures, even when one would not be elected.
  const specialPeople = new Set([...c.specialCandidatures.map(s => s.candidateId),
    ...c.ordinarySlates.filter(s => taaColleges.some(t => t.id === s.collegeId)).flatMap(s => s.candidateIds)]);
  if (c.bonusSlates.some(s => s.candidates.some(p => specialPeople.has(p.candidateId)))) {
    throw new RangeError('Incompatible special/bonus candidatures');
  }
  const blockers = [ordinary.status !== 'complete' ? 'ordinary-allocation-non-computable' : '',
    bonus.status === 'non-computable' ? 'bonus-non-computable' : '',
    taa?.status === 'non-computable' ? 'taa-allocation-non-computable' : ''].filter(Boolean);
  const base = { chamber: c.chamber, simulationId };
  if (blockers.length) return { ...base, status: 'non-computable', blockers, seats: [], elections: [] };
  if (ordinary.status !== 'complete' || bonus.status === 'non-computable' || taa?.status === 'non-computable') throw new Error('Unreachable prerequisite');
  const seats: SeatLedgerEntry[] = [];
  const elections: CandidateElection[] = [];
  function add(seat: Omit<FilledSeat, 'status' | 'candidateId' | 'electedAt'>, candidateId: string | undefined,
    role: CandidateElection['role'], affiliationListId: string | null, listPercentage: Percentage | null,
    individualPercentage: Percentage | null, missingReason: string): void {
    if (!candidateId) { seats.push({ ...seat, status: 'unresolved', reason: missingReason }); return; }
    const person = people.get(candidateId);
    if (!person) throw new RangeError('Unknown proclaimed candidate');
    if (person.availability.kind !== 'available') {
      seats.push({ ...seat, status: 'unresolved', reason: 'candidate-unavailable-requires-recovery' }); return;
    }
    const filled: FilledSeat = { ...seat, status: 'filled', electedAt: structuredClone(seat.entitlement), candidateId };
    seats.push(filled); elections.push({ seat: filled, role, affiliationListId, listPercentage, individualPercentage });
  }
  const award = bonus.status === 'awarded' ? bonus.awards.find(a => a.chamber === c.chamber) : undefined;
  if (bonus.status === 'awarded' && !award) throw new RangeError('Missing chamber bonus award');
  const ordinaryCapacity = c.colleges.filter(t => t.kind === 'ordinary')
    .reduce((n, t) => n + (award ? t.seatsWithBonus : t.seatsWithoutBonus), 0);
  if (ordinary.targets.reduce((n, t) => n + t.ordinarySeats, 0) !== ordinaryCapacity ||
      (taa && taa.listEntitlements.reduce((n, t) => n + t.seats, 0) !== taa.proportionalEndowment)) {
    throw new RangeError('List entitlements disagree with the selected seat endowments');
  }
  if (award) {
    const territories = ordered(c.territories.filter(t => t.bonusSeats > 0), t => t.id);
    if (territories.reduce((n, t) => n + t.bonusSeats, 0) !== award.seats) throw new RangeError('Bonus endowments disagree with the award');
    for (const t of territories) {
      const slates = c.bonusSlates.filter(s => s.territoryId === t.id && same(s.subject, award.subject));
      if (slates.length !== 1 || slates[0].candidates.length !== t.bonusSeats) throw new RangeError('Supply the filed winning bonus slate');
      const slate = slates[0];
      slate.candidates.forEach((p, i) => {
        const list = c.lists.find(l => l.id === p.affiliationListId);
        if (!list || (award.subject.kind === 'list' ? list.id !== award.subject.id : list.coalitionId !== award.subject.id)) throw new RangeError('Bonus affiliation is outside the winning subject');
        const entitlement: SeatLocation = { chamber: c.chamber, territoryId: t.id, collegeId: null,
          subject: award.subject, listId: award.subject.kind === 'list' ? award.subject.id : null };
        add({ seatId: key(c.chamber, 'bonus', t.id, i + 1), category: 'bonus', entitlement },
          p.candidateId, 'bonus', p.affiliationListId, null, null, 'bonus-shortfall-requires-recovery');
      });
    }
  }
  const targets = ordinary.targets.map(t => ({ ...t, category: 'ordinary' as const, count: t.ordinarySeats }));
  const proportional = taa ? taa.listEntitlements.map(t => ({ ...t, territoryId: taa.territoryId,
    collegeId: taa.collegeId, category: 'taa-proportional' as const, count: t.seats })) : [];
  const seen = new Set<string>();
  for (const t of ordered([...targets, ...proportional], t => key(t.category, t.collegeId, t.listId))) {
    const targetKey = key(t.collegeId, t.listId);
    const college = c.colleges.find(r => r.id === t.collegeId);
    const list = c.lists.find(r => r.id === t.listId);
    if (seen.has(targetKey) || !Number.isSafeInteger(t.count) || t.count < 0 || !list ||
        college?.kind !== t.category || college.territoryId !== t.territoryId ||
        (t.subject.kind === 'list' ? t.subject.id !== list.id : t.subject.id !== list.coalitionId)) {
      throw new RangeError('Invalid or duplicate list entitlement');
    }
    seen.add(targetKey);
    if (!t.count) continue;
    const slates = c.ordinarySlates.filter(s => s.collegeId === t.collegeId && s.listId === t.listId);
    const figures = (t.category === 'ordinary' ? input.figures.colleges : input.figures.taaProportional).filter(f => f.collegeId === t.collegeId);
    if (slates.length !== 1 || slates[0].candidateIds.length !== 7 || new Set(slates[0].candidateIds).size !== 7 ||
        slates[0].candidateIds.some(id => !people.has(id)) || figures.length !== 1) {
      throw new RangeError('Supply a validated seven-slot slate and its college figures');
    }
    if (figures[0].figures.lists.find(l => l.listId === t.listId)?.votes !== t.votes) {
      throw new RangeError('List entitlement disagrees with its electoral figure');
    }
    const ranking = rankPreferenceCandidates(slates[0], figures[0].figures);
    for (let i = 0; i < t.count; i++) {
      const ranked = ranking.status === 'complete' ? ranking.candidates[i - 1] : undefined;
      const id = i === 0 ? ranking.capolistaId : ranked?.candidateId;
      const entitlement: SeatLocation = { chamber: c.chamber, territoryId: t.territoryId, collegeId: t.collegeId,
        subject: t.subject, listId: t.listId };
      add({ seatId: key(c.chamber, t.category, t.collegeId, t.listId, i + 1), category: t.category, entitlement }, id,
        i === 0 ? 'capolista' : 'preference', t.listId, ranking.listPercentage, ranked?.percentage ?? null,
        ranking.status === 'missing-preferences' ? 'missing-preferences' : 'candidate-exhaustion-requires-recovery');
    }
  }
  const specialColleges = c.colleges.filter(r => r.kind === 'special-uninominal');
  if (special.seats.length !== specialColleges.length || new Set(special.seats.map(s => s.entitlement.collegeId)).size !== specialColleges.length) throw new RangeError('Supply every special seat exactly once');
  for (const seat of special.seats) {
    const college = specialColleges.find(r => r.id === seat.entitlement.collegeId);
    if (!college || college.territoryId !== seat.entitlement.territoryId || seat.entitlement.chamber !== c.chamber || seat.category !== 'special-uninominal') throw new RangeError('Invalid special seat communication');
    if (seat.status !== 'filled') { seats.push(seat); continue; }
    const filed = c.specialCandidatures.find(s => s.collegeId === college.id && s.candidateId === seat.candidateId);
    if (!filed || people.get(seat.candidateId)?.availability.kind !== 'available') throw new RangeError('Unknown or unavailable special winner');
    seats.push(seat); elections.push({ seat, role: 'special-uninominal', affiliationListId: filed.affiliationListId,
      listPercentage: null, individualPercentage: null });
  }
  if (new Set(seats.map(s => s.seatId)).size !== seats.length) throw new RangeError('Duplicate seat identity');
  return structuredClone({ ...base, status: seats.some(s => s.status !== 'filled') ? 'partial' : 'complete', blockers: [],
    seats: ordered(seats, s => s.seatId), elections: ordered(elections, e => e.seat.seatId) });
}
