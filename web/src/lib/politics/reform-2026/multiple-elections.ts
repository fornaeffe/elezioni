import type { CandidateElection, CandidateProclamations } from './proclamations';
import type { DrawTrace, ReformWarning, ScrutinyExecutionContext, SeatLedgerEntry } from './result';
import { resolveTie } from './ties';

export type MultipleElectionReason = 'single-election' | 'special-uninominal-priority' | 'bonus-priority' |
  'capolista-priority' | 'lowest-list-percentage' | 'highest-individual-percentage' | 'undefined-percentage';
export interface MultipleElectionDecision {
  readonly candidateId: string;
  readonly consideredSeatIds: readonly string[];
  readonly eligibleSeatIds: readonly string[];
  readonly retainedSeatId: string | null;
  readonly reason: MultipleElectionReason;
  readonly lawReference: string;
  readonly drawContextIds: readonly string[];
}
export interface MultipleElectionsResult {
  readonly chamber: CandidateProclamations['chamber'];
  readonly simulationId: number;
  readonly status: 'complete' | 'partial' | 'non-computable';
  readonly blockers: readonly string[];
  readonly seats: readonly SeatLedgerEntry[];
  readonly retained: readonly CandidateElection[];
  readonly released: readonly { readonly seatId: string; readonly candidateId: string; readonly retainedSeatId: string }[];
  readonly decisions: readonly MultipleElectionDecision[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}

/**
 * N24: C93-octies(3), S20-bis(01): "si intende eletto nel collegio uninominale".
 * C85(1-bis), imported by S17-bis(3): bonus wins precede ordinary wins.
 * C85(1): capolista precedes other positions; repeated capolista wins use the
 * LOWEST list percentage, other wins the HIGHEST individual percentage.
 * Equal percentages have no express final criterion: provisional warned I03.
 * No preference substitutes for an undefined percentage (I04). Released seats
 * keep their entitlement and identity; N25-N27 must fill them in a later stage.
 */
export function resolveMultipleElections(input: CandidateProclamations, execution: ScrutinyExecutionContext): MultipleElectionsResult {
  const { chamber, simulationId } = input;
  const seatMap = new Map(input.seats.map(s => [s.seatId, s]));
  const elected = new Map(input.elections.map(e => [e.seat.seatId, e]));
  if (!Number.isSafeInteger(simulationId) || simulationId < 0 ||
      seatMap.size !== input.seats.length || elected.size !== input.elections.length ||
      input.seats.some(s => s.entitlement.chamber !== chamber || (s.status === 'filled' && !elected.has(s.seatId))) ||
      input.elections.some(e => JSON.stringify(seatMap.get(e.seat.seatId)) !== JSON.stringify(e.seat) ||
        (e.role === 'bonus' || e.role === 'special-uninominal' ? e.role !== e.seat.category :
          !['ordinary', 'taa-proportional'].includes(e.seat.category)))) {
    throw new RangeError('Supply each filled seat and its proclamation metadata exactly once');
  }
  const groups = new Map<string, CandidateElection[]>();
  for (const election of input.elections) groups.set(election.seat.candidateId, [...(groups.get(election.seat.candidateId) ?? []), election]);
  // Preflight all groups before consuming any draw.
  for (const wins of groups.values()) {
    if (wins.filter(e => e.role === 'bonus').length > 1 || wins.filter(e => e.role === 'special-uninominal').length > 1 ||
        (wins.some(e => e.role === 'bonus') && wins.some(e => e.role === 'special-uninominal' || e.seat.category === 'taa-proportional'))) {
      throw new RangeError('Incompatible special/bonus multiple elections');
    }
  }
  const retained: CandidateElection[] = [], released: MultipleElectionsResult['released'][number][] = [];
  const decisions: MultipleElectionDecision[] = [], draws: DrawTrace[] = [], warnings: ReformWarning[] = [];
  for (const candidateId of [...groups.keys()].sort()) {
    const wins = groups.get(candidateId)!.sort((a, b) => a.seat.seatId < b.seat.seatId ? -1 : 1);
    let eligible = wins;
    let reason: MultipleElectionReason = 'single-election';
    let lawReference = chamber === 'camera' ? 'C85(1)' : 'S17-bis(3)/C85(1)';
    if (wins.length > 1) {
      const special = wins.filter(e => e.role === 'special-uninominal');
      const bonus = wins.filter(e => e.role === 'bonus');
      const heads = wins.filter(e => e.role === 'capolista');
      if (special.length) { eligible = special; reason = 'special-uninominal-priority'; lawReference = chamber === 'camera' ? 'C93-octies(3)' : 'S20-bis(01)'; }
      else if (bonus.length) { eligible = bonus; reason = 'bonus-priority'; lawReference = chamber === 'camera' ? 'C85(1-bis)' : 'S17-bis(3)/C85(1-bis)'; }
      else if (heads.length) { eligible = heads; reason = heads.length === 1 ? 'capolista-priority' : 'lowest-list-percentage'; }
      else reason = 'highest-individual-percentage';
    }
    let winner: CandidateElection | undefined = eligible.length === 1 ? eligible[0] : undefined;
    const contextIds: string[] = [];
    if (!winner) {
      const head = eligible[0].role === 'capolista';
      const percentages = eligible.map(e => head ? e.listPercentage : e.individualPercentage);
      if (percentages.some(p => p?.status !== 'defined')) reason = 'undefined-percentage';
      else {
        const alternatives = eligible.map((e, i) => {
          const percentage = percentages[i];
          if (percentage?.status !== 'defined') throw new Error('Undefined percentage');
          return { id: e.seat.seatId, values: [percentage.value] };
        });
        const resolved = resolveTie(alternatives,
          [{ name: head ? 'list-percentage' : 'individual-percentage', direction: head ? 'ascending' : 'descending' }],
          { simulationId, request: { contextId: JSON.stringify(['N24', chamber, simulationId, candidateId, eligible.map(e => e.seat.seatId)]), chamber,
            norm: 'N24', rule: 'I03', lawReference } }, execution);
        winner = eligible.find(e => e.seat.seatId === resolved.chosenId)!;
        draws.push(...resolved.draws); warnings.push(...resolved.warnings); contextIds.push(...resolved.draws.map(d => d.contextId));
      }
    }
    decisions.push({ candidateId, consideredSeatIds: wins.map(e => e.seat.seatId), eligibleSeatIds: eligible.map(e => e.seat.seatId),
      retainedSeatId: winner?.seat.seatId ?? null, reason, lawReference, drawContextIds: contextIds });
    if (winner) {
      retained.push(winner);
      released.push(...wins.filter(e => e !== winner).map(e => ({ seatId: e.seat.seatId, candidateId, retainedSeatId: winner.seat.seatId })));
    } else {
      for (const e of wins) seatMap.set(e.seat.seatId, { seatId: e.seat.seatId, category: e.seat.category,
        entitlement: e.seat.entitlement, status: 'unresolved', reason: 'multiple-election-undefined-percentage' });
    }
  }
  for (const r of released) {
    const seat = seatMap.get(r.seatId)!;
    seatMap.set(r.seatId, { seatId: seat.seatId, category: seat.category, entitlement: seat.entitlement,
      status: 'unfilled', reason: 'multiple-election-requires-recovery' });
  }
  const seats = [...seatMap.values()].sort((a, b) => a.seatId < b.seatId ? -1 : 1);
  return structuredClone({ chamber, simulationId, blockers: input.blockers, status: input.status === 'non-computable' ? 'non-computable' :
    seats.some(s => s.status !== 'filled') ? 'partial' : 'complete', seats, retained, released, decisions, draws, warnings });
}
