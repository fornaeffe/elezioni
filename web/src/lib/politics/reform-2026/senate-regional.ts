import type { AdmissionResult } from './admission';
import { allocateByQuotaAndRemainders, type QuotaAllocationResult } from './allocation';
import type { GovernabilityBonusResult } from './bonus';
import { compareExact, exactFraction } from './exact';
import { sumVoteCounts as sum, type AggregatedElectoralFigures } from './figures';
import type { ChamberInput, SeatTableSource, Subject } from './input';
import type { DrawTrace, ExactFraction, ReformWarning, ScrutinyExecutionContext } from './result';
import type { SpecialSeatTotals } from './special-seats';
import { resolveTie, type TieResolution } from './ties';

const key = (s: Subject) => JSON.stringify([s.kind, s.id]);
const ordered = <T extends { readonly regionId: string }>(rows: readonly T[]) =>
  [...rows].sort((a, b) => a.regionId < b.regionId ? -1 : a.regionId > b.regionId ? 1 : 0);

export interface SenateRegionalInput {
  readonly chamber: ChamberInput;
  readonly figures: AggregatedElectoralFigures;
  readonly admission: AdmissionResult;
  readonly bonus: GovernabilityBonusResult;
  readonly specialSeats: SpecialSeatTotals;
  readonly simulationId: number;
}
export interface SenateRegionAllocation {
  readonly regionId: string;
  readonly ordinarySeats: number;
  /** All regional list votes, including excluded lists; denominator for N20. */
  readonly validVotes: number;
  readonly subjects: readonly { readonly subject: Subject; readonly votes: number }[];
  readonly allocation: QuotaAllocationResult;
}
export interface SenateRegionTarget {
  readonly regionId: string;
  readonly subject: Subject;
  readonly votes: number;
  readonly ordinarySeats: number;
}
export interface SenateSubjectTotal {
  readonly subject: Subject;
  readonly votes: number;
  readonly ordinarySeats: number;
}
export interface SenateUsedRemainder {
  readonly regionId: string;
  /** Raw vote remainder, NOT the decimal part of the regional quotient. */
  readonly remainder: ExactFraction;
  /** Equal values share a rank. I03 resolves their order only when removing. */
  readonly rank: number;
}
export interface SenateCapCheck {
  readonly subject: Subject;
  readonly provisionalOrdinarySeats: number;
  readonly specialSeats: number;
  readonly bonusSeats: 35;
  readonly provisionalDomesticSeats: number;
  readonly exceedsCap: boolean;
}
type RemainderState = 'unused' | 'used' | 'consumed';
export interface SenateRemainderState {
  readonly regionId: string;
  readonly subject: Subject;
  readonly remainder: ExactFraction | null;
  readonly initialState: 'unused' | 'used';
  readonly state: RemainderState;
}
export interface SenateCapMovement {
  readonly sequence: number;
  readonly regionId: string;
  readonly removal: {
    readonly route: 'used-remainder' | 'regional-percentage';
    /** Zero for remainder removals; percentage passes start at one. */
    readonly pass: number;
    readonly rankValue: ExactFraction;
    readonly seatsBefore: number;
    readonly seatsAfter: number;
    readonly remainderBefore: RemainderState;
    readonly remainderAfter: RemainderState;
    readonly resolution: TieResolution;
  };
  readonly recipient: {
    readonly subject: Subject;
    readonly route: 'unused-remainder' | 'regional-votes';
    readonly seatsBefore: number;
    readonly seatsAfter: number;
    readonly remainderBefore: RemainderState;
    readonly remainderAfter: RemainderState;
    readonly resolution: TieResolution;
  };
}
interface CapBase {
  readonly check: SenateCapCheck;
  readonly majorityOrdinaryTarget: number;
  readonly ranking: readonly SenateUsedRemainder[];
  readonly movements: readonly SenateCapMovement[];
  readonly remainders: readonly SenateRemainderState[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
  readonly interpretations: readonly { readonly id: 'I06'; readonly version: 'I06-v1' }[];
}
export type SenateCapResult = CapBase & (
  | { readonly status: 'complete'; readonly targets: readonly SenateRegionTarget[] }
  | { readonly status: 'non-computable'; readonly reason: string; readonly affectedRegionId: string | null }
);
interface RegionalBase {
  readonly chamber: 'senato';
  readonly simulationId: number;
  readonly seatTable: SeatTableSource;
  readonly ordinarySeats: number | null;
  readonly regions: readonly SenateRegionAllocation[];
  readonly provisionalTotals: readonly SenateSubjectTotal[] | null;
  readonly cap: SenateCapResult | null;
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}
export type SenateRegionalAllocationResult = RegionalBase & (
  | { readonly status: 'non-computable'; readonly stage: 'prerequisites' | 'provisional' | 'cap'; readonly reason: string; readonly affectedRegionId: string | null }
  | { readonly status: 'complete'; readonly ordinarySeats: number;
      readonly path: 'no-bonus' | 'bonus-without-cap' | 'bonus-with-cap';
      readonly targets: readonly SenateRegionTarget[];
      readonly subjectTotals: readonly SenateSubjectTotal[];
      /** Ownership stays with the subject, never an invented split among lists. */
      readonly bonus: { readonly subject: Subject; readonly seats: 35 } | null }
);

function targets(regions: readonly SenateRegionAllocation[]): SenateRegionTarget[] {
  return regions.flatMap(r => r.subjects.map(s => ({ regionId: r.regionId, ...s,
    ordinarySeats: r.allocation.assignments.find(a => a.id === key(s.subject))!.seats })));
}

function totals(rows: readonly SenateRegionTarget[]): SenateSubjectTotal[] {
  const subjects = [...new Map(rows.map(r => [key(r.subject), r.subject])).entries()].sort(([a], [b]) => a < b ? -1 : 1);
  return subjects.map(([id, subject]) => ({ subject,
    votes: sum(rows.filter(r => key(r.subject) === id).map(r => r.votes)),
    ordinarySeats: sum(rows.filter(r => key(r.subject) === id).map(r => r.ordinarySeats)) }));
}

/** S16-bis(1)(f), last sentence: "graduatoria nazionale crescente i resti ...
 * hanno dato luogo all'attribuzione di seggi". Regional quotas differ, so
 * compare raw vote remainders, not normalized decimal parts. Equal ranks stay
 * explicit until needed; serialization order never settles a legal tie.
 */
function usedRanking(regions: readonly SenateRegionAllocation[], subject: Subject): SenateUsedRemainder[] {
  const rows = regions.flatMap(r => r.allocation.assignments.filter(a =>
    a.id === key(subject) && a.remainderState === 'used').map(a => ({ regionId: r.regionId, remainder: a.remainder! })));
  rows.sort((a, b) => compareExact(a.remainder, b.remainder) || (a.regionId < b.regionId ? -1 : 1));
  let rank = 0;
  return rows.map((r, i) => {
    if (!i || compareExact(r.remainder, rows[i - 1].remainder) !== 0) rank = i + 1;
    return { ...r, rank };
  });
}

/** N20, S16-bis(1)(f-bis)-(f-quater),(1-ter).
 * "superiore a centotredici"; retain 35 bonus seats, include special seats
 * once. Ordinary plus special target: 78. Requires complete N19 allocations;
 * does not re-emit their upstream events or allocate coalition lists (N21).
 */
export function applySenateCap(input: {
  readonly regions: readonly SenateRegionAllocation[];
  readonly subject: Subject;
  readonly specialSeats: number;
  readonly simulationId: number;
}, execution: ScrutinyExecutionContext): SenateCapResult {
  const { subject, simulationId, specialSeats } = input;
  if (!Number.isSafeInteger(simulationId) || simulationId < 0 || !Number.isSafeInteger(specialSeats) || specialSeats < 0 || specialSeats > 7) {
    throw new RangeError('Invalid Senate cap simulation or special entitlement');
  }
  const regions = ordered(input.regions).map(r => ({ ...r,
    subjects: [...r.subjects].sort((a, b) => key(a.subject) < key(b.subject) ? -1 : 1) }));
  if (!regions.length || new Set(regions.map(r => r.regionId)).size !== regions.length) throw new RangeError('Supply unique Senate regions');
  const expected = regions[0]?.subjects.map(s => key(s.subject)).sort() ?? [];
  for (const r of regions) {
    const ids = r.subjects.map(s => key(s.subject));
    const assignments = r.allocation.assignments;
    if (r.allocation.status !== 'complete' || !ids.includes(key(subject)) || new Set(ids).size !== ids.length ||
        JSON.stringify([...ids].sort()) !== JSON.stringify(expected) || assignments.length !== ids.length ||
        new Set(assignments.map(a => a.id)).size !== ids.length || assignments.some(a => !ids.includes(a.id)) ||
        sum(assignments.map(a => a.seats)) !== r.ordinarySeats || sum(r.subjects.map(s => s.votes)) > sum([r.validVotes]) ||
        assignments.some(a => a.remainderSeats !== (a.remainderState === 'used' ? 1 : 0) || a.seats !== a.integerSeats + a.remainderSeats ||
          (r.ordinarySeats > 0 && a.remainder === null))) {
      throw new RangeError('Supply complete, consistent Senate regional allocations');
    }
  }
  const final = targets(regions);
  const majority = (regionId: string) => final.find(t => t.regionId === regionId && key(t.subject) === key(subject))!;
  const provisionalOrdinarySeats = sum(final.filter(t => key(t.subject) === key(subject)).map(t => t.ordinarySeats));
  const provisionalDomesticSeats = provisionalOrdinarySeats + specialSeats + 35;
  const check: SenateCapCheck = { subject, provisionalOrdinarySeats, specialSeats, bonusSeats: 35,
    provisionalDomesticSeats, exceedsCap: provisionalDomesticSeats > 113 };
  const ranking = usedRanking(regions, subject);
  const states = regions.flatMap(r => r.subjects.map(s => {
    const a = r.allocation.assignments.find(a => a.id === key(s.subject))!;
    return { regionId: r.regionId, subject: s.subject, remainder: a.remainder, initialState: a.remainderState, state: a.remainderState as RemainderState };
  }));
  const state = (regionId: string, s: Subject) => states.find(r => r.regionId === regionId && key(r.subject) === key(s))!;
  const movements: SenateCapMovement[] = [], draws: DrawTrace[] = [], warnings: ReformWarning[] = [];
  const base = (): CapBase => ({ check, majorityOrdinaryTarget: 78 - specialSeats, ranking, movements, remainders: states,
    draws, warnings, interpretations: check.exceedsCap ? [{ id: 'I06', version: 'I06-v1' }] : [] });
  const blocked = (reason: string, affectedRegionId: string | null = null): SenateCapResult =>
    structuredClone({ ...base(), status: 'non-computable', reason, affectedRegionId });
  const complete = (): SenateCapResult => structuredClone({ ...base(), status: 'complete', targets: final });
  if (!check.exceedsCap) return complete();
  if (expected.length < 2) return blocked('no-eligible-recipient');
  const choose = (rows: readonly { id: string; value: ExactFraction }[], criterion: string, direction: 'ascending' | 'descending', suffix: string) => {
    const resolution = resolveTie(rows.map(r => ({ id: r.id, values: [r.value] })), [{ name: criterion, direction }],
      { simulationId, request: { contextId: JSON.stringify(['N20', 'senato', simulationId, suffix]),
        chamber: 'senato', norm: 'N20', rule: 'I03', lawReference: 'S16-bis(1-ter); I06-v1' } }, execution);
    draws.push(...resolution.draws); warnings.push(...resolution.warnings);
    return resolution;
  };
  let remaining = provisionalOrdinarySeats + specialSeats - 78;
  const removedByRemainder = new Set<string>();
  const move = (regionId: string, route: SenateCapMovement['removal']['route'], pass: number, rankValue: ExactFraction, resolution: TieResolution) => {
    const donor = majority(regionId), donorState = state(regionId, subject);
    const minority = final.filter(t => t.regionId === regionId && key(t.subject) !== key(subject));
    const unused = minority.filter(t => state(regionId, t.subject).state === 'unused' && state(regionId, t.subject).remainder !== null);
    // S16-bis(1-ter)(b): "maggiore resto non utilizzato, ovvero ... maggiore
    // cifra elettorale regionale". No express secondary recipient criterion:
    // I03, rather than silently borrowing the initial regional vote tie-break.
    const candidates = unused.length ? unused : minority;
    const recipientResolution = choose(candidates.map(t => ({ id: key(t.subject), value: unused.length
      ? state(regionId, t.subject).remainder! : exactFraction(t.votes) })),
      unused.length ? 'unused-remainder' : 'regional-electoral-figure', 'descending', `recipient-${movements.length + 1}:${regionId}`);
    const recipient = candidates.find(t => key(t.subject) === recipientResolution.chosenId)!;
    const recipientState = state(regionId, recipient.subject);
    const removal = { route, pass, rankValue, seatsBefore: donor.ordinarySeats, seatsAfter: donor.ordinarySeats - 1,
      remainderBefore: donorState.state, remainderAfter: route === 'used-remainder' ? 'consumed' as const : donorState.state, resolution };
    const reassignment = { subject: recipient.subject, route: unused.length ? 'unused-remainder' as const : 'regional-votes' as const,
      seatsBefore: recipient.ordinarySeats, seatsAfter: recipient.ordinarySeats + 1, remainderBefore: recipientState.state,
      remainderAfter: unused.length ? 'used' as const : recipientState.state, resolution: recipientResolution };
    // Paired transfers conserve the region after every recorded movement.
    final[final.indexOf(donor)] = { ...donor, ordinarySeats: removal.seatsAfter };
    final[final.indexOf(recipient)] = { ...recipient, ordinarySeats: reassignment.seatsAfter };
    donorState.state = removal.remainderAfter; recipientState.state = reassignment.remainderAfter;
    movements.push({ sequence: movements.length + 1, regionId, removal, recipient: reassignment });
    remaining--;
  };
  // S16-bis(1-ter)(a), second sentence: used remainders first, once each.
  // Last-seat protection is prescribed for the percentage passes, not here.
  while (remaining > 0) {
    const available = ranking.filter(r => !removedByRemainder.has(r.regionId));
    if (!available.length) break;
    const resolution = choose(available.map(r => ({ id: r.regionId, value: r.remainder })), 'used-remainder', 'ascending', `removal-${movements.length + 1}`);
    const r = available.find(r => r.regionId === resolution.chosenId)!;
    move(r.regionId, 'used-remainder', 0, r.remainder, resolution);
    removedByRemainder.add(r.regionId);
  }
  // S16-bis(1-ter)(a), six sentences: (1) excess, (2) used-remainder removal,
  // (3) "minore cifra elettorale regionale percentuale", (4) "uno per regione",
  // (5) first-pass exclusions including "già ... sottratto ... secondo periodo",
  // (6) "ripete le operazioni ... terzo e quarto" (NOT fifth), excluding regions
  // where "anche a seguito ... risulti attribuito un solo seggio". Thus later
  // passes readmit remainder-phase regions, protecting the CURRENT last seat.
  for (let pass = 1; remaining > 0; pass++) {
    const eligible = regions.filter(r => majority(r.regionId).ordinarySeats > 1 && (pass > 1 || !removedByRemainder.has(r.regionId)));
    if (!eligible.length) {
      if (pass === 1 && removedByRemainder.size) continue;
      return blocked('no-lawful-removal-progress');
    }
    if (eligible.some(r => r.validVotes === 0)) return blocked('zero-regional-valid-votes', eligible.find(r => r.validVotes === 0)!.regionId);
    let available = eligible.map(r => ({ id: r.regionId, value: exactFraction(BigInt(majority(r.regionId).votes) * 100n, r.validVotes) }));
    while (remaining > 0 && available.length) {
      const resolution = choose(available, 'regional-percentage', 'ascending', `pass-${pass}:removal-${movements.length + 1}`);
      const r = available.find(r => r.id === resolution.chosenId)!;
      move(r.id, 'regional-percentage', pass, r.value, resolution);
      available = available.filter(item => item.id !== r.id);
    }
  }
  return complete();
}

/** S1(2-ter), S16-bis(1-sexies): 196 domestic = 189 ordinary + 7 special;
 * the bonus path reserves 35, leaving 154 ordinary seats. I08 tables stay
 * labelled; incompatible totals/region alternatives are never rescaled.
 */
function prepare(input: SenateRegionalInput) {
  const { chamber, admission, bonus, specialSeats, simulationId } = input;
  if (chamber.chamber !== 'senato' || admission.chamber !== 'senato' || specialSeats.chamber !== 'senato' ||
      !Number.isSafeInteger(simulationId) || simulationId < 0 || bonus.simulationId !== simulationId || !bonus.renewedChambers.includes('senato')) {
    throw new RangeError('Supply matching Senate stages and bonus simulation');
  }
  const ordinary = chamber.colleges.filter(c => c.kind === 'ordinary'), special = chamber.colleges.filter(c => c.kind !== 'ordinary');
  const territories = new Map(chamber.territories.map(t => [t.id, t]));
  const regions = [...new Set(chamber.territories.filter(t => t.kind === 'ordinary').map(t => t.regionId))].sort();
  if (sum(ordinary.map(c => c.seatsWithoutBonus)) !== 189 || sum(ordinary.map(c => c.seatsWithBonus)) !== 154 ||
      sum(special.map(c => c.seatsWithoutBonus)) !== 7 || sum(chamber.territories.map(t => t.bonusSeats)) !== 35 ||
      chamber.colleges.some(c => !territories.has(c.territoryId) || c.seatsWithBonus > c.seatsWithoutBonus ||
        (territories.get(c.territoryId)!.kind === 'ordinary') !== (c.kind === 'ordinary')) ||
      special.some(c => c.kind !== 'special-uninominal' || c.seatsWithBonus !== 1 || c.seatsWithoutBonus !== 1) ||
      chamber.territories.some(t => t.kind !== 'ordinary' && (t.bonusSeats !== 0 || regions.includes(t.regionId))) ||
      regions.some(id => sum(ordinary.filter(c => territories.get(c.territoryId)!.regionId === id).map(c => c.seatsWithoutBonus - c.seatsWithBonus)) !==
        sum(chamber.territories.filter(t => t.regionId === id).map(t => t.bonusSeats)))) {
    throw new RangeError('Incompatible Senate endowment: require 189/154 ordinary, 7 special and 35 regional bonus seats');
  }
  return { regions, ordinary, territories };
}

/** N19/N20, S16-bis(1)(f),(f-bis)-(f-quater),(1-bis),(1-ter),(1-quater).
 * "non tiene conto dell'eventuale parte frazionaria del quoziente";
 * "maggiori resti", then regional votes, then statutory draw. All linked
 * coalition votes count here; internal list allocation belongs to N21.
 * Requires validated inputs and same-run N06/N08/N10/N12 communications.
 * N06/N08/N10 have no run ID: pairing those remains the caller's responsibility.
 */
export function allocateSenateRegions(input: SenateRegionalInput, execution: ScrutinyExecutionContext): SenateRegionalAllocationResult {
  const prepared = prepare(input);
  const { chamber, admission, bonus, specialSeats, simulationId } = input;
  const regions: SenateRegionAllocation[] = [];
  let cap: SenateCapResult | null = null;
  const ordinarySeats = bonus.status === 'non-computable' ? null : bonus.status === 'awarded' ? 154 : 189;
  const base = (): RegionalBase => ({ chamber: 'senato', simulationId, seatTable: chamber.seatTable, ordinarySeats, regions, cap,
    provisionalTotals: regions.length === prepared.regions.length && regions.every(r => r.allocation.status === 'complete') ? totals(targets(regions)) : null,
    draws: [...regions.flatMap(r => r.allocation.draws), ...(cap?.draws ?? [])], warnings: [...regions.flatMap(r => r.allocation.warnings), ...(cap?.warnings ?? [])] });
  const blocked = (stage: 'prerequisites' | 'provisional' | 'cap', reason: string, affectedRegionId: string | null = null): SenateRegionalAllocationResult =>
    structuredClone({ ...base(), status: 'non-computable', stage, reason, affectedRegionId });
  if (admission.status !== 'complete') return blocked('prerequisites', 'admission-non-computable');
  if (bonus.status === 'non-computable') return blocked('prerequisites', 'bonus-non-computable');
  const award = bonus.status === 'awarded' ? bonus.awards.find(a => a.chamber === 'senato') : null;
  if (bonus.status === 'awarded' && (!award || award.seats !== 35 || bonus.awards.filter(a => a.chamber === 'senato').length !== 1)) throw new RangeError('Invalid Senate bonus award');
  if (award && specialSeats.status !== 'complete') return blocked('prerequisites', 'special-seats-incomplete');
  if (award && (specialSeats.totalSeats !== 7 || specialSeats.uninominalSeats !== 7 || specialSeats.taaProportionalSeats !== 0 ||
      sum(specialSeats.subjects.map(s => s.totalSeats!)) !== 7 || new Set(specialSeats.subjects.map(s => key(s.subject))).size !== specialSeats.subjects.length ||
      specialSeats.subjects.some(s => s.totalSeats !== s.uninominalSeats || s.taaProportionalSeats !== 0))) {
    throw new RangeError('Special entitlements must account for all seven Senate seats once');
  }
  const subjects = [...admission.admittedSubjects].sort((a, b) => key(a) < key(b) ? -1 : 1);
  const members = subjects.map(s => {
    const ids = s.kind === 'list' ? chamber.lists.filter(l => l.id === s.id).map(l => l.id) : chamber.coalitions.find(c => c.id === s.id)?.listIds;
    if (!ids?.length || ids.some(id => !chamber.lists.some(l => l.id === id))) throw new RangeError('Unknown admitted Senate subject');
    return ids;
  });
  if (new Set(subjects.map(key)).size !== subjects.length || new Set(members.flat()).size !== members.flat().length ||
      (award && !subjects.some(s => key(s) === key(award.subject)))) throw new RangeError('Overlapping subjects or unadmitted Senate winner');
  const figureIds = input.figures.regions.map(r => r.regionId);
  if (new Set(figureIds).size !== figureIds.length) throw new RangeError('Duplicate Senate regional figures');
  // Preflight every region before consuming allocation draws.
  const inputs = prepared.regions.map(regionId => {
    const f = input.figures.regions.find(r => r.regionId === regionId)?.figures;
    if (!f || f.lists.length !== chamber.lists.length || new Set(f.lists.map(l => l.listId)).size !== chamber.lists.length ||
        chamber.lists.some(l => !f.lists.some(row => row.listId === l.id)) || sum(f.lists.map(l => l.votes)) !== f.validVotes) {
      throw new RangeError('Supply every regional list figure once and its full valid-vote total');
    }
    const colleges = prepared.ordinary.filter(c => prepared.territories.get(c.territoryId)!.regionId === regionId);
    return { regionId, ordinarySeats: sum(colleges.map(c => award ? c.seatsWithBonus : c.seatsWithoutBonus)), validVotes: f.validVotes,
      subjects: subjects.map((subject, i) => ({ subject, votes: sum(members[i].map(id => f.lists.find(l => l.listId === id)!.votes)) })) };
  });
  for (const r of inputs) {
    const allocation = allocateByQuotaAndRemainders({ seats: r.ordinarySeats, quotaMode: 'integer',
      subjects: r.subjects.map(s => ({ id: key(s.subject), figure: exactFraction(s.votes), tieValues: [exactFraction(s.votes)] })),
      tieCriteria: [{ name: 'regional-electoral-figure', direction: 'descending' }],
      context: { simulationId, request: { contextId: JSON.stringify(['N19', 'senato', simulationId, r.regionId]), chamber: 'senato', norm: 'N19',
        rule: 'statutory', lawReference: award ? 'S16-bis(1)(f)' : 'S16-bis(1-bis),(1-quater)' } } }, execution);
    regions.push({ ...r, allocation });
    if (allocation.status !== 'complete') return blocked('provisional', allocation.reason, r.regionId);
  }
  if (award) {
    cap = applySenateCap({ regions, subject: award.subject, simulationId,
      specialSeats: specialSeats.subjects.find(s => key(s.subject) === key(award.subject))?.totalSeats ?? 0 }, execution);
    if (cap.status !== 'complete') return blocked('cap', cap.reason, cap.affectedRegionId);
  }
  const finalTargets = cap?.status === 'complete' ? cap.targets : targets(regions);
  return structuredClone({ ...base(), status: 'complete', ordinarySeats: ordinarySeats!,
    path: !award ? 'no-bonus' : cap!.check.exceedsCap ? 'bonus-with-cap' : 'bonus-without-cap',
    targets: finalTargets, subjectTotals: totals(finalTargets), bonus: award ? { subject: award.subject, seats: 35 } : null });
}
