import type { AdmissionResult } from './admission';
import { allocateByQuotaAndRemainders, type QuotaAllocationResult } from './allocation';
import type { GovernabilityBonusResult } from './bonus';
import { exactFraction, floorExact } from './exact';
import { sumVoteCounts, type AggregatedElectoralFigures } from './figures';
import type { ChamberInput, SeatTableSource, Subject } from './input';
import type { DrawTrace, ExactFraction, ReformWarning, ScrutinyExecutionContext } from './result';
import type { SpecialSeatTotals } from './special-seats';

export interface CameraNationalInput {
  readonly chamber: ChamberInput;
  readonly figures: AggregatedElectoralFigures;
  readonly admission: AdmissionResult;
  readonly bonus: GovernabilityBonusResult;
  /** N10 entitlements, including TAA proportional seats even if identities are unresolved. */
  readonly specialSeats: SpecialSeatTotals;
  readonly simulationId: number;
}
export interface CameraSubjectTarget {
  readonly subject: Subject;
  /** C83(1)(d-bis), excluding both special territories. */
  readonly votes: number;
  readonly ordinarySeats: number;
}
export interface CameraListTarget extends CameraSubjectTarget {
  readonly listId: string;
}
export interface CameraCapCheck {
  readonly subject: Subject;
  readonly provisionalOrdinarySeats: number;
  readonly specialSeats: number;
  readonly bonusSeats: 70;
  readonly provisionalDomesticSeats: number;
  readonly exceedsCap: boolean;
}
export interface CameraCapCorrection {
  readonly majorityOrdinarySeats: number;
  readonly minorityOrdinarySeats: number;
  readonly majorityRawQuota: ExactFraction;
  readonly majorityQuota: ExactFraction;
  readonly minorityAllocation: QuotaAllocationResult;
}
interface NationalBase {
  readonly chamber: 'camera';
  readonly simulationId: number;
  readonly seatTable: SeatTableSource;
  readonly ordinarySeats: number | null;
  readonly provisionalAllocation: QuotaAllocationResult | null;
  readonly capCheck: CameraCapCheck | null;
  readonly capCorrection: CameraCapCorrection | null;
  readonly coalitionAllocations: readonly { readonly coalitionId: string; readonly allocation: QuotaAllocationResult }[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}
export type CameraNationalAllocationResult = NationalBase & (
  | { readonly status: 'non-computable'; readonly stage: 'prerequisites' | 'provisional' | 'cap' | 'coalition-lists'; readonly reason: string }
  | { readonly status: 'complete'; readonly ordinarySeats: number; readonly path: 'no-bonus' | 'bonus-without-cap' | 'bonus-with-cap';
      readonly subjectTargets: readonly CameraSubjectTarget[];
      readonly listTargets: readonly CameraListTarget[];
      /** Coalition ownership is retained; these are never divided among member lists. */
      readonly bonus: { readonly subject: Subject; readonly seats: 70 } | null }
);

const key = (s: Subject) => JSON.stringify([s.kind, s.id]);
const count = (n: number) => sumVoteCounts([n]);

/** C3/K56 and I08: the literal C83(1-ter)(b) 314 is the ordinary pool
 * AFTER reserving 70 bonus seats, excluding all eight special seats.
 * Reject incompatible tables instead of silently rescaling the cap path.
 */
function ordinaryEndowment(chamber: ChamberInput): { withoutBonus: number; withBonus: number; special: number } {
  const ordinary = chamber.colleges.filter(c => c.kind === 'ordinary');
  const special = chamber.colleges.filter(c => c.kind !== 'ordinary');
  const withoutBonus = sumVoteCounts(ordinary.map(c => c.seatsWithoutBonus));
  const withBonus = sumVoteCounts(ordinary.map(c => c.seatsWithBonus));
  const specialTotal = sumVoteCounts(special.map(c => c.seatsWithoutBonus));
  if (withoutBonus !== 384 || withBonus !== 314 || specialTotal !== 8 ||
      sumVoteCounts(chamber.territories.map(t => t.bonusSeats)) !== 70 ||
      chamber.colleges.some(c => c.seatsWithBonus > c.seatsWithoutBonus ||
        !chamber.territories.some(t => t.id === c.territoryId && (t.kind === 'ordinary') === (c.kind === 'ordinary'))) ||
      special.some(c => c.seatsWithBonus !== c.seatsWithoutBonus)) {
    throw new RangeError('Incompatible Camera endowment: require 392 domestic = 384 ordinary + 8 special, and 314 ordinary + 70 bonus');
  }
  return { withoutBonus, withBonus, special: specialTotal };
}

function allocate(
  rows: readonly { readonly id: string; readonly votes: number }[], seats: number,
  simulationId: number, suffix: string, norm: 'N13' | 'N14', lawReference: string,
  execution: ScrutinyExecutionContext
): QuotaAllocationResult {
  return allocateByQuotaAndRemainders({ seats, quotaMode: 'integer',
    subjects: rows.map(r => ({ id: r.id, figure: exactFraction(r.votes), tieValues: [exactFraction(r.votes)] })),
    tieCriteria: [{ name: 'national-electoral-figure', direction: 'descending' }],
    context: { simulationId, request: { contextId: JSON.stringify([norm, 'camera', suffix]),
      chamber: 'camera', norm, rule: 'statutory', lawReference } } }, execution);
}

/** C83(1)(f-bis): "superiore a duecentoventi seggi complessivi";
 * "sono compresi i seggi ... Valle d'Aosta ... e Trentino-Alto Adige".
 * Estero is absent from every input to this check. Exactly 220 is unchanged.
 */
function checkCameraCap(target: CameraSubjectTarget, specialSeats: number): CameraCapCheck {
  const total = target.ordinarySeats + 70 + specialSeats;
  return { subject: target.subject, provisionalOrdinarySeats: target.ordinarySeats,
    specialSeats, bonusSeats: 70, provisionalDomesticSeats: total, exceedsCap: total > 220 };
}

/** C83(1-ter)(a)-(b): retain the bonus ("ferma restando l'applicazione").
 * Majority seats are assigned DIRECTLY: "differenza tra ... centocinquanta"
 * and its special seats. Its floored quota is for the later territorial stage,
 * not a second integer apportionment which could increase its fixed target.
 * Minority seats: "differenza tra il numero 314" and the majority target.
 */
function applyCameraCap(
  targets: readonly CameraSubjectTarget[], check: CameraCapCheck, simulationId: number,
  execution: ScrutinyExecutionContext
): { readonly status: 'complete'; readonly targets: readonly CameraSubjectTarget[]; readonly correction: CameraCapCorrection } |
   { readonly status: 'non-computable'; readonly reason: string; readonly correction: CameraCapCorrection | null } {
  const majority = targets.find(t => key(t.subject) === key(check.subject))!;
  const majorityOrdinarySeats = 150 - check.specialSeats;
  const minorityOrdinarySeats = 314 - majorityOrdinarySeats;
  const majorityRawQuota = exactFraction(majority.votes, majorityOrdinarySeats);
  const majorityQuota = exactFraction(floorExact(majorityRawQuota));
  if (majority.votes === 0 || majorityQuota.numerator === '0') {
    return { status: 'non-computable', reason: majority.votes === 0 ? 'zero-vote-total' : 'zero-quota', correction: null };
  }
  const minority = targets.filter(t => key(t.subject) !== key(check.subject));
  const minorityAllocation = allocate(minority.map(t => ({ id: key(t.subject), votes: t.votes })),
    minorityOrdinarySeats, simulationId, 'cap-minority', 'N14', 'C83(1-ter)(b)', execution);
  const correction: CameraCapCorrection = { majorityOrdinarySeats, minorityOrdinarySeats,
    majorityRawQuota, majorityQuota, minorityAllocation };
  if (minorityAllocation.status !== 'complete') return { status: 'non-computable', reason: minorityAllocation.reason, correction };
  return { status: 'complete', correction, targets: targets.map(t => ({ ...t,
    ordinarySeats: key(t.subject) === key(check.subject) ? majorityOrdinarySeats
      : minorityAllocation.assignments.find(a => a.id === key(t.subject))!.seats })) };
}

/** N13/N14, C83(1)(f)-(g),(1-bis),(1-ter)(a)-(c).
 * "non tiene conto dell'eventuale parte frazionaria del quoziente";
 * "maggiori resti", then national electoral figure, then statutory draw.
 * Requires validated chamber data and the N06/N07/N10/N12 communications from
 * the same run. N06/N07/N10 totals lack run IDs, so their pairing is the caller's
 * responsibility. No territorial allocation or candidate proclamation runs here.
 */
export function allocateCameraNationalSeats(
  input: CameraNationalInput, execution: ScrutinyExecutionContext
): CameraNationalAllocationResult {
  const { chamber, admission, bonus, specialSeats, simulationId } = input;
  if (chamber.chamber !== 'camera' || admission.chamber !== 'camera' || specialSeats.chamber !== 'camera' ||
      !Number.isSafeInteger(simulationId) || simulationId < 0 || bonus.simulationId !== simulationId ||
      !bonus.renewedChambers.includes('camera')) {
    throw new RangeError('Supply matching Camera stages and bonus simulation');
  }
  const endowment = ordinaryEndowment(chamber);
  const base: NationalBase = { chamber: 'camera', simulationId, seatTable: chamber.seatTable,
    ordinarySeats: bonus.status === 'non-computable' ? null : bonus.status === 'awarded' ? endowment.withBonus : endowment.withoutBonus,
    provisionalAllocation: null, capCheck: null, capCorrection: null, coalitionAllocations: [], draws: [], warnings: [] };
  // Own all transferable metadata/traces, including non-computable results.
  const finish = (result: CameraNationalAllocationResult) => structuredClone(result);
  const blocked = (reason: string) => finish({ ...base, status: 'non-computable', stage: 'prerequisites', reason });
  if (admission.status !== 'complete') return blocked('admission-non-computable');
  if (bonus.status === 'non-computable') return blocked('bonus-non-computable');
  const ordinarySeats = bonus.status === 'awarded' ? endowment.withBonus : endowment.withoutBonus;
  const award = bonus.status === 'awarded' ? bonus.awards[0] : null;
  if (award && (award.chamber !== 'camera' || award.seats !== 70)) throw new RangeError('Invalid Camera bonus award');
  if (award && specialSeats.status !== 'complete') return blocked('special-seats-incomplete');
  if (award && (specialSeats.totalSeats !== endowment.special ||
      sumVoteCounts(specialSeats.subjects.map(s => count(s.totalSeats!))) !== endowment.special ||
      new Set(specialSeats.subjects.map(s => key(s.subject))).size !== specialSeats.subjects.length)) {
    throw new RangeError('Special entitlements must account for all eight Camera special seats once');
  }
  const votes = new Map(input.figures.ordinaryAllocation.lists.map(l => [l.listId, count(l.votes)]));
  if (votes.size !== chamber.lists.length || votes.size !== input.figures.ordinaryAllocation.lists.length ||
      chamber.lists.some(l => !votes.has(l.id))) throw new RangeError('Supply every ordinary-allocation list figure once');
  const prepared = admission.admittedSubjects.map(subject => {
    const coalition = subject.kind === 'coalition' ? chamber.coalitions.find(c => c.id === subject.id) : null;
    const list = subject.kind === 'list' ? chamber.lists.find(l => l.id === subject.id) : null;
    if (!coalition && !list) throw new RangeError('Unknown admitted Camera subject');
    const members = coalition ? admission.lists.filter(l => l.admitted && l.admittedAs === 'coalition-member' &&
      l.coalitionId === coalition.id).map(l => l.listId).sort() : [subject.id];
    if (coalition && members.some(id => !coalition.listIds.includes(id))) throw new RangeError('Invalid admitted coalition member');
    // C83(1)(f) uses ALL linked votes; (g) restricts only the second denominator
    // to admitted e)(2-bis)/(2-ter) members. Never reuse admission/bonus totals.
    return { subject, members, votes: sumVoteCounts((coalition ? coalition.listIds : [subject.id]).map(id => {
      if (!votes.has(id)) throw new RangeError('Missing ordinary list figure');
      return votes.get(id)!;
    })) };
  }).sort((a, b) => key(a.subject) < key(b.subject) ? -1 : 1);
  const ids = prepared.map(s => key(s.subject));
  const members = prepared.flatMap(s => s.subject.kind === 'coalition'
    ? chamber.coalitions.find(c => c.id === s.subject.id)!.listIds : [s.subject.id]);
  if (new Set(ids).size !== ids.length || new Set(members).size !== members.length || members.some(id => !votes.has(id))) {
    throw new RangeError('Admitted subjects or internal lists overlap or are unknown');
  }
  if (award && !ids.includes(key(award.subject))) throw new RangeError('Bonus winner must be an admitted Camera subject');
  const provisionalAllocation = allocate(prepared.map(s => ({ id: key(s.subject), votes: s.votes })),
    ordinarySeats, simulationId, 'provisional-subjects', 'N13', 'C83(1)(f),(1-bis)', execution);
  let trace: NationalBase = { ...base, provisionalAllocation,
    draws: provisionalAllocation.draws, warnings: provisionalAllocation.warnings };
  if (provisionalAllocation.status !== 'complete') return finish({ ...trace, status: 'non-computable', stage: 'provisional', reason: provisionalAllocation.reason });
  let targets: readonly CameraSubjectTarget[] = prepared.map(s => ({ subject: s.subject, votes: s.votes,
    ordinarySeats: provisionalAllocation.assignments.find(a => a.id === key(s.subject))!.seats }));
  if (award) {
    const check = checkCameraCap(targets.find(t => key(t.subject) === key(award.subject))!,
      specialSeats.subjects.find(s => key(s.subject) === key(award.subject))?.totalSeats ?? 0);
    trace = { ...trace, capCheck: check };
    if (check.exceedsCap) {
      const result = applyCameraCap(targets, check, simulationId, execution);
      const minority = result.correction?.minorityAllocation;
      trace = { ...trace, capCorrection: result.correction,
        draws: [...trace.draws, ...(minority?.draws ?? [])], warnings: [...trace.warnings, ...(minority?.warnings ?? [])] };
      if (result.status !== 'complete') return finish({ ...trace, status: 'non-computable', stage: 'cap', reason: result.reason });
      targets = result.targets;
    }
  }
  const listTargets: CameraListTarget[] = [];
  const coalitionAllocations: { coalitionId: string; allocation: QuotaAllocationResult }[] = [];
  for (const target of targets) {
    if (target.subject.kind === 'list') listTargets.push({ ...target, listId: target.subject.id });
    else {
      const coalition = prepared.find(s => key(s.subject) === key(target.subject))!;
      // C83(1)(g)/(1-ter)(c). Zero-seat coalitions do not require a division.
      const allocation = allocate(coalition.members.map(id => ({ id, votes: votes.get(id)! })), target.ordinarySeats,
        simulationId, `coalition:${target.subject.id}`, 'N13', trace.capCorrection ? 'C83(1-ter)(c); C83(1)(g)' : 'C83(1)(g)', execution);
      coalitionAllocations.push({ coalitionId: target.subject.id, allocation });
      trace = { ...trace, coalitionAllocations, draws: [...trace.draws, ...allocation.draws], warnings: [...trace.warnings, ...allocation.warnings] };
      if (allocation.status !== 'complete') return finish({ ...trace, status: 'non-computable', stage: 'coalition-lists', reason: allocation.reason });
      listTargets.push(...allocation.assignments.map(a => ({ subject: target.subject, listId: a.id,
        votes: votes.get(a.id)!, ordinarySeats: a.seats })));
    }
  }
  return finish({ ...trace, status: 'complete', path: !award ? 'no-bonus' : trace.capCorrection ? 'bonus-with-cap' : 'bonus-without-cap',
    ordinarySeats, subjectTargets: targets, listTargets, bonus: award ? { subject: award.subject, seats: 70 } : null });
}
