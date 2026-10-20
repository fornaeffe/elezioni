import type { CameraNationalAllocationResult } from './camera-national';
import { allocateTerritorialMatrix, type TerritorialMatrixResult } from './territorial-matrix';
import { exactFraction, floorExact } from './exact';
import { territorialEligibility } from './territorial-eligibility';
import { sumVoteCounts, type AggregatedElectoralFigures, type ElectoralFigures } from './figures';
import type { ChamberInput, SeatTableSource, Subject } from './input';
import type { DrawTrace, ReformWarning, ScrutinyExecutionContext } from './result';

export interface CameraTerritorialInput {
  readonly chamber: ChamberInput;
  readonly figures: AggregatedElectoralFigures;
  readonly national: CameraNationalAllocationResult;
}
interface TerritorialBase {
  readonly chamber: 'camera';
  readonly simulationId: number;
  readonly seatTable: SeatTableSource;
  readonly matrices: readonly TerritorialMatrixResult[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
  readonly interpretations: readonly { readonly id: 'I05'; readonly version: 'I05-v1' }[];
}
export interface CameraConstituencyTarget {
  readonly territoryId: string;
  readonly subject: Subject;
  readonly votes: number;
  readonly ordinarySeats: number;
}
export interface CameraConstituencyListTarget extends CameraConstituencyTarget { readonly listId: string }
export interface CameraCollegeTarget extends CameraConstituencyListTarget { readonly collegeId: string }
type StageResult<T> = TerritorialBase & (
  | { readonly status: 'complete'; readonly targets: readonly T[] }
  | { readonly status: 'non-computable'; readonly reason: string; readonly affectedId: string | null }
);
export type CameraConstituencyResult = StageResult<CameraConstituencyTarget>;
export type CameraConstituencyListResult = StageResult<CameraConstituencyListTarget>;
export type CameraCollegeResult = StageResult<CameraCollegeTarget>;

const key = (s: Subject) => JSON.stringify([s.kind, s.id]);
const sorted = <T extends { readonly id: string }>(items: readonly T[]) => [...items].sort((a, b) => a.id < b.id ? -1 : a.id > b.id ? 1 : 0);
const same = (a: Subject, b: Subject) => key(a) === key(b);
const sum = sumVoteCounts;

function prerequisites(input: CameraTerritorialInput): void {
  const { chamber, national } = input;
  if (chamber.chamber !== 'camera' || national.chamber !== 'camera' ||
      JSON.stringify(chamber.seatTable) !== JSON.stringify(national.seatTable)) {
    throw new RangeError('Supply matching Camera national/geographic stages');
  }
  if (national.status !== 'complete') return;
  const capacity = sum(chamber.colleges.filter(c => c.kind === 'ordinary').map(c => national.bonus ? c.seatsWithBonus : c.seatsWithoutBonus));
  if (capacity !== national.ordinarySeats || sum(national.subjectTargets.map(t => t.ordinarySeats)) !== capacity ||
      sum(national.listTargets.map(t => t.ordinarySeats)) !== capacity ||
      new Set(national.subjectTargets.map(t => key(t.subject))).size !== national.subjectTargets.length ||
      new Set(national.listTargets.map(t => t.listId)).size !== national.listTargets.length) {
    throw new RangeError('Camera national targets must account for the selected ordinary capacity once');
  }
  for (const t of national.subjectTargets) {
    if (sum(national.listTargets.filter(l => same(l.subject, t.subject)).map(l => l.ordinarySeats)) !== t.ordinarySeats ||
        (t.subject.kind === 'coalition' ? !chamber.coalitions.some(c => c.id === t.subject.id) : !chamber.lists.some(l => l.id === t.subject.id))) {
      throw new RangeError('Inconsistent Camera subject/list targets');
    }
  }
  for (const l of national.listTargets) {
    if (!national.subjectTargets.some(t => same(t.subject, l.subject)) || !chamber.lists.some(item => item.id === l.listId) ||
        (l.subject.kind === 'list' ? l.listId !== l.subject.id : !chamber.coalitions.find(c => c.id === l.subject.id)?.listIds.includes(l.listId))) {
      throw new RangeError('Unknown or incorrectly linked Camera list target');
    }
  }
}

function stage(input: CameraTerritorialInput, matrices: readonly TerritorialMatrixResult[] = [], i05 = false): TerritorialBase {
  return { chamber: 'camera', simulationId: input.national.simulationId, seatTable: input.chamber.seatTable, matrices,
    draws: matrices.flatMap(m => m.draws), warnings: matrices.flatMap(m => m.warnings),
    interpretations: i05 ? [{ id: 'I05', version: 'I05-v1' }] : [] };
}
function blocked<T>(base: TerritorialBase, reason: string, affectedId: string | null = null): StageResult<T> {
  return structuredClone({ ...base, status: 'non-computable', reason, affectedId });
}
function completed<T>(base: TerritorialBase, targets: readonly T[]): StageResult<T> {
  return structuredClone({ ...base, status: 'complete', targets });
}
function communication(input: CameraTerritorialInput, previous: TerritorialBase): void {
  if (previous.chamber !== 'camera' || previous.simulationId !== input.national.simulationId ||
      JSON.stringify(previous.seatTable) !== JSON.stringify(input.chamber.seatTable)) {
    throw new RangeError('Supply matching Camera territorial communications');
  }
}
function getFigures(rows: readonly { readonly id: string; readonly figures: ElectoralFigures }[], id: string): ElectoralFigures {
  const matches = rows.filter(r => r.id === id);
  if (matches.length !== 1) throw new RangeError(`Supply exactly one electoral figure for ${id}`);
  return matches[0].figures;
}
function votes(f: ElectoralFigures, subject: Subject): number {
  const rows = subject.kind === 'list' ? f.lists.filter(l => l.listId === subject.id) : f.coalitions.filter(c => c.coalitionId === subject.id);
  if (rows.length !== 1) throw new RangeError('Missing or duplicated Camera electoral figure');
  sum([rows[0].votes]);
  return rows[0].votes;
}
function territoryFigures(input: CameraTerritorialInput, id: string): ElectoralFigures {
  return getFigures(input.figures.territories.map(t => ({ id: t.territoryId, figures: t.figures })), id);
}
function collegeFigures(input: CameraTerritorialInput, id: string): ElectoralFigures {
  return getFigures(input.figures.colleges.map(c => ({ id: c.collegeId, figures: c.figures })), id);
}
const list = (id: string): Subject => ({ kind: 'list', id });

/** N15/N17: C83(1)(h), (1-bis), (1-ter)(d). Special territories and
 * bonus slates never enter the ordinary matrix. N13/N14 targets are final. */
export function allocateCameraConstituencies(input: CameraTerritorialInput, execution: ScrutinyExecutionContext): CameraConstituencyResult {
  prerequisites(input);
  const national = input.national;
  if (national.status !== 'complete') return blocked(stage(input), 'national-allocation-non-computable');
  if (national.path === 'bonus-with-cap') return allocateCameraCapConstituencies(input, execution);
  return constituencies(input, execution, false);
}

/** N17 alone, for testing the cap's separate majority/minority quota path. */
export function allocateCameraCapConstituencies(input: CameraTerritorialInput, execution: ScrutinyExecutionContext): CameraConstituencyResult {
  prerequisites(input);
  if (input.national.status !== 'complete') return blocked(stage(input), 'national-allocation-non-computable');
  if (input.national.path !== 'bonus-with-cap' || !input.national.capCorrection || !input.national.bonus) {
    throw new RangeError('Cap constituency allocation requires the corrected national cap path');
  }
  return constituencies(input, execution, true);
}

function constituencies(input: CameraTerritorialInput, execution: ScrutinyExecutionContext, cap: boolean): CameraConstituencyResult {
  const { chamber, national } = input;
  if (national.status !== 'complete') throw new Error('Incomplete national communication');
  const targets = national.subjectTargets;
  const eligible = territorialEligibility(chamber);
  const result = allocateTerritorialMatrix({ chamber: 'camera', contextId: `camera/${national.simulationId}/constituencies`,
    simulationId: national.simulationId, norm: cap ? 'N17' : 'N15',
    lawReference: cap ? 'C83(1-ter)(d), I05-v1' : 'C83(1)(h),(1-bis)',
    targets: targets.map(t => ({ id: key(t.subject), seats: t.ordinarySeats, votes: t.votes })),
    areas: sorted(chamber.territories.filter(t => t.kind === 'ordinary')).map(t => ({ id: t.id,
      seats: sum(chamber.colleges.filter(c => c.kind === 'ordinary' && c.territoryId === t.id).map(c => national.bonus ? c.seatsWithBonus : c.seatsWithoutBonus)),
      entries: targets.map(s => ({ id: key(s.subject), votes: votes(territoryFigures(input, t.id), s.subject), tieVotes: s.votes,
        eligible: eligible.subject(t.id, s.subject) })) })),
    correction: 'constituencies', initialTieFigure: 'national-electoral-figure',
    ...(cap ? { indexQuotas: targets.map(t => ({ id: key(t.subject), quota: t.ordinarySeats === 0 ? null :
      same(t.subject, national.bonus!.subject) ? national.capCorrection!.majorityQuota : national.capCorrection!.minorityAllocation.quota })) } : {})
  }, execution);
  const base = stage(input, [result], cap);
  if (result.status !== 'complete') return blocked(base, result.reason);
  return completed(base, result.cells.map(c => ({ territoryId: c.areaId,
    subject: targets.find(t => key(t.subject) === c.targetId)!.subject, votes: c.votes, ordinarySeats: c.seats })));
}

/** N16: C83(1)(i),(1-ter)(e). Initial residual ties use LOCAL list figures;
 * corrections import h's national-figure criteria, rather than reusing i's.
 * Only admitted internal list targets are supplied by N13/N14. */
export function allocateCameraCoalitionListsInConstituencies(
  input: CameraTerritorialInput, constituencies: CameraConstituencyResult, execution: ScrutinyExecutionContext
): CameraConstituencyListResult {
  prerequisites(input); communication(input, constituencies);
  const base = stage(input);
  if (input.national.status !== 'complete' || constituencies.status !== 'complete') return blocked(base, 'constituency-allocation-non-computable');
  const matrices: TerritorialMatrixResult[] = [];
  const rows: CameraConstituencyListTarget[] = [];
  const eligible = territorialEligibility(input.chamber);
  for (const t of sorted(input.national.subjectTargets.map(t => ({ ...t, id: key(t.subject) })))) {
    const local = constituencies.targets.filter(c => same(c.subject, t.subject));
    if (sum(local.map(c => c.ordinarySeats)) !== t.ordinarySeats) throw new RangeError('Constituency subject targets do not match national targets');
    const members = input.national.listTargets.filter(l => same(l.subject, t.subject));
    if (t.subject.kind === 'list') {
      rows.push(...local.map(c => ({ ...c, listId: t.subject.id })));
      continue;
    }
    const m = allocateTerritorialMatrix({ chamber: 'camera', contextId: `camera/${base.simulationId}/coalition/${t.subject.id}`,
      simulationId: base.simulationId, norm: 'N16', lawReference: 'C83(1)(i),(1-ter)(e); C83(1)(h), periods 9+',
      targets: members.map(l => ({ id: l.listId, seats: l.ordinarySeats, votes: l.votes })),
      areas: local.map(c => ({ id: c.territoryId, seats: c.ordinarySeats, entries: members.map(l => ({ id: l.listId,
        votes: votes(territoryFigures(input, c.territoryId), list(l.listId)), tieVotes: votes(territoryFigures(input, c.territoryId), list(l.listId)),
        eligible: eligible.list(c.territoryId, l.listId) })) })),
      correction: 'constituencies', initialTieFigure: 'constituency-electoral-figure'
    }, execution);
    matrices.push(m);
    if (m.status !== 'complete') return blocked(stage(input, matrices), m.reason, t.subject.id);
    rows.push(...m.cells.map(c => ({ territoryId: c.areaId, subject: t.subject, listId: c.targetId, votes: c.votes, ordinarySeats: c.seats })));
  }
  return completed(stage(input, matrices), rows);
}

/** N18: C83-bis(1)(b),(1-bis). Only lists with positive constituency
 * entitlement enter college denominators. Group corrections preserve each
 * college; subsequent list corrections may move seats across colleges. */
export function allocateCameraColleges(
  input: CameraTerritorialInput, lists: CameraConstituencyListResult, execution: ScrutinyExecutionContext
): CameraCollegeResult {
  prerequisites(input); communication(input, lists);
  if (input.national.status !== 'complete' || lists.status !== 'complete') return blocked(stage(input), 'constituency-lists-non-computable');
  const national = input.national;
  if (sum(lists.targets.map(l => l.ordinarySeats)) !== national.ordinarySeats || national.listTargets.some(t =>
    sum(lists.targets.filter(l => l.listId === t.listId).map(l => l.ordinarySeats)) !== t.ordinarySeats)) {
    throw new RangeError('Constituency list targets must preserve national list entitlements');
  }
  const matrices: TerritorialMatrixResult[] = [];
  const rows: CameraCollegeTarget[] = [];
  for (const territory of sorted(input.chamber.territories.filter(t => t.kind === 'ordinary'))) {
    const colleges = sorted(input.chamber.colleges.filter(c => c.kind === 'ordinary' && c.territoryId === territory.id));
    const local = lists.targets.filter(l => l.territoryId === territory.id && l.ordinarySeats > 0);
    const capacity = (id: string) => { const c = colleges.find(c => c.id === id)!; return national.bonus ? c.seatsWithBonus : c.seatsWithoutBonus; };
    if (national.path === 'bonus-with-cap' && sum(local.map(l => l.ordinarySeats)) !== sum(colleges.map(c => capacity(c.id)))) {
      // C83-bis(1-bis)(c) transfers seats "nei medesimi collegi". Unlike
      // independent b(3) list corrections, it cannot change the group total.
      return blocked(stage(input, matrices), 'constituency-capacity-target-mismatch', territory.id);
    }
    const runLists = (members: readonly CameraConstituencyListTarget[], capacities: ReadonlyMap<string, number>, suffix: string): string | null => {
      const matrix = allocateTerritorialMatrix({ chamber: 'camera', contextId: `camera/${national.simulationId}/colleges/${territory.id}/${suffix}`,
        simulationId: national.simulationId, norm: 'N18', lawReference: national.path === 'bonus-with-cap'
          ? 'C83-bis(1-bis)(d); C83-bis(1)(b)(1)-(3)' : 'C83-bis(1)(b)(1)-(3)',
        targets: members.map(l => ({ id: l.listId, seats: l.ordinarySeats, votes: l.votes })),
        areas: colleges.map(c => ({ id: c.id, seats: capacities.get(c.id)!, entries: members.map(l => ({ id: l.listId,
          votes: votes(collegeFigures(input, c.id), list(l.listId)), tieVotes: l.votes })) })),
        correction: national.path === 'bonus-with-cap' ? 'colleges' : 'independent-colleges',
        initialTieFigure: 'constituency-electoral-figure'
      }, execution);
      matrices.push(matrix);
      if (matrix.status !== 'complete') return matrix.reason;
      rows.push(...matrix.cells.map(c => ({ territoryId: territory.id, collegeId: c.areaId,
        subject: members.find(l => l.listId === c.targetId)!.subject, listId: c.targetId, votes: c.votes, ordinarySeats: c.seats })));
      return null;
    };
    if (national.path !== 'bonus-with-cap') {
      const failure = runLists(local, new Map(colleges.map(c => [c.id, capacity(c.id)])), 'lists');
      if (failure) return blocked(stage(input, matrices), failure, territory.id);
      continue;
    }
    const groups = ['majority', 'minority'].map(id => ({ id, members: local.filter(l =>
      same(l.subject, national.bonus!.subject) === (id === 'majority')) }));
    const groupTargets = groups.map(g => ({ id: g.id, seats: sum(g.members.map(l => l.ordinarySeats)), votes: sum(g.members.map(l => l.votes)) }));
    const groupMatrix = allocateTerritorialMatrix({ chamber: 'camera', contextId: `camera/${national.simulationId}/colleges/${territory.id}/groups`,
      simulationId: national.simulationId, norm: 'N18', lawReference: 'C83-bis(1-bis)(a)-(c)', targets: groupTargets,
      areas: colleges.map(c => ({ id: c.id, seats: capacity(c.id), entries: groups.map(g => ({ id: g.id,
        votes: sum(g.members.map(l => votes(collegeFigures(input, c.id), list(l.listId)))), tieVotes: groupTargets.find(t => t.id === g.id)!.votes })) })),
      indexQuotas: groupTargets.map(t => ({ id: t.id, quota: t.seats === 0 ? null : exactFraction(floorExact(exactFraction(t.votes, t.seats))) })),
      correction: 'college-groups', initialTieFigure: 'constituency-electoral-figure'
    }, execution);
    matrices.push(groupMatrix);
    if (groupMatrix.status !== 'complete') return blocked(stage(input, matrices), groupMatrix.reason, territory.id);
    for (const group of groups) {
      if (!group.members.length) continue; // I04: inactive zero-target group.
      const capacities = new Map(groupMatrix.cells.filter(c => c.targetId === group.id).map(c => [c.areaId, c.seats]));
      const failure = runLists(group.members, capacities, group.id);
      if (failure) return blocked(stage(input, matrices), failure, territory.id);
    }
  }
  return completed(stage(input, matrices), rows);
}

/** Scoped N15-N18 composition, without proclamations, recovery or registration. */
export function allocateCameraTerritorialSeats(input: CameraTerritorialInput, execution: ScrutinyExecutionContext) {
  const constituencies = allocateCameraConstituencies(input, execution);
  const lists = allocateCameraCoalitionListsInConstituencies(input, constituencies, execution);
  const colleges = allocateCameraColleges(input, lists, execution);
  return { status: colleges.status, constituencies, lists, colleges };
}
