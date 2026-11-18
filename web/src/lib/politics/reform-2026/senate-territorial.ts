import type { AdmissionResult } from './admission';
import { allocateByQuotaAndRemainders, type QuotaAllocationResult } from './allocation';
import { exactFraction, floorExact } from './exact';
import { territorialEligibility } from './territorial-eligibility';
import { sumVoteCounts as sum, type AggregatedElectoralFigures, type ElectoralFigures } from './figures';
import type { ChamberInput, SeatTableSource, Subject } from './input';
import type { DrawTrace, ReformWarning, ScrutinyExecutionContext } from './result';
import type { SenateRegionalAllocationResult } from './senate-regional';
import { allocateTerritorialMatrix, type TerritorialMatrixResult } from './territorial-matrix';

export interface SenateTerritorialInput {
  readonly chamber: ChamberInput;
  readonly figures: AggregatedElectoralFigures;
  readonly admission: AdmissionResult;
  readonly regional: SenateRegionalAllocationResult;
}
export interface SenateRegionalListTarget {
  readonly regionId: string;
  readonly subject: Subject;
  readonly listId: string;
  readonly votes: number;
  readonly ordinarySeats: number;
}
export interface SenateCollegeTarget extends SenateRegionalListTarget {
  readonly territoryId: string;
  readonly collegeId: string;
}
interface TerritorialBase {
  readonly chamber: 'senato';
  readonly simulationId: number;
  readonly seatTable: SeatTableSource;
  readonly coalitionAllocations: readonly {
    readonly regionId: string; readonly coalitionId: string; readonly allocation: QuotaAllocationResult;
  }[];
  readonly matrices: readonly TerritorialMatrixResult[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}
type StageResult<T> = TerritorialBase & (
  | { readonly status: 'complete'; readonly targets: readonly T[] }
  | { readonly status: 'non-computable'; readonly reason: string; readonly affectedRegionId: string | null }
);
export type SenateRegionalListResult = StageResult<SenateRegionalListTarget>;
export type SenateCollegeResult = StageResult<SenateCollegeTarget>;
const key = (s: Subject) => JSON.stringify([s.kind, s.id]);
const same = (a: Subject, b: Subject) => key(a) === key(b);
const sorted = <T extends { readonly id: string }>(rows: readonly T[]) => [...rows].sort((a, b) => a.id < b.id ? -1 : a.id > b.id ? 1 : 0);

function stage(input: SenateTerritorialInput, coalitionAllocations: TerritorialBase['coalitionAllocations'] = [],
  matrices: readonly TerritorialMatrixResult[] = []): TerritorialBase {
  const events = [...coalitionAllocations.map(a => a.allocation), ...matrices];
  return { chamber: 'senato', simulationId: input.regional.simulationId, seatTable: input.chamber.seatTable,
    coalitionAllocations, matrices, draws: events.flatMap(a => a.draws), warnings: events.flatMap(a => a.warnings) };
}
function blocked<T>(base: TerritorialBase, reason: string, affectedRegionId: string | null = null): StageResult<T> {
  return structuredClone({ ...base, status: 'non-computable', reason, affectedRegionId });
}
function complete<T>(base: TerritorialBase, targets: readonly T[]): StageResult<T> {
  return structuredClone({ ...base, status: 'complete', targets });
}
function figure(rows: readonly { readonly id: string; readonly figures: ElectoralFigures }[], id: string,
  chamber: ChamberInput): ElectoralFigures {
  const matches = rows.filter(r => r.id === id);
  if (matches.length !== 1) throw new RangeError('Supply each Senate electoral figure exactly once');
  const f = matches[0].figures;
  if (f.lists.length !== chamber.lists.length || new Set(f.lists.map(l => l.listId)).size !== f.lists.length ||
      chamber.lists.some(l => !f.lists.some(r => r.listId === l.id)) || sum(f.lists.map(l => l.votes)) !== f.validVotes) {
    throw new RangeError('Supply complete Senate list figures and valid-vote totals');
  }
  return f;
}
const regionalFigure = (input: SenateTerritorialInput, id: string) => figure(
  input.figures.regions.map(r => ({ id: r.regionId, figures: r.figures })), id, input.chamber);
const collegeFigure = (input: SenateTerritorialInput, id: string) => figure(
  input.figures.colleges.map(r => ({ id: r.collegeId, figures: r.figures })), id, input.chamber);
const votes = (f: ElectoralFigures, id: string) => f.lists.find(l => l.listId === id)!.votes;
const regions = (input: SenateTerritorialInput) => [...new Set(input.chamber.territories.filter(t => t.kind === 'ordinary').map(t => t.regionId))].sort();
function colleges(input: SenateTerritorialInput, regionId: string) {
  return sorted(input.chamber.colleges.filter(c => c.kind === 'ordinary' &&
    input.chamber.territories.find(t => t.id === c.territoryId)?.regionId === regionId));
}
function capacity(input: SenateTerritorialInput, c: ChamberInput['colleges'][number]) {
  if (input.regional.status !== 'complete') throw new Error('Incomplete regional allocation');
  return input.regional.bonus ? c.seatsWithBonus : c.seatsWithoutBonus;
}
function members(input: SenateTerritorialInput, subject: Subject): readonly string[] {
  if (input.admission.status !== 'complete') throw new Error('Incomplete admission');
  if (subject.kind === 'list') return [subject.id];
  const coalition = input.admission.coalitions.find(c => c.coalitionId === subject.id && c.admitted);
  if (!coalition || new Set(coalition.admittedListIds).size !== coalition.admittedListIds.length ||
      coalition.admittedListIds.some(id => !input.chamber.coalitions.find(c => c.id === subject.id)?.listIds.includes(id))) {
    throw new RangeError('Supply admitted Senate coalition members');
  }
  return [...coalition.admittedListIds].sort();
}

/** Inputs are validated legal envelopes and same-run N06/N08/N19/N20
 * communications. N06/N08 lack run IDs; pairing remains the caller's duty. */
function prerequisites(input: SenateTerritorialInput): void {
  const { chamber, regional, admission } = input;
  if (chamber.chamber !== 'senato' || regional.chamber !== 'senato' || admission.chamber !== 'senato' ||
      !Number.isSafeInteger(regional.simulationId) || regional.simulationId < 0 ||
      JSON.stringify(chamber.seatTable) !== JSON.stringify(regional.seatTable)) {
    throw new RangeError('Supply matching Senate regional/geographic stages');
  }
  if (regional.status !== 'complete' || admission.status !== 'complete') return;
  const ids = regions(input), subjects = admission.admittedSubjects;
  if ((regional.path === 'no-bonus') !== (regional.bonus === null) ||
      (regional.bonus && (regional.bonus.seats !== 35 || !subjects.some(s => same(s, regional.bonus!.subject)))) ||
      (regional.path === 'bonus-with-cap' && (!regional.cap || regional.cap.status !== 'complete' || !regional.cap.check.exceedsCap)) ||
      new Set(subjects.map(key)).size !== subjects.length ||
      subjects.some(s => s.kind === 'list' ? !chamber.lists.some(l => l.id === s.id) : !chamber.coalitions.some(c => c.id === s.id)) ||
      regional.targets.length !== ids.length * subjects.length ||
      new Set(regional.targets.map(t => JSON.stringify([t.regionId, key(t.subject)]))).size !== regional.targets.length ||
      regional.targets.some(t => !ids.includes(t.regionId) || !subjects.some(s => same(s, t.subject)))) {
    throw new RangeError('Supply consistent final Senate subject targets');
  }
  const lists = subjects.flatMap(s => members(input, s));
  if (new Set(lists).size !== lists.length || lists.some(id => !chamber.lists.some(l => l.id === id))) {
    throw new RangeError('Overlapping or unknown admitted Senate lists');
  }
  for (const id of ids) {
    const local = colleges(input, id);
    if (!local.length || sum(regional.targets.filter(t => t.regionId === id).map(t => t.ordinarySeats)) !== sum(local.map(c => capacity(input, c)))) {
      throw new RangeError('Senate regional targets must conserve each regional endowment');
    }
    const f = regionalFigure(input, id);
    for (const t of regional.targets.filter(t => t.regionId === id)) {
      const all = t.subject.kind === 'list' ? [t.subject.id] : chamber.coalitions.find(c => c.id === t.subject.id)!.listIds;
      if (t.votes !== sum(all.map(l => votes(f, l)))) throw new RangeError('Regional subject figures disagree with N06');
    }
  }
  if (sum(regional.targets.map(t => t.ordinarySeats)) !== regional.ordinarySeats) throw new RangeError('Inconsistent Senate ordinary total');
}

/** N21, S17(1)(b),(2): "liste collegate ammesse ... 2-bis) e 2-ter)";
 * "non tiene conto ... parte frazionaria"; "maggiori resti", then regional
 * list votes and statutory draw. Use corrected N20 targets, never N19's
 * provisional seats; bonus ownership does not enter this ordinary split. */
export function allocateSenateRegionalLists(input: SenateTerritorialInput, execution: ScrutinyExecutionContext): SenateRegionalListResult {
  prerequisites(input);
  if (input.regional.status !== 'complete') return blocked(stage(input), 'regional-allocation-non-computable');
  if (input.admission.status !== 'complete') return blocked(stage(input), 'admission-non-computable');
  const allocations: TerritorialBase['coalitionAllocations'][number][] = [];
  const targets: SenateRegionalListTarget[] = [];
  const coverage = territorialEligibility(input.chamber);
  for (const regionId of regions(input)) {
    const f = regionalFigure(input, regionId);
    for (const t of sorted(input.regional.targets.filter(t => t.regionId === regionId).map(t => ({ ...t, id: key(t.subject) })))) {
      const ids = members(input, t.subject);
      if (t.subject.kind === 'list') {
        targets.push({ regionId, subject: t.subject, listId: t.subject.id, votes: votes(f, t.subject.id), ordinarySeats: t.ordinarySeats });
        continue;
      }
      const allocation = allocateByQuotaAndRemainders({ seats: t.ordinarySeats, quotaMode: 'integer',
        subjects: ids.map(id => ({ id, figure: exactFraction(votes(f, id)), tieValues: [exactFraction(votes(f, id))],
          remainderEligible: input.chamber.territories.some(t => t.kind === 'ordinary' && t.regionId === regionId && coverage.list(t.id, id)) })),
        tieCriteria: [{ name: 'regional-electoral-figure', direction: 'descending' }],
        context: { simulationId: input.regional.simulationId, request: { chamber: 'senato', norm: 'N21', rule: 'statutory',
          lawReference: 'S17(1)(b),(2)', contextId: JSON.stringify(['N21', input.regional.simulationId, regionId, t.subject]) } }
      }, execution);
      allocations.push({ regionId, coalitionId: t.subject.id, allocation });
      if (allocation.status !== 'complete') return blocked(stage(input, allocations), allocation.reason, regionId);
      targets.push(...allocation.assignments.map(a => ({ regionId, subject: t.subject, listId: a.id, votes: votes(f, a.id), ordinarySeats: a.seats })));
    }
  }
  return complete(stage(input, allocations), targets);
}

function communication(input: SenateTerritorialInput, lists: SenateRegionalListResult): void {
  if (lists.chamber !== 'senato' || lists.simulationId !== input.regional.simulationId ||
      JSON.stringify(lists.seatTable) !== JSON.stringify(input.chamber.seatTable)) throw new RangeError('Supply matching Senate list communication');
  if (lists.status !== 'complete' || input.regional.status !== 'complete' || input.admission.status !== 'complete') return;
  const admitted = input.admission.admittedSubjects;
  const expected = regions(input).flatMap(regionId => admitted.flatMap(s =>
    members(input, s).map(listId => ({ regionId, subject: s, listId }))));
  if (lists.targets.length !== expected.length || new Set(lists.targets.map(t => JSON.stringify([t.regionId, t.listId]))).size !== lists.targets.length ||
      expected.some(e => !lists.targets.some(t => t.regionId === e.regionId && t.listId === e.listId && same(t.subject, e.subject)))) {
    throw new RangeError('Supply each admitted regional list target once');
  }
  for (const t of input.regional.targets) {
    const local = lists.targets.filter(l => l.regionId === t.regionId && same(l.subject, t.subject));
    if (sum(local.map(l => l.ordinarySeats)) !== t.ordinarySeats || local.some(l => l.votes !== votes(regionalFigure(input, t.regionId), l.listId))) {
      throw new RangeError('Senate list communication disagrees with regional targets/figures');
    }
  }
  // Preflight every college before consuming any matrix draws, including
  // single-college regions. Special ballots never enter these regional sums.
  for (const regionId of regions(input)) {
    const fs = colleges(input, regionId).map(c => collegeFigure(input, c.id));
    if (input.chamber.lists.some(l => sum(fs.map(f => votes(f, l.id))) !== votes(regionalFigure(input, regionId), l.id))) {
      throw new RangeError('Senate college figures must sum to their regional list figures');
    }
  }
}

/** N21, S17(3)-(4). A single college takes the regional list targets.
 * Otherwise "cifra elettorale di collegio" breaks residual ties, unlike
 * Camera C83-bis. S17(4)(c) independently ranks donor/recipient decimals;
 * list corrections conserve regional targets but can change college totals. */
export function allocateSenateColleges(input: SenateTerritorialInput, lists: SenateRegionalListResult,
  execution: ScrutinyExecutionContext): SenateCollegeResult {
  prerequisites(input); communication(input, lists);
  if (input.regional.status !== 'complete') return blocked(stage(input), 'regional-allocation-non-computable');
  if (input.admission.status !== 'complete') return blocked(stage(input), 'admission-non-computable');
  if (lists.status !== 'complete') return blocked(stage(input), 'regional-lists-non-computable');
  return collegeAllocation(input, lists, execution, input.regional.path === 'bonus-with-cap');
}

/** N22 alone. S17(5) is used only after an actual S16-bis(1-ter) correction. */
export function allocateSenateCapColleges(input: SenateTerritorialInput, lists: SenateRegionalListResult,
  execution: ScrutinyExecutionContext): SenateCollegeResult {
  prerequisites(input); communication(input, lists);
  if (input.regional.status !== 'complete') return blocked(stage(input), 'regional-allocation-non-computable');
  if (input.admission.status !== 'complete') return blocked(stage(input), 'admission-non-computable');
  if (lists.status !== 'complete') return blocked(stage(input), 'regional-lists-non-computable');
  if (input.regional.path !== 'bonus-with-cap') throw new RangeError('Senate cap colleges require the corrected cap path');
  return collegeAllocation(input, lists, execution, true);
}

function collegeAllocation(input: SenateTerritorialInput, lists: Extract<SenateRegionalListResult, { status: 'complete' }>,
  execution: ScrutinyExecutionContext, cap: boolean): SenateCollegeResult {
  if (input.regional.status !== 'complete') throw new Error('Incomplete regional allocation');
  const regional = input.regional, matrices: TerritorialMatrixResult[] = [], rows: SenateCollegeTarget[] = [];
  for (const regionId of regions(input)) {
    const local = lists.targets.filter(l => l.regionId === regionId && l.ordinarySeats > 0), cs = colleges(input, regionId);
    if (cs.length === 1) {
      rows.push(...sorted(lists.targets.filter(l => l.regionId === regionId).map(l => ({ ...l, id: l.listId })))
        .map(({ id: _, ...l }) => ({ ...l, collegeId: cs[0].id, territoryId: cs[0].territoryId })));
      continue;
    }
    const runLists = (targets: readonly SenateRegionalListTarget[], capacities: ReadonlyMap<string, number>, suffix: string) => {
      const m = allocateTerritorialMatrix({ chamber: 'senato', simulationId: regional.simulationId,
        contextId: JSON.stringify(['senato', regional.simulationId, regionId, 'colleges', suffix]), norm: cap ? 'N22' : 'N21',
        lawReference: cap ? 'S17(5)(d); S17(4)(a)-(c)' : 'S17(4)(a)-(c)',
        targets: targets.map(t => ({ id: t.listId, seats: t.ordinarySeats, votes: t.votes })),
        areas: cs.map(c => ({ id: c.id, seats: capacities.get(c.id)!, entries: targets.map(t => ({ id: t.listId,
          votes: votes(collegeFigure(input, c.id), t.listId), tieVotes: votes(collegeFigure(input, c.id), t.listId) })) })),
        correction: 'colleges', initialTieFigure: 'college-electoral-figure'
      }, execution);
      matrices.push(m);
      if (m.status !== 'complete') return m.reason;
      rows.push(...m.cells.map(c => ({ regionId, territoryId: cs.find(p => p.id === c.areaId)!.territoryId,
        collegeId: c.areaId, listId: c.targetId, subject: targets.find(t => t.listId === c.targetId)!.subject, votes: c.votes, ordinarySeats: c.seats })));
      return null;
    };
    if (!cap) {
      const failure = runLists(local, new Map(cs.map(c => [c.id, capacity(input, c)])), 'lists');
      if (failure) return blocked(stage(input, [], matrices), failure, regionId);
      continue;
    }
    // S17(5)(a): only lists with regional seats enter either group divisor.
    // S17(5)(b): BOTH indices "troncato alla sesta cifra decimale", then
    // index*college capacity/sum(indices); college group votes break ties.
    const groups = ['majority', 'minority'].map(id => ({ id, members: local.filter(l => same(l.subject, regional.bonus!.subject) === (id === 'majority')) }));
    const targets = groups.map(g => ({ id: g.id, seats: sum(g.members.map(l => l.ordinarySeats)), votes: sum(g.members.map(l => l.votes)) }));
    const m = allocateTerritorialMatrix({ chamber: 'senato', simulationId: regional.simulationId,
      contextId: JSON.stringify(['senato', regional.simulationId, regionId, 'colleges', 'groups']), norm: 'N22', lawReference: 'S17(5)(a)-(c)', targets,
      areas: cs.map(c => ({ id: c.id, seats: capacity(input, c), entries: groups.map(g => { const n = sum(g.members.map(l => votes(collegeFigure(input, c.id), l.listId)));
        return { id: g.id, votes: n, tieVotes: n }; }) })),
      indexQuotas: targets.map(t => ({ id: t.id, quota: t.seats === 0 ? null : exactFraction(floorExact(exactFraction(t.votes, t.seats))) })),
      correction: 'college-groups', initialTieFigure: 'college-electoral-figure'
    }, execution);
    matrices.push(m);
    if (m.status !== 'complete') return blocked(stage(input, [], matrices), m.reason, regionId);
    // S17(5)(c): group transfers remain "nei medesimi collegi". (d) then
    // applies the ordinary list operation separately within each group.
    for (const g of groups) {
      if (!g.members.length) continue; // I04: inactive zero-target group.
      const failure = runLists(g.members, new Map(m.cells.filter(c => c.targetId === g.id).map(c => [c.areaId, c.seats])), g.id);
      if (failure) return blocked(stage(input, [], matrices), failure, regionId);
    }
  }
  return complete(stage(input, [], matrices), rows);
}

/** Scoped N21/N22 composition. Upstream events remain in their own stages. */
export function allocateSenateListsAndColleges(input: SenateTerritorialInput, execution: ScrutinyExecutionContext) {
  const lists = allocateSenateRegionalLists(input, execution);
  const colleges = allocateSenateColleges(input, lists, execution);
  return { status: colleges.status, lists, colleges };
}
