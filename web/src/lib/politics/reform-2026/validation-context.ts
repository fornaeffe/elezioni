import { otherListErrors } from './other-list';
import type { Candidate, ChamberInput, College, PoliticalList, ReformScrutinyInput, Subject, Territory } from './input';
import type { ValidationIssue } from './shape';

export const key = (...parts: string[]): string => JSON.stringify(parts);
export class ValidationContext {
  readonly issues: ValidationIssue[] = [];
  readonly people: Map<string, Candidate>;
  constructor(readonly input: ReformScrutinyInput) {
    this.people = this.index(input.candidates, p => p.id, '$.candidates');
  }
  add(code: ValidationIssue['code'], path: string, message: string, lawReference: string | null = null): void {
    this.issues.push({ code, path, message, lawReference });
  }
  index<T>(rows: readonly T[], id: (row: T) => string, path: string): Map<string, T> {
    const map = new Map<string, T>();
    rows.forEach((row, i) => {
      const value = id(row);
      if (map.has(value)) this.add('DUPLICATE_ID', `${path}[${i}]`, `Duplicate identity/key ${value}.`);
      else map.set(value, row);
    });
    return map;
  }
  reference<T>(map: ReadonlyMap<string, T>, id: string, path: string): T | undefined {
    const value = map.get(id);
    if (!value) this.add('UNKNOWN_REFERENCE', path, `Unknown reference ${id}.`);
    return value;
  }
  sum(values: readonly number[], path: string, code: ValidationIssue['code'] = 'INVALID_VOTES'): number {
    const total = values.reduce((a, b) => a + b, 0);
    if (!Number.isSafeInteger(total)) this.add(code, path, 'Aggregate exceeds the safe integer domain.');
    return total;
  }
}

export class ChamberContext {
  readonly path: string;
  readonly lists: Map<string, PoliticalList>;
  readonly territories: Map<string, Territory>;
  readonly colleges: Map<string, College>;
  readonly regions;
  readonly coalitions;
  constructor(readonly validation: ValidationContext, readonly data: ChamberInput) {
    this.path = validation.input.renewal.kind === 'single' ? '$.renewal.chamber' : `$.renewal.${data.chamber}`;
    this.lists = validation.index(data.lists, r => r.id, `${this.path}.lists`);
    this.territories = validation.index(data.territories, r => r.id, `${this.path}.territories`);
    this.colleges = validation.index(data.colleges, r => r.id, `${this.path}.colleges`);
    this.regions = validation.index(data.regions, r => r.id, `${this.path}.regions`);
    this.coalitions = validation.index(data.coalitions, r => r.id, `${this.path}.coalitions`);
  }
  subject(ref: Subject, path: string): readonly string[] {
    const v = this.validation;
    if (ref.kind === 'coalition') return v.reference(this.coalitions, ref.id, path)?.listIds ?? [];
    const list = v.reference(this.lists, ref.id, path);
    if (list?.kind === 'other') v.add('INVALID_LINK', path, 'Other cannot act as an electoral subject.');
    if (list?.coalitionId) v.add('INVALID_LINK', path, 'A coalition member cannot also act as a standalone subject.', 'C14-bis/S8');
    return list ? [list.id] : [];
  }
  subjectForList(listId: string): Subject {
    const coalition = this.lists.get(listId)?.coalitionId;
    return coalition ? { kind: 'coalition', id: coalition } : { kind: 'list', id: listId };
  }
  territoryForCollege(collegeId: string): Territory | undefined {
    const c = this.colleges.get(collegeId);
    return c && this.territories.get(c.territoryId);
  }
}

export function validatePoliticalLinks(c: ChamberContext): void {
  const { validation: v, data: d, path: p } = c;
  for (const message of otherListErrors(d.lists)) v.add('INVALID_LINK', `${p}.lists`, message);
  v.index(d.otherListCoverage ?? [], r => key(r.collegeId, r.listId), `${p}.otherListCoverage`);
  for (const r of d.otherListCoverage ?? []) {
    v.reference(c.colleges, r.collegeId, `${p}.otherListCoverage`);
    if (v.reference(c.lists, r.listId, `${p}.otherListCoverage`)?.kind !== 'other') v.add('INVALID_COVERAGE', p, 'Candidate-free coverage is reserved for Other.');
  }
  // C14-bis(1): «Le dichiarazioni di collegamento devono essere reciproche».
  for (const list of d.lists) {
    if (list.coalitionId) {
      const coalition = v.reference(c.coalitions, list.coalitionId, `${p}.lists.${list.id}.coalitionId`);
      if (coalition && !coalition.listIds.includes(list.id)) v.add('INVALID_LINK', p, 'List/coalition membership is not reciprocal.', 'C14-bis(1)');
    }
    if (list.protectedMinority) {
      const region = v.reference(c.regions, list.protectedMinority.regionId, `${p}.lists.${list.id}.protectedMinority`);
      if (region && !region.autonomous) v.add('INVALID_COVERAGE', p, 'The protected minority exception requires an autonomous region.', 'C18-bis(1)');
    }
  }
  for (const coalition of d.coalitions) {
    v.index(coalition.listIds, x => x, `${p}.coalitions.${coalition.id}.listIds`);
    if (!coalition.listIds.length) v.add('INVALID_LINK', p, 'A coalition must contain lists.');
    for (const id of coalition.listIds) {
      const list = v.reference(c.lists, id, `${p}.coalitions.${coalition.id}.listIds`);
      if (list && list.coalitionId !== coalition.id) v.add('INVALID_LINK', p, 'Coalition/list membership is not reciprocal.', 'C14-bis(1)');
    }
  }
}

export function validateEndowments(c: ChamberContext): void {
  const { validation: v, data: d, path: p } = c;
  for (const t of d.territories) {
    const r = v.reference(c.regions, t.regionId, `${p}.territories.${t.id}.regionId`);
    if (t.kind !== 'ordinary' && (t.bonusSeats !== 0 || !r?.autonomous)) v.add('INVALID_ENDOWMENT', p, 'Special territories require autonomy and have no bonus endowment.', 'C2/C3; S1');
    if (!d.colleges.some(x => x.territoryId === t.id)) v.add('INVALID_ENDOWMENT', p, `Territory ${t.id} has no colleges.`);
  }
  if (d.chamber === 'senato') v.index(d.territories, t => t.regionId, `${p}.territories.regionId`);
  // C93-septies(2): one constituency-level TAA proportional allocation.
  // Composition must not choose an arbitrary first row or count two copies.
  if (d.chamber === 'camera' && d.colleges.filter(x => x.kind === 'taa-proportional').length !== 1) {
    v.add('INVALID_ENDOWMENT', `${p}.colleges`, 'Camera requires exactly one TAA proportional college.', 'C93-septies(2)');
  }
  for (const college of d.colleges) {
    const t = v.reference(c.territories, college.territoryId, `${p}.colleges.${college.id}.territoryId`);
    if (!t) continue;
    const compatible = college.kind === 'ordinary' ? t.kind === 'ordinary' :
      college.kind === 'taa-proportional' ? t.kind === 'taa' && d.chamber === 'camera' : t.kind !== 'ordinary';
    if (!compatible || college.seatsWithBonus > college.seatsWithoutBonus || college.seatsWithoutBonus === 0) v.add('INVALID_ENDOWMENT', p, 'Invalid college kind or capacities.', 'C2/C3; S1');
    if (college.kind === 'special-uninominal' && (college.seatsWithoutBonus !== 1 || college.seatsWithBonus !== 1)) v.add('INVALID_ENDOWMENT', p, 'A special uninominal college has one seat.');
    if (t.kind !== 'ordinary' && college.seatsWithBonus !== college.seatsWithoutBonus) v.add('INVALID_ENDOWMENT', p, 'Special capacity cannot change with the bonus.');
  }
  for (const t of d.territories) {
    const colleges = d.colleges.filter(x => x.territoryId === t.id);
    if (v.sum(colleges.map(x => x.seatsWithoutBonus - x.seatsWithBonus), p, 'INVALID_ENDOWMENT') !== t.bonusSeats) v.add('INVALID_ENDOWMENT', p, 'College capacity reduction must equal the territorial bonus endowment.', 'C3/S1');
  }
  // K56: «quattrocento, otto dei quali eletti nella circoscrizione Estero»;
  // K57: «duecento, quattro dei quali eletti nella circoscrizione Estero».
  // C1/S1: «premio di governabilità di settanta seggi» / «trentacinque seggi complessivi».
  // These contracts cover domestic seats only.
  const expected = d.chamber === 'camera' ? 392 : 196;
  const bonus = d.chamber === 'camera' ? 70 : 35;
  if (v.sum(d.colleges.map(x => x.seatsWithoutBonus), p, 'INVALID_ENDOWMENT') !== expected ||
      v.sum(d.territories.map(x => x.bonusSeats), p, 'INVALID_ENDOWMENT') !== bonus) v.add('INVALID_ENDOWMENT', p, 'Domestic/bonus seat totals do not match the reform.', 'K56/K57; C1; S1');
  for (const kind of ['vda', 'taa'] as const) {
    const territories = d.territories.filter(x => x.kind === kind);
    const seats = d.colleges.filter(x => territories.some(t => t.id === x.territoryId));
    const total = v.sum(seats.map(x => x.seatsWithoutBonus), p, 'INVALID_ENDOWMENT');
    // Camera's literal 314 cap pool requires 8 special seats (R26-01 accounting).
    const target = kind === 'vda' ? 1 : d.chamber === 'camera' ? 7 : 6;
    if (territories.length !== 1 || total !== target) v.add('INVALID_ENDOWMENT', p, 'Inconsistent special-territory endowment.', 'C2/C83(1-ter); S1(3)-(4)');
  }
  if (d.seatTable.kind === 'provisional-census' &&
      (d.seatTable.geographyVersion !== v.input.geographyVersion || d.seatTable.censusDate > v.input.electionDate)) v.add('INVALID_METADATA', `${p}.seatTable`, 'Census/geography provenance does not match this input.', 'I08');
}
