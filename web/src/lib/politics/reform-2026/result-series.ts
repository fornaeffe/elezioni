import { OTHER_LIST_ID } from '../../core/other-list';
import type { ResultTable } from '../../core/types';
import type { ReformGenerationScenario, ReformGenerationSnapshot } from './generation-types';
import type { ChamberInput, ReformScrutinyInput } from './input';
import type { ComposedReformScrutinyResult } from './scrutiny';
import type { SeatLedgerEntry } from './result';
import { reformPresentation, subjectColor } from './presentation-metadata';

export const reformSeriesTable = 'Reform statistical series v1';
export const reformCollegeSeriesTable = 'Reform college series v1';
export const reformSeriesColumns = ['Simulation', 'Chamber', 'Kind', 'Owner', 'College', 'Territory', 'Name', 'Color', 'Order', 'Votes', 'Valid votes', 'Vote share %', 'Elected', 'Confirmed', 'Ordinary', 'Bonus', 'Special', 'TAA', 'Entitled'];
export type SeriesKind = 'list' | 'subject' | 'college';
export const unaffiliatedOwner = 'unaffiliated';
export const seriesKey = (chamber: string, kind: string, owner: string, college: string | null = null) => JSON.stringify([chamber, kind, owner, college]);

/** Final personal affiliation is not the list owning the original entitlement.
 * In particular, initial coalition bonus/special seats have a null ledger list. */
export function finalAffiliations(input: ReformScrutinyInput, result: ComposedReformScrutinyResult): Map<string, string | null> {
  const affiliations = new Map<string, string | null>();
  for (const c of result.chambers) for (const e of c.recovery?.retained ?? c.multiple?.retained ?? []) affiliations.set(e.seat.seatId, e.affiliationListId);
  const chambers = input.renewal.kind === 'joint' ? [input.renewal.camera, input.renewal.senato] : [input.renewal.chamber];
  for (const seat of result.seats) if (seat.status === 'filled' && !affiliations.has(seat.seatId)) {
    const c = chambers.find(c => c.chamber === seat.entitlement.chamber)!;
    affiliations.set(seat.seatId, seat.category === 'special-uninominal'
      ? c.specialCandidatures.find(p => p.collegeId === seat.electedAt.collegeId && p.candidateId === seat.candidateId)?.affiliationListId ?? null
      : seat.electedAt.listId);
  }
  return affiliations;
}

/** Small per-run, exportable observations. No municipal input or legal stage is
 * retained. Unknown final counts are null; Confirmed remains a lower bound. */
export function reformRunSeries(input: ReformScrutinyInput, result: ComposedReformScrutinyResult,
  scenario: ReformGenerationScenario, snapshot: ReformGenerationSnapshot, affiliations = finalAffiliations(input, result)): ResultTable['rows'] {
  const rows: ResultTable['rows'] = [], presentation = reformPresentation(scenario);
  const chambers: readonly ChamberInput[] = input.renewal.kind === 'joint' ? [input.renewal.camera, input.renewal.senato] : [input.renewal.chamber];
  for (const c of result.chambers) {
    const source = chambers.find(s => s.chamber === c.chamber)!;
    const seats = result.seats.filter(s => s.entitlement.chamber === c.chamber);
    const known = c.recovery !== undefined && c.recovery.status !== 'non-computable' && seats.every(s => s.status !== 'unresolved');
    const filled = seats.filter((s): s is Extract<SeatLedgerEntry, { status: 'filled' }> => s.status === 'filled');
    const color = (id: string, coalition = false) => (coalition ? presentation.coalitions : presentation.lists).find(p => p.id === id)?.color ?? subjectColor(id);
    const add = (kind: SeriesKind, owner: string, name: string, colour: string, order: number, college: string | null, territory: string | null,
      votes: number | null, valid: number | null, selected: typeof filled, entitled: number | null) => {
      const count = (category: string) => selected.filter(s => s.category === category).length;
      rows.push({ Simulation: input.simulationId, Chamber: c.chamber, Kind: kind, Owner: owner, College: college, Territory: territory,
        Name: name, Color: /^#[0-9a-f]{6}$/i.test(colour) ? colour : '#64748b', Order: order, Votes: votes, 'Valid votes': valid,
        'Vote share %': valid !== null && valid > 0 && votes !== null ? votes / valid * 100 : null,
        Elected: owner === `list:${OTHER_LIST_ID}` ? 0 : known ? selected.length : null, Confirmed: selected.length, Ordinary: count('ordinary'), Bonus: count('bonus'),
        Special: count('special-uninominal'), TAA: count('taa-proportional'), Entitled: entitled });
    };
    source.lists.forEach((l, i) => {
      const v = c.figures?.national.lists.find(v => v.listId === l.id);
      add('list', `list:${l.id}`, l.name, color(l.id), i, null, null, v?.votes ?? null, c.figures?.national.validVotes ?? null,
        filled.filter(s => affiliations.get(s.seatId) === l.id), null);
    });
    // Always retain the bucket, including explicit zero observations. An
    // independent special winner must never be invented as a member-list win.
    add('list', unaffiliatedOwner, 'Senza affiliazione dichiarata', '#64748b', source.lists.length, null, null, null, null,
      filled.filter(s => affiliations.get(s.seatId) === null), null);
    const subjects = [...source.coalitions.map(s => ({ id: `coalition:${s.id}`, members: s.listIds, name: presentation.coalitions.find(p => p.id === s.id)?.name ?? source.lists.filter(l => s.listIds.includes(l.id)).map(l => l.name).join(' + '), color: color(s.id, true) })),
      ...source.lists.filter(l => l.coalitionId === null).map(l => ({ id: `list:${l.id}`, members: [l.id], name: l.name, color: color(l.id) }))];
    subjects.forEach((s, i) => add('subject', s.id, s.name, s.color, i, null, null,
      c.figures ? s.members.reduce((n, id) => n + (c.figures!.national.lists.find(l => l.listId === id)?.votes ?? 0), 0) : null,
      c.figures?.national.validVotes ?? null, filled.filter(e => `${e.entitlement.subject.kind}:${e.entitlement.subject.id}` === s.id), null));
    // Other has geographic vote support without an ordinary candidate slate.
    const collegeLists = [...source.ordinarySlates, ...(source.otherListCoverage ?? []).filter(r => source.colleges.find(c => c.id === r.collegeId)?.kind !== 'special-uninominal')];
    for (const slate of collegeLists) {
      const college = source.colleges.find(c => c.id === slate.collegeId)!;
      const geography = snapshot.seats.geography.colleges.find(c => c.chamber === source.chamber && c.id === college.id);
      const l = source.lists.find(l => l.id === slate.listId)!;
      const figures = [...c.figures?.colleges ?? [], ...c.figures?.taaProportional ?? []].find(f => f.collegeId === college.id)?.figures;
      const targets = c.cameraTerritorial?.colleges ?? c.senateTerritorial?.colleges;
      const entitled = l.kind === 'other' ? 0 : college.kind === 'taa-proportional'
        ? c.taa && c.taa.status !== 'non-computable' ? c.taa.listEntitlements.find(t => t.listId === l.id)?.seats ?? 0 : null
        : targets?.status === 'complete' ? targets.targets.find(t => t.collegeId === college.id && t.listId === l.id)?.ordinarySeats ?? 0 : null;
      add('college', `list:${l.id}`, `${l.name} · ${geography?.name ?? college.id}`, color(l.id), source.lists.indexOf(l), college.id, college.territoryId,
        figures?.lists.find(v => v.listId === l.id)?.votes ?? null, figures?.validVotes ?? null,
        filled.filter(s => s.electedAt.collegeId === college.id && affiliations.get(s.seatId) === l.id), entitled);
    }
  }
  return rows;
}
