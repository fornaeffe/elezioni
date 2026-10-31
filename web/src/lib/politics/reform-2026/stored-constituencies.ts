import type { ResultTable } from '../../core/types';
import type { StoredResult } from '../../core/result-store';
import type { SeriesDescriptor, SeriesSummary } from '../../results/statistics';
import { finiteNumber } from '../../results/statistics';
import { reformCollegeSeriesTable, reformSeriesTable, seriesKey } from './result-series';
import type { CollegeSeatEndowment } from './stored-statistics';

type Row = ResultTable['rows'][number];
type ReadRows = (name: string) => AsyncIterable<Row>;

/** Compatibility requires actual raw votes, archived coverage and final affiliations.
 * Never substitute percentages, current geography or a new legal execution. */
export function archivedConstituencyCatalogue(record: StoredResult, national: SeriesSummary[], endowments: CollegeSeatEndowment[]): SeriesDescriptor[] {
  const required: Record<string, string[]> = {
    [reformCollegeSeriesTable]: ['Votes', 'Valid votes', 'Entitled'],
    [reformSeriesTable]: ['Elected', 'Confirmed'],
    'Seat entitlement and election locations': ['Seat', 'Election territory', 'Election college', 'Election list', 'Subject', 'Category', 'Status'],
    'Candidate outcomes (conditional)': ['Seat', 'Affiliation list']
  };
  if (!('coverage' in record.scenario) || Object.entries(required).some(([name, columns]) =>
    !columns.every(c => record.result.storage!.tables.find(t => t.name === name)?.columns.includes(c)))) return [];
  const territories = new Map(endowments.filter(c => c.kind === 'Ordinario').map(c => [`${c.chamber}:${c.territory}`, c]));
  return [...territories.values()].flatMap(t => national.filter(s => s.chamber === t.chamber && s.owner !== 'unaffiliated').map(s => {
    const kind = s.kind === 'list' ? 'constituency-list' : 'constituency-subject';
    return { ...s, id: seriesKey(s.chamber, kind, s.owner, null, t.territory), kind, college: null, territory: t.territory,
      name: `${s.name} · ${t.territoryName}` } as SeriesDescriptor;
  }));
}

/** At most one simulation of each evidence stream is retained. */
async function* grouped(rows: AsyncIterable<Row>) {
  let simulation = 0, group: Row[] = [];
  for await (const row of rows) {
    const next = Number(row.Simulation);
    if (!Number.isSafeInteger(next) || next < simulation || next < 1) throw new Error('Invalid archived constituency evidence order');
    if (simulation !== next && group.length) { yield { simulation, rows: group }; group = []; }
    simulation = next; group.push(row);
  }
  if (group.length) yield { simulation, rows: group };
}

export async function* archivedConstituencyRows(record: StoredResult, descriptor: SeriesDescriptor, endowments: CollegeSeatEndowment[], readRows: ReadRows): AsyncGenerator<Row> {
  if (!('coverage' in record.scenario)) return;
  const scenario = record.scenario, chamber = descriptor.chamber;
  const lists = scenario.lists[chamber];
  const members = descriptor.kind === 'constituency-list' ? [descriptor.owner.slice(5)] :
    lists.filter(l => (l.coalitionId ? `coalition:${l.coalitionId}` : `list:${l.id}`) === descriptor.owner).map(l => l.id);
  const colleges = endowments.filter(c => c.chamber === chamber && c.territory === descriptor.territory && c.kind === 'Ordinario');
  const sources = ['Seat entitlement and election locations', 'Candidate outcomes (conditional)', reformCollegeSeriesTable]
    .map(name => grouped(readRows(name)));
  const current = await Promise.all(sources.map(s => s.next()));
  try {
    for await (const run of grouped(readRows(reformSeriesTable))) {
      const evidence: Row[][] = [];
      for (let i = 0; i < sources.length; i++) {
        let item = current[i];
        while (!item.done && item.value.simulation < run.simulation) item = await sources[i].next();
        current[i] = item;
        evidence.push(!item.done && item.value.simulation === run.simulation ? item.value.rows.filter(r => r.Chamber === chamber) : []);
      }
      const [ledger, candidates, local] = evidence;
      let votes: number | null = 0, valid: number | null = 0, entitled: number | null = 0;
      for (const college of colleges) {
        const rows = local.filter(r => r.College === college.college);
        const denominators = rows.map(r => finiteNumber(r['Valid votes']));
        const denominator = denominators[0];
        if (denominator === undefined || denominator === null || denominators.some(n => n !== denominator)) valid = null;
        else if (valid !== null) valid += denominator;
        const coverage = scenario.coverage.find(c => c.chamber === chamber && c.collegeId === college.college);
        if (!coverage) { votes = null; entitled = null; continue; }
        for (const member of members) {
          if (!coverage.listIds.includes(member)) continue; // Explicit non-contestation, not missing data.
          const matches = rows.filter(r => r.Owner === `list:${member}`);
          const v = matches.length === 1 ? finiteNumber(matches[0].Votes) : null;
          const e = matches.length === 1 ? finiteNumber(matches[0].Entitled) : null;
          votes = votes !== null && v !== null ? votes + v : null;
          entitled = entitled !== null && e !== null ? entitled + e : null;
        }
      }
      const national = run.rows.filter(r => r.Chamber === chamber);
      const outcome = national.find(r => r.Kind === (descriptor.kind === 'constituency-list' ? 'list' : 'subject') && r.Owner === descriptor.owner);
      const filled = ledger.filter(r => r.Status === 'filled');
      const expected = national.filter(r => r.Kind === 'list').reduce((n, r) => n + Number(r.Confirmed), 0);
      const affiliations = new Map(candidates.map(r => [r.Seat, r['Affiliation list']]));
      const relevant = filled.filter(r => ['ordinary', 'bonus'].includes(String(r.Category)) && r['Election territory'] === descriptor.territory);
      const known = finiteNumber(outcome?.Elected) !== null && filled.length === expected && ledger.every(r => r.Status !== 'unresolved') &&
        (descriptor.kind !== 'constituency-list' || relevant.every(r => affiliations.has(r.Seat)));
      const selected = relevant.filter(r => descriptor.kind === 'constituency-list' ? `list:${affiliations.get(r.Seat)}` === descriptor.owner : r.Subject === descriptor.owner);
      const ordinary = selected.filter(r => r.Category === 'ordinary').length, bonus = selected.filter(r => r.Category === 'bonus').length;
      yield { Simulation: run.simulation, Chamber: chamber, Kind: descriptor.kind, Owner: descriptor.owner, College: null, Territory: descriptor.territory,
        Name: descriptor.name, Color: descriptor.color, Order: descriptor.order, Votes: votes, 'Valid votes': valid,
        'Vote share %': votes !== null && valid !== null && valid > 0 ? 100 * votes / valid : null,
        Elected: known ? ordinary + bonus : null, Confirmed: ordinary + bonus, Ordinary: ordinary, Bonus: bonus, Special: 0, TAA: 0, Entitled: entitled };
    }
  } finally { await Promise.all(sources.map(s => s.return(undefined))); }
}
