import type { PoliticsScenario } from '../scenario/schema';
import { summarizePoliticsScrutinyRun, type PoliticsRunPresentationSummary } from '../result-presentation';
import { OTHER_LIST_ID, OTHER_LIST_NAME } from '../../core/other-list';
import type { ResultTable } from '../../core/types';
import { reformSeriesColumns, reformSeriesTable } from '../reform-2026/result-series';

/** Restore common stable IDs after the unchanged scrutiny's canonical-name boundary. */
export function summarizeCommonRosatellum(params: Parameters<typeof summarizePoliticsScrutinyRun>[0]) {
  if (!params.listeNaz.some(l => l.kind === 'other')) return summarizePoliticsScrutinyRun(params);
  const id = (value: string) => value === OTHER_LIST_NAME ? OTHER_LIST_ID : value;
  const summary = summarizePoliticsScrutinyRun({ ...params,
    input: { ...params.input, liste_uni: params.input.liste_uni.map(l => l.LISTA === OTHER_LIST_NAME ? { ...l, LISTA: id(l.LISTA) } : l) },
    output: { ...params.output, liste_pluri: params.output.liste_pluri.map(l => l.LISTA === OTHER_LIST_NAME ? { ...l, LISTA: id(l.LISTA) } : l) },
    listeNaz: params.listeNaz.map(l => l.LISTA === OTHER_LIST_NAME ? { ...l, LISTA: id(l.LISTA) } : l) });
  summary.seatVoteByCoalition = new Map([...summary.seatVoteByCoalition].map(([key, value]) =>
    [key === OTHER_LIST_ID ? `list:${OTHER_LIST_ID}` : key, value]));
  return summary;
}

/** Reuse the disk statistics contract. Rosatellum list seats are plurinominal;
 * coalition/support seats also include uninominal winners, as in the existing
 * presentation contract. Labels preserve that legal distinction. */
export function rosatellumSeries(s: PoliticsScenario,
  summaries: Pick<PoliticsRunPresentationSummary, 'ramo' | 'sim' | 'seatVoteByList' | 'seatVoteByCoalition'>[]): ResultTable {
  const rows: ResultTable['rows'] = [];
  for (const run of summaries) {
    const add = (kind: 'list' | 'subject', id: string, name: string, color: string, order: number, votes: number, elected: number) =>
      rows.push({ Simulation: run.sim, Chamber: run.ramo, Kind: kind, Owner: kind === 'list' ? `list:${id}` : id, College: null, Territory: null,
        Name: name, Color: color, Order: order, Votes: null, 'Valid votes': null, 'Vote share %': votes,
        Elected: elected, Confirmed: elected, Ordinary: elected, Bonus: 0, Special: 0, TAA: 0, Entitled: null });
    for (const [i, l] of s.subjects.lists.entries()) {
      const point = run.seatVoteByList.get(l.id);
      if (point) add('list', l.id, `${l.name} · seggi plurinominali`, l.color, i, point.voteShare, point.seats);
    }
    let i = 0;
    for (const [id, point] of run.seatVoteByCoalition) {
      const subject = id.startsWith('coalition:') ? s.subjects.coalitions.find(c => c.id === id.slice(10))
        : id.startsWith('list:') ? s.subjects.lists.find(l => l.id === id.slice(5)) : undefined;
      add('subject', id, subject?.name ?? id, subject?.color ?? '#64748b', i++, point.voteShare, point.seats);
    }
  }
  return { name: reformSeriesTable, columns: reformSeriesColumns, rows };
}
