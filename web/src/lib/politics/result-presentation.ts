import type { ResultTable } from '$lib/core/types';
import type {
  CandidatoUniInputRow,
  ListaNazRow,
  ListeUniRow,
  PoliticsScrutinyInput,
  PoliticsScrutinyOutput,
  Ramo
} from './types';

export interface PoliticsRunPresentationSummary {
  ramo: Ramo;
  sim: number;
  elapsedMs: number;
  pluriSeats: number;
  electedUni: number;
  electedPluri: number;
  seatsByList: Map<string, number>;
  validVoteShareByList: Map<string, number>;
  uninominalWinnersBySupport: Map<string, number>;
  seatVoteByList: Map<string, PoliticsSeatVotePoint>;
  seatVoteByCoalition: Map<string, PoliticsSeatVotePoint>;
  plurinominalSeatVotePoints: PoliticsPlurinominalSeatVotePoint[];
}

export interface PoliticsSeatVotePoint {
  voteShare: number;
  seats: number;
}

export interface PoliticsPlurinominalSeatVotePoint extends PoliticsSeatVotePoint {
  circoscrizione: string;
  collegioPlurinominale: string;
  list: string;
}

export const politicsListSeatVotePlotTableName = 'List seat-vote plot data';
export const politicsCoalitionSeatVotePlotTableName = 'Coalition seat-vote plot data';
export const politicsPlurinominalSeatVotePlotTableName = 'Plurinominal seat-vote plot data';
export const politicsResultPlotTableNames = [
  politicsListSeatVotePlotTableName,
  politicsCoalitionSeatVotePlotTableName,
  politicsPlurinominalSeatVotePlotTableName
] as const;

interface NumericStats {
  average: number;
  min: number;
  max: number;
  probabilityAtLeastOne: number;
}

interface DistributionStats {
  average: number;
  p05: number;
  p50: number;
  p95: number;
}

function round(value: number, digits = 2): number {
  return Number(value.toFixed(digits));
}

function collegeCandidateKey(row: {
  CIRCOSCRIZIONE: unknown;
  COLLEGIOPLURINOMINALE: unknown;
  COLLEGIOUNINOMINALE: unknown;
  CANDIDATO: string;
}): string {
  return `${String(row.CIRCOSCRIZIONE)}\u001f${String(row.COLLEGIOPLURINOMINALE)}\u001f${String(
    row.COLLEGIOUNINOMINALE
  )}\u001f${row.CANDIDATO}`;
}

function sumByList(rows: readonly ListeUniRow[]): Map<string, number> {
  const totals = new Map<string, number>();
  for (const row of rows) {
    totals.set(row.LISTA, (totals.get(row.LISTA) ?? 0) + row.VOTI_LISTA);
  }
  return totals;
}

function buildCoalitionByList(listeNaz: readonly ListaNazRow[]): Map<string, string | null> {
  const result = new Map<string, string | null>();
  for (const row of listeNaz) {
    result.set(row.LISTA, row.COALIZIONE);
  }
  return result;
}

function supportSubjectForWinner(
  candidate: CandidatoUniInputRow,
  lists: readonly ListeUniRow[],
  coalitionByList: ReadonlyMap<string, string | null>
): string {
  const candidateKey = collegeCandidateKey(candidate);
  const linkedLists = lists.filter((row) => collegeCandidateKey(row) === candidateKey);
  const coalitions = new Set(
    linkedLists
      .map((row) => coalitionByList.get(row.LISTA) ?? null)
      .filter((coalition): coalition is string => coalition !== null && coalition.length > 0)
  );

  if (coalitions.size > 0) return [...coalitions].sort().join(' + ');

  const standaloneLists = [...new Set(linkedLists.map((row) => row.LISTA))].sort();
  return standaloneLists.length > 0 ? standaloneLists.join(' + ') : 'Senza collegamento';
}

function subjectForList(list: string, coalitionByList: ReadonlyMap<string, string | null>): string {
  const coalition = coalitionByList.get(list);
  return coalition && coalition.length > 0 ? coalition : list;
}

function summarizeValidVoteShares(input: PoliticsScrutinyInput): Map<string, number> {
  const totals = sumByList(input.liste_uni);
  const totalValidVotes = [...totals.values()].reduce((sum, votes) => sum + votes, 0);
  const shares = new Map<string, number>();

  for (const [list, votes] of totals) {
    shares.set(list, totalValidVotes === 0 ? 0 : (votes / totalValidVotes) * 100);
  }

  return shares;
}

function summarizeCoalitionVoteShares(
  input: PoliticsScrutinyInput,
  listeNaz: readonly ListaNazRow[]
): Map<string, number> {
  const coalitionByList = buildCoalitionByList(listeNaz);
  const listTotals = sumByList(input.liste_uni);
  const totalValidVotes = [...listTotals.values()].reduce((sum, votes) => sum + votes, 0);
  const totals = new Map<string, number>();
  const shares = new Map<string, number>();

  for (const [list, votes] of listTotals) {
    const subject = subjectForList(list, coalitionByList);
    totals.set(subject, (totals.get(subject) ?? 0) + votes);
  }

  for (const [subject, votes] of totals) {
    shares.set(subject, totalValidVotes === 0 ? 0 : (votes / totalValidVotes) * 100);
  }

  return shares;
}

function summarizeUninominalWinners(
  input: PoliticsScrutinyInput,
  output: PoliticsScrutinyOutput,
  listeNaz: readonly ListaNazRow[]
): Map<string, number> {
  const inputCandidatesByKey = new Map(input.candidati_uni.map((row) => [collegeCandidateKey(row), row]));
  const coalitionByList = buildCoalitionByList(listeNaz);
  const winners = new Map<string, number>();

  for (const elected of output.candidati_uni) {
    if (!elected.ELETTO) continue;

    const inputCandidate = inputCandidatesByKey.get(collegeCandidateKey(elected));
    const subject = inputCandidate
      ? supportSubjectForWinner(inputCandidate, input.liste_uni, coalitionByList)
      : 'Senza collegamento';
    winners.set(subject, (winners.get(subject) ?? 0) + 1);
  }

  return winners;
}

function buildSeatVoteByList(
  seatsByList: ReadonlyMap<string, number>,
  validVoteShareByList: ReadonlyMap<string, number>
): Map<string, PoliticsSeatVotePoint> {
  const result = new Map<string, PoliticsSeatVotePoint>();
  const lists = new Set([...seatsByList.keys(), ...validVoteShareByList.keys()]);

  for (const list of lists) {
    result.set(list, {
      voteShare: validVoteShareByList.get(list) ?? 0,
      seats: seatsByList.get(list) ?? 0
    });
  }

  return result;
}

function buildSeatVoteByCoalition(params: {
  seatsByList: ReadonlyMap<string, number>;
  coalitionVoteShareBySubject: ReadonlyMap<string, number>;
  uninominalWinnersBySupport: ReadonlyMap<string, number>;
  listeNaz: readonly ListaNazRow[];
}): Map<string, PoliticsSeatVotePoint> {
  const coalitionByList = buildCoalitionByList(params.listeNaz);
  const seatsBySubject = new Map<string, number>();
  const subjects = new Set<string>([
    ...params.coalitionVoteShareBySubject.keys(),
    ...params.uninominalWinnersBySupport.keys()
  ]);

  for (const [list, seats] of params.seatsByList) {
    const subject = subjectForList(list, coalitionByList);
    seatsBySubject.set(subject, (seatsBySubject.get(subject) ?? 0) + seats);
    subjects.add(subject);
  }

  for (const [subject, seats] of params.uninominalWinnersBySupport) {
    seatsBySubject.set(subject, (seatsBySubject.get(subject) ?? 0) + seats);
    subjects.add(subject);
  }

  const result = new Map<string, PoliticsSeatVotePoint>();
  for (const subject of subjects) {
    result.set(subject, {
      voteShare: params.coalitionVoteShareBySubject.get(subject) ?? 0,
      seats: seatsBySubject.get(subject) ?? 0
    });
  }

  return result;
}

function plurinominalKey(row: { CIRCOSCRIZIONE: unknown; COLLEGIOPLURINOMINALE: unknown }): string {
  return `${String(row.CIRCOSCRIZIONE)}\u001f${String(row.COLLEGIOPLURINOMINALE)}`;
}

function summarizePlurinominalVoteShares(input: PoliticsScrutinyInput): Map<string, Map<string, number>> {
  const votesByPluriAndList = new Map<string, Map<string, number>>();
  const totalByPluri = new Map<string, number>();

  for (const row of input.liste_uni) {
    const key = plurinominalKey(row);
    const listVotes = votesByPluriAndList.get(key) ?? new Map<string, number>();
    listVotes.set(row.LISTA, (listVotes.get(row.LISTA) ?? 0) + row.VOTI_LISTA);
    votesByPluriAndList.set(key, listVotes);
    totalByPluri.set(key, (totalByPluri.get(key) ?? 0) + row.VOTI_LISTA);
  }

  const shares = new Map<string, Map<string, number>>();
  for (const [key, listVotes] of votesByPluriAndList) {
    const total = totalByPluri.get(key) ?? 0;
    const listShares = new Map<string, number>();
    for (const [list, votes] of listVotes) {
      listShares.set(list, total === 0 ? 0 : (votes / total) * 100);
    }
    shares.set(key, listShares);
  }

  return shares;
}

function buildPlurinominalSeatVotePoints(
  input: PoliticsScrutinyInput,
  output: PoliticsScrutinyOutput
): PoliticsPlurinominalSeatVotePoint[] {
  const voteShareByPluri = summarizePlurinominalVoteShares(input);

  return output.liste_pluri.map((row) => {
    const key = plurinominalKey(row);
    return {
      circoscrizione: String(row.CIRCOSCRIZIONE),
      collegioPlurinominale: String(row.COLLEGIOPLURINOMINALE),
      list: row.LISTA,
      voteShare: voteShareByPluri.get(key)?.get(row.LISTA) ?? 0,
      seats: row.NUMERO_MAX
    };
  });
}

export function summarizePoliticsScrutinyRun(params: {
  ramo: Ramo;
  sim: number;
  elapsedMs: number;
  input: PoliticsScrutinyInput;
  output: PoliticsScrutinyOutput;
  listeNaz: readonly ListaNazRow[];
}): PoliticsRunPresentationSummary {
  const seatsByList = new Map<string, number>();

  for (const row of params.output.liste_pluri) {
    seatsByList.set(row.LISTA, (seatsByList.get(row.LISTA) ?? 0) + row.ELETTI);
  }

  const validVoteShareByList = summarizeValidVoteShares(params.input);
  const uninominalWinnersBySupport = summarizeUninominalWinners(params.input, params.output, params.listeNaz);
  const coalitionVoteShareBySubject = summarizeCoalitionVoteShares(params.input, params.listeNaz);

  return {
    ramo: params.ramo,
    sim: params.sim,
    elapsedMs: params.elapsedMs,
    pluriSeats: params.output.liste_pluri.reduce((sum, row) => sum + row.ELETTI, 0),
    electedUni: params.output.candidati_uni.filter((row) => row.ELETTO).length,
    electedPluri: params.output.candidati_pluri.filter((row) => row.ELETTO).length,
    seatsByList,
    validVoteShareByList,
    uninominalWinnersBySupport,
    seatVoteByList: buildSeatVoteByList(seatsByList, validVoteShareByList),
    seatVoteByCoalition: buildSeatVoteByCoalition({
      seatsByList,
      coalitionVoteShareBySubject,
      uninominalWinnersBySupport,
      listeNaz: params.listeNaz
    }),
    plurinominalSeatVotePoints: buildPlurinominalSeatVotePoints(params.input, params.output)
  };
}

function numericStats(values: readonly number[]): NumericStats {
  if (values.length === 0) {
    return {
      average: 0,
      min: 0,
      max: 0,
      probabilityAtLeastOne: 0
    };
  }

  return {
    average: values.reduce((sum, value) => sum + value, 0) / values.length,
    min: Math.min(...values),
    max: Math.max(...values),
    probabilityAtLeastOne: values.filter((value) => value > 0).length / values.length
  };
}

function quantile(values: readonly number[], probability: number): number {
  if (values.length === 0) return 0;

  const sorted = [...values].sort((left, right) => left - right);
  const index = (sorted.length - 1) * probability;
  const lowerIndex = Math.floor(index);
  const upperIndex = Math.ceil(index);
  if (lowerIndex === upperIndex) return sorted[lowerIndex];

  const fraction = index - lowerIndex;
  return sorted[lowerIndex] * (1 - fraction) + sorted[upperIndex] * fraction;
}

function distributionStats(values: readonly number[]): DistributionStats {
  if (values.length === 0) {
    return {
      average: 0,
      p05: 0,
      p50: 0,
      p95: 0
    };
  }

  return {
    average: values.reduce((sum, value) => sum + value, 0) / values.length,
    p05: quantile(values, 0.05),
    p50: quantile(values, 0.5),
    p95: quantile(values, 0.95)
  };
}

function runsByRamo(runs: readonly PoliticsRunPresentationSummary[]): Map<Ramo, PoliticsRunPresentationSummary[]> {
  const result = new Map<Ramo, PoliticsRunPresentationSummary[]>();
  for (const run of runs) {
    const rows = result.get(run.ramo) ?? [];
    rows.push(run);
    result.set(run.ramo, rows);
  }
  return result;
}

function mapStatRows(
  runs: readonly PoliticsRunPresentationSummary[],
  mapSelector: (run: PoliticsRunPresentationSummary) => ReadonlyMap<string, number>
): Array<{ ramo: Ramo; key: string; values: number[] }> {
  const result: Array<{ ramo: Ramo; key: string; values: number[] }> = [];

  for (const [ramo, ramoRuns] of runsByRamo(runs)) {
    const keys = new Set<string>();
    for (const run of ramoRuns) {
      for (const key of mapSelector(run).keys()) keys.add(key);
    }

    for (const key of keys) {
      result.push({
        ramo,
        key,
        values: ramoRuns.map((run) => mapSelector(run).get(key) ?? 0)
      });
    }
  }

  return result.sort((left, right) => {
    if (left.ramo !== right.ramo) return left.ramo.localeCompare(right.ramo);
    return left.key.localeCompare(right.key);
  });
}

export function summarizePoliticsGeneratedRuns(runs: readonly PoliticsRunPresentationSummary[]): ResultTable {
  return {
    name: 'Generated pipeline runs',
    columns: ['Ramo', 'Sim', 'Seggi pluri', 'Eletti uni', 'Candidati pluri eletti', 'Tempo ms'],
    rows: runs.map((run) => ({
      Ramo: run.ramo,
      Sim: run.sim,
      'Seggi pluri': run.pluriSeats,
      'Eletti uni': run.electedUni,
      'Candidati pluri eletti': run.electedPluri,
      'Tempo ms': round(run.elapsedMs, 1)
    }))
  };
}

function electionOverviewTable(runs: readonly PoliticsRunPresentationSummary[]): ResultTable {
  return {
    name: 'Election overview',
    columns: ['Ramo', 'Simulazioni', 'Media seggi pluri', 'Media eletti uni', 'Media candidati pluri eletti', 'Tempo medio ms'],
    rows: [...runsByRamo(runs)]
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([ramo, ramoRuns]) => ({
        Ramo: ramo,
        Simulazioni: ramoRuns.length,
        'Media seggi pluri': round(numericStats(ramoRuns.map((run) => run.pluriSeats)).average),
        'Media eletti uni': round(numericStats(ramoRuns.map((run) => run.electedUni)).average),
        'Media candidati pluri eletti': round(numericStats(ramoRuns.map((run) => run.electedPluri)).average),
        'Tempo medio ms': round(numericStats(ramoRuns.map((run) => run.elapsedMs)).average, 1)
      }))
  };
}

function plurinominalSeatsByListTable(runs: readonly PoliticsRunPresentationSummary[]): ResultTable {
  return {
    name: 'Average plurinominal seats by list',
    columns: ['Ramo', 'Lista', 'Media', 'Min', 'Max', 'Probabilità >=1'],
    rows: mapStatRows(runs, (run) => run.seatsByList).map((row) => {
      const stats = numericStats(row.values);
      return {
        Ramo: row.ramo,
        Lista: row.key,
        Media: round(stats.average),
        Min: stats.min,
        Max: stats.max,
        'Probabilità >=1': round(stats.probabilityAtLeastOne * 100, 1)
      };
    })
  };
}

function voteShareByListTable(runs: readonly PoliticsRunPresentationSummary[]): ResultTable {
  return {
    name: 'Vote share by list',
    columns: ['Ramo', 'Lista', 'Media %', 'P05 %', 'P50 %', 'P95 %'],
    rows: mapStatRows(runs, (run) => run.validVoteShareByList).map((row) => {
      const stats = distributionStats(row.values);
      return {
        Ramo: row.ramo,
        Lista: row.key,
        'Media %': round(stats.average),
        'P05 %': round(stats.p05),
        'P50 %': round(stats.p50),
        'P95 %': round(stats.p95)
      };
    })
  };
}

function uninominalWinnersBySupportTable(runs: readonly PoliticsRunPresentationSummary[]): ResultTable {
  return {
    name: 'Uninominal winners by support',
    columns: ['Ramo', 'Soggetto', 'Media', 'Min', 'Max', 'Probabilità >=1'],
    rows: mapStatRows(runs, (run) => run.uninominalWinnersBySupport).map((row) => {
      const stats = numericStats(row.values);
      return {
        Ramo: row.ramo,
        Soggetto: row.key,
        Media: round(stats.average),
        Min: stats.min,
        Max: stats.max,
        'Probabilità >=1': round(stats.probabilityAtLeastOne * 100, 1)
      };
    })
  };
}

function listSeatVotePlotTable(runs: readonly PoliticsRunPresentationSummary[]): ResultTable {
  return {
    name: politicsListSeatVotePlotTableName,
    columns: ['Ramo', 'Sim', 'Lista', 'Percentuale %', 'Seggi'],
    rows: runs.flatMap((run) =>
      [...run.seatVoteByList]
        .sort(([left], [right]) => left.localeCompare(right))
        .map(([list, point]) => ({
          Ramo: run.ramo,
          Sim: run.sim,
          Lista: list,
          'Percentuale %': round(point.voteShare),
          Seggi: point.seats
        }))
    )
  };
}

function coalitionSeatVotePlotTable(runs: readonly PoliticsRunPresentationSummary[]): ResultTable {
  return {
    name: politicsCoalitionSeatVotePlotTableName,
    columns: ['Ramo', 'Sim', 'Soggetto', 'Percentuale liste %', 'Seggi'],
    rows: runs.flatMap((run) =>
      [...run.seatVoteByCoalition]
        .sort(([left], [right]) => left.localeCompare(right))
        .map(([subject, point]) => ({
          Ramo: run.ramo,
          Sim: run.sim,
          Soggetto: subject,
          'Percentuale liste %': round(point.voteShare),
          Seggi: point.seats
        }))
    )
  };
}

function plurinominalSeatVotePlotTable(runs: readonly PoliticsRunPresentationSummary[]): ResultTable {
  return {
    name: politicsPlurinominalSeatVotePlotTableName,
    columns: ['Ramo', 'Sim', 'Circoscrizione', 'Collegio pluri', 'Lista', 'Percentuale %', 'Numero max'],
    rows: runs.flatMap((run) =>
      run.plurinominalSeatVotePoints
        .slice()
        .sort((left, right) => {
          const circ = left.circoscrizione.localeCompare(right.circoscrizione);
          if (circ !== 0) return circ;
          const pluri = left.collegioPlurinominale.localeCompare(right.collegioPlurinominale);
          if (pluri !== 0) return pluri;
          return left.list.localeCompare(right.list);
        })
        .map((point) => ({
          Ramo: run.ramo,
          Sim: run.sim,
          Circoscrizione: point.circoscrizione,
          'Collegio pluri': point.collegioPlurinominale,
          Lista: point.list,
          'Percentuale %': round(point.voteShare),
          'Numero max': point.seats
        }))
    )
  };
}

export function buildPoliticsResultTables(runs: readonly PoliticsRunPresentationSummary[]): ResultTable[] {
  return [
    electionOverviewTable(runs),
    plurinominalSeatsByListTable(runs),
    voteShareByListTable(runs),
    uninominalWinnersBySupportTable(runs)
  ];
}

export function buildPoliticsResultPlotTables(runs: readonly PoliticsRunPresentationSummary[]): ResultTable[] {
  return [listSeatVotePlotTable(runs), coalitionSeatVotePlotTable(runs), plurinominalSeatVotePlotTable(runs)];
}
