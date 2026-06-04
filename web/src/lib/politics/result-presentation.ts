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
}

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

function summarizeValidVoteShares(input: PoliticsScrutinyInput): Map<string, number> {
  const totals = sumByList(input.liste_uni);
  const totalValidVotes = [...totals.values()].reduce((sum, votes) => sum + votes, 0);
  const shares = new Map<string, number>();

  for (const [list, votes] of totals) {
    shares.set(list, totalValidVotes === 0 ? 0 : (votes / totalValidVotes) * 100);
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

  return {
    ramo: params.ramo,
    sim: params.sim,
    elapsedMs: params.elapsedMs,
    pluriSeats: params.output.liste_pluri.reduce((sum, row) => sum + row.ELETTI, 0),
    electedUni: params.output.candidati_uni.filter((row) => row.ELETTO).length,
    electedPluri: params.output.candidati_pluri.filter((row) => row.ELETTO).length,
    seatsByList,
    validVoteShareByList: summarizeValidVoteShares(params.input),
    uninominalWinnersBySupport: summarizeUninominalWinners(params.input, params.output, params.listeNaz)
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

export function buildPoliticsResultTables(runs: readonly PoliticsRunPresentationSummary[]): ResultTable[] {
  return [
    electionOverviewTable(runs),
    plurinominalSeatsByListTable(runs),
    voteShareByListTable(runs),
    uninominalWinnersBySupportTable(runs)
  ];
}
