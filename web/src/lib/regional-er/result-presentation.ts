import type { ResultTable } from '$lib/core/types';
import type {
  RegionalErRunPresentationSummary,
  RegionalErScrutinyInputRow,
  RegionalErScrutinyOutput
} from './types';

export type { RegionalErRunPresentationSummary } from './types';

export const regionalErListSeatVotePlotTableName = 'Regional list seat-vote plot data';
export const regionalErCoalitionSeatVotePlotTableName = 'Regional coalition seat-vote plot data';
export const regionalErProvinceSeatPlotTableName = 'Regional province-list seat plot data';
export const regionalErResultPlotTableNames = [
  regionalErListSeatVotePlotTableName,
  regionalErCoalitionSeatVotePlotTableName,
  regionalErProvinceSeatPlotTableName
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

function numericStats(values: readonly number[]): NumericStats {
  if (values.length === 0) return { average: 0, min: 0, max: 0, probabilityAtLeastOne: 0 };
  return {
    average: values.reduce((sum, value) => sum + value, 0) / values.length,
    min: Math.min(...values),
    max: Math.max(...values),
    probabilityAtLeastOne: values.filter((value) => value > 0).length / values.length
  };
}

function distributionStats(values: readonly number[]): DistributionStats {
  if (values.length === 0) return { average: 0, p05: 0, p50: 0, p95: 0 };
  return {
    average: values.reduce((sum, value) => sum + value, 0) / values.length,
    p05: quantile(values, 0.05),
    p50: quantile(values, 0.5),
    p95: quantile(values, 0.95)
  };
}

function mapStatRows(
  runs: readonly RegionalErRunPresentationSummary[],
  mapSelector: (run: RegionalErRunPresentationSummary) => ReadonlyMap<string, number>
): Array<{ key: string; values: number[] }> {
  const keys = new Set<string>();
  for (const run of runs) for (const key of mapSelector(run).keys()) keys.add(key);

  return [...keys]
    .sort((left, right) => left.localeCompare(right, 'it'))
    .map((key) => ({
      key,
      values: runs.map((run) => mapSelector(run).get(key) ?? 0)
    }));
}

export function summarizeRegionalErScrutinyRun(params: {
  sim: number;
  elapsedMs: number;
  input: {
    comuni_liste: readonly RegionalErScrutinyInputRow[];
  };
  output: RegionalErScrutinyOutput;
}): RegionalErRunPresentationSummary {
  const seatsByList = new Map(params.output.liste.map((row) => [row.LISTA, row.ELETTI]));
  const validVoteShareByList = new Map(params.output.liste.map((row) => [row.LISTA, row.PERCENTUALE * 100]));
  const seatsByCoalition = new Map(params.output.coalizioni.map((row) => [row.COALIZIONE, row.ELETTI]));
  const totalSeatsByCoalition = new Map(params.output.coalizioni.map((row) => [row.COALIZIONE, row.ELETTI_TOT]));
  const validVoteShareByCoalition = new Map(params.output.coalizioni.map((row) => [row.COALIZIONE, row.PERCENTUALE * 100]));
  const provinceSeatsByList = new Map(
    params.output.prov_lista.map((row) => [`${row.PROVINCIA}\u001f${row.LISTA}`, row.ELETTI])
  );

  return {
    sim: params.sim,
    elapsedMs: params.elapsedMs,
    councilSeats: params.output.liste.reduce((sum, row) => sum + row.ELETTI, 0),
    presidentCoalition: params.output.coalizioni.find((row) => row.PRESIDENTE)?.COALIZIONE ?? null,
    runnerUpCoalition: params.output.coalizioni.find((row) => row.MIGLIOR_PERDENTE)?.COALIZIONE ?? null,
    seatsByList,
    seatsByCoalition,
    totalSeatsByCoalition,
    validVoteShareByList,
    validVoteShareByCoalition,
    provinceSeatsByList
  };
}

export function summarizeRegionalErGeneratedRuns(runs: readonly RegionalErRunPresentationSummary[]): ResultTable {
  return {
    name: 'Generated regional runs',
    columns: ['Sim', 'Seggi liste', 'Presidente', 'Miglior perdente', 'Tempo ms'],
    rows: runs.map((run) => ({
      Sim: run.sim,
      'Seggi liste': run.councilSeats,
      Presidente: run.presidentCoalition,
      'Miglior perdente': run.runnerUpCoalition,
      'Tempo ms': round(run.elapsedMs, 1)
    }))
  };
}

function electionOverviewTable(runs: readonly RegionalErRunPresentationSummary[]): ResultTable {
  const stats = numericStats(runs.map((run) => run.councilSeats));
  const elapsed = numericStats(runs.map((run) => run.elapsedMs));
  return {
    name: 'Regional election overview',
    columns: ['Simulazioni', 'Media seggi liste', 'Min seggi liste', 'Max seggi liste', 'Tempo medio ms'],
    rows: [
      {
        Simulazioni: runs.length,
        'Media seggi liste': round(stats.average),
        'Min seggi liste': stats.min,
        'Max seggi liste': stats.max,
        'Tempo medio ms': round(elapsed.average, 1)
      }
    ]
  };
}

function coalitionOutcomesTable(runs: readonly RegionalErRunPresentationSummary[]): ResultTable {
  const coalitions = new Set<string>();
  for (const run of runs) {
    for (const coalition of run.validVoteShareByCoalition.keys()) coalitions.add(coalition);
    if (run.presidentCoalition) coalitions.add(run.presidentCoalition);
    if (run.runnerUpCoalition) coalitions.add(run.runnerUpCoalition);
  }

  return {
    name: 'Regional coalition outcomes',
    columns: ['Coalizione', 'Prob. presidente %', 'Prob. miglior perdente %', 'Media seggi liste', 'Media seggi tot', 'Media voti %'],
    rows: [...coalitions]
      .sort((left, right) => left.localeCompare(right, 'it'))
      .map((coalition) => {
        const seats = numericStats(runs.map((run) => run.seatsByCoalition.get(coalition) ?? 0));
        const totalSeats = numericStats(runs.map((run) => run.totalSeatsByCoalition.get(coalition) ?? 0));
        const voteShare = distributionStats(runs.map((run) => run.validVoteShareByCoalition.get(coalition) ?? 0));
        return {
          Coalizione: coalition,
          'Prob. presidente %': round((runs.filter((run) => run.presidentCoalition === coalition).length / Math.max(runs.length, 1)) * 100, 1),
          'Prob. miglior perdente %': round((runs.filter((run) => run.runnerUpCoalition === coalition).length / Math.max(runs.length, 1)) * 100, 1),
          'Media seggi liste': round(seats.average),
          'Media seggi tot': round(totalSeats.average),
          'Media voti %': round(voteShare.average)
        };
      })
  };
}

function seatsByListTable(runs: readonly RegionalErRunPresentationSummary[]): ResultTable {
  return {
    name: 'Regional average seats by list',
    columns: ['Lista', 'Media', 'Min', 'Max', 'Probabilita >=1'],
    rows: mapStatRows(runs, (run) => run.seatsByList).map((row) => {
      const stats = numericStats(row.values);
      return {
        Lista: row.key,
        Media: round(stats.average),
        Min: stats.min,
        Max: stats.max,
        'Probabilita >=1': round(stats.probabilityAtLeastOne * 100, 1)
      };
    })
  };
}

function voteShareByListTable(runs: readonly RegionalErRunPresentationSummary[]): ResultTable {
  return {
    name: 'Regional vote share by list',
    columns: ['Lista', 'Media %', 'P05 %', 'P50 %', 'P95 %'],
    rows: mapStatRows(runs, (run) => run.validVoteShareByList).map((row) => {
      const stats = distributionStats(row.values);
      return {
        Lista: row.key,
        'Media %': round(stats.average),
        'P05 %': round(stats.p05),
        'P50 %': round(stats.p50),
        'P95 %': round(stats.p95)
      };
    })
  };
}

function provinceSeatsByListTable(runs: readonly RegionalErRunPresentationSummary[]): ResultTable {
  return {
    name: 'Regional province seats by list',
    columns: ['Provincia', 'Lista', 'Media', 'Min', 'Max', 'Probabilita >=1'],
    rows: mapStatRows(runs, (run) => run.provinceSeatsByList).map((row) => {
      const [province, list] = row.key.split('\u001f');
      const stats = numericStats(row.values);
      return {
        Provincia: province,
        Lista: list,
        Media: round(stats.average),
        Min: stats.min,
        Max: stats.max,
        'Probabilita >=1': round(stats.probabilityAtLeastOne * 100, 1)
      };
    })
  };
}

function listSeatVotePlotTable(runs: readonly RegionalErRunPresentationSummary[]): ResultTable {
  return {
    name: regionalErListSeatVotePlotTableName,
    columns: ['Sim', 'Lista', 'Percentuale %', 'Seggi'],
    rows: runs.flatMap((run) =>
      [...run.seatsByList.entries()].map(([list, seats]) => ({
        Sim: run.sim,
        Lista: list,
        'Percentuale %': round(run.validVoteShareByList.get(list) ?? 0),
        Seggi: seats
      }))
    )
  };
}

function coalitionSeatVotePlotTable(runs: readonly RegionalErRunPresentationSummary[]): ResultTable {
  return {
    name: regionalErCoalitionSeatVotePlotTableName,
    columns: ['Sim', 'Coalizione', 'Percentuale liste %', 'Seggi tot'],
    rows: runs.flatMap((run) =>
      [...run.totalSeatsByCoalition.entries()].map(([coalition, seats]) => ({
        Sim: run.sim,
        Coalizione: coalition,
        'Percentuale liste %': round(run.validVoteShareByCoalition.get(coalition) ?? 0),
        'Seggi tot': seats
      }))
    )
  };
}

function provinceSeatPlotTable(runs: readonly RegionalErRunPresentationSummary[]): ResultTable {
  return {
    name: regionalErProvinceSeatPlotTableName,
    columns: ['Sim', 'Provincia', 'Lista', 'Seggi'],
    rows: runs.flatMap((run) =>
      [...run.provinceSeatsByList.entries()].map(([rowKey, seats]) => {
        const [province, list] = rowKey.split('\u001f');
        return {
          Sim: run.sim,
          Provincia: province,
          Lista: list,
          Seggi: seats
        };
      })
    )
  };
}

export function buildRegionalErResultTables(runs: readonly RegionalErRunPresentationSummary[]): ResultTable[] {
  return [
    electionOverviewTable(runs),
    coalitionOutcomesTable(runs),
    seatsByListTable(runs),
    voteShareByListTable(runs),
    provinceSeatsByListTable(runs)
  ];
}

export function buildRegionalErResultPlotTables(runs: readonly RegionalErRunPresentationSummary[]): ResultTable[] {
  return [listSeatVotePlotTable(runs), coalitionSeatVotePlotTable(runs), provinceSeatPlotTable(runs)];
}
