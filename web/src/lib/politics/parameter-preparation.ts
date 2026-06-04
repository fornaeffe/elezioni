import type { Scenario, ScenarioListCorrespondence } from '$lib/core/types';
import type {
  AdministrativeCode,
  PoliticsHistoricalMunicipalListVoteRow,
  PoliticsMunicipalListParameterRow,
  PoliticsPipelineListRow,
  PoliticsScenarioListCorrespondenceSnapshotRow,
  PoliticsScenarioListElectionSnapshotRow
} from './types';

const abstentionListName = 'astensione';
const dayMs = 24 * 60 * 60 * 1000;

export interface ProjectedHistoricalMunicipalVoteRow {
  DATA: string;
  ELEZIONE: string;
  CODICE_COMUNE: AdministrativeCode;
  LISTA: string;
  VOTI: number;
  ELETTORI: number;
  PERCENTUALE: number;
  LOGIT_P: number;
}

export interface PoliticsParameterPreparationResult {
  liste: PoliticsPipelineListRow[];
  comuni_liste: PoliticsMunicipalListParameterRow[];
  liste_elezioni: PoliticsScenarioListElectionSnapshotRow[];
  projected_municipal_votes: ProjectedHistoricalMunicipalVoteRow[];
  warnings: string[];
}

interface CorrespondenceEntry {
  futureList: string;
  factor: number;
}

interface AggregatedProjectedVote {
  DATA: string;
  ELEZIONE: string;
  CODICE_COMUNE: AdministrativeCode;
  LISTA: string;
  VOTI: number;
}

interface MunicipalDeltaRow extends ProjectedHistoricalMunicipalVoteRow {
  DELTA: number;
}

type CorrespondenceInput = ScenarioListCorrespondence | PoliticsScenarioListCorrespondenceSnapshotRow;

function listKey(name: string): string {
  return name.trim().toLocaleLowerCase('it-IT');
}

function correspondenceSourceKey(election: string, pastList: string): string {
  return `${election.trim()}\u001f${listKey(pastList)}`;
}

function municipalVoteKey(row: Pick<AggregatedProjectedVote, 'DATA' | 'ELEZIONE' | 'CODICE_COMUNE' | 'LISTA'>): string {
  return [row.DATA, row.ELEZIONE, String(row.CODICE_COMUNE), row.LISTA].join('\u001f');
}

function electionListKey(row: Pick<PoliticsScenarioListElectionSnapshotRow, 'ELEZIONE' | 'LISTA'>): string {
  return `${row.ELEZIONE}\u001f${row.LISTA}`;
}

function dateTime(rawDate: string): number {
  const isoCandidate = /^\d{4}-\d{2}-\d{2}$/.test(rawDate) ? `${rawDate}T00:00:00.000Z` : rawDate;
  const parsed = Date.parse(isoCandidate);
  return Number.isFinite(parsed) ? parsed : 0;
}

function dateDiffDays(left: string, right: string): number {
  return (dateTime(right) - dateTime(left)) / dayMs;
}

function latestDate(rows: readonly { DATA: string }[]): string {
  let latest = rows[0]?.DATA ?? '1970-01-01T00:00:00.000Z';
  let latestTime = dateTime(latest);

  for (const row of rows) {
    const rowTime = dateTime(row.DATA);
    if (rowTime > latestTime) {
      latest = row.DATA;
      latestTime = rowTime;
    }
  }

  return latest;
}

function logit(probability: number): number {
  if (probability <= 0) return -Infinity;
  if (probability >= 1) return Infinity;
  return Math.log(probability / (1 - probability));
}

function sampleStandardDeviation(values: readonly number[]): number {
  const finiteValues = values.filter(Number.isFinite);
  if (finiteValues.length < 2) return Number.NaN;
  const mean = finiteValues.reduce((sum, value) => sum + value, 0) / finiteValues.length;
  const variance =
    finiteValues.reduce((sum, value) => {
      const centered = value - mean;
      return sum + centered * centered;
    }, 0) /
    (finiteValues.length - 1);

  return Math.sqrt(variance);
}

function meanFinite(values: readonly number[]): number {
  const finiteValues = values.filter(Number.isFinite);
  if (finiteValues.length === 0) return 0;
  return finiteValues.reduce((sum, value) => sum + value, 0) / finiteValues.length;
}

function snapshotCorrespondenceToScenario(row: CorrespondenceInput): ScenarioListCorrespondence {
  if ('LISTA_ORIGINALE' in row) {
    return {
      id: [row.DATA, row.ELEZIONE, row.LISTA_ORIGINALE, row.LISTA].join('|'),
      pastDate: row.DATA,
      pastElection: row.ELEZIONE,
      pastList: row.LISTA_ORIGINALE,
      futureList: row.LISTA,
      factor: row.FATTORE,
      source: 'bundled'
    };
  }

  return row;
}

function normalizeCorrespondences(
  correspondences: readonly CorrespondenceInput[],
  activeFutureListByKey: ReadonlyMap<string, string>
): Map<string, CorrespondenceEntry[]> {
  const rawBySource = new Map<string, Array<{ futureList: string; factor: number }>>();

  for (const inputRow of correspondences) {
    const row = snapshotCorrespondenceToScenario(inputRow);
    const factor = Number(row.factor);
    if (!row.pastElection.trim() || !row.pastList.trim() || !Number.isFinite(factor) || factor <= 0) continue;

    const futureList = activeFutureListByKey.get(listKey(row.futureList)) ?? abstentionListName;
    const sourceKey = correspondenceSourceKey(row.pastElection, row.pastList);
    const sourceRows = rawBySource.get(sourceKey) ?? [];
    sourceRows.push({ futureList, factor });
    rawBySource.set(sourceKey, sourceRows);
  }

  const normalized = new Map<string, CorrespondenceEntry[]>();

  for (const [sourceKey, rows] of rawBySource) {
    const factorTotal = rows.reduce((sum, row) => sum + row.factor, 0);
    if (factorTotal <= 0) continue;

    const combined = new Map<string, number>();
    for (const row of rows) {
      combined.set(row.futureList, (combined.get(row.futureList) ?? 0) + row.factor / factorTotal);
    }
    normalized.set(
      sourceKey,
      [...combined.entries()].map(([futureList, factor]) => ({ futureList, factor }))
    );
  }

  return normalized;
}

function projectedDestinations(
  row: PoliticsHistoricalMunicipalListVoteRow,
  correspondencesBySource: ReadonlyMap<string, CorrespondenceEntry[]>,
  activeFutureListByKey: ReadonlyMap<string, string>
): readonly CorrespondenceEntry[] {
  const explicit = correspondencesBySource.get(correspondenceSourceKey(row.ELEZIONE, row.LISTA));
  if (explicit) return explicit;

  if (listKey(row.LISTA) === listKey(abstentionListName)) {
    return [{ futureList: abstentionListName, factor: 1 }];
  }

  const homonymousFutureList = activeFutureListByKey.get(listKey(row.LISTA));
  if (homonymousFutureList) return [{ futureList: homonymousFutureList, factor: 1 }];

  return [{ futureList: abstentionListName, factor: 1 }];
}

export function projectHistoricalMunicipalVotes(
  historicalVotes: readonly PoliticsHistoricalMunicipalListVoteRow[],
  scenario: Scenario,
  correspondences: readonly CorrespondenceInput[] = scenario.listCorrespondences
): ProjectedHistoricalMunicipalVoteRow[] {
  const activeFutureListByKey = new Map<string, string>(
    scenario.lists.map((row) => [listKey(row.name), row.name])
  );
  activeFutureListByKey.set(listKey(abstentionListName), abstentionListName);

  const correspondencesBySource = normalizeCorrespondences(correspondences, activeFutureListByKey);
  const projectedVotesByKey = new Map<string, AggregatedProjectedVote>();

  for (const row of historicalVotes) {
    const votes = Number(row.VOTI);
    if (!Number.isFinite(votes)) continue;

    for (const destination of projectedDestinations(row, correspondencesBySource, activeFutureListByKey)) {
      const projectedRow: AggregatedProjectedVote = {
        DATA: row.DATA,
        ELEZIONE: row.ELEZIONE,
        CODICE_COMUNE: row.CODICE_COMUNE,
        LISTA: destination.futureList,
        VOTI: votes * destination.factor
      };
      const key = municipalVoteKey(projectedRow);
      const existing = projectedVotesByKey.get(key);

      if (existing) {
        existing.VOTI += projectedRow.VOTI;
      } else {
        projectedVotesByKey.set(key, projectedRow);
      }
    }
  }

  const electorTotalsByMunicipalityElection = new Map<string, number>();
  for (const row of projectedVotesByKey.values()) {
    const key = `${String(row.CODICE_COMUNE)}\u001f${row.ELEZIONE}`;
    electorTotalsByMunicipalityElection.set(key, (electorTotalsByMunicipalityElection.get(key) ?? 0) + row.VOTI);
  }

  return [...projectedVotesByKey.values()]
    .map((row) => {
      const electors = electorTotalsByMunicipalityElection.get(`${String(row.CODICE_COMUNE)}\u001f${row.ELEZIONE}`) ?? 0;
      const percentage = electors > 0 ? row.VOTI / electors : 0;

      return {
        ...row,
        ELETTORI: electors,
        PERCENTUALE: percentage,
        LOGIT_P: logit(Math.max(percentage, electors > 0 ? 0.5 / electors : 0))
      };
    })
    .sort((left, right) =>
      [String(left.CODICE_COMUNE), left.LISTA, left.DATA, left.ELEZIONE].join('\u001f').localeCompare(
        [String(right.CODICE_COMUNE), right.LISTA, right.DATA, right.ELEZIONE].join('\u001f')
      )
    );
}

function buildListElectionRows(
  projectedVotes: readonly ProjectedHistoricalMunicipalVoteRow[]
): PoliticsScenarioListElectionSnapshotRow[] {
  const totalsByElectionList = new Map<string, PoliticsScenarioListElectionSnapshotRow>();
  const electionTotals = new Map<string, number>();

  for (const row of projectedVotes) {
    const key = [row.DATA, row.ELEZIONE, row.LISTA].join('\u001f');
    const existing = totalsByElectionList.get(key);
    if (existing) {
      existing.VOTI += row.VOTI;
    } else {
      totalsByElectionList.set(key, {
        DATA: row.DATA,
        ELEZIONE: row.ELEZIONE,
        LISTA: row.LISTA,
        VOTI: row.VOTI,
        PERCENTUALE: 0,
        LOGIT_P: 0
      });
    }
    electionTotals.set(row.ELEZIONE, (electionTotals.get(row.ELEZIONE) ?? 0) + row.VOTI);
  }

  return [...totalsByElectionList.values()]
    .map((row) => {
      const electionTotal = electionTotals.get(row.ELEZIONE) ?? 0;
      const percentage = electionTotal > 0 ? row.VOTI / electionTotal : 0;
      return {
        ...row,
        PERCENTUALE: percentage,
        LOGIT_P: logit(percentage)
      };
    })
    .sort((left, right) => [left.DATA, left.ELEZIONE, left.LISTA].join('\u001f').localeCompare([right.DATA, right.ELEZIONE, right.LISTA].join('\u001f')));
}

function rowsByList<T extends { LISTA: string; DATA: string }>(rows: readonly T[]): Map<string, T[]> {
  const grouped = new Map<string, T[]>();

  for (const row of rows) {
    const listRows = grouped.get(row.LISTA) ?? [];
    listRows.push(row);
    grouped.set(row.LISTA, listRows);
  }

  for (const listRows of grouped.values()) {
    listRows.sort((left, right) => dateTime(left.DATA) - dateTime(right.DATA));
  }

  return grouped;
}

function buildGlobalSigmaByList(listeElezioni: readonly PoliticsScenarioListElectionSnapshotRow[]): Map<string, number> {
  const sigmaByList = new Map<string, number>();

  for (const [list, rows] of rowsByList(listeElezioni)) {
    const driftValues: number[] = [];
    for (let index = 1; index < rows.length; index += 1) {
      const days = dateDiffDays(rows[index - 1].DATA, rows[index].DATA);
      if (days <= 0) continue;
      driftValues.push((rows[index].LOGIT_P - rows[index - 1].LOGIT_P) / Math.sqrt(days));
    }
    sigmaByList.set(list, sampleStandardDeviation(driftValues));
  }

  return sigmaByList;
}

function selectStartingRows(
  listeElezioni: readonly PoliticsScenarioListElectionSnapshotRow[],
  percentualiPartenza?: string | null
): Map<string, PoliticsScenarioListElectionSnapshotRow> {
  const selected = new Map<string, PoliticsScenarioListElectionSnapshotRow>();
  const grouped = rowsByList(listeElezioni);

  for (const [list, rows] of grouped) {
    const preferredRows = percentualiPartenza ? rows.filter((row) => row.ELEZIONE.startsWith(percentualiPartenza)) : rows;
    const candidates = preferredRows.length > 0 ? preferredRows : rows;
    selected.set(list, candidates[candidates.length - 1]);
  }

  return selected;
}

function buildMunicipalDeltas(
  projectedVotes: readonly ProjectedHistoricalMunicipalVoteRow[],
  listeElezioni: readonly PoliticsScenarioListElectionSnapshotRow[]
): MunicipalDeltaRow[] {
  const globalByElectionList = new Map(listeElezioni.map((row) => [electionListKey(row), row]));

  return projectedVotes.map((row) => {
    const globalRow = globalByElectionList.get(electionListKey(row));
    return {
      ...row,
      DELTA: globalRow ? row.LOGIT_P - globalRow.LOGIT_P : 0
    };
  });
}

function buildDeltaSigmaByList(rows: readonly MunicipalDeltaRow[]): Map<string, number> {
  const byListCommune = new Map<string, MunicipalDeltaRow[]>();
  const driftByList = new Map<string, number[]>();

  for (const row of rows) {
    const key = `${row.LISTA}\u001f${String(row.CODICE_COMUNE)}`;
    const groupedRows = byListCommune.get(key) ?? [];
    groupedRows.push(row);
    byListCommune.set(key, groupedRows);
  }

  for (const rowsForCommune of byListCommune.values()) {
    rowsForCommune.sort((left, right) => dateTime(left.DATA) - dateTime(right.DATA));
    for (let index = 1; index < rowsForCommune.length; index += 1) {
      const previous = rowsForCommune[index - 1];
      const current = rowsForCommune[index];
      const days = dateDiffDays(previous.DATA, current.DATA);
      if (days <= 0) continue;

      const driftRows = driftByList.get(current.LISTA) ?? [];
      driftRows.push((current.DELTA - previous.DELTA) / Math.sqrt(days));
      driftByList.set(current.LISTA, driftRows);
    }
  }

  return new Map([...driftByList.entries()].map(([list, values]) => [list, sampleStandardDeviation(values)]));
}

function buildMunicipalListRows(
  rows: readonly MunicipalDeltaRow[],
  deltaSigmaByList: ReadonlyMap<string, number>,
  fallbackSigmaDelta: number
): PoliticsMunicipalListParameterRow[] {
  const latestByCommuneList = new Map<string, MunicipalDeltaRow>();

  for (const row of rows) {
    const key = `${String(row.CODICE_COMUNE)}\u001f${row.LISTA}`;
    const existing = latestByCommuneList.get(key);
    if (!existing || dateTime(row.DATA) > dateTime(existing.DATA)) {
      latestByCommuneList.set(key, row);
    }
  }

  return [...latestByCommuneList.values()]
    .map((row) => ({
      CODICE_COMUNE: row.CODICE_COMUNE,
      LISTA: row.LISTA,
      DATA: row.DATA,
      DELTA: row.DELTA,
      SIGMA_DELTA: Number.isFinite(deltaSigmaByList.get(row.LISTA))
        ? (deltaSigmaByList.get(row.LISTA) as number)
        : fallbackSigmaDelta
    }))
    .sort((left, right) =>
      [String(left.CODICE_COMUNE), left.LISTA].join('\u001f').localeCompare([String(right.CODICE_COMUNE), right.LISTA].join('\u001f'))
    );
}

export function buildPoliticsParametersFromHistoricalVotes(
  historicalVotes: readonly PoliticsHistoricalMunicipalListVoteRow[],
  scenario: Scenario,
  options: {
    correspondences?: readonly CorrespondenceInput[];
    percentualiPartenza?: string | null;
  } = {}
): PoliticsParameterPreparationResult {
  const projectedMunicipalVotes = projectHistoricalMunicipalVotes(
    historicalVotes,
    scenario,
    options.correspondences ?? scenario.listCorrespondences
  );
  const listeElezioni = buildListElectionRows(projectedMunicipalVotes);
  const globalSigmaByList = buildGlobalSigmaByList(listeElezioni);
  const startingRowsByList = selectStartingRows(listeElezioni, options.percentualiPartenza);
  const municipalDeltas = buildMunicipalDeltas(projectedMunicipalVotes, listeElezioni);
  const deltaSigmaByList = buildDeltaSigmaByList(municipalDeltas);
  const fallbackSigmaGlobal = meanFinite([...globalSigmaByList.values()]);
  const fallbackSigmaDelta = meanFinite([...deltaSigmaByList.values()]);
  const fallbackData = latestDate(projectedMunicipalVotes);
  const futureLists = [
    ...scenario.lists.map((row) => ({
      LISTA: row.name,
      COALIZIONE: row.coalition
    })),
    {
      LISTA: abstentionListName,
      COALIZIONE: null
    }
  ];
  const liste = futureLists.map((row) => {
    const startingRow = startingRowsByList.get(row.LISTA);
    const percentage = startingRow?.PERCENTUALE ?? 0;
    const sigmaGlobal = globalSigmaByList.get(row.LISTA);

    return {
      LISTA: row.LISTA,
      COALIZIONE: row.COALIZIONE,
      PERCENTUALE: percentage,
      DATA: startingRow?.DATA ?? fallbackData,
      LOGIT_P: startingRow?.LOGIT_P ?? logit(percentage),
      SIGMA_GLOBAL: Number.isFinite(sigmaGlobal) ? (sigmaGlobal as number) : fallbackSigmaGlobal
    };
  });

  return {
    liste,
    comuni_liste: buildMunicipalListRows(municipalDeltas, deltaSigmaByList, fallbackSigmaDelta),
    liste_elezioni: listeElezioni,
    projected_municipal_votes: projectedMunicipalVotes,
    warnings: []
  };
}
