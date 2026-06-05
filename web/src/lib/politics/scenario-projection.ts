import type {
  Scenario,
  ScenarioCandidateTemplate,
  ScenarioList,
  ScenarioLocalShareOverride,
  ScenarioLocalShareOverrideScope
} from '$lib/core/types';
import { plurinominalCandidacyCountSharesToFractions } from '$lib/scenario/politics';
import { buildPoliticsParametersFromHistoricalVotes } from './parameter-preparation';
import type {
  PoliticsBaseDataRow,
  PoliticsCandidatePluriGenerationTemplateRow,
  PoliticsCandidateUniTemplateRow,
  PoliticsHistoricalMunicipalListVoteRow,
  PoliticsMunicipalityCatalogRow,
  PoliticsMunicipalListParameterRow,
  PoliticsPipelineListRow,
  PoliticsPipelineRamoSource,
  PoliticsPipelineSource
} from './types';

export interface PoliticsScenarioProjectionRow {
  list: string;
  coalition: string | null;
  scenarioShare: number | null;
  shareOverride: boolean;
  projectedShare: number | null;
  parameterSource: 'historical' | 'static-snapshot' | 'synthetic';
  status: 'active';
}

export interface PoliticsScenarioProjectionWarning {
  code: string;
  message: string;
  todoReference?: string;
}

export interface PoliticsScenarioProjection {
  source: PoliticsPipelineSource;
  rows: PoliticsScenarioProjectionRow[];
  warnings: PoliticsScenarioProjectionWarning[];
}

interface ActiveScenarioList {
  source: PoliticsPipelineListRow;
  scenario: ScenarioList;
  projectedListName: string;
  parameterSource: PoliticsScenarioProjectionRow['parameterSource'];
}

function listKey(name: string): string {
  return name.trim().toLocaleLowerCase('it-IT');
}

function logit(probability: number): number {
  const bounded = Math.min(Math.max(probability, 1e-9), 1 - 1e-9);
  return Math.log(bounded / (1 - bounded));
}

function logistic(value: number): number {
  return 1 / (1 + Math.exp(-value));
}

function boundedFraction(value: number, fallback: number): number {
  return Number.isFinite(value) ? Math.min(Math.max(value, 0), 0.999999999) : fallback;
}

function meanFinite(values: readonly number[]): number {
  const finiteValues = values.filter(Number.isFinite);
  if (finiteValues.length === 0) return 0;
  return finiteValues.reduce((sum, value) => sum + value, 0) / finiteValues.length;
}

function baseAbstentionFraction(rows: readonly PoliticsPipelineListRow[], politicalTotal: number): number {
  const abstentionRow = rows.find((row) => listKey(row.LISTA) === 'astensione');
  return boundedFraction(abstentionRow?.PERCENTUALE ?? 1 - politicalTotal, Math.max(1 - politicalTotal, 0));
}

function requestElectionDateIso(rawDate: string | undefined, fallback: string): string {
  const candidate = rawDate || fallback;
  const isoCandidate = /^\d{4}-\d{2}-\d{2}$/.test(candidate) ? `${candidate}T00:00:00.000Z` : candidate;
  const parsed = new Date(isoCandidate);
  return Number.isFinite(parsed.getTime()) ? parsed.toISOString() : fallback;
}

function dateOnlyIso(rawDate: string | undefined, fallback: string): string {
  const candidate = rawDate || fallback;
  const isoCandidate = /^\d{4}-\d{2}-\d{2}$/.test(candidate) ? `${candidate}T00:00:00.000Z` : candidate;
  const parsed = new Date(isoCandidate);
  const date = Number.isFinite(parsed.getTime()) ? parsed : new Date(fallback);

  return new Date(Date.UTC(date.getUTCFullYear(), date.getUTCMonth(), date.getUTCDate())).toISOString();
}

function overrideReferenceDateIso(rawCurrentDate: string | undefined, electionDateIso: string): string {
  const referenceDate = dateOnlyIso(rawCurrentDate, new Date().toISOString());
  return Date.parse(referenceDate) > Date.parse(electionDateIso) ? electionDateIso : referenceDate;
}

function uniqueBy<T>(rows: readonly T[], key: (row: T) => string): T[] {
  const seen = new Set<string>();
  const result: T[] = [];

  for (const row of rows) {
    const rowKey = key(row);
    if (seen.has(rowKey)) continue;
    seen.add(rowKey);
    result.push(row);
  }

  return result;
}

function projectPercentages(
  activeLists: readonly ActiveScenarioList[],
  sourcePoliticalTotal: number,
  warnings: PoliticsScenarioProjectionWarning[]
): Map<string, number> {
  const projected = new Map<string, number>();
  const overridden = activeLists.filter((row) => row.scenario.shareOverride);
  const nonOverridden = activeLists.filter((row) => !row.scenario.shareOverride);
  const overrideShareTotal = overridden.reduce((sum, row) => sum + Math.max(Number(row.scenario.startingShare) || 0, 0), 0);
  const nonOverrideSourceTotal = nonOverridden.reduce((sum, row) => sum + row.source.PERCENTUALE, 0);

  if (overridden.length === 0) {
    const activeSourceTotal = activeLists.reduce((sum, row) => sum + row.source.PERCENTUALE, 0);
    for (const row of activeLists) {
      projected.set(row.projectedListName, activeSourceTotal > 0 ? (sourcePoliticalTotal * row.source.PERCENTUALE) / activeSourceTotal : 0);
    }
    return projected;
  }

  if (nonOverridden.length === 0 && overrideShareTotal > 0 && Math.abs(overrideShareTotal - 100) > 1e-9) {
    const shareTotal = Number(overrideShareTotal.toFixed(2));
    warnings.push({
      code: 'POLITICS_SCENARIO_OVERRIDES_RENORMALIZED',
      message: `All active lists have explicit valid-vote share overrides totaling ${shareTotal}%; they were normalized to 100% across active lists before conversion to elector fractions.`,
      todoReference: 'MIGRATION_PLAN.md#current-caveats'
    });

    for (const row of overridden) {
      projected.set(row.projectedListName, (sourcePoliticalTotal * Math.max(Number(row.scenario.startingShare) || 0, 0)) / overrideShareTotal);
    }
    return projected;
  }

  for (const row of overridden) {
    projected.set(row.projectedListName, (sourcePoliticalTotal * Math.max(Number(row.scenario.startingShare) || 0, 0)) / 100);
  }

  const remainingShare = Math.max(100 - overrideShareTotal, 0);
  const remainingPoliticalTotal = (sourcePoliticalTotal * remainingShare) / 100;

  for (const row of nonOverridden) {
    projected.set(
      row.projectedListName,
      nonOverrideSourceTotal > 0 ? (remainingPoliticalTotal * row.source.PERCENTUALE) / nonOverrideSourceTotal : 0
    );
  }

  return projected;
}

function uninominalCandidateKey(coalition: string, code: string | number | null): string {
  return `${coalition}\u001f${String(code ?? '')}`;
}

function plurinominalSlotKey(row: Pick<PoliticsCandidatePluriGenerationTemplateRow, 'PLURI_COD' | 'NUMERO_CANDIDATO' | 'MINORANZA'>): string {
  return [String(row.PLURI_COD ?? ''), String(row.NUMERO_CANDIDATO), String(row.MINORANZA === true)].join('\u001f');
}

function synthesizeRamoSource(
  source: PoliticsPipelineRamoSource,
  activeLists: readonly ActiveScenarioList[],
  activeCoalitions: readonly string[]
): PoliticsPipelineRamoSource {
  const existingUninominalByKey = new Map(
    source.candidati_uni
      .filter((row) => row.COALIZIONE !== null)
      .map((row) => [uninominalCandidateKey(row.COALIZIONE as string, row.UNI_COD), row])
  );
  const uninominalCollegeRows = uniqueBy(source.uni, (row) => String(row.UNI_COD));
  const candidatiUni: PoliticsCandidateUniTemplateRow[] = activeCoalitions.flatMap((coalition) =>
    uninominalCollegeRows.map((row) => {
      const existing = existingUninominalByKey.get(uninominalCandidateKey(coalition, row.UNI_COD));
      return existing
        ? { ...existing, COALIZIONE: coalition, UNI_COD: row.UNI_COD }
        : {
            COALIZIONE: coalition,
            UNI_COD: row.UNI_COD,
            LISTA_MINORANZA: null,
            CANDIDATO_ID: null,
            DATA_NASCITA: null
          };
    })
  );

  const existingPlurinominalByListSlot = new Map(
    source.candidati_pluri.map((row) => [`${listKey(row.LISTA)}\u001f${plurinominalSlotKey(row)}`, row])
  );
  const slotGrid = uniqueBy(source.candidati_pluri, plurinominalSlotKey);
  const candidatiPluri: PoliticsPipelineRamoSource['candidati_pluri'] = activeLists.flatMap((active) =>
    slotGrid.map((slot) => {
      const existing = existingPlurinominalByListSlot.get(`${listKey(active.projectedListName)}\u001f${plurinominalSlotKey(slot)}`);
      return existing
        ? { ...existing, LISTA: active.projectedListName }
        : {
            CIRC_COD: slot.CIRC_COD,
            LISTA: active.projectedListName,
            PLURI_COD: slot.PLURI_COD,
            NUMERO_CANDIDATO: slot.NUMERO_CANDIDATO,
            MINORANZA: slot.MINORANZA,
            CANDIDATO_ID: null,
            DATA_NASCITA: null
          };
    })
  );

  return {
    ...source,
    candidati_uni: candidatiUni,
    candidati_pluri: candidatiPluri
  };
}

function candidateSlotCode(value: string | number | null | undefined): string {
  return String(value ?? '').trim();
}

function candidateTemplateBirthDate(template: ScenarioCandidateTemplate, fallback: string | null): string | null {
  return template.birthDate ? dateOnlyIso(template.birthDate, fallback ?? template.birthDate) : fallback;
}

function uninominalCandidateTemplateKey(template: ScenarioCandidateTemplate): string {
  return `${template.ramo}\u001f${template.coalition?.trim() ?? ''}\u001f${candidateSlotCode(template.uninominalCode)}`;
}

function uninominalCandidateRowKey(ramo: string, row: PoliticsCandidateUniTemplateRow): string {
  return `${ramo}\u001f${row.COALIZIONE ?? ''}\u001f${candidateSlotCode(row.UNI_COD)}`;
}

function plurinominalCandidateTemplateKey(template: ScenarioCandidateTemplate): string {
  return [
    template.ramo,
    template.list?.trim() ?? '',
    candidateSlotCode(template.plurinominalCode),
    String(template.candidateNumber ?? ''),
    String(template.minority === true)
  ].join('\u001f');
}

function plurinominalCandidateRowKey(ramo: string, row: PoliticsCandidatePluriGenerationTemplateRow): string {
  return [ramo, row.LISTA, candidateSlotCode(row.PLURI_COD), String(row.NUMERO_CANDIDATO), String(row.MINORANZA === true)].join(
    '\u001f'
  );
}

function candidateTemplateLabel(template: ScenarioCandidateTemplate): string {
  if (template.kind === 'uninominal') {
    return `${template.ramo} uninominal ${template.coalition ?? 'coalition'} / ${template.uninominalCode ?? 'college'}`;
  }
  return `${template.ramo} plurinominal ${template.list ?? 'list'} / ${template.plurinominalCode ?? 'college'} #${
    template.candidateNumber ?? '?'
  }`;
}

function applyCandidateTemplatesToRamo(
  ramo: 'camera' | 'senato',
  source: PoliticsPipelineRamoSource,
  templates: readonly ScenarioCandidateTemplate[],
  warnings: PoliticsScenarioProjectionWarning[]
): PoliticsPipelineRamoSource {
  const relevantTemplates = templates.filter((template) => template.ramo === ramo);
  if (relevantTemplates.length === 0) return source;

  const uninominalTemplates = new Map(
    relevantTemplates
      .filter((template) => template.kind === 'uninominal' && template.candidateName.trim())
      .map((template) => [uninominalCandidateTemplateKey(template), template])
  );
  const plurinominalTemplates = new Map(
    relevantTemplates
      .filter((template) => template.kind === 'plurinominal' && template.candidateName.trim())
      .map((template) => [plurinominalCandidateTemplateKey(template), template])
  );
  const usedTemplateIds = new Set<string>();

  const candidatiUni = source.candidati_uni.map((row) => {
    const template = uninominalTemplates.get(uninominalCandidateRowKey(ramo, row));
    if (!template) return row;

    usedTemplateIds.add(template.id);
    return {
      ...row,
      CANDIDATO_ID: template.candidateName.trim(),
      DATA_NASCITA: candidateTemplateBirthDate(template, row.DATA_NASCITA)
    };
  });
  const candidatiPluri = source.candidati_pluri.map((row) => {
    const template = plurinominalTemplates.get(plurinominalCandidateRowKey(ramo, row));
    if (!template) return row;

    usedTemplateIds.add(template.id);
    return {
      ...row,
      CANDIDATO_ID: template.candidateName.trim(),
      DATA_NASCITA: candidateTemplateBirthDate(template, row.DATA_NASCITA)
    };
  });
  const unused = relevantTemplates.filter((template) => !usedTemplateIds.has(template.id));

  if (unused.length > 0) {
    warnings.push({
      code: 'POLITICS_SCENARIO_CANDIDATE_TEMPLATES_UNUSED',
      message: `Some candidate templates did not match active ${ramo} candidate slots and were ignored: ${unused
        .map(candidateTemplateLabel)
        .join(', ')}.`,
      todoReference: 'MIGRATION_PLAN.md#current-caveats'
    });
  }

  return {
    ...source,
    candidati_uni: candidatiUni,
    candidati_pluri: candidatiPluri
  };
}

function localShareOverrideKey(scope: ScenarioLocalShareOverride['scope'], locationCode: string | number): string {
  return `${scope}\u001f${String(locationCode).trim()}`;
}

function localOverrideLabel(scope: ScenarioLocalShareOverride['scope'], locationCode: string): string {
  if (scope === 'region') return `region ${locationCode}`;
  if (scope === 'province') return `province ${locationCode}`;
  return `municipality ${locationCode}`;
}

function localScopeRank(scope: ScenarioLocalShareOverrideScope): number {
  if (scope === 'region') return 0;
  if (scope === 'province') return 1;
  return 2;
}

function projectedMunicipalRows(
  rows: readonly PoliticsMunicipalListParameterRow[],
  projectedListRows: readonly PoliticsPipelineListRow[],
  baseRows: readonly PoliticsBaseDataRow[]
): PoliticsMunicipalListParameterRow[] {
  const projectedListByKey = new Map(projectedListRows.map((row) => [listKey(row.LISTA), row]));
  const filteredRows = rows.filter((row) => projectedListByKey.has(listKey(row.LISTA))).map((row) => ({ ...row }));
  const rowsByKey = new Map(filteredRows.map((row) => [`${String(row.CODICE_COMUNE)}\u001f${listKey(row.LISTA)}`, row]));
  const municipalityCodes = new Map<string, PoliticsMunicipalListParameterRow['CODICE_COMUNE']>();
  for (const row of baseRows) municipalityCodes.set(String(row.CODICE_COMUNE), row.CODICE_COMUNE);
  for (const row of filteredRows) municipalityCodes.set(String(row.CODICE_COMUNE), row.CODICE_COMUNE);
  const fallbackSigmaDelta = meanFinite(filteredRows.map((row) => row.SIGMA_DELTA));

  for (const [municipalityKey, municipalityCode] of municipalityCodes) {
    for (const listRow of projectedListRows) {
      const key = `${municipalityKey}\u001f${listKey(listRow.LISTA)}`;
      if (rowsByKey.has(key)) continue;

      rowsByKey.set(key, {
        CODICE_COMUNE: municipalityCode,
        LISTA: listRow.LISTA,
        DATA: listRow.DATA,
        DELTA: 0,
        SIGMA_DELTA: fallbackSigmaDelta
      });
    }
  }

  return [...rowsByKey.values()].sort((left, right) =>
    [String(left.CODICE_COMUNE), left.LISTA].join('\u001f').localeCompare([String(right.CODICE_COMUNE), right.LISTA].join('\u001f'))
  );
}

function localBaseFractions(
  rows: readonly PoliticsMunicipalListParameterRow[],
  projectedGlobalByList: ReadonlyMap<string, PoliticsPipelineListRow>
): Map<string, number> {
  const rawByList = new Map<string, number>();
  let total = 0;

  for (const row of rows) {
    const projectedGlobal = projectedGlobalByList.get(listKey(row.LISTA));
    if (!projectedGlobal) continue;

    const raw = logistic(projectedGlobal.LOGIT_P + row.DELTA);
    rawByList.set(row.LISTA, raw);
    total += raw;
  }

  return new Map([...rawByList.entries()].map(([list, raw]) => [list, total > 0 ? raw / total : 0]));
}

interface LocalOverrideAreaApplication {
  scope: ScenarioLocalShareOverrideScope;
  locationCode: string;
  municipalityCodes: Set<string>;
}

interface LocalMunicipalityState {
  code: string;
  rows: PoliticsMunicipalListParameterRow[];
  fractions: Map<string, number>;
  abstentionFraction: number;
  politicalTotal: number;
  weight: number;
  validShares: Map<string, number>;
}

function rowMunicipalityListKey(row: Pick<PoliticsMunicipalListParameterRow, 'CODICE_COMUNE' | 'LISTA'>): string {
  return `${String(row.CODICE_COMUNE)}\u001f${row.LISTA}`;
}

function groupRowsByMunicipality(
  rows: readonly PoliticsMunicipalListParameterRow[]
): Map<string, PoliticsMunicipalListParameterRow[]> {
  const rowsByMunicipality = new Map<string, PoliticsMunicipalListParameterRow[]>();

  for (const row of rows) {
    const key = String(row.CODICE_COMUNE);
    const grouped = rowsByMunicipality.get(key) ?? [];
    grouped.push(row);
    rowsByMunicipality.set(key, grouped);
  }

  return rowsByMunicipality;
}

function electorsByMunicipality(rows: readonly PoliticsBaseDataRow[]): Map<string, number> {
  const electors = new Map<string, number>();
  for (const row of rows) {
    const key = String(row.CODICE_COMUNE);
    electors.set(key, (electors.get(key) ?? 0) + (Number(row.ELETTORI) || 0));
  }
  return electors;
}

function municipalityCatalogByCode(
  rows: readonly PoliticsMunicipalityCatalogRow[] | undefined
): Map<string, PoliticsMunicipalityCatalogRow> {
  return new Map((rows ?? []).map((row) => [String(row.CODICE_COMUNE), row]));
}

function overrideMunicipalityCodes(
  scope: ScenarioLocalShareOverrideScope,
  locationCode: string,
  rowsByMunicipality: ReadonlyMap<string, PoliticsMunicipalListParameterRow[]>,
  municipalitiesByCode: ReadonlyMap<string, PoliticsMunicipalityCatalogRow>
): string[] {
  if (scope === 'municipality') {
    return rowsByMunicipality.has(locationCode) ? [locationCode] : [];
  }

  if (municipalitiesByCode.size === 0) return [];

  return [...rowsByMunicipality.keys()].filter((code) => {
    const municipality = municipalitiesByCode.get(code);
    if (!municipality) return false;
    return scope === 'province'
      ? String(municipality.CODICE_PROVINCIA) === locationCode
      : String(municipality.CODICE_REGIONE) === locationCode;
  });
}

function activePoliticalLists(
  projectedGlobalByList: ReadonlyMap<string, PoliticsPipelineListRow>,
  rows: readonly PoliticsMunicipalListParameterRow[]
): string[] {
  const active = new Set<string>();
  for (const row of rows) {
    const key = listKey(row.LISTA);
    if (key === 'astensione' || !projectedGlobalByList.has(key)) continue;
    active.add(row.LISTA);
  }
  return [...active].sort((left, right) => listKey(left).localeCompare(listKey(right), 'it'));
}

function normalizeValidShares(shares: Map<string, number>, activeLists: readonly string[]): Map<string, number> {
  const epsilon = 1e-9;
  const raw = new Map(activeLists.map((list) => [list, Math.max(shares.get(list) ?? 0, epsilon)]));
  const total = [...raw.values()].reduce((sum, value) => sum + value, 0);
  return new Map([...raw.entries()].map(([list, value]) => [list, total > 0 ? value / total : 1 / Math.max(activeLists.length, 1)]));
}

function localMunicipalityStates(
  municipalityCodes: readonly string[],
  rowsByMunicipality: ReadonlyMap<string, PoliticsMunicipalListParameterRow[]>,
  projectedGlobalByList: ReadonlyMap<string, PoliticsPipelineListRow>,
  electors: ReadonlyMap<string, number>,
  activeLists: readonly string[]
): LocalMunicipalityState[] {
  return municipalityCodes.flatMap((code) => {
    const rows = rowsByMunicipality.get(code);
    if (!rows) return [];

    const fractions = localBaseFractions(rows, projectedGlobalByList);
    const abstentionFraction = boundedFraction(fractions.get('astensione') ?? 0, 0);
    const politicalTotal = Math.max(1 - abstentionFraction, 0);
    const validShares = new Map(
      activeLists.map((list) => [list, politicalTotal > 0 ? (fractions.get(list) ?? 0) / politicalTotal : 0])
    );
    const normalizedShares = normalizeValidShares(validShares, activeLists);
    const weight = Math.max((electors.get(code) ?? 0) * politicalTotal, 0);

    return [
      {
        code,
        rows,
        fractions,
        abstentionFraction,
        politicalTotal,
        weight,
        validShares: normalizedShares
      }
    ];
  });
}

function weightedTotal(states: readonly LocalMunicipalityState[]): number {
  const total = states.reduce((sum, state) => sum + state.weight, 0);
  return total > 0 ? total : states.length;
}

function aggregateValidShares(
  states: readonly LocalMunicipalityState[],
  activeLists: readonly string[],
  adjustedShares?: ReadonlyMap<string, ReadonlyMap<string, number>>
): Map<string, number> {
  const totalWeight = weightedTotal(states);
  const aggregate = new Map(activeLists.map((list) => [list, 0]));

  for (const state of states) {
    const weight = state.weight > 0 ? state.weight : 1;
    const shares = adjustedShares?.get(state.code) ?? state.validShares;
    for (const list of activeLists) {
      aggregate.set(list, (aggregate.get(list) ?? 0) + (weight * (shares.get(list) ?? 0)) / totalWeight);
    }
  }

  return aggregate;
}

function areaTargetValidShares(
  overrides: readonly ScenarioLocalShareOverride[],
  activeLists: readonly string[],
  currentAggregate: ReadonlyMap<string, number>,
  scope: ScenarioLocalShareOverrideScope,
  locationCode: string,
  warnings: PoliticsScenarioProjectionWarning[]
): Map<string, number> | null {
  const overrideByList = new Map(overrides.map((override) => [listKey(override.list), override]));
  const overriddenLists = activeLists.filter((list) => overrideByList.has(listKey(list)));
  const nonOverriddenLists = activeLists.filter((list) => !overrideByList.has(listKey(list)));
  const overrideShareTotal = overriddenLists.reduce(
    (sum, list) => sum + Math.max(Number(overrideByList.get(listKey(list))?.startingShare) || 0, 0),
    0
  );
  const target = new Map<string, number>();

  if (overriddenLists.length === 0) {
    warnings.push({
      code: 'POLITICS_SCENARIO_LOCAL_OVERRIDES_UNUSED',
      message: `Local share overrides for ${localOverrideLabel(scope, locationCode)} did not match active scenario lists and were ignored.`,
      todoReference: 'MIGRATION_PLAN.md#current-caveats'
    });
    return null;
  }

  if (nonOverriddenLists.length === 0 && overrideShareTotal > 0 && Math.abs(overrideShareTotal - 100) > 1e-9) {
    warnings.push({
      code: 'POLITICS_SCENARIO_LOCAL_OVERRIDES_RENORMALIZED',
      message: `All active lists have explicit local valid-vote share overrides totaling ${Number(
        overrideShareTotal.toFixed(2)
      )}% in ${localOverrideLabel(scope, locationCode)}; they were normalized to 100% before conversion to elector fractions.`,
      todoReference: 'MIGRATION_PLAN.md#current-caveats'
    });

    for (const list of overriddenLists) {
      target.set(list, Math.max(Number(overrideByList.get(listKey(list))?.startingShare) || 0, 0) / overrideShareTotal);
    }
    return target;
  }

  for (const list of overriddenLists) {
    target.set(list, Math.max(Number(overrideByList.get(listKey(list))?.startingShare) || 0, 0) / 100);
  }

  const remainingShare = Math.max(1 - overrideShareTotal / 100, 0);
  const nonOverrideCurrentTotal = nonOverriddenLists.reduce((sum, list) => sum + (currentAggregate.get(list) ?? 0), 0);

  for (const list of nonOverriddenLists) {
    target.set(
      list,
      nonOverrideCurrentTotal > 0 ? (remainingShare * (currentAggregate.get(list) ?? 0)) / nonOverrideCurrentTotal : remainingShare / nonOverriddenLists.length
    );
  }

  return target;
}

function calibratedMunicipalityValidShares(
  states: readonly LocalMunicipalityState[],
  activeLists: readonly string[],
  targetAggregate: ReadonlyMap<string, number>
): Map<string, Map<string, number>> {
  const factors = new Map(activeLists.map((list) => [list, 1]));

  const sharesFromFactors = (): Map<string, Map<string, number>> => {
    const result = new Map<string, Map<string, number>>();
    for (const state of states) {
      const denominator = activeLists.reduce(
        (sum, list) => sum + (state.validShares.get(list) ?? 0) * (factors.get(list) ?? 0),
        0
      );
      const shares = new Map<string, number>();
      for (const list of activeLists) {
        shares.set(
          list,
          denominator > 0 ? ((state.validShares.get(list) ?? 0) * (factors.get(list) ?? 0)) / denominator : targetAggregate.get(list) ?? 0
        );
      }
      result.set(state.code, shares);
    }
    return result;
  };

  for (let iteration = 0; iteration < 80; iteration += 1) {
    const current = aggregateValidShares(states, activeLists, sharesFromFactors());
    for (const list of activeLists) {
      const target = targetAggregate.get(list) ?? 0;
      const actual = current.get(list) ?? 0;
      if (target <= 0) {
        factors.set(list, 0);
      } else if (actual > 0) {
        factors.set(list, (factors.get(list) ?? 1) * (target / actual));
      }
    }
  }

  return sharesFromFactors();
}

function replaceAdjustedRows(
  rows: readonly PoliticsMunicipalListParameterRow[],
  adjustedRows: ReadonlyMap<string, PoliticsMunicipalListParameterRow>
): PoliticsMunicipalListParameterRow[] {
  if (adjustedRows.size === 0) return rows.map((row) => ({ ...row }));
  return rows.map((row) => adjustedRows.get(rowMunicipalityListKey(row)) ?? { ...row });
}

function applyAreaOverride(
  rows: readonly PoliticsMunicipalListParameterRow[],
  municipalityCodes: readonly string[],
  overrides: readonly ScenarioLocalShareOverride[],
  scope: ScenarioLocalShareOverrideScope,
  locationCode: string,
  projectedGlobalByList: ReadonlyMap<string, PoliticsPipelineListRow>,
  electors: ReadonlyMap<string, number>,
  overrideReferenceDate: string,
  warnings: PoliticsScenarioProjectionWarning[]
): PoliticsMunicipalListParameterRow[] {
  const rowsByMunicipality = groupRowsByMunicipality(rows);
  const municipalityRows = municipalityCodes.flatMap((code) => rowsByMunicipality.get(code) ?? []);
  const activeLists = activePoliticalLists(projectedGlobalByList, municipalityRows);
  const states = localMunicipalityStates(
    municipalityCodes,
    rowsByMunicipality,
    projectedGlobalByList,
    electors,
    activeLists
  );
  const currentAggregate = aggregateValidShares(states, activeLists);
  const targetAggregate = areaTargetValidShares(overrides, activeLists, currentAggregate, scope, locationCode, warnings);

  if (!targetAggregate || states.length === 0 || activeLists.length === 0) return rows.map((row) => ({ ...row }));

  const calibratedShares = calibratedMunicipalityValidShares(states, activeLists, targetAggregate);
  const adjustedRows = new Map<string, PoliticsMunicipalListParameterRow>();

  for (const state of states) {
    const validShares = calibratedShares.get(state.code) ?? state.validShares;

    for (const row of state.rows) {
      const projectedGlobal = projectedGlobalByList.get(listKey(row.LISTA));
      if (!projectedGlobal) continue;

      const target =
        listKey(row.LISTA) === 'astensione'
          ? state.abstentionFraction
          : state.politicalTotal * (validShares.get(row.LISTA) ?? 0);

      adjustedRows.set(rowMunicipalityListKey(row), {
        ...row,
        DATA: overrideReferenceDate,
        DELTA: logit(target) - projectedGlobal.LOGIT_P
      });
    }
  }

  return replaceAdjustedRows(rows, adjustedRows);
}

function warnIfLocalOverrideOverlaps(
  appliedAreas: readonly LocalOverrideAreaApplication[],
  scope: ScenarioLocalShareOverrideScope,
  locationCode: string,
  municipalityCodes: ReadonlySet<string>,
  warnings: PoliticsScenarioProjectionWarning[]
): void {
  const broader = appliedAreas.find(
    (area) => localScopeRank(area.scope) < localScopeRank(scope) && [...municipalityCodes].some((code) => area.municipalityCodes.has(code))
  );
  if (!broader) return;

  warnings.push({
    code: 'POLITICS_SCENARIO_LOCAL_OVERRIDES_OVERLAP',
    message: `Local share overrides for ${localOverrideLabel(scope, locationCode)} overlap broader ${localOverrideLabel(
      broader.scope,
      broader.locationCode
    )}; the more specific override was applied later, so the broader aggregate may no longer match exactly.`,
    todoReference: 'MIGRATION_PLAN.md#current-caveats'
  });
}

function applyLocalShareOverrides(
  rows: readonly PoliticsMunicipalListParameterRow[],
  projectedListRows: readonly PoliticsPipelineListRow[],
  baseRows: readonly PoliticsBaseDataRow[],
  municipalities: readonly PoliticsMunicipalityCatalogRow[] | undefined,
  localOverrides: readonly ScenarioLocalShareOverride[],
  overrideReferenceDate: string,
  warnings: PoliticsScenarioProjectionWarning[]
): PoliticsMunicipalListParameterRow[] {
  if (localOverrides.length === 0) return rows.map((row) => ({ ...row }));

  const projectedGlobalByList = new Map(projectedListRows.map((row) => [listKey(row.LISTA), row]));
  const municipalitiesByCode = municipalityCatalogByCode(municipalities);
  const electors = electorsByMunicipality(baseRows);
  const overridesByLocation = new Map<string, ScenarioLocalShareOverride[]>();
  let currentRows = rows.map((row) => ({ ...row }));
  const appliedAreas: LocalOverrideAreaApplication[] = [];

  for (const override of localOverrides) {
    const key = localShareOverrideKey(override.scope, override.locationCode);
    const grouped = overridesByLocation.get(key) ?? [];
    grouped.push(override);
    overridesByLocation.set(key, grouped);
  }

  for (const scope of ['region', 'province', 'municipality'] as const) {
    const scopedOverrides = [...overridesByLocation.entries()]
      .filter(([key]) => key.startsWith(`${scope}\u001f`))
      .sort(([left], [right]) => left.localeCompare(right, 'it'));

    for (const [locationKey, overrides] of scopedOverrides) {
      const [, locationCode] = locationKey.split('\u001f');
      const rowsByMunicipality = groupRowsByMunicipality(currentRows);
      const municipalityCodes = overrideMunicipalityCodes(scope, locationCode, rowsByMunicipality, municipalitiesByCode);

      if (municipalityCodes.length === 0) {
        warnings.push({
          code: 'POLITICS_SCENARIO_LOCAL_OVERRIDES_UNUSED',
          message: `Local share overrides for ${localOverrideLabel(scope, locationCode)} did not match the current scenario geography and were ignored.`,
          todoReference: 'MIGRATION_PLAN.md#current-caveats'
        });
        continue;
      }

      const municipalityCodeSet = new Set(municipalityCodes);
      warnIfLocalOverrideOverlaps(appliedAreas, scope, locationCode, municipalityCodeSet, warnings);
      currentRows = applyAreaOverride(
        currentRows,
        municipalityCodes,
        overrides,
        scope,
        locationCode,
        projectedGlobalByList,
        electors,
        overrideReferenceDate,
        warnings
      );
      appliedAreas.push({
        scope,
        locationCode,
        municipalityCodes: municipalityCodeSet
      });
    }
  }

  return currentRows;
}

function buildHistoricalParameterSource(
  scenario: Scenario,
  options: {
    historicalVotes?: readonly PoliticsHistoricalMunicipalListVoteRow[];
    parameterPercentualiPartenza?: string | null;
  }
): Pick<PoliticsPipelineSource, 'liste' | 'comuni_liste'> | null {
  if (!options.historicalVotes || options.historicalVotes.length === 0 || scenario.lists.length === 0) return null;

  const parameters = buildPoliticsParametersFromHistoricalVotes(options.historicalVotes, scenario, {
    percentualiPartenza: options.parameterPercentualiPartenza ?? null
  });

  return {
    liste: parameters.liste,
    comuni_liste: parameters.comuni_liste
  };
}

export function projectScenarioOntoPoliticsSource(
  source: PoliticsPipelineSource,
  scenario: Scenario,
  options: {
    simulations: number;
    electionDate?: string;
    currentDate?: string;
    historicalVotes?: readonly PoliticsHistoricalMunicipalListVoteRow[];
    municipalities?: readonly PoliticsMunicipalityCatalogRow[];
    parameterPercentualiPartenza?: string | null;
  }
): PoliticsScenarioProjection {
  const warnings: PoliticsScenarioProjectionWarning[] = [];
  const electionDateIso = requestElectionDateIso(options.electionDate ?? scenario.electionDate, source.data_elezione);
  const overrideReferenceDate = overrideReferenceDateIso(options.currentDate, electionDateIso);
  const sourcePoliticalRows = source.liste.filter((row) => row.LISTA !== 'astensione');
  const sourcePoliticalTotal = sourcePoliticalRows.reduce((sum, row) => sum + row.PERCENTUALE, 0);
  const projectedCandidateGeneration = {
    frazione_uni_in_pluri: scenario.candidateGeneration.uninominalToPlurinominalShare,
    frazioni_pluricandidature: plurinominalCandidacyCountSharesToFractions(
      scenario.candidateGeneration.plurinominalCandidacyCountShares
    )
  };
  const historicalParameterSource = buildHistoricalParameterSource(scenario, options);
  const baseListRows = historicalParameterSource?.liste ?? source.liste;
  const baseMunicipalRows = historicalParameterSource?.comuni_liste ?? source.comuni_liste;
  const baseListByKey = new Map(baseListRows.map((row) => [listKey(row.LISTA), row]));
  const sourceListByKey = new Map(source.liste.map((row) => [listKey(row.LISTA), row]));
  const fallbackListDate = baseListRows[0]?.DATA ?? source.liste[0]?.DATA ?? electionDateIso;
  const fallbackSigmaGlobal = meanFinite(baseListRows.map((row) => row.SIGMA_GLOBAL));
  const activeLists: ActiveScenarioList[] = scenario.lists.map((scenarioRow) => {
    const baseParameter = baseListByKey.get(listKey(scenarioRow.name));
    const sourceParameter = sourceListByKey.get(listKey(scenarioRow.name));
    const parameter = baseParameter ?? sourceParameter ?? {
      LISTA: scenarioRow.name,
      COALIZIONE: scenarioRow.coalition,
      DATA: fallbackListDate,
      LOGIT_P: logit(0),
      SIGMA_GLOBAL: fallbackSigmaGlobal,
      PERCENTUALE: 0
    };

    return {
      source: {
        ...parameter,
        LISTA: scenarioRow.name,
        COALIZIONE: scenarioRow.coalition
      },
      scenario: scenarioRow,
      projectedListName: scenarioRow.name,
      parameterSource: historicalParameterSource ? 'historical' : sourceParameter ? 'static-snapshot' : 'synthetic'
    };
  });
  const projectionRows: PoliticsScenarioProjectionRow[] = [];

  if (activeLists.length === 0) {
    warnings.push({
      code: 'POLITICS_SCENARIO_NO_LISTS',
      message: 'No scenario list is available, so the bundled default source was used unchanged.',
      todoReference: 'MIGRATION_PLAN.md#current-caveats'
    });

    return {
      source: {
        ...source,
        data_elezione: electionDateIso,
        simulazioni: options.simulations,
        ...projectedCandidateGeneration
      },
      rows: [
        ...sourcePoliticalRows.map((row) => ({
          list: row.LISTA,
          coalition: row.COALIZIONE,
          scenarioShare: null,
          shareOverride: false,
          projectedShare: sourcePoliticalTotal > 0 ? (100 * row.PERCENTUALE) / sourcePoliticalTotal : 0,
          parameterSource: 'static-snapshot' as const,
          status: 'active' as const
        }))
      ],
      warnings
    };
  }

  const basePoliticalTotal = baseListRows
    .filter((row) => row.LISTA !== 'astensione' && scenario.lists.some((list) => listKey(list.name) === listKey(row.LISTA)))
    .reduce((sum, row) => sum + row.PERCENTUALE, 0);
  const targetAbstention = scenario.abstentionOverride
    ? boundedFraction(scenario.abstentionShare / 100, baseAbstentionFraction(baseListRows, basePoliticalTotal))
    : baseAbstentionFraction(baseListRows, basePoliticalTotal);
  const projectionPoliticalTotal =
    scenario.abstentionOverride || historicalParameterSource ? Math.max(1 - targetAbstention, 0) : sourcePoliticalTotal;
  const projectedPercentages = projectPercentages(
    activeLists,
    projectionPoliticalTotal,
    warnings
  );
  const activeCoalitions = [
    ...new Set(
      activeLists
        .map((row) => row.scenario.coalition)
        .filter((coalition): coalition is string => coalition !== null && coalition !== '')
    )
  ];
  const baseAbstentionRow =
    baseListRows.find((row) => listKey(row.LISTA) === 'astensione') ??
    source.liste.find((row) => listKey(row.LISTA) === 'astensione') ?? {
      LISTA: 'astensione',
      COALIZIONE: null,
      DATA: fallbackListDate,
      LOGIT_P: logit(targetAbstention),
      SIGMA_GLOBAL: fallbackSigmaGlobal,
      PERCENTUALE: targetAbstention
    };
  const projectedLists: PoliticsPipelineListRow[] = [
    ...activeLists.map((active) => {
      const percentage = projectedPercentages.get(active.projectedListName) ?? active.source.PERCENTUALE;
      return {
        ...active.source,
        LISTA: active.projectedListName,
        COALIZIONE: active.scenario.coalition,
        DATA: active.scenario.shareOverride ? overrideReferenceDate : active.source.DATA,
        PERCENTUALE: percentage,
        LOGIT_P: logit(percentage)
      };
    }),
    {
      ...baseAbstentionRow,
      LISTA: 'astensione',
      COALIZIONE: null,
      PERCENTUALE: targetAbstention,
      LOGIT_P: logit(targetAbstention),
      SIGMA_GLOBAL: scenario.abstentionOverride ? 0 : baseAbstentionRow.SIGMA_GLOBAL
    }
  ];
  const camera = applyCandidateTemplatesToRamo(
    'camera',
    synthesizeRamoSource(source.camera, activeLists, activeCoalitions),
    scenario.candidateTemplates,
    warnings
  );
  const senato = applyCandidateTemplatesToRamo(
    'senato',
    synthesizeRamoSource(source.senato, activeLists, activeCoalitions),
    scenario.candidateTemplates,
    warnings
  );
  const projectedMunicipalParameterRows = applyLocalShareOverrides(
    projectedMunicipalRows(baseMunicipalRows, projectedLists, source.base_dati),
    projectedLists,
    source.base_dati,
    options.municipalities,
    scenario.localShareOverrides,
    overrideReferenceDate,
    warnings
  );

  for (const active of activeLists) {
    const percentage = projectedPercentages.get(active.projectedListName) ?? active.source.PERCENTUALE;
    projectionRows.push({
      list: active.projectedListName,
      coalition: active.scenario.coalition,
      scenarioShare: active.scenario.startingShare,
      shareOverride: active.scenario.shareOverride,
      projectedShare: projectionPoliticalTotal > 0 ? (100 * percentage) / projectionPoliticalTotal : 0,
      parameterSource: active.parameterSource,
      status: 'active'
    });
  }

  return {
    source: {
      ...source,
      data_elezione: electionDateIso,
      simulazioni: options.simulations,
      ...projectedCandidateGeneration,
      liste: projectedLists,
      comuni_liste: projectedMunicipalParameterRows,
      camera,
      senato
    },
    rows: projectionRows,
    warnings
  };
}
