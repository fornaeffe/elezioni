import type { Scenario, ScenarioList, ScenarioLocalShareOverride, ScenarioLocalShareOverrideScope } from '$lib/core/types';
import { buildPoliticsParametersFromHistoricalVotes } from '$lib/politics/parameter-preparation';
import type {
  RegionalErHistoricalMunicipalListVoteRow,
  RegionalErListParameterRow,
  RegionalErMunicipalListParameterRow,
  RegionalErMunicipalityRow,
  RegionalErPipelineSource,
  RegionalErScenarioProjection,
  RegionalErScenarioProjectionRow,
  RegionalErScenarioProjectionWarning
} from './types';

interface ActiveScenarioList {
  source: RegionalErListParameterRow;
  scenario: ScenarioList;
  projectedListName: string;
  parameterSource: RegionalErScenarioProjectionRow['parameterSource'];
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

function baseAbstentionFraction(rows: readonly RegionalErListParameterRow[], politicalTotal: number): number {
  const abstentionRow = rows.find((row) => listKey(row.LISTA) === 'astensione');
  return boundedFraction(abstentionRow?.PERCENTUALE ?? 1 - politicalTotal, Math.max(1 - politicalTotal, 0));
}

function projectPercentages(
  activeLists: readonly ActiveScenarioList[],
  sourcePoliticalTotal: number,
  warnings: RegionalErScenarioProjectionWarning[]
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
    warnings.push({
      code: 'REGIONAL_ER_SCENARIO_OVERRIDES_RENORMALIZED',
      message: `All active regional lists have explicit valid-vote share overrides totaling ${Number(
        overrideShareTotal.toFixed(2)
      )}%; they were normalized to 100%.`,
      todoReference: 'MIGRATION_PLAN.md#next-work'
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

function buildHistoricalParameterSource(
  scenario: Scenario,
  historicalVotes?: readonly RegionalErHistoricalMunicipalListVoteRow[]
): {
  liste: RegionalErListParameterRow[];
  comuni_liste: Pick<RegionalErMunicipalListParameterRow, 'CODICE_COMUNE' | 'LISTA' | 'DATA' | 'DELTA' | 'SIGMA_DELTA'>[];
} | null {
  if (!historicalVotes || historicalVotes.length === 0 || scenario.lists.length === 0) return null;

  const parameters = buildPoliticsParametersFromHistoricalVotes(historicalVotes, scenario, {
    percentualiPartenza: null
  });

  return {
    liste: parameters.liste.map((row) => ({
      LISTA: row.LISTA,
      COALIZIONE: row.COALIZIONE,
      PERCENTUALE: row.PERCENTUALE,
      DATA: row.DATA,
      LOGIT_P: row.LOGIT_P,
      SIGMA_GLOBAL: row.SIGMA_GLOBAL
    })),
    comuni_liste: parameters.comuni_liste
  };
}

function sourceMunicipalRowKey(row: Pick<RegionalErMunicipalListParameterRow, 'CODICE_COMUNE' | 'LISTA'>): string {
  return `${String(row.CODICE_COMUNE)}\u001f${listKey(row.LISTA)}`;
}

function enrichMunicipalRows(
  rows: readonly Pick<RegionalErMunicipalListParameterRow, 'CODICE_COMUNE' | 'LISTA' | 'DATA' | 'DELTA' | 'SIGMA_DELTA'>[],
  municipalities: readonly RegionalErMunicipalityRow[],
  sourceRows: readonly RegionalErMunicipalListParameterRow[]
): RegionalErMunicipalListParameterRow[] {
  const municipalityByCode = new Map(municipalities.map((row) => [String(row.CODICE_COMUNE), row]));
  const sourceByKey = new Map(sourceRows.map((row) => [sourceMunicipalRowKey(row), row]));

  return rows.flatMap((row) => {
    const source = sourceByKey.get(sourceMunicipalRowKey(row));
    const municipality = municipalityByCode.get(String(row.CODICE_COMUNE));
    if (!source && !municipality) return [];

    return [
      {
        CODICE_COMUNE: row.CODICE_COMUNE,
        COMUNE: source?.COMUNE ?? municipality?.COMUNE ?? String(row.CODICE_COMUNE),
        CODICE_PROVINCIA: source?.CODICE_PROVINCIA ?? municipality?.CODICE_PROVINCIA ?? '',
        PROVINCIA: source?.PROVINCIA ?? municipality?.PROVINCIA ?? '',
        CODICE_REGIONE: source?.CODICE_REGIONE ?? municipality?.CODICE_REGIONE ?? '',
        REGIONE: source?.REGIONE ?? municipality?.REGIONE ?? 'Emilia-Romagna',
        LISTA: row.LISTA,
        DATA: row.DATA,
        DELTA: row.DELTA,
        ELETTORI: source?.ELETTORI ?? municipality?.ELETTORI ?? 0,
        SIGMA_DELTA: row.SIGMA_DELTA
      }
    ];
  });
}

function projectedMunicipalRows(
  rows: readonly RegionalErMunicipalListParameterRow[],
  projectedListRows: readonly RegionalErListParameterRow[],
  municipalities: readonly RegionalErMunicipalityRow[]
): RegionalErMunicipalListParameterRow[] {
  const projectedListByKey = new Map(projectedListRows.map((row) => [listKey(row.LISTA), row]));
  const filteredRows = rows.filter((row) => projectedListByKey.has(listKey(row.LISTA))).map((row) => ({ ...row }));
  const rowsByKey = new Map(filteredRows.map((row) => [sourceMunicipalRowKey(row), row]));
  const fallbackSigmaDelta = meanFinite(filteredRows.map((row) => row.SIGMA_DELTA));

  for (const municipality of municipalities) {
    for (const listRow of projectedListRows) {
      const rowKey = `${String(municipality.CODICE_COMUNE)}\u001f${listKey(listRow.LISTA)}`;
      if (rowsByKey.has(rowKey)) continue;

      rowsByKey.set(rowKey, {
        CODICE_COMUNE: municipality.CODICE_COMUNE,
        COMUNE: municipality.COMUNE,
        CODICE_PROVINCIA: municipality.CODICE_PROVINCIA,
        PROVINCIA: municipality.PROVINCIA,
        CODICE_REGIONE: municipality.CODICE_REGIONE,
        REGIONE: municipality.REGIONE,
        LISTA: listRow.LISTA,
        DATA: listRow.DATA,
        DELTA: 0,
        ELETTORI: municipality.ELETTORI,
        SIGMA_DELTA: fallbackSigmaDelta
      });
    }
  }

  return [...rowsByKey.values()].sort((left, right) =>
    [String(left.CODICE_COMUNE), left.LISTA].join('\u001f').localeCompare([String(right.CODICE_COMUNE), right.LISTA].join('\u001f'), 'it')
  );
}

function groupRowsByMunicipality(
  rows: readonly RegionalErMunicipalListParameterRow[]
): Map<string, RegionalErMunicipalListParameterRow[]> {
  const grouped = new Map<string, RegionalErMunicipalListParameterRow[]>();
  for (const row of rows) {
    const key = String(row.CODICE_COMUNE);
    const municipalityRows = grouped.get(key) ?? [];
    municipalityRows.push(row);
    grouped.set(key, municipalityRows);
  }
  return grouped;
}

function overrideMunicipalityCodes(
  scope: ScenarioLocalShareOverrideScope,
  locationCode: string,
  rowsByMunicipality: ReadonlyMap<string, RegionalErMunicipalListParameterRow[]>,
  municipalities: readonly RegionalErMunicipalityRow[]
): string[] {
  if (scope === 'municipality') return rowsByMunicipality.has(locationCode) ? [locationCode] : [];

  const municipalitiesByCode = new Map(municipalities.map((row) => [String(row.CODICE_COMUNE), row]));
  return [...rowsByMunicipality.keys()].filter((code) => {
    const row = municipalitiesByCode.get(code);
    if (!row) return false;
    return scope === 'province'
      ? String(row.CODICE_PROVINCIA) === locationCode
      : String(row.CODICE_REGIONE) === locationCode;
  });
}

function localShareOverrideKey(scope: ScenarioLocalShareOverride['scope'], locationCode: string | number): string {
  return `${scope}\u001f${String(locationCode).trim()}`;
}

function localScopeRank(scope: ScenarioLocalShareOverrideScope): number {
  if (scope === 'region') return 0;
  if (scope === 'province') return 1;
  return 2;
}

function normalizeValidShares(shares: Map<string, number>, activeLists: readonly string[]): Map<string, number> {
  const raw = new Map(activeLists.map((list) => [list, Math.max(shares.get(list) ?? 0, 1e-9)]));
  const total = [...raw.values()].reduce((sum, value) => sum + value, 0);
  return new Map([...raw.entries()].map(([list, value]) => [list, total > 0 ? value / total : 0]));
}

function targetValidShares(
  overrides: readonly ScenarioLocalShareOverride[],
  activeLists: readonly string[],
  currentShares: ReadonlyMap<string, number>,
  warnings: RegionalErScenarioProjectionWarning[]
): Map<string, number> | null {
  const overrideByList = new Map(overrides.map((override) => [listKey(override.list), override]));
  const overriddenLists = activeLists.filter((list) => overrideByList.has(listKey(list)));
  const nonOverriddenLists = activeLists.filter((list) => !overrideByList.has(listKey(list)));
  const overrideTotal = overriddenLists.reduce(
    (sum, list) => sum + Math.max(Number(overrideByList.get(listKey(list))?.startingShare) || 0, 0),
    0
  );
  const target = new Map<string, number>();

  if (overriddenLists.length === 0) return null;

  if (nonOverriddenLists.length === 0 && overrideTotal > 0 && Math.abs(overrideTotal - 100) > 1e-9) {
    warnings.push({
      code: 'REGIONAL_ER_SCENARIO_LOCAL_OVERRIDES_RENORMALIZED',
      message: `All active lists have explicit local valid-vote share overrides totaling ${Number(
        overrideTotal.toFixed(2)
      )}%; they were normalized to 100%.`,
      todoReference: 'MIGRATION_PLAN.md#next-work'
    });
    for (const list of overriddenLists) target.set(list, Math.max(Number(overrideByList.get(listKey(list))?.startingShare) || 0, 0) / overrideTotal);
    return target;
  }

  for (const list of overriddenLists) target.set(list, Math.max(Number(overrideByList.get(listKey(list))?.startingShare) || 0, 0) / 100);

  const remainingShare = Math.max(1 - overrideTotal / 100, 0);
  const currentRemainder = nonOverriddenLists.reduce((sum, list) => sum + (currentShares.get(list) ?? 0), 0);
  for (const list of nonOverriddenLists) {
    target.set(
      list,
      currentRemainder > 0 ? (remainingShare * (currentShares.get(list) ?? 0)) / currentRemainder : remainingShare / nonOverriddenLists.length
    );
  }
  return target;
}

function applyAreaOverride(
  rows: readonly RegionalErMunicipalListParameterRow[],
  municipalityCodes: readonly string[],
  overrides: readonly ScenarioLocalShareOverride[],
  projectedGlobalByList: ReadonlyMap<string, RegionalErListParameterRow>,
  overrideReferenceDate: string,
  warnings: RegionalErScenarioProjectionWarning[]
): RegionalErMunicipalListParameterRow[] {
  const rowsByMunicipality = groupRowsByMunicipality(rows);
  const updatedByKey = new Map<string, RegionalErMunicipalListParameterRow>();

  for (const municipalityCode of municipalityCodes) {
    const municipalityRows = rowsByMunicipality.get(municipalityCode) ?? [];
    const activeLists = municipalityRows
      .map((row) => row.LISTA)
      .filter((list) => listKey(list) !== 'astensione' && projectedGlobalByList.has(listKey(list)));
    const uniqueActiveLists = [...new Set(activeLists)].sort((left, right) => listKey(left).localeCompare(listKey(right), 'it'));
    const rawByList = new Map<string, number>();
    let rawTotal = 0;

    for (const row of municipalityRows) {
      const global = projectedGlobalByList.get(listKey(row.LISTA));
      if (!global) continue;
      const raw = logistic(global.LOGIT_P + row.DELTA);
      rawByList.set(row.LISTA, raw);
      rawTotal += raw;
    }

    const fractions = new Map([...rawByList.entries()].map(([list, raw]) => [list, rawTotal > 0 ? raw / rawTotal : 0]));
    const abstentionFraction = boundedFraction(fractions.get('astensione') ?? 0, 0);
    const politicalTotal = Math.max(1 - abstentionFraction, 0);
    const currentValidShares = normalizeValidShares(
      new Map(uniqueActiveLists.map((list) => [list, politicalTotal > 0 ? (fractions.get(list) ?? 0) / politicalTotal : 0])),
      uniqueActiveLists
    );
    const targetShares = targetValidShares(overrides, uniqueActiveLists, currentValidShares, warnings);
    if (!targetShares) continue;

    for (const row of municipalityRows) {
      const global = projectedGlobalByList.get(listKey(row.LISTA));
      if (!global) continue;

      const target =
        listKey(row.LISTA) === 'astensione'
          ? abstentionFraction
          : politicalTotal * (targetShares.get(row.LISTA) ?? currentValidShares.get(row.LISTA) ?? 0);

      updatedByKey.set(sourceMunicipalRowKey(row), {
        ...row,
        DATA: overrideReferenceDate,
        DELTA: logit(target) - global.LOGIT_P
      });
    }
  }

  if (updatedByKey.size === 0) return rows.map((row) => ({ ...row }));
  return rows.map((row) => updatedByKey.get(sourceMunicipalRowKey(row)) ?? { ...row });
}

function applyLocalShareOverrides(
  rows: readonly RegionalErMunicipalListParameterRow[],
  projectedListRows: readonly RegionalErListParameterRow[],
  municipalities: readonly RegionalErMunicipalityRow[],
  localOverrides: readonly ScenarioLocalShareOverride[],
  overrideReferenceDate: string,
  warnings: RegionalErScenarioProjectionWarning[]
): RegionalErMunicipalListParameterRow[] {
  if (localOverrides.length === 0) return rows.map((row) => ({ ...row }));

  const projectedGlobalByList = new Map(projectedListRows.map((row) => [listKey(row.LISTA), row]));
  const overridesByLocation = new Map<string, ScenarioLocalShareOverride[]>();
  let currentRows = rows.map((row) => ({ ...row }));
  const appliedAreas: Array<{ scope: ScenarioLocalShareOverrideScope; locationCode: string; municipalityCodes: Set<string> }> = [];

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
      const municipalityCodes = overrideMunicipalityCodes(scope, locationCode, rowsByMunicipality, municipalities);

      if (municipalityCodes.length === 0) {
        warnings.push({
          code: 'REGIONAL_ER_SCENARIO_LOCAL_OVERRIDES_UNUSED',
          message: `Local share overrides for ${scope} ${locationCode} did not match Emilia-Romagna geography and were ignored.`,
          todoReference: 'MIGRATION_PLAN.md#next-work'
        });
        continue;
      }

      const municipalityCodeSet = new Set(municipalityCodes);
      const broader = appliedAreas.find(
        (area) => localScopeRank(area.scope) < localScopeRank(scope) && [...municipalityCodeSet].some((code) => area.municipalityCodes.has(code))
      );
      if (broader) {
        warnings.push({
          code: 'REGIONAL_ER_SCENARIO_LOCAL_OVERRIDES_OVERLAP',
          message: `Local share overrides for ${scope} ${locationCode} overlap broader ${broader.scope} ${broader.locationCode}; the more specific override was applied later.`,
          todoReference: 'MIGRATION_PLAN.md#next-work'
        });
      }

      currentRows = applyAreaOverride(
        currentRows,
        municipalityCodes,
        overrides,
        projectedGlobalByList,
        overrideReferenceDate,
        warnings
      );
      appliedAreas.push({ scope, locationCode, municipalityCodes: municipalityCodeSet });
    }
  }

  return currentRows;
}

export function projectScenarioOntoRegionalErSource(
  source: RegionalErPipelineSource,
  scenario: Scenario,
  options: {
    simulations: number;
    electionDate?: string;
    currentDate?: string;
    historicalVotes?: readonly RegionalErHistoricalMunicipalListVoteRow[];
    municipalities: readonly RegionalErMunicipalityRow[];
  }
): RegionalErScenarioProjection {
  const warnings: RegionalErScenarioProjectionWarning[] = [];
  const electionDateIso = requestElectionDateIso(options.electionDate ?? scenario.electionDate, source.data_elezione);
  const overrideReferenceDate = overrideReferenceDateIso(options.currentDate, electionDateIso);
  const historicalParameterSource = buildHistoricalParameterSource(scenario, options.historicalVotes);
  const baseListRows = historicalParameterSource?.liste ?? source.liste;
  const baseMunicipalRows = historicalParameterSource
    ? enrichMunicipalRows(historicalParameterSource.comuni_liste, options.municipalities, source.comuni_liste)
    : source.comuni_liste;
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
  const basePoliticalTotal = baseListRows
    .filter((row) => row.LISTA !== 'astensione' && scenario.lists.some((list) => listKey(list.name) === listKey(row.LISTA)))
    .reduce((sum, row) => sum + row.PERCENTUALE, 0);
  const targetAbstention = scenario.abstentionOverride
    ? boundedFraction(scenario.abstentionShare / 100, baseAbstentionFraction(baseListRows, basePoliticalTotal))
    : baseAbstentionFraction(baseListRows, basePoliticalTotal);
  const projectionPoliticalTotal = Math.max(1 - targetAbstention, 0);
  const projectedPercentages = projectPercentages(activeLists, projectionPoliticalTotal, warnings);
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
  const projectedLists: RegionalErListParameterRow[] = [
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
  const projectedMunicipalParameterRows = applyLocalShareOverrides(
    projectedMunicipalRows(baseMunicipalRows, projectedLists, options.municipalities),
    projectedLists,
    options.municipalities,
    scenario.localShareOverrides,
    overrideReferenceDate,
    warnings
  );
  const rows: RegionalErScenarioProjectionRow[] = activeLists.map((active) => {
    const percentage = projectedPercentages.get(active.projectedListName) ?? active.source.PERCENTUALE;
    return {
      list: active.projectedListName,
      coalition: active.scenario.coalition,
      scenarioShare: active.scenario.startingShare,
      shareOverride: active.scenario.shareOverride,
      projectedShare: projectionPoliticalTotal > 0 ? (100 * percentage) / projectionPoliticalTotal : 0,
      parameterSource: active.parameterSource,
      status: 'active'
    };
  });

  return {
    source: {
      ...source,
      data_elezione: electionDateIso,
      simulazioni: options.simulations,
      liste: projectedLists,
      comuni_liste: projectedMunicipalParameterRows
    },
    rows,
    warnings
  };
}
