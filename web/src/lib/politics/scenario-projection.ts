import type {
  Scenario,
  ScenarioCandidateTemplate,
  ScenarioList,
  ScenarioListCorrespondence,
  ScenarioLocalShareOverride
} from '$lib/core/types';
import { buildPoliticsParametersFromHistoricalVotes } from './parameter-preparation';
import type {
  PoliticsCandidatePluriGenerationTemplateRow,
  PoliticsCandidateUniTemplateRow,
  PoliticsHistoricalMunicipalListVoteRow,
  PoliticsMunicipalListParameterRow,
  PoliticsPipelineListRow,
  PoliticsPipelineRamoSource,
  PoliticsPipelineSource
} from './types';

const sourceModelCorrespondenceElection = 'politics-static source model';

export interface PoliticsScenarioProjectionRow {
  list: string;
  sourceList: string | null;
  coalition: string | null;
  scenarioShare: number | null;
  shareOverride: boolean;
  projectedShare: number | null;
  matchMode: 'homonymous' | 'declared-correspondence' | 'none';
  status: 'matched' | 'removed' | 'unmatched';
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

interface ActiveSourceList {
  source: PoliticsPipelineListRow;
  scenario: ScenarioList;
  projectedListName: string;
  matchMode: 'homonymous' | 'declared-correspondence';
  correspondence?: ScenarioListCorrespondence;
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

function baseAbstentionFraction(rows: readonly PoliticsPipelineListRow[], politicalTotal: number): number {
  const abstentionRow = rows.find((row) => listKey(row.LISTA) === 'astensione');
  return boundedFraction(abstentionRow?.PERCENTUALE ?? 1 - politicalTotal, Math.max(1 - politicalTotal, 0));
}

function isSourceModelCorrespondence(correspondence: ScenarioListCorrespondence): boolean {
  return correspondence.pastElection === sourceModelCorrespondenceElection;
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
  activeLists: readonly ActiveSourceList[],
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
      message: `All matched lists have explicit valid-vote share overrides totaling ${shareTotal}%; they were normalized to 100% across matched lists before conversion to elector fractions.`,
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

function projectRamoSource(
  source: PoliticsPipelineRamoSource,
  activeBySourceKey: ReadonlyMap<string, ActiveSourceList>,
  activeCoalitions: ReadonlySet<string>
): { source: PoliticsPipelineRamoSource; syntheticCoalitions: string[] } {
  const existingCoalitions = new Set(
    source.candidati_uni
      .map((row) => row.COALIZIONE)
      .filter((coalition): coalition is string => coalition !== null && coalition !== '')
  );
  const syntheticCoalitions = [...activeCoalitions].filter((coalition) => !existingCoalitions.has(coalition));
  const baseUninominalRows = uniqueBy(source.candidati_uni, (row) => String(row.UNI_COD));
  const syntheticRows: PoliticsCandidateUniTemplateRow[] = syntheticCoalitions.flatMap((coalition) =>
    baseUninominalRows.map((row) => ({
      COALIZIONE: coalition,
      UNI_COD: row.UNI_COD,
      LISTA_MINORANZA: null,
      CANDIDATO_ID: null,
      DATA_NASCITA: null
    }))
  );

  return {
    source: {
      ...source,
      candidati_uni: [
        ...source.candidati_uni.filter((row) => row.COALIZIONE !== null && activeCoalitions.has(row.COALIZIONE)),
        ...syntheticRows
      ],
      candidati_pluri: source.candidati_pluri.flatMap((row) => {
        const active = activeBySourceKey.get(listKey(row.LISTA));
        return active ? [{ ...row, LISTA: active.projectedListName }] : [];
      })
    },
    syntheticCoalitions
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
  return scope === 'municipality' ? `municipality ${locationCode}` : `${scope} ${locationCode}`;
}

function projectedMunicipalRows(
  rows: readonly PoliticsMunicipalListParameterRow[],
  activeBySourceKey: ReadonlyMap<string, ActiveSourceList>,
  activeByProjectedKey: ReadonlyMap<string, ActiveSourceList>
): PoliticsMunicipalListParameterRow[] {
  return rows.flatMap((row) => {
    if (row.LISTA === 'astensione') return { ...row };
    const active = activeBySourceKey.get(listKey(row.LISTA)) ?? activeByProjectedKey.get(listKey(row.LISTA));
    return active ? [{ ...row, LISTA: active.projectedListName }] : [];
  });
}

function localBaseFractions(
  rows: readonly PoliticsMunicipalListParameterRow[],
  baseGlobalByList: ReadonlyMap<string, PoliticsPipelineListRow>,
  projectedGlobalByList: ReadonlyMap<string, PoliticsPipelineListRow>
): Map<string, number> {
  const rawByList = new Map<string, number>();
  let total = 0;

  for (const row of rows) {
    const baseGlobal = baseGlobalByList.get(listKey(row.LISTA)) ?? projectedGlobalByList.get(listKey(row.LISTA));
    if (!baseGlobal) continue;

    const raw = logistic(baseGlobal.LOGIT_P + row.DELTA);
    rawByList.set(row.LISTA, raw);
    total += raw;
  }

  return new Map([...rawByList.entries()].map(([list, raw]) => [list, total > 0 ? raw / total : 0]));
}

function applyLocalShareOverrides(
  rows: readonly PoliticsMunicipalListParameterRow[],
  baseListRows: readonly PoliticsPipelineListRow[],
  projectedListRows: readonly PoliticsPipelineListRow[],
  localOverrides: readonly ScenarioLocalShareOverride[],
  overrideReferenceDate: string,
  warnings: PoliticsScenarioProjectionWarning[]
): PoliticsMunicipalListParameterRow[] {
  if (localOverrides.length === 0) return rows.map((row) => ({ ...row }));

  const baseGlobalByList = new Map(baseListRows.map((row) => [listKey(row.LISTA), row]));
  const projectedGlobalByList = new Map(projectedListRows.map((row) => [listKey(row.LISTA), row]));
  const rowsByMunicipality = new Map<string, PoliticsMunicipalListParameterRow[]>();
  const overridesByLocation = new Map<string, ScenarioLocalShareOverride[]>();

  for (const row of rows) {
    const key = localShareOverrideKey('municipality', row.CODICE_COMUNE);
    const grouped = rowsByMunicipality.get(key) ?? [];
    grouped.push(row);
    rowsByMunicipality.set(key, grouped);
  }

  for (const override of localOverrides) {
    const key = localShareOverrideKey(override.scope, override.locationCode);
    const grouped = overridesByLocation.get(key) ?? [];
    grouped.push(override);
    overridesByLocation.set(key, grouped);
  }

  const adjustedByRowKey = new Map<string, PoliticsMunicipalListParameterRow>();

  for (const [locationKey, overrides] of overridesByLocation) {
    const municipalityRows = rowsByMunicipality.get(locationKey);
    const [scope, locationCode] = locationKey.split('\u001f');

    if (!municipalityRows || scope !== 'municipality') {
      warnings.push({
        code: 'POLITICS_SCENARIO_LOCAL_OVERRIDES_UNUSED',
        message: `Local share overrides for ${localOverrideLabel('municipality', locationCode)} did not match the current source model and were ignored.`,
        todoReference: 'MIGRATION_PLAN.md#current-caveats'
      });
      continue;
    }

    const baseFractions = localBaseFractions(municipalityRows, baseGlobalByList, projectedGlobalByList);
    const abstentionFraction = boundedFraction(baseFractions.get('astensione') ?? 0, 0);
    const politicalTotal = Math.max(1 - abstentionFraction, 0);
    const politicalRows = municipalityRows.filter((row) => listKey(row.LISTA) !== 'astensione' && projectedGlobalByList.has(listKey(row.LISTA)));
    const overrideByList = new Map(overrides.map((override) => [listKey(override.list), override]));
    const overriddenRows = politicalRows.filter((row) => overrideByList.has(listKey(row.LISTA)));
    const nonOverriddenRows = politicalRows.filter((row) => !overrideByList.has(listKey(row.LISTA)));
    const overrideShareTotal = overriddenRows.reduce(
      (sum, row) => sum + Math.max(Number(overrideByList.get(listKey(row.LISTA))?.startingShare) || 0, 0),
      0
    );
    const targetByList = new Map<string, number>();

    if (overriddenRows.length === 0) {
      warnings.push({
        code: 'POLITICS_SCENARIO_LOCAL_OVERRIDES_UNUSED',
        message: `Local share overrides for ${localOverrideLabel('municipality', locationCode)} did not match active scenario lists and were ignored.`,
        todoReference: 'MIGRATION_PLAN.md#current-caveats'
      });
      continue;
    }

    if (nonOverriddenRows.length === 0 && overrideShareTotal > 0 && Math.abs(overrideShareTotal - 100) > 1e-9) {
      warnings.push({
        code: 'POLITICS_SCENARIO_LOCAL_OVERRIDES_RENORMALIZED',
        message: `All active lists have explicit local valid-vote share overrides totaling ${Number(
          overrideShareTotal.toFixed(2)
        )}% in ${localOverrideLabel('municipality', locationCode)}; they were normalized to 100% before conversion to elector fractions.`,
        todoReference: 'MIGRATION_PLAN.md#current-caveats'
      });

      for (const row of overriddenRows) {
        targetByList.set(
          row.LISTA,
          (politicalTotal * Math.max(Number(overrideByList.get(listKey(row.LISTA))?.startingShare) || 0, 0)) / overrideShareTotal
        );
      }
    } else {
      for (const row of overriddenRows) {
        targetByList.set(row.LISTA, (politicalTotal * Math.max(Number(overrideByList.get(listKey(row.LISTA))?.startingShare) || 0, 0)) / 100);
      }

      const remainingShare = Math.max(100 - overrideShareTotal, 0);
      const remainingPoliticalTotal = (politicalTotal * remainingShare) / 100;
      const nonOverrideBaseTotal = nonOverriddenRows.reduce((sum, row) => sum + (baseFractions.get(row.LISTA) ?? 0), 0);

      for (const row of nonOverriddenRows) {
        targetByList.set(row.LISTA, nonOverrideBaseTotal > 0 ? (remainingPoliticalTotal * (baseFractions.get(row.LISTA) ?? 0)) / nonOverrideBaseTotal : 0);
      }
    }

    targetByList.set('astensione', abstentionFraction);

    for (const row of municipalityRows) {
      const projectedGlobal = projectedGlobalByList.get(listKey(row.LISTA));
      const target = targetByList.get(row.LISTA);
      if (!projectedGlobal || target === undefined) continue;

      adjustedByRowKey.set(`${String(row.CODICE_COMUNE)}\u001f${row.LISTA}`, {
        ...row,
        DATA: overrideReferenceDate,
        DELTA: logit(target) - projectedGlobal.LOGIT_P
      });
    }
  }

  return rows.map((row) => adjustedByRowKey.get(`${String(row.CODICE_COMUNE)}\u001f${row.LISTA}`) ?? { ...row });
}

function correspondenceLabel(correspondence: ScenarioListCorrespondence): string {
  return `${correspondence.pastElection || 'elezione sconosciuta'}:${correspondence.pastList || 'lista sconosciuta'} -> ${
    correspondence.futureList || 'lista sconosciuta'
  }`;
}

function retargetHistoricalCorrespondences(
  correspondences: readonly ScenarioListCorrespondence[],
  activeBySourceKey: ReadonlyMap<string, ActiveSourceList>
): ScenarioListCorrespondence[] {
  return correspondences
    .filter((correspondence) => !isSourceModelCorrespondence(correspondence))
    .map((correspondence) => {
      const active = activeBySourceKey.get(listKey(correspondence.futureList));
      return active ? { ...correspondence, futureList: active.projectedListName } : correspondence;
    });
}

function buildHistoricalParameterSource(
  scenario: Scenario,
  activeLists: readonly ActiveSourceList[],
  activeBySourceKey: ReadonlyMap<string, ActiveSourceList>,
  options: {
    historicalVotes?: readonly PoliticsHistoricalMunicipalListVoteRow[];
    parameterPercentualiPartenza?: string | null;
  }
): Pick<PoliticsPipelineSource, 'liste' | 'comuni_liste'> | null {
  if (!options.historicalVotes || options.historicalVotes.length === 0 || activeLists.length === 0) return null;

  const parameterScenario: Scenario = {
    ...scenario,
    lists: activeLists.map((row) => row.scenario),
    listCorrespondences: retargetHistoricalCorrespondences(scenario.listCorrespondences, activeBySourceKey)
  };
  const parameters = buildPoliticsParametersFromHistoricalVotes(options.historicalVotes, parameterScenario, {
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
    parameterPercentualiPartenza?: string | null;
  }
): PoliticsScenarioProjection {
  const warnings: PoliticsScenarioProjectionWarning[] = [];
  const electionDateIso = requestElectionDateIso(options.electionDate ?? scenario.electionDate, source.data_elezione);
  const overrideReferenceDate = overrideReferenceDateIso(options.currentDate, electionDateIso);
  const sourcePoliticalRows = source.liste.filter((row) => row.LISTA !== 'astensione');
  const sourcePoliticalTotal = sourcePoliticalRows.reduce((sum, row) => sum + row.PERCENTUALE, 0);
  const scenarioByKey = new Map(scenario.lists.map((row) => [listKey(row.name), row]));
  const activeLists: ActiveSourceList[] = [];
  const projectionRows: PoliticsScenarioProjectionRow[] = [];
  const activeBySourceKey = new Map<string, ActiveSourceList>();
  const activeScenarioKeys = new Set<string>();
  const usedCorrespondenceIds = new Set<string>();

  for (const sourceRow of sourcePoliticalRows) {
    const sourceKey = listKey(sourceRow.LISTA);
    const scenarioRow = scenarioByKey.get(listKey(sourceRow.LISTA));
    if (!scenarioRow) continue;

    const active = {
      source: sourceRow,
      scenario: scenarioRow,
      projectedListName: scenarioRow.name,
      matchMode: 'homonymous' as const
    };
    activeLists.push(active);
    activeBySourceKey.set(sourceKey, active);
    activeScenarioKeys.add(listKey(scenarioRow.name));
  }

  for (const sourceRow of sourcePoliticalRows) {
    const sourceKey = listKey(sourceRow.LISTA);
    if (activeBySourceKey.has(sourceKey)) continue;

    const candidates = scenario.listCorrespondences
      .filter((correspondence) => isSourceModelCorrespondence(correspondence) && listKey(correspondence.pastList) === sourceKey)
      .map((correspondence) => ({
        correspondence,
        scenario: scenarioByKey.get(listKey(correspondence.futureList))
      }))
      .filter(
        (candidate): candidate is { correspondence: ScenarioListCorrespondence; scenario: ScenarioList } =>
          candidate.scenario !== undefined && !activeScenarioKeys.has(listKey(candidate.scenario.name))
      );

    if (candidates.length === 0) continue;

    if (candidates.length > 1) {
      warnings.push({
        code: 'POLITICS_SCENARIO_CORRESPONDENCE_AMBIGUOUS',
        message: `More than one declared correspondence can use ${sourceRow.LISTA} as a source model list; no correspondence was applied for that source: ${candidates
          .map((candidate) => correspondenceLabel(candidate.correspondence))
          .join(', ')}.`,
        todoReference: 'MIGRATION_PLAN.md#current-caveats'
      });
      continue;
    }

    const [{ correspondence, scenario: scenarioRow }] = candidates;
    const active = {
      source: sourceRow,
      scenario: scenarioRow,
      projectedListName: scenarioRow.name,
      matchMode: 'declared-correspondence' as const,
      correspondence
    };
    activeLists.push(active);
    activeBySourceKey.set(sourceKey, active);
    activeScenarioKeys.add(listKey(scenarioRow.name));
    usedCorrespondenceIds.add(correspondence.id);
  }

  const unusedCorrespondences = scenario.listCorrespondences.filter(
    (correspondence) =>
      correspondence.source === 'manual' && isSourceModelCorrespondence(correspondence) && !usedCorrespondenceIds.has(correspondence.id)
  );
  if (unusedCorrespondences.length > 0) {
    warnings.push({
      code: 'POLITICS_SCENARIO_CORRESPONDENCES_UNUSED',
      message: `Some declared list correspondences did not match the current static snapshot and were not used: ${unusedCorrespondences
        .map(correspondenceLabel)
        .join(', ')}.`,
      todoReference: 'MIGRATION_PLAN.md#current-caveats'
    });
  }

  for (const sourceRow of sourcePoliticalRows) {
    const active = activeBySourceKey.get(listKey(sourceRow.LISTA));
    if (active) {
      continue;
    }

    projectionRows.push({
      list: sourceRow.LISTA,
      sourceList: sourceRow.LISTA,
      coalition: sourceRow.COALIZIONE,
      scenarioShare: null,
      shareOverride: false,
      projectedShare: 0,
      matchMode: 'none',
      status: 'removed'
    });
  }

  const unmatchedScenarioLists = scenario.lists.filter((row) => !activeScenarioKeys.has(listKey(row.name)));
  if (unmatchedScenarioLists.length > 0) {
    warnings.push({
      code: 'POLITICS_SCENARIO_LISTS_IGNORED',
      message: `Some scenario lists do not have a homonymous source list or usable declared correspondence in the current static snapshot and were ignored: ${unmatchedScenarioLists
        .map((row) => row.name)
        .join(', ')}.`,
      todoReference: 'MIGRATION_PLAN.md#current-caveats'
    });
  }

  if (activeLists.length === 0) {
    warnings.push({
      code: 'POLITICS_SCENARIO_NO_MATCHING_LISTS',
      message: 'No scenario list matched the current static snapshot, so the bundled default source was used unchanged.',
      todoReference: 'MIGRATION_PLAN.md#current-caveats'
    });

    return {
      source: {
        ...source,
        data_elezione: electionDateIso,
        simulazioni: options.simulations
      },
      rows: [
        ...sourcePoliticalRows.map((row) => ({
          list: row.LISTA,
          sourceList: row.LISTA,
          coalition: row.COALIZIONE,
          scenarioShare: null,
          shareOverride: false,
          projectedShare: sourcePoliticalTotal > 0 ? (100 * row.PERCENTUALE) / sourcePoliticalTotal : 0,
          matchMode: 'none' as const,
          status: 'matched' as const
        })),
        ...unmatchedScenarioLists.map((row) => ({
          list: row.name,
          sourceList: null,
          coalition: row.coalition,
          scenarioShare: row.startingShare,
          shareOverride: row.shareOverride,
          projectedShare: null,
          matchMode: 'none' as const,
          status: 'unmatched' as const
        }))
      ],
      warnings
    };
  }

  const historicalParameterSource = buildHistoricalParameterSource(scenario, activeLists, activeBySourceKey, options);
  const baseListRows = historicalParameterSource?.liste ?? source.liste;
  const baseMunicipalRows = historicalParameterSource?.comuni_liste ?? source.comuni_liste;
  const parameterByListKey = new Map(baseListRows.map((row) => [listKey(row.LISTA), row]));
  const activeListsWithParameters = activeLists.map((active) => ({
    ...active,
    source: parameterByListKey.get(listKey(active.projectedListName)) ?? active.source
  }));
  const basePoliticalTotal = baseListRows
    .filter((row) => row.LISTA !== 'astensione' && activeScenarioKeys.has(listKey(row.LISTA)))
    .reduce((sum, row) => sum + row.PERCENTUALE, 0);
  const targetAbstention = scenario.abstentionOverride
    ? boundedFraction(scenario.abstentionShare / 100, baseAbstentionFraction(baseListRows, basePoliticalTotal))
    : baseAbstentionFraction(baseListRows, basePoliticalTotal);
  const projectionPoliticalTotal =
    scenario.abstentionOverride || historicalParameterSource ? Math.max(1 - targetAbstention, 0) : sourcePoliticalTotal;
  const projectedPercentages = projectPercentages(
    activeListsWithParameters,
    projectionPoliticalTotal,
    warnings
  );
  const activeCoalitions = new Set(
    activeLists
      .map((row) => row.scenario.coalition)
      .filter((coalition): coalition is string => coalition !== null && coalition !== '')
  );
  const cameraProjection = projectRamoSource(source.camera, activeBySourceKey, activeCoalitions);
  const senatoProjection = projectRamoSource(source.senato, activeBySourceKey, activeCoalitions);
  const camera = applyCandidateTemplatesToRamo('camera', cameraProjection.source, scenario.candidateTemplates, warnings);
  const senato = applyCandidateTemplatesToRamo('senato', senatoProjection.source, scenario.candidateTemplates, warnings);
  const syntheticCoalitions = [...new Set([...cameraProjection.syntheticCoalitions, ...senatoProjection.syntheticCoalitions])];

  if (syntheticCoalitions.length > 0) {
    warnings.push({
      code: 'POLITICS_SCENARIO_SYNTHETIC_COALITIONS',
      message: `Some scenario coalitions do not exist in the current candidate template; generated placeholder uninominal candidates were created for: ${syntheticCoalitions.join(
        ', '
      )}.`,
      todoReference: 'MIGRATION_PLAN.md#current-caveats'
    });
  }

  const activeScenarioBySourceName = new Map(activeLists.map((row) => [row.source.LISTA, row.scenario]));
  const activeByProjectedKey = new Map(activeLists.map((row) => [listKey(row.projectedListName), row]));
  const projectedLists = baseListRows
    .flatMap((row) => {
      if (listKey(row.LISTA) === 'astensione') {
        return {
          ...row,
          PERCENTUALE: targetAbstention,
          LOGIT_P: logit(targetAbstention),
          SIGMA_GLOBAL: scenario.abstentionOverride ? 0 : row.SIGMA_GLOBAL
        };
      }

      const active = activeBySourceKey.get(listKey(row.LISTA)) ?? activeByProjectedKey.get(listKey(row.LISTA));
      if (!active) return [];

      const scenarioRow = activeScenarioBySourceName.get(row.LISTA) ?? active.scenario;
      const percentage = projectedPercentages.get(active.projectedListName) ?? row.PERCENTUALE;

      return [{
        ...row,
        LISTA: active.projectedListName,
        COALIZIONE: scenarioRow?.coalition ?? row.COALIZIONE,
        DATA: active.scenario.shareOverride ? overrideReferenceDate : row.DATA,
        PERCENTUALE: percentage,
        SIGMA_GLOBAL: row.SIGMA_GLOBAL,
        LOGIT_P: logit(percentage)
      }];
    });
  const projectedMunicipalParameterRows = applyLocalShareOverrides(
    projectedMunicipalRows(baseMunicipalRows, activeBySourceKey, activeByProjectedKey),
    baseListRows,
    projectedLists,
    scenario.localShareOverrides,
    overrideReferenceDate,
    warnings
  );

  for (const { source: sourceRow, scenario: scenarioRow } of activeLists) {
    const active = activeBySourceKey.get(listKey(sourceRow.LISTA));
    const projectedListName = active?.projectedListName ?? sourceRow.LISTA;
    const baseParameterRow = parameterByListKey.get(listKey(projectedListName));
    const percentage = projectedPercentages.get(projectedListName) ?? baseParameterRow?.PERCENTUALE ?? sourceRow.PERCENTUALE;
    projectionRows.push({
      list: projectedListName,
      sourceList: sourceRow.LISTA,
      coalition: scenarioRow.coalition,
      scenarioShare: scenarioRow.startingShare,
      shareOverride: scenarioRow.shareOverride,
      projectedShare: projectionPoliticalTotal > 0 ? (100 * percentage) / projectionPoliticalTotal : 0,
      matchMode: active?.matchMode ?? 'none',
      status: 'matched'
    });
  }

  for (const row of unmatchedScenarioLists) {
    projectionRows.push({
      list: row.name,
      sourceList: null,
      coalition: row.coalition,
      scenarioShare: row.startingShare,
      shareOverride: row.shareOverride,
      projectedShare: null,
      matchMode: 'none',
      status: 'unmatched'
    });
  }

  return {
    source: {
      ...source,
      data_elezione: electionDateIso,
      simulazioni: options.simulations,
      liste: projectedLists,
      comuni_liste: projectedMunicipalParameterRows,
      camera,
      senato
    },
    rows: projectionRows,
    warnings
  };
}
