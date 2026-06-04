import type { Scenario, ScenarioList, ScenarioListCorrespondence } from '$lib/core/types';
import type {
  PoliticsCandidateUniTemplateRow,
  PoliticsPipelineListRow,
  PoliticsPipelineRamoSource,
  PoliticsPipelineSource
} from './types';

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

function requestElectionDateIso(rawDate: string | undefined, fallback: string): string {
  const candidate = rawDate || fallback;
  const isoCandidate = /^\d{4}-\d{2}-\d{2}$/.test(candidate) ? `${candidate}T00:00:00.000Z` : candidate;
  const parsed = new Date(isoCandidate);
  return Number.isFinite(parsed.getTime()) ? parsed.toISOString() : fallback;
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
    warnings.push({
      code: 'POLITICS_SCENARIO_OVERRIDES_RENORMALIZED',
      message:
        'All matched lists have an explicit share override, but the override total is not 100; overrides were renormalized across matched lists.',
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

function correspondenceLabel(correspondence: ScenarioListCorrespondence): string {
  return `${correspondence.pastElection || 'elezione sconosciuta'}:${correspondence.pastList || 'lista sconosciuta'} -> ${
    correspondence.futureList || 'lista sconosciuta'
  }`;
}

export function projectScenarioOntoPoliticsSource(
  source: PoliticsPipelineSource,
  scenario: Scenario,
  options: {
    simulations: number;
    electionDate?: string;
  }
): PoliticsScenarioProjection {
  const warnings: PoliticsScenarioProjectionWarning[] = [];
  const sourcePoliticalRows = source.liste.filter((row) => row.LISTA !== 'astensione');
  const sourcePoliticalTotal = sourcePoliticalRows.reduce((sum, row) => sum + row.PERCENTUALE, 0);
  const sourceByKey = new Map(sourcePoliticalRows.map((row) => [listKey(row.LISTA), row]));
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
      .filter((correspondence) => listKey(correspondence.pastList) === sourceKey)
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
    (correspondence) => correspondence.source === 'manual' && !usedCorrespondenceIds.has(correspondence.id)
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
        data_elezione: requestElectionDateIso(options.electionDate ?? scenario.electionDate, source.data_elezione),
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

  const projectedPercentages = projectPercentages(activeLists, sourcePoliticalTotal, warnings);
  const activeCoalitions = new Set(
    activeLists
      .map((row) => row.scenario.coalition)
      .filter((coalition): coalition is string => coalition !== null && coalition !== '')
  );
  const camera = projectRamoSource(source.camera, activeBySourceKey, activeCoalitions);
  const senato = projectRamoSource(source.senato, activeBySourceKey, activeCoalitions);
  const syntheticCoalitions = [...new Set([...camera.syntheticCoalitions, ...senato.syntheticCoalitions])];

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
  const projectedLists = source.liste
    .flatMap((row) => {
      if (row.LISTA === 'astensione') return { ...row };

      const active = activeBySourceKey.get(listKey(row.LISTA));
      if (!active) return [];

      const scenarioRow = activeScenarioBySourceName.get(row.LISTA);
      const percentage = projectedPercentages.get(active.projectedListName) ?? row.PERCENTUALE;

      return [{
        ...row,
        LISTA: active.projectedListName,
        COALIZIONE: scenarioRow?.coalition ?? row.COALIZIONE,
        PERCENTUALE: percentage,
        SIGMA_GLOBAL: scenario.globalShareMode === 'fixed' ? 0 : row.SIGMA_GLOBAL,
        LOGIT_P: logit(percentage)
      }];
    });

  for (const { source: sourceRow, scenario: scenarioRow } of activeLists) {
    const active = activeBySourceKey.get(listKey(sourceRow.LISTA));
    const projectedListName = active?.projectedListName ?? sourceRow.LISTA;
    const percentage = projectedPercentages.get(projectedListName) ?? sourceRow.PERCENTUALE;
    projectionRows.push({
      list: projectedListName,
      sourceList: sourceRow.LISTA,
      coalition: scenarioRow.coalition,
      scenarioShare: scenarioRow.startingShare,
      shareOverride: scenarioRow.shareOverride,
      projectedShare: sourcePoliticalTotal > 0 ? (100 * percentage) / sourcePoliticalTotal : 0,
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
      data_elezione: requestElectionDateIso(options.electionDate ?? scenario.electionDate, source.data_elezione),
      simulazioni: options.simulations,
      liste: projectedLists,
      comuni_liste: source.comuni_liste.flatMap((row) => {
        if (row.LISTA === 'astensione') return { ...row };
        const active = activeBySourceKey.get(listKey(row.LISTA));
        return active ? [{ ...row, LISTA: active.projectedListName }] : [];
      }),
      camera: camera.source,
      senato: senato.source
    },
    rows: projectionRows,
    warnings
  };
}
