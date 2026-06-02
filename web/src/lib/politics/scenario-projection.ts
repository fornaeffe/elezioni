import type { Scenario, ScenarioList } from '$lib/core/types';
import type {
  PoliticsCandidateUniTemplateRow,
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
      projected.set(row.source.LISTA, activeSourceTotal > 0 ? (sourcePoliticalTotal * row.source.PERCENTUALE) / activeSourceTotal : 0);
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
      projected.set(row.source.LISTA, (sourcePoliticalTotal * Math.max(Number(row.scenario.startingShare) || 0, 0)) / overrideShareTotal);
    }
    return projected;
  }

  for (const row of overridden) {
    projected.set(row.source.LISTA, (sourcePoliticalTotal * Math.max(Number(row.scenario.startingShare) || 0, 0)) / 100);
  }

  const remainingShare = Math.max(100 - overrideShareTotal, 0);
  const remainingPoliticalTotal = (sourcePoliticalTotal * remainingShare) / 100;

  for (const row of nonOverridden) {
    projected.set(
      row.source.LISTA,
      nonOverrideSourceTotal > 0 ? (remainingPoliticalTotal * row.source.PERCENTUALE) / nonOverrideSourceTotal : 0
    );
  }

  return projected;
}

function projectRamoSource(
  source: PoliticsPipelineRamoSource,
  activeListKeys: ReadonlySet<string>,
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
      candidati_pluri: source.candidati_pluri.filter((row) => activeListKeys.has(listKey(row.LISTA)))
    },
    syntheticCoalitions
  };
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

  for (const sourceRow of sourcePoliticalRows) {
    const scenarioRow = scenarioByKey.get(listKey(sourceRow.LISTA));
    if (!scenarioRow) {
      projectionRows.push({
        list: sourceRow.LISTA,
        coalition: sourceRow.COALIZIONE,
        scenarioShare: null,
        shareOverride: false,
        projectedShare: 0,
        status: 'removed'
      });
      continue;
    }

    activeLists.push({ source: sourceRow, scenario: scenarioRow });
  }

  const unmatchedScenarioLists = scenario.lists.filter((row) => !sourceByKey.has(listKey(row.name)));
  if (unmatchedScenarioLists.length > 0) {
    warnings.push({
      code: 'POLITICS_SCENARIO_LISTS_IGNORED',
      message: `Some scenario lists do not exist in the current static snapshot and were ignored: ${unmatchedScenarioLists
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
          coalition: row.COALIZIONE,
          scenarioShare: null,
          shareOverride: false,
          projectedShare: sourcePoliticalTotal > 0 ? (100 * row.PERCENTUALE) / sourcePoliticalTotal : 0,
          status: 'matched' as const
        })),
        ...unmatchedScenarioLists.map((row) => ({
          list: row.name,
          coalition: row.coalition,
          scenarioShare: row.startingShare,
          shareOverride: row.shareOverride,
          projectedShare: null,
          status: 'unmatched' as const
        }))
      ],
      warnings
    };
  }

  const projectedPercentages = projectPercentages(activeLists, sourcePoliticalTotal, warnings);
  const activeListKeys = new Set(activeLists.map((row) => listKey(row.source.LISTA)));
  const activeCoalitions = new Set(
    activeLists
      .map((row) => row.scenario.coalition)
      .filter((coalition): coalition is string => coalition !== null && coalition !== '')
  );
  const camera = projectRamoSource(source.camera, activeListKeys, activeCoalitions);
  const senato = projectRamoSource(source.senato, activeListKeys, activeCoalitions);
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
    .filter((row) => row.LISTA === 'astensione' || activeListKeys.has(listKey(row.LISTA)))
    .map((row) => {
      if (row.LISTA === 'astensione') return { ...row };

      const scenarioRow = activeScenarioBySourceName.get(row.LISTA);
      const percentage = projectedPercentages.get(row.LISTA) ?? row.PERCENTUALE;

      return {
        ...row,
        COALIZIONE: scenarioRow?.coalition ?? row.COALIZIONE,
        PERCENTUALE: percentage,
        LOGIT_P: logit(percentage)
      };
    });

  for (const { source: sourceRow, scenario: scenarioRow } of activeLists) {
    const percentage = projectedPercentages.get(sourceRow.LISTA) ?? sourceRow.PERCENTUALE;
    projectionRows.push({
      list: sourceRow.LISTA,
      coalition: scenarioRow.coalition,
      scenarioShare: scenarioRow.startingShare,
      shareOverride: scenarioRow.shareOverride,
      projectedShare: sourcePoliticalTotal > 0 ? (100 * percentage) / sourcePoliticalTotal : 0,
      status: 'matched'
    });
  }

  for (const row of unmatchedScenarioLists) {
    projectionRows.push({
      list: row.name,
      coalition: row.coalition,
      scenarioShare: row.startingShare,
      shareOverride: row.shareOverride,
      projectedShare: null,
      status: 'unmatched'
    });
  }

  return {
    source: {
      ...source,
      data_elezione: requestElectionDateIso(options.electionDate ?? scenario.electionDate, source.data_elezione),
      simulazioni: options.simulations,
      liste: projectedLists,
      comuni_liste: source.comuni_liste.filter((row) => row.LISTA === 'astensione' || activeListKeys.has(listKey(row.LISTA))),
      camera: camera.source,
      senato: senato.source
    },
    rows: projectionRows,
    warnings
  };
}
