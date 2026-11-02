import { runtimeDataUrl, requireSimulationCount } from '$lib/core/runtime-data';
import { loadWorkspaceSnapshot } from './data';
import { snapshotDescriptors } from './snapshots.generated';
import { validateScenario } from '../scenario/politics';
import { validateRegionalErScenario } from '../scenario/regional-er';
import { WorkspaceExecutionError } from './execution-error';
import { runPoliticsSimulation } from '$lib/politics/simulation-scrutiny';
import type {
  ResultTable,
  ScrutinyWarning,
  SimulationProgress,
  SimulationRequest,
  SimulationResult,
  SimulationWorkerMessage
} from '$lib/core/types';
import { buildPoliticsDirectScrutinySnapshot } from '$lib/politics/pipeline';
import {
  projectScenarioOntoPoliticsSource,
  type PoliticsScenarioProjectionRow
} from '$lib/politics/scenario-projection';
import {
  buildPoliticsResultPlotTables,
  buildPoliticsResultTables,
  summarizePoliticsGeneratedRuns,
  summarizePoliticsScrutinyRun,
  type PoliticsRunPresentationSummary
} from '$lib/politics/result-presentation';
import { resolvePoliticsScrutinyAlgorithm } from '$lib/politics/scrutiny-algorithms';
import { buildPoliticsPipelineSourceFromSnapshot } from '$lib/politics/static-snapshot';
import type { PoliticsStaticSnapshot, Ramo } from '$lib/politics/types';
import { buildRegionalErDirectScrutinySnapshot } from '$lib/regional-er/pipeline';
import {
  buildRegionalErResultPlotTables,
  buildRegionalErResultTables,
  summarizeRegionalErGeneratedRuns,
  summarizeRegionalErScrutinyRun,
  type RegionalErRunPresentationSummary
} from '$lib/regional-er/result-presentation';
import { projectScenarioOntoRegionalErSource } from '$lib/regional-er/scenario-projection';
import { runRegionalErScrutiny } from '$lib/regional-er/scrutiny';
import { buildRegionalErPipelineSourceFromSnapshot } from '$lib/regional-er/static-snapshot';
import type { RegionalErScenarioProjectionRow, RegionalErStaticSnapshot } from '$lib/regional-er/types';

const politicsGeneratedChunkSize = 50;
const politicsStaticSnapshotPath = snapshotDescriptors.politics!.path;
const regionalErGeneratedChunkSize = 50;
const regionalErStaticSnapshotPath = snapshotDescriptors['regional-er']!.path;

interface LoadedPoliticsStaticSnapshot {
  snapshot: PoliticsStaticSnapshot;
  path: string;
}

interface LoadedRegionalErStaticSnapshot {
  snapshot: RegionalErStaticSnapshot;
  path: string;
}

function progress(
  post: (message: SimulationWorkerMessage) => void,
  startedAt: number,
  phase: SimulationProgress['phase'],
  completed: number,
  total: number
): void {
  post({
    type: 'progress',
    phase,
    completed,
    total,
    elapsedMs: performance.now() - startedAt
  });
}

function scenarioProjectionTable(rows: Array<PoliticsScenarioProjectionRow | RegionalErScenarioProjectionRow>): ResultTable {
  return {
    name: 'Scenario projection',
    columns: ['Lista', 'Fonte parametri', 'Coalizione', 'Quota scenario', 'Usata', 'Quota proiettata', 'Stato'],
    rows: rows.map((row) => ({
      Lista: row.list,
      'Fonte parametri': row.parameterSource,
      Coalizione: row.coalition,
      'Quota scenario': row.scenarioShare === null ? null : Number(row.scenarioShare.toFixed(2)),
      Usata: row.shareOverride,
      'Quota proiettata': row.projectedShare === null ? null : Number(row.projectedShare.toFixed(2)),
      Stato: row.status
    }))
  };
}

async function loadPoliticsStaticSnapshot(base: string): Promise<LoadedPoliticsStaticSnapshot> {
  return { snapshot: await loadWorkspaceSnapshot('politics', base), path: politicsStaticSnapshotPath };
}
async function loadRegionalErStaticSnapshot(base: string): Promise<LoadedRegionalErStaticSnapshot> {
  return { snapshot: await loadWorkspaceSnapshot('regional-er', base), path: regionalErStaticSnapshotPath };
}

export async function runLegacyWorkspace(request: SimulationRequest, post: (message: SimulationWorkerMessage) => void): Promise<void> {
  const startedAt = performance.now();
  const startedIso = new Date().toISOString();
  const base = request.basePath ?? '';

  progress(post, startedAt, 'validate', 0, request.simulations);
  requireSimulationCount(request.simulations);
  if ('kind' in request.scenario) throw new Error('Reform scenario requires the reform algorithm');
  const scenario = request.scenario;
  const errors = request.kind === 'politiche' ? validateScenario(scenario) : validateRegionalErScenario(scenario);
  if (errors.length) throw new Error(errors.join('\n'));
  const descriptor = snapshotDescriptors[request.kind === 'politiche' ? 'politics' : 'regional-er'];
  if (!descriptor || request.dataVersion !== 'v1' && request.dataVersion !== descriptor.version)
    throw new Error('Scenario/workspace data version mismatch');
  if (scenario.defaultSource.kind === 'bundled' && scenario.defaultSource.dataVersion !== 'v1' && scenario.defaultSource.dataVersion !== descriptor.version)
    throw new Error('Bundled scenario snapshot unavailable. Reset the scenario or restore its matching data explicitly.');
  // The unchanged legacy scatter plots mount every subject/run SVG point.
  // Keep their existing DOM bound until that view has a separate rendering
  // redesign. Reform details are paginated and have no arbitrary run cap.
  if (request.kind === 'politiche' && request.simulations > 1000) {
    throw new RangeError('La legge precedente ammette al massimo 1000 simulazioni: i grafici esistenti visualizzano un elemento SVG per ogni punto. La riforma usa dettagli paginati senza questo limite.');
  }
  progress(post, startedAt, 'validate', 0, 5);

  if (request.kind !== 'politiche' && request.kind !== 'regionali-er') throw new Error('Workspace non disponibile');

  if (request.kind === 'regionali-er') {
    progress(post, startedAt, 'prepare', 1, 5);
    const loadedSnapshot = await loadRegionalErStaticSnapshot(base);
    const staticSnapshot = loadedSnapshot.snapshot;
    const simulationCount = request.simulations;
    const source = buildRegionalErPipelineSourceFromSnapshot(staticSnapshot, { simulations: simulationCount });
    const projection = projectScenarioOntoRegionalErSource(source, scenario, {
      currentDate: startedIso,
      electionDate: request.electionDate,
      historicalVotes: staticSnapshot.data.comuni_liste_elezioni,
      municipalities: staticSnapshot.data.municipalities,
      simulations: simulationCount
    });
    const scenarioSource = projection.source;
    const runs: RegionalErRunPresentationSummary[] = [];
    let generatedSimulations = 0;
    let scrutinizedRuns = 0;

    progress(post, startedAt, 'simulate', generatedSimulations, simulationCount);

    for (let chunkStart = 1; chunkStart <= simulationCount; chunkStart += regionalErGeneratedChunkSize) {
      const chunkSize = Math.min(regionalErGeneratedChunkSize, simulationCount - chunkStart + 1);
      const snapshot = buildRegionalErDirectScrutinySnapshot(
        {
          ...scenarioSource,
          simulazioni: chunkSize
        },
        { seed: `${request.seed}:regional-er:chunk:${chunkStart}` }
      );

      generatedSimulations += chunkSize;
      progress(post, startedAt, 'simulate', generatedSimulations, simulationCount);
      progress(post, startedAt, 'scrutinize', scrutinizedRuns, simulationCount);

      for (const simulation of snapshot.simulations) {
        const globalSimulation = chunkStart + simulation.sim - 1;
        const runStartedAt = performance.now();
        let output: ReturnType<typeof runRegionalErScrutiny>;
        try { output = runRegionalErScrutiny(
          simulation.input,
          {
            pop_legale: scenarioSource.pop_legale,
            liste: scenarioSource.liste
          },
          { seed: `${request.seed}:regional-er:scrutiny:${globalSimulation}` }
        ); } catch (error) { throw new WorkspaceExecutionError(error, globalSimulation); }

        runs.push(
          summarizeRegionalErScrutinyRun({
            sim: globalSimulation,
            elapsedMs: performance.now() - runStartedAt,
            input: simulation.input,
            output
          })
        );
        scrutinizedRuns += 1;
        if (scrutinizedRuns % 10 === 0 || scrutinizedRuns === simulationCount) {
          progress(post, startedAt, 'scrutinize', scrutinizedRuns, simulationCount);
        }
      }
    }

    const resultWarnings: ScrutinyWarning[] = [
      {
        code: 'REGIONAL_ER_STATIC_SNAPSHOT',
        electionKind: request.kind,
        severity: 'info',
        message: `Running the TypeScript regional pipeline on ${loadedSnapshot.path}: ${staticSnapshot.metadata.source}.`
      },
      {
        code: 'REGIONAL_ER_SCENARIO_PROJECTION',
        electionKind: request.kind,
        severity: 'info',
        message:
          'Scenario lists are projected from Emilia-Romagna historical correspondences when raw historical votes are available; regional workflow does not generate individual candidates.',
        todoReference: 'MIGRATION_PLAN.md#next-work'
      }
    ];

    for (const note of staticSnapshot.metadata.preparation?.notes ?? []) resultWarnings.push({ code: 'WORKSPACE_DATA_ASSUMPTION', electionKind: request.kind, severity: 'warning', message: note });
    for (const warning of projection.warnings) {
      resultWarnings.push({
        code: warning.code,
        electionKind: request.kind,
        severity: 'warning',
        message: warning.message,
        todoReference: warning.todoReference
      });
    }


    const result: SimulationResult = {
      type: 'result',
      status: 'completed',
      tables: [
        ...buildRegionalErResultTables(runs),
        ...buildRegionalErResultPlotTables(runs),
        scenarioProjectionTable(projection.rows),
        summarizeRegionalErGeneratedRuns(runs)
      ],
      warnings: resultWarnings,
      benchmark: {
        startedAt: startedIso,
        elapsedMs: performance.now() - startedAt,
        simulations: simulationCount,
        dataVersion: descriptor.version,
        scrutinyAlgorithmId: request.scrutinyAlgorithmId,
        seed: request.seed
      }
    };

    progress(post, startedAt, 'summarize', 5, 5);
    post(result);
    return;
  }

  progress(post, startedAt, 'prepare', 1, 5);
  const algorithmResolution = resolvePoliticsScrutinyAlgorithm(request.scrutinyAlgorithmId);
  const scrutinyAlgorithm = algorithmResolution.algorithm;
  const loadedSnapshot = await loadPoliticsStaticSnapshot(base);
  const staticSnapshot = loadedSnapshot.snapshot;
  const simulationCount = request.simulations;
  const source = buildPoliticsPipelineSourceFromSnapshot(staticSnapshot, { simulations: simulationCount });
  const projection = projectScenarioOntoPoliticsSource(source, scenario, {
    currentDate: startedIso,
    electionDate: request.electionDate,
    historicalVotes: staticSnapshot.data.comuni_liste_elezioni,
    municipalities: staticSnapshot.data.municipalities,
    parameterPercentualiPartenza: 'europee',
    simulations: simulationCount
  });
  const scenarioSource = projection.source;

  const runs: PoliticsRunPresentationSummary[] = [];
  let generatedSimulations = 0;
  let scrutinizedRuns = 0;
  const totalScrutinyRuns = simulationCount * 2;

  progress(post, startedAt, 'simulate', generatedSimulations, simulationCount);

  for (let chunkStart = 1; chunkStart <= simulationCount; chunkStart += politicsGeneratedChunkSize) {
    const chunkSize = Math.min(politicsGeneratedChunkSize, simulationCount - chunkStart + 1);
    const snapshot = buildPoliticsDirectScrutinySnapshot(
      {
        ...scenarioSource,
        simulazioni: chunkSize
      },
      { seed: `${request.seed}:chunk:${chunkStart}` }
    );

    generatedSimulations += chunkSize;
    progress(post, startedAt, 'simulate', generatedSimulations, simulationCount);
    progress(post, startedAt, 'scrutinize', scrutinizedRuns, totalScrutinyRuns);

    for (let localSim = 1; localSim <= chunkSize; localSim++) {
      const globalSimulation = chunkStart + localSim - 1;
      const pairs = (['camera', 'senato'] as Ramo[]).map(ramo => {
        const data = snapshot.rami[ramo];
        const simulation = data.simulations.find(s => s.sim === localSim);
        if (!simulation) throw new Error(`Missing paired legacy chamber ${ramo}/${globalSimulation}`);
        const withGlobalId = <T extends { SIM?: number }>(rows: T[]): T[] => rows.map(r => ({ ...r, SIM: globalSimulation }));
        return { ramo, data, input: { ...simulation.input, liste_uni: withGlobalId(simulation.input.liste_uni),
          candidati_uni: withGlobalId(simulation.input.candidati_uni), candidati_pluri: withGlobalId(simulation.input.candidati_pluri) } };
      });
      const chamberTimes = new Map<Ramo, number>();
      let output: ReturnType<typeof runPoliticsSimulation>;
      try { output = runPoliticsSimulation({ algorithmId: 'politiche-r-parity-v1', simulationId: globalSimulation,
        chambers: pairs.map(p => ({ input: p.input, context: { ramo: p.ramo, liste_naz: p.data.liste_naz,
          totali_pluri: p.data.totali_pluri, totale_seggi: p.data.totale_seggi } })) }, undefined,
        (chamber, elapsedMs) => chamberTimes.set(chamber, elapsedMs)); }
      catch (error) { throw new WorkspaceExecutionError(error, globalSimulation, pairs.find(p => !chamberTimes.has(p.ramo))?.ramo); }
      if (output.algorithmId !== 'politiche-r-parity-v1') throw new Error('Unexpected legacy result');
      for (const chamber of output.chambers) {
        const pair = pairs.find(p => p.ramo === chamber.chamber)!;
        runs.push(summarizePoliticsScrutinyRun({ ramo: pair.ramo, sim: globalSimulation,
          elapsedMs: chamberTimes.get(pair.ramo)!, input: pair.input,
          output: chamber.output, listeNaz: pair.data.liste_naz }));
        scrutinizedRuns++;
      }
      if (scrutinizedRuns % 10 === 0 || scrutinizedRuns === totalScrutinyRuns) progress(post, startedAt, 'scrutinize', scrutinizedRuns, totalScrutinyRuns);
    }
  }

  const resultWarnings: ScrutinyWarning[] = [];

  if (algorithmResolution.fallback) {
    resultWarnings.push({
      code: 'POLITICS_SCRUTINY_ALGORITHM_FALLBACK',
      electionKind: request.kind,
      severity: 'warning',
      message: `Requested scrutiny algorithm ${algorithmResolution.requestedId}, but it is not registered; using ${scrutinyAlgorithm.id}.`,
      lawReference: scrutinyAlgorithm.lawReference,
      todoReference: 'MIGRATION_PLAN.md#implementation-checklist'
    });
  }

  resultWarnings.push(
    {
      code: 'POLITICS_STATIC_SNAPSHOT',
      electionKind: request.kind,
      severity: 'info',
      message: `Running the TypeScript generated pipeline on ${loadedSnapshot.path}: ${staticSnapshot.metadata.source}.`
    },
    {
      code: 'POLITICS_SCENARIO_PROJECTION',
      electionKind: request.kind,
      severity: 'info',
      message:
        'Scenario lists are projected from historical correspondences when raw historical votes are available; candidate slots are generated from the legal college grid for every active scenario list.',
      todoReference: 'MIGRATION_PLAN.md#current-caveats'
    }
  );

  for (const note of staticSnapshot.metadata.preparation?.notes ?? []) resultWarnings.push({ code: 'WORKSPACE_DATA_ASSUMPTION', electionKind: request.kind, severity: 'warning', message: note });
  for (const warning of projection.warnings) {
    resultWarnings.push({
      code: warning.code,
      electionKind: request.kind,
      severity: 'warning',
      message: warning.message,
      todoReference: warning.todoReference
    });
  }


  const result: SimulationResult = {
    type: 'result',
    status: 'completed',
    tables: [
      ...buildPoliticsResultTables(runs),
      ...buildPoliticsResultPlotTables(runs),
      scenarioProjectionTable(projection.rows),
      summarizePoliticsGeneratedRuns(runs)
    ],
    warnings: resultWarnings,
    benchmark: {
      startedAt: startedIso,
      elapsedMs: performance.now() - startedAt,
      simulations: simulationCount,
      dataVersion: descriptor.version,
      scrutinyAlgorithmId: scrutinyAlgorithm.id,
      seed: request.seed
    }
  };

  progress(post, startedAt, 'summarize', 5, 5);
  post(result);
}

