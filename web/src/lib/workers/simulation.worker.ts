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

const politicsGeneratedSimulationLimit = 1000;
const politicsGeneratedChunkSize = 50;
const politicsStaticSnapshotPath = '/data/v1/politics-static.json';
const politicsDebugStaticSnapshotPath = '/data/v1/politics-static-debug.json';

interface LoadedPoliticsStaticSnapshot {
  snapshot: PoliticsStaticSnapshot;
  path: string;
  fallback: boolean;
}

function post(message: SimulationWorkerMessage): void {
  self.postMessage(message);
}

function progress(
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

function scenarioProjectionTable(rows: PoliticsScenarioProjectionRow[]): ResultTable {
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

async function fetchPoliticsStaticSnapshot(path: string): Promise<PoliticsStaticSnapshot | null> {
  const response = await fetch(path);
  if (!response.ok) return null;
  return (await response.json()) as PoliticsStaticSnapshot;
}

async function loadPoliticsStaticSnapshot(): Promise<LoadedPoliticsStaticSnapshot> {
  const productionSnapshot = await fetchPoliticsStaticSnapshot(politicsStaticSnapshotPath);
  if (productionSnapshot) {
    return {
      snapshot: productionSnapshot,
      path: politicsStaticSnapshotPath,
      fallback: false
    };
  }

  const debugSnapshot = await fetchPoliticsStaticSnapshot(politicsDebugStaticSnapshotPath);
  if (debugSnapshot) {
    return {
      snapshot: debugSnapshot,
      path: politicsDebugStaticSnapshotPath,
      fallback: true
    };
  }

  throw new Error(`Unable to load politics static snapshot from ${politicsStaticSnapshotPath} or ${politicsDebugStaticSnapshotPath}`);
}

async function handleRequest(request: SimulationRequest): Promise<void> {
  const startedAt = performance.now();
  const startedIso = new Date().toISOString();

  progress(startedAt, 'validate', 0, 5);

  if (request.kind !== 'politiche') {
    progress(startedAt, 'summarize', 5, 5);
    post({
      type: 'result',
      status: 'not_implemented',
      tables: [
        scenarioProjectionTable(
          request.scenario.lists.map((row) => ({
            list: row.name,
            coalition: row.coalition,
            scenarioShare: row.startingShare,
            shareOverride: row.shareOverride,
            projectedShare: null,
            parameterSource: 'synthetic' as const,
            status: 'active' as const
          }))
        )
      ],
      warnings: [
        {
          code: 'ELECTION_KIND_NOT_PORTED',
          electionKind: request.kind,
          severity: 'warning',
          message: 'Only the politics scrutiny bridge is currently available.',
          todoReference: 'MIGRATION_PLAN.md#implementation-checklist'
        }
      ],
      benchmark: {
        startedAt: startedIso,
        elapsedMs: performance.now() - startedAt,
        simulations: request.simulations,
        dataVersion: request.dataVersion
      }
    });
    return;
  }

  progress(startedAt, 'prepare', 1, 5);
  const algorithmResolution = resolvePoliticsScrutinyAlgorithm(request.scrutinyAlgorithmId);
  const scrutinyAlgorithm = algorithmResolution.algorithm;
  const loadedSnapshot = await loadPoliticsStaticSnapshot();
  const staticSnapshot = loadedSnapshot.snapshot;
  const requestedSimulationInput = Math.floor(Number(request.simulations));
  const requestedSimulations =
    Number.isFinite(requestedSimulationInput) && requestedSimulationInput > 0 ? requestedSimulationInput : 1;
  const simulationCount = Math.min(requestedSimulations, politicsGeneratedSimulationLimit);
  const source = buildPoliticsPipelineSourceFromSnapshot(staticSnapshot, { simulations: simulationCount });
  const projection = projectScenarioOntoPoliticsSource(source, request.scenario, {
    currentDate: startedIso,
    electionDate: request.electionDate,
    historicalVotes: staticSnapshot.data.comuni_liste_elezioni,
    parameterPercentualiPartenza: 'europee',
    simulations: simulationCount
  });
  const scenarioSource = projection.source;

  const runs: PoliticsRunPresentationSummary[] = [];
  let generatedSimulations = 0;
  let scrutinizedRuns = 0;
  const totalScrutinyRuns = simulationCount * 2;

  progress(startedAt, 'simulate', generatedSimulations, simulationCount);

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
    progress(startedAt, 'simulate', generatedSimulations, simulationCount);
    progress(startedAt, 'scrutinize', scrutinizedRuns, totalScrutinyRuns);

    for (const ramo of ['camera', 'senato'] as Ramo[]) {
      const ramoSnapshot = snapshot.rami[ramo];
      for (const simulation of ramoSnapshot.simulations) {
        const runStartedAt = performance.now();
        const output = scrutinyAlgorithm.run(simulation.input, {
          ramo,
          liste_naz: ramoSnapshot.liste_naz,
          totali_pluri: ramoSnapshot.totali_pluri,
          totale_seggi: ramoSnapshot.totale_seggi
        });
        const globalSimulation = chunkStart + simulation.sim - 1;
        runs.push(
          summarizePoliticsScrutinyRun({
            ramo,
            sim: globalSimulation,
            elapsedMs: performance.now() - runStartedAt,
            input: simulation.input,
            output,
            listeNaz: ramoSnapshot.liste_naz
          })
        );

        scrutinizedRuns += 1;
        if (scrutinizedRuns % 10 === 0 || scrutinizedRuns === totalScrutinyRuns) {
          progress(startedAt, 'scrutinize', scrutinizedRuns, totalScrutinyRuns);
        }
      }
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
      code: loadedSnapshot.fallback ? 'POLITICS_DEBUG_STATIC_SNAPSHOT' : 'POLITICS_STATIC_SNAPSHOT',
      electionKind: request.kind,
      severity: loadedSnapshot.fallback ? 'warning' : 'info',
      message: loadedSnapshot.fallback
        ? 'Running the TypeScript generated pipeline on the debug static snapshot because the production static snapshot was not available.'
        : `Running the TypeScript generated pipeline on ${loadedSnapshot.path} exported from the current R preparation pipeline.`,
      todoReference: 'MIGRATION_PLAN.md#current-caveats'
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

  for (const warning of projection.warnings) {
    resultWarnings.push({
      code: warning.code,
      electionKind: request.kind,
      severity: 'warning',
      message: warning.message,
      todoReference: warning.todoReference
    });
  }

  if (requestedSimulations > simulationCount) {
    resultWarnings.push({
      code: 'POLITICS_GENERATED_PIPELINE_LIMIT',
      electionKind: request.kind,
      severity: 'warning',
      message: `Requested ${requestedSimulations} simulations, but the generated worker path is capped at ${simulationCount}.`,
      todoReference: 'MIGRATION_PLAN.md#current-caveats'
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
      dataVersion: request.dataVersion,
      scrutinyAlgorithmId: scrutinyAlgorithm.id
    }
  };

  progress(startedAt, 'summarize', 5, 5);
  post(result);
}

self.onmessage = (event: MessageEvent<SimulationRequest>) => {
  handleRequest(event.data).catch((error) => {
    post({
      type: 'result',
      status: 'not_implemented',
      tables: [],
      warnings: [
        {
          code: 'WORKER_ERROR',
          electionKind: event.data.kind,
          severity: 'error',
          message: error instanceof Error ? error.message : String(error)
        }
      ],
      benchmark: {
        startedAt: new Date().toISOString(),
        elapsedMs: 0,
        simulations: event.data.simulations,
        dataVersion: event.data.dataVersion,
        scrutinyAlgorithmId: event.data.scrutinyAlgorithmId
      }
    });
  });
};

export {};
