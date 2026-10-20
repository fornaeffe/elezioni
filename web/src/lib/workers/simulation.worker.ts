import { runtimeDataUrl, requireSimulationCount } from '$lib/core/runtime-data';
import { runStoredReform, handleResultService, type ResultServiceRequest } from './result-service';
import { REFORM_ALGORITHM_ID } from '$lib/politics/reform-2026/input';
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
const politicsStaticSnapshotPath = '/data/v1/politics-static.json';
const regionalErGeneratedChunkSize = 50;
const regionalErStaticSnapshotPath = '/data/v1/regional-er-static.json';

interface LoadedPoliticsStaticSnapshot {
  snapshot: PoliticsStaticSnapshot;
  path: string;
}

interface LoadedRegionalErStaticSnapshot {
  snapshot: RegionalErStaticSnapshot;
  path: string;
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

async function fetchPoliticsStaticSnapshot(path: string, base: string): Promise<PoliticsStaticSnapshot | null> {
  const response = await fetch(runtimeDataUrl(base, path));
  if (!response.ok) return null;
  return (await response.json()) as PoliticsStaticSnapshot;
}

async function loadPoliticsStaticSnapshot(base: string): Promise<LoadedPoliticsStaticSnapshot> {
  const productionSnapshot = await fetchPoliticsStaticSnapshot(politicsStaticSnapshotPath, base);
  if (productionSnapshot) {
    return {
      snapshot: productionSnapshot,
      path: politicsStaticSnapshotPath
    };
  }

  throw new Error(`Missing production politics snapshot ${runtimeDataUrl(base, politicsStaticSnapshotPath)}. Run the production data generators.`);
}

async function fetchRegionalErStaticSnapshot(path: string, base: string): Promise<RegionalErStaticSnapshot | null> {
  const response = await fetch(runtimeDataUrl(base, path));
  if (!response.ok) return null;
  return (await response.json()) as RegionalErStaticSnapshot;
}

async function loadRegionalErStaticSnapshot(base: string): Promise<LoadedRegionalErStaticSnapshot> {
  const snapshot = await fetchRegionalErStaticSnapshot(regionalErStaticSnapshotPath, base);
  if (!snapshot) {
    throw new Error(`Unable to load Emilia-Romagna regional static snapshot from ${regionalErStaticSnapshotPath}. Run scripts/export_regional_er_static_snapshot.R.`);
  }
  return { snapshot, path: regionalErStaticSnapshotPath };
}

async function handleRequest(request: SimulationRequest): Promise<void> {
  const startedAt = performance.now();
  const startedIso = new Date().toISOString();
  const base = request.basePath ?? '';

  progress(startedAt, 'validate', 0, request.simulations);
  requireSimulationCount(request.simulations);
  if (request.kind === 'politiche' && request.scrutinyAlgorithmId === REFORM_ALGORITHM_ID) {
    post(await runStoredReform(request, post));
    return;
  }
  if ('kind' in request.scenario) throw new Error('Reform scenario requires the reform algorithm');
  const scenario = request.scenario;
  // The unchanged legacy scatter plots mount every subject/run SVG point.
  // Keep their existing DOM bound until that view has a separate rendering
  // redesign. Reform details are paginated and have no arbitrary run cap.
  if (request.kind === 'politiche' && request.simulations > 1000) {
    throw new RangeError('La legge precedente ammette al massimo 1000 simulazioni: i grafici esistenti visualizzano un elemento SVG per ogni punto. La riforma usa dettagli paginati senza questo limite.');
  }
  progress(startedAt, 'validate', 0, 5);

  if (request.kind !== 'politiche' && request.kind !== 'regionali-er') {
    progress(startedAt, 'summarize', 5, 5);
    post({
      type: 'result',
      status: 'not_implemented',
      tables: [
        scenarioProjectionTable(
          scenario.lists.map((row) => ({
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

  if (request.kind === 'regionali-er') {
    progress(startedAt, 'prepare', 1, 5);
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

    progress(startedAt, 'simulate', generatedSimulations, simulationCount);

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
      progress(startedAt, 'simulate', generatedSimulations, simulationCount);
      progress(startedAt, 'scrutinize', scrutinizedRuns, simulationCount);

      for (const simulation of snapshot.simulations) {
        const globalSimulation = chunkStart + simulation.sim - 1;
        const runStartedAt = performance.now();
        const output = runRegionalErScrutiny(
          simulation.input,
          {
            pop_legale: scenarioSource.pop_legale,
            liste: scenarioSource.liste
          },
          { seed: `${request.seed}:regional-er:scrutiny:${globalSimulation}` }
        );

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
          progress(startedAt, 'scrutinize', scrutinizedRuns, simulationCount);
        }
      }
    }

    const resultWarnings: ScrutinyWarning[] = [
      {
        code: 'REGIONAL_ER_STATIC_SNAPSHOT',
        electionKind: request.kind,
        severity: 'info',
        message: `Running the TypeScript regional pipeline on ${loadedSnapshot.path} exported from the current R preparation pipeline.`,
        todoReference: 'MIGRATION_PLAN.md#next-work'
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
        dataVersion: request.dataVersion
      }
    };

    progress(startedAt, 'summarize', 5, 5);
    post(result);
    return;
  }

  progress(startedAt, 'prepare', 1, 5);
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
      const output = runPoliticsSimulation({ algorithmId: 'politiche-r-parity-v1', simulationId: globalSimulation,
        chambers: pairs.map(p => ({ input: p.input, context: { ramo: p.ramo, liste_naz: p.data.liste_naz,
          totali_pluri: p.data.totali_pluri, totale_seggi: p.data.totale_seggi } })) }, undefined,
        (chamber, elapsedMs) => chamberTimes.set(chamber, elapsedMs));
      if (output.algorithmId !== 'politiche-r-parity-v1') throw new Error('Unexpected legacy result');
      for (const chamber of output.chambers) {
        const pair = pairs.find(p => p.ramo === chamber.chamber)!;
        runs.push(summarizePoliticsScrutinyRun({ ramo: pair.ramo, sim: globalSimulation,
          elapsedMs: chamberTimes.get(pair.ramo)!, input: pair.input,
          output: chamber.output, listeNaz: pair.data.liste_naz }));
        scrutinizedRuns++;
      }
      if (scrutinizedRuns % 10 === 0 || scrutinizedRuns === totalScrutinyRuns) progress(startedAt, 'scrutinize', scrutinizedRuns, totalScrutinyRuns);
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
      message: `Running the TypeScript generated pipeline on ${loadedSnapshot.path} exported from the current R preparation pipeline.`,
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
      scrutinyAlgorithmId: scrutinyAlgorithm.id,
      seed: request.seed
    }
  };

  progress(startedAt, 'summarize', 5, 5);
  post(result);
}

self.onmessage = (event: MessageEvent<SimulationRequest | ResultServiceRequest>) => {
  const request = event.data;
  if ('type' in request && request.type === 'result-service') {
    void handleResultService(request).then(reply => self.postMessage(reply)); return;
  }
  const simulationRequest = request as SimulationRequest;
  handleRequest(simulationRequest).catch((error) => {
    post({
      type: 'result',
      status: 'not_implemented',
      tables: [],
      warnings: [
        {
          code: 'WORKER_ERROR',
          electionKind: simulationRequest.kind,
          severity: 'error',
          message: error instanceof Error ? error.message : String(error)
        }
      ],
      benchmark: {
        startedAt: new Date().toISOString(),
        elapsedMs: 0,
        simulations: simulationRequest.simulations,
        dataVersion: simulationRequest.dataVersion,
        scrutinyAlgorithmId: simulationRequest.scrutinyAlgorithmId,
        seed: simulationRequest.seed
      }
    });
  });
};

export {};
