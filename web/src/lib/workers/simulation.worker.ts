import type {
  ResultTable,
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
import { runPoliticsScrutiny } from '$lib/politics/scrutiny';
import { buildPoliticsPipelineSourceFromSnapshot } from '$lib/politics/static-snapshot';
import type { PoliticsScrutinyOutput, PoliticsStaticSnapshot, Ramo } from '$lib/politics/types';

const politicsGeneratedSimulationLimit = 1000;
const politicsGeneratedChunkSize = 50;

interface GeneratedRunSummary {
  ramo: Ramo;
  sim: number;
  elapsedMs: number;
  pluriSeats: number;
  electedUni: number;
  electedPluri: number;
  seatsByList: Map<string, number>;
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
    columns: ['Lista', 'Coalizione', 'Quota scenario', 'Usata', 'Quota proiettata', 'Stato'],
    rows: rows.map((row) => ({
      Lista: row.list,
      Coalizione: row.coalition,
      'Quota scenario': row.scenarioShare === null ? null : Number(row.scenarioShare.toFixed(2)),
      Usata: row.shareOverride,
      'Quota proiettata': row.projectedShare === null ? null : Number(row.projectedShare.toFixed(2)),
      Stato: row.status
    }))
  };
}

async function loadPoliticsStaticSnapshot(): Promise<PoliticsStaticSnapshot> {
  const response = await fetch('/data/v1/politics-static-debug.json');
  if (!response.ok) {
    throw new Error(`Unable to load politics static snapshot: ${response.status}`);
  }

  return (await response.json()) as PoliticsStaticSnapshot;
}

function summarizeGeneratedRuns(runs: GeneratedRunSummary[]): ResultTable {
  return {
    name: 'Generated pipeline runs',
    columns: ['Ramo', 'Sim', 'Seggi pluri', 'Eletti uni', 'Candidati pluri eletti', 'Tempo ms'],
    rows: runs.map((run) => ({
      Ramo: run.ramo,
      Sim: run.sim,
      'Seggi pluri': run.pluriSeats,
      'Eletti uni': run.electedUni,
      'Candidati pluri eletti': run.electedPluri,
      'Tempo ms': Number(run.elapsedMs.toFixed(1))
    }))
  };
}

function summarizeListSeats(runs: GeneratedRunSummary[], simulationCount: number): ResultTable {
  const byRamoList = new Map<string, { ramo: Ramo; lista: string; total: number; values: number[] }>();

  for (const run of runs) {
    for (const [lista, seats] of run.seatsByList) {
      const key = `${run.ramo}\u001f${lista}`;
      const existing = byRamoList.get(key) ?? { ramo: run.ramo, lista, total: 0, values: [] };
      existing.total += seats;
      existing.values.push(seats);
      byRamoList.set(key, existing);
    }
  }

  return {
    name: 'Average plurinominal seats by list',
    columns: ['Ramo', 'Lista', 'Media', 'Min', 'Max'],
    rows: [...byRamoList.values()]
      .sort((left, right) => {
        if (left.ramo !== right.ramo) return left.ramo.localeCompare(right.ramo);
        return left.lista.localeCompare(right.lista);
      })
      .map((row) => ({
        Ramo: row.ramo,
        Lista: row.lista,
        Media: Number((row.total / simulationCount).toFixed(2)),
        Min: Math.min(...row.values),
        Max: Math.max(...row.values)
      }))
  };
}

function summarizeScrutinyOutput(
  ramo: Ramo,
  sim: number,
  elapsedMs: number,
  output: PoliticsScrutinyOutput
): GeneratedRunSummary {
  const seatsByList = new Map<string, number>();

  for (const row of output.liste_pluri) {
    seatsByList.set(row.LISTA, (seatsByList.get(row.LISTA) ?? 0) + row.ELETTI);
  }

  return {
    ramo,
    sim,
    elapsedMs,
    pluriSeats: output.liste_pluri.reduce((sum, row) => sum + row.ELETTI, 0),
    electedUni: output.candidati_uni.filter((row) => row.ELETTO).length,
    electedPluri: output.candidati_pluri.filter((row) => row.ELETTO).length,
    seatsByList
  };
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
            status: 'unmatched' as const
          }))
        )
      ],
      warnings: [
        {
          code: 'ELECTION_KIND_NOT_PORTED',
          electionKind: request.kind,
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
  const staticSnapshot = await loadPoliticsStaticSnapshot();
  const requestedSimulationInput = Math.floor(Number(request.simulations));
  const requestedSimulations =
    Number.isFinite(requestedSimulationInput) && requestedSimulationInput > 0 ? requestedSimulationInput : 1;
  const simulationCount = Math.min(requestedSimulations, politicsGeneratedSimulationLimit);
  const source = buildPoliticsPipelineSourceFromSnapshot(staticSnapshot, { simulations: simulationCount });
  const projection = projectScenarioOntoPoliticsSource(source, request.scenario, {
    electionDate: request.electionDate,
    simulations: simulationCount
  });
  const scenarioSource = projection.source;

  const runs: GeneratedRunSummary[] = [];
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
        const output = runPoliticsScrutiny(simulation.input, {
          ramo,
          liste_naz: ramoSnapshot.liste_naz,
          totali_pluri: ramoSnapshot.totali_pluri,
          totale_seggi: ramoSnapshot.totale_seggi
        });
        const globalSimulation = chunkStart + simulation.sim - 1;
        runs.push(summarizeScrutinyOutput(ramo, globalSimulation, performance.now() - runStartedAt, output));

        scrutinizedRuns += 1;
        if (scrutinizedRuns % 10 === 0 || scrutinizedRuns === totalScrutinyRuns) {
          progress(startedAt, 'scrutinize', scrutinizedRuns, totalScrutinyRuns);
        }
      }
    }
  }

  const result: SimulationResult = {
    type: 'result',
    status: 'completed',
    tables: [
      scenarioProjectionTable(projection.rows),
      summarizeGeneratedRuns(runs),
      summarizeListSeats(runs, simulationCount)
    ],
    warnings: [
      {
        code: 'POLITICS_DEBUG_STATIC_SNAPSHOT',
        electionKind: request.kind,
        message:
          'Running the TypeScript generated pipeline on a production-shaped debug static snapshot; full production data packaging is still pending.',
        todoReference: 'MIGRATION_PLAN.md#current-caveats'
      },
      {
        code: 'POLITICS_SCENARIO_PROJECTION',
        electionKind: request.kind,
        message:
          'Scenario lists are matched by name against the static snapshot; matched list presence, coalitions, and explicit global share overrides are projected into the generated pipeline.',
        todoReference: 'MIGRATION_PLAN.md#current-caveats'
      },
      ...projection.warnings.map((warning) => ({
        code: warning.code,
        electionKind: request.kind,
        message: warning.message,
        todoReference: warning.todoReference
      })),
      ...(requestedSimulations > simulationCount
        ? [
            {
              code: 'POLITICS_GENERATED_PIPELINE_LIMIT',
              electionKind: request.kind,
              message: `Requested ${requestedSimulations} simulations, but the generated worker path is capped at ${simulationCount}.`,
              todoReference: 'MIGRATION_PLAN.md#current-caveats'
            }
          ]
        : [])
    ],
    benchmark: {
      startedAt: startedIso,
      elapsedMs: performance.now() - startedAt,
      simulations: simulationCount,
      dataVersion: request.dataVersion
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
          message: error instanceof Error ? error.message : String(error)
        }
      ],
      benchmark: {
        startedAt: new Date().toISOString(),
        elapsedMs: 0,
        simulations: event.data.simulations,
        dataVersion: event.data.dataVersion
      }
    });
  });
};

export {};
