import type {
  ResultTable,
  ScenarioList,
  SimulationProgress,
  SimulationRequest,
  SimulationResult,
  SimulationWorkerMessage
} from '$lib/core/types';
import { runPoliticsScrutiny } from '$lib/politics/scrutiny';
import type { PoliticsDirectScrutinySnapshot, PoliticsScrutinyOutput, Ramo } from '$lib/politics/types';

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

function scenarioShareTable(lists: ScenarioList[]): ResultTable {
  const total = lists.reduce((sum, row) => sum + Math.max(row.startingShare, 0), 0);

  return {
    name: 'Scenario shares',
    columns: ['Lista', 'Coalizione', 'Quota'],
    rows: lists.map((row) => ({
      Lista: row.name,
      Coalizione: row.coalition,
      Quota: total > 0 ? Number(((100 * row.startingShare) / total).toFixed(2)) : 0
    }))
  };
}

async function loadPoliticsSnapshot(): Promise<PoliticsDirectScrutinySnapshot> {
  const response = await fetch('/data/v1/politics-debug-scrutiny.json');
  if (!response.ok) {
    throw new Error(`Unable to load politics direct-scrutiny snapshot: ${response.status}`);
  }

  return (await response.json()) as PoliticsDirectScrutinySnapshot;
}

function summarizeDirectScrutinyRuns(
  runs: Array<{ ramo: Ramo; sim: number; elapsedMs: number; output: PoliticsScrutinyOutput }>
): ResultTable {
  return {
    name: 'Direct scrutiny runs',
    columns: ['Ramo', 'Sim', 'Seggi pluri', 'Eletti uni', 'Candidati pluri eletti', 'Tempo ms'],
    rows: runs.map((run) => ({
      Ramo: run.ramo,
      Sim: run.sim,
      'Seggi pluri': run.output.liste_pluri.reduce((sum, row) => sum + row.ELETTI, 0),
      'Eletti uni': run.output.candidati_uni.filter((row) => row.ELETTO).length,
      'Candidati pluri eletti': run.output.candidati_pluri.filter((row) => row.ELETTO).length,
      'Tempo ms': Number(run.elapsedMs.toFixed(1))
    }))
  };
}

function summarizeListSeats(
  runs: Array<{ ramo: Ramo; sim: number; output: PoliticsScrutinyOutput }>,
  simulationCount: number
): ResultTable {
  const byRamoList = new Map<string, { ramo: Ramo; lista: string; total: number; values: number[] }>();

  for (const run of runs) {
    const seatsByList = new Map<string, number>();
    for (const row of run.output.liste_pluri) {
      seatsByList.set(row.LISTA, (seatsByList.get(row.LISTA) ?? 0) + row.ELETTI);
    }

    for (const [lista, seats] of seatsByList) {
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

async function handleRequest(request: SimulationRequest): Promise<void> {
  const startedAt = performance.now();
  const startedIso = new Date().toISOString();

  progress(startedAt, 'validate', 0, 5);

  if (request.kind !== 'politiche') {
    progress(startedAt, 'summarize', 5, 5);
    post({
      type: 'result',
      status: 'not_implemented',
      tables: [scenarioShareTable(request.scenario.lists)],
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
  const snapshot = await loadPoliticsSnapshot();
  const availableSimulations = Math.min(
    snapshot.rami.camera.simulations.length,
    snapshot.rami.senato.simulations.length
  );
  const requestedSimulations = Math.max(1, Math.floor(request.simulations));
  const simulationCount = Math.min(requestedSimulations, availableSimulations);

  progress(startedAt, 'simulate', 2, 5);
  const runs: Array<{ ramo: Ramo; sim: number; elapsedMs: number; output: PoliticsScrutinyOutput }> = [];

  progress(startedAt, 'scrutinize', 3, 5);
  for (const ramo of ['camera', 'senato'] as Ramo[]) {
    const ramoSnapshot = snapshot.rami[ramo];
    for (const simulation of ramoSnapshot.simulations.slice(0, simulationCount)) {
      const runStartedAt = performance.now();
      const output = runPoliticsScrutiny(simulation.input, {
        ramo,
        liste_naz: ramoSnapshot.liste_naz,
        totali_pluri: ramoSnapshot.totali_pluri,
        totale_seggi: ramoSnapshot.totale_seggi
      });
      runs.push({
        ramo,
        sim: simulation.sim,
        elapsedMs: performance.now() - runStartedAt,
        output
      });
    }
  }

  const result: SimulationResult = {
    type: 'result',
    status: 'completed',
    tables: [
      scenarioShareTable(request.scenario.lists),
      summarizeDirectScrutinyRuns(runs),
      summarizeListSeats(runs, simulationCount)
    ],
    warnings: [
      {
        code: 'POLITICS_SCENARIO_GENERATOR_PENDING',
        electionKind: request.kind,
        message:
          'Running the TypeScript scrutiny core on the bundled debug snapshot; scenario-to-vote generation is not ported yet.',
        todoReference: 'MIGRATION_PLAN.md#implementation-checklist'
      },
      ...(requestedSimulations > simulationCount
        ? [
            {
              code: 'POLITICS_SNAPSHOT_SIMULATION_LIMIT',
              electionKind: request.kind,
              message: `Requested ${requestedSimulations} simulations, but the debug snapshot contains ${simulationCount}.`,
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
