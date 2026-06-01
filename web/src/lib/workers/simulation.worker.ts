import type {
  ResultTable,
  ScenarioList,
  SimulationProgress,
  SimulationRequest,
  SimulationResult,
  SimulationWorkerMessage
} from '$lib/core/types';

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

self.onmessage = (event: MessageEvent<SimulationRequest>) => {
  const request = event.data;
  const startedAt = performance.now();
  const startedIso = new Date().toISOString();

  progress(startedAt, 'validate', 0, 4);
  progress(startedAt, 'prepare', 1, 4);
  progress(startedAt, 'simulate', 2, 4);
  progress(startedAt, 'scrutinize', 3, 4);

  const result: SimulationResult = {
    type: 'result',
    status: 'not_implemented',
    tables: [scenarioShareTable(request.scenario.lists)],
    warnings: [
      {
        code: 'POLITICS_SCRUTINY_NOT_PORTED',
        electionKind: request.kind,
        message: 'The TypeScript politics scrutiny core is not ported yet.',
        todoReference: 'MIGRATION_PLAN.md#implementation-checklist'
      }
    ],
    benchmark: {
      startedAt: startedIso,
      elapsedMs: performance.now() - startedAt,
      simulations: request.simulations,
      dataVersion: request.dataVersion
    }
  };

  progress(startedAt, 'summarize', 4, 4);
  post(result);
};

export {};
