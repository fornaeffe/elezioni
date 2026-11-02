import { validateWorkspaceRequest } from '../workspaces/registry';
import type { SimulationRequest, SimulationWorkerMessage } from '../core/types';
import { handleResultService, runStoredReform, type ResultServiceRequest } from './result-service';
import { WorkspaceExecutionError } from '../workspaces/execution-error';

function post(message: SimulationWorkerMessage) { self.postMessage(message); }
async function run(request: SimulationRequest) {
  const definition = validateWorkspaceRequest(request);
  if (definition.storedResults) post(await runStoredReform(request, post));
  else {
    const { runLegacyWorkspace } = await import('../workspaces/legacy-runner');
    await runLegacyWorkspace(request, post);
  }
}
self.onmessage = (event: MessageEvent<SimulationRequest | ResultServiceRequest>) => {
  const request = event.data;
  if ('type' in request && request.type === 'result-service') {
    void handleResultService(request).then(reply => self.postMessage(reply)); return;
  }
  const simulation = request as SimulationRequest;
  const startedAt = new Date().toISOString(), started = performance.now();
  void run(simulation).catch(error => post({
    type: 'result', status: 'not_implemented', tables: [],
    warnings: [{ code: 'WORKER_ERROR', electionKind: simulation.kind, severity: 'error',
      ...(error instanceof WorkspaceExecutionError ? { simulationId: error.simulationId, chamber: error.chamber, scope: 'execution', reason: error.reason } : {}),
      message: error instanceof Error ? error.message : String(error) }],
    benchmark: { startedAt, elapsedMs: performance.now() - started, simulations: simulation.simulations,
      dataVersion: simulation.dataVersion, scrutinyAlgorithmId: simulation.scrutinyAlgorithmId, seed: simulation.seed },
  }));
};
export {};
