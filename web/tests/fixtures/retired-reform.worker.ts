import { runStoredReform, handleRetiredResultService as handleResultService } from './retired-result-service';
import type { ResultServiceRequest } from '../../src/lib/workers/result-service';
import type { SimulationRequest } from '../../src/lib/core/types';
import { validateWorkspaceRequest } from '../../src/lib/workspaces/registry';
self.onmessage = ({ data }: {
    data: SimulationRequest | ResultServiceRequest;
}) => {
    if ('type' in data && data.type === 'result-service') {
        void handleResultService(data).then(reply => self.postMessage(reply));
        return;
    }
    const request = data as SimulationRequest, startedAt = new Date().toISOString(), start = performance.now();
    void (async () => { validateWorkspaceRequest(request); return runStoredReform(request, p => self.postMessage(p)); })()
        .then(r => self.postMessage(r)).catch(error => self.postMessage({ type: 'result', status: 'not_implemented', tables: [],
        warnings: [{ code: 'WORKER_ERROR', electionKind: request.kind, severity: 'error', message: String(error) }],
        benchmark: { startedAt, elapsedMs: performance.now() - start, simulations: request.simulations, dataVersion: request.dataVersion,
            scrutinyAlgorithmId: request.scrutinyAlgorithmId, seed: request.seed } }));
};
