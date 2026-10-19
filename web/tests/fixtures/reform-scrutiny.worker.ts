import { runPoliticsSimulation, resolvePoliticsSimulationAlgorithm, type PoliticsSimulationRequest } from '../../src/lib/politics/simulation-scrutiny';

// Test-only worker: no route, generator or production-worker activation.
self.onmessage = (event: MessageEvent<{ request: PoliticsSimulationRequest; production?: boolean }>) => {
  try {
    if (event.data.production) resolvePoliticsSimulationAlgorithm(event.data.request.algorithmId, 'production');
    self.postMessage({ ok: true, result: runPoliticsSimulation(event.data.request) });
  } catch (error) {
    self.postMessage({ ok: false, message: error instanceof Error ? error.message : String(error) });
  }
};
