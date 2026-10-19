import { runPoliticsSimulation, resolvePoliticsSimulationAlgorithm, type PoliticsSimulationRequest } from '../../src/lib/politics/simulation-scrutiny';
import { generateReformSimulation, prepareReformGeneration } from '../../src/lib/politics/reform-2026/generation';
import type { ReformGenerationSnapshot } from '../../src/lib/politics/reform-2026/generation-types';

// Test-only worker: no route, generator or production-worker activation.
self.onmessage = (event: MessageEvent<{ request?: PoliticsSimulationRequest; production?: boolean;
  generation?: { scenario: unknown; snapshot: ReformGenerationSnapshot; simulationId: number } }>) => {
  try {
    const { generation } = event.data;
    const generated = generation && generateReformSimulation(prepareReformGeneration(generation.scenario, generation.snapshot), generation.simulationId);
    const request = generated ? { algorithmId: generated.input.algorithmId, simulationId: generated.input.simulationId, input: generated.input } : event.data.request;
    if (!request) throw new Error('Missing test request');
    if (event.data.production) resolvePoliticsSimulationAlgorithm(request.algorithmId, 'production');
    self.postMessage({ ok: true, result: runPoliticsSimulation(request) });
  } catch (error) {
    self.postMessage({ ok: false, message: error instanceof Error ? error.message : String(error) });
  }
};
