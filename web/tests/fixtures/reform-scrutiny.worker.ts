import { runPoliticsSimulation, resolvePoliticsSimulationAlgorithm, type PoliticsSimulationRequest } from '../../src/lib/politics/simulation-scrutiny';
import { prepareFastGeneration } from '../../src/lib/politics/reform-2026/vote-share/generation';
import type {FastVoteAssets,FastGenerationScenario} from '../../src/lib/politics/reform-2026/vote-share/forecast-types';
import type { ReformAssemblyContext } from '../../src/lib/politics/reform-2026/generation-types';
import { allocateTerritorialMatrix, type TerritorialMatrixInput } from '../../src/lib/politics/reform-2026/territorial-matrix';
import { createSeededDrawContext } from '../../src/lib/politics/reform-2026/ties';

// Test-only worker: no route, generator or production-worker activation.
self.onmessage = (event: MessageEvent<{ request?: PoliticsSimulationRequest; production?: boolean;
  matrix?: TerritorialMatrixInput;
  generation?: { scenario: FastGenerationScenario; assets:FastVoteAssets; context: ReformAssemblyContext; simulationId: number } }>) => {
  try {
    if (event.data.matrix) {
      self.postMessage({ ok: true, matrix: allocateTerritorialMatrix(event.data.matrix, createSeededDrawContext('senate-I14', 'browser')) });
      return;
    }
    const { generation } = event.data;
    const generated = generation && prepareFastGeneration(generation.scenario, generation.assets, generation.context).run(generation.simulationId);
    const request = generated ? { algorithmId: generated.input.algorithmId, simulationId: generated.input.simulationId, input: generated.input } : event.data.request;
    if (!request) throw new Error('Missing test request');
    if (event.data.production) resolvePoliticsSimulationAlgorithm(request.algorithmId, 'production');
    self.postMessage({ ok: true, result: runPoliticsSimulation(request) });
  } catch (error) {
    self.postMessage({ ok: false, message: error instanceof Error ? error.message : String(error) });
  }
};
