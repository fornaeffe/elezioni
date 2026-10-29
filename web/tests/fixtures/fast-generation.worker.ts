import { prepareFastGeneration } from '../../src/lib/politics/reform-2026/vote-share/generation';
import type {
  FastGenerationScenario,
  FastVoteAssets
} from '../../src/lib/politics/reform-2026/vote-share/forecast-types';
import type { ReformAssemblyContext } from '../../src/lib/politics/reform-2026/generation-types';
import { calibrateReferences } from '../../src/lib/politics/reform-2026/vote-share/reference-family';
export interface FastGenerationRequest {
  scenario: FastGenerationScenario;
  assets: FastVoteAssets;
  context: ReformAssemblyContext;
  simulationIds: number[];
  chunkSize: number;
}
const scope = self as unknown as {
  onmessage: ((event: MessageEvent<FastGenerationRequest>) => void) | null;
  postMessage(value: unknown): void;
};
scope.onmessage = async (event) => {
  const started = performance.now();
  try {
    const r = event.data,
      p = prepareFastGeneration(r.scenario, r.assets, r.context, { chunkSize: r.chunkSize }),
      results = [];
    for (const id of r.simulationIds) {
      const { input, audit } = p.run(id),
        bytes = new TextEncoder().encode(JSON.stringify(input));
      const digest = await crypto.subtle.digest('SHA-256', bytes);
      results.push({
        id,
        hash: [...new Uint8Array(digest)].map((x) => x.toString(16).padStart(2, '0')).join(''),
        audit
      });
    }
    const calibration = calibrateReferences(
      [
        { id: 'a', members: ['a'], mean: 0.3, sdPercentagePoints: 2 },
        { id: 'b', members: ['b'], mean: 0.4, sdPercentagePoints: 3 }
      ],
      true
    );
    scope.postMessage({ ok: true, results, calibration, milliseconds: performance.now() - started });
  } catch (e) {
    scope.postMessage({ ok: false, error: e instanceof Error ? e.message : String(e) });
  }
};
