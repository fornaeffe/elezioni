import catalog from '../politics/reform-2026/data/generation-catalog.generated.json';
import { loadVerifiedJson } from '../core/runtime-data';
import { previewReformScenario, type PreviewSelection } from '../politics/reform-2026/editor-preview';
import type { ReformGenerationScenario, ReformGenerationSnapshot } from '../politics/reform-2026/generation-types';

let snapshot: Promise<ReformGenerationSnapshot> | undefined;
self.onmessage = async (event: MessageEvent<{ base: string; scenario: ReformGenerationScenario; selection?: PreviewSelection }>) => {
  try {
    snapshot ??= loadVerifiedJson(event.data.base, catalog).then(v => v as ReformGenerationSnapshot);
    self.postMessage({ preview: previewReformScenario(event.data.scenario, await snapshot, event.data.selection) });
  } catch (error) { snapshot = undefined; self.postMessage({ error: error instanceof Error ? error.message : String(error) }); }
};
