import catalog from './data/generation-catalog.generated.json';
import { requireReformScenario } from './generation-scenario';
import type { ReformModelBundle } from './generation-types';

// Follow the regenerated catalog rather than pinning old immutable filenames in
// application code. Only the selected defaults/models enter the UI contract.
const defaults = import.meta.glob('./data/r26-defaults-*.json', { eager: true, import: 'default' });
const models = import.meta.glob<ReformModelBundle>('./data/r26-models-*.json', { eager: true, import: 'default' });

export function bundledReformScenario() {
  const scenario = requireReformScenario(defaults[`./data/${catalog.defaultsPath}`]);
  if (scenario.snapshotVersion !== catalog.snapshotVersion || scenario.modelVersion !== catalog.modelVersion) throw new Error('Bundled reform defaults/catalog version mismatch');
  return scenario;
}

export function bundledReformModels(): ReformModelBundle {
  const model = models[`./data/${catalog.modelsPath}`];
  if (!model || model.version !== catalog.modelVersion) throw new Error('Bundled reform models/catalog version mismatch');
  return structuredClone(model);
}
