/** Explicit authored-default update when the supported model law changes.
 * Regenerate/package afterwards with npm run data:prepare. */
import { readFile, writeFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve } from 'node:path';
import { releaseInputs } from '../data-preparation/fast-share-release-inputs';
import { buildPoliticsAssets } from './politics-assets';
import { modelRegistry } from '../../src/lib/politics/reform-2026/vote-share/model-registry';
import { parsePoliticsScenario } from '../../src/lib/politics/scenario/validation';
const root = fileURLToPath(new URL('../../../', import.meta.url));
const path = resolve(root, 'data/politics-default-scenario.json');
const scenario = JSON.parse(await readFile(path, 'utf8'));
const inputs = await releaseInputs(root, resolve(root, 'data/sources'), resolve(root, 'data/derived/fast-share-cache'));
const built = await buildPoliticsAssets(root, resolve(root, 'data/sources'), inputs);
for (const [id, model] of Object.entries(modelRegistry)) {
  scenario.modelSettings[id].probabilityLawVersion = model.version;
  for (const law of Object.values(scenario.data.lawAssets) as { models: Record<string, unknown> }[])
    law.models[id] = [built.assets.get(id)!.pin];
}
for (const selected of Object.values(scenario.selection.generatorsByLaw) as { id: keyof typeof modelRegistry; version: string }[])
  selected.version = modelRegistry[selected.id].version;
parsePoliticsScenario(scenario);
await writeFile(path, JSON.stringify(scenario, null, 2) + '\n');
console.log('Refreshed authored model versions and exact parameter pins; run data:prepare.');
