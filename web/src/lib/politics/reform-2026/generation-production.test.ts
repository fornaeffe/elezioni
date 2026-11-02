import { readFile, access } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
import { gunzipSync } from 'fflate';
import { expect, it } from 'vitest';
import catalog from './data/generation-catalog.generated.json';
import { generateReformSimulation, prepareReformGeneration } from './generation';
import { runPoliticsReformScrutiny } from './scrutiny';
import type { ReformGenerationSnapshot } from './generation-types';
import { setReformReference } from './scenario-editor';

const path = fileURLToPath(new URL(`../../../../static/${catalog.path}`, import.meta.url));
const present = await access(path).then(() => true, () => false);
it.skipIf(!present).each(['common', 'camera', 'senato'] as const)('reconciles the production FdI 26.7%% %s editor reference and generates a valid sample', async side => {
  const snapshot: ReformGenerationSnapshot = JSON.parse(new TextDecoder().decode(gunzipSync(await readFile(path))));
  const defaults = JSON.parse(await readFile(new URL(`./data/${catalog.defaultsPath}`, import.meta.url), 'utf8'));
  const listId = defaults.lists.camera.find((l: { name: string }) => l.name === "FRATELLI D'ITALIA CON GIORGIA MELONI").id;
  const edited = setReformReference(defaults, { kind: 'national' }, listId, side, 26.7);
  const prepared = prepareReformGeneration(edited, snapshot);
  for (const ch of ['camera', 'senato'] as const) {
    const units = prepared.references.units;
    const valid = (u: typeof units[number]) => u.municipality[ch].electors * (1 - u[ch].abstention);
    const actual = units.reduce((n, u) => n + valid(u) * u[ch].shares.get(listId)!, 0) / units.reduce((n, u) => n + valid(u), 0);
    const ref = prepared.references.trace.references.find(r => r.scope.kind === 'national' && r.listIds[ch] === listId)![ch]!;
    expect(actual).toBeCloseTo(ref.share, 8);
    if (side === ch) expect(actual).toBeCloseTo(0.267, 8);
  }
  const generated = generateReformSimulation(prepared, 1);
  expect(generated.input.chamberModel.kind).toBe('generated');
  // Generation validates the complete legal input before returning it.
  expect(generated.input.renewal.kind).toBe('joint');
}, 30000);

it.skipIf(!present)('R26-19 required production bundle: acquire/generate with data:reform commands when absent (release check never skips)', async () => {
  const compressed = await readFile(path), hash = (b: Uint8Array) => createHash('sha256').update(b).digest('hex');
  expect(hash(compressed)).toBe(catalog.sha256);
  const bytes = gunzipSync(compressed); expect(hash(bytes)).toBe(catalog.uncompressedSha256);
  const snapshot: ReformGenerationSnapshot = JSON.parse(new TextDecoder().decode(bytes));
  const defaults = JSON.parse(await readFile(new URL(`./data/${catalog.defaultsPath}`, import.meta.url), 'utf8'));
  const prepared = prepareReformGeneration(defaults, snapshot);
  const generated = generateReformSimulation(prepared, 1);
  expect(generated.input.chamberModel.kind).toBe('generated');
  const result = runPoliticsReformScrutiny(generated.input);
  expect(result.seats).toHaveLength(588);
  expect(result.stages.filter(s => s.status === 'non-computable')).toEqual([]);
}, 30000);
