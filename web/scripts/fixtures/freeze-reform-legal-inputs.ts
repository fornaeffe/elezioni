/** One-time capture of the R26-19 legal regressions, never production inputs.
 * Refuse replacement: future legal corrections must review a new fixture version.
 * Run from web: node --import tsx scripts/fixtures/freeze-reform-legal-inputs.ts
 */
import { readFile, writeFile } from 'node:fs/promises';
import { createHash } from 'node:crypto';
import { gzipSync, gunzipSync } from 'node:zlib';
import { resolve } from 'node:path';
import { prepareReformGeneration, generateReformSimulation } from '../../src/lib/politics/reform-2026/generation';

const root = resolve(import.meta.dirname, '../../..');
const catalog = JSON.parse(await readFile(resolve(root, 'web/src/lib/politics/reform-2026/data/generation-catalog.generated.json'), 'utf8'));
const bytes = await readFile(resolve(root, 'web/static', catalog.path));
const snapshot = JSON.parse(gunzipSync(bytes).toString('utf8'));
const defaults = JSON.parse(await readFile(resolve(root, 'web/src/lib/politics/reform-2026/data', catalog.defaultsPath), 'utf8'));
const prepared = prepareReformGeneration(defaults, snapshot);
const ids = [1, 4, 165, 31, 42, 80, 598, 792, 49, 242];
const runs = Object.fromEntries(ids.map(id => [id, generateReformSimulation(prepared, id)]));
const { version, sourceVersions, assumptions, seats, models } = snapshot;
const content = Buffer.from(JSON.stringify({ defaults, context: { version, sourceVersions, assumptions, seats, models }, runs }));
const compressed = gzipSync(content, { level: 9 });
const sha256 = (b: Uint8Array) => createHash('sha256').update(b).digest('hex');
const path = 'reform-legal-inputs-v1.json.gz';
const manifest = { schemaVersion: 1, purpose: 'Frozen legal scrutiny regressions; historical generator, not current M01/M02 acceptance',
  path, bytes: compressed.length, sha256: sha256(compressed), uncompressedSha256: sha256(content),
  source: { catalog, seed: defaults.seed, runtimeSha256: sha256(bytes) },
  runs: ids.map(id => ({ id, inputSha256: sha256(Buffer.from(JSON.stringify(runs[id].input))) })) };
await writeFile(resolve(root, 'test/fixtures', path), compressed, { flag: 'wx' });
await writeFile(resolve(root, 'test/fixtures/reform-legal-inputs-v1.manifest.json'), JSON.stringify(manifest, null, 2) + '\n', { flag: 'wx' });
console.log(JSON.stringify({ path, bytes: compressed.length, runs: ids.length }));
