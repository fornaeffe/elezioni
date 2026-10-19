import { fileURLToPath } from 'node:url';
import { resolve, dirname } from 'node:path';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { gzipSync, gunzipSync } from 'fflate';
import { acquireSource, sha256, writeVersion } from './sources';
import { prepareReformModels, readReformManifest } from './reform-models';
import { prepareReformGeneration, generateReformSimulation } from '../../src/lib/politics/reform-2026/generation';
import { runPoliticsReformScrutiny } from '../../src/lib/politics/reform-2026/scrutiny';
import type { ReformGenerationSnapshot } from '../../src/lib/politics/reform-2026/generation-types';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const [command, ...args] = process.argv.slice(2);
const option = (name: string, fallback: string) => args.includes(name) ? args[args.indexOf(name) + 1] ?? fallback : fallback;
const sources = resolve(option('--sources', resolve(root, 'data/sources')));
const output = resolve(option('--output', resolve(root, 'data/derived')));
const runtimeDirectory = resolve(root, 'web/static/data/reform-2026');
async function writeImmutableBytes(path: string, bytes: Uint8Array) {
  try { if (!Buffer.from(bytes).equals(await readFile(path))) throw new Error(`Immutable runtime artifact differs: ${path}`); return; }
  catch (error) { if ((error as NodeJS.ErrnoException).code !== 'ENOENT') throw error; }
  await mkdir(dirname(path), { recursive: true }); await writeFile(path, bytes, { flag: 'wx' });
}
if (command === 'download') {
  const manifest = await readReformManifest(root), only = option('--only', ''), selected = manifest.sources.filter(s => !only || s.id === only);
  if (!selected.length) throw new Error(`Unknown reform source ${only}`);
  for (const s of selected) { await acquireSource(sources, s); console.log(`Verified ${s.id}`); }
} else if (command === 'generate') {
  const prepared = await prepareReformModels(root, sources);
  const defaultsVersion = `r26-defaults-v1-${sha256(JSON.stringify(prepared.defaults)).slice(0, 12)}`;
  await writeVersion(resolve(output, `${prepared.snapshot.version}.json`), prepared.snapshot);
  await writeVersion(resolve(output, `${prepared.snapshot.version}-audit.json`), prepared.audit);
  await writeVersion(resolve(output, `${prepared.models.version}.json`), prepared.models);
  await writeVersion(resolve(output, `${defaultsVersion}.json`), prepared.defaults);
  const bytes = new TextEncoder().encode(JSON.stringify(prepared.snapshot));
  const compressed = gzipSync(bytes, { level: 9, mtime: 0 });
  const catalog = { schemaVersion: 1, kind: 'politics-reform-generation', snapshotVersion: prepared.snapshot.version, modelVersion: prepared.models.version, defaultsVersion,
    path: `data/reform-2026/${prepared.snapshot.version}.json.gz`, sha256: sha256(compressed), bytes: compressed.length, uncompressedSha256: sha256(bytes),
    defaultsPath: `${defaultsVersion}.json`, modelsPath: `${prepared.models.version}.json` };
  await writeVersion(resolve(output, `${prepared.snapshot.version}-${defaultsVersion}-catalog.json`), catalog);
  if (args.includes('--publish')) {
    await writeImmutableBytes(resolve(runtimeDirectory, `${prepared.snapshot.version}.json.gz`), compressed);
    await writeVersion(resolve(root, `web/src/lib/politics/reform-2026/data/${prepared.models.version}.json`), prepared.models);
    await writeVersion(resolve(root, `web/src/lib/politics/reform-2026/data/${defaultsVersion}.json`), prepared.defaults);
    // The catalog pointer is generated; immutable contents use versioned paths.
    await writeFile(resolve(root, 'web/src/lib/politics/reform-2026/data/generation-catalog.generated.json'), `${JSON.stringify(catalog, null, 2)}\n`);
  }
  console.log(JSON.stringify({ snapshotVersion: prepared.snapshot.version, modelVersion: prepared.models.version, municipalities: prepared.audit.municipalities,
    fit: { passed: prepared.audit.preferenceFit.passed, intensityMae: prepared.audit.preferenceFit.validation.intensityMae, leadingShareMae: prepared.audit.preferenceFit.validation.leadingShareMae },
    runtimeBytes: compressed.length, uncompressedBytes: bytes.length, publishedLocally: args.includes('--publish') }, null, 2));
} else if (command === 'check') {
  const catalog = JSON.parse(await readFile(resolve(root, 'web/src/lib/politics/reform-2026/data/generation-catalog.generated.json'), 'utf8'));
  const compressed = await readFile(resolve(root, 'web/static', catalog.path));
  if (compressed.length !== catalog.bytes || sha256(compressed) !== catalog.sha256) throw new Error('Runtime bundle checksum mismatch');
  const bytes = gunzipSync(compressed);
  if (sha256(bytes) !== catalog.uncompressedSha256) throw new Error('Decompressed runtime checksum mismatch');
  const snapshot: ReformGenerationSnapshot = JSON.parse(new TextDecoder().decode(bytes));
  const defaults = JSON.parse(await readFile(resolve(root, 'web/src/lib/politics/reform-2026/data', catalog.defaultsPath), 'utf8'));
  const models = JSON.parse(await readFile(resolve(root, 'web/src/lib/politics/reform-2026/data', catalog.modelsPath), 'utf8'));
  if (JSON.stringify(models) !== JSON.stringify(snapshot.models) || snapshot.version !== catalog.snapshotVersion) throw new Error('Catalog/default/model versions differ');
  if (catalog.defaultsVersion !== `r26-defaults-v1-${sha256(JSON.stringify(defaults)).slice(0, 12)}`) throw new Error('Generated defaults fingerprint mismatch');
  const input = generateReformSimulation(prepareReformGeneration(defaults, snapshot), 1).input;
  const result = runPoliticsReformScrutiny(input);
  if (result.stages.some(s => s.status === 'non-computable')) throw new Error(`Production vertical slice is non-computable: ${JSON.stringify(result.stages)}`);
  console.log(JSON.stringify({ snapshotVersion: snapshot.version, simulationId: input.simulationId, status: result.status,
    stages: result.stages.length, seats: result.seats.length, filled: result.seats.filter(s => s.status === 'filled').length,
    unfilled: result.seats.filter(s => s.status === 'unfilled').length, unresolved: result.seats.filter(s => s.status === 'unresolved').length, checksumVerified: true }, null, 2));
} else throw new Error('Usage: data:reform:download | data:reform:generate [--publish] | data:reform:check');
