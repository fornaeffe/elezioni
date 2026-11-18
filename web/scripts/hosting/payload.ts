/** Stage only the current pinned production graph; never publish the checkout. */
import { createHash } from 'node:crypto';
import { copyFile, lstat, mkdir, readdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, isAbsolute, relative, resolve } from 'node:path';
import { gzipSync } from 'node:zlib';
import { decodeAsset, type ByteAsset } from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import { declaredAssets } from '../../src/lib/politics/scenario/dependencies';
import { authoredPoliticsDefault } from '../../src/lib/politics/scenario/defaults';
import { snapshotDescriptors } from '../../src/lib/workspaces/snapshots.generated';
import generation from '../../src/lib/politics/reform-2026/data/generation-catalog.generated.json' with { type: 'json' };

export const routes = ['', 'politics', 'politics/editor', 'emilia-romagna'];
export const digest = (bytes: Uint8Array | string) => createHash('sha256').update(bytes).digest('hex');
export function inside(root: string, path: string) {
  if (!path || path.includes('\\') || path.split('/').includes('..') || isAbsolute(path)) throw new Error(`Unsafe payload path: ${path}`);
  const target = resolve(root, path), rel = relative(root, target);
  if (rel.startsWith('..') || isAbsolute(rel)) throw new Error(`Outside payload: ${path}`);
  return target;
}
export async function files(root: string, prefix = ''): Promise<string[]> {
  const result: string[] = [];
  for (const entry of await readdir(inside(root, prefix || '.'), { withFileTypes: true })) {
    const path = prefix ? `${prefix}/${entry.name}` : entry.name;
    if (entry.isSymbolicLink()) throw new Error(`Payload contains link: ${path}`);
    if (entry.isDirectory()) result.push(...await files(root, path));
    else if (entry.isFile()) result.push(path);
    else throw new Error(`Unsupported payload entry: ${path}`);
  }
  return result.sort();
}

/** Follow and verify compressed metadata and column/chunk descriptors transitively. */
export async function compressedGraph(root: string, pointer: unknown) {
  const graph = new Map<string, ByteAsset>();
  async function visit(value: unknown, binary = false): Promise<void> {
    if (!value || typeof value !== 'object') return;
    const row = value as Record<string, unknown>;
    if (typeof row.path === 'string' && typeof row.compressedBytes === 'number' && typeof row.decodedSha256 === 'string') {
      const asset = Object.fromEntries(['path', 'compressedBytes', 'decodedBytes', 'sha256', 'decodedSha256']
        .map(key => [key, row[key]])) as unknown as ByteAsset;
      const previous = graph.get(asset.path);
      if (previous) {
        if (JSON.stringify(previous) !== JSON.stringify(asset)) throw new Error(`Conflicting payload descriptor: ${asset.path}`);
        return;
      }
      graph.set(asset.path, asset);
      const decoded = await decodeAsset(asset, await readFile(inside(root, asset.path)));
      // The v1 release stores typed-array state chunks and history .u32 chunks.
      // Catalogs may repeat the latter as bare ByteAsset descriptors.
      if (!binary && row.byteOrder !== 'little-endian' && !asset.path.includes('.u32-')) {
        let metadata: unknown;
        try { metadata = JSON.parse(new TextDecoder().decode(decoded)); }
        catch (error) { throw new Error(`Invalid release metadata ${asset.path}: ${error}`); }
        await visit(metadata);
      }
    } else {
      for (const [key, child] of Object.entries(row))
        await visit(child, key === 'chunk' && row.byteOrder === 'little-endian');
    }
  }
  await visit(pointer);
  if (!graph.size) throw new Error('Empty production release graph');
  return graph;
}

export async function stagePayload(web: string, output: string, revision: string) {
  const build = resolve(web, 'build'), required = new Map<string, { bytes?: number; sha256?: string }>();
  for (const pin of Object.values(snapshotDescriptors)) if (pin) required.set(pin.path, pin);
  required.set('data/v1/metadata.json', {});
  required.set(generation.path, generation);
  for (const pin of declaredAssets(authoredPoliticsDefault)) required.set(pin.uri, pin);
  const pointerPath = 'data/reform-2026/fast-share/candidate.json';
  const pointer = JSON.parse(await readFile(inside(build, pointerPath), 'utf8'));
  if (pointer.assetRoot !== 'data/reform-2026/fast-share/') throw new Error('Unexpected release root');
  required.set(pointerPath, {});
  const graph = await compressedGraph(inside(build, pointer.assetRoot), pointer);
  for (const asset of graph.values()) required.set(pointer.assetRoot + asset.path, { bytes: asset.compressedBytes, sha256: asset.sha256 });
  // Explicit route and compiled client trees exclude historical static versions,
  // archives, source maps, preparation receipts, fixtures and research output.
  for (const route of routes) required.set(route ? `${route}/index.html` : 'index.html', {});
  for (const path of await files(inside(build, '_app'))) {
    if (path.endsWith('.map')) throw new Error(`Unexpected production source map: ${path}`);
    required.set(`_app/${path}`, {});
  }
  await mkdir(output, { recursive: false }); // Existing output must never silently survive.
  for (const [path, pin] of required) {
    const source = inside(build, path);
    if (!(await lstat(source)).isFile()) throw new Error(`Production file required: ${path}`);
    const bytes = await readFile(source);
    if (pin.bytes !== undefined && pin.bytes !== bytes.length || pin.sha256 !== undefined && pin.sha256 !== digest(bytes))
      throw new Error(`Production checksum mismatch: ${path}`);
    const target = inside(output, path);
    await mkdir(dirname(target), { recursive: true });
    await copyFile(source, target);
  }
  const release = { schemaVersion: 1, revision, base: '/elezioni',
    snapshots: snapshotDescriptors, generation, catalogId: pointer.catalogId,
    authoredScenarioSha256: digest(JSON.stringify(authoredPoliticsDefault)) };
  await writeFile(inside(output, 'release.json'), `${JSON.stringify(release, null, 2)}\n`);
  await writeFile(inside(output, '.nojekyll'), '');
  const manifest = [];
  for (const path of await files(output)) {
    const bytes = await readFile(inside(output, path));
    manifest.push({ path, bytes: bytes.length, sha256: digest(bytes),
      // Estimate only: actual CDN transfer is measured by public browser smoke.
      gzipBytes: path.endsWith('.gz') ? bytes.length : gzipSync(bytes).length });
  }
  const bytes = manifest.reduce((n, f) => n + f.bytes, 0);
  if (bytes >= 1_000_000_000) throw new Error(`Pages 1 GB site budget exceeded: ${bytes}`);
  return { bytes, gzipEstimateBytes: manifest.reduce((n, f) => n + f.gzipBytes, 0),
    manifestSha256: digest(JSON.stringify(manifest)), files: manifest, release,
    excludedBuildFiles: (await files(build)).filter(path => !required.has(path)) };
}
