import { afterEach, expect, test } from 'vitest';
import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { resolve } from 'node:path';
import { compressAsset } from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import { compressedGraph, inside } from './payload';
const temporary: string[] = [];
afterEach(async () => { await Promise.all(temporary.splice(0).map(path => rm(path, { recursive: true, force: true }))); });

test('publish graph requires lazy transitive columns, verifies bytes and excludes unrelated versions', async () => {
  const root = await mkdtemp(resolve(tmpdir(), 'elezioni-hosting-')); temporary.push(root);
  const column = await compressAsset('fast-share-test-columns', new Uint8Array([1, 2, 3, 4]));
  const history = await compressAsset('history-test.u32', new Uint8Array([0, 0, 1, 0]));
  const metadata = await compressAsset('fast-share-test', new TextEncoder().encode(JSON.stringify({
    payload: { byteOrder: 'little-endian', chunk: column.descriptor }, sourceChunks: [history.descriptor],
  })));
  await writeFile(resolve(root, metadata.descriptor.path), metadata.compressed);
  await writeFile(resolve(root, 'stale-version.gz'), new Uint8Array([0]));
  await expect(compressedGraph(root, { catalog: metadata.descriptor })).rejects.toThrow();
  await writeFile(resolve(root, column.descriptor.path), column.compressed);
  await expect(compressedGraph(root, { catalog: metadata.descriptor })).rejects.toThrow();
  await writeFile(resolve(root, history.descriptor.path), history.compressed);
  expect([...(await compressedGraph(root, { catalog: metadata.descriptor })).keys()].sort())
    .toEqual([column.descriptor.path, history.descriptor.path, metadata.descriptor.path].sort());
  await writeFile(resolve(root, column.descriptor.path), new Uint8Array(column.compressed.length));
  await expect(compressedGraph(root, { catalog: metadata.descriptor })).rejects.toThrow('SOURCE_ASSET_INTEGRITY');
});

test('publish graph refuses parent and absolute paths', () => {
  for (const path of ['../outside', 'data/../../outside', '/outside', 'C:\\outside', 'data\\outside'])
    expect(() => inside(resolve('site'), path)).toThrow();
});
