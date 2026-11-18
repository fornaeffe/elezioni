import { describe, it, expect, afterEach } from 'vitest';
import { mkdtemp, readFile, writeFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { resolve, sep } from 'node:path';
import { encodeState } from '../../src/lib/politics/reform-2026/vote-share/state-codec';
import {
  immutableBytes,
  immutableJson,
  writeAssets,
  readCachedState,
  selectCandidate,
} from './fast-share-release-io';
const directories: string[] = [];
async function temporary() {
  const p = await mkdtemp(resolve(tmpdir(), 'elezioni-fv05-'));
  directories.push(p);
  return p;
}
afterEach(async () => {
  for (const directory of directories.splice(0)) {
    if (!resolve(directory).startsWith(resolve(tmpdir()) + sep))
      throw new Error('Unsafe test cleanup target');
    await rm(directory, { recursive: true, force: true });
  }
});
describe('FV-05 immutable caches and atomic selection', () => {
  it('distinguishes a cache miss from missing/corrupt content and identity mismatches', async () => {
    const dir = await temporary();
    expect(await readCachedState(dir, 'absent', 'fast-share-fit')).toBeNull();
    const a = await encodeState(
      'fast-share-fit',
      { values: Float64Array.of(1, 2) },
      { sourceVersions: [], methodVersions: [], assumptions: [] },
    );
    await writeAssets(dir, a.assets);
    await immutableJson(resolve(dir, 'cache.json'), {
      id: a.envelope.id,
      descriptor: a.descriptor,
    });
    const hit = await readCachedState<{ values: Float64Array }>(
      dir,
      'cache',
      'fast-share-fit',
    );
    expect([...hit!.state.values]).toEqual([1, 2]);
    const chunk = a.envelope.payload.chunk!;
    await writeFile(resolve(dir, chunk.path), new Uint8Array(20));
    await expect(
      readCachedState(dir, 'cache', 'fast-share-fit'),
    ).rejects.toThrow('INTEGRITY');
    await writeFile(resolve(dir, chunk.path), a.assets.get(chunk.path)!);
    await writeFile(
      resolve(dir, 'cache.json'),
      JSON.stringify({ id: 'different', descriptor: a.descriptor }),
    );
    await expect(
      readCachedState(dir, 'cache', 'fast-share-fit'),
    ).rejects.toThrow('identity mismatch');
  });
  it('never overwrites immutable content with different bytes', async () => {
    const dir = await temporary(),
      path = resolve(dir, 'fixed.bin');
    await immutableBytes(path, Uint8Array.of(1));
    await immutableBytes(path, Uint8Array.of(1));
    await expect(immutableBytes(path, Uint8Array.of(2))).rejects.toThrow(
      'Immutable',
    );
    expect([...(await readFile(path))]).toEqual([1]);
  });
  it('replaces a complete pointer and preserves the old selection after a failed write', async () => {
    const dir = await temporary(),
      path = resolve(dir, 'candidate.json');
    await selectCandidate(path, { catalogId: 'first' });
    await selectCandidate(path, { catalogId: 'second' });
    const before = await readFile(path, 'utf8');
    expect(JSON.parse(before).catalogId).toBe('second');
    await expect(selectCandidate(path, { catalogId: NaN })).rejects.toThrow();
    expect(await readFile(path, 'utf8')).toBe(before);
  });
});
