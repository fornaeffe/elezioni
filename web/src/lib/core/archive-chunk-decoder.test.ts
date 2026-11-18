import { afterEach, expect, it, vi } from 'vitest';
import { gzipSync } from 'fflate';
import { ArchiveChunkDecoder } from './archive-chunk-decoder';

afterEach(() => { vi.unstubAllGlobals(); vi.useRealTimers(); });
it('keeps small archives serial and retries original bytes after worker/transfer failures', async () => {
  const raw = new TextEncoder().encode('[[0,false,null,"Città 😀"]]'), bytes = gzipSync(raw), original = bytes.slice();
  let creations = 0, stopped = 0;
  vi.stubGlobal('navigator', { hardwareConcurrency: 8 });
  vi.stubGlobal('Worker', class {
    constructor() { creations++; }
    postMessage() { throw new Error('Injected transfer failure'); }
    terminate() { stopped++; }
  });
  const small = new ArchiveChunkDecoder(1000);
  expect(await small.decode([bytes])).toEqual([raw]); expect(creations).toBe(0); small.close();
  const large = new ArchiveChunkDecoder(4 * 1024 * 1024);
  expect(await large.decode([bytes, bytes])).toEqual([raw, raw]);
  expect(bytes).toEqual(original); expect(creations).toBe(2); expect(stopped).toBe(2);
  expect(await large.decode([bytes])).toEqual([raw]); expect(creations).toBe(2); large.close();
});
it('preserves chunk order despite reversed worker replies and terminates on close', async () => {
  vi.stubGlobal('navigator', { hardwareConcurrency: 8 });
  let stopped = 0;
  vi.stubGlobal('Worker', class {
    onmessage?: (event: { data: { id: number; bytes: Uint8Array } }) => void;
    postMessage({ id, bytes }: { id: number; bytes: Uint8Array }) {
      setTimeout(() => this.onmessage?.({ data: { id, bytes } }), id % 2 ? 10 : 0);
    }
    terminate() { stopped++; }
  });
  const chunks = [new Uint8Array([1]), new Uint8Array([2]), new Uint8Array([3])];
  const decoder = new ArchiveChunkDecoder(4 * 1024 * 1024);
  expect(await decoder.decode(chunks)).toEqual(chunks); decoder.close(); expect(stopped).toBe(2);
});
it('times out a stalled pool and completes from the retained compressed bytes', async () => {
  vi.useFakeTimers(); vi.stubGlobal('navigator', { hardwareConcurrency: 8 });
  let stopped = 0;
  vi.stubGlobal('Worker', class { postMessage() {} terminate() { stopped++; } });
  const raw = new TextEncoder().encode('[[0]]'), bytes = gzipSync(raw);
  const decoder = new ArchiveChunkDecoder(4 * 1024 * 1024), result = decoder.decode([bytes]);
  await vi.advanceTimersByTimeAsync(10000);
  expect(await result).toEqual([raw]); expect(stopped).toBe(2); decoder.close();
});
