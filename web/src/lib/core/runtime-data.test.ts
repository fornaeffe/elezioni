import { gzipSync } from 'fflate';
import { expect, it } from 'vitest';
import { decodeVerifiedJson, requireSimulationCount, runtimeDataUrl } from './runtime-data';

it('preserves project-base paths and accepts counts beyond the former arbitrary cap', () => {
  expect(runtimeDataUrl('/elezioni', '/data/v1/politics-static.json')).toBe('/elezioni/data/v1/politics-static.json');
  expect(runtimeDataUrl('', 'data/reform.json.gz')).toBe('/data/reform.json.gz');
  expect(requireSimulationCount(1001)).toBe(1001);
  for (const count of [NaN, Infinity, 0, -1, 1.5, Number.MAX_SAFE_INTEGER + 1]) expect(() => requireSimulationCount(count)).toThrow();
});

it('checks both compressed and decoded checksums rather than accepting a different production snapshot', async () => {
  const raw = new TextEncoder().encode('{"schemaVersion":1}'), bytes = gzipSync(raw);
  const hash = async (b: Uint8Array) => [...new Uint8Array(await crypto.subtle.digest('SHA-256', new Uint8Array(b).buffer))].map(n => n.toString(16).padStart(2, '0')).join('');
  const descriptor = { path: 'data/test.gz', bytes: bytes.length, sha256: await hash(bytes), uncompressedSha256: await hash(raw) };
  await expect(decodeVerifiedJson(bytes, descriptor)).resolves.toEqual({ schemaVersion: 1 });
  await expect(decodeVerifiedJson(raw, descriptor, true)).resolves.toEqual({ schemaVersion: 1 });
  await expect(decodeVerifiedJson(raw, descriptor)).rejects.toThrow(/checksum/);
  await expect(decodeVerifiedJson(raw, { ...descriptor, uncompressedSha256: 'wrong' }, true)).rejects.toThrow(/checksum/);
  await expect(decodeVerifiedJson(bytes, { ...descriptor, sha256: 'wrong' })).rejects.toThrow(/checksum/);
  await expect(decodeVerifiedJson(bytes, { ...descriptor, uncompressedSha256: 'wrong' })).rejects.toThrow(/Decompressed/);
});
