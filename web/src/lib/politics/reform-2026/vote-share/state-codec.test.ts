import { describe, it, expect } from 'vitest';
import { encodeState, decodeState, type StateEnvelope } from './state-codec';
import {
  compressAsset,
  canonicalJson,
  sourceKey,
  decodeAsset,
} from './source-codec';
const metadata = {
  sourceVersions: ['s2', 's1'],
  methodVersions: ['lossless-test-v1'],
  assumptions: ['Constructed portable-state test'],
};
describe('FV-05 lossless portable fitted/profile state', () => {
  it('preserves Float64 bits, integer ranges, shapes and owned arrays', async () => {
    const values = Float64Array.from([
      0,
      -0,
      Number.MIN_VALUE,
      1.2345678901234567,
      -1e200,
      Number.MAX_VALUE,
    ]);
    const encoded = await encodeState(
      'fast-share-fit',
      {
        values,
        flags: Uint8Array.from([0, 1, 2]),
        counts: Uint32Array.from([0, 4294967295]),
      },
      metadata,
      { '/values': [2, 3] },
    );
    const result = await decodeState<{
      values: Float64Array;
      flags: Uint8Array;
      counts: Uint32Array;
    }>(
      encoded.descriptor,
      async (p) => encoded.assets.get(p)!,
      'fast-share-fit',
    );
    expect(new Uint8Array(result.state.values.buffer)).toEqual(
      new Uint8Array(values.buffer),
    );
    expect(result.state.flags).toBeInstanceOf(Uint8Array);
    expect([...result.state.counts]).toEqual([0, 4294967295]);
    expect(
      result.envelope.payload.columns.find((c) => c.name === '/values')!.shape,
    ).toEqual([2, 3]);
    result.state.values[0] = 99;
    expect(values[0]).toBe(0);
  });
  it('has a specified little-endian wire representation and deterministic identities', async () => {
    const a = await encodeState(
        'fast-share-fit',
        { x: Float64Array.of(1) },
        metadata,
      ),
      b = await encodeState(
        'fast-share-fit',
        { x: Float64Array.of(1) },
        { ...metadata, sourceVersions: ['s1', 's2'] },
      );
    expect(a.envelope.id).toBe(b.envelope.id);
    expect(a.descriptor).toEqual(b.descriptor);
    const chunk = a.envelope.payload.chunk!;
    expect([...(await decodeAsset(chunk, a.assets.get(chunk.path)!))]).toEqual([
      0, 0, 0, 0, 0, 0, 240, 63,
    ]);
    const changed = await encodeState(
      'fast-share-fit',
      { x: Float64Array.of(1 + Number.EPSILON) },
      metadata,
    );
    expect(changed.envelope.id).not.toBe(a.envelope.id);
  });
  it('supports empty numeric arrays without inventing a nonempty chunk', async () => {
    const a = await encodeState(
      'fast-share-profiles',
      { empty: new Uint8Array(), other: new Float64Array() },
      metadata,
    );
    expect(a.envelope.payload.chunk).toBeNull();
    const b = await decodeState<{ empty: Uint8Array; other: Float64Array }>(
      a.descriptor,
      async (p) => a.assets.get(p)!,
      'fast-share-profiles',
    );
    expect(b.state.empty.length).toBe(0);
    expect(b.state.other).toBeInstanceOf(Float64Array);
  });
  it('verifies transparent HTTP decoding without relaxing disk/cache integrity', async () => {
    const encoded = await encodeState(
      'fast-share-fit',
      { x: Float64Array.of(1, -0, 2) },
      metadata,
    );
    const read = async (path: string) => {
      const descriptor =
        path === encoded.descriptor.path
          ? encoded.descriptor
          : encoded.envelope.payload.chunk!;
      return {
        transport: 'http' as const,
        body: await decodeAsset(descriptor, encoded.assets.get(path)!),
      };
    };
    const decoded = await decodeState<{ x: Float64Array }>(
      encoded.descriptor,
      read,
      'fast-share-fit',
    );
    expect(new Uint8Array(decoded.state.x.buffer)).toEqual(
      new Uint8Array(Float64Array.of(1, -0, 2).buffer),
    );
    const body = (await read(encoded.descriptor.path)).body;
    await expect(decodeAsset(encoded.descriptor, body)).rejects.toThrow(
      'INTEGRITY',
    );
    body[0] ^= 1;
    await expect(
      decodeAsset(encoded.descriptor, { transport: 'http', body }),
    ).rejects.toThrow('INTEGRITY');
    expect(
      await decodeAsset(encoded.descriptor, {
        transport: 'http',
        body: encoded.assets.get(encoded.descriptor.path)!,
      }),
    ).toBeInstanceOf(Uint8Array);
  });
  it('fails on missing/corrupted assets and unsupported envelope versions', async () => {
    const a = await encodeState(
        'fast-share-fit',
        { values: Float64Array.of(1, 2) },
        metadata,
      ),
      path = a.envelope.payload.chunk!.path,
      corrupt = a.assets.get(path)!.slice();
    corrupt[15] ^= 1;
    await expect(
      decodeState(
        a.descriptor,
        async (p) => (p === path ? corrupt : a.assets.get(p)!),
        'fast-share-fit',
      ),
    ).rejects.toThrow('INTEGRITY');
    await expect(
      decodeState(
        a.descriptor,
        async (p) => {
          if (p === path) throw new Error('missing required chunk');
          return a.assets.get(p)!;
        },
        'fast-share-fit',
      ),
    ).rejects.toThrow('missing required');
    await expect(
      decodeState(
        a.descriptor,
        async (p) => a.assets.get(p)!,
        'fast-share-profiles',
      ),
    ).rejects.toThrow('STATE_ENVELOPE');
  });
  it('validates independently rehashed column layouts and does not silently coerce unsupported values', async () => {
    const a = await encodeState(
        'fast-share-fit',
        { x: Float64Array.of(1, 2) },
        metadata,
      ),
      bad: StateEnvelope = structuredClone(a.envelope);
    bad.payload.columns[0].shape = [3];
    const { id: _, ...body } = bad;
    bad.id = await sourceKey('fast-share-fit-v1', body);
    const wire = await compressAsset(
      'bad-state',
      new TextEncoder().encode(canonicalJson(bad)),
    );
    await expect(
      decodeState(
        wire.descriptor,
        async (p) =>
          p === wire.descriptor.path ? wire.compressed : a.assets.get(p)!,
        'fast-share-fit',
      ),
    ).rejects.toThrow('COLUMN_LAYOUT');
    await expect(
      encodeState('fast-share-fit', { x: Float64Array.of(NaN) }, metadata),
    ).rejects.toThrow('NONFINITE');
    await expect(
      encodeState('fast-share-fit', { x: new Float32Array(2) }, metadata),
    ).rejects.toThrow('DTYPE');
    await expect(
      encodeState('fast-share-fit', { $array: 0 }, metadata),
    ).rejects.toThrow('RESERVED');
  });
});
