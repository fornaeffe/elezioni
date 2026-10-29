/** Portable, lossless fitted/profile state. No filesystem or application imports. */
import {
  byteHash,
  canonicalJson,
  compressAsset,
  decodeAsset,
  sourceKey,
  type ByteAsset,
  type AssetBytes,
} from './source-codec';
import {
  FAST_SHARE_MODEL,
  FAST_SHARE_NUMERICS,
  requireWorkspace,
} from './types';
export type StateKind =
  | 'fast-share-fit'
  | 'fast-share-profiles'
  | 'fast-share-runtime'
  | 'fast-share-scenario'
  | 'fast-share-catalog';
type Json = null | boolean | number | string | Json[] | { [key: string]: Json };
export interface StateColumn {
  name: string;
  dtype: 'float64' | 'uint8' | 'uint32';
  shape: number[];
  offset: number;
  length: number;
}
export interface StateEnvelope {
  schemaVersion: 1;
  kind: StateKind;
  id: string;
  modelId: typeof FAST_SHARE_MODEL;
  numericalVersion: typeof FAST_SHARE_NUMERICS;
  sourceVersions: string[];
  methodVersions: string[];
  assumptions: string[];
  payload: {
    state: Json;
    byteOrder: 'little-endian';
    columns: StateColumn[];
    chunk: ByteAsset | null;
  };
}
export interface StateMetadata {
  sourceVersions: string[];
  methodVersions: string[];
  assumptions: string[];
}
const kinds: StateKind[] = [
  'fast-share-fit',
  'fast-share-profiles',
  'fast-share-runtime',
  'fast-share-scenario',
  'fast-share-catalog',
];
const width = (dtype: StateColumn['dtype']) =>
  dtype === 'float64' ? 8 : dtype === 'uint32' ? 4 : 1;
export async function encodeState(
  kind: StateKind,
  state: unknown,
  metadata: StateMetadata,
  shapes: Record<string, number[]> = {},
) {
  if (!kinds.includes(kind)) throw new Error('STATE_UNSUPPORTED_KIND');
  const columns: StateColumn[] = [],
    arrays: (Float64Array | Uint8Array | Uint32Array)[] = [];
  let size = 0;
  const visit = (value: unknown, path: string): Json => {
    if (ArrayBuffer.isView(value)) {
      if (!(
        value instanceof Float64Array ||
        value instanceof Uint8Array ||
        value instanceof Uint32Array
      ))
        throw new Error('STATE_UNSUPPORTED_DTYPE');
      if (value.some((x) => !Number.isFinite(x)))
        throw new Error('STATE_NONFINITE');
      const dtype =
          value instanceof Float64Array
            ? 'float64'
            : value instanceof Uint32Array
              ? 'uint32'
              : 'uint8',
        shape = shapes[path] ?? [value.length];
      if (
        !shape.length ||
        shape.some((x) => !Number.isSafeInteger(x) || x < 0) ||
        shape.reduce((a, b) => a * b, 1) !== value.length
      )
        throw new Error('STATE_SHAPE');
      const index = columns.length;
      columns.push({
        name: path,
        dtype,
        shape,
        offset: size,
        length: value.length,
      });
      arrays.push(value);
      size += value.length * width(dtype);
      return { $array: index };
    }
    if (
      value === null ||
      typeof value === 'string' ||
      typeof value === 'boolean'
    )
      return value;
    if (typeof value === 'number') {
      if (!Number.isFinite(value)) throw new Error('STATE_NONFINITE');
      return value;
    }
    if (Array.isArray(value))
      return value.map((v, i) => visit(v, path + '/' + i));
    if (value && typeof value === 'object') {
      if ('$array' in value) throw new Error('STATE_RESERVED_FIELD');
      return Object.fromEntries(
        Object.keys(value)
          .sort()
          .map((k) => [
            k,
            visit((value as Record<string, unknown>)[k], path + '/' + k),
          ]),
      );
    }
    throw new Error('STATE_UNSUPPORTED_VALUE:' + path);
  };
  const tree = visit(state, ''),
    assets = new Map<string, Uint8Array>();
  requireWorkspace(size);
  let chunk: ByteAsset | null = null;
  if (size) {
    const bytes = new Uint8Array(size),
      view = new DataView(bytes.buffer);
    columns.forEach((c, i) => {
      for (let j = 0; j < c.length; j++) {
        const at = c.offset + j * width(c.dtype);
        if (c.dtype === 'float64') view.setFloat64(at, arrays[i][j], true);
        else if (c.dtype === 'uint32') view.setUint32(at, arrays[i][j], true);
        else view.setUint8(at, arrays[i][j]);
      }
    });
    const encoded = await compressAsset(kind + '-columns', bytes);
    chunk = encoded.descriptor;
    assets.set(chunk.path, encoded.compressed);
  }
  const body = {
    schemaVersion: 1 as const,
    kind,
    modelId: FAST_SHARE_MODEL,
    numericalVersion: FAST_SHARE_NUMERICS,
    sourceVersions: [...new Set(metadata.sourceVersions)].sort(),
    methodVersions: [...new Set(metadata.methodVersions)].sort(),
    assumptions: metadata.assumptions,
    payload: {
      state: tree,
      byteOrder: 'little-endian' as const,
      columns,
      chunk,
    },
  } satisfies Omit<StateEnvelope, 'id'>;
  const envelope: StateEnvelope = {
      ...body,
      id: await sourceKey(kind + '-v1', body),
    },
    encoded = await compressAsset(
      kind,
      new TextEncoder().encode(canonicalJson(envelope)),
    );
  assets.set(encoded.descriptor.path, encoded.compressed);
  return { envelope, descriptor: encoded.descriptor, assets };
}
export async function decodeState<T>(
  descriptor: ByteAsset,
  read: (path: string) => Promise<AssetBytes>,
  expectedKind: StateKind,
) {
  const bytes = await decodeAsset(descriptor, await read(descriptor.path)),
    envelope: StateEnvelope = JSON.parse(
      new TextDecoder('utf-8', { fatal: true }).decode(bytes),
    );
  const { id, ...body } = envelope;
  if (
    envelope.schemaVersion !== 1 ||
    !kinds.includes(envelope.kind) ||
    envelope.kind !== expectedKind ||
    envelope.modelId !== FAST_SHARE_MODEL ||
    envelope.numericalVersion !== FAST_SHARE_NUMERICS ||
    id !== (await sourceKey(expectedKind + '-v1', body)) ||
    envelope.payload.byteOrder !== 'little-endian'
  )
    throw new Error('STATE_ENVELOPE');
  const p = envelope.payload,
    raw = p.chunk
      ? await decodeAsset(p.chunk, await read(p.chunk.path))
      : new Uint8Array(),
    view = new DataView(raw.buffer, raw.byteOffset, raw.length),
    arrays: (Float64Array | Uint8Array | Uint32Array)[] = [];
  let end = 0;
  const names = new Set<string>();
  for (const c of p.columns) {
    if (
      !['float64', 'uint8', 'uint32'].includes(c.dtype) ||
      names.has(c.name) ||
      c.offset !== end ||
      !Number.isSafeInteger(c.length) ||
      c.length < 0 ||
      !c.shape.length ||
      c.shape.some((x) => !Number.isSafeInteger(x) || x < 0) ||
      c.shape.reduce((a, b) => a * b, 1) !== c.length ||
      end + c.length * width(c.dtype) > raw.length
    )
      throw new Error('STATE_COLUMN_LAYOUT');
    const value =
      c.dtype === 'float64'
        ? new Float64Array(c.length)
        : c.dtype === 'uint32'
          ? new Uint32Array(c.length)
          : new Uint8Array(c.length);
    for (let i = 0; i < c.length; i++) {
      const at = c.offset + i * width(c.dtype);
      value[i] =
        c.dtype === 'float64'
          ? view.getFloat64(at, true)
          : c.dtype === 'uint32'
            ? view.getUint32(at, true)
            : view.getUint8(at);
    }
    if (value.some((x) => !Number.isFinite(x)))
      throw new Error('STATE_NONFINITE');
    arrays.push(value);
    names.add(c.name);
    end += value.byteLength;
  }
  if (
    end !== raw.length ||
    p.columns.some((c) => c.length > 0) !== Boolean(p.chunk)
  )
    throw new Error('STATE_TRAILING_BYTES');
  const used = new Set<number>();
  const restore = (value: Json, path: string): unknown => {
    if (
      value &&
      typeof value === 'object' &&
      !Array.isArray(value) &&
      '$array' in value
    ) {
      const i = value.$array;
      if (
        Object.keys(value).length !== 1 ||
        typeof i !== 'number' ||
        !Number.isSafeInteger(i) ||
        !arrays[i] ||
        used.has(i) ||
        p.columns[i].name !== path
      )
        throw new Error('STATE_COLUMN_BINDING');
      used.add(i);
      return arrays[i];
    }
    if (Array.isArray(value))
      return value.map((v, i) => restore(v, path + '/' + i));
    if (value && typeof value === 'object')
      return Object.fromEntries(
        Object.keys(value).map((k) => [k, restore(value[k], path + '/' + k)]),
      );
    return value;
  };
  const state = restore(p.state, '') as T;
  if (used.size !== arrays.length) throw new Error('STATE_UNUSED_COLUMN');
  return { state, envelope, metadataSha256: await byteHash(bytes) };
}
