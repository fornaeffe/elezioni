import { byteHash } from '../politics/reform-2026/vote-share/source-codec';

/** Bind a structured-clone graph without expanding numeric buffers into JSON
 * numbers. Map order and object aliases survive transfer; object key order need
 * not. Only metadata is stringified, and each numeric column is hashed once. */
export async function structuredHash(value: unknown): Promise<string> {
  const seen = new Map<object, number>();
  async function encode(x: unknown): Promise<unknown> {
    if (x === undefined) return ['undefined'];
    if (x === null || typeof x === 'string' || typeof x === 'boolean') return x;
    if (typeof x === 'number' && Number.isFinite(x)) return x;
    if (typeof x !== 'object' || !x) throw new Error('Unsupported structured hash value');
    const previous = seen.get(x);
    if (previous !== undefined) return ['ref', previous];
    const id = seen.size; seen.set(x, id);
    if (ArrayBuffer.isView(x)) {
      if (!(x.buffer instanceof ArrayBuffer)) throw new Error('Shared numeric buffers are unsupported');
      return [id, x.constructor.name, x.byteLength, await byteHash(new Uint8Array(x.buffer, x.byteOffset, x.byteLength))];
    }
    if (x instanceof ArrayBuffer) return [id, 'ArrayBuffer', x.byteLength, await byteHash(new Uint8Array(x))];
    if (Array.isArray(x)) {
      const values = [];
      for (const entry of x) values.push(await encode(entry));
      return [id, 'array', values];
    }
    if (x instanceof Map) {
      const entries = [];
      for (const [key, entry] of x) entries.push([await encode(key), await encode(entry)]);
      return [id, 'map', entries];
    }
    if (![Object.prototype, null].includes(Object.getPrototypeOf(x))) throw new Error('Unsupported structured hash object');
    const entries = [];
    for (const key of Object.keys(x).sort()) entries.push([key, await encode((x as Record<string, unknown>)[key])]);
    return [id, 'object', entries];
  }
  return byteHash(new TextEncoder().encode(JSON.stringify(await encode(value))));
}
