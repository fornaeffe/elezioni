/** URLs are supplied by SvelteKit at the worker boundary; also usable by future
 * municipal workflows without importing framework globals into data helpers. */
export function runtimeDataUrl(base: string, path: string): string {
  if (path.includes('..') || /^[a-z]+:/i.test(path)) throw new Error('Expected a local runtime data path');
  return `${base.replace(/\/$/, '')}/${path.replace(/^\//, '')}`;
}

export interface CompressedJsonDescriptor {
  path: string;
  bytes: number;
  sha256: string;
  uncompressedSha256: string;
}

async function checksum(bytes: Uint8Array): Promise<string> {
  const digest = await crypto.subtle.digest('SHA-256', new Uint8Array(bytes).buffer);
  return [...new Uint8Array(digest)].map(n => n.toString(16).padStart(2, '0')).join('');
}

export async function decodeVerifiedJson(bytes: Uint8Array, descriptor: CompressedJsonDescriptor, transportDecoded = false): Promise<unknown> {
  // Fetch transparently removes HTTP Content-Encoding (Vite preview does this
  // for .json.gz). Those bytes must match the pinned decoded digest directly;
  // the compressed wire bytes are unavailable to JavaScript in that case.
  if (transportDecoded) {
    if (await checksum(bytes) !== descriptor.uncompressedSha256) throw new Error('Transport-decoded runtime checksum mismatch');
    return JSON.parse(new TextDecoder().decode(bytes));
  }
  if (bytes.length !== descriptor.bytes || await checksum(bytes) !== descriptor.sha256) throw new Error('Runtime bundle checksum mismatch');
  const stream = new Blob([new Uint8Array(bytes).buffer]).stream().pipeThrough(new DecompressionStream('gzip'));
  const decoded = new Uint8Array(await new Response(stream).arrayBuffer());
  if (await checksum(decoded) !== descriptor.uncompressedSha256) throw new Error('Decompressed runtime checksum mismatch');
  return JSON.parse(new TextDecoder().decode(decoded));
}

export async function loadVerifiedJson(base: string, descriptor: CompressedJsonDescriptor): Promise<unknown> {
  const url = runtimeDataUrl(base, descriptor.path), response = await fetch(url);
  if (!response.ok) throw new Error(`Missing production bundle ${url}. Generate the required runtime data first.`);
  return decodeVerifiedJson(new Uint8Array(await response.arrayBuffer()), descriptor, /\bgzip\b/i.test(response.headers.get('content-encoding') ?? ''));
}

export function requireSimulationCount(value: number): number {
  if (!Number.isSafeInteger(value) || value < 1) throw new RangeError('Simulation count must be a positive safe integer');
  return value;
}
