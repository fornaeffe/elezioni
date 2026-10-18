import { createHash } from 'node:crypto';
import { mkdir, readFile, rename, rm, writeFile } from 'node:fs/promises';
import { dirname, isAbsolute, relative, resolve } from 'node:path';

export interface Source {
  id: string;
  url: string;
  path: string;
  sha256: string;
  bytes: number;
  headers: Record<string, string>;
}
export interface SourceManifest {
  schemaVersion: 1;
  id: string;
  reviewedOn: string;
  censusDate: string;
  legalReference: string;
  sources: Source[];
}
export const sha256 = (bytes: Uint8Array | string): string => createHash('sha256').update(bytes).digest('hex');

export function sourcePath(directory: string, source: Source): string {
  const path = resolve(directory, source.path);
  const inside = relative(resolve(directory), path);
  if (!inside || inside.startsWith('..') || isAbsolute(inside)) throw new Error(`Unsafe source path: ${source.path}`);
  return path;
}

export function verifySource(bytes: Uint8Array, source: Source): void {
  if (bytes.length !== source.bytes || sha256(bytes) !== source.sha256) {
    throw new Error(`Source ${source.id} differs from its pinned version; review a new manifest instead of accepting it automatically.`);
  }
}

export async function readSource(directory: string, source: Source): Promise<Buffer> {
  const bytes = await readFile(sourcePath(directory, source));
  verifySource(bytes, source);
  return bytes;
}

/** Download only missing files. Existing corrupt caches are errors, not silent refreshes. */
export async function acquireSource(directory: string, source: Source, localFile?: string): Promise<void> {
  const path = sourcePath(directory, source);
  try {
    await readSource(directory, source);
    return;
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== 'ENOENT') throw error;
  }
  let bytes: Buffer;
  if (localFile) bytes = await readFile(localFile);
  else {
    const response = await fetch(source.url, { headers: source.headers, signal: AbortSignal.timeout(180_000) });
    if (!response.ok) throw new Error(`${source.id}: HTTP ${response.status}`);
    bytes = Buffer.from(await response.arrayBuffer());
  }
  verifySource(bytes, source);
  await mkdir(dirname(path), { recursive: true });
  const temporary = `${path}.${process.pid}.tmp`;
  try {
    await writeFile(temporary, bytes, { flag: 'wx' });
    await rename(temporary, path);
  } finally {
    await rm(temporary, { force: true });
  }
}

/** Immutable versions: existing content must match; new content needs a new filename. */
export async function writeVersion(path: string, value: unknown): Promise<void> {
  const text = `${JSON.stringify(value, null, 2)}\n`;
  try {
    if (await readFile(path, 'utf8') !== text) throw new Error(`Refusing to replace immutable version ${path}`);
    return;
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== 'ENOENT') throw error;
  }
  await mkdir(dirname(path), { recursive: true });
  const temporary = `${path}.${process.pid}.tmp`;
  try {
    await writeFile(temporary, text, { flag: 'wx' });
    await rename(temporary, path);
  } finally {
    await rm(temporary, { force: true });
  }
}
