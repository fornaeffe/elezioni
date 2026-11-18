import { createHash } from 'node:crypto';
import { mkdir, readFile, rename, rm, writeFile } from 'node:fs/promises';
import { dirname, isAbsolute, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { downloadSource } from './source-download';

export interface Source {
  /** Reviewed repository bytes when a live historical endpoint is mutable. */
  repositoryPath?: string;
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

/** A local acquisition mirror does not change original bytes or their provenance.
 * Retain all other manifest fields, including URL, path, size and checksum. */
export function sourceManifestContent(manifest: { sources: Source[] }) {
  return { ...manifest, sources: manifest.sources.map(({ repositoryPath: _repositoryPath, ...source }) => source) };
}

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
  const repositoryFile = source.repositoryPath
    ? sourcePath(fileURLToPath(new URL('../../../', import.meta.url)), { ...source, path: source.repositoryPath })
    : undefined;
  if (localFile || repositoryFile) bytes = await readFile(localFile ?? repositoryFile!);
  else bytes = await downloadSource(source);
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
