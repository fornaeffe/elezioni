import { mkdir, readFile, writeFile, rename } from 'node:fs/promises';
import { dirname, resolve, relative } from 'node:path';
import {
  sourceKey,
  decodeAsset,
  canonicalJson,
  type ByteAsset,
} from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import {
  decodeState,
  type StateKind,
} from '../../src/lib/politics/reform-2026/vote-share/state-codec';
export async function immutableBytes(path: string, bytes: Uint8Array) {
  try {
    if (!Buffer.from(bytes).equals(await readFile(path)))
      throw new Error('Immutable release artifact differs: ' + path);
    return;
  } catch (e) {
    if ((e as NodeJS.ErrnoException).code !== 'ENOENT') throw e;
  }
  await mkdir(dirname(path), { recursive: true });
  await writeFile(path, bytes, { flag: 'wx' });
}
export const immutableJson = (path: string, value: unknown) =>
  immutableBytes(path, new TextEncoder().encode(canonicalJson(value) + '\n'));
export const readBytes = (directory: string) => (path: string) => {
  if (!/^[\w.-]+$/.test(path)) throw new Error('Unsafe release asset path');
  return readFile(resolve(directory, path));
};
export async function writeAssets(
  directory: string,
  assets: Map<string, Uint8Array>,
) {
  for (const [path, bytes] of assets)
    await immutableBytes(resolve(directory, path), bytes);
}
export async function readCachedState<T>(
  directory: string,
  key: string,
  kind: StateKind,
) {
  let pointer: { id: string; descriptor: ByteAsset };
  try {
    pointer = JSON.parse(
      await readFile(resolve(directory, key + '.json'), 'utf8'),
    );
  } catch (e) {
    if ((e as NodeJS.ErrnoException).code === 'ENOENT') return null;
    throw e;
  }
  const decoded = await decodeState<T>(
    pointer.descriptor,
    readBytes(directory),
    kind,
  );
  if (decoded.envelope.id !== pointer.id)
    throw new Error('Cached state identity mismatch');
  return { ...decoded, descriptor: pointer.descriptor };
}
/** Pointer replacement is the final operation, after the complete immutable set
 * has been verified. Existing accepted candidates survive failed preparation. */
export async function selectCandidate(path: string, pointer: unknown) {
  const temporary = path + '.' + process.pid + '.tmp';
  await mkdir(dirname(path), { recursive: true });
  await writeFile(temporary, canonicalJson(pointer) + '\n', { flag: 'wx' });
  await rename(temporary, path);
}
/** Transitive local code/JSON plus the lockfile define implementation identity.
 * Runtime filesystem manifests/reviewed JSON are explicit parents elsewhere. */
export async function implementationKey(root: string, entry: string) {
  const files = new Map<string, string>(),
    visit = async (path: string): Promise<void> => {
      const name = relative(root, path).replaceAll('\\', '/');
      if (name.startsWith('../'))
        throw new Error('Implementation dependency escaped repository');
      if (files.has(name)) return;
      const body = (await readFile(path, 'utf8')).replaceAll('\r\n', '\n');
      files.set(name, body);
      if (!path.endsWith('.ts')) return;
      for (const m of body.matchAll(
        /(?:from\s*|import\s*\()(['"])((?:\.|\$lib\/)[^'"]+)\1/g,
      )) {
        const parent = m[2].startsWith('$lib/')
          ? resolve(root, 'web/src/lib', m[2].slice(5))
          : resolve(dirname(path), m[2]);
        let target: string | undefined;
        for (const candidate of /\.(ts|json)$/.test(parent)
          ? [parent]
          : [parent + '.ts', resolve(parent, 'index.ts')])
          try {
            await readFile(candidate);
            target = candidate;
            break;
          } catch (e) {
            if ((e as NodeJS.ErrnoException).code !== 'ENOENT') throw e;
          }
        if (!target)
          throw new Error('Missing implementation dependency ' + m[2]);
        await visit(target);
      }
    };
  await visit(resolve(root, entry));
  files.set(
    'web/package-lock.json',
    (await readFile(resolve(root, 'web/package-lock.json'), 'utf8')).replaceAll(
      '\r\n',
      '\n',
    ),
  );
  files.set(
    'web/tsconfig.node.json',
    (
      await readFile(resolve(root, 'web/tsconfig.node.json'), 'utf8')
    ).replaceAll('\r\n', '\n'),
  );
  return sourceKey(
    'implementation-v1',
    Object.fromEntries([...files].sort(([a], [b]) => a.localeCompare(b))),
  );
}
export async function verifyAsset(descriptor: ByteAsset, directory: string) {
  return decodeAsset(descriptor, await readBytes(directory)(descriptor.path));
}
