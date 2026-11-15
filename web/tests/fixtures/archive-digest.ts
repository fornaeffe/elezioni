import { createReadStream } from 'node:fs';
import { createGunzip } from 'node:zlib';
import { createHash } from 'node:crypto';

/** Hash every decoded byte with bounded RAM, normalizing only export time. */
export async function archiveDigest(path: string) {
  const hash = createHash('sha256');
  let prefix = Buffer.alloc(0), header = false, bytes = 0;
  for await (const part of createReadStream(path).pipe(createGunzip())) {
    const chunk = Buffer.from(part); bytes += chunk.length;
    if (header) hash.update(chunk);
    else {
      prefix = Buffer.concat([prefix, chunk]);
      if (prefix.length < 256) continue;
      const text = prefix.toString('latin1');
      const normalized = text.replace(/^(\{"kind":"politics-result-archive","schemaVersion":1,"exportedAt":)"[^"]+"/, '$1"fixed"');
      if (normalized === text) throw new Error('Missing archive export timestamp');
      hash.update(Buffer.from(normalized, 'latin1')); header = true; prefix = Buffer.alloc(0);
    }
  }
  if (!header) throw new Error('Truncated result archive');
  return { bytes, sha256: hash.digest('hex') };
}
