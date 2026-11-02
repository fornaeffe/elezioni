// Optional archive verification. Node 24 only; no npm, downloads or production fit.
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { createReadStream, existsSync } from 'node:fs';
import { readFile } from 'node:fs/promises';
import { gunzipSync } from 'node:zlib';
import { fileURLToPath } from 'node:url';
import { resolve, sep } from 'node:path';
const root = fileURLToPath(new URL('../../../', import.meta.url));
const local = p => {
  const result = resolve(root, p);
  assert(result.startsWith(resolve(root) + sep), 'Path escapes repository');
  return result;
};
const digest = b => createHash('sha256').update(b).digest('hex');
const hash = async p => {
  const h = createHash('sha256');
  for await (const b of createReadStream(local(p))) h.update(b);
  return h.digest('hex');
};
const manifest = JSON.parse(await readFile(new URL('./manifest.json', import.meta.url)));
let verified = 0;
for (const e of manifest.entries) {
  if (e.action === 'archive' || e.action === 'extract') {
    if (e.action === 'archive') assert(!existsSync(local(e.path)), 'Original was not moved: ' + e.path);
    assert.equal(await hash(e.destination), e.archivedSha256 ?? e.sha256, e.destination);
    verified++;
  } else if (e.action === 'preserve') {
    const b = gunzipSync(await readFile(local(e.destination)));
    assert.equal(b.length, e.bytes, e.path);
    assert.equal(digest(b), e.sha256, e.path);
    verified++;
  } else if (e.action === 'delete' && manifest.status === 'executed') {
    assert(!existsSync(local(e.path)), 'Listed deletion still exists: ' + e.path);
  }
}
assert.equal(await hash(manifest.retainedInventory.path), manifest.retainedInventory.sha256);
for (const e of manifest.frozenContracts ?? []) assert.equal(await hash(e.path), e.sha256, e.path);
// Independent reduced Gaussian fixture: precision [[4,1],[1,3]], rhs [1,2].
// Exact inverse is [[3,-1],[-1,4]] / 11; do not derive the target with the solver.
const { chol, solve, inverse, basis, mm, tr } = await import('../code/web/scripts/data-preparation/drift-math.ts');
const l = chol(Float64Array.from([4, 1, 1, 3]), 2);
const x = solve(l, Float64Array.from([1, 2]), 2);
const cov = inverse(l, 2);
const close = (a,b) => assert(Math.abs(a-b) < 1e-12, `${a} differs from ${b}`);
x.forEach((v,i) => close(v, [1/11,7/11][i]));
cov.forEach((v,i) => close(v, [3/11,-1/11,-1/11,4/11][i]));
const h = basis(3), gram = mm(tr(h,3,2),h,2,3,2);
gram.forEach((v,i) => close(v, [1,0,0,1][i]));
console.log(JSON.stringify({ archiveFilesVerified: verified, independentReducedGaussian: 'passed', helmertBasis: 'passed', heavyDownloads: false }));
