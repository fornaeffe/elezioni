/** Full-component Camera marginal parity with the pre-adoption M02 law.
 * The old source is extracted into ignored evidence storage, never bundled. */
import { mkdir, readFile, writeFile, realpath, symlink } from 'node:fs/promises';
import { execFileSync } from 'node:child_process';
import { resolve, relative, dirname, isAbsolute } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { unzipSync } from 'fflate';
import { createHash } from 'node:crypto';
import { readAuthoredPolitics } from '../workspaces/politics-default';
import { preparePoliticsExecution } from '../../src/lib/politics/execution/prepare';
import { prepareM02Votes } from '../../src/lib/politics/generation/m02';
import { implementationKey } from '../data-preparation/fast-share-release-io';
const root = fileURLToPath(new URL('../../../', import.meta.url));
const output = resolve(root, 'data/derived/model-acceptance-2026-11'); await mkdir(output, { recursive: true });
const revision = execFileSync('git', ['rev-parse', 'HEAD'], { cwd: root, encoding: 'utf8' }).trim();
const oldRoot = resolve(output, 'pre-adoption');
for (const [name, bytes] of Object.entries(unzipSync(execFileSync('git', ['archive', '--format=zip', revision, 'web/src/lib'], { cwd: root, maxBuffer: 64 * 1024 ** 2 })))) {
  if (name.endsWith('/')) continue;
  const path = resolve(oldRoot, name), within = relative(oldRoot, path);
  if (within.startsWith('..') || isAbsolute(within)) throw new Error('OLD_SOURCE_OUTSIDE_EVIDENCE');
  await mkdir(dirname(path), { recursive: true }); await writeFile(path, bytes);
}
const modules = resolve(root, 'web/node_modules'), link = resolve(oldRoot, 'web/node_modules');
try { if (await realpath(link) !== await realpath(modules)) throw new Error('UNEXPECTED_MODULE_LINK'); }
catch (e) { if ((e as NodeJS.ErrnoException).code !== 'ENOENT') throw e; await symlink(modules, link, process.platform === 'win32' ? 'junction' : 'dir'); }
const { prepareM02Votes: oldPrepare } = await import(pathToFileURL(resolve(oldRoot, 'web/src/lib/politics/generation/m02.ts')).href);
const scenario = await readAuthoredPolitics(root), implementationId = await implementationKey(root, 'web/src/lib/workers/politics-compute.ts');
const prepared = await preparePoliticsExecution(scenario, { implementationId,
  asset: async p => new Uint8Array(await readFile(resolve(root, 'web/static', p.uri))),
  sourceChunk: async path => new Uint8Array(await readFile(resolve(root, 'web/static/data/reform-2026/fast-share', path))) });
const g = prepared.computeInputs.generator;
if (g.id !== 'M02') throw new Error('PARITY_REQUIRES_M02_DEFAULT');
const s = structuredClone(g.settings); s.references = []; s.nonValidReferences = []; s.components = s.components.filter(c => c.kind === 'fitted');
for (const ch of ['camera', 'senato'] as const) s.lists[ch] = s.components.map(c => ({ id: c.id, name: c.id, coalitionId: null, protectedMinority: null, componentIds: [c.id] }));
for (const c of s.coverage) c.listIds = s.components.map(c => c.id);
const old = { ...structuredClone(s), generation: { ...s.generation, modelVersion: '2.0.0' } };
const current = prepareM02Votes(s, g.assets, g.context), previous = oldPrepare(old, g.assets, g.context);
const hash = (p: ReturnType<typeof current.run>) => createHash('sha256').update(JSON.stringify([...p.counts]
  .filter(([id]) => g.context.geography.colleges.some(c => c.id === id && c.chamber === 'camera'))
  .map(([id, row]) => [id, row.electors, [...row.votes]]))).digest('hex');
const draws = [1, 2, 7].map(id => {
  const current = hash(prepareRun(id)), baseline = hash(previous.run(id));
  if (current !== baseline) throw new Error('CAMERA_FULL_PANEL_PARITY_FAILED:' + id);
  return { id, sha256: current };
});
function prepareRun(id: number) { return current.run(id); }
const report = { status: 'passed', revision, implementationId, components: s.components.length,
  scope: 'Unrestricted full-component Camera law and seeded integer counts, no explicit interventions. Different filings and explicit references are separately tested; no claim of unchanged Senate law.', draws };
await writeFile(resolve(output, 'camera-parity.json'), JSON.stringify(report, null, 2) + '\n'); console.log(JSON.stringify(report));
