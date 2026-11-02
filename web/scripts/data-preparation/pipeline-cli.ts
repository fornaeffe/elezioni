/** Complete production preparation; no R, workbooks, research exports or legacy caches. */
import { fileURLToPath } from 'node:url';
import { resolve } from 'node:path';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { execFileSync } from 'node:child_process';
import { acquireSource, readSource, sha256, type Source } from './sources';
import { workspaceSources } from './workspace-history';
import { writeWorkspaceSnapshots, replaceGenerated } from './workspace-snapshots';
import { releaseInputs } from './fast-share-release-inputs';
import { prepareShareRelease } from './fast-share-release';
import { verifyShareRelease } from './fast-share-release-check';
import { immutableBytes, readBytes, selectCandidate } from './fast-share-release-io';

const root = fileURLToPath(new URL('../../../', import.meta.url)), web = resolve(root, 'web');
const [command, ...args] = process.argv.slice(2);
const options = new Map<string, string>();
let download = false;
for (let i = 0; i < args.length; i++) {
  const key = args[i];
  if (key === '--download' && !download) { download = true; continue; }
  if (!['--sources', '--output', '--cache'].includes(key) || options.has(key) || !args[i + 1] || args[i + 1].startsWith('--')) throw new Error(`Invalid preparation option ${key}`);
  options.set(key, args[++i]);
}
if (!['prepare', 'check'].includes(command) || (command === 'check' && download)) throw new Error('Use data:prepare [--download] [--sources DIR] [--output DIR] [--cache DIR] | data:check');
if (process.versions.node.split('.')[0] !== '24') throw new Error('Production preparation requires Node 24');
const sources = resolve(options.get('--sources') ?? resolve(root, 'data/sources')),
  output = resolve(options.get('--output') ?? resolve(root, 'data/derived/production')),
  cache = resolve(options.get('--cache') ?? resolve(root, 'data/derived/fast-share-cache')),
  runtime = resolve(web, 'static/data/reform-2026/fast-share'),
  started = performance.now();
const npm = process.env.npm_execpath;
if (!npm) throw new Error('Run through npm run data:prepare or npm run data:check');
const run = (name: string, more: string[] = []) => execFileSync(process.execPath, [npm, 'run', name, ...(more.length ? ['--', ...more] : [])], { cwd: web, stdio: 'inherit' });
const script = (name: string) => execFileSync(process.execPath, [resolve(root, 'scripts', name), ...(command === 'check' ? ['--check'] : [])], { cwd: root, stdio: 'inherit' });
const metadataPath = resolve(web, 'static/data/v1/metadata.json');
const metadata = `${JSON.stringify({ version: 'node-production-v1', source: 'Pinned Node/TypeScript production preparation', workflows: ['politiche', 'regionali-er'],
  files: { politicsStatic: 'politics-static.json', regionalErStatic: 'regional-er-static.json' },
  notes: ['Prepared by npm run data:prepare; verified by npm run data:check.', 'Runtime checksums and schemas are pinned by the generated workspace descriptors.', 'Reform assets have separate versioned catalogs. Debug bridges and R caches are not production inputs.'] }, null, 2)}\n`;

if (command === 'prepare') {
  const entries = await workspaceSources(root), verified = new Map<string, Source>();
  for (const source of entries) {
    // Equal pinned originals shared by manifests need only one network request.
    const same = verified.get(source.sha256);
    if (download && same) {
      try { await readSource(sources, source); }
      catch (e) { if ((e as NodeJS.ErrnoException).code !== 'ENOENT') throw e; await immutableBytes(resolve(sources, source.path), await readSource(sources, same)); }
    } else if (download) await acquireSource(sources, source, source.repositoryPath ? resolve(root, source.repositoryPath) : undefined);
    await readSource(sources, source);
    verified.set(source.sha256, source);
    console.log(`Verified ${source.id}`);
  }
  // Release preparation requires fine Roma geography; the generic geography
  // generator's labelled missing-input fallback remains available separately.
  run('data:generate', ['--sources', sources, '--output', resolve(output, 'geography'), '--publish']);
  await writeWorkspaceSnapshots(root, sources, resolve(output, 'workspaces'), true);
  run('data:reform:generate', ['--sources', sources, '--output', resolve(output, 'reform'), '--publish']);
  const inputs = await releaseInputs(root, sources, cache), candidateOutput = resolve(output, 'shares');
  const candidate = await prepareShareRelease(root, candidateOutput, cache, inputs);
  await verifyShareRelease(candidate.pointer, candidateOutput);
  for (const file of candidate.files) await immutableBytes(resolve(runtime, file), await readBytes(candidateOutput)(file));
  await verifyShareRelease(candidate.pointer, runtime);
  await selectCandidate(resolve(runtime, 'candidate.json'), candidate.pointer);
  await selectCandidate(resolve(candidateOutput, 'candidate.json'), candidate.pointer);
} else {
  await writeWorkspaceSnapshots(root, sources, resolve(output, 'workspaces'), false, true);
}
if (command === 'prepare') await replaceGenerated(metadataPath, metadata);
else if (await readFile(metadataPath, 'utf8') !== metadata) throw new Error('Stale production metadata; run npm run data:prepare');
// Every compiler is checked, including the small catalogs and ER defaults.
for (const name of ['export_politics_municipality_catalog.mjs', 'export_politics_candidate_slots.mjs', 'export_regional_er_scenario_defaults.mjs', 'export_politics_scenario_defaults.mjs']) script(name);
run('data:reform:check');
run('data:reform:shares:release:check', ['--catalog', resolve(runtime, 'candidate.json')]);
run('data:workspaces:check');

const catalog = JSON.parse(await readFile(resolve(web, 'src/lib/politics/reform-2026/data/generation-catalog.generated.json'), 'utf8'));
const paths = ['static/data/v1/metadata.json', 'static/data/v1/politics-static.json', 'static/data/v1/regional-er-static.json', `static/${catalog.path}`,
  'static/data/reform-2026/fast-share/candidate.json', 'src/lib/workspaces/snapshots.generated.ts',
  ...['politics-defaults', 'politics-snapshot-defaults', 'politics-candidate-slots', 'politics-municipalities', 'regional-er-defaults'].map(n => `src/lib/scenario/${n}.generated.ts`)];
const artifacts = await Promise.all(paths.map(async path => { const bytes = await readFile(resolve(web, path)); return { path, bytes: bytes.length, sha256: sha256(bytes) }; }));
const receipt = { method: 'node-production-preparation-v1', artifacts };
await mkdir(output, { recursive: true });
await writeFile(resolve(output, `${command}-report.json`), `${JSON.stringify({ ...receipt, node: process.version, platform: process.platform, elapsedMs: performance.now() - started }, null, 2)}\n`);
console.log(JSON.stringify({ status: 'passed', command, receiptSha256: sha256(JSON.stringify(receipt)), artifacts: artifacts.length, elapsedMs: performance.now() - started }));
