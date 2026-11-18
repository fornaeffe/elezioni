import { fileURLToPath } from 'node:url';
import { resolve, dirname } from 'node:path';
import { readFile, mkdir, writeFile } from 'node:fs/promises';
import { execFileSync } from 'node:child_process';
import { cpus, platform } from 'node:os';
import { releaseInputs } from './fast-share-release-inputs';
import { prepareShareRelease } from './fast-share-release';
import { verifyShareRelease } from './fast-share-release-check';
import {
  readBytes,
  immutableBytes,
  selectCandidate,
} from './fast-share-release-io';
import type { CandidatePointer } from '../../src/lib/politics/reform-2026/vote-share/release';
const root = fileURLToPath(new URL('../../../', import.meta.url)),
  [command, ...args] = process.argv.slice(2),
  options = new Map<string, string>();
const values = new Set([
  '--sources',
  '--output',
  '--cache',
  '--catalog',
  '--static',
  '--base',
  '--report',
]);
let download = false;
for (let i = 0; i < args.length; i++) {
  const key = args[i];
  if (key === '--download') {
    if (download) throw new Error('Duplicate --download');
    download = true;
    continue;
  }
  if (
    !values.has(key) ||
    options.has(key) ||
    !args[i + 1] ||
    args[i + 1].startsWith('--')
  )
    throw new Error('Invalid/duplicate release option ' + key);
  options.set(key, args[++i]);
}
if (!['prepare', 'release', 'check'].includes(command))
  throw new Error(
    'Use shares:release [--download] | shares:release:prepare | shares:release:check --catalog FILE',
  );
const option = (key: string, fallback: string) => options.get(key) ?? fallback,
  output = resolve(
    option('--output', resolve(root, 'data/derived/fast-share-release')),
  ),
  cache = resolve(
    option('--cache', resolve(root, 'data/derived/fast-share-cache')),
  ),
  sources = resolve(option('--sources', resolve(root, 'data/sources'))),
  staticDirectory = resolve(
    option('--static', resolve(root, 'web/static/data/reform-2026/fast-share')),
  ),
  started = performance.now();
if (process.versions.node.split('.')[0] !== '24')
  throw new Error('Candidate release preparation requires Node 24');
const relativeAssets = 'data/reform-2026/fast-share',
  base = option('--base', process.env.BASE_PATH ?? '');
if (base && !/^\/[a-zA-Z0-9_-]+(?:\/[a-zA-Z0-9_-]+)*$/.test(base))
  throw new Error('Invalid project base path');
if (
  command === 'release' &&
  staticDirectory !== resolve(root, 'web/static', relativeAssets)
)
  throw new Error(
    'Static build requires the configured repository asset root; use prepare for alternate output only',
  );
if (command === 'check') {
  if (download)
    throw new Error('--download is not valid for read-only release checks');
  const file = resolve(option('--catalog', resolve(output, 'candidate.json'))),
    pointer: CandidatePointer = JSON.parse(await readFile(file, 'utf8'));
  console.log(
    JSON.stringify(await verifyShareRelease(pointer, dirname(file)), null, 2),
  );
} else {
  const inputs = await releaseInputs(root, sources, cache, download),
    prepared = await prepareShareRelease(root, output, cache, inputs),
    verified = await verifyShareRelease(prepared.pointer, output);
  if (command === 'release') {
    // Stage immutable assets; leave all prior candidate contents available.
    for (const name of prepared.files)
      await immutableBytes(
        resolve(staticDirectory, name),
        await readBytes(output)(name),
      );
    const npm = process.env.npm_execpath;
    if (!npm)
      throw new Error(
        'Run release through npm so the same Node/npm installation builds the app',
      );
    for (const check of ['check:data', 'check', 'build'])
      execFileSync(process.execPath, [npm, 'run', check], {
        cwd: resolve(root, 'web'),
        stdio: 'inherit',
        env: { ...process.env, BASE_PATH: base },
      });
    const built = resolve(root, 'web/build', relativeAssets);
    await verifyShareRelease(prepared.pointer, built);
    // A pointer is the last write after source and final copied assets pass.
    await selectCandidate(
      resolve(staticDirectory, 'candidate.json'),
      prepared.pointer,
    );
    await selectCandidate(resolve(built, 'candidate.json'), prepared.pointer);
  }
  await selectCandidate(resolve(output, 'candidate.json'), prepared.pointer);
  const report = {
    stage: 'FV-05',
    status: 'candidate-only',
    environment: {
      node: process.version,
      platform: platform(),
      cpu: cpus()[0].model,
    },
    catalog: prepared.pointer.catalogId,
    cache: prepared.cache,
    audit: prepared.audit,
    byteBudget: prepared.byteBudget,
    verification: verified,
    staticBuild: command === 'release',
    applicationChecks: command === 'release',
    basePath: option('--base', process.env.BASE_PATH ?? ''),
    elapsedMs: performance.now() - started,
    peakRssKiB: process.resourceUsage().maxRSS,
    qualification:
      'Pinned upcoming-election candidate, not predictive/default activation acceptance. No research export, application selector or deployment is used.',
  };
  const reportPath = resolve(
    option(
      '--report',
      resolve(root, 'data/derived/fv05-acceptance', command + '-report.json'),
    ),
  );
  await mkdir(dirname(reportPath), { recursive: true });
  await writeFile(reportPath, JSON.stringify(report, null, 2) + '\n');
  console.log(
    JSON.stringify(
      {
        catalog: prepared.pointer.catalogId,
        pointer: resolve(output, 'candidate.json'),
        cache: prepared.cache,
        byteBudget: prepared.byteBudget,
        elapsedMs: report.elapsedMs,
        report: reportPath,
        verification: {
          ...verified,
          politicalDiagnostics: undefined,
          participationDiagnostics: undefined,
        },
      },
      null,
      2,
    ),
  );
}
