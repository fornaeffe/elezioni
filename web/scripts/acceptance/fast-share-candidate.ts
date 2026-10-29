/** Isolated source-only cold/warm candidate acceptance. Does not publish a site. */
import { execFileSync, spawn } from 'node:child_process';
import { createWriteStream } from 'node:fs';
import {
  mkdtemp,
  mkdir,
  readFile,
  writeFile,
  rename,
  stat,
} from 'node:fs/promises';
import { resolve, dirname, relative, isAbsolute } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
import { unzipSync } from 'fflate';
import { decodeState } from '../../src/lib/politics/reform-2026/vote-share/state-codec';
import type {
  CandidatePointer,
  ReleaseCatalog,
} from '../../src/lib/politics/reform-2026/vote-share/release';
const root = fileURLToPath(new URL('../../../', import.meta.url)),
  npm = process.env.npm_execpath;
if (!npm)
  throw new Error('Run through npm run acceptance:reform:shares:candidate');
const output = resolve(root, 'data/derived/fv05-acceptance');
await mkdir(output, { recursive: true });
const checkout = await mkdtemp(resolve(output, 'clean-'));
const inside = (path: string) => {
  const p = resolve(path),
    rel = relative(checkout, p);
  if (!rel || rel.startsWith('..') || isAbsolute(rel))
    throw new Error('Isolated acceptance path escaped checkout');
  return p;
};
const archive = unzipSync(
  execFileSync('git', ['archive', '--format=zip', 'HEAD'], {
    cwd: root,
    maxBuffer: 256 * 1024 * 1024,
  }),
);
for (const [name, bytes] of Object.entries(archive)) {
  if (name.startsWith('research/') || name.endsWith('/')) continue;
  const path = inside(resolve(checkout, name));
  await mkdir(dirname(path), { recursive: true });
  await writeFile(path, bytes);
}
const gitFiles = (args: string[]) =>
  execFileSync('git', [...args, '-z'], { cwd: root, encoding: 'utf8' })
    .split('\0')
    .filter(Boolean);
const changed = [
  ...new Set([
    ...gitFiles(['diff', '--name-only', 'HEAD']),
    ...gitFiles(['ls-files', '--others', '--exclude-standard']),
  ]),
];
for (const name of changed) {
  if (name.startsWith('research/')) continue;
  const path = inside(resolve(checkout, name));
  await mkdir(dirname(path), { recursive: true });
  await writeFile(path, await readFile(resolve(root, name)));
}
const report = {
  stage: 'FV-05',
  status: 'running',
  checkout,
  baseRevision: execFileSync('git', ['rev-parse', 'HEAD'], {
    cwd: root,
    encoding: 'utf8',
  }).trim(),
  node: process.version,
  scope:
    'HEAD plus the current task diff, freshly installed dependencies, initially empty raw/numerical/output caches, no research directory or ignored production/debug data. Candidate assets only; whole-application publication acceptance remains separate.',
  checks: [] as object[],
};
const save = () =>
  writeFile(
    resolve(output, 'isolated-acceptance.json'),
    JSON.stringify(report, null, 2) + '\n',
  );
async function run(name: string, args: string[], expectFailure = false) {
  const path = resolve(checkout, name + '.log'),
    log = createWriteStream(path),
    start = performance.now();
  console.log('FV05 ACCEPTANCE ' + name + ' -> ' + path);
  const code = await new Promise<number | null>((done, reject) => {
    const child = spawn(process.execPath, [npm!, ...args], {
      cwd: resolve(checkout, 'web'),
      env: { ...process.env, BASE_PATH: '', CI: 'true' },
      stdio: ['ignore', 'pipe', 'pipe'],
    });
    child.stdout.on('data', (b) => log.write(b));
    child.stderr.on('data', (b) => log.write(b));
    child.on('error', reject);
    child.on('close', (code) => {
      log.end(() => done(code));
    });
  });
  if (expectFailure ? code === 0 : code !== 0)
    throw new Error(
      name +
        ' exited ' +
        code +
        '; ' +
        (await readFile(path, 'utf8')).slice(-3500),
    );
  report.checks.push({
    name,
    expectedFailure: expectFailure,
    exitCode: code,
    milliseconds: performance.now() - start,
    log: relative(checkout, path),
  });
  await save();
  return readFile(path, 'utf8');
}
const hash = (bytes: Uint8Array) =>
  createHash('sha256').update(bytes).digest('hex');
async function assets(directory: string) {
  const pointer: CandidatePointer = JSON.parse(
      await readFile(resolve(directory, 'candidate.json'), 'utf8'),
    ),
    read = (p: string) => readFile(resolve(directory, p));
  const c = await decodeState<ReleaseCatalog>(
      pointer.catalog,
      read,
      'fast-share-catalog',
    ),
    names = [
      'candidate.json',
      pointer.catalog.path,
      c.state.assets.source.path,
      ...c.state.sourceChunks.map((a) => a.path),
    ];
  let fitChunk = '';
  for (const kind of ['fit', 'profiles', 'runtime', 'scenario'] as const) {
    const d = c.state.assets[kind],
      state = await decodeState(
        d,
        read,
        kind === 'fit'
          ? 'fast-share-fit'
          : kind === 'profiles'
            ? 'fast-share-profiles'
            : kind === 'runtime'
              ? 'fast-share-runtime'
              : 'fast-share-scenario',
      );
    names.push(d.path);
    if (state.envelope.payload.chunk) {
      names.push(state.envelope.payload.chunk.path);
      if (kind === 'fit') fitChunk = state.envelope.payload.chunk.path;
    }
  }
  const files = Object.fromEntries(
    await Promise.all(
      [...new Set(names)].sort().map(async (p) => [p, hash(await read(p))]),
    ),
  );
  return {
    catalogId: pointer.catalogId,
    files,
    fitChunk,
    sourceChunk: c.state.sourceChunks.at(-1)!.path,
  };
}
try {
  await run('install', ['ci']);
  await run('cold', [
    'run',
    'data:reform:shares:release',
    '--',
    '--download',
    '--report',
    resolve(checkout, 'cold.json'),
  ]);
  const directory = resolve(checkout, 'data/derived/fast-share-release'),
    cold = await assets(directory),
    coldReport = JSON.parse(
      await readFile(resolve(checkout, 'cold.json'), 'utf8'),
    );
  if (Object.values(coldReport.cache).some(Boolean))
    throw new Error('Cold acceptance reused a generated cache');
  await run('warm', [
    'run',
    'data:reform:shares:release',
    '--',
    '--report',
    resolve(checkout, 'warm.json'),
  ]);
  const warm = await assets(directory),
    warmReport = JSON.parse(
      await readFile(resolve(checkout, 'warm.json'), 'utf8'),
    );
  if (
    !Object.values(warmReport.cache).every(Boolean) ||
    JSON.stringify(cold) !== JSON.stringify(warm)
  )
    throw new Error('Cold/warm candidate bytes or cache reuse differ');
  const local = resolve(root, 'data/derived/fast-share-release');
  try {
    await stat(resolve(local, 'candidate.json'));
    if (
      JSON.stringify((await assets(local)).files) !== JSON.stringify(warm.files)
    )
      throw new Error('Clean/local candidate bytes differ');
    report.checks.push({ name: 'clean/local byte equality', passed: true });
  } catch (e) {
    if ((e as NodeJS.ErrnoException).code !== 'ENOENT') throw e;
  }
  report.checks.push({
    name: 'cold/warm byte equality',
    passed: true,
    catalogId: cold.catalogId,
    files: cold.files,
    coldCache: coldReport.cache,
    warmCache: warmReport.cache,
  });
  const required = inside(resolve(directory, cold.sourceChunk)),
    withheld = inside(required + '.withheld');
  await rename(required, withheld);
  try {
    await run(
      'missing-refit-asset',
      [
        'run',
        'data:reform:shares:release:check',
        '--',
        '--catalog',
        resolve(directory, 'candidate.json'),
      ],
      true,
    );
  } finally {
    await rename(withheld, required);
  }
  const pointerPaths = [
      resolve(directory, 'candidate.json'),
      resolve(
        checkout,
        'web/static/data/reform-2026/fast-share/candidate.json',
      ),
      resolve(checkout, 'web/build/data/reform-2026/fast-share/candidate.json'),
    ],
    before = await Promise.all(pointerPaths.map((p) => readFile(p))),
    chunk = inside(
      resolve(checkout, 'data/derived/fast-share-cache', cold.fitChunk),
    ),
    original = await readFile(chunk);
  await writeFile(chunk, new Uint8Array(20));
  try {
    const error = await run(
      'corrupt-fit-cache',
      ['run', 'data:reform:shares:release:prepare'],
      true,
    );
    if (!error.includes('INTEGRITY'))
      throw new Error('Wrong corruption failure');
  } finally {
    await writeFile(chunk, original);
  }
  for (const [p, i] of pointerPaths.map((p, i) => [p, i] as const))
    if (!before[i].equals(await readFile(p)))
      throw new Error('Failed preparation changed selected candidate');
  report.checks.push({
    name: 'selected candidates survive failed preparation',
    passed: true,
  });
  report.status = 'passed';
} catch (e) {
  report.status = 'failed';
  report.checks.push({ error: String(e) });
  process.exitCode = 1;
} finally {
  await save();
  console.log(
    'FV05 isolated acceptance ' +
      report.status +
      ': ' +
      resolve(output, 'isolated-acceptance.json'),
  );
}
