/** Durable hosting gate. The hosted cold run starts from an uncached checkout. */
import { execFileSync } from 'node:child_process';
import { cpus, totalmem, freemem } from 'node:os';
import { copyFile, mkdir, readFile, statfs, writeFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { runLoggedCommand, type CommandResult } from '../acceptance/logged-command';
import { requireBrowserAcceptance } from '../acceptance/reports';
import { workspaceSources } from '../data-preparation/workspace-history';
import { files, inside, stagePayload } from './payload';
import { lstat } from 'node:fs/promises';

const web = fileURLToPath(new URL('../../', import.meta.url)), root = resolve(web, '..');
const mode = process.argv[2];
if (mode !== 'cold' && mode !== 'warm') throw new Error('Use hosting:reassess -- cold|warm');
if (mode === 'cold' && (existsSync(resolve(root, 'data/derived/fast-share-cache')) || existsSync(resolve(web, 'static/data/v1/politics-static.json'))))
  throw new Error('Cold hosting acceptance requires a fresh checkout without downloaded/generated caches');
if (mode === 'cold') for (const source of await workspaceSources(root)) {
  if (!source.repositoryPath && existsSync(inside(resolve(root, 'data/sources'), source.path)))
    throw new Error(`Cold hosting acceptance found an already downloaded original: ${source.id}`);
}
const output = resolve(root, 'data/derived/hosting', mode);
if (existsSync(resolve(output, 'report.json'))) throw new Error(`Preserve prior evidence; use a fresh checkout or move ${output} before rerunning`);
await mkdir(output, { recursive: true });
const npm = process.env.npm_execpath;
if (!npm) throw new Error('Run via npm run hosting:reassess');
const revision = execFileSync('git', ['rev-parse', 'HEAD'], { cwd: root, encoding: 'utf8' }).trim();
const workingTreeDirty = execFileSync('git', ['status', '--porcelain'], { cwd: root, encoding: 'utf8' }).trim().length > 0;
const started = performance.now();
const report = { status: 'running', mode, revision, workingTreeDirty, created: new Date().toISOString(),
  hosted: process.env.GITHUB_ACTIONS === 'true', runner: process.env.RUNNER_NAME ?? null,
  platform: process.platform, node: process.version, cpus: cpus().length, ramBytes: totalmem(),
  minimumAvailableRamBytes: freemem(), minimumFreeDiskBytes: Infinity,
  elapsedMs: 0, linuxResourceFiles: [] as string[],
  sourceBytes: null as number | null, modelCacheBytes: null as number | null,
  steps: [] as CommandResult[], payload: null as Awaited<ReturnType<typeof stagePayload>> | null,
  acceptance: null as unknown, smoke: null as unknown, error: null as string | null };
let saving = Promise.resolve();
const save = () => {
  report.elapsedMs = performance.now() - started;
  const json = `${JSON.stringify(report, null, 2)}\n`;
  saving = saving.then(() => writeFile(resolve(output, 'report.json'), json));
  return saving;
};
async function sample() {
  report.minimumAvailableRamBytes = Math.min(report.minimumAvailableRamBytes, freemem());
  const disk = await statfs(root);
  report.minimumFreeDiskBytes = Math.min(report.minimumFreeDiskBytes, disk.bavail * disk.bsize);
}
await sample(); await save();
const timer = setInterval(() => { void sample().then(save).catch(error => console.error(`Resource sample: ${error}`)); }, 5000);
async function run(name: string, args: string[], env: NodeJS.ProcessEnv = {}) {
  const log = resolve(output, 'logs', `${report.steps.length + 1}-${name}.log`);
  // GNU time records child-tree high-water RSS/CPU/I/O on the actual Linux host.
  const timed = process.platform === 'linux';
  const result = await runLoggedCommand({ name, command: timed ? '/usr/bin/time' : process.execPath,
    args: timed ? ['-v', '-o', `${log}.resources.txt`, process.execPath, npm!, ...args] : [npm!, ...args],
    cwd: web, log, env: { ...process.env, BUILD_REVISION: revision, ACCEPTANCE_TEST_WORKERS: '2', ...env } });
  if (timed) report.linuxResourceFiles.push(`${log}.resources.txt`);
  report.steps.push(result); await sample(); await save();
  if (result.status !== 'passed') throw new Error(`${name} failed; see ${log}`);
}
try {
  await run('prepare', ['run', 'data:prepare', ...(mode === 'cold' ? ['--', '--download'] : [])]);
  const directoryBytes = async (directory: string) => {
    let bytes = 0;
    for (const path of await files(directory)) bytes += (await lstat(inside(directory, path))).size;
    return bytes;
  };
  report.sourceBytes = await directoryBytes(resolve(root, 'data/sources'));
  report.modelCacheBytes = await directoryBytes(resolve(root, 'data/derived/fast-share-cache'));
  await copyFile(resolve(root, 'data/derived/production/prepare-report.json'), resolve(output, 'prepare-report.json'));
  const acceptanceOutput = resolve(output, 'application');
  await run('application', ['run', 'acceptance:reform'], { ACCEPTANCE_OUTPUT: acceptanceOutput,
    ACCEPTANCE_ARCHIVE_OUTPUT: resolve(output, 'archives') });
  await copyFile(resolve(root, 'data/derived/production/check-report.json'), resolve(output, 'check-report.json'));
  const acceptance = JSON.parse(await readFile(resolve(acceptanceOutput, 'report.json'), 'utf8'));
  if (acceptance.status !== 'passed') throw new Error('Application acceptance did not pass');
  report.acceptance = { status: acceptance.status, inputs: acceptance.inputs,
    counts: acceptance.checks.filter((c: { passed?: number }) => c.passed !== undefined) };
  await run('pages-build', ['run', 'build'], { BASE_PATH: '/elezioni' });
  const site = resolve(output, 'site');
  report.payload = await stagePayload(web, site, revision); await save();
  const browserReport = resolve(output, 'browser.json');
  await run('staged-site', ['run', 'test:hosting', '--', '--reporter=json'], {
    BASE_PATH: '/elezioni', ACCEPTANCE_BUILD_ROOT: site, ACCEPTANCE_GZIP_ENCODING: '0',
    HOSTING_BROWSER_OUTPUT: resolve(output, 'browser-output'),
    HOSTING_TRANSFER_OUTPUT: resolve(output, 'transfers'), HOSTING_EXPECTED_REVISION: revision,
    PLAYWRIGHT_JSON_OUTPUT_NAME: browserReport,
  });
  report.smoke = requireBrowserAcceptance(JSON.parse(await readFile(browserReport, 'utf8')));
  if (mode === 'warm' && process.env.GITHUB_ACTIONS === 'true') {
    const cold = JSON.parse(await readFile(resolve(output, '../cold/report.json'), 'utf8'));
    if (cold.status !== 'passed' || cold.payload.manifestSha256 !== report.payload.manifestSha256)
      throw new Error('Cold/warm production payloads differ or cold acceptance failed');
  }
  report.status = 'passed';
} catch (error) {
  report.status = 'failed'; report.error = String(error); process.exitCode = 1;
  console.error(report.error);
} finally {
  clearInterval(timer); await sample(); await save();
  console.log(`HOSTING ${report.status}: mode=${mode}; hosted=${report.hosted}; siteBytes=${report.payload?.bytes ?? 'unavailable'}; report=${resolve(output, 'report.json')}`);
}
