import { createHash } from 'node:crypto';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve } from 'node:path';
import { requireUnitAcceptance } from './reports';
import { runLoggedCommand } from './logged-command';
import { acceptanceWorkers } from './worker-limits';
import { acceptancePolicy, acceptanceProfile, browserCases, requireBrowserCases } from './policy';

const web = fileURLToPath(new URL('../../', import.meta.url));
const profile = acceptanceProfile(process.argv[2] ?? process.env.ACCEPTANCE_PROFILE);
const output = resolve(web, process.env.ACCEPTANCE_OUTPUT ?? `../data/derived/acceptance-${profile}`);
await mkdir(output, { recursive: true });
const report = { policy: acceptancePolicy, profile, created: new Date().toISOString(), status: 'running', node: process.version, platform: process.platform,
  workers: acceptanceWorkers(process.env),
  scope: 'Named application acceptance profile; constructed UI preparation is not production fitting evidence; no deployment.',
  inputs: [] as { path: string; bytes: number; sha256: string }[],
  checks: [] as { name?: string; passed?: number; [key: string]: unknown }[] };
const save = () => writeFile(resolve(output, 'report.json'), `${JSON.stringify(report, null, 2)}\n`);
const npmCli = process.env.npm_execpath;
if (!npmCli) throw new Error('Run through npm run acceptance:reform');
// Production worker tests already size their own compute pools. Allow a bounded
// test pool on large desktop hosts instead of multiplying both pool sizes.
const unitWorkers = ['--maxWorkers', String(report.workers.units)];
const browserWorkers = ['--workers', String(report.workers.browsers)];
async function run(name: string, args: string[], environment: Record<string, string> = {}) {
  const log = resolve(output, 'logs', `${String(report.checks.length + 1).padStart(2, '0')}-${name.replace(/[^a-z0-9]+/gi, '-').replace(/-$/, '')}.log`);
  const result = await runLoggedCommand({ name, command: process.execPath, args: [npmCli!, ...args], cwd: web, log,
    env: { ...process.env, BASE_PATH: '', ...environment } });
  report.checks.push({ ...result }); await save();
  if (result.status === 'failed') throw new Error(`${name} failed; full output: ${log}`);
}
async function accept<T>(name: string, path: string, validate: (report: T) => { passed: number; skipped: number }) {
  try {
    const counts = validate(JSON.parse(await readFile(path, 'utf8')));
    report.checks.push({ name, ...counts, report: path }); await save();
    console.log(`ACCEPTANCE ${name}: ${counts.passed} passed; ${counts.skipped} skipped; report=${path}`);
  } catch (error) { throw new Error(`${String(error)}; full test report: ${path}`); }
}
try {
  // Missing production artifacts fail before any development suite can skip.
  const catalog = JSON.parse(await readFile(resolve(web, 'src/lib/politics/reform-2026/data/generation-catalog.generated.json'), 'utf8'));
  for (const path of ['static/data/v1/politics-static.json', 'static/data/v1/regional-er-static.json', `static/${catalog.path}`]) {
    const bytes = await readFile(resolve(web, path));
    report.inputs.push({ path, bytes: bytes.length, sha256: createHash('sha256').update(bytes).digest('hex') });
  }
  await run('complete production preparation and generated data', ['run', 'data:check']);
  // data:check already requires the reform slice, workspaces and share release.
  await run('preparation types', ['run', 'check:data']);
  await run('application types', ['run', 'check']);
  const units = resolve(output, 'unit.json');
  await run('unit suite', ['run', 'test', '--', '--reporter=json', `--outputFile=${units}`, ...unitWorkers]);
  await accept('unit acceptance', units, requireUnitAcceptance);
  const extended = profile === 'extended', development = profile === 'development';
  const bases = extended ? ['/elezioni', ''] : ['/elezioni'];
  async function browser(name: string, script: string, stem: string, expected: number, env: Record<string, string>, args: string[] = []) {
    const path = resolve(output, `${stem}.json`);
    await run(name, ['run', script, '--', '--reporter=line,json', '--retries=0', '--max-failures=1', ...browserWorkers,
      '--output', resolve(output, `${stem}-output`), ...args], { ...env, PLAYWRIGHT_JSON_OUTPUT_NAME: path });
    await accept(`${name} acceptance`, path, data => requireBrowserCases(data as Parameters<typeof requireBrowserCases>[0], expected));
  }
  for (const base of bases) {
    const suffix = base ? 'project' : 'root';
    const env = { BASE_PATH: base, ACCEPTANCE_SERVER_PORT: '4175', ACCEPTANCE_PREBUILT: '1',
      ACCEPTANCE_FULL_MATRIX: extended ? '1' : '0' };
    await run(`static build ${base || '/'}`, ['run', 'build'], env);
    // The retained Firefox/M02 production workflow runs early, once. No duplicated preflight.
    if (!development) await browser(`production archive workflows ${base || '/'}`, 'test:reform:shares:unified',
      `unified-${suffix}`, extended ? browserCases.fullUnified : browserCases.representative,
      { ...env, FV08_DRAWS: base ? '3' : '1' });
    await browser(`browser suite ${base || '/'}`, development ? 'test:e2e' : 'test:e2e:all', `browser-${suffix}`,
      development ? browserCases.development : browserCases.release, env);
    await browser(`editor cancellation ${base || '/'}`, 'test:reform:shares:ui', `editor-${suffix}`,
      browserCases.editor, env, ['--project=chromium']);
  }
  if (!development && !extended) {
    const env = { BASE_PATH: '', ACCEPTANCE_SERVER_PORT: '4175', ACCEPTANCE_PREBUILT: '1' };
    await run('static build /', ['run', 'build'], env);
    await browser('root route/worker smoke', 'test:base-smoke', 'root-smoke', browserCases.rootSmoke, env);
  }
  report.status = 'passed';
} catch (error) {
  report.status = 'failed';
  console.error(`ACCEPTANCE failed: ${String(error)}`);
  report.checks.push({ error: String(error) }); process.exitCode = 1;
} finally {
  await save();
  const counts = report.checks.filter(c => c.passed !== undefined);
  console.log(`Acceptance ${report.status}: units=${counts.find(c => c.name === 'unit acceptance')?.passed ?? 'unavailable'}; browserChecks=${counts.filter(c => c.name !== 'unit acceptance').reduce((n, c) => n + c.passed!, 0)}; report=${resolve(output, 'report.json')}`);
}
