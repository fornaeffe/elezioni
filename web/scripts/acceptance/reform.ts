import { createHash } from 'node:crypto';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve } from 'node:path';
import { requireBrowserAcceptance, requireUnitAcceptance } from './reports';
import { runLoggedCommand } from './logged-command';
import { acceptanceWorkers } from './worker-limits';

const web = fileURLToPath(new URL('../../', import.meta.url));
const output = resolve(web, process.env.ACCEPTANCE_OUTPUT ?? '../data/derived/r26-21-acceptance');
await mkdir(output, { recursive: true });
const report = { created: new Date().toISOString(), status: 'running', node: process.version, platform: process.platform,
  workers: acceptanceWorkers(process.env),
  diagnostics: [] as { base: string; status: string; report: string; passed?: number; skipped?: number }[],
  scope: 'Local production application acceptance; provisional I08 tables; conditional synthetic preferences; no deployment.',
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
  for (const base of ['/elezioni', '']) {
    const smoke = base === '';
    const browserReport = resolve(output, base ? 'browser-project.json' : 'browser-root.json');
    const env = { BASE_PATH: base, ACCEPTANCE_SERVER_PORT:'4175',  ACCEPTANCE_PREBUILT: '1', PLAYWRIGHT_JSON_OUTPUT_NAME: browserReport };
    await run(`static build ${base || '/'}`, ['run', 'build'], env);
    if (!smoke) {
      const diagnostic = { base, status: 'running', report: resolve(output, 'browser-diagnostic.json') };
      report.diagnostics.push(diagnostic); await save();
      try {
        await run('browser diagnostic preflight', ['run', 'test:e2e', '--', '--workers=1', '--retries=0', '--max-failures=1',
          '--reporter=line,json', '--output', resolve(output, 'browser-diagnostic-output'), '--grep',
          'status remains fixed and compact|new lists initialize every law|sparse result tables reconstruct|politiche-reform-2026-v1/M02: residual creation'],
          { ...env, PLAYWRIGHT_JSON_OUTPUT_NAME: diagnostic.report });
        const counts = requireBrowserAcceptance(JSON.parse(await readFile(diagnostic.report, 'utf8')));
        if (counts.passed !== 4) throw new Error(`Expected all four diagnostic checks, received ${counts.passed}`);
        Object.assign(diagnostic, counts, { status: 'passed' });
      } catch (error) { diagnostic.status = 'failed'; throw error; }
      await save();
    }
    // CP-08 requires the common domain/editor/session and independent regional
    // matrix at both bases, including all four supported worker pairs.
    await run(`browser suite ${base || '/'}`, ['run', 'test:e2e', '--', '--reporter=line,json', '--retries=0', '--max-failures=1', ...browserWorkers,
      '--output', resolve(output, base ? 'browser-project-output' : 'browser-root-output')], env);
    await accept(`browser acceptance ${base || '/'}`, browserReport, requireBrowserAcceptance);
    {
      const editorReport = resolve(output, base ? 'editor-project.json' : 'editor-root.json');
      await run(`current scenario editor ${base}`, ['run','test:reform:shares:ui','--','--project=chromium','--reporter=line,json','--retries=0','--max-failures=1', ...browserWorkers,
        '--output', resolve(output, base ? 'editor-project-output' : 'editor-root-output')], {...env, PLAYWRIGHT_JSON_OUTPUT_NAME: editorReport});
      await accept(`editor acceptance ${base}`, editorReport, requireBrowserAcceptance);
    }
    const unifiedReport=resolve(output,base?'unified-project.json':'unified-root.json'),unifiedEnv={...env,PLAYWRIGHT_JSON_OUTPUT_NAME:unifiedReport,
      FV08_DRAWS:smoke?'1':'3'};
    await run(`unified ${smoke ? 'URL smoke' : 'browser/storage/export'} ${base || '/'}`,['run','test:reform:shares:unified','--','--reporter=line,json','--retries=0','--max-failures=1', ...browserWorkers,
      '--output', resolve(output, base ? 'unified-project-output' : 'unified-root-output'), ...(smoke ? ['--grep', 'unified browser workflow'] : [])],unifiedEnv);
    await accept(`unified browser acceptance ${base || '/'}`, unifiedReport, requireBrowserAcceptance);
  }
  // Root was built last, so the usable local build is already restored.
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
