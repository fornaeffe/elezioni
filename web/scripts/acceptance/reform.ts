import { spawn } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve } from 'node:path';
import { requireBrowserAcceptance, requireUnitAcceptance } from './reports';

const web = fileURLToPath(new URL('../../', import.meta.url));
const output = resolve(web, '../data/derived/r26-21-acceptance');
await mkdir(output, { recursive: true });
const report = { created: new Date().toISOString(), status: 'running', node: process.version, platform: process.platform,
  scope: 'Local production application acceptance; provisional I08 tables; conditional synthetic preferences; no deployment.',
  inputs: [] as { path: string; bytes: number; sha256: string }[], checks: [] as object[] };
const save = () => writeFile(resolve(output, 'report.json'), `${JSON.stringify(report, null, 2)}\n`);
const npmCli = process.env.npm_execpath;
if (!npmCli) throw new Error('Run through npm run acceptance:reform');
// Production worker tests already size their own compute pools. Allow a bounded
// test pool on large desktop hosts instead of multiplying both pool sizes.
const testWorkers = process.env.ACCEPTANCE_TEST_WORKERS;
if (testWorkers !== undefined && (!/^\d+$/.test(testWorkers) || Number(testWorkers) < 1 || Number(testWorkers) > 64)) {
  throw new Error('ACCEPTANCE_TEST_WORKERS must be an integer from 1 through 64');
}
async function run(name: string, args: string[], environment: Record<string, string> = {}) {
  console.log(`ACCEPTANCE ${name}`);
  const started = performance.now();
  await new Promise<void>((done, reject) => {
    const child = spawn(process.execPath, [npmCli!, ...args], { cwd: web, stdio: 'inherit',
      env: { ...process.env, BASE_PATH: '', ...environment } });
    child.on('error', reject);
    child.on('exit', code => code === 0 ? done() : reject(new Error(`${name} exited ${code}`)));
  });
  report.checks.push({ name, elapsedMs: performance.now() - started }); await save();
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
  await run('unit suite', ['run', 'test', '--', '--reporter=json', `--outputFile=${units}`, ...(testWorkers ? ['--maxWorkers', testWorkers] : [])]);
  report.checks.push({ name: 'unit acceptance', ...requireUnitAcceptance(JSON.parse(await readFile(units, 'utf8'))) });
  for (const base of ['/elezioni', '']) {
    const smoke = base === '';
    const browserReport = resolve(output, base ? 'browser-project.json' : 'browser-root.json');
    const env = { BASE_PATH: base, ACCEPTANCE_SERVER_PORT:'4175', REFORM_FAST_CANDIDATE:'1', ACCEPTANCE_PREBUILT: '1', PLAYWRIGHT_JSON_OUTPUT_NAME: browserReport };
    await run(`static build ${base || '/'}`, ['run', 'build'], env);
    // The full behavioral matrix runs once at the deployed project path. Root
    // still exercises navigation/refresh, imports and real previous-law/ER workers.
    const rootSpecs = ['static-routes.spec.ts', 'workflow-selector.spec.ts', 'workspace-uniformity.spec.ts', 'regional-er.spec.ts'];
    await run(`browser ${smoke ? 'URL smoke' : 'suite'} ${base || '/'}`, ['run', 'test:e2e', '--', ...(smoke ? rootSpecs : []), '--reporter=json', '--retries=0', ...(testWorkers ? ['--workers', testWorkers] : [])], env);
    report.checks.push({ name: `browser acceptance ${base || '/'}`, ...requireBrowserAcceptance(JSON.parse(await readFile(browserReport, 'utf8'))) });
    if (!smoke) {
      const editorReport = resolve(output, 'editor-project.json');
      await run(`current scenario editor ${base}`, ['run','test:reform:shares:ui','--','--project=chromium','--reporter=json','--retries=0'], {...env, PLAYWRIGHT_JSON_OUTPUT_NAME: editorReport});
      report.checks.push({name: `editor acceptance ${base}`, ...requireBrowserAcceptance(JSON.parse(await readFile(editorReport, 'utf8')))});
    }
    const unifiedReport=resolve(output,base?'unified-project.json':'unified-root.json'),unifiedEnv={...env,REFORM_FAST_CANDIDATE:'1',PLAYWRIGHT_JSON_OUTPUT_NAME:unifiedReport,
      FV08_FULL:'0',FV08_DRAWS:smoke?'1':'3',FV08_WORKERS:smoke?'1':'', ACCEPTANCE_ROOT_SMOKE:smoke?'1':'0'};
    await run(`unified ${smoke ? 'URL smoke' : 'browser/storage/export'} ${base || '/'}`,['run','test:reform:shares:unified','--','--reporter=json','--retries=0', ...(smoke ? ['--grep', 'unified browser workflow'] : [])],unifiedEnv);
    report.checks.push({name:`unified browser acceptance ${base||'/'}`,...requireBrowserAcceptance(JSON.parse(await readFile(unifiedReport,'utf8')))});
  }
  // Root was built last, so the usable local build is already restored.
  report.status = 'passed';
} catch (error) {
  report.status = 'failed';
  report.checks.push({ error: String(error) }); process.exitCode = 1;
} finally { await save(); console.log(`Acceptance ${report.status}: ${resolve(output, 'report.json')}`); }
