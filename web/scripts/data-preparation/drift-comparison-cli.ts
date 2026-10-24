/** Resumable staged research. Independent OS processes parallelize numerical
 * fits; the selection file is written before any confirmation forecasts. */
import { spawn } from 'node:child_process';
import { readFile, mkdir, writeFile, readdir } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { DRIFT_POLICY } from './drift-structure';
import type { evaluateFittedDrift } from './drift-evaluate';
import { aggregateScores, type GenerationScores } from './generation-metrics';

const root = fileURLToPath(new URL('../../../', import.meta.url)), web = resolve(root, 'web');
const tsx = resolve(web, 'node_modules/tsx/dist/cli.mjs'), cli = resolve(web, 'scripts/data-preparation/drift-study-cli.ts');
const workers = Number(process.env.DRIFT_WORKERS ?? 6);
const base = 'literal.pooled.common.common.independent.brownian';
const dev = [...DRIFT_POLICY.developmentIds], confirm = [...DRIFT_POLICY.confirmationIds];
const logs = resolve(root, 'data/derived/drift/logs'); await mkdir(logs, { recursive: true });
interface Job { model: string; target: string; draws: number; extra?: string[]; }
async function child(args: string[], log?: string) {
  return new Promise<string>((ok, fail) => {
    const p = spawn(process.execPath, [tsx, '--tsconfig', 'tsconfig.json', cli, ...args], { cwd: web, windowsHide: true });
    let output = '', error = '';
    p.stdout.on('data', b => output += String(b)); p.stderr.on('data', b => error += String(b));
    p.on('error', fail);
    p.on('close', async code => {
      if (log) await writeFile(resolve(logs, log + '.log'), output + '\n' + error);
      if (code) fail(new Error('Forecast process failed: ' + args.join(' ') + '\n' + error)); else ok(output);
    });
  });
}
const versions = new Map<string, string>();
async function version(draws: number, extra: string[] = []) {
  const key = JSON.stringify([draws, extra]);
  if (!versions.has(key)) {
    const output = await child(['check', '--target=camera-2013', '--horizon=15', '--draws=' + draws, ...extra]);
    versions.set(key, JSON.parse(output.split('\n').find(s => s.startsWith('{'))!).runVersion);
  }
  return versions.get(key)!;
}
async function run(models: string[], targets: string[], draws: number, extra: string[] = []) {
  const jobs = models.flatMap(model => targets.map(target => ({ model, target, draws, extra } satisfies Job)));
  let index = 0, done = 0;
  await Promise.all(Array.from({ length: Math.min(workers, jobs.length) }, async () => {
    while (index < jobs.length) {
      const job = jobs[index++], label = [job.model, job.target, draws, ...extra].join('_').replaceAll(/[^\w.-]/g, '');
      console.log(JSON.stringify({ phase: 'start', ...job }));
      await child([job.model === 'current' ? 'current' : 'generate', '--model=' + (job.model === 'current' ? base : job.model), '--target=' + job.target, '--draws=' + draws, ...extra], label);
      console.log(JSON.stringify({ phase: 'done', model: job.model, target: job.target, completed: ++done, total: jobs.length }));
    }
  }));
}
type Metrics = ReturnType<typeof evaluateFittedDrift>;
interface Result { targetId: string; horizon: number; metrics: Metrics; fit?: { converged: boolean }; }
const scoreKeys = ['nationalPoliticalValid', 'nationalNonvalid', 'regionalPoliticalValid', 'regionalNonvalid', 'municipalPoliticalValid', 'municipalNonvalid'] as const;
async function summarize(model: string, targets: string[], draws: number, extra: string[] = []) {
  const dir = resolve(root, 'data/derived/drift', await version(draws, extra)), rows: Result[] = [];
  for (const target of targets) for (const horizon of DRIFT_POLICY.horizons) {
    if (extra.some(x => x.startsWith('--horizon=')) && !extra.includes('--horizon=' + horizon)) continue;
    rows.push(JSON.parse(await readFile(resolve(dir, model + '-' + target + '-' + horizon + '.json'), 'utf8')));
  }
  const metrics = Object.fromEntries(scoreKeys.map(k => [k, aggregateScores(rows.map(r => r.metrics[k]))])) as Record<typeof scoreKeys[number], GenerationScores>;
  const invalid = rows.reduce((s, r) => s + r.metrics.plausibility.invalid, 0);
  const numericalFailures = rows.filter(r => r.fit && (!r.fit.converged || Math.max(...r.metrics.posterior.hyper.splitRhat) > 1.1 || Math.min(...r.metrics.posterior.hyper.ess) < 30 || r.metrics.posterior.geographicBridgeJitter > 1e-6)).length;
  const extreme = Math.max(...rows.map(r => Math.max(r.metrics.plausibility.nationalNamedPartyOver80, r.metrics.plausibility.nationalNonvalidOver90)));
  const deficit = scoreKeys.reduce((s, k) => s + Math.max(0, .85 - metrics[k].coverage90) / scoreKeys.length, 0);
  const wis = scoreKeys.reduce((s, k) => s + metrics[k].wis / scoreKeys.length, 0);
  const mae = scoreKeys.reduce((s, k) => s + metrics[k].meanMae / scoreKeys.length, 0);
  return { model, cases: rows.length, metrics, invalid, numericalFailures, extreme, deficit, wis, mae,
    byHorizon: Object.fromEntries(DRIFT_POLICY.horizons.filter(h => rows.some(r => r.horizon === h)).map(h => [h, Object.fromEntries(scoreKeys.map(k => [k, aggregateScores(rows.filter(r => r.horizon === h).map(r => r.metrics[k]))]))])),
    byElection: Object.fromEntries(targets.map(t => [t, Object.fromEntries(scoreKeys.map(k => [k, aggregateScores(rows.filter(r => r.targetId === t).map(r => r.metrics[k]))]))])) };
}
type Summary = Awaited<ReturnType<typeof summarize>>;
// Lexicographic rule fixed before scoring. Practically tied coverage (1pp)
// and WIS (2%) retain the earlier, simpler incumbent.
function better(a: Summary, b: Summary) {
  if (a.invalid !== b.invalid) return a.invalid < b.invalid;
  if (a.numericalFailures !== b.numericalFailures) return a.numericalFailures < b.numericalFailures;
  const excessA = Math.max(0, a.extreme - .005), excessB = Math.max(0, b.extreme - .005);
  if (Math.abs(excessA - excessB) > .01) return excessA < excessB;
  if (Math.abs(a.deficit - b.deficit) > .01) return a.deficit < b.deficit;
  if (Math.abs(a.wis / b.wis - 1) > .02) return a.wis < b.wis;
  return false;
}
const runVersion = await version(DRIFT_POLICY.screeningDraws), dir = resolve(root, 'data/derived/drift', runVersion);
await mkdir(dir, { recursive: true });
const stages: { stage: string; summaries: Summary[]; selected: string }[] = [];
async function stage(name: string, models: string[]) {
  const unique = [...new Set(models)]; await run(unique, dev, DRIFT_POLICY.screeningDraws);
  const summaries = await Promise.all(unique.map(m => summarize(m, dev, DRIFT_POLICY.screeningDraws)));
  const selected = summaries.reduce((b, a) => better(a, b) ? a : b).model;
  stages.push({ stage: name, summaries, selected });
  await writeFile(resolve(dir, 'development-stages.json'), JSON.stringify(stages, null, 2) + '\n');
  console.log(JSON.stringify({ phase: 'selected', stage: name, selected, summaries: summaries.map(s => ({ model: s.model, wis: s.wis, deficit: s.deficit, numericalFailures: s.numericalFailures })) }));
  return selected;
}
const change = (s: string, i: number, v: string) => s.split('.').map((x, j) => j === i ? v : x).join('.');
let selected = await stage('1-poll-link', [base, change(base, 0, 'aggregate')]);
selected = await stage('2-election-kind', [selected, change(selected, 1, 'adjusted')]);
selected = await stage('3-national-scale', [selected, change(selected, 2, 'outcome')]);
const common = selected, outcome = change(common, 3, 'outcome'), municipality = change(common, 3, 'municipality');
selected = await stage('4-local-scale', [common, outcome, municipality]);
const localStage = stages.at(-1)!, incumbent = localStage.summaries[0];
if (better(localStage.summaries[1], incumbent) && better(localStage.summaries[2], incumbent)) selected = await stage('5-separable-local-scale', [selected, change(common, 3, 'separable')]);
else stages.push({ stage: '5-not-triggered', summaries: [], selected });
selected = await stage('6-national-recheck', [selected, change(selected, 2, selected.split('.')[2] === 'common' ? 'outcome' : 'common')]);
const simpler = selected, regional = change(simpler, 4, 'regional'), ou = change(simpler, 5, 'ou');
selected = await stage('7-regional-and-reversion', [simpler, regional, ou]);
const frozen = { runVersion, frozenAt: new Date().toISOString(), selected, simpler, base, regional, ou, stages };
const freezePath = resolve(dir, 'frozen-selection.json');
try {
  const previous = JSON.parse(await readFile(freezePath, 'utf8'));
  if (previous.selected !== selected) throw new Error('Previously frozen selection differs');
} catch (e) { if ((e as NodeJS.ErrnoException).code !== 'ENOENT') throw e; await writeFile(freezePath, JSON.stringify(frozen, null, 2) + '\n'); }
// No confirmation metrics have been read above this line.
const finalModels = [...new Set([base, simpler, regional, ou, selected])];
await run(['current'], dev, DRIFT_POLICY.screeningDraws);
await run(['current', ...finalModels], confirm, DRIFT_POLICY.finalDraws);
const confirmation = await Promise.all(['current', ...finalModels].map(m => summarize(m, confirm, DRIFT_POLICY.finalDraws)));
const currentDevelopment = await summarize('current', dev, DRIFT_POLICY.screeningDraws);
// Restricted sensitivities, no new variant selection.
await run([...new Set([base, selected])], [...dev, ...confirm], DRIFT_POLICY.screeningDraws, ['--panel=six', '--horizon=15']);
const six = await Promise.all([...new Set([base, selected])].map(m => summarize(m, [...dev, ...confirm], DRIFT_POLICY.screeningDraws, ['--panel=six', '--horizon=15'])));
await run([selected], ['camera-2018', 'camera-2022'], DRIFT_POLICY.screeningDraws, ['--history=camera']);
const cameraOnly = await summarize(selected, ['camera-2018', 'camera-2022'], DRIFT_POLICY.screeningDraws, ['--history=camera']);
const scaleSensitivity = [];
for (const scale of [.85, 1.15]) {
  const extra = ['--localScale=' + scale, '--horizon=15'];
  await run([selected], confirm, DRIFT_POLICY.screeningDraws, extra);
  scaleSensitivity.push({ scale, summary: await summarize(selected, confirm, DRIFT_POLICY.screeningDraws, extra) });
}
const report = { ...frozen, finalRunVersion: await version(DRIFT_POLICY.finalDraws), currentDevelopment, confirmation, six, cameraOnly, scaleSensitivity,
  artifactVersions: Object.fromEntries(versions), completedAt: new Date().toISOString() };
await writeFile(resolve(dir, 'comparison.json'), JSON.stringify(report, null, 2) + '\n');
console.log(JSON.stringify({ phase: 'complete', selected, path: resolve(dir, 'comparison.json') }));
