import { readFile, readdir } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { loadDriftInputs, availableInputs } from './drift-data';
import { fitDrift } from './drift-fit';
import { currentDriftComparator, evaluateDriftSamples, evaluateFittedDrift } from './drift-evaluate';
import { makeDriftProblem } from './drift-structure';
import { BASE_DRIFT, DRIFT_POLICY, type DriftVariant } from './drift-structure';
import { sha256, writeVersion } from './sources';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const command = process.argv[2], options = process.argv.slice(3);
const option = (key: string) => options.find(o => o.startsWith('--' + key + '='))?.slice(key.length + 3);
if (!['check', 'fit', 'generate', 'current'].includes(command) || options.some(o => !/^--(model|target|horizon|draws|only|panel|history|localScale)=/.test(o))) throw new Error('Usage: drift-study-cli check|fit|generate|current [--model=id] [--target=id] [--horizon=15] [--draws=128] [--only=development|confirmation|all] [--panel=core|six] [--history=all|camera] [--localScale=1]');
function model(name: string): DriftVariant {
  const [pollLink, kind, national, local, geo, time] = name.split('.');
  if (!['literal', 'aggregate'].includes(pollLink) || !['pooled', 'adjusted'].includes(kind) || !['common', 'outcome'].includes(national) ||
    !['common', 'outcome', 'municipality', 'separable'].includes(local) || !['independent', 'regional'].includes(geo) || !['brownian', 'ou'].includes(time) || geo === 'regional' && time === 'ou') throw new Error('Invalid staged drift variant');
  return { id: name, pollLink: pollLink as DriftVariant['pollLink'], kindEffects: kind === 'adjusted',
    national: national as DriftVariant['national'], local: local as DriftVariant['local'], regional: geo === 'regional', reversion: time === 'ou' };
}
const panel = option('panel') ?? 'core';
if (!['core', 'six'].includes(panel)) throw new Error('Invalid panel');
const variant = model(option('model') ?? 'literal.pooled.common.common.independent.brownian'), input = await loadDriftInputs(root, panel as 'core' | 'six');
const history = option('history') ?? 'all', localScale = Number(option('localScale') ?? 1);
if (!['all', 'camera'].includes(history) || !(localScale >= .5 && localScale <= 2)) throw new Error('Invalid sensitivity');
const allElections = [...input.elections];
if (history === 'camera') { input.elections = input.elections.filter(e => e.kind === 'camera'); input.polls = input.polls.filter(p => p.kind === 'camera'); }
// Pin the entire preparation/runtime source tree, including transitive imports.
const code = (await readdir(resolve(root, 'web'), { recursive: true })).filter(n => /^(scripts|src)[\\/]/.test(n) && /\.ts$/.test(n) && !n.endsWith('.test.ts')).sort();
const versions = [...input.sourceVersions, ...await Promise.all([...code.map(n => 'web/' + n.replaceAll('\\', '/')), 'web/package-lock.json'].map(async path =>
  path + ':' + sha256(await readFile(resolve(root, path)))))];
const draws = Number(option('draws') ?? DRIFT_POLICY.screeningDraws);
if (!Number.isInteger(draws) || draws < 16 || draws > 4096) throw new Error('Invalid draw count');
const runVersion = 'latent-drift-v1-' + sha256(JSON.stringify({ versions, policy: DRIFT_POLICY, draws, panel, history, localScale })).slice(0, 12);
console.log(JSON.stringify({ runVersion, model: variant.id, sources: input.version, audit: { surveys: input.audit.surveys,
  questions: input.audit.questions, excluded: input.audit.excluded }, phase: 'inputs' }));
const targets = allElections.filter(e => e.date >= '2013-01-01' && (!option('target') || e.id === option('target')) &&
  (option('only') === 'confirmation' ? DRIFT_POLICY.confirmationIds.includes(e.id as never) :
    option('only') === 'development' ? DRIFT_POLICY.developmentIds.includes(e.id as never) : true));
for (const target of targets) for (const horizon of DRIFT_POLICY.horizons) {
  if (option('horizon') && String(horizon) !== option('horizon')) continue;
  const cutoff = new Date(Date.parse(target.date) - horizon * 86400000).toISOString().slice(0, 10);
  if (command === 'check') {
    const available = availableInputs(input, cutoff);
    console.log(JSON.stringify({ target: target.id, horizon, cutoff, events: available.elections.map(e => e.id), polls: available.polls.map(p => p.id) })); continue;
  }
  const path = resolve(root, 'data/derived/drift', runVersion, (command === 'current' ? 'current' : variant.id) + '-' + target.id + '-' + horizon + (command === 'fit' ? '-fit' : '') + '.json');
  try { await readFile(path); console.log(JSON.stringify({ phase: 'cached', runVersion, path })); continue; } catch { /* New case. */ }
  const tic = performance.now();
  if (command === 'current') {
    const problem = makeDriftProblem(input, cutoff, target.kind, BASE_DRIFT), comparator = currentDriftComparator(problem, target.date, DRIFT_POLICY.version + ':' + target.id + ':' + horizon);
    const metrics = evaluateDriftSamples(problem, target, draws, comparator.draw);
    await writeVersion(path, { runVersion, panel, targetId: target.id, horizon, cutoff, versions, metrics, current: comparator.audit });
    console.log(JSON.stringify({ phase: 'current-complete', target: target.id, horizon, seconds: (performance.now() - tic) / 1000 })); continue;
  }
  const fit = fitDrift(input, cutoff, target.kind, variant, row => console.log(JSON.stringify({ target: target.id, horizon, model: variant.id, phase: 'laplace', ...row as object })), localScale);
  const summary = { runVersion, panel, history, localScale, variant, targetId: target.id, horizon, cutoff, inputVersion: input.version, versions,
    fit: { parameters: { eta: fit.parameters.eta, delta: fit.parameters.delta, poll: fit.parameters.poll,
      etaShape: fit.parameters.etaShape, deltaShape: fit.parameters.deltaShape, regional: fit.parameters.regionFraction, kappa: fit.parameters.kappa },
      localAudit: fit.localAudit, iterations: fit.iterations, converged: fit.converged, seconds: fit.seconds,
      cities: fit.problem.cities.length, forecastCities: fit.problem.forecast.length, globalDimension: fit.problem.layout.m,
      electionIds: fit.problem.elections.map(e => e.id), pollIds: fit.problem.polls.map(p => p.id) } };
  if (command === 'fit') {
    console.log(JSON.stringify({ phase: 'fit-complete', ...summary, totalSeconds: (performance.now() - tic) / 1000 }));
    await writeVersion(path, summary);
    continue;
  }
  const metrics = evaluateFittedDrift(fit, target, draws, DRIFT_POLICY.version + ':' + target.id + ':' + horizon);
  await writeVersion(path, { ...summary, draws, metrics, totalSeconds: (performance.now() - tic) / 1000 });
  console.log(JSON.stringify({ phase: 'forecast-complete', target: target.id, horizon, model: variant.id, diagnostics: metrics.posterior, seconds: (performance.now() - tic) / 1000 }));
}
