import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { aggregateScores } from './generation-metrics';
import { evaluateSixCase, fitSixModel, prepareSixCategoryStudy, SIX_PARTIES, SIX_POLICY } from './generation-six-category';
import { sha256, writeVersion } from './sources';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const command = process.argv[2], options = process.argv.slice(3);
if (!['check', 'generate'].includes(command) || options.some(o => !/^--(draws|target)=/.test(o))) throw new Error('Usage: generation-six-category-cli check|generate [--draws=256] [--target=camera-2018]');
const option = (name: string) => options.find(o => o.startsWith(`--${name}=`))?.split('=')[1];
const draws = Number(option('draws') ?? SIX_POLICY.draws);
if (!Number.isSafeInteger(draws) || draws < 16 || draws > 8192) throw new Error('Six-category draws must be 16..8192');
const prepared = await prepareSixCategoryStudy(root);
const code = ['generation-six-category.ts', 'generation-six-category-cli.ts', 'generation-history.ts', 'generation-metrics.ts'];
const sourceVersions = [...prepared.sourceVersions,
  ...await Promise.all(code.map(async name => `${name}:${sha256(await readFile(resolve(root, 'web/scripts/data-preparation', name)))}`)),
  `runtime:${sha256(await readFile(resolve(root, 'web/src/lib/politics/reform-2026/paired-vote-generation.ts')))}`];
const runVersion = `camera-six-category-v1-${sha256(JSON.stringify({ sourceVersions, draws, policy: SIX_POLICY })).slice(0, 12)}`;
const models = ['current', 'joint-gaussian', 'joint-student-t'] as const;
const folds = [];
for (const c of prepared.cases) {
  if (option('target') && c.event.id !== option('target')) continue;
  const fit = fitSixModel(c, prepared.cases);
  const metadata = { targetId: c.event.id, date: c.event.date, cutoff: c.cutoff, baselineId: c.baseline.id,
    pollIds: c.pollIds, point: c.point, validPoint: c.validPoint, trainingIds: fit.trainingIds,
    priorOnly: fit.trainingIds.length === 0, predictiveDf: fit.df };
  if (command === 'check') { console.log(JSON.stringify(metadata)); continue; }
  const path = resolve(root, `data/derived/generation/${runVersion}/${c.event.id}.json`);
  let results: ReturnType<typeof evaluateSixCase>[];
  try {
    const cached = JSON.parse(await readFile(path, 'utf8'));
    if (cached.runVersion !== runVersion || JSON.stringify(cached.metadata) !== JSON.stringify(metadata)) throw new Error('Six-category cached metadata mismatch');
    results = cached.results;
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== 'ENOENT') throw error;
    results = [];
    for (const model of models) {
      const start = performance.now(); results.push(evaluateSixCase(c, fit, model, draws));
      console.log(JSON.stringify({ target: c.event.id, model, seconds: Math.round((performance.now() - start) / 100) / 10,
        nationalCoverage90: results.at(-1)!.national.coverage90, municipalCoverage90: results.at(-1)!.municipal.coverage90 }));
    }
    await writeVersion(path, { runVersion, metadata, results });
  }
  folds.push({ ...metadata, results });
}
if (command === 'check') { console.log(JSON.stringify({ runVersion, events: prepared.events.length, draws })); process.exit(0); }
const scored = folds.filter(f => !f.priorOnly);
const metrics = models.map(model => {
  const rs = scored.map(f => f.results.find(r => r.model === model)!);
  return { model, scoredElections: rs.length,
    ...(rs.length ? { national: aggregateScores(rs.map(r => r.national)), regional: aggregateScores(rs.map(r => r.regional)),
      municipal: aggregateScores(rs.map(r => r.municipal)), municipalVoterWeighted: aggregateScores(rs.map(r => r.municipalVoterWeighted)),
      nationalEnergy: rs.reduce((n, r) => n + r.nationalEnergy / rs.length, 0),
      nationalJointCoverage90: rs.reduce((n, r) => n + r.nationalJointCoverage90 / rs.length, 0),
      invalid: rs.reduce((n, r) => n + r.plausibility.invalid, 0),
      extremeNationalDrawRate: rs.reduce((n, r) => n + r.plausibility.extremeNationalDrawRate / rs.length, 0),
      extremeMunicipalRate: rs.reduce((n, r) => n + r.plausibility.extremeMunicipalRate / rs.length, 0),
      roundedNationalMarginError: Math.max(...rs.map(r => r.plausibility.roundedNationalMarginError ?? 0)),
      municipalByParty: SIX_PARTIES.map((party, j) => ({ party, scores: aggregateScores(rs.map(r => r.municipalByParty[j].scores)) })),
      validBallotRate: { national: aggregateScores(rs.map(r => r.validBallotRate.national)), municipal: aggregateScores(rs.map(r => r.validBallotRate.municipal)) } } : {}) };
});
const report = { schemaVersion: 1, kind: 'camera-six-category-major-party-sensitivity', runVersion,
  policy: { ...SIX_POLICY, draws }, sourceVersions, historyVersion: prepared.historyVersion,
  correspondenceDefaults: prepared.correspondenceDefaults, sourceAudits: prepared.sourceAudits, folds, metrics,
  assumptions: [
    'Research only: the active generator, scenario editor and statistical profiles are unchanged.',
    'Six categories separately expose PD, M5S, Lega, FdI and FI/PdL; other remains a changing complement. This is a major-party sensitivity, not all-list validation.',
    'Only terminal Camera forecasts are evaluated: one complete six-category poll in 2013, three each in 2018 and 2022; no full European or long-horizon claim follows.',
    'Camera2013 is a prior-only audit; aggregated scored results contain only Camera2018 and Camera2022. Covariance learns only earlier whole-election blocks available before each cutoff.',
    'FI and PdL form a declared predecessor-family research category, never an assertion of equal voters. Before FdI has observed votes, the explicitly uncertain geographic donor is PdL; M5S uses a uniform emerging-party proxy.',
    'Positive-support floor is 0.5 percent per category before normalization. It is a computational prior, not invented recorded historical votes.',
    'Five ILR coordinates plus valid-ballot logit are modeled jointly. IW prior df=d+2 yields Student predictive df=n+3; Gaussian and Student-t have identical learned covariance.',
    'Regional and municipal covariance are pooled within each earlier election and weighted once per event; thousands of municipality observations do not create thousands of independent elections.',
    'Both candidates use the same central national polling reference as current runtime. Geographic and valid-rate draws are reconciled afterward, followed by largest-remainder municipal integer rounding.',
    'Every current-model draw calls the untouched production generatePairedMunicipalVotes implementation with six IDs, source-based municipal profiles and current measurement sigmas.',
    'A fixed historical electorate is forecast. National and regional scoring uses complete observed source domains; local scores disclose matched identities and retained valid-vote mass.',
    'Valid-ballot rate is validVotes/electors; it combines turnout and invalid ballots, not separate modeled voter turnout. Municipal source anomalies are retained nationally and audited outside local fitting.',
    'No held-out target vote is used to choose a donor, split a list, fit covariance, set national point, or prepare geographic profiles.'
  ] };
const reportName = `${runVersion}${option('target') ? `-${option('target')}` : ''}.json`;
await writeVersion(resolve(root, 'data/derived/generation', reportName), report);
console.log(JSON.stringify({ runVersion, report: `data/derived/generation/${reportName}`, metrics }, null, 2));
