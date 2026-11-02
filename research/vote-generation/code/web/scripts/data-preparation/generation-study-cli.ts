import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { loadGenerationHistory } from './generation-history';
import { loadPollCalibrationCases, weightedPollPoint } from './poll-calibration';
import { createBaselineGenerator } from './generation-baseline';
import { aggregateScores, scoreVectorEnergy, summarizeScalarSamples, type GenerationScores } from './generation-metrics';
import { GENERATION_STUDY_POLICY, fitGenerationModel, makeModelGenerator, prepareProfiles, reconcileMargins,
  selectForecastInput, type ResearchEvent, type ResearchPoll, type ForecastInput, type GenerationModel } from './generation-models';
import { inverseIlr } from './poll-uncertainty';
import { logistic, logit } from '../../src/lib/politics/reform-2026/generation-math';
import { sha256, writeVersion } from './sources';
import { estimatePollReference } from '../../src/lib/polls/forecast';
import type { NormalizedPollQuestion, PollEntityCatalog } from '../../src/lib/polls/types';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const command = process.argv[2], options = process.argv.slice(3);
if (!['check', 'generate'].includes(command) || options.some(o => !/^--(draws|target|horizon|regime)=/.test(o))) throw new Error('Usage: generation-study-cli check|generate [--draws=256] [--target=id] [--horizon=days] [--regime=camera-only|mixed]');
const option = (name: string) => options.find(o => o.startsWith(`--${name}=`))?.split('=').slice(1).join('=');
const draws = Number(option('draws') ?? GENERATION_STUDY_POLICY.draws);
if (!Number.isSafeInteger(draws) || draws < 16 || draws > 8192) throw new Error('Draws must be an integer from 16 to 8192');
const history = await loadGenerationHistory(root);
const terminal = await loadPollCalibrationCases(root);
const earlyPath = resolve(root, 'data/sources/polls/history/generation-horizon-observations.json');
const earlyBytes = await readFile(earlyPath);
const earlyManifestBytes = await readFile(resolve(root, 'data/sources/polls/history/generation-horizon-manifest.json'));
const earlyManifest = JSON.parse(earlyManifestBytes.toString('utf8'));
if (earlyManifest.observationsSha256 !== sha256(earlyBytes)) throw new Error('Early polling manifest checksum mismatch');
const early: { schemaVersion: number; observations: ResearchPoll[] } = JSON.parse(earlyBytes.toString('utf8'));
if (early.schemaVersion !== 1 || !early.observations.length || new Set(early.observations.map(p => p.id)).size !== early.observations.length) throw new Error('Invalid early-horizon archive');
for (const p of early.observations) if (!p.sourceUrl.startsWith('https://') || p.shares.length !== 4 ||
  p.shares.some(v => !Number.isFinite(v) || v < 0) || Math.abs(p.shares.reduce((a, b) => a + b, 0) - 1) > 1e-8 ||
  !Number.isFinite(Date.parse(p.publicationDate)) || !Number.isFinite(Date.parse(p.fieldworkEnd)) ||
  p.fieldworkEnd > p.publicationDate || !Number.isFinite(p.sampleSize) || p.sampleSize <= 0) throw new Error(`Invalid early poll ${p.id}`);
const polls: ResearchPoll[] = [...early.observations, ...terminal.cases.map(c => ({
  id: `terminal-aggregate:${c.id}`, publicationDate: c.cutoff,
  fieldworkEnd: c.questions.map(q => q.source.fieldworkEnd).sort().at(-1)!, kind: c.kind,
  shares: weightedPollPoint(c), sampleSize: 2000, sourceUrl: 'https://www.sondaggipoliticoelettorali.it/', targetElectionId: c.id
}))];
// Impossible source denominators stay in source national totals and the audit;
// they never become fabricated municipal probabilities or future predictors.
const events: ResearchEvent[] = history.events.map(e => {
  // Camera's source omits the Valle d'Aosta special ballot. Use one shared
  // geographic domain for both kinds rather than inventing missing forecasts.
  const domain = e.municipalities.filter(m => m.regionId !== '02');
  const sumField = (key: 'electors' | 'validVotes') => domain.reduce((n, m) => n + m[key], 0);
  return { ...e, date: terminal.cases.find(c => c.id === e.id)?.date ?? e.date,
    national: { electors: sumField('electors'), validVotes: sumField('validVotes'),
      votes: [0, 1, 2, 3].map(j => domain.reduce((n, m) => n + m.votes[j], 0)) },
    municipalities: domain.filter(m => m.matchable && m.validVotes > 0 && m.electors >= m.validVotes) };
});
const codeFiles = ['generation-study-cli.ts', 'generation-models.ts', 'generation-history.ts', 'generation-baseline.ts', 'generation-metrics.ts'];
const codeVersions = await Promise.all(codeFiles.map(async name => `${name}:${sha256(await readFile(resolve(root, 'web/scripts/data-preparation', name)))}`));
const sourceVersions = [...history.sourceVersions, ...terminal.sourceVersions, `early-polls:${sha256(earlyBytes)}`,
  `early-polls-manifest:${sha256(earlyManifestBytes)}`, ...codeVersions];
const runVersion = `municipal-generation-v1-${sha256(JSON.stringify({ sourceVersions, policy: GENERATION_STUDY_POLICY, draws })).slice(0, 12)}`;
const models = ['current', 'joint-gaussian', 'joint-student-t', 'transition-bootstrap'] as const;
const sum = (xs: readonly number[]) => xs.reduce((a, b) => a + b, 0);
const average = (xs: number[]) => sum(xs) / Math.max(1, xs.length);
const catalog: PollEntityCatalog = { schemaVersion: 1, version: 'generation-comparison-panel-v1',
  entities: GENERATION_STUDY_POLICY.categories.map(id => ({ id, name: id, aliases: [], components: [], proxyEntityIds: [], proxyNote: null })) };
function baselineMeasurementSigma(input: ForecastInput) {
  const original = terminal.cases.find(c => input.pollIds.includes(`terminal-aggregate:${c.id}`));
  const collapse = (q: NormalizedPollQuestion): NormalizedPollQuestion => {
    const core = GENERATION_STUDY_POLICY.categories.slice(0, 3).map(id => {
      const rows = q.support.filter(r => r.entityIds.includes(id));
      if (rows.length !== 1 || rows[0].entityIds.length !== 1) throw new Error(`Nonseparable baseline polling category ${q.id}:${id}`);
      return { ...rows[0], label: id, entityIds: [id] };
    });
    return { ...q, support: [...core, { label: 'other', entityIds: ['other'], kind: 'list', share: 1 - sum(core.map(r => r.share)) }] };
  };
  const questions = original ? original.questions.map(collapse) : input.polls.map(p => ({
    id: p.id, sourceId: p.id, questionNumber: 1, pages: [], text: 'Reviewed national Camera vote intentions', notes: [],
    denominator: { kind: 'declared-vote-intentions', basis: 'reported', note: 'Primary source table' },
    reportedTotal: 100, normalization: { method: 'reported-total', factor: 1 },
    independenceGroup: p.id, questionSampleSize: null, answers: [], ballot: { kind: 'generic', label: null, assumptions: [] },
    source: { id: p.id, reference: p.sourceUrl, url: p.sourceUrl, retrieval: 'reviewed-json', capturedOn: '2026-10-23',
      title: 'National Camera vote intentions', commissioner: 'source methodology', purchaser: 'source methodology',
      fieldworkStart: p.fieldworkEnd, publicationDate: p.publicationDate, fieldworkEnd: p.fieldworkEnd,
      sampleSize: p.sampleSize, pollster: 'Demos & Pi', population: 'source methodology', territory: 'Italy',
      sampleDescription: 'source methodology', samplingDescription: 'source methodology', collectionModes: [], marginOfError: null, confidenceLevel: null },
    support: GENERATION_STUDY_POLICY.categories.map((id, j) => ({ label: id, entityIds: [id], kind: 'list', share: p.shares[j] }))
  } satisfies NormalizedPollQuestion));
  if (!questions.length) return [0, 0, 0, 0];
  return estimatePollReference(questions, catalog, { lists: GENERATION_STUDY_POLICY.categories.map((id, j) => ({ id, name: id, baselineShare: input.point[j] })),
    historicalLists: [], correspondences: [] }, input.cutoff).rows.map(r => r.sigma);
}

function baselineInput(input: ForecastInput) {
  const profiles = prepareProfiles(input, false), municipalities = input.baseline.municipalities;
  const shares = new Float64Array(municipalities.length * 4), weights = new Float64Array(municipalities.length);
  // Same central reference and turnout anchor for every model.
  let low = -40, high = 40;
  const electorate = sum(municipalities.map(m => m.electors));
  for (let k = 0; k < 50; k++) { const mid = (low + high) / 2;
    const mass = sum(municipalities.map((m, i) => m.electors * logistic(profiles[i][3] + mid))) / electorate;
    if (mass < input.validPoint) low = mid; else high = mid;
  }
  const shift = (low + high) / 2;
  profiles.forEach((z, i) => { inverseIlr(z.slice(0, 3)).forEach((v, j) => shares[4 * i + j] = v);
    weights[i] = municipalities[i].electors * logistic(z[3] + shift); });
  reconcileMargins(shares, weights, input.point);
  return { electionDate: input.date, cutoff: input.pointDate, baselineDate: input.baseline.date,
    municipalities: municipalities.map((m, i) => ({ id: m.id, regionId: m.regionId, electors: m.electors,
      shares: Array.from(shares.slice(4 * i, 4 * i + 4)), validFraction: weights[i] / m.electors })),
    nationalPoint: input.point, pollSigma: baselineMeasurementSigma(input),
    seed: `${GENERATION_STUDY_POLICY.version}:${input.targetId}:${input.horizon}` };
}
/** Largest-remainder statistical rounding, separate from legal allocation.
 * Every municipality sums to its forecast electorate. National discrepancy is
 * reported explicitly; row and national rounding have different tolerances. */
function roundDraw(output: Float64Array, municipalities: ResearchEvent['municipalities']) {
  for (let i = 0; i < municipalities.length; i++) {
    const n = Math.round(municipalities[i].electors * output[5 * i + 4]);
    const raw = [0, 1, 2, 3].map(j => n * output[5 * i + j]), counts = raw.map(Math.floor);
    const order = [0, 1, 2, 3].sort((a, b) => raw[b] - counts[b] - (raw[a] - counts[a]) || a - b);
    const remaining = n - sum(counts);
    for (let k = 0; k < remaining; k++) counts[order[k]]++;
    for (let j = 0; j < 4; j++) output[5 * i + j] = n ? counts[j] / n : 0;
    output[5 * i + 4] = n / municipalities[i].electors;
  }
}
function evaluate(input: ForecastInput, target: ResearchEvent, model: typeof models[number], fit: ReturnType<typeof fitGenerationModel>) {
  const municipalities = input.baseline.municipalities, targetMap = new Map(target.municipalities.map(m => [m.id, m]));
  const regions = [...new Set([...municipalities, ...target.municipalities].map(m => m.regionId))].sort();
  const regionIndex = new Map(regions.map((id, i) => [id, i]));
  const electors = sum(municipalities.map(m => m.electors));
  const national: number[][] = [], regional: number[][] = [], samples: Float64Array[] = [];
  let extremeNation = 0, extremeLocal = 0, testedLocal = 0, invalid = 0, maxMarginError = 0, maxContinuousMarginError = 0;
  const current = model === 'current' ? createBaselineGenerator(baselineInput(input)) : null;
  const candidate = model === 'current' ? null : makeModelGenerator(input, fit, model as GenerationModel, GENERATION_STUDY_POLICY.version);
  for (let draw = 0; draw < draws; draw++) {
    const generated = candidate?.(draw), output = current ? current(draw + 1) : generated!.output;
    if (generated) { maxContinuousMarginError = Math.max(maxContinuousMarginError, generated.margin.error); roundDraw(output, municipalities); }
    const counts = Array(4).fill(0), regionalCounts = regions.map(() => ({ counts: Array(4).fill(0), valid: 0, electors: 0 }));
    let valid = 0;
    for (let i = 0; i < municipalities.length; i++) {
      const m = municipalities[i], amount = m.electors * output[5 * i + 4], r = regionalCounts[regionIndex.get(m.regionId)!];
      let mass = 0;
      for (let j = 0; j < 4; j++) {
        const share = output[5 * i + j], votes = amount * share; mass += share;
        invalid += Number(!Number.isFinite(share) || share < 0 || share > 1 || Math.abs(votes - Math.round(votes)) > 1e-5);
        counts[j] += votes; r.counts[j] += votes;
      }
      invalid += Number(!Number.isFinite(amount) || amount < 0 || amount > m.electors || amount > 0 && Math.abs(mass - 1) > 1e-8);
      if (amount >= 500) { testedLocal++; extremeLocal += Number(Math.max(...Array.from(output.slice(5 * i, 5 * i + 4))) > .95); }
      valid += amount; r.valid += amount; r.electors += m.electors;
    }
    const nat = [...counts.map(v => v / valid), valid / electors];
    invalid += Number(!valid || nat.some(v => !Number.isFinite(v) || v < 0 || v > 1));
    if (generated) maxMarginError = Math.max(maxMarginError, ...nat.slice(0, 4).map((v, j) => Math.abs(v - generated.national[j])));
    extremeNation += Number(Math.max(...nat.slice(0, 4)) > .8);
    national.push(nat); regional.push(regionalCounts.flatMap(r => [...r.counts.map(v => r.valid ? v / r.valid : 0), r.electors ? r.valid / r.electors : 0])); samples.push(output);
  }
  const nationalTruth = [...target.national.votes.map(v => v / target.national.validVotes), target.national.validVotes / target.national.electors];
  const nationalRows = nationalTruth.map((truth, j) => ({ category: [...GENERATION_STUDY_POLICY.categories, 'valid-ballot-rate'][j], truth,
    ...summarizeScalarSamples(national.map(row => row[j]), truth) }));
  const municipalScores = [], municipalWeights = [], municipalValidScores = [], municipalValidWeights = [];
  let matchedValid = 0, matched = 0, trueExtremeLocal = 0, trueTestedLocal = 0, municipalJointCovered = 0;
  for (let i = 0; i < municipalities.length; i++) {
    const truth = targetMap.get(municipalities[i].id); if (!truth) continue;
    matched++; matchedValid += truth.validVotes;
    const ps = truth.votes.map(v => v / truth.validVotes);
    const scored = ps.map((p, j) => summarizeScalarSamples(samples.map(s => s[5 * i + j]), p));
    municipalJointCovered += Number(scored.every(s => s.coverage90));
    municipalScores.push(...scored); municipalWeights.push(...ps.map(() => truth.validVotes));
    municipalValidScores.push(summarizeScalarSamples(samples.map(s => s[5 * i + 4]), truth.validVotes / truth.electors)); municipalValidWeights.push(truth.electors);
    if (truth.validVotes >= 500) { trueTestedLocal++; trueExtremeLocal += Number(Math.max(...ps) > .95); }
  }
  const regionalScores = [], regionalWeights = [], regionalValid = [];
  for (let i = 0; i < regions.length; i++) {
    const group = target.municipalities.filter(m => m.regionId === regions[i]);
    const valid = sum(group.map(m => m.validVotes)), el = sum(group.map(m => m.electors)); if (!valid || !el) continue;
    for (let j = 0; j < 4; j++) { const truth = sum(group.map(m => m.votes[j])) / valid;
      regionalScores.push(summarizeScalarSamples(regional.map(s => s[5 * i + j]), truth)); regionalWeights.push(valid); }
    regionalValid.push(summarizeScalarSamples(regional.map(s => s[5 * i + 4]), valid / el));
  }
  return { model, national: aggregateScores(nationalRows.slice(0, 4)), nationalRows,
    nationalJointCoverage90: Number(nationalRows.slice(0, 4).every(r => r.coverage90)),
    nationalEnergy: scoreVectorEnergy(national.map(s => s.slice(0, 4)), nationalTruth.slice(0, 4)),
    regional: aggregateScores(regionalScores), regionalVoterWeighted: aggregateScores(regionalScores, regionalWeights),
    municipal: aggregateScores(municipalScores), municipalVoterWeighted: aggregateScores(municipalScores, municipalWeights),
    municipalJointCoverage90: municipalJointCovered / matched,
    validBallotRate: { national: aggregateScores(nationalRows.slice(4)), regional: aggregateScores(regionalValid),
      municipal: aggregateScores(municipalValidScores), municipalElectorWeighted: aggregateScores(municipalValidScores, municipalValidWeights) },
    plausibility: { invalid, extremeNationalDrawRate: extremeNation / draws,
      extremeMunicipalRate: extremeLocal / testedLocal, actualExtremeNational: Number(Math.max(...nationalTruth.slice(0, 4)) > .8),
      actualExtremeMunicipalRate: trueExtremeLocal / trueTestedLocal, maxContinuousMarginError,
      maxRoundedNationalMarginError: model === 'current' ? null : maxMarginError },
    geography: { forecastMunicipalities: municipalities.length, matchedMunicipalities: matched,
      matchedTargetValidVoteFraction: matchedValid / target.national.validVotes,
      forecastElectors: electors, actualElectors: target.national.electors } };
}
type Fold = { targetId: string; kind: ResearchEvent['kind']; date: string; cutoff: string; horizon: number; regime: string;
  baselineId: string; pollIds: string[]; trainingIds: string[]; point: number[]; pointDate: string; validPoint: number;
  results: ReturnType<typeof evaluate>[] };
const folds: Fold[] = [], unavailable: unknown[] = [];
for (const regime of ['camera-only', 'mixed'] as const) for (const target of events.filter(e => e.date >= '2013-01-01')) {
  if (regime === 'camera-only' && target.kind !== 'camera' || option('regime') && regime !== option('regime') || option('target') && target.id !== option('target')) continue;
  for (const horizon of GENERATION_STUDY_POLICY.horizons) {
    if (option('horizon') && String(horizon) !== option('horizon')) continue;
    const input = selectForecastInput(target, events, polls, horizon, regime);
    if (!input) { unavailable.push({ targetId: target.id, horizon, regime, reason: 'No historical result available before cutoff' }); continue; }
    const fit = fitGenerationModel(input, events, polls, regime);
    const metadata = { targetId: target.id, kind: target.kind, date: target.date, cutoff: input.cutoff, horizon, regime,
      baselineId: input.baseline.id, pollIds: input.pollIds, point: input.point, pointDate: input.pointDate,
      validPoint: input.validPoint, trainingIds: fit.trainingIds };
    if (command === 'check') { console.log(JSON.stringify(metadata)); continue; }
    const path = resolve(root, `data/derived/generation/${runVersion}/${regime}-${target.id}-${horizon}.json`);
    let fold: Fold;
    try { const cached = JSON.parse(await readFile(path, 'utf8'));
      if (cached.runVersion !== runVersion || JSON.stringify(cached.metadata) !== JSON.stringify(metadata)) throw new Error('Cached fold metadata mismatch');
      fold = { ...metadata, results: cached.results };
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== 'ENOENT') throw error;
      const results = [];
      for (const model of models) { const start = performance.now(); results.push(evaluate(input, target, model, fit));
        console.log(JSON.stringify({ target: target.id, horizon, regime, model, seconds: Math.round((performance.now() - start) / 100) / 10,
          nationalCoverage90: results.at(-1)!.national.coverage90, municipalCoverage90: results.at(-1)!.municipal.coverage90 })); }
      fold = { ...metadata, results };
      await writeVersion(path, { runVersion, metadata, results });
    }
    folds.push(fold);
  }
}
if (command === 'check') { console.log(JSON.stringify({ runVersion, events: events.length, earlyPolls: early.observations.length, unavailable })); process.exit(0); }
function summarize(fs: Fold[]) {
  return models.map(model => {
    const rs = fs.map(f => f.results.find(r => r.model === model)!);
    const aggregate = (key: 'national' | 'regional' | 'municipal' | 'municipalVoterWeighted') => aggregateScores(rs.map(r => r[key]));
    return { model, elections: new Set(fs.map(f => f.targetId)).size, forecasts: rs.length,
      national: aggregate('national'), regional: aggregate('regional'), municipal: aggregate('municipal'), municipalVoterWeighted: aggregate('municipalVoterWeighted'),
      validBallotRate: { national: aggregateScores(rs.map(r => r.validBallotRate.national)), municipal: aggregateScores(rs.map(r => r.validBallotRate.municipal)) },
      nationalEnergy: average(rs.map(r => r.nationalEnergy)), jointCoverage90: average(rs.map(r => r.nationalJointCoverage90)),
      invalid: sum(rs.map(r => r.plausibility.invalid)), extremeNationalDrawRate: average(rs.map(r => r.plausibility.extremeNationalDrawRate)),
      extremeMunicipalRate: average(rs.map(r => r.plausibility.extremeMunicipalRate)),
      maxRoundedNationalMarginError: Math.max(...rs.map(r => r.plausibility.maxRoundedNationalMarginError ?? 0)) };
  });
}
const groups = ['camera-only', 'mixed'].flatMap(regime => GENERATION_STUDY_POLICY.horizons.flatMap(horizon =>
  ['camera', 'european', 'all'].flatMap(kind => { const fs = folds.filter(f => f.regime === regime && f.horizon === horizon && (kind === 'all' || f.kind === kind));
    return fs.length ? [{ regime, horizon, kind, metrics: summarize(fs) }] : []; })));
const report = { schemaVersion: 1, kind: 'municipal-generation-comparison', runVersion, policy: { ...GENERATION_STUDY_POLICY, draws }, sourceVersions,
  scope: 'PD/M5S/Lega/changing other, domestic Camera/European municipal geography; research only',
  historyVersion: history.version, sourceAudits: history.events.map(e => ({ id: e.id, audit: e.audit, mapping: e.mapping })),
  unmatchedTransitions: history.transitions, unavailable, folds, groups };
const reportPath = resolve(root, `data/derived/generation/${runVersion}/report-${sha256(JSON.stringify(folds.map(f => [f.targetId, f.regime, f.horizon]))).slice(0, 8)}.json`);
await writeVersion(reportPath, report);
console.log(JSON.stringify({ runVersion, reportPath, folds: folds.length,
  groups: groups.map(g => ({ ...g, metrics: g.metrics.map(m => ({ model: m.model, elections: m.elections,
    nationalCoverage90: m.national.coverage90, municipalCoverage90: m.municipal.coverage90,
    nationalMeanMae: m.national.meanMae, municipalMeanMae: m.municipal.meanMae,
    nationalWis: m.national.wis, municipalWis: m.municipal.wis, invalid: m.invalid })) })) }));
