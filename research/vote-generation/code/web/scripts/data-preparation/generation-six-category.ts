import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { createNormalSampler, createSeededRng } from '../../src/lib/core/rng';
import { logistic, logit } from '../../src/lib/politics/reform-2026/generation-math';
import { generatePairedMunicipalVotes, type ProjectedMunicipality } from '../../src/lib/politics/reform-2026/paired-vote-generation';
import type { HistoricalMunicipality, ReformGenerationScenario, ReformGenerationSnapshot } from '../../src/lib/politics/reform-2026/generation-types';
import { estimatePollReference } from '../../src/lib/polls/forecast';
import type { NormalizedPollQuestion, PollEntityCatalog } from '../../src/lib/polls/types';
import { aggregateScores, scoreVectorEnergy, summarizeScalarSamples } from './generation-metrics';
import { loadGenerationHistory, type GenerationMunicipality } from './generation-history';
import { electionManifest } from './poll-calibration-sources';
import { prepareSelectedPolls } from './polls';
import { sha256 } from './sources';

export const SIX_PARTIES = ['pd', 'm5s', 'lega', 'fdi', 'fi-pdl', 'other'] as const;
export const SIX_POLICY = { version: 'camera-six-category-v1', draws: 256, dimension: 6, emergingPartyMass: .005,
  nationalSigma: .2, nationalAnnualSigma: .35, regionalAnnualSigma: .12, municipalAnnualSigma: .18,
  validRateSigma: .2, priorEventWeight: 1, nationalConcentration: .8, municipalConcentration: .95,
  minimumTailValidVotes: 500 } as const;
const K = SIX_PARTIES.length, D = K, STRIDE = K + 1;
const sum = (xs: readonly number[]) => xs.reduce((a, b) => a + b, 0);
const years = (a: string, b: string) => (Date.parse(a) - Date.parse(b)) / (365.25 * 86400000);
const basis = Array.from({ length: K - 1 }, (_, i) => Array.from({ length: K }, (_, j) =>
  j <= i ? 1 / Math.sqrt((i + 1) * (i + 2)) : j === i + 1 ? -(i + 1) / Math.sqrt((i + 1) * (i + 2)) : 0));

export function sixIlr(p: readonly number[]) {
  if (p.length !== K || p.some(v => !Number.isFinite(v) || v <= 0) || Math.abs(sum(p) - 1) > 1e-8) throw new Error('Six-category ILR requires positive composition');
  return basis.map(b => b.reduce((n, v, j) => n + v * Math.log(p[j]), 0));
}
export function sixInverseIlr(z: readonly number[]) {
  if (z.length !== K - 1 || z.some(v => !Number.isFinite(v))) throw new Error('Invalid six-category ILR coordinate');
  const logs = Array.from({ length: K }, (_, j) => basis.reduce((n, b, i) => n + b[j] * z[i], 0));
  const max = Math.max(...logs), exp = logs.map(v => Math.exp(v - max)), total = sum(exp);
  return exp.map(v => v / total);
}
function positive(votes: readonly number[]) {
  const total = sum(votes);
  if (votes.length !== K || total <= 0 || votes.some(v => !Number.isFinite(v) || v < 0)) throw new Error('Invalid six-category counts');
  const p = votes.map(v => Math.max(v / total, SIX_POLICY.emergingPartyMass));
  return p.map(v => v / sum(p));
}

const names = {
  pd: ['PARTITO DEMOCRATICO', 'PARTITO DEMOCRATICO - ITALIA DEMOCRATICA E PROGRESSISTA'],
  m5s: ['MOVIMENTO 5 STELLE BEPPEGRILLO.IT', 'MOVIMENTO 5 STELLE'],
  lega: ['LEGA NORD', 'LEGA', 'LEGA PER SALVINI PREMIER'],
  fdi: ["FRATELLI D'ITALIA", "FRATELLI D'ITALIA CON GIORGIA MELONI"],
  'fi-pdl': ["IL POPOLO DELLA LIBERTA'", 'FORZA ITALIA']
};
export function sixListCategory(name: string) {
  return (Object.keys(names) as (keyof typeof names)[]).find(id => names[id].includes(name)) ?? 'other';
}
export interface SixMunicipality { id: string; regionId: string; electors: number; validVotes: number; votes: number[]; }
export interface SixEvent { id: string; date: string; municipalities: SixMunicipality[];
  national: { electors: number; validVotes: number; votes: number[] }; }
export interface SixCase { event: SixEvent; baseline: SixEvent; cutoff: string; point: number[]; sigma: number[]; pollIds: string[]; validPoint: number; }
type Matrix = number[][];
const matrix = () => Array.from({ length: D }, () => Array(D).fill(0) as number[]);
function outer(m: Matrix, v: number[], weight: number) {
  for (let i = 0; i < D; i++) for (let j = 0; j < D; j++) m[i][j] += v[i] * v[j] * weight;
}
function cholesky(m: Matrix) {
  const l = matrix();
  for (let i = 0; i < D; i++) for (let j = 0; j <= i; j++) {
    let value = m[i][j]; for (let h = 0; h < j; h++) value -= l[i][h] * l[j][h];
    if (i === j && value <= 0) throw new Error('Nonpositive six-category covariance');
    l[i][j] = i === j ? Math.sqrt(value) : value / l[j][j];
  }
  return l;
}
const position = (e: SixEvent) => [...sixIlr(positive(e.national.votes)), logit(e.national.validVotes / e.national.electors)];

/** Before its first ballot FdI has no measured municipal counts. PdL is an
 * explicit donor geography; M5S uses a national uniform emerging-party proxy.
 * These priors never modify the stored historical source votes. */
function municipalPosition(event: SixEvent, m: SixMunicipality) {
  const p = positive(m.votes);
  if (event.national.votes[3] === 0) p[3] = SIX_POLICY.emergingPartyMass * (m.votes[4] / m.validVotes) / Math.max(.005, event.national.votes[4] / event.national.validVotes);
  const normalized = p.map(v => Math.max(v, 1e-8)); const total = sum(normalized);
  return [...sixIlr(normalized.map(v => v / total)), logit(m.validVotes / m.electors)];
}

export async function prepareSixCategoryStudy(root: string) {
  const history = await loadGenerationHistory(root), manifest = await electionManifest(root);
  const bytes = await readFile(resolve(root, 'data/sources/polls/history/generation-six-category-observations.json'));
  const observations: { schemaVersion: number; panel: string[]; inputPins: { path: string; sha256: string }[];
    observations: { id: string; electionId: string; shares: number[]; publicationDate: string; fieldworkEnd: string }[] } = JSON.parse(bytes.toString('utf8'));
  if (observations.schemaVersion !== 1 || observations.panel.join(',') !== SIX_PARTIES.join(',') || !observations.observations.length) throw new Error('Invalid six-category poll observations');
  for (const pin of observations.inputPins) {
    if (!['archive.json', 'manifest.json', 'elections-manifest.json', 'generation-horizon-observations.json', 'generation-horizon-manifest.json'].includes(pin.path) ||
      sha256(await readFile(resolve(root, 'data/sources/polls/history', pin.path))) !== pin.sha256) throw new Error('Six-category input pin mismatch');
  }
  const archive = await prepareSelectedPolls(root, resolve(root, 'data/sources/polls/history'));
  const convert = (m: GenerationMunicipality): SixMunicipality => ({ id: m.id, regionId: m.regionId, electors: m.electors, validVotes: m.validVotes,
    votes: SIX_PARTIES.map(id => sum(m.lists.filter(l => sixListCategory(l.name) === id).map(l => l.votes))) });
  const events: SixEvent[] = history.events.filter(e => e.kind === 'camera').map(e => ({ id: e.id, date: e.date,
    municipalities: e.municipalities.filter(m => m.matchable && m.validVotes > 0 && m.validVotes <= m.electors).map(convert),
    national: { electors: e.national.electors, validVotes: e.national.validVotes,
      votes: SIX_PARTIES.map(id => sum(e.municipalities.flatMap(m => m.lists).filter(l => sixListCategory(l.name) === id).map(l => l.votes))) } }));
  const catalog: PollEntityCatalog = { schemaVersion: 1, version: 'six-category-terminal-v1', entities: SIX_PARTIES.map(id =>
    ({ id, name: id, aliases: [], components: [], proxyEntityIds: [], proxyNote: null })) };
  const collapse = (q: NormalizedPollQuestion): NormalizedPollQuestion => {
    const core = SIX_PARTIES.slice(0, -1).map(id => {
      const values = q.support.filter(s => s.entityIds.length === 1 && (id === 'fi-pdl' ? ['fi', 'pdl'].includes(s.entityIds[0]) : s.entityIds[0] === id));
      if (values.length !== 1) throw new Error(`Unobserved six-category poll option ${q.id}:${id}`);
      return { ...values[0], entityIds: [id], label: id };
    });
    return { ...q, support: [...core, { label: 'other', entityIds: ['other'], kind: 'list', share: 1 - sum(core.map(r => r.share)) }] };
  };
  const cases: SixCase[] = events.slice(1).map(event => {
    const election = manifest.elections.find(e => e.id === event.id)!;
    const rows = observations.observations.filter(p => p.electionId === event.id);
    if (!rows.length || rows.some(p => p.publicationDate > election.cutoff || p.fieldworkEnd > election.cutoff || p.shares.length !== K || p.shares.some(v => v < 0 || !Number.isFinite(v)) || Math.abs(sum(p.shares) - 1) > 1e-8)) throw new Error('Missing or post-cutoff six-category polling');
    const questions = rows.map(p => {
      const q = archive.bundle.questions.find(q => q.id === p.id); if (!q) throw new Error(`Missing normalized six-category poll ${p.id}`);
      const collapsed = collapse(q);
      if (collapsed.support.some((r, j) => Math.abs(r.share - p.shares[j]) > 1e-8)) throw new Error(`Six-category transcription mismatch ${p.id}`);
      return collapsed;
    });
    const baseline = events.filter(e => e.date < election.cutoff).at(-1)!;
    const reference = (baselineShare: number[]) => estimatePollReference(questions, catalog,
      { lists: SIX_PARTIES.map((id, i) => ({ id, name: id, baselineShare: baselineShare[i] })), historicalLists: [], correspondences: [] }, election.cutoff);
    const first = reference(positive(baseline.national.votes)), total = sum(first.evidence.map(e => e.weight));
    const point = SIX_PARTIES.map((_, j) => first.evidence.reduce((n, e) => n + e.weight * e.reconciliation.shares[j].share, 0) / total);
    return { event, baseline, cutoff: election.cutoff, point, sigma: reference(point).rows.map(r => r.sigma),
      pollIds: first.evidence.map(e => e.questionId), validPoint: baseline.national.validVotes / baseline.national.electors };
  });
  return { events, cases, historyVersion: history.version,
    sourceVersions: [...history.sourceVersions, `six-polls:${sha256(bytes)}`, `normalized-terminal-polls:${archive.bundle.version}`],
    correspondenceDefaults: names, sourceAudits: history.events.filter(e => e.kind === 'camera').map(e => ({ id: e.id, audit: e.audit })) };
}

export function fitSixModel(c: SixCase, cases: SixCase[]) {
  const past = cases.filter(p => p.event.date < c.cutoff);
  const n = past.length, national = matrix(), regional = matrix(), local = matrix();
  for (let j = 0; j < D; j++) {
    national[j][j] = j < K - 1 ? SIX_POLICY.nationalSigma ** 2 + SIX_POLICY.nationalAnnualSigma ** 2 * years(c.event.date, c.cutoff) : SIX_POLICY.validRateSigma ** 2;
    regional[j][j] = SIX_POLICY.regionalAnnualSigma ** 2;
    local[j][j] = SIX_POLICY.municipalAnnualSigma ** 2;
  }
  for (const p of past) {
    const error = position(p.event).map((v, j) => v - [...sixIlr(p.point), logit(p.validPoint)][j]); outer(national, error, 1);
    const before = new Map(p.baseline.municipalities.map(m => [m.id, m]));
    const shift = position(p.event).map((v, j) => v - position(p.baseline)[j]);
    const rows = p.event.municipalities.flatMap(m => {
      const b = before.get(m.id); if (!b) return [];
      const old = municipalPosition(p.baseline, b), next = municipalPosition(p.event, m);
      return [{ m, error: next.map((v, j) => v - old[j] - shift[j]) }];
    });
    const regions = new Map<string, { mean: number[]; mass: number }>();
    for (const { m, error: e } of rows) {
      const r = regions.get(m.regionId) ?? { mean: Array(D).fill(0), mass: 0 };
      r.mass += m.validVotes; e.forEach((v, j) => r.mean[j] += v * m.validVotes); regions.set(m.regionId, r);
    }
    for (const r of regions.values()) r.mean = r.mean.map(v => v / r.mass);
    const duration = Math.max(.25, years(p.event.date, p.baseline.date));
    for (const r of regions.values()) outer(regional, r.mean, 1 / regions.size / duration);
    for (const { m, error: e } of rows) outer(local, e.map((v, j) => v - regions.get(m.regionId)!.mean[j]), 1 / rows.length / duration);
  }
  const scale = (m: Matrix) => m.map(row => row.map(v => v / (n + 1)));
  return { trainingIds: past.map(p => p.event.id), df: n + 3,
    nationalFactor: cholesky(scale(national)), regionalFactor: cholesky(scale(regional)), localFactor: cholesky(scale(local)) };
}

/** Generic positive-support IPF, after valid-rate and geographic draws. */
export function reconcileSixMargins(shares: Float64Array, weights: Float64Array, target: readonly number[], tolerance = 1e-8) {
  if (target.length !== K || shares.length !== weights.length * K || target.some(v => !Number.isFinite(v) || v < 0) ||
      Math.abs(sum(target) - 1) > 1e-8 || weights.some(v => !Number.isFinite(v) || v < 0) || shares.some(v => !Number.isFinite(v) || v < 0)) throw new Error('Invalid six-category margin input');
  const total = sum([...weights]); if (!total) throw new Error('No six-category valid weight');
  for (let iteration = 0; iteration < 400; iteration++) {
    const observed = Array(K).fill(0) as number[];
    for (let i = 0; i < weights.length; i++) for (let j = 0; j < K; j++) observed[j] += weights[i] * shares[K * i + j];
    const factors = observed.map((v, j) => {
      if (!v && target[j]) throw new Error('Infeasible six-category support');
      return v ? target[j] * total / v : 0;
    });
    for (let i = 0; i < weights.length; i++) {
      let mass = 0; for (let j = 0; j < K; j++) { shares[K * i + j] *= factors[j]; mass += shares[K * i + j]; }
      if (!mass) throw new Error('Six-category empty row');
      for (let j = 0; j < K; j++) shares[K * i + j] /= mass;
    }
    const check = Array(K).fill(0) as number[];
    for (let i = 0; i < weights.length; i++) for (let j = 0; j < K; j++) check[j] += weights[i] * shares[K * i + j] / total;
    const error = Math.max(...check.map((v, j) => Math.abs(v - target[j])));
    if (error < tolerance) return { error, iterations: iteration + 1 };
  }
  throw new Error('Six-category IPF failed to converge');
}

function alignValidRate(logits: number[], municipalities: SixMunicipality[], target: number) {
  let low = -40, high = 40; const electors = sum(municipalities.map(m => m.electors));
  for (let iteration = 0; iteration < 45; iteration++) {
    const mid = (low + high) / 2;
    let mass = 0; for (let i = 0; i < logits.length; i++) mass += municipalities[i].electors * logistic(logits[i] + mid);
    if (mass / electors < target) low = mid; else high = mid;
  }
  return logits.map(v => logistic(v + (low + high) / 2));
}

export function createSixCandidate(c: SixCase, fit: ReturnType<typeof fitSixModel>, model: 'joint-gaussian' | 'joint-student-t', seed: string) {
  const profiles = c.baseline.municipalities.map(m => municipalPosition(c.baseline, m));
  const regions = [...new Set(c.baseline.municipalities.map(m => m.regionId))];
  const centre = [...sixIlr(c.point), logit(c.validPoint)], age = Math.max(.25, years(c.event.date, c.baseline.date));
  return (draw: number) => {
    const normal = createNormalSampler(createSeededRng(`${seed}:${c.event.id}:${draw}`));
    const error = (l: Matrix) => {
      const z = Array.from({ length: D }, () => normal(0, 1));
      const multiplier = model === 'joint-student-t' ? Math.sqrt((fit.df - 2) / sum(Array.from({ length: fit.df }, () => normal(0, 1) ** 2))) : 1;
      return l.map(row => row.reduce((n, v, j) => n + v * z[j], 0) * multiplier);
    };
    const nationalError = error(fit.nationalFactor), national = sixInverseIlr(centre.slice(0, K - 1).map((v, j) => v + nationalError[j]));
    const nationalValid = logistic(centre[D - 1] + nationalError[D - 1]);
    const regional = new Map(regions.map(id => [id, error(fit.regionalFactor).map(v => v * Math.sqrt(age))]));
    const shares = new Float64Array(c.baseline.municipalities.length * K), logits: number[] = [];
    for (let i = 0; i < c.baseline.municipalities.length; i++) {
      const r = regional.get(c.baseline.municipalities[i].regionId)!, e = error(fit.localFactor).map(v => v * Math.sqrt(age));
      const p = sixInverseIlr(profiles[i].slice(0, K - 1).map((v, j) => v + r[j] + e[j]));
      p.forEach((v, j) => shares[K * i + j] = v); logits.push(profiles[i][D - 1] + r[D - 1] + e[D - 1]);
    }
    const valid = alignValidRate(logits, c.baseline.municipalities, nationalValid);
    const weights = new Float64Array(valid.map((v, i) => v * c.baseline.municipalities[i].electors));
    const margin = reconcileSixMargins(shares, weights, national), output = new Float64Array(valid.length * STRIDE);
    for (let i = 0; i < valid.length; i++) { for (let j = 0; j < K; j++) output[STRIDE * i + j] = shares[K * i + j]; output[STRIDE * i + K] = valid[i]; }
    return { output, national, nationalValid, margin };
  };
}

/** Calls the actual untouched runtime generator, with six list IDs. */
export function createSixCurrent(c: SixCase, seed: string) {
  const scenario = { renewal: 'camera', chamberProfileId: 'central-v1', seed, electionDate: c.event.date } as ReformGenerationScenario;
  const snapshot = { historicalDate: c.baseline.date, models: { chambers: {
    sigmaPerSqrtDay: { national: .02, region: .01, local: .01 },
    profiles: [{ id: 'central-v1', sharedVariance: .5, residualCorrelation: .7, gapMultiplier: 1 }] } } } as ReformGenerationSnapshot;
  const profiles = c.baseline.municipalities.map(m => municipalPosition(c.baseline, m));
  const valid = alignValidRate(profiles.map(p => p[D - 1]), c.baseline.municipalities, c.validPoint);
  const shares = new Float64Array(valid.length * K), weights = new Float64Array(valid.map((v, i) => v * c.baseline.municipalities[i].electors));
  profiles.forEach((p, i) => sixInverseIlr(p.slice(0, K - 1)).forEach((v, j) => shares[K * i + j] = v));
  reconcileSixMargins(shares, weights, c.point);
  const prepared: ProjectedMunicipality[] = c.baseline.municipalities.map((m, i) => {
    const historical = { electors: m.electors, votes: SIX_PARTIES.map((listId, j) => ({ listId, votes: weights[i] * shares[K * i + j] })) };
    const municipality: HistoricalMunicipality = { code: m.id, regionId: m.regionId, provinceId: m.regionId, name: m.id, camera: historical, senato: historical };
    const reference = () => ({ shares: new Map(SIX_PARTIES.map((id, j) => [id, shares[K * i + j]])), abstention: 1 - valid[i],
      dates: new Map(SIX_PARTIES.map(id => [id, c.cutoff])), measurement: new Map(SIX_PARTIES.map((id, j) => [id, c.sigma[j]])), abstentionDate: c.baseline.date });
    return { municipality, camera: reference(), senato: reference() };
  });
  return (draw: number) => {
    const rows = generatePairedMunicipalVotes(scenario, snapshot, prepared, draw + 1), output = new Float64Array(rows.length * STRIDE);
    rows.forEach((r, i) => { const valid = sum(r.votes.map(v => v.votes));
      r.votes.forEach((v, j) => output[STRIDE * i + j] = valid ? v.votes / valid : 0); output[STRIDE * i + K] = valid / r.electors; });
    return output;
  };
}

/** Statistical rounding conserves each integer electorate, not legal seats. */
export function roundSixDraw(output: Float64Array, municipalities: SixMunicipality[]) {
  for (let i = 0; i < municipalities.length; i++) {
    const valid = Math.round(municipalities[i].electors * output[STRIDE * i + K]);
    const raw = SIX_PARTIES.map((_, j) => valid * output[STRIDE * i + j]), counts = raw.map(Math.floor);
    const order = SIX_PARTIES.map((_, j) => j).sort((a, b) => (raw[b] - counts[b]) - (raw[a] - counts[a]) || a - b);
    const remaining = valid - sum(counts); if (remaining < 0 || remaining > K) throw new Error('Invalid six-category rounding mass');
    for (let h = 0; h < remaining; h++) counts[order[h]]++;
    for (let j = 0; j < K; j++) output[STRIDE * i + j] = valid ? counts[j] / valid : 0;
    output[STRIDE * i + K] = valid / municipalities[i].electors;
  }
}

export function evaluateSixCase(c: SixCase, fit: ReturnType<typeof fitSixModel>, model: 'current' | 'joint-gaussian' | 'joint-student-t', draws = 256) {
  const seed = SIX_POLICY.version, municipalities = c.baseline.municipalities;
  const current = model === 'current' ? createSixCurrent(c, seed) : null;
  const candidate = model === 'current' ? null : createSixCandidate(c, fit, model, seed);
  const regionIds = [...new Set([...municipalities, ...c.event.municipalities].map(m => m.regionId))].sort();
  const regionIndex = new Map(regionIds.map((id, i) => [id, i]));
  const samples: Float64Array[] = [], nationalSamples: number[][] = [], regionalSamples: number[][] = [];
  let invalid = 0, extremeNational = 0, extremeLocal = 0, testedLocal = 0, continuousMarginError = 0, roundedMarginError = 0;
  for (let draw = 0; draw < draws; draw++) {
    const generated = candidate?.(draw), output = current ? current(draw) : generated!.output;
    if (generated) { continuousMarginError = Math.max(continuousMarginError, generated.margin.error); roundSixDraw(output, municipalities); }
    const regions = regionIds.map(() => ({ votes: Array(K).fill(0) as number[], valid: 0, electors: 0 }));
    const counts = Array(K).fill(0) as number[]; let valid = 0, electors = 0;
    for (let i = 0; i < municipalities.length; i++) {
      const m = municipalities[i], amount = m.electors * output[STRIDE * i + K], r = regions[regionIndex.get(m.regionId)!];
      let mass = 0;
      for (let j = 0; j < K; j++) {
        const p = output[STRIDE * i + j], count = p * amount; mass += p;
        invalid += Number(!Number.isFinite(p) || p < 0 || p > 1 || Math.abs(count - Math.round(count)) > 1e-5);
        counts[j] += count; r.votes[j] += count;
      }
      invalid += Number(!Number.isFinite(amount) || amount < 0 || amount > m.electors || amount > 0 && Math.abs(mass - 1) > 1e-8 || Math.abs(amount - Math.round(amount)) > 1e-5);
      if (amount >= SIX_POLICY.minimumTailValidVotes) { testedLocal++; extremeLocal += Number(Math.max(...Array.from(output.slice(STRIDE * i, STRIDE * i + K))) > SIX_POLICY.municipalConcentration); }
      valid += amount; electors += m.electors; r.valid += amount; r.electors += m.electors;
    }
    const national = [...counts.map(v => v / valid), valid / electors];
    invalid += Number(national.some(v => !Number.isFinite(v) || v < 0 || v > 1));
    if (generated) roundedMarginError = Math.max(roundedMarginError, ...national.slice(0, K).map((v, j) => Math.abs(v - generated.national[j])));
    extremeNational += Number(Math.max(...national.slice(0, K)) > SIX_POLICY.nationalConcentration);
    nationalSamples.push(national); regionalSamples.push(regions.flatMap(r => [...r.votes.map(v => r.valid ? v / r.valid : 0), r.valid / r.electors])); samples.push(output);
  }
  const nationalTruth = [...c.event.national.votes.map(v => v / c.event.national.validVotes), c.event.national.validVotes / c.event.national.electors];
  const nationalRows = nationalTruth.map((truth, j) => ({ category: [...SIX_PARTIES, 'valid-ballot-rate'][j], truth,
    ...summarizeScalarSamples(nationalSamples.map(s => s[j]), truth) }));
  const target = new Map(c.event.municipalities.map(m => [m.id, m]));
  const baselineIds = new Set(municipalities.map(m => m.id));
  const municipal = [], municipalWeights: number[] = [], municipalValid = [], municipalValidWeights: number[] = [];
  const partyScores = SIX_PARTIES.map(() => [] as ReturnType<typeof summarizeScalarSamples>[]);
  let matched = 0, matchedValid = 0, matchedBaselineValid = 0, actualExtreme = 0, actualTested = 0, jointLocal = 0;
  for (let i = 0; i < municipalities.length; i++) {
    const truth = target.get(municipalities[i].id); if (!truth) continue;
    matched++; matchedValid += truth.validVotes; matchedBaselineValid += municipalities[i].validVotes;
    const p = truth.votes.map(v => v / truth.validVotes);
    const scored = p.map((v, j) => summarizeScalarSamples(samples.map(s => s[STRIDE * i + j]), v));
    jointLocal += Number(scored.every(s => s.coverage90));
    scored.forEach((s, j) => partyScores[j].push(s)); municipal.push(...scored); municipalWeights.push(...p.map(() => truth.validVotes));
    municipalValid.push(summarizeScalarSamples(samples.map(s => s[STRIDE * i + K]), truth.validVotes / truth.electors)); municipalValidWeights.push(truth.electors);
    if (truth.validVotes >= SIX_POLICY.minimumTailValidVotes) { actualTested++; actualExtreme += Number(Math.max(...p) > SIX_POLICY.municipalConcentration); }
  }
  const regional = [], regionalWeights: number[] = [], regionalValid = [];
  for (let i = 0; i < regionIds.length; i++) {
    const group = c.event.municipalities.filter(m => m.regionId === regionIds[i]), valid = sum(group.map(m => m.validVotes)), el = sum(group.map(m => m.electors));
    for (let j = 0; j < K; j++) { regional.push(summarizeScalarSamples(regionalSamples.map(s => s[STRIDE * i + j]), sum(group.map(m => m.votes[j])) / valid)); regionalWeights.push(valid); }
    regionalValid.push(summarizeScalarSamples(regionalSamples.map(s => s[STRIDE * i + K]), valid / el));
  }
  return { model, national: aggregateScores(nationalRows.slice(0, K)), nationalRows,
    nationalJointCoverage90: Number(nationalRows.slice(0, K).every(s => s.coverage90)),
    nationalEnergy: scoreVectorEnergy(nationalSamples.map(s => s.slice(0, K)), nationalTruth.slice(0, K)),
    regional: aggregateScores(regional), regionalVoterWeighted: aggregateScores(regional, regionalWeights),
    municipal: aggregateScores(municipal), municipalVoterWeighted: aggregateScores(municipal, municipalWeights),
    municipalByParty: SIX_PARTIES.map((party, j) => ({ party, scores: aggregateScores(partyScores[j]) })), municipalJointCoverage90: jointLocal / matched,
    validBallotRate: { national: aggregateScores(nationalRows.slice(K)), regional: aggregateScores(regionalValid),
      municipal: aggregateScores(municipalValid), municipalElectorWeighted: aggregateScores(municipalValid, municipalValidWeights) },
    plausibility: { invalid, extremeNationalDrawRate: extremeNational / draws, extremeMunicipalRate: extremeLocal / testedLocal,
      actualExtremeNational: Number(Math.max(...nationalTruth.slice(0, K)) > SIX_POLICY.nationalConcentration), actualExtremeMunicipalRate: actualExtreme / actualTested,
      continuousMarginError, roundedNationalMarginError: model === 'current' ? null : roundedMarginError },
    geography: { forecastMunicipalities: municipalities.length, targetMunicipalities: c.event.municipalities.length, matchedMunicipalities: matched,
      matchedTargetValidVoteFraction: matchedValid / c.event.national.validVotes, matchedBaselineValidVoteFraction: matchedBaselineValid / c.baseline.national.validVotes,
      baselineOnly: municipalities.filter(m => !target.has(m.id)).map(m => m.id),
      targetOnly: c.event.municipalities.filter(m => !baselineIds.has(m.id)).map(m => m.id) } };
}
