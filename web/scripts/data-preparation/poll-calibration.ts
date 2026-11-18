import { resolve } from 'node:path';
import { prepareSelectedPolls } from './polls';
import { readCalibrationResults, PANEL, type CalibrationElection } from './poll-calibration-sources';
import { sha256 } from './sources';
import { estimatePollReference, POLL_POLICY } from '../../src/lib/polls/forecast';
import type { NormalizedPollQuestion, PollEntityCatalog } from '../../src/lib/polls/types';
import { generationNormal, logistic, logit } from '../../src/lib/politics/reform-2026/generation-math';

const average = (v: number[]) => v.reduce((a, b) => a + b, 0) / Math.max(1, v.length);
const normalize = (v: number[]) => { const s = v.reduce((a, b) => a + b, 0); return v.map(p => p / s); };
const day = (s: string) => Date.parse(s) / 86400000;
export const CALIBRATION_POLICY = { version: 'poll-calibration-v2', poolingElectionCount: 3,
  nationalSigmaPerSqrtDay: .02, intervalLevel: .9, simulations: 4000,
  minimumCameraHoldouts: 3, minimumRelativeImprovement: .05 } as const;
const catalog: PollEntityCatalog = { schemaVersion: 1, version: 'calibration-panel-v1', entities: PANEL.map(id =>
  ({ id, name: id, aliases: [], components: [], proxyEntityIds: [], proxyNote: null })) };

export interface CalibrationCase extends CalibrationElection {
  baselineDate: string; baseline: number[]; truth: number[]; questions: NormalizedPollQuestion[];
}
export function calibrationBaselineDate(sourceId: string) {
  const match = /-(\d{4})(\d{2})(\d{2})$/.exec(sourceId);
  if (!match) throw new Error('Invalid calibration baseline source date');
  return `${match[1]}-${match[2]}-${match[3]}`;
}
const MODES = ['history', 'poll-only', 'v1', 'bias', 'uncertainty', 'combined',
  'history-original', 'poll-only-original', 'history-floor', 'poll-only-floor'] as const;
type Mode = typeof MODES[number];
/** Only these three parties are observed separately in every selected survey.
 * Other is a changing complement, never a historical party correspondence.
 * Collapsing full source tables avoids guessed within-group party splits. */
function collapseQuestion(q: NormalizedPollQuestion): NormalizedPollQuestion {
  const core = PANEL.slice(0, 3).map(id => {
    const rows = q.support.filter(r => r.entityIds.includes(id));
    if (rows.length !== 1 || rows[0].entityIds.length !== 1) throw new Error(`Calibration party not separately observed: ${q.id}:${id}`);
    return { label: id, entityIds: [id], kind: 'list' as const, share: rows[0].share };
  });
  const other = 1 - core.reduce((n, r) => n + r.share, 0);
  if (other < 0) throw new Error('Invalid historical poll panel');
  return { ...q, support: [...core, { label: 'other', entityIds: ['other'], kind: 'list', share: other }] };
}
function reference(c: CalibrationCase) {
  return estimatePollReference(c.questions.map(collapseQuestion), catalog,
    { lists: PANEL.map((id, i) => ({ id, name: id, baselineShare: c.baseline[i] })), historicalLists: [], correspondences: [] }, c.cutoff);
}
type Reference = ReturnType<typeof reference>;
interface FittedModel { trainingElectionIds: string[]; bias: number[]; extraSigma: number[]; uncorrectedExtraSigma: number[];
  benchmarkFloors: { history: number[]; pollOnly: number[] };
  benchmarkFloorTrainingCounts: { history: number[]; pollOnly: number[] };
  houses: { pollster: string; elections: number; offsets: number[] }[]; }
function originalBenchmark(c: CalibrationCase, r: Reference, history: boolean) {
  const total = r.evidence.reduce((n, e) => n + e.weight, 0);
  if (!total) throw new Error('Poll benchmark requires usable evidence');
  const share = history ? c.baseline : PANEL.map((_, i) =>
    r.evidence.reduce((n, e) => n + e.weight * e.reconciliation.shares[i].share, 0) / total);
  const referenceDate = history ? c.baselineDate : c.cutoff;
  return { share, referenceDate, nationalVariance: (day(c.date) - day(referenceDate)) * CALIBRATION_POLICY.nationalSigmaPerSqrtDay ** 2 };
}
function houseExposure(r: Reference, model: FittedModel) {
  const total = r.evidence.reduce((n, e) => n + e.weight, 0);
  return PANEL.map((_, i) => total ? r.evidence.reduce((n, e) => n + e.weight *
    (model.houses.find(h => h.pollster === e.pollster)?.offsets[i] ?? 0), 0) / (POLL_POLICY.historicalPseudoSample + total) : 0);
}
function predict(r: Reference, model?: FittedModel) {
  if (!model) return r.rows.map(r => r.share);
  const exposure = houseExposure(r, model);
  return normalize(r.rows.map((r, i) => logistic(logit(r.share) - model.bias[i] - exposure[i])));
}
/** One residual vector per election. House deviations are centered within that
 * election before pooling. More surveys cannot masquerade as more elections.
 * Error variance is an endpoint error budget: subtract the existing sampling/
 * prior variance AND assumed future national movement before adding a floor. */
export function fitCalibration(cases: CalibrationCase[]): FittedModel {
  if (!cases.length) throw new Error('Calibration requires past elections');
  const refs = cases.map(reference), k = CALIBRATION_POLICY.poolingElectionCount;
  const houses = [...new Set(refs.flatMap(r => r.evidence.map(e => e.pollster)))].sort().map(pollster => {
    const offsets = refs.flatMap(r => {
      const e = r.evidence.find(e => e.pollster === pollster);
      if (!e) return [];
      return [PANEL.map((_, i) => logit(e.reconciliation.shares[i].share) - average(r.evidence.map(e => logit(e.reconciliation.shares[i].share))))];
    });
    return { pollster, elections: offsets.length, offsets: PANEL.map((_, i) => average(offsets.map(o => o[i])) * offsets.length / (offsets.length + k)) };
  });
  const model: FittedModel = { trainingElectionIds: cases.map(c => c.id), houses, bias: PANEL.map(() => 0), extraSigma: [], uncorrectedExtraSigma: [],
    benchmarkFloors: { history: [], pollOnly: [] }, benchmarkFloorTrainingCounts: { history: [], pollOnly: [] } };
  const houseAdjusted = refs.map(r => predict(r, model));
  model.bias = PANEL.map((_, i) => average(cases.map((c, n) => logit(houseAdjusted[n][i]) - logit(c.truth[i]))) * cases.length / (cases.length + k));
  const errorFloor = (correctBias: boolean) => PANEL.map((_, i) => {
    const excess = average(cases.map((c, n) => {
      const error = logit(predict(refs[n], correctBias ? model : undefined)[i]) - logit(c.truth[i]);
      return error ** 2 - refs[n].rows[i].sigma ** 2 - (day(c.date) - day(c.cutoff)) * CALIBRATION_POLICY.nationalSigmaPerSqrtDay ** 2;
    }));
    return Math.sqrt(Math.max(0, excess * cases.length / (cases.length + k)));
  });
  model.extraSigma = errorFloor(true);
  model.uncorrectedExtraSigma = errorFloor(false);
  for (const kind of ['history', 'pollOnly'] as const) {
    const benchmarks = cases.map((c, i) => originalBenchmark(c, refs[i], kind === 'history'));
    for (const i of PANEL.keys()) {
      // The original generator preserves structural zero support. Such a
      // category has no estimable logit error and cannot be revived by noise.
      const excess = benchmarks.flatMap((b, j) => b.share[i] > 0 && b.share[i] < 1 && cases[j].truth[i] > 0 && cases[j].truth[i] < 1
        ? [(logit(b.share[i]) - logit(cases[j].truth[i])) ** 2 - b.nationalVariance] : []);
      model.benchmarkFloorTrainingCounts[kind].push(excess.length);
      model.benchmarkFloors[kind].push(Math.sqrt(Math.max(0, average(excess) * excess.length / (excess.length + k))));
    }
  }
  return model;
}
function score(c: CalibrationCase, r: Reference, mode: Mode, model: FittedModel) {
  const benchmark = ['history-original', 'poll-only-original', 'history-floor', 'poll-only-floor'].includes(mode)
    ? originalBenchmark(c, r, mode.startsWith('history-')) : null;
  const share = benchmark?.share ?? (mode === 'history' ? c.baseline : mode === 'poll-only' ? PANEL.map((_, i) => {
    const total = r.evidence.reduce((n, e) => n + e.weight, 0);
    return r.evidence.reduce((n, e) => n + e.weight * e.reconciliation.shares[i].share, 0) / total;
  }) : predict(r, mode === 'bias' || mode === 'combined' ? model : undefined));
  if (mode === 'history' || mode === 'poll-only') return { mode, mae: average(share.map((p, i) => Math.abs(p - c.truth[i]))), rows: PANEL.map((id, i) => ({ id, share: share[i], truth: c.truth[i] })) };
  const benchmarkExtra = PANEL.map((_, i) => mode.endsWith('-floor') ? model.benchmarkFloors[mode.startsWith('history-') ? 'history' : 'pollOnly'][i] : 0);
  const sigma = benchmark ? benchmarkExtra.map(extra => Math.sqrt(benchmark.nationalVariance + extra ** 2)) : r.rows.map((row, i) => Math.sqrt(row.sigma ** 2 + (day(c.date) - day(c.cutoff)) * CALIBRATION_POLICY.nationalSigmaPerSqrtDay ** 2 +
    (mode === 'uncertainty' ? model.uncorrectedExtraSigma[i] ** 2 : mode === 'combined' ? model.extraSigma[i] ** 2 : 0)));
  // National valid-share surrogate, not full municipal/turnout/chamber runtime.
  // Same seeded draws for all candidates make interval-score comparisons stable.
  const samples = Array.from({ length: CALIBRATION_POLICY.simulations }, (_, n) => normalize(share.map((p, i) =>
    benchmark && (p === 0 || p === 1) ? p : logistic(logit(p) + sigma[i] * generationNormal('poll-calibration-interval-v1', n + 1, c.id, PANEL[i])))));
  const rows = PANEL.map((id, i) => {
    const sorted = samples.map(s => s[i]).sort((a, b) => a - b);
    const lower = sorted[Math.floor(.05 * sorted.length)], upper = sorted[Math.floor(.95 * sorted.length)];
    const truth = c.truth[i], intervalScore = upper - lower + 20 * Math.max(0, lower - truth, truth - upper);
    return { id, share: share[i], truth, lower, upper, covered: truth >= lower && truth <= upper, intervalScore };
  });
  return { mode, ...(benchmark ? { uncertainty: { referenceDate: benchmark.referenceDate, nationalVariance: benchmark.nationalVariance, extraSigma: benchmarkExtra,
    method: 'original-national-component', structuralZerosPreserved: true } } : {}),
    mae: average(rows.map(r => Math.abs(r.share - r.truth))), coverage: average(rows.map(r => Number(r.covered))),
    intervalScore: average(rows.map(r => r.intervalScore)), rows };
}
/** Expanding window: every forecast is fitted only on earlier elections. */
export function evaluateCalibration(cases: CalibrationCase[]) {
  const ordered = [...cases].sort((a, b) => a.date.localeCompare(b.date));
  if (new Set(ordered.map(c => c.id)).size !== ordered.length || ordered.length < 2) throw new Error('Calibration requires distinct election events');
  for (const c of ordered) {
    if (!/^\d{4}-\d{2}-\d{2}$/.test(c.baselineDate) || !Number.isFinite(Date.parse(c.baselineDate)) ||
      new Date(c.baselineDate).toISOString().slice(0, 10) !== c.baselineDate || c.baselineDate >= c.cutoff ||
      c.cutoff >= c.date || c.truth.length !== 4 || c.baseline.length !== 4 ||
      [c.truth, c.baseline].some(ps => ps.some(p => !Number.isFinite(p) || p < 0 || p > 1) || Math.abs(ps.reduce((a, b) => a + b, 0) - 1) > 1e-9) ||
      !c.questions.length || c.questions.some(q => q.source.publicationDate > c.cutoff || q.source.fieldworkEnd > c.cutoff)) throw new Error('Invalid calibration case or post-cutoff information');
  }
  const folds = ordered.slice(1).map((c, n) => {
    const training = ordered.slice(0, n + 1), model = fitCalibration(training), r = reference(c);
    return { electionId: c.id, kind: c.kind, cutoff: c.cutoff, electionDate: c.date, horizonDays: day(c.date) - day(c.cutoff),
      trainingElectionIds: model.trainingElectionIds, model,
      selectedQuestionIds: r.evidence.map(e => e.questionId),
      scores: MODES.map(mode => score(c, r, mode, model)) };
  });
  const metrics = (fs: typeof folds) => MODES.map(mode => {
    const ss = fs.map(f => f.scores.find(s => s.mode === mode)!);
    return { mode, elections: fs.length, mae: average(ss.map(s => s.mae)),
      coverage: ss.every(s => 'coverage' in s) && ss.length ? average(ss.map(s => s.coverage!)) : null,
      intervalScore: ss.every(s => 'intervalScore' in s) && ss.length ? average(ss.map(s => s.intervalScore!)) : null };
  });
  const camera = folds.filter(f => f.kind === 'camera'), cameraMetrics = metrics(camera);
  const base = cameraMetrics.find(m => m.mode === 'v1')!;
  const sufficientCameraHoldouts = camera.length >= CALIBRATION_POLICY.minimumCameraHoldouts;
  const candidateChecks = (['bias', 'uncertainty', 'combined'] as const).map(mode => {
    const candidate = cameraMetrics.find(m => m.mode === mode)!;
    return { mode, metricsPass: candidate.mae <= base.mae * (mode === 'uncertainty' ? 1 : 1 - CALIBRATION_POLICY.minimumRelativeImprovement) &&
      candidate.intervalScore! < base.intervalScore! && candidate.coverage! >= .8 };
  });
  // The surrogate does not establish full generator interval coverage. It must
  // never silently activate a fitted correction in an arbitrary scenario.
  const runtimeCoverageValidated = false;
  return { policy: CALIBRATION_POLICY, folds, metrics: metrics(folds), byKind: { camera: cameraMetrics, european: metrics(folds.filter(f => f.kind === 'european')) },
    fitted: fitCalibration(ordered), gate: { accepted: sufficientCameraHoldouts && candidateChecks.some(c => c.metricsPass) && runtimeCoverageValidated,
      sufficientCameraHoldouts, candidateChecks, runtimeCoverageValidated,
      reasons: [...(!sufficientCameraHoldouts ? [`Only ${camera.length} chronological Camera holdouts; require ${CALIBRATION_POLICY.minimumCameraHoldouts}.`] : []),
        ...(!candidateChecks.find(c => c.mode === 'combined')!.metricsPass ? ['Bias/combined correction does not pass Camera MAE, interval score and coverage requirements.'] : []),
        'Panel intervals are a national valid-share surrogate; full generator interval coverage and transfer to other lists/chambers remain unvalidated.'] } };
}

export function weightedPollPoint(c: CalibrationCase) {
  return originalBenchmark(c, reference(c), false).share;
}

export async function loadPollCalibrationCases(root: string) {
  const archive = await prepareSelectedPolls(root, resolve(root, 'data/sources/polls/history'));
  const { manifest, results, sourceVersions, archiveBytes } = await readCalibrationResults(root);
  const cases = manifest.elections.map(e => {
    const shares = (id: string) => results.find(r => r.sourceId === id)!.panel.map(p => p.share);
    const questions = e.questionIds.map(id => {
      const q = archive.bundle.questions.find(q => q.id === id);
      if (!q) throw new Error(`Missing historical question ${id}`);
      return q;
    });
    return { ...e, baselineDate: calibrationBaselineDate(e.baselineSourceId), baseline: shares(e.baselineSourceId), truth: shares(e.resultSourceId), questions };
  });
  const questionIds = cases.flatMap(c => c.questionIds);
  if (new Set(questionIds).size !== questionIds.length || questionIds.length !== archive.bundle.questions.length) throw new Error('Historical questions must pair to exactly one election');
  const totalArchiveBytes = archive.audit.archiveBytes + archiveBytes;
  if (totalArchiveBytes > archive.audit.maximumArchiveBytes) throw new Error('Historical poll/target archive exceeds size budget');
  return { archive, cases, sourceVersions, totalArchiveBytes };
}

export async function preparePollCalibration(root: string) {
  const { archive, cases, sourceVersions, totalArchiveBytes } = await loadPollCalibrationCases(root);
  const report = { schemaVersion: 1, kind: 'poll-historical-calibration', archiveVersion: archive.bundle.version, sourceVersions,
    archiveBytes: totalArchiveBytes, panel: PANEL, elections: cases.map(({ questions, ...c }) => ({ ...c, sources: questions.map(q => ({
      questionId: q.id, pollster: q.source.pollster, fieldworkStart: q.source.fieldworkStart, fieldworkEnd: q.source.fieldworkEnd, publicationDate: q.source.publicationDate })) })),
    ...evaluateCalibration(cases), limitations: [
      'Purposive panel of selected houses near publication blackout, not a complete historical poll census.',
      'Camera target: domestic proportional lists excluding Valle d’Aosta and overseas; European target: domestic lists including Valle d’Aosta and 2024 out-of-town voters, excluding overseas.',
      'Adult national poll populations approximate the election electorate; nonresponse is not turnout. Senate age and geography are not calibrated.',
      'Only PD, M5S and Lega are separately comparable throughout. Other is a changing complement, not a party; no invented historical split for FdI or new lists.',
      'European elections add independent events for diagnostics and partial pooling; Camera-specific validation controls transfer to national forecasts.',
      'Election-endpoint errors mix house bias, common sampling/nonresponse effects, blackout movement and electorate differences; these are not separately identified.',
      'Partial pooling uses three prior election equivalents. Extra measurement variance subtracts existing variance and assumed future national movement; territorial movement is not fitted.',
      'Intervals use a compositional national logistic-normal surrogate with the same future national movement convention; they are not whole-generator or simultaneous coverage.',
      'Original uncertainty benchmarks use only the pre-poll national logit movement component (0.02 per sqrt day), without poll measurement/prior variance. Historical references start at the previous election date; weighted poll references start at the cutoff. Regional/local movement, turnout coupling and chamber effects are outside this national surrogate.',
      'Benchmark error floors are fitted separately using earlier elections only, after subtracting original national movement variance. Structural zero shares remain zero; boundary observations are excluded from logit-floor fitting and training counts are reported.',
      'Corrections remain research candidates. Unsupported parties retain existing explicit uncertainty assumptions and simulation computability.'
    ] };
  return { ...report, version: `poll-calibration-v2-${sha256(JSON.stringify(report)).slice(0, 12)}` };
}
