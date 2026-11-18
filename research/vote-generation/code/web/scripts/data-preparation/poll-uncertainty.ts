import { generationNormal, generationRng } from '../../src/lib/politics/reform-2026/generation-math';

/** Research-only national endpoint distributions. Never a runtime policy. */
export const UNCERTAINTY_POLICY = { version: 'poll-uncertainty-v1', simulations: 4000,
  priorCovarianceSigma: .2, priorInverseWishartDf: 5, nominalCoverage: .9,
  scope: 'four-category national valid-share endpoint at 15-16 days' } as const;
export interface PollUncertaintyCase { id: string; kind: 'camera' | 'european'; date: string; cutoff: string; point: number[]; truth: number[]; }
const mean = (xs: number[]) => xs.reduce((a, b) => a + b, 0) / xs.length;
const basis = Array.from({ length: 3 }, (_, i) => Array.from({ length: 4 }, (_, j) =>
  j <= i ? 1 / Math.sqrt((i + 1) * (i + 2)) : j === i + 1 ? -(i + 1) / Math.sqrt((i + 1) * (i + 2)) : 0));
function validate(p: number[]) {
  if (p.length !== 4 || p.some(x => !Number.isFinite(x) || x <= 0 || x >= 1) || Math.abs(p.reduce((a, b) => a + b, 0) - 1) > 1e-9) throw new Error('ILR research panel requires four positive shares summing to one');
}
export function ilr(p: number[]) { validate(p); return basis.map(b => b.reduce((n, h, i) => n + h * Math.log(p[i]), 0)); }
export function inverseIlr(z: number[]) {
  if (z.length !== 3 || z.some(x => !Number.isFinite(x))) throw new Error('Invalid ILR vector');
  const log = Array.from({ length: 4 }, (_, i) => basis.reduce((n, b, j) => n + b[i] * z[j], 0));
  const exp = log.map(x => Math.exp(x - Math.max(...log))), total = exp.reduce((a, b) => a + b, 0);
  return exp.map(x => x / total);
}
function cholesky(m: number[][]) {
  const l = Array.from({ length: 3 }, () => [0, 0, 0]);
  for (let i = 0; i < 3; i++) for (let j = 0; j <= i; j++) {
    const v = m[i][j] - l[i].slice(0, j).reduce((n, x, k) => n + x * l[j][k], 0);
    if (i === j && v <= 0) throw new Error('Non-positive predictive covariance');
    l[i][j] = i === j ? Math.sqrt(v) : v / l[j][j];
  }
  return l;
}
export function fitEndpointErrors(cases: PollUncertaintyCase[], priorSigma: number = UNCERTAINTY_POLICY.priorCovarianceSigma) {
  if (!Number.isFinite(priorSigma) || priorSigma <= 0) throw new Error('Invalid endpoint prior');
  const errors = cases.map(c => { const p = ilr(c.point), y = ilr(c.truth); return y.map((x, i) => x - p[i]); });
  // Fixed zero mean keeps weighted polling as the point forecast. Uncentered
  // errors retain the risk of persistent bias instead of subtracting it away.
  // Sigma ~ IW_3(nu0=5, Psi0=priorSigma^2 I), E[Sigma]=Psi0.
  const psi = Array.from({ length: 3 }, (_, i) => Array.from({ length: 3 }, (_, j) =>
    (i === j ? priorSigma ** 2 : 0) + errors.reduce((n, e) => n + e[i] * e[j], 0)));
  const df = UNCERTAINTY_POLICY.priorInverseWishartDf + cases.length - 3 + 1;
  return { trainingElectionIds: cases.map(c => c.id), errors, priorSigma, posteriorPsi: psi,
    predictiveDf: df, posteriorMeanCovariance: psi.map(row => row.map(x => x / (df - 2))),
    factor: cholesky(psi) };
}
const modes = ['gaussian-iw-mean', 'student-t', 'election-block-bootstrap', 'conformal-box',
  'student-t-prior-010', 'student-t-prior-030', 'student-t-same-kind'] as const;
type Mode = typeof modes[number];
function distance(a: number[], b: number[]) { return Math.hypot(...a.map((x, i) => x - b[i])); }
export function evaluateEndpointUncertainty(cases: PollUncertaintyCase[]) {
  const ordered = [...cases].sort((a, b) => a.date.localeCompare(b.date));
  if (ordered.length < 2 || new Set(ordered.map(c => c.id)).size !== ordered.length) throw new Error('Distinct endpoint election cases required');
  for (const c of ordered) {
    validate(c.point); validate(c.truth);
    const horizon = (Date.parse(c.date) - Date.parse(c.cutoff)) / 86400000;
    if (!Number.isFinite(horizon) || horizon < 15 || horizon > 16) throw new Error('Endpoint study supports only the selected 15-16 day horizons');
  }
  const folds = ordered.slice(1).map((c, index) => {
    const past = ordered.slice(0, index + 1), centre = ilr(c.point);
    if (past.some(p => p.date >= c.cutoff)) throw new Error('Training election outcome is not available before forecast cutoff');
    const scores = modes.map(mode => {
      const training = mode === 'student-t-same-kind' ? past.filter(p => p.kind === c.kind) : past;
      const prior = mode === 'student-t-prior-010' ? .1 : mode === 'student-t-prior-030' ? .3 : UNCERTAINTY_POLICY.priorCovarianceSigma;
      const fit = fitEndpointErrors(training, prior);
      const rank = Math.ceil((training.length + 1) * UNCERTAINTY_POLICY.nominalCoverage);
      const errors = training.map(t => Math.max(...t.truth.map((p, i) => Math.abs(p - t.point[i])))).sort((a, b) => a - b);
      const conformalRadius = rank > errors.length ? Infinity : errors[rank - 1];
      const samples = mode === 'conformal-box' ? [] : Array.from({ length: UNCERTAINTY_POLICY.simulations }, (_, i) => {
        const normals = [0, 1, 2].map(j => generationNormal('poll-endpoint-v1', i + 1, c.id, 'normal', j));
        let delta: number[];
        if (mode === 'election-block-bootstrap') {
          const rng = generationRng('poll-endpoint-v1', i + 1, c.id, 'block');
          const e = fit.errors[Math.floor(rng() * fit.errors.length)], sign = rng() < .5 ? -1 : 1;
          delta = e.map(x => sign * x);
        } else {
          // Gaussian and Student-t have the SAME covariance; only tail shape
          // differs. One chi-square draw scales the whole vector, not each party.
          const divisor = mode === 'gaussian-iw-mean' ? fit.predictiveDf - 2 :
            Array.from({ length: fit.predictiveDf }, (_, j) => generationNormal('poll-endpoint-v1', i + 1, c.id, 'chi', j) ** 2).reduce((a, b) => a + b, 0);
          delta = fit.factor.map(row => row.reduce((n, l, j) => n + l * normals[j], 0) / Math.sqrt(divisor));
        }
        return inverseIlr(centre.map((x, j) => x + delta[j]));
      });
      const rows = c.point.map((point, j) => {
        const sorted = samples.map(s => s[j]).sort((a, b) => a - b);
        const lower = mode === 'conformal-box' ? Math.max(0, point - conformalRadius) : sorted[200];
        const upper = mode === 'conformal-box' ? Math.min(1, point + conformalRadius) : sorted[3800];
        return { category: j, point, truth: c.truth[j], lower, upper, covered: c.truth[j] >= lower && c.truth[j] <= upper,
          intervalScore: upper - lower + 20 * Math.max(0, lower - c.truth[j], c.truth[j] - upper),
          predictiveMean: samples.length ? mean(samples.map(s => s[j])) : null,
          predictiveMedian: samples.length ? sorted[2000] : null };
      });
      const energyScore = samples.length ? mean(samples.slice(0, 2000).map((s, i) =>
        (distance(s, c.truth) + distance(samples[i + 2000], c.truth) - distance(s, samples[i + 2000])) / 2)) : null;
      return { mode, fit, mae: mean(rows.map(r => Math.abs(r.point - r.truth))),
        coverage: mean(rows.map(r => Number(r.covered))), jointCoverage: Number(rows.every(r => r.covered)),
        intervalScore: mean(rows.map(r => r.intervalScore)), width: mean(rows.map(r => r.upper - r.lower)), energyScore,
        predictiveMeanMae: samples.length ? mean(rows.map(r => Math.abs(r.predictiveMean! - r.truth))) : null,
        predictiveMedianMae: samples.length ? mean(rows.map(r => Math.abs(r.predictiveMedian! - r.truth))) : null,
        conformal: mode === 'conformal-box' ? { calibrationEvents: training.length, rank, finite: Number.isFinite(conformalRadius),
          radius: Number.isFinite(conformalRadius) ? conformalRadius : null, exchangeabilityVerified: false } : null, rows };
    });
    return { electionId: c.id, kind: c.kind, date: c.date, cutoff: c.cutoff, scores };
  });
  const summarize = (fs: typeof folds) => modes.map(mode => {
    const ss = fs.map(f => f.scores.find(s => s.mode === mode)!);
    const aggregate = (key: 'mae' | 'coverage' | 'jointCoverage' | 'intervalScore' | 'width') => mean(ss.map(s => s[key]));
    return { mode, elections: ss.length, mae: aggregate('mae'), coverage: aggregate('coverage'), jointCoverage: aggregate('jointCoverage'),
      intervalScore: aggregate('intervalScore'), width: aggregate('width'), energyScore: mode === 'conformal-box' ? null : mean(ss.map(s => s.energyScore!)),
      predictiveMeanMae: mode === 'conformal-box' ? null : mean(ss.map(s => s.predictiveMeanMae!)),
      predictiveMedianMae: mode === 'conformal-box' ? null : mean(ss.map(s => s.predictiveMedianMae!)) };
  });
  return { policy: UNCERTAINTY_POLICY, folds, metrics: summarize(folds), byKind: {
    camera: summarize(folds.filter(f => f.kind === 'camera')), european: summarize(folds.filter(f => f.kind === 'european')) } };
}
