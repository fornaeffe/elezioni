import type { DriftInputs, DriftElection } from './drift-data';
import type { DriftFit } from './drift-fit';
import type { DriftProblem } from './drift-structure';
import { dateDay } from './drift-math';
import { makeDriftPredictor } from './drift-predict';
import { aggregateScores, summarizeScalarSamples, scoreVectorEnergy, type GenerationScores } from './generation-metrics';
import { reconcileMargins } from './generation-models';
import { reconcileSixMargins } from './generation-six-category';
import { POLL_POLICY } from '../../src/lib/polls/forecast';
import { generatePairedMunicipalVotes, type ProjectedMunicipality } from '../../src/lib/politics/reform-2026/paired-vote-generation';
import type { ReformGenerationScenario, ReformGenerationSnapshot, HistoricalMunicipality } from '../../src/lib/politics/reform-2026/generation-types';

const average = (xs: number[]) => xs.reduce((a, b) => a + b, 0) / Math.max(1, xs.length);
export function currentDriftComparator(problem: DriftProblem, electionDate: string, seed: string) {
  const k = problem.j - 1, ids = problem.outcomes.slice(1);
  const latest = problem.elections.at(-1)!, same = problem.elections.filter(e => e.kind === problem.targetKind).at(-1) ?? latest;
  const localRows = new Map(latest.municipalities.map(r => [r.id, r]));
  const sameRows = new Map(same.municipalities.map(r => [r.id, r]));
  const totalValid = same.national.counts.slice(1).reduce((a, b) => a + b, 0), baseline = same.national.counts.slice(1).map(v => v / totalValid);
  const eligible = problem.polls.filter(p => dateDay(problem.cutoff) - dateDay(p.fieldworkEnd) <= POLL_POLICY.maximumAgeDays);
  const selected = [...new Set(eligible.map(p => p.house))].map(house => eligible.filter(p => p.house === house).sort((a, b) => b.fieldworkEnd.localeCompare(a.fieldworkEnd))[0]);
  const weights = selected.map(p => p.sample * 2 ** (-(dateDay(problem.cutoff) - dateDay(p.fieldworkEnd)) / POLL_POLICY.halfLifeDays));
  const sumWeight = weights.reduce((a, b) => a + b, 0), denominator = POLL_POLICY.historicalPseudoSample + sumWeight;
  const expanded = selected.map(p => {
    const shares = Array(k).fill(0);
    p.partitions.forEach((group, g) => {
      const total = group.reduce((s, i) => s + baseline[i - 1], 0);
      for (const i of group) shares[i - 1] += p.shares[g] * (total ? baseline[i - 1] / total : 1 / group.length);
    });
    return shares;
  });
  const share = baseline.map((p, j) => (POLL_POLICY.historicalPseudoSample * p + selected.reduce((s, q, i) => {
    return s + weights[i] * expanded[i][j];
  }, 0)) / denominator);
  const sigma = share.map((p, j) => {
    if (!selected.length) return 0;
    const variance = (POLL_POLICY.historicalPseudoSample * baseline[j] * (1 - baseline[j]) + selected.reduce((s, q, i) => {
      const qj = expanded[i][j]; return s + weights[i] ** 2 * qj * (1 - qj) / q.sample;
    }, 0)) / denominator ** 2;
    return Math.hypot(Math.sqrt(variance) / Math.max(.01, p * (1 - p)), POLL_POLICY.sharedErrorLogitSigma);
  });
  const probabilities = new Float64Array(problem.forecast.length * k), weightsLocal = new Float64Array(problem.forecast.length);
  const rows = problem.forecast.map((f, i) => {
    const row = localRows.get(f.city.id)!, turnout = sameRows.get(f.city.id) ?? row;
    const validFraction = 1 - turnout.counts[0] / turnout.electors, valid = row.counts.slice(1).reduce((a, b) => a + b, 0);
    row.counts.slice(1).forEach((n, j) => probabilities[k * i + j] = valid ? Math.max(1e-12, n / valid) : 1 / k);
    weightsLocal[i] = f.electors * validFraction;
    return { id: f.city.id, regionId: f.city.regionId, electors: f.electors, validFraction };
  });
  if (k === 4) reconcileMargins(probabilities, weightsLocal, share);
  else if (k === 6) reconcileSixMargins(probabilities, weightsLocal, share);
  else throw new Error('Unsupported current-model research panel');
  const scenario = { renewal: 'camera', chamberProfileId: 'central-v1', seed, electionDate } as ReformGenerationScenario;
  const snapshot = { historicalDate: latest.date, models: { chambers: {
    sigmaPerSqrtDay: { national: .02, region: .01, local: .01 },
    profiles: [{ id: 'central-v1', sharedVariance: .5, residualCorrelation: .7, gapMultiplier: 1 }]
  } } } as ReformGenerationSnapshot;
  const prepared: ProjectedMunicipality[] = rows.map((row, i) => {
    const history = { electors: row.electors, votes: ids.map((listId, a) => ({ listId, votes: weightsLocal[i] * probabilities[k * i + a] })) };
    const municipality = { code: row.id, regionId: row.regionId, provinceId: row.regionId, name: row.id, camera: history, senato: history } as HistoricalMunicipality;
    const reference = () => ({ shares: new Map(ids.map((id, a) => [id, probabilities[k * i + a]])), abstention: 1 - row.validFraction,
      dates: new Map(ids.map(id => [id, selected.length ? problem.cutoff : same.date])),
      measurement: new Map(ids.map((id, a) => [id, sigma[a]])), abstentionDate: latest.date });
    return { municipality, camera: reference(), senato: reference() };
  });
  return { audit: { point: share, measurementSigma: sigma, pollIds: selected.map(p => p.id), historicalPoint: same.id,
    method: 'actual-current-generator-with-current-reference-rules-adapted-to-shared-research-panel' },
    draw(sim: number) {
      const current = generatePairedMunicipalVotes(scenario, snapshot, prepared, sim + 1), output = new Float64Array(rows.length * (k + 1));
      current.forEach((row, i) => {
        output[(k + 1) * i] = row.abstention / row.electors;
        row.votes.forEach((v, a) => output[(k + 1) * i + a + 1] = v.votes / row.electors);
      });
      return { output, invalid: 0 };
    } };
}
interface Draw { output: Float64Array; invalid: number; }
export function evaluateDriftSamples(problem: DriftProblem, target: DriftElection, draws: number, generate: (i: number) => Draw) {
  const j = problem.j, targetMap = new Map(target.municipalities.map(r => [r.id, r])), regions = [...new Set(problem.forecast.map(f => f.city.regionId))].sort();
  const regionIndex = new Map(regions.map((r, i) => [r, i]));
  const samples: Float64Array[] = [], national: number[][] = [], regional: number[][] = [];
  let invalid = 0, politicalConcentration = 0, nonValidNation = 0, municipalConcentration = 0, testedLocal = 0, zeroValidNational = 0, zeroValidMunicipal = 0;
  for (let sim = 0; sim < draws; sim++) {
    const draw = generate(sim), amounts = Array(j).fill(0), groups = regions.map(() => ({ e: 0, n: Array(j).fill(0) }));
    invalid += draw.invalid;
    let electors = 0;
    problem.forecast.forEach((row, i) => {
      let total = 0;
      const group = groups[regionIndex.get(row.city.regionId)!]; group.e += row.electors; electors += row.electors;
      for (let a = 0; a < j; a++) {
        const v = draw.output[i * j + a], count = v * row.electors; total += v;
        invalid += Number(!Number.isFinite(v) || v < 0 || v > 1 || Math.abs(count - Math.round(count)) > 1e-5);
        amounts[a] += count; group.n[a] += count;
      }
      invalid += Number(Math.abs(total - 1) > 1e-8);
      const valid = 1 - draw.output[i * j];
      if (valid <= 0) zeroValidMunicipal++;
      else if (valid * row.electors >= 500) {
        testedLocal++; municipalConcentration += Number(Math.max(...Array.from(draw.output.slice(i * j + 1, i * j + j))) / valid > .95);
      }
    });
    const ns = amounts.map(n => n / electors), validTotal = electors - amounts[0];
    if (validTotal <= 0) zeroValidNational++;
    else politicalConcentration += Number(Math.max(...amounts.slice(1, j - 1)) / validTotal > .8);
    nonValidNation += Number(ns[0] > .9);
    samples.push(draw.output); national.push(ns);
    regional.push(groups.flatMap(g => g.n.map((n: number) => n / g.e)));
  }
  const scalar = (xs: number[], truth: number) => summarizeScalarSamples(xs, truth);
  const nationalTruth = target.national.counts.map(n => n / target.national.electors);
  const nationalRows = nationalTruth.map((truth, a) => scalar(national.map(n => n[a]), truth));
  const nationalValidRows = target.national.counts.slice(1).map((n, a) =>
    scalar(national.filter(row => row[0] < 1).map(row => row[a + 1] / (1 - row[0])), n / (target.national.electors - target.national.counts[0])));
  const municipal: GenerationScores[] = [], municipalPolitical: GenerationScores[] = [], municipalValid: GenerationScores[] = [], municipalNonvalid: GenerationScores[] = [];
  const electorWeights: number[] = [], validWeights: number[] = [], nonvalidWeights: number[] = [];
  let matchedElectors = 0, matchedValid = 0, matching = 0;
  for (let i = 0; i < problem.forecast.length; i++) {
    const row = targetMap.get(problem.forecast[i].city.id); if (!row) continue;
    matching++; matchedElectors += row.electors; matchedValid += row.electors - row.counts[0];
    const scores = row.counts.map((n, a) => scalar(samples.map(s => s[i * j + a]), n / row.electors));
    municipal.push(...scores); electorWeights.push(...scores.map(() => row.electors));
    municipalPolitical.push(...scores.slice(1)); municipalNonvalid.push(scores[0]); nonvalidWeights.push(row.electors);
    const eligible = samples.filter(s => s[i * j] < 1);
    if (row.electors > row.counts[0] && eligible.length) for (let a = 1; a < j; a++) {
      municipalValid.push(scalar(eligible.map(s => s[i * j + a] / (1 - s[i * j])), row.counts[a] / (row.electors - row.counts[0])));
      validWeights.push(row.electors - row.counts[0]);
    }
  }
  const regionalScores: GenerationScores[] = [], regionalValid: GenerationScores[] = [], regionalNonvalid: GenerationScores[] = [];
  regions.forEach((region, i) => {
    const actual = target.municipalities.filter(row => row.regionId === region), e = actual.reduce((s, r) => s + r.electors, 0);
    if (!e) return;
    const counts = Array.from({ length: j }, (_, a) => actual.reduce((s, r) => s + r.counts[a], 0));
    const scores = counts.map((n, a) => scalar(regional.map(s => s[i * j + a]), n / e));
    regionalScores.push(...scores); regionalNonvalid.push(scores[0]);
    const eligible = regional.filter(s => s[i * j] < 1);
    if (e > counts[0] && eligible.length) for (let a = 1; a < j; a++) regionalValid.push(scalar(eligible.map(s => s[i * j + a] / (1 - s[i * j])), counts[a] / (e - counts[0])));
  });
  return {
    national: aggregateScores(nationalRows), nationalPoliticalElector: aggregateScores(nationalRows.slice(1)),
    nationalPoliticalValid: aggregateScores(nationalValidRows), nationalNonvalid: nationalRows[0],
    nationalRows, nationalPoliticalValidRows: nationalValidRows, nationalEnergy: scoreVectorEnergy(national, nationalTruth),
    regional: aggregateScores(regionalScores), regionalPoliticalValid: aggregateScores(regionalValid), regionalNonvalid: aggregateScores(regionalNonvalid),
    municipal: aggregateScores(municipal), municipalElectorWeighted: aggregateScores(municipal, electorWeights),
    municipalPoliticalElector: aggregateScores(municipalPolitical), municipalPoliticalValid: aggregateScores(municipalValid),
    municipalValidWeighted: aggregateScores(municipalValid, validWeights), municipalNonvalid: aggregateScores(municipalNonvalid),
    municipalNonvalidElectorWeighted: aggregateScores(municipalNonvalid, nonvalidWeights),
    plausibility: { invalid, nationalNamedPartyOver80: politicalConcentration / draws, nationalNonvalidOver90: nonValidNation / draws,
      nationalZeroValidRate: zeroValidNational / draws, municipalPoliticalConcentration: municipalConcentration / Math.max(1, testedLocal),
      municipalZeroValidRate: zeroValidMunicipal / (draws * problem.forecast.length) },
    matching: { forecast: problem.forecast.length, matched: matching, electorFraction: matchedElectors / target.national.electors,
      validVoteFraction: matchedValid / (target.national.electors - target.national.counts[0]),
      actualElectors: target.national.electors, forecastElectors: problem.forecast.reduce((s, r) => s + r.electors, 0) }
  };
}
export function evaluateFittedDrift(fit: DriftFit, target: DriftElection, draws: number, seed: string) {
  const predictor = makeDriftPredictor(fit, target.date, draws, seed);
  return { ...evaluateDriftSamples(fit.problem, target, draws, predictor.draw), posterior: predictor.diagnostics };
}
