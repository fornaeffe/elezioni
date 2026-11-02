import { add, mv, zeros, type Mat } from './drift-math';
import { makeDriftProblem, defaultParameters, updateWorkingObservations, collapseCounts, localMean,
  DRIFT_POLICY, type DriftProblem, type DriftParameters, type CollapsedCounts, type DriftVariant } from './drift-structure';
import { estimateLocalScales, type LocalScaleAudit } from './drift-local';
import { buildPollGeometry, covarianceResponse, jacobianForces, type PollGeometry } from './drift-polls';
import { GlobalSystem, optimizeGlobal, sampleGlobal, type GlobalEvaluation } from './drift-global';
import type { DriftInputs } from './drift-data';

export interface DriftFit {
  problem: DriftProblem; parameters: DriftParameters; localAudit: LocalScaleAudit;
  counts: CollapsedCounts; geometry: PollGeometry; system: GlobalSystem; evaluation: GlobalEvaluation;
  iterations: { iteration: number; maxChange: number; globalChange: number; rmsChange: number; eta: number; poll: number; score: number; seconds: number }[];
  converged: boolean; seconds: number;
}
export function fitDrift(input: DriftInputs, cutoff: string, kind: 'camera' | 'european', variant: DriftVariant,
  progress?: (x: unknown) => void, localScaleMultiplier = 1): DriftFit {
  const start = performance.now(), problem = makeDriftProblem(input, cutoff, kind, variant), parameters = defaultParameters(problem);
  const localAudit = estimateLocalScales(problem, parameters), mode = zeros(problem.layout.m, 1);
  parameters.delta *= localScaleMultiplier;
  for (const time of problem.layout.times) mode.set(problem.nationalMean, problem.layout.national(time));
  let previous: PollGeometry | undefined, final: Omit<DriftFit, 'iterations' | 'converged' | 'seconds'> | undefined;
  const iterations: DriftFit['iterations'] = [];
  for (let iteration = 0; iteration < DRIFT_POLICY.stateIterations; iteration++) {
    const tic = performance.now(), correction = updateWorkingObservations(problem), counts = collapseCounts(problem, parameters);
    const geometry = buildPollGeometry(problem, parameters, counts, mode, previous);
    const system = new GlobalSystem(problem, parameters, counts, geometry, correction);
    const optimum = optimizeGlobal(system, parameters);
    parameters.eta = optimum.evaluation.parameters.eta; parameters.poll = optimum.evaluation.parameters.poll;
    parameters.etaShape = [...optimum.evaluation.parameters.etaShape];
    const evaluation = optimum.evaluation;
    geometry.update = mv(system.pollInverse, add(geometry.response, mv(geometry.jacobian, evaluation.mean, geometry.p), -1), geometry.p);
    let maxChange = 0, square = 0, size = 0;
    for (const city of problem.cities) {
      const gp = counts.locals.get(city.id)!, cell = geometry.cities.get(city.id)!;
      const response = covarianceResponse(cell.grid, jacobianForces(geometry, cell, geometry.update, problem.d), problem.d);
      for (const record of city.records.slice(1)) {
        const local = localMean(problem, city, gp, evaluation.mean, record.time).mean;
        const target = add(city.theta, add(local, response[cell.grid.times.indexOf(record.time)]));
        for (let r = 0; r < problem.d; r++) for (const [offset, w] of problem.layout.blocks(city, record.time, record.election.kind, parameters.cityScale.get(city.id) ?? 1))
          target[r] += w * evaluation.mean[offset + r];
        for (let r = 0; r < problem.d; r++) {
          const change = target[r] - record.state[r]; maxChange = Math.max(maxChange, Math.abs(change)); square += change ** 2; size++;
          // Damping the nonlinear mode update does not alter its fixed point.
          record.state[r] += .85 * change;
        }
      }
    }
    const globalChange = Math.max(...Array.from(evaluation.mean, (v, i) => Math.abs(v - mode[i])));
    maxChange = Math.max(maxChange, globalChange);
    mode.set(evaluation.mean); previous = geometry;
    const row = { iteration: iteration + 1, maxChange, globalChange, rmsChange: Math.sqrt(square / Math.max(1, size)),
      eta: parameters.eta, poll: parameters.poll, score: evaluation.score, seconds: (performance.now() - tic) / 1000 };
    iterations.push(row); progress?.(row);
    final = { problem, parameters, localAudit, counts, geometry, system, evaluation };
    if (iteration >= 1 && maxChange < DRIFT_POLICY.stateTolerance) break;
  }
  if (!final) throw new Error('No Laplace iteration');
  return { ...final, iterations, converged: iterations.at(-1)!.maxChange < DRIFT_POLICY.stateTolerance, seconds: (performance.now() - start) / 1000 };
}
export function posteriorDrift(fit: DriftFit, draws: number, seed: string) {
  return sampleGlobal(fit.system, fit.evaluation, draws, seed);
}
