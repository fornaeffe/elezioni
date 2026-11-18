import type { Candidate, Preferences, Provenance } from './input';
import type { PreferenceParameters, PreferenceProfile, ReformModelBundle } from './generation-types';
import { generationNormal, generationRng, multinomial, sum } from './generation-math';

export function preferenceParametersForRegion(models: ReformModelBundle, regionId: string): PreferenceParameters {
  const pools = models.preferences.pools.filter(p => p.scope === regionId);
  return pools.length ? { intensity: sum(pools.map(p => p.parameters.intensity)) / pools.length,
    strengthSigma: sum(pools.map(p => p.parameters.strengthSigma)) / pools.length,
    localSigma: sum(pools.map(p => p.parameters.localSigma)) / pools.length } : models.preferences.baseline;
}

/** Persistence partitions a fixed fitted log-strength variance budget. Changing
 * persistence alone must not also change candidate concentration. */
export function candidatePreferenceStrength(seed: string, simulationId: number, chamber: string, collegeId: string, listId: string,
  candidateId: string, parameters: PreferenceParameters, profile: PreferenceProfile): number {
  const shared = generationNormal(seed, simulationId, 'preference-person', chamber, candidateId);
  const local = generationNormal(seed, simulationId, 'preference-local', chamber, collegeId, listId, candidateId);
  const sigma = Math.hypot(parameters.strengthSigma, parameters.localSigma) * profile.concentrationMultiplier;
  return sigma * (Math.sqrt(profile.persistence) * shared + Math.sqrt(1 - profile.persistence) * local);
}

/** C58/C93-sexies, S14: at most three distinct non-capolista names;
 * «Nel caso di più preferenze ... candidati di sesso diverso».
 * This enumerates valid inputs, not the adjudication of invalid ballots. */
export function validPreferenceSubsets(eligible: readonly Pick<Candidate, 'id' | 'gender'>[]): string[][] {
  if (eligible.length > 6 || new Set(eligible.map(c => c.id)).size !== eligible.length) throw new RangeError('Expected up to six distinct eligible candidates');
  const result: string[][] = [[]];
  for (let mask = 1; mask < 2 ** eligible.length; mask++) {
    const subset = eligible.filter((_, i) => mask & (1 << i));
    if (subset.length > 3 || (subset.length > 1 && new Set(subset.map(c => c.gender)).size !== 2)) continue;
    result.push(subset.map(c => c.id));
  }
  return result;
}

/** Maximum entropy relative to a declared multiplicity base measure. At the
 * central profile the measure is uniform over valid subsets. Candidate log
 * strengths are potentials; a Lagrange multiplier enforces the selected mean
 * mark intensity. No joint-ballot behavior is inferred from historical marginals. */
export function preferenceCombinationDistribution(eligible: readonly Pick<Candidate, 'id' | 'gender'>[],
  strengths: ReadonlyMap<string, number>, intensity: number, tripleWeight = 1): { candidateIds: string[]; probability: number }[] {
  if (!Number.isFinite(intensity) || intensity < 0 || !Number.isFinite(tripleWeight) || tripleWeight < 0 ||
      eligible.some(c => !Number.isFinite(strengths.get(c.id)))) throw new RangeError('Invalid preference distribution parameters');
  const subsets = validPreferenceSubsets(eligible).filter(s => s.length !== 3 || tripleWeight > 0);
  const maximum = Math.max(...subsets.map(s => s.length));
  if (intensity > maximum) throw new RangeError('Preference intensity is incompatible with valid subsets');
  const potentials = subsets.map(s => sum(s.map(id => strengths.get(id)!)) + (s.length === 3 ? Math.log(tripleWeight) : 0));
  const probabilities = (lambda: number) => {
    const logits = subsets.map((s, i) => potentials[i] + lambda * s.length);
    const high = Math.max(...logits), weights = logits.map(x => Math.exp(x - high)), total = sum(weights);
    return weights.map(w => w / total);
  };
  let ps: number[];
  if (intensity === 0) ps = subsets.map(s => Number(s.length === 0));
  else if (intensity === maximum) {
    const weights = subsets.map((s, i) => s.length === maximum ? Math.exp(potentials[i] - Math.max(...potentials)) : 0);
    ps = weights.map(w => w / sum(weights));
  } else {
    let lower = -128, upper = 128;
    for (let i = 0; i < 80; i++) {
      const middle = (lower + upper) / 2, p = probabilities(middle);
      if (sum(p.map((v, j) => v * subsets[j].length)) < intensity) lower = middle; else upper = middle;
    }
    ps = probabilities((lower + upper) / 2);
  }
  return subsets.map((s, i) => ({ candidateIds: s, probability: ps[i] }));
}

export function generateSyntheticPreferences(options: {
  seed: string; simulationId: number; chamber: string; collegeId: string; listId: string;
  eligible: readonly Pick<Candidate, 'id' | 'gender'>[]; ballotBudget: number;
  parameters: PreferenceParameters; profile: PreferenceProfile; provenance: Provenance;
  supplied?: Preferences;
}): Preferences {
  // Explicit missing counts remain unresolved. A supplied zero is also preserved.
  if (options.supplied !== undefined) return structuredClone(options.supplied);
  const { seed, simulationId, chamber, collegeId, listId, eligible, parameters: p, profile } = options;
  const strengths = new Map(eligible.map(c => [c.id, candidatePreferenceStrength(seed, simulationId, chamber, collegeId, listId, c.id, p, profile)]));
  const intensity = Math.min(profile.tripleWeight === 0 ? 2 : 3, p.intensity * profile.intensityMultiplier);
  const distribution = preferenceCombinationDistribution(eligible, strengths, intensity, profile.tripleWeight);
  const counts = multinomial(options.ballotBudget, distribution.map(d => d.probability),
    generationRng(seed, simulationId, 'preference-combinations', chamber, collegeId, listId));
  const ballots = distribution.map((d, i) => ({ candidateIds: d.candidateIds, count: counts[i] }));
  return { kind: 'counts', counts: eligible.map(c => ({ candidateId: c.id,
    count: sum(ballots.filter(b => b.candidateIds.includes(c.id)).map(b => b.count)) })),
  provenance: options.provenance, validity: { kind: 'combinations', ballots } };
}
