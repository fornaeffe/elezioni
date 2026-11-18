import type { ScenarioPlurinominalCandidacyCountShares } from '../core/types';

/** Convert probabilities of 1–5 candidacies to fractions of occupied slots.
 * The common domain validates the input; never consult a retired UI default. */
export function plurinominalCandidacyCountSharesToFractions(shares: readonly number[]): ScenarioPlurinominalCandidacyCountShares {
  if (shares.length !== 5 || shares.some(s => !Number.isFinite(s) || s < 0) || shares.reduce((a, b) => a + b, 0) <= 0)
    throw new Error('Invalid plurinominal candidacy count shares');
  // Retain the original arithmetic order as well as its slot-weighted meaning.
  const total = shares.reduce((sum, share) => sum + share, 0);
  const normalized = shares.map(share => share / total);
  const slots = normalized.reduce((sum, share, i) => sum + share * (i + 1), 0);
  return normalized.map((_, i) => normalized.slice(i).reduce((sum, share) => sum + share, 0) / slots) as ScenarioPlurinominalCandidacyCountShares;
}
