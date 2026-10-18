import { allocateByQuotaAndRemainders } from './allocation';
import { exactFraction } from './exact';
import type { DrawTrace, ReformWarning, ScrutinyExecutionContext } from './result';

export interface PopulationPart {
  readonly id: string;
  readonly observed: number | null;
  readonly historicalWeight: number;
}

/** Preserve the legal municipal total. Missing fine geography is an explicit estimate, not zero. */
export function reconcilePopulation(
  population: number,
  parts: readonly PopulationPart[],
  execution: ScrutinyExecutionContext,
  contextId: string
): { populations: readonly { id: string; population: number }[]; estimated: boolean; method: string;
  draws: readonly DrawTrace[]; warnings: readonly ReformWarning[] } {
  const valid = (n: number) => Number.isSafeInteger(n) && n >= 0;
  if (!valid(population) || !parts.length || new Set(parts.map(p => p.id)).size !== parts.length ||
      parts.some(p => !p.id || !valid(p.historicalWeight) || (p.observed !== null && !valid(p.observed)))) throw new Error('Invalid population reconciliation input');
  const observed = parts.reduce((s, p) => s + (p.observed ?? 0), 0);
  if (!valid(observed)) throw new Error('Unsafe aggregate population');
  if (parts.every(p => p.observed !== null) && observed === population) {
    return { populations: parts.map(p => ({ id: p.id, population: p.observed! })), estimated: false, method: 'complete-census-parts', draws: [], warnings: [] };
  }
  const missing = parts.filter(p => p.observed === null);
  // Keep available observations when they fit; allocate only the missing residue.
  const preserve = missing.length > 0 && observed <= population;
  const active = preserve ? missing : parts;
  const weights = active.map(p => ({ id: p.id, weight: p.observed ?? p.historicalWeight }));
  if (!weights.some(w => w.weight > 0)) weights.forEach((w, i) => { w.weight = active[i].historicalWeight; });
  if (!weights.some(w => w.weight > 0)) for (const w of weights) w.weight = 1;
  const allocation = allocateByQuotaAndRemainders({ subjects: weights.map(w => ({ id: w.id, figure: exactFraction(w.weight), tieValues: [] })),
    seats: preserve ? population - observed : population, quotaMode: 'exact', tieCriteria: [],
    context: { simulationId: 0, request: { contextId, chamber: null, norm: 'N03', rule: 'I03', lawReference: 'Population reconstruction; user-approved estimation, not a statutory seat formula' } }
  }, execution);
  if (allocation.status !== 'complete') throw new Error(`Population estimate failed: ${allocation.reason}`);
  const byId = new Map(allocation.assignments.map(a => [a.id, a.seats]));
  return { populations: parts.map(p => ({ id: p.id, population: preserve && p.observed !== null ? p.observed : byId.get(p.id)! })), estimated: true,
    method: preserve ? 'observed-parts-plus-weighted-residue' : 'rescaled-census-or-historical-weights',
    draws: allocation.draws, warnings: allocation.warnings };
}
