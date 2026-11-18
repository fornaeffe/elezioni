import { createSeededRng } from '$lib/core/rng';
import { compareExact, exactFraction } from './exact';
import type { DrawRequest, DrawTrace, ExactFraction, ReformWarning, ScrutinyExecutionContext } from './result';

export interface TieCriterion {
  readonly name: string;
  readonly direction: 'ascending' | 'descending';
}
export interface TieAlternative {
  readonly id: string;
  /** One exact value per criterion, in statutory priority order. */
  readonly values: readonly ExactFraction[];
}
export interface TieContext {
  readonly simulationId: number;
  readonly request: Omit<DrawRequest, 'alternativeIds'>;
}
export interface TieResolution {
  readonly chosenId: string;
  readonly steps: readonly { readonly criterion: string; readonly remainingIds: readonly string[] }[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}

function canonicalIds(ids: readonly string[]): string[] {
  if (!ids.length || ids.some((id) => !id) || new Set(ids).size !== ids.length) {
    throw new RangeError('Draw alternatives must have unique nonempty IDs');
  }
  return [...ids].sort(); // Code-unit order, independent of locale and source-array order.
}

/** Explicit, per-run seeded stream; no shared/default RNG and no legacy stream changes. */
export function createSeededDrawContext(seed: string | number, streamId: string): ScrutinyExecutionContext {
  if (!streamId) throw new RangeError('A draw stream ID is required');
  if (typeof seed === 'number' && !Number.isSafeInteger(seed)) throw new RangeError('Invalid draw seed');
  const rng = createSeededRng(JSON.stringify([seed, streamId]));
  return {
    draw(request) {
      const alternativeIds = canonicalIds(request.alternativeIds);
      // Rejection sampling avoids modulo bias when the number of alternatives
      // does not divide the seeded RNG's 2^32 possible integer outputs.
      const range = 2 ** 32;
      const limit = range - range % alternativeIds.length;
      let sample: number;
      do { sample = Math.floor(rng() * range); } while (sample >= limit);
      return { ...request, alternativeIds, chosenId: alternativeIds[sample % alternativeIds.length], streamId };
    }
  };
}

/** Apply every express criterion first; only an unresolved final tie consumes a draw. */
export function resolveTie(
  alternatives: readonly TieAlternative[],
  criteria: readonly TieCriterion[],
  context: TieContext,
  execution: ScrutinyExecutionContext
): TieResolution {
  const ids = canonicalIds(alternatives.map((alternative) => alternative.id));
  if (!Number.isSafeInteger(context.simulationId) || context.simulationId < 0) {
    throw new RangeError('Invalid simulation ID');
  }
  for (const criterion of criteria) {
    if (!criterion.name || !['ascending', 'descending'].includes(criterion.direction)) {
      throw new RangeError('Invalid tie criterion');
    }
  }
  const normalized = alternatives.map((alternative) => {
    if (alternative.values.length !== criteria.length) throw new RangeError('Missing or extra tie values');
    return { id: alternative.id, values: alternative.values.map((value) => exactFraction(value.numerator, value.denominator)) };
  });
  let remaining = ids.map((id) => normalized.find((alternative) => alternative.id === id)!);
  const steps: { criterion: string; remainingIds: string[] }[] = [];
  for (const [index, criterion] of criteria.entries()) {
    if (remaining.length === 1) break;
    const best = remaining.reduce((left, right) => {
      const comparison = compareExact(left.values[index], right.values[index]);
      return (criterion.direction === 'descending' ? comparison >= 0 : comparison <= 0) ? left : right;
    });
    remaining = remaining.filter((alternative) => compareExact(alternative.values[index], best.values[index]) === 0);
    steps.push({ criterion: criterion.name, remainingIds: remaining.map((alternative) => alternative.id) });
  }
  if (remaining.length === 1) return { chosenId: remaining[0].id, steps, draws: [], warnings: [] };

  const request: DrawRequest = { ...context.request, alternativeIds: remaining.map((alternative) => alternative.id) };
  // Keep a separate request to detect mutation or an inconsistent injected resolver.
  const drawn = execution.draw({ ...request, alternativeIds: [...request.alternativeIds] });
  if (!drawn.streamId || !request.alternativeIds.includes(drawn.chosenId) ||
      drawn.contextId !== request.contextId || drawn.chamber !== request.chamber ||
      drawn.norm !== request.norm || drawn.rule !== request.rule || drawn.lawReference !== request.lawReference ||
      JSON.stringify(drawn.alternativeIds) !== JSON.stringify(request.alternativeIds)) {
    throw new Error('Injected draw returned an inconsistent trace');
  }
  const draw: DrawTrace = { ...request, alternativeIds: [...request.alternativeIds], chosenId: drawn.chosenId, streamId: drawn.streamId };
  const warnings: ReformWarning[] = request.rule === 'I03' ? [{
    code: 'I03_DRAW', severity: 'warning', electionKind: 'politiche',
    simulationId: context.simulationId,
    ...(request.chamber === null ? {} : { chamber: request.chamber }),
    interpretationId: 'I03', drawContextId: request.contextId,
    lawReference: request.lawReference,
    message: 'Final tie resolved by the provisional I03 seeded draw after all express legal criteria.'
  }] : [];
  return { chosenId: draw.chosenId, steps, draws: [draw], warnings };
}
