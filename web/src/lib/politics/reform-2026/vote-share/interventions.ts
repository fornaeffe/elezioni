import { projectMargins } from './projection';
import { sum } from './types';
export function removeAbsent(input: ArrayLike<number>, offered: readonly boolean[]) {
  if (input.length !== offered.length || !offered[0]) throw new Error('Invalid elector choice set');
  const p = Float64Array.from(input, (x, i) => (offered[i] ? x : 0)),
    mass = sum(p);
  if (!(mass > 0) || p.some((x) => !Number.isFinite(x) || x < 0)) throw new Error('Empty country choice set');
  for (let j = 0; j < p.length; j++) p[j] /= mass;
  return p;
}
/** Scoped group raking with immutable per-cell turnout and within-group/free
 * proportions. fallback is explicit original support or declared new policy,
 * not epsilon support for an unfiled/unsupported fitted component. */
export function politicalIntervention(
  rows: Float64Array[],
  electors: number[],
  groups: number[][] | ((i: number) => number[][]),
  values: number[],
  fallback: (i: number, j: number) => number,
  options: { chunkSize?: number } = {}
) {
  if (!rows.length || values.some((v) => !Number.isFinite(v) || v < 0) || sum(values) > 1 + 1e-12)
    throw new Error('REFERENCE_CONFLICT');
  const n = rows[0].length,
    original = rows.map((_, i) => (typeof groups === 'function' ? groups(i) : groups));
  for (const g of original)
    if (
      g.length !== values.length ||
      g.flat().some((j) => j < 1 || j >= n) ||
      new Set(g.flat()).size !== g.flat().length
    )
      throw new Error('REFERENCE_OVERLAP');
  const free = original.map((g, i) => {
    const used = new Set(g.flat());
    return Array.from({ length: n - 1 }, (_, j) => j + 1).filter(
      (j) => !used.has(j) && (rows[i][j] > 0 || fallback(i, j) > 0)
    );
  });
  const hasFree = free.some((g) => g.length > 0),
    sets = original.map((g, i) => [...g, ...(hasFree ? [free[i]] : [])]);
  const target = [...values, ...(hasFree ? [Math.max(0, 1 - sum(values))] : [])];
  if (!hasFree && Math.abs(sum(values) - 1) > 1e-12) throw new Error('REFERENCE_CONFLICT: full target');
  const base = rows.map((row, i) =>
    sets[i].map((members) => {
      const mass = sum(members.map((j) => row[j]));
      return members.map((j) => (mass > 0 ? row[j] / (1 - row[0]) : fallback(i, j)));
    })
  );
  const masses = base.map((gs) => gs.map(sum));
  const fit = projectMargins(
    target,
    rows.length,
    (i) => ({
      weight: electors[i] * (1 - rows[i][0]),
      scores: masses[i].map((x) => (x > 0 ? Math.log(x) : -Infinity))
    }),
    options
  );
  rows.forEach((row, i) => {
    const p = fit.probabilities(i),
      valid = 1 - row[0];
    row.fill(0, 1);
    for (let g = 0; g < sets[i].length; g++)
      sets[i][g].forEach((j, k) => {
        row[j] = masses[i][g] ? (valid * p[g] * base[i][g][k]) / masses[i][g] : 0;
      });
  });
  return { residual: fit.residual, iterations: fit.iterations };
}
/** Non-valid references use elector denominators and preserve political ratios. */
export function nonValidIntervention(
  rows: Float64Array[],
  electors: number[],
  target: number,
  fallback: (i: number, j: number) => number,
  options: { chunkSize?: number } = {}
) {
  if (!(target >= 0 && target <= 1)) throw new Error('REFERENCE_CONFLICT: non-valid');
  const political = rows.map((r, i) => {
    const p = Float64Array.from(r.slice(1), (x, j) => (1 - r[0] > 0 ? x / (1 - r[0]) : fallback(i, j + 1))),
      mass = sum(p);
    if (mass) for (let j = 0; j < p.length; j++) p[j] /= mass;
    return p;
  });
  const fit = projectMargins(
    [target, 1 - target],
    rows.length,
    (i) => ({
      weight: electors[i],
      scores: [
        Math.log(rows[i][0] > 0 ? rows[i][0] : 0.5), // An explicit non-valid reference is itself the new support policy.
        sum(political[i]) ? Math.log(1 - rows[i][0] > 0 ? 1 - rows[i][0] : 0.5) : -Infinity
      ]
    }),
    options
  );
  rows.forEach((r, i) => {
    const p = fit.probabilities(i);
    r[0] = p[0];
    for (let j = 1; j < r.length; j++) r[j] = p[1] * political[i][j - 1];
  });
  return { residual: fit.residual, iterations: fit.iterations };
}
