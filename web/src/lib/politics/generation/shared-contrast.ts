/** Fixed descriptive contrasts transport one common field, never a second shock.
 * Endpoint overrides retain explicit zero/one. Positive support must be supplied
 * to the common field before movement, rather than fabricated during transport. */
const logit = (p: number) => Math.log(p / (1 - p));
const logistic = (z: number) => z >= 0 ? 1 / (1 + Math.exp(-z)) : Math.exp(z) / (1 + Math.exp(z));
export function contrastBetween(from: Float64Array, to: Float64Array) {
  const gap = new Float64Array(from.length);
  if (from.every((p, j) => p === to[j])) return gap;
  gap[0] = to[0] === 0 ? -Infinity : to[0] === 1 ? Infinity : logit(to[0]) - logit(from[0]);
  for (let j = 1; j < gap.length; j++) {
    if (to[j] > 0 && !(from[j] > 0)) throw new Error('COMMON_FIELD_SUPPORT_REQUIRED');
    gap[j] = from[j] === 0 && to[j] === 0 ? 0 : to[j] > 0 ? Math.log(to[j] / (1 - to[0])) - Math.log(from[j] / (1 - from[0])) : -Infinity;
  }
  return gap;
}
export function transportContrast(row: Float64Array, gap: Float64Array) {
  if (gap.every(x => x === 0)) return row.slice();
  if (row.length !== gap.length) throw new Error('CHAMBER_CONTRAST_BASIS_MISMATCH');
  const a = gap[0] === -Infinity ? 0 : gap[0] === Infinity ? 1 :
    row[0] === 0 || row[0] === 1 ? row[0] : logistic(logit(row[0]) + gap[0]);
  const scores = row.slice(1).map((p, j) => p > 0 ? Math.log(p) + gap[j + 1] : -Infinity);
  const max = Math.max(...scores), weights = scores.map(x => Math.exp(x - max));
  const total = weights.reduce((s, x) => s + x, 0);
  if (!(total > 0)) {
    if (a === 1) return Float64Array.from([1, ...scores.map(() => 0)]);
    throw new Error('EMPTY_COMMON_POLITICAL_FIELD');
  }
  return Float64Array.from([a, ...weights.map(p => (1 - a) * p / total)]);
}
