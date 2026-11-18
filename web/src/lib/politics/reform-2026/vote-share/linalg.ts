import { requireWorkspace, type Vector } from './types';

export function dot(a: ArrayLike<number>, b: ArrayLike<number>): number {
  let total = 0, correction = 0;
  for (let i = 0; i < a.length; i++) {
    const value = a[i] * b[i] - correction, next = total + value;
    correction = (next - total) - value; total = next;
  }
  return total;
}
export interface SpdFactor { lower: Vector; scale: Vector; size: number; pivotRatio: number }
/** Diagonal equilibration is algebraic, not a new statistical ridge. */
export function factorSpd(a: ArrayLike<number>, n: number): SpdFactor {
  if (!Number.isInteger(n) || n < 1 || a.length !== n * n) throw new RangeError('Invalid SPD dimensions');
  const scale = new Float64Array(n), lower = new Float64Array(n * n);
  for (let i = 0; i < n; i++) {
    if (!(a[i * n + i] > 0) || !Number.isFinite(a[i * n + i])) throw new Error('Nonpositive SPD diagonal');
    scale[i] = 1 / Math.sqrt(a[i * n + i]);
  }
  let smallest = Infinity, largest = 0;
  for (let i = 0; i < n; i++) for (let j = 0; j <= i; j++) {
    let value = a[i * n + j] * scale[i] * scale[j];
    if (!Number.isFinite(value) || Math.abs(a[i * n + j] - a[j * n + i]) >
      1e-12 * Math.max(1, Math.abs(a[i * n + j]), Math.abs(a[j * n + i]))) throw new Error('Invalid asymmetric SPD matrix');
    for (let k = 0; k < j; k++) value -= lower[i * n + k] * lower[j * n + k];
    if (i === j) {
      if (!(value > 0)) throw new Error('Nonpositive SPD pivot at ' + i + '; no covariance repair applied');
      lower[i * n + j] = Math.sqrt(value);
      smallest = Math.min(smallest, value); largest = Math.max(largest, value);
    } else lower[i * n + j] = value / lower[j * n + j];
  }
  return { lower, scale, size: n, pivotRatio: smallest / largest };
}
export function solveSpd(f: SpdFactor, rhs: ArrayLike<number>, out = new Float64Array(f.size)): Vector {
  const { lower: l, size: n, scale } = f;
  if (rhs.length !== n || out.length !== n) throw new RangeError('Invalid solve dimensions');
  for (let i = 0; i < n; i++) {
    let value = rhs[i] * scale[i];
    for (let j = 0; j < i; j++) value -= l[i * n + j] * out[j];
    out[i] = value / l[i * n + i];
  }
  for (let i = n - 1; i >= 0; i--) {
    let value = out[i];
    for (let j = i + 1; j < n; j++) value -= l[j * n + i] * out[j];
    out[i] = value / l[i * n + i];
  }
  for (let i = 0; i < n; i++) out[i] *= scale[i];
  return out;
}
export function multiply(a: ArrayLike<number>, x: ArrayLike<number>, n: number, out = new Float64Array(n)): Vector {
  for (let i = 0; i < n; i++) {
    let value = 0;
    for (let j = 0; j < x.length; j++) value += a[i * x.length + j] * x[j];
    out[i] = value;
  }
  return out;
}
export function helmert(groups: number): Vector {
  const out = new Float64Array((groups - 1) * groups);
  for (let r = 0; r < groups - 1; r++) {
    const divisor = Math.sqrt((r + 1) * (r + 2));
    for (let j = 0; j <= r; j++) out[r * groups + j] = 1 / divisor;
    out[r * groups + r + 1] = -(r + 1) / divisor;
  }
  return out;
}
export interface LinearOperator {
  size: number;
  apply(x: Vector, out: Vector): void;
  precondition(x: Vector, out: Vector): void;
}
/** PCG checks the actual residual before accepting, never a repaired target. */
export function conjugateGradient(op: LinearOperator, rhs: Vector, tolerance = 1e-11, maximum = 4096) {
  const n = op.size, x = new Float64Array(n), r = rhs.slice(), z = new Float64Array(n);
  const p = new Float64Array(n), applied = new Float64Array(n);
  const norm = Math.sqrt(dot(rhs, rhs));
  if (!norm) return { solution: x, iterations: 0, relativeResidual: 0 };
  op.precondition(r, z); p.set(z); let rz = dot(r, z);
  if (!(rz > 0)) throw new Error('Invalid PCG preconditioner');
  for (let iteration = 0; iteration < maximum; iteration++) {
    op.apply(p, applied); const denominator = dot(p, applied);
    if (!(denominator > 0)) throw new Error('Nonpositive covariance direction');
    const alpha = rz / denominator;
    for (let i = 0; i < n; i++) { x[i] += alpha * p[i]; r[i] -= alpha * applied[i]; }
    if (Math.sqrt(dot(r, r)) <= tolerance * norm || iteration % 64 === 63) {
      op.apply(x, applied);
      for (let i = 0; i < n; i++) r[i] = rhs[i] - applied[i];
      const residual = Math.sqrt(dot(r, r)) / norm;
      if (residual <= tolerance) return { solution: x, iterations: iteration + 1, relativeResidual: residual };
      // Restart after residual replacement to avoid accumulated loss of conjugacy.
      op.precondition(r, z); p.set(z); rz = dot(r, z); continue;
    }
    op.precondition(r, z); const next = dot(r, z), beta = next / rz;
    if (!(next > 0) || !Number.isFinite(next)) throw new Error('Invalid PCG residual');
    for (let i = 0; i < n; i++) p[i] = z[i] + beta * p[i];
    rz = next;
  }
  throw new Error('COVARIANCE_SOLVE_FAILED: PCG did not meet its residual tolerance');
}
export function denseFromOperator(op: LinearOperator, maximumBytes?: number): Vector {
  requireWorkspace(op.size * op.size * 8 + op.size * 16, maximumBytes);
  const out = new Float64Array(op.size * op.size), unit = new Float64Array(op.size), column = new Float64Array(op.size);
  for (let j = 0; j < op.size; j++) {
    unit[j] = 1; op.apply(unit, column); unit[j] = 0;
    for (let i = 0; i < op.size; i++) out[i * op.size + j] = column[i];
  }
  for (let i = 0; i < op.size; i++) for (let j = 0; j < i; j++)
    out[i * op.size + j] = out[j * op.size + i] = (out[i * op.size + j] + out[j * op.size + i]) / 2;
  return out;
}
