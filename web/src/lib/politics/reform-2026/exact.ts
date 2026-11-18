import type { ExactFraction } from './result';

type Integer = number | bigint | string;

function integer(value: Integer): bigint {
  if (typeof value === 'number' && (!Number.isSafeInteger(value) || value < 0)) {
    throw new RangeError('Expected a nonnegative safe integer');
  }
  if (typeof value === 'string' && !/^(0|[1-9][0-9]*)$/.test(value)) {
    throw new RangeError('Expected a nonnegative decimal integer string');
  }
  const result = BigInt(value);
  if (result < 0n) throw new RangeError('Expected a nonnegative integer');
  return result;
}

/** Nonnegative legal quantities; bigint stays internal to the JSON-safe boundary. */
export function exactFraction(numerator: Integer, denominator: Integer = 1): ExactFraction {
  let n = integer(numerator);
  let d = integer(denominator);
  if (d === 0n) throw new RangeError('Undefined fraction: zero denominator');
  let a = n;
  let b = d;
  while (b !== 0n) [a, b] = [b, a % b];
  n /= a;
  d /= a;
  return { numerator: n.toString(), denominator: d.toString() };
}

function parts(value: ExactFraction): readonly [bigint, bigint] {
  const canonical = exactFraction(value.numerator, value.denominator);
  return [BigInt(canonical.numerator), BigInt(canonical.denominator)];
}

export function compareExact(left: ExactFraction, right: ExactFraction): -1 | 0 | 1 {
  const [a, b] = parts(left);
  const [c, d] = parts(right);
  const difference = a * d - c * b;
  return difference < 0n ? -1 : difference > 0n ? 1 : 0;
}

export function addExact(left: ExactFraction, right: ExactFraction): ExactFraction {
  const [a, b] = parts(left);
  const [c, d] = parts(right);
  return exactFraction(a * d + c * b, b * d);
}

export function subtractExact(left: ExactFraction, right: ExactFraction): ExactFraction {
  const [a, b] = parts(left);
  const [c, d] = parts(right);
  return exactFraction(a * d - c * b, b * d);
}

export function multiplyExact(left: ExactFraction, right: ExactFraction): ExactFraction {
  const [a, b] = parts(left);
  const [c, d] = parts(right);
  return exactFraction(a * c, b * d);
}

export function divideExact(left: ExactFraction, right: ExactFraction): ExactFraction {
  const [a, b] = parts(left);
  const [c, d] = parts(right);
  return exactFraction(a * d, b * c);
}

/** A decimal string because the integer part need not fit in a JS number. */
export function floorExact(value: ExactFraction): string {
  const [n, d] = parts(value);
  return (n / d).toString();
}

export function fractionalPart(value: ExactFraction): ExactFraction {
  const [n, d] = parts(value);
  return exactFraction(n % d, d);
}

/**
 * C83(1-ter)(d), C83-bis(1-bis)(b), S17(5)(b):
 * "il cui valore è troncato alla sesta cifra decimale".
 * floor(x * 10^6) / 10^6, never rounded. C83 minority symmetry is policy I05;
 * callers apply it to both active sides and retain I05 in run metadata.
 */
export function truncateToSixDecimals(value: ExactFraction): ExactFraction {
  const [n, d] = parts(value);
  return exactFraction(n * 1_000_000n / d, 1_000_000n);
}
