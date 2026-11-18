import { describe, expect, it } from 'vitest';
import { addExact, compareExact, divideExact, exactFraction as f, floorExact, fractionalPart, multiplyExact, subtractExact, truncateToSixDecimals } from './exact';

describe('exact nonnegative legal arithmetic', () => {
  it('normalizes fractions and stays JSON/worker safe beyond the JS integer range', () => {
    expect(f(0, 17)).toEqual({ numerator: '0', denominator: '1' });
    expect(f(14, 6)).toEqual({ numerator: '7', denominator: '3' });
    const value = addExact(f('9007199254740991'), f(2));
    expect(value).toEqual({ numerator: '9007199254740993', denominator: '1' });
    expect(structuredClone(JSON.parse(JSON.stringify(value)))).toEqual(value);
  });

  it('uses exact cross-products even where double precision cannot distinguish ratios', () => {
    const a = f('9007199254740993', '9007199254740992');
    const b = f('9007199254740992', '9007199254740991');
    expect(compareExact(a, b)).toBe(-1);
    expect(compareExact(b, a)).toBe(1);
    expect(compareExact(f(2, 3), f(4, 6))).toBe(0);
  });

  it('adds, subtracts, multiplies, divides and separates integer/decimal parts exactly', () => {
    expect(addExact(f(2, 3), f(3, 5))).toEqual(f(19, 15));
    expect(subtractExact(f(2, 3), f(3, 5))).toEqual(f(1, 15));
    expect(multiplyExact(f(2, 3), f(3, 5))).toEqual(f(2, 5));
    expect(divideExact(f(2, 3), f(3, 5))).toEqual(f(10, 9));
    expect(floorExact(f(100, 7))).toBe('14');
    expect(fractionalPart(f(100, 7))).toEqual(f(2, 7));
  });

  it.each([3, 10, 20, 42])('keeps the inclusive %i percent threshold exact', (percent) => {
    const boundary = f(percent, 100);
    expect(compareExact(f(percent * 100, 10000), boundary)).toBe(0);
    expect(compareExact(f(percent * 100 - 1, 10000), boundary)).toBe(-1);
    expect(compareExact(f(percent * 100 + 1, 10000), boundary)).toBe(1);
  });

  it.each([
    [f(2, 3), f(666666, 1000000)],
    [f(12345669, 10000000), f(1234566, 1000000)],
    [f(1234567, 1000000), f(1234567, 1000000)],
    [f(9999999, 10000000), f(999999, 1000000)],
    [f(9, 10000000), f(0)],
    [f(0), f(0)],
    [f('90071992547409930000009', '10000000'), f('9007199254740993000000', '1000000')]
  ])('truncates at the sixth decimal without rounding: %j', (value, expected) => {
    expect(truncateToSixDecimals(value)).toEqual(expected);
  });

  it.each([-1, 0.1, Infinity, NaN, Number.MAX_SAFE_INTEGER + 1, '-1', '01', '1.5', '1e3', ''])('rejects invalid integer input %s', (value) => {
    expect(() => f(value)).toThrow(RangeError);
  });

  it('rejects undefined and negative arithmetic rather than manufacturing zero', () => {
    expect(() => f(1, 0)).toThrow(RangeError);
    expect(() => divideExact(f(1), f(0))).toThrow(RangeError);
    expect(() => subtractExact(f(0), f(1))).toThrow(RangeError);
    expect(() => compareExact({ numerator: '1', denominator: '0' }, f(1))).toThrow(RangeError);
  });
});
