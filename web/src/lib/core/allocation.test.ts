import { describe, expect, test } from 'vitest';
import { dHondt, hareNiemeyer, hareNiemeyerDetails, hareNiemeyerWithMinimum } from './allocation';
import { createSeededRng } from './rng';

describe('allocation primitives', () => {
  test('allocates seats with Hare-Niemeyer highest remainders', () => {
    expect(hareNiemeyer([100, 80, 30], 5, createSeededRng('hn-basic'))).toEqual([2, 2, 1]);
  });

  test('returns Hare-Niemeyer details for fixture parity', () => {
    const details = hareNiemeyerDetails([100, 80, 30], 5, createSeededRng('hn-details'));

    expect(details.assigned).toEqual([2, 2, 1]);
    expect(details.remainderSeats).toEqual([0, 1, 1]);
    expect(details.remainders.map((value) => Number(value.toFixed(6)))).toEqual([16, 38, 30]);
  });

  test('allocates Hare-Niemeyer seats after minimum guarantees', () => {
    expect(hareNiemeyerWithMinimum([100, 80, 30], 5, [1, 0, 0], createSeededRng('hn-min'))).toEqual([
      2, 2, 1
    ]);
  });

  test('allocates seats with D Hondt quotients', () => {
    expect(dHondt([100, 80, 30], 5, undefined, createSeededRng('dhondt-basic'))).toEqual([3, 2, 0]);
  });

  test('respects candidate caps in D Hondt allocations', () => {
    expect(dHondt([100, 80, 30], 5, [1, 5, 5], createSeededRng('dhondt-caps'))).toEqual([1, 3, 1]);
  });

  test('seeded tie-breaks are reproducible', () => {
    const left = dHondt([100, 100], 1, undefined, createSeededRng('same-seed'));
    const right = dHondt([100, 100], 1, undefined, createSeededRng('same-seed'));

    expect(left).toEqual(right);
  });
});
