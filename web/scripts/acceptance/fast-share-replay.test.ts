import { it, expect } from 'vitest';
import { score, quantile } from './fast-share-replay';
it('matches hand-computed linear quantiles and proper interval scores', () => {
  const s = score([0, 1, 2, 3], 2);
  expect(quantile([0, 1, 2, 3], 0.05)).toBeCloseTo(0.15, 12);
  expect(s.mean).toBe(1.5);
  expect(s.meanBias).toBe(-0.5);
  expect(s.meanMae).toBe(0.5);
  expect(s.width90).toBeCloseTo(2.7, 12);
  expect(s.coverage90).toBe(1);
  expect(s.wis).toBeCloseTo((0.25 + 0.375 + 0.24 + 0.135 + 0.07125) / 4.5, 12);
});
