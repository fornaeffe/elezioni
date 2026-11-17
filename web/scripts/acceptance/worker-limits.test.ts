import { expect, it } from 'vitest';
import { acceptanceWorkers } from './worker-limits';

it('separates hosted browser contexts from unit workers and honors explicit overrides', () => {
  expect(acceptanceWorkers({ CI: 'true' })).toEqual({ units: 2, browsers: 1 });
  expect(acceptanceWorkers({})).toEqual({ units: 2, browsers: 2 });
  expect(acceptanceWorkers({ CI: 'true', ACCEPTANCE_UNIT_WORKERS: '4' })).toEqual({ units: 4, browsers: 1 });
  expect(acceptanceWorkers({ ACCEPTANCE_TEST_WORKERS: '3', ACCEPTANCE_BROWSER_WORKERS: '1' })).toEqual({ units: 3, browsers: 1 });
});
it.each(['0', '65', '-1', '1.5', 'NaN', ''])('rejects invalid worker limits %s', value => {
  for (const name of ['ACCEPTANCE_UNIT_WORKERS', 'ACCEPTANCE_BROWSER_WORKERS', 'ACCEPTANCE_TEST_WORKERS'])
    expect(() => acceptanceWorkers({ [name]: value })).toThrow('integer from 1 through 64');
});
