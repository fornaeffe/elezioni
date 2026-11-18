import { expect, it } from 'vitest';
import { requireBrowserAcceptance, requireUnitAcceptance } from './reports';

it('blocks unit skips, TODOs, failures and empty suites', () => {
  const report = { success: true, numTotalTests: 3, numPassedTests: 3, numPendingTests: 0, numTodoTests: 0 };
  expect(requireUnitAcceptance(report)).toEqual({ passed: 3, skipped: 0 });
  for (const change of [{ numPendingTests: 1 }, { numTodoTests: 1 }, { success: false }, { numPassedTests: 2 }, { numTotalTests: 0 }]) expect(() => requireUnitAcceptance({ ...report, ...change })).toThrow();
});
it('blocks browser skips, expected failures, retries, global errors and empty suites', () => {
  const test = { expectedStatus: 'passed', results: [{ status: 'passed' }] };
  const report = (t = test) => ({ suites: [{ suites: [{ specs: [{ tests: [t] }] }] }] });
  expect(requireBrowserAcceptance(report())).toEqual({ passed: 1, skipped: 0 });
  for (const t of [{ ...test, expectedStatus: 'skipped' }, { ...test, expectedStatus: 'failed' }, { ...test, results: [{ status: 'skipped' }] }, { ...test, results: [{ status: 'failed' }, { status: 'passed' }] }]) expect(() => requireBrowserAcceptance(report(t))).toThrow();
  expect(() => requireBrowserAcceptance({ ...report(), errors: ['server error'] })).toThrow();
  expect(() => requireBrowserAcceptance({ suites: [] })).toThrow();
});
