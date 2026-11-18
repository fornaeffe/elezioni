import { expect, it } from 'vitest';
import { acceptanceProfile, requireBrowserCases, requireIdenticalArtifacts } from './policy';
const browser = (count: number) => ({ suites: [{ specs: Array.from({ length: count }, () => ({ tests: [
  { expectedStatus: 'passed', results: [{ status: 'passed' }] }
] })) }] });
it('requires a named profile and rejects an accidental partial or expanded matrix', () => {
  expect(acceptanceProfile()).toBe('release');
  expect(acceptanceProfile('development')).toBe('development');
  expect(acceptanceProfile('extended')).toBe('extended');
  expect(() => acceptanceProfile('fast')).toThrow();
  expect(requireBrowserCases(browser(2), 2).passed).toBe(2);
  expect(() => requireBrowserCases(browser(1), 2)).toThrow();
  expect(() => requireBrowserCases(browser(3), 2)).toThrow();
});
it('requires matching artifact membership, sizes and hashes independent of ordering', () => {
  const a = { path: 'a', bytes: 1, sha256: 'aaa' }, b = { path: 'b', bytes: 2, sha256: 'bbb' };
  expect(requireIdenticalArtifacts([a, b], [b, a]).compared).toBe(2);
  for (const warm of [[a], [a, a], [a, { ...b, bytes: 3 }], [a, { ...b, sha256: 'changed' }]])
    expect(() => requireIdenticalArtifacts([a, b], warm)).toThrow();
  expect(() => requireIdenticalArtifacts([], [])).toThrow();
});
