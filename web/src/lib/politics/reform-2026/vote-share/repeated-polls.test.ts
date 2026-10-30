import { it, expect } from 'vitest';
import { syntheticNational } from '../../../../../tests/fixtures/fast-share-scale';
import { buildPoliticalTarget } from './information';
it('exact repeated-poll sufficient statistics preserve the full unconstrained precision and mode', () => {
  const source = syntheticNational(8, 80);
  const direct = buildPoliticalTarget(source, {
      solver: 'dense',
      collapseRepeatedPolls: false,
    }),
    collapsed = buildPoliticalTarget(source, { solver: 'dense' });
  expect(collapsed.audit.covarianceRows).toBeLessThan(
    direct.audit.observations,
  );
  expect(collapsed.audit.observations).toBe(direct.audit.observations);
  for (let i = 0; i < direct.target.precision.length; i++)
    expect(
      Math.abs(collapsed.target.precision[i] - direct.target.precision[i]) /
        Math.max(1, Math.abs(direct.target.precision[i])),
    ).toBeLessThan(1e-9);
  for (let i = 0; i < direct.mode.point.length; i++)
    expect(
      Math.abs(collapsed.mode.point[i] - direct.mode.point[i]),
    ).toBeLessThan(1e-10);
});
it('retains partial offered sets and distinct error groups independently', () => {
  const source = syntheticNational(4, 2);
  source.polls[1] = {
    ...source.polls[0],
    id: 'independent',
    surveyId: 'independent',
    errorGroup: 'different',
  };
  const full = buildPoliticalTarget(source);
  expect(full.audit.covarianceRows).toBeUndefined();
  source.polls[1].errorGroup = source.polls[0].errorGroup;
  source.polls = source.polls.map((p) => ({
    ...p,
    groups: [['p000'], ['p001']],
    shares: [0.4, 0.6],
  }));
  const partial = buildPoliticalTarget(source);
  expect(partial.audit.covarianceRows).toBeUndefined();
});
