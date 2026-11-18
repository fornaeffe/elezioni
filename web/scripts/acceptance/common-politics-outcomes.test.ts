import { expect, it } from 'vitest';
import { assessCampaignLegalOutcomes } from './common-politics-outcomes';

const expected = { code: 'ROSATELLUM_EXECUTION_ERROR', simulationId: 2,
  message: 'Non ho a chi dare il seggio eccedente', chamber: 'camera', reason: 'scrutiny-failed' };
const simulations = [{ id: 1, outcome: 'complete' }, { id: 2, outcome: 'failed' }];
it('accepts the preserved Camera error while retaining failed membership', () => {
  expect(assessCampaignLegalOutcomes('politiche-r-parity-v1', simulations, [expected])).toEqual({
    accepted: true, acceptedFailureIds: [2], unexpectedSimulationIds: [], unmatchedErrorSimulationIds: [] });
  expect(simulations[1].outcome).toBe('failed');
});
it('rejects unexplained failures, changed diagnostics, other chambers and other laws', () => {
  for (const errors of [[], [{ ...expected, message: 'another failure' }], [{ ...expected, chamber: 'senato' }],
    [expected, { ...expected, reason: 'infrastructure' }]])
    expect(assessCampaignLegalOutcomes('politiche-r-parity-v1', simulations, errors).accepted).toBe(false);
  expect(assessCampaignLegalOutcomes('politiche-reform-2026-v1', simulations, [expected]).accepted).toBe(false);
});
it('rejects error membership inconsistent with complete or absent simulations', () => {
  expect(assessCampaignLegalOutcomes('politiche-r-parity-v1', [{ id: 2, outcome: 'complete' }], [expected]).accepted).toBe(false);
  expect(assessCampaignLegalOutcomes('politiche-r-parity-v1', [{ id: 1, outcome: 'complete' }], [expected])
    .unmatchedErrorSimulationIds).toEqual([2]);
});
