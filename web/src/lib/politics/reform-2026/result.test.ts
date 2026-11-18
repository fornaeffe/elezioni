import { expect, it } from 'vitest';
import type { DrawTrace, NormId, ReformScrutinyResult, SeatLocation } from './index';
import { jointFixture } from './contracts.fixture';

it('keeps conditional partial results, I03 draws, I06 state and I07 destinations transferable', () => {
  const input = jointFixture();
  const original: SeatLocation = { chamber: 'senato', territoryId: 'original-region', collegeId: 'original-college', subject: { kind: 'coalition', id: 'C' }, listId: 'A' };
  const destination: SeatLocation = { ...original, territoryId: 'other-region', collegeId: 'other-college' };
  const draw: DrawTrace = { contextId: 'N20/cap-seat/tie-1', chamber: 'senato', norm: 'N20', rule: 'I03', lawReference: 'S16-bis(1-ter)/I03', alternativeIds: ['r1', 'r2'], chosenId: 'r1', streamId: 'legal-draws-v1/cap' };
  const result: ReformScrutinyResult = {
    schemaVersion: 1, algorithmId: input.algorithmId, simulationId: input.simulationId,
    scope: 'domestic-initial', renewedChambers: ['camera', 'senato'], status: 'partial',
    legalVersion: input.legalVersion, geographyVersion: input.geographyVersion,
    chamberModel: input.chamberModel, seatTables: [], provenance: [],
    interpretations: [{ id: 'I03', version: 'provisional-v1' }, { id: 'I06', version: 'provisional-v1' }, { id: 'I07', version: 'approved-2026-10-16' }],
    conditions: [{ kind: 'missing-preferences', chamber: 'camera', affectedIds: ['O0'], reference: 'No candidate ranking input' }],
    subjectDecisions: [{ norm: 'N12', chamber: 'senato', subject: original.subject, decision: 'bonus-absent', reason: 'Other chamber condition not satisfied', lawReference: 'S16-bis' }],
    warnings: [
      { code: 'I07_CROSS_REGION', electionKind: 'politiche', simulationId: 1, severity: 'warning', message: 'Provisional cross-region recovery', chamber: 'senato', interpretationId: 'I07', seatId: 'seat-1', lawReference: 'S19(2)/C86(2)/C84(4)' },
      { code: 'I03_DRAW', electionKind: 'politiche', simulationId: 1, severity: 'warning', message: 'Final tie resolved by provisional draw', interpretationId: 'I03', drawContextId: draw.contextId }
    ],
    stages: [{ norm: 'N25', chamber: 'senato', status: 'complete' }, { norm: 'N24', chamber: 'camera', status: 'partial', reason: 'Missing preferences', affectedIds: ['O0'] }],
    draws: [draw],
    seats: [
      { seatId: 'seat-1', category: 'ordinary', entitlement: original, status: 'filled', electedAt: destination, candidateId: 'b' },
      { seatId: 'seat-2', category: 'bonus', entitlement: original, status: 'unfilled', reason: 'No permitted replacement' },
      { seatId: 'seat-3', category: 'ordinary', entitlement: original, status: 'unresolved', reason: 'Missing ranking input' }
    ],
    transfers: [{ seatId: 'cap-seat', chamber: 'senato', norm: 'N20', lawReference: 'S16-bis(1-ter)', interpretationId: 'I06', from: original, to: { ...original, subject: { kind: 'list', id: 'B' }, listId: 'B' }, remainders: [{ id: 'r1', value: { numerator: '9007199254740993', denominator: '7' }, before: 'used', after: 'consumed' }], drawContextIds: [draw.contextId] }],
    recoveries: [{ seatId: 'seat-1', route: 'same-list-cross-region', lawReference: 'S19(2)/C86(2)/C84(4)', interpretationId: 'I07', originalEntitlement: original, electedAt: destination, candidateId: 'b', consideredCandidateIds: ['a', 'b'], remainders: [], drawContextIds: [], outcome: 'filled' }]
  };
  const restored: ReformScrutinyResult = structuredClone(JSON.parse(JSON.stringify(result)));
  expect(restored).toEqual(result);
  expect(restored.seats[0].entitlement.territoryId).not.toBe(destination.territoryId);
  expect(restored.transfers[0].remainders[0].value.numerator).toBe('9007199254740993');
});

// Compile-time boundaries are checked by npm run check, not by an executed algorithm.
const firstNorm: NormId = 'N01';
const lastNorm: NormId = 'N29';
// @ts-expect-error There is no N00 in the normative matrix.
const invalidNorm: NormId = 'N00';
void [firstNorm, lastNorm, invalidNorm];
