import { expect, it } from 'vitest';
import { errorDiagnostic, restoreOperationError } from '../../core/operation-error';
import { referenceMembers } from '../reform-2026/vote-share/reference-plans';
import { politicsErrorDiagnostic } from './error-diagnostic';

it('sends the established reference failure as a code and plain diagnostic', () => {
  const settings: Parameters<typeof referenceMembers>[0] = {
    components: [], lists: { camera: [], senato: [] }, references: [], nonValidReferences: [],
  };
  const reference: Parameters<typeof referenceMembers>[1] = {
    id: 'orphan', subject: { kind: 'component', id: 'orphan' }, scope: { kind: 'national' },
    channel: 'common', mean: 0.1, sdPercentagePoints: 2,
  };
  let caught: unknown;
  try { referenceMembers(settings, reference, 'camera'); } catch (error) { caught = error; }
  const diagnostic = politicsErrorDiagnostic(caught, 'POLITICS_WORKER_ERROR');
  expect(diagnostic).toMatchObject({
    code: 'REFERENCE_SUBJECT', name: 'Error', message: 'component references require a declared new component',
  });
  expect(restoreOperationError(structuredClone(diagnostic))).toMatchObject({ code: diagnostic.code, message: diagnostic.message });
});

it('preserves native names, stacks and causes without prefixing the message', () => {
  const cause = new TypeError('Invalid source');
  const original = new RangeError('Invalid range', { cause });
  const wire = errorDiagnostic(original, 'POLITICS_WORKER_ERROR');
  const restored = restoreOperationError(structuredClone(wire));
  expect(restored).toMatchObject({
    name: 'RangeError', message: 'Invalid range', stack: original.stack,
    cause: { name: 'TypeError', message: 'Invalid source', stack: cause.stack },
  });
});
