/** File schemas are independent of authored scenarios, models and fitted-state codecs. */
import { array, boolean, count, isCalendarDate, literal, nullable, object, optional, positive, scalar, text, union, type Infer } from '../../core/json-shape';
import { assetShape, nonnegative, scenarioShape } from './schema';

export const RESULT_ENVELOPE_VERSION = 1;
export const ARCHIVE_ENVELOPE_VERSION = 1;
export const sha256Shape = scalar<string>(v => typeof v === 'string' && /^[a-f0-9]{64}$/.test(v), 'a lowercase SHA-256');
export const timestampShape = scalar<string>(v => typeof v === 'string' &&
  /^\d{4}-\d\d-\d\dT(?:[01]\d|2[0-3]):[0-5]\d:[0-5]\d(?:\.\d{1,3})?Z$/.test(v) && isCalendarDate(v.slice(0, 10)), 'a UTC ISO timestamp');
const identity = object({ id: text, version: text });
export const executionShape = object({
  generator: identity, scrutiny: identity, adapter: identity, data: array(assetShape),
  numericalVersion: text, rngVersion: text, implementationSha256: sha256Shape,
  requestedWorkers: union(object({ kind: literal('auto'), policy: literal('model-aware-v1') }), object({ kind: literal('fixed'), count: positive })),
  actualWorkers: positive, fittingSeed: text, simulationSeed: text, startedAt: timestampShape, elapsedMs: nonnegative,
  preparationKey: text, generationKey: text, assemblyKey: text,
});
export type ExecutionProvenance = Infer<typeof executionShape>;
export const simulationEvidenceShape = object({ id: positive, outcome: literal('complete', 'partial', 'failed') });
export const sessionShape = object({ id: text, state: literal('complete', 'interrupted'), simulations: array(simulationEvidenceShape) });
export const warningShape = object({ code: text, electionKind: literal('politiche'), message: text,
  severity: literal('info', 'warning', 'error'), simulationId: optional(positive),
  lawReference: optional(text), todoReference: optional(text), chamber: optional(literal('camera', 'senato')),
  interpretationId: optional(text), seatId: optional(text), drawContextId: optional(text),
  scope: optional(literal('global', 'simulation', 'execution')), occurrences: optional(positive), reason: optional(text) });
export const cellShape = union(nullable(scalar<string>(v => typeof v === 'string', 'a string')),
  scalar<number>(v => typeof v === 'number' && Number.isFinite(v), 'a finite number'), boolean);
export const tableShape = object({ name: text, columns: array(text), rows: array(array(cellShape)) });
export const workerResultShape = object({ type: literal('result'), status: literal('completed', 'not_implemented'),
  completedSimulations: count, tables: array(tableShape), warnings: array(warningShape),
  benchmark: object({ startedAt: timestampShape, elapsedMs: nonnegative, simulations: positive,
    dataVersion: text, scrutinyAlgorithmId: text, seed: text,
    execution: optional(object({ requestedWorkers: positive, computeWorkers: positive, maximumWorkers: positive,
      estimatedWorkerMemoryMiB: nonnegative, estimateVersion: text, generationMs: nonnegative,
      scrutinyMs: nonnegative, presentationMs: nonnegative, storageMs: nonnegative })) }) });
export const resultEnvelopeShape = object({ kind: literal('politics-result'), schemaVersion: literal(RESULT_ENVELOPE_VERSION),
  executedScenario: scenarioShape, execution: executionShape, session: sessionShape, result: workerResultShape });
export type PoliticsResultEnvelope = Infer<typeof resultEnvelopeShape>;

/** Reuse state-codec v1; never embed a former scenario/catalog/runtime checkpoint. */
export const byteAssetShape = object({ path: text, compressedBytes: positive, decodedBytes: positive,
  sha256: sha256Shape, decodedSha256: sha256Shape });
export const checkpointShape = object({ kind: literal('politics-checkpoint'), schemaVersion: literal(1),
  scenarioSha256: sha256Shape, dependencies: array(assetShape), implementationSha256: sha256Shape,
  preparationKey: text, generator: identity, numericalVersion: text, fittingSeed: text,
  states: array(object({ kind: literal('fast-share-fit', 'fast-share-profiles'), descriptor: byteAssetShape })),
  assets: array(object({ path: text, chunks: array(text) })) });
export type PoliticsCheckpoint = Infer<typeof checkpointShape>;
export const archiveEnvelopeShape = object({ kind: literal('politics-result-archive'), schemaVersion: literal(ARCHIVE_ENVELOPE_VERSION),
  exportedAt: timestampShape, run: resultEnvelopeShape, portableFit: nullable(checkpointShape) });
export type PoliticsArchiveEnvelope = Infer<typeof archiveEnvelopeShape>;
