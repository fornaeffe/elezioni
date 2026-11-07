import type { PoliticsScenario } from './schema';
import type { Asset } from './schema';

/** Design boundary only: CP-07 implements strict worker-result/portable-archive payload schemas.
 * These versions do not control scenario import, model versions or reusable checkpoint payloads. */
export const RESULT_ENVELOPE_VERSION = 1;
export const ARCHIVE_ENVELOPE_VERSION = 1;
export interface ExecutionProvenance {
  readonly generator: { readonly id: string; readonly version: string };
  readonly scrutiny: { readonly id: string; readonly version: string };
  readonly adapter: { readonly id: string; readonly version: string };
  readonly data: readonly Asset[];
  readonly numericalVersion: string;
  readonly rngVersion: string;
  readonly implementationSha256: string;
  readonly requestedWorkers: PoliticsScenario['execution']['workers'];
  readonly actualWorkers: number;
  readonly fittingSeed: string;
  readonly simulationSeed: string;
  readonly startedAt: string;
  readonly elapsedMs: number;
}
export interface PoliticsResultEnvelope<Result> {
  readonly kind: 'politics-result'; readonly schemaVersion: typeof RESULT_ENVELOPE_VERSION;
  readonly executedScenario: PoliticsScenario; readonly execution: ExecutionProvenance; readonly result: Result;
}
export interface PoliticsArchiveEnvelope<Result, PortableFit> {
  readonly kind: 'politics-result-archive'; readonly schemaVersion: typeof ARCHIVE_ENVELOPE_VERSION;
  readonly run: PoliticsResultEnvelope<Result>; readonly portableFit: PortableFit | null;
}
