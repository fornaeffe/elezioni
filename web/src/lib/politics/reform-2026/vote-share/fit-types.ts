import type { FitProgress } from './execution';
import type { FastVoteAssets } from './forecast-types';
import type { ReformAssemblyContext } from '../generation-types';
import type {
  CandidatePointer,
  ReleaseCatalog,
  ReleasedScenario,
} from './release';
import type { SourceSelection } from './source-compiler';

export interface FitImplementations {
  generation: string;
  fitting: string;
  profiles: string;
}
export interface FitConfiguration {
  base: string;
  pointer: CandidatePointer;
  implementations: FitImplementations;
  /** Aggregate planning ceiling for the task, not measured free browser RAM. */
  maximumAggregateBytes?: number;
  maximumWorkspaceBytes?: number;
  resultPipelineBytes?: number;
}
export interface GenerationPacket {
  scenario: ReleasedScenario;
  assets: FastVoteAssets;
  context: ReformAssemblyContext;
  keys: { compositionKey: string; runKey: string };
}
export interface AcceptedFit extends GenerationPacket {
  sourceCatalog: ReleaseCatalog;
  selection: SourceSelection;
  implementations: FitImplementations;
  nationalInputKey: string;
  profileInputKey: string;
  pairedInputKey: string;
  electorateKey: string;
  /** Evidence fingerprint excludes all presentation and provenance-only edits. */
  evidenceKey: string;
  provenanceId: string;
}
export interface FitPlan {
  national: boolean;
  profiles: boolean;
  paired: boolean;
  reasons: string[];
  estimatedAggregateBytes: number;
  reused: { national: boolean; cameraProfile: boolean; senateAdapter: boolean };
}
export interface FitTaskResult {
  plan: FitPlan;
  /** Absent for validation that requires numerical work. */
  accepted?: AcceptedFit;
  audit: {
    nationalFits: number;
    profileFits: number;
    pairedFits: number;
    sourceLoaded: boolean;
    numericBytes: number;
    elapsedMs?: number;
    maximumNumericBatchMs?: number;
    progressMessages?: number;
  };
}
export type FitCommand =
  | { action: 'load'; configuration: FitConfiguration }
  | {
      action: 'validate' | 'fit';
      configuration: FitConfiguration;
      previous: AcceptedFit;
      scenario: ReleasedScenario;
    };
export type FitWorkerRequest =
  | ({ requestId: number; revision: number; fingerprint: string } & FitCommand)
  | {
      action: 'cancel';
      requestId: number;
      revision: number;
      fingerprint: string;
    };
export type FitWorkerReply = {
  requestId: number;
  revision: number;
  fingerprint: string;
} & (
  | { action: 'progress'; progress: FitProgress }
  | { action: 'result'; result: FitTaskResult }
  | { action: 'error'; error: string; cancelled: boolean }
);
