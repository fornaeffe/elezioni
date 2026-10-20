import type { ScrutinyWarning } from '$lib/core/types';
import type { Chamber, ChamberModel, Provenance, SeatTableSource, Subject } from './input';
import { REFORM_ALGORITHM_ID, REFORM_INPUT_VERSION } from './input';

export type InterpretationId = 'I01' | 'I02' | 'I03' | 'I04' | 'I05' | 'I06' | 'I07' | 'I08' | 'I09' | 'I10' | 'I11' | 'I12' | 'I13';
type Digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';
export type NormId = `N0${Exclude<Digit, '0'>}` | `N${'1' | '2'}${Digit}`;
/** Decimal strings preserve exact fractions in JSON, workers and exports (R26-06). */
export interface ExactFraction { readonly numerator: string; readonly denominator: string }
export interface DrawRequest {
  readonly contextId: string;
  readonly chamber: Chamber | null;
  readonly norm: NormId;
  readonly rule: 'statutory' | 'I03';
  readonly lawReference: string;
  readonly alternativeIds: readonly string[];
}
export interface DrawTrace extends DrawRequest { readonly chosenId: string; readonly streamId: string }
/** Execution dependency, deliberately outside the transferable input envelope. */
export interface ScrutinyExecutionContext { readonly draw: (request: DrawRequest) => DrawTrace }

export interface ReformWarning extends ScrutinyWarning {
  readonly severity: 'info' | 'warning' | 'error';
  readonly electionKind: 'politiche';
  readonly simulationId: number;
  readonly chamber?: Chamber;
  readonly interpretationId?: InterpretationId;
  readonly seatId?: string;
  readonly drawContextId?: string;
}

export interface SeatLocation {
  readonly chamber: Chamber;
  readonly territoryId: string;
  readonly collegeId: string | null;
  readonly subject: Subject;
  readonly listId: string | null;
}
interface SeatBase {
  readonly seatId: string;
  readonly category: 'ordinary' | 'bonus' | 'special-uninominal' | 'taa-proportional';
  readonly entitlement: SeatLocation;
}
export type SeatLedgerEntry = SeatBase & (
  | { readonly status: 'filled'; readonly electedAt: SeatLocation; readonly candidateId: string }
  | { readonly status: 'unfilled' | 'unresolved'; readonly reason: string }
);

export interface RemainderTransition {
  readonly id: string;
  readonly value: ExactFraction;
  readonly before: 'unused' | 'used' | 'consumed';
  readonly after: 'unused' | 'used' | 'consumed';
}

/** Territorial/cap corrections are distinct from candidate exhaustion recovery. */
export interface SeatTransferTrace {
  readonly seatId: string;
  readonly chamber: Chamber;
  readonly norm: NormId;
  readonly lawReference: string;
  readonly interpretationId: InterpretationId | null;
  /** Independent C83-bis(1)(b)(3) additions/removals have one null endpoint. */
  readonly from: SeatLocation | null;
  readonly to: SeatLocation | null;
  readonly remainders: readonly RemainderTransition[];
  readonly drawContextIds: readonly string[];
}

export interface RecoveryTrace {
  readonly seatId: string;
  readonly route: string;
  readonly lawReference: string;
  readonly interpretationId: 'I07' | null;
  readonly originalEntitlement: SeatLocation;
  readonly electedAt: SeatLocation | null;
  readonly candidateId: string | null;
  readonly consideredCandidateIds: readonly string[];
  readonly remainders: readonly RemainderTransition[];
  readonly drawContextIds: readonly string[];
  readonly outcome: 'filled' | 'unfilled' | 'unresolved';
}

export type StageOutcome = {
  readonly norm: NormId;
  readonly chamber: Chamber | null;
} & (
  | { readonly status: 'complete' }
  | { readonly status: 'partial' | 'non-computable' | 'not-run'; readonly reason: string; readonly affectedIds: readonly string[] }
);

export interface ReformScrutinyResult {
  readonly schemaVersion: typeof REFORM_INPUT_VERSION;
  readonly algorithmId: typeof REFORM_ALGORITHM_ID;
  readonly simulationId: number;
  readonly status: 'complete' | 'partial' | 'non-computable';
  readonly scope: 'domestic-initial';
  readonly renewedChambers: readonly Chamber[];
  readonly legalVersion: string;
  readonly geographyVersion: string;
  readonly chamberModel: ChamberModel;
  readonly seatTables: readonly { readonly chamber: Chamber; readonly source: SeatTableSource }[];
  readonly provenance: readonly Provenance[];
  readonly interpretations: readonly { readonly id: InterpretationId; readonly version: string }[];
  /** Conditions on candidate/recovery conclusions, independent of stage success. */
  readonly conditions: readonly {
    readonly kind: 'missing-preferences' | 'synthetic-preferences' | 'unknown-ballot-composition' |
      'provisional-seats' | 'modelled-chamber-votes';
    readonly chamber: Chamber | null;
    readonly affectedIds: readonly string[];
    readonly reference: string;
  }[];
  readonly subjectDecisions: readonly {
    readonly norm: NormId;
    readonly chamber: Chamber;
    readonly subject: Subject;
    readonly decision: 'admitted' | 'excluded' | 'bonus-awarded' | 'bonus-absent' | 'unresolved';
    readonly reason: string;
    readonly lawReference: string;
  }[];
  readonly warnings: readonly ReformWarning[];
  readonly stages: readonly StageOutcome[];
  readonly draws: readonly DrawTrace[];
  readonly seats: readonly SeatLedgerEntry[];
  readonly transfers: readonly SeatTransferTrace[];
  readonly recoveries: readonly RecoveryTrace[];
}
