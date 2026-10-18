/** Versioned, JSON/worker-safe legal inputs. No statistical generation runs here. */
export const REFORM_INPUT_VERSION = 1 as const;
export const REFORM_ALGORITHM_ID = 'politiche-reform-2026-v1' as const;
export type Chamber = 'camera' | 'senato';
export type Gender = 'female' | 'male';
export type Subject = { readonly kind: 'list' | 'coalition'; readonly id: string };

export type Provenance =
  | { readonly kind: 'supplied'; readonly reference: string }
  | {
      readonly kind: 'generated';
      readonly modelId: string;
      readonly parameterVersion: string;
      readonly profileId: string;
      readonly sourceVersions: readonly string[];
      readonly seed: string;
      readonly assumptions: readonly string[];
    };

export interface Candidate {
  readonly id: string;
  readonly name: string;
  /** Calendar dates, YYYY-MM-DD; no timezone-dependent age calculation. */
  readonly birthDate: string;
  readonly gender: Gender;
  /** Preserve the filed slate when initial availability subsequently changes. */
  readonly availability:
    | { readonly kind: 'available' }
    | { readonly kind: 'unavailable'; readonly reason: string };
  /** Includes other-chamber declarations even in a single-chamber input. */
  readonly externalCandidacies: readonly {
    readonly chamber: Chamber;
    readonly kind: 'domestic' | 'foreign';
  }[];
}

export interface PoliticalList {
  readonly id: string;
  readonly name: string;
  readonly coalitionId: string | null;
  readonly protectedMinority: {
    readonly regionId: string;
    readonly legalReference: string;
  } | null;
}

export interface Coalition {
  readonly id: string;
  readonly listIds: readonly string[];
}

export type SeatTableSource = (
  | { readonly kind: 'official-decree' | 'constructed'; readonly reference: string; readonly version: string }
  | {
      readonly kind: 'provisional-census';
      readonly reference: string;
      readonly version: string;
      readonly censusDate: string;
      readonly geographyVersion: string;
      readonly method: 'I08-v1';
    }
) & { readonly populationNotes?: readonly string[] };

export interface Territory {
  readonly id: string;
  readonly regionId: string;
  readonly kind: 'ordinary' | 'vda' | 'taa';
  readonly bonusSeats: number;
}

export interface College {
  readonly id: string;
  readonly territoryId: string;
  readonly kind: 'ordinary' | 'taa-proportional' | 'special-uninominal';
  readonly seatsWithoutBonus: number;
  readonly seatsWithBonus: number;
}

export interface ListVotes {
  readonly listId: string;
  readonly votes: number;
}

export type OrdinaryVotes =
  | {
      readonly kind: 'components';
      readonly directListVotes: readonly ListVotes[];
      readonly bonusOnlyVotes: readonly { readonly subject: Subject; readonly votes: number }[];
      readonly provenance: Provenance;
    }
  | {
      readonly kind: 'attributed';
      readonly listVotes: readonly ListVotes[];
      readonly provenance: Provenance;
    };

export type Preferences =
  | { readonly kind: 'missing'; readonly reason: string }
  | {
      readonly kind: 'counts';
      /** All non-capolista candidates, including explicit zeros. */
      readonly counts: readonly { readonly candidateId: string; readonly count: number }[];
      readonly provenance: Provenance;
      readonly validity:
        | { readonly kind: 'adjudicated'; readonly reference: string }
        | {
            readonly kind: 'combinations';
            /** A constructive certificate, including ballots with no preferences. */
            readonly ballots: readonly { readonly candidateIds: readonly string[]; readonly count: number }[];
          };
    };

export interface OrdinarySlate {
  readonly collegeId: string;
  readonly listId: string;
  /** Filed order; index zero is the capolista. Unavailability does not delete slots. */
  readonly candidateIds: readonly string[];
  readonly preferences: Preferences;
}

export interface BonusSlate {
  readonly territoryId: string;
  /** One slate for the whole coalition, not invented per-list bonus quotas. */
  readonly subject: Subject;
  readonly candidates: readonly { readonly candidateId: string; readonly affiliationListId: string }[];
}

export interface SpecialCandidature {
  readonly collegeId: string;
  readonly candidateId: string;
  readonly linkedListIds: readonly string[];
  readonly affiliationListId: string | null;
}

export type SpecialVotes =
  | {
      readonly kind: 'components';
      readonly directListVotes: readonly ListVotes[];
      readonly candidateOnlyVotes: readonly { readonly candidateId: string; readonly votes: number }[];
      readonly candidateTotals: readonly { readonly candidateId: string; readonly votes: number }[];
      readonly provenance: Provenance;
    }
  | {
      readonly kind: 'attributed';
      readonly listVotes: readonly ListVotes[];
      readonly candidateTotals: readonly { readonly candidateId: string; readonly votes: number }[];
      readonly provenance: Provenance;
    };

export interface Electorate {
  readonly minimumAge: 18;
  readonly electors: number | null;
}

export interface ChamberInput {
  readonly chamber: Chamber;
  readonly regions: readonly { readonly id: string; readonly autonomous: boolean }[];
  readonly territories: readonly Territory[];
  readonly colleges: readonly College[];
  readonly seatTable: SeatTableSource;
  readonly lists: readonly PoliticalList[];
  readonly coalitions: readonly Coalition[];
  readonly ordinarySlates: readonly OrdinarySlate[];
  readonly bonusSlates: readonly BonusSlate[];
  readonly specialCandidatures: readonly SpecialCandidature[];
  readonly ordinaryVotes: readonly {
    readonly collegeId: string;
    readonly electorate: Electorate;
    readonly votes: OrdinaryVotes;
  }[];
  readonly specialVotes: readonly {
    readonly collegeId: string;
    readonly electorate: Electorate;
    readonly votes: SpecialVotes;
  }[];
}

export type Renewal =
  | { readonly kind: 'joint'; readonly camera: ChamberInput; readonly senato: ChamberInput }
  | { readonly kind: 'single'; readonly chamber: ChamberInput };

export type ReferenceScope =
  | { readonly kind: 'national' }
  | { readonly kind: 'region' | 'province' | 'municipality'; readonly code: string };

export interface ShareReference {
  readonly share: number;
  readonly referenceDate: string;
}

export interface ResolvedChamberReference extends ShareReference {
  readonly origin: 'supplied' | 'model-derived';
}

export type ChamberModel =
  | { readonly kind: 'supplied'; readonly reference: string }
  | {
      readonly kind: 'generated';
      readonly modelId: 'paired-chamber-votes-v1';
      readonly parameterVersion: string;
      readonly covarianceProfileVersion: string;
      readonly sourceVersions: readonly string[];
      readonly seed: string;
      readonly assumptions: readonly string[];
      readonly mode: 'mean';
      readonly references: readonly {
        readonly scope: ReferenceScope;
        readonly listIds: { readonly camera: string | null; readonly senato: string | null };
        readonly common: ShareReference | null;
        readonly camera: ResolvedChamberReference | null;
        readonly senato: ResolvedChamberReference | null;
      }[];
    };

export interface ReformScrutinyInput {
  readonly schemaVersion: typeof REFORM_INPUT_VERSION;
  readonly algorithmId: typeof REFORM_ALGORITHM_ID;
  readonly simulationId: number;
  readonly electionDate: string;
  readonly legalVersion: string;
  readonly geographyVersion: string;
  readonly randomness: { readonly seed: string; readonly streamVersion: string };
  readonly candidates: readonly Candidate[];
  readonly renewal: Renewal;
  /** Each entry records reciprocal declarations, not a match inferred from names. */
  readonly bonusLinks: readonly {
    readonly camera: Subject;
    readonly senato: Subject;
    readonly cameraDeclares: boolean;
    readonly senatoDeclares: boolean;
  }[];
  readonly chamberModel: ChamberModel;
}
