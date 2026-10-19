import type { Candidate, Chamber, ChamberModel, OrdinaryVotes, PoliticalList, Preferences, ReferenceScope, ReformScrutinyInput, SpecialVotes } from './input';
import type { SeatTableBundle } from './endowments';

export const REFORM_SNAPSHOT_VERSION = 1 as const;
export const REFORM_SCENARIO_VERSION = 1 as const;
export const REFORM_MODEL_IDS = { votes: 'final-attributed-list-totals-v1', preferences: 'preferences-empirical-synthetic-v1', chambers: 'paired-chamber-votes-v1' } as const;
export interface PreferenceParameters {
  intensity: number;
  strengthSigma: number;
  localSigma: number;
}
export interface PreferenceProfile {
  id: string;
  intensityMultiplier: number;
  concentrationMultiplier: number;
  persistence: number;
  /** 0 suppresses triples; 1 is the central counting-measure max-entropy model. */
  tripleWeight: number;
}
export interface ChamberProfile {
  id: string;
  /** Correlation of each pair of chamber-specific residuals. */
  residualCorrelation: number;
  sharedVariance: number;
  gapMultiplier: number;
}
export interface ReformModelBundle {
  schemaVersion: 1;
  version: string;
  modelIds: typeof REFORM_MODEL_IDS;
  sourceVersions: string[];
  assumptions: string[];
  preferences: {
    parameterVersion: string;
    baseline: PreferenceParameters;
    pools: { election: string; scope: string; observations: number; parameters: PreferenceParameters }[];
    profiles: PreferenceProfile[];
  };
  chambers: {
    parameterVersion: string;
    /** One common marginal budget, split rather than inflated by chamber noise. */
    sigmaPerSqrtDay: { national: number; region: number; local: number };
    profiles: ChamberProfile[];
  };
}
export interface HistoricalMunicipality {
  code: string;
  regionId: string;
  provinceId: string;
  name: string;
  camera: { electors: number; votes: { listId: string; votes: number }[] };
  senato: { electors: number; votes: { listId: string; votes: number }[] };
}
export interface ReformGenerationSnapshot {
  schemaVersion: typeof REFORM_SNAPSHOT_VERSION;
  kind: 'politics-reform-generation';
  version: string;
  historicalDate: string;
  sourceVersions: string[];
  assumptions: string[];
  seats: SeatTableBundle;
  models: ReformModelBundle;
  historicalLists: { id: string; name: string; kind: 'list' | 'special-ballot-group' }[];
  historicalCoalitions: { chamber: Chamber; listIds: string[] }[];
  municipalities: HistoricalMunicipality[];
  /** Census-weighted submunicipal reconstruction, labelled in assumptions. */
  memberships: { chamber: Chamber; municipalityCode: string; collegeId: string; population: number }[];
}
export interface ReformShareOverride {
  scope: ReferenceScope;
  listId: string;
  common: number | null;
  camera: number | null;
  senato: number | null;
}
export type ReformCandidateTemplate = {
  chamber: Chamber;
  role: 'ordinary' | 'bonus' | 'special';
  locationId: string;
  /** Ordinary list, or special/bonus subject ID. */
  ownerId: string;
  /** One-based filed order. Special contests require position 1. */
  position: number;
  /** null keeps the declared special candidate independent; ordinary ownership
   * and generated bonus placement already determine their list affiliation. */
  affiliationListId: string | null;
  candidate: Candidate;
};
export interface ReformGenerationScenario {
  schemaVersion: typeof REFORM_SCENARIO_VERSION;
  kind: 'politics-reform-scenario';
  algorithmId: 'politiche-reform-2026-v1';
  snapshotVersion: string;
  modelVersion: string;
  modelIds: typeof REFORM_MODEL_IDS;
  preferenceProfileId: string;
  chamberProfileId: string;
  seed: string;
  electionDate: string;
  projectionDate: string;
  mode: 'mean';
  renewal: 'joint' | Chamber;
  lists: { camera: PoliticalList[]; senato: PoliticalList[] };
  coverage: { chamber: Chamber; collegeId: string; listIds: string[] }[];
  correspondences: { chamber: Chamber; pastListId: string; futureListId: string; factor: number }[];
  shares: ReformShareOverride[];
  /** Valid votes are list shares; abstention includes invalid/unmapped ballots,
   * following the existing scenario convention. null preserves historical rates. */
  abstention: { scope: ReferenceScope; common: number | null; camera: number | null; senato: number | null }[];
  candidateGeneration: { reuseProbability: number; maximumOrdinaryCandidacies: number };
  candidateTemplates: ReformCandidateTemplate[];
  suppliedPreferences: { chamber: Chamber; collegeId: string; listId: string; preferences: Preferences }[];
  suppliedVotes: { chamber: Chamber; collegeId: string; votes: OrdinaryVotes | SpecialVotes }[];
  bonusLinks: ReformScrutinyInput['bonusLinks'];
  conversion: { from: string; assumptions: string[] } | null;
}
export interface GenerationTrace {
  warnings: { code: string; message: string }[];
  references: Extract<ChamberModel, { kind: 'generated' }>['references'][number][];
  normalization: { scope: ReferenceScope; chamber: Chamber; reason: string; before: number; after: number }[];
  streamVersion: string;
  snapshotVersion: string;
  modelVersion: string;
}
