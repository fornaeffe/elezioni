import type { PoliticalList, ReferenceScope, Chamber } from '../input';
import type { ReformGenerationScenario } from '../generation-types';
import type { fitReference } from './information';
import type { fitProfiles } from './profiles';
import type { SourceGeography } from './source-types';
export const FAST_GENERATION_VERSION = 'fast-share-physical-cells-v1';
export const SENATE_ADAPTER_VERSION = 'paired-18plus-logratio-adapter-v1';
export type ReferenceFit = ReturnType<typeof fitReference>;
export type GeographicProfile = ReturnType<typeof fitProfiles>;
export interface NewComponentPolicy {
  geographicLogProfile: number;
  profileSd: number;
  localInnovationSd: number;
  nationalInnovationSd: number;
}
export interface FastComponent {
  id: string;
  kind: 'fitted' | 'new';
  provenance: string;
  fallback?: NewComponentPolicy;
}
export interface FastList extends PoliticalList {
  componentIds: string[];
  nationalUncertaintyMultiplier?: number;
  localUncertaintyMultiplier?: number;
}
export interface FastReference {
  id: string;
  scope: ReferenceScope;
  channel: 'common' | Chamber;
  subject: { kind: 'list' | 'component'; id: string };
  mean: number;
  sdPercentagePoints: number;
}
export interface FastNonValidReference {
  id: string;
  scope: ReferenceScope;
  channel: 'common' | Chamber;
  mean: number;
  sdPercentagePoints: number;
}
export interface TargetElectorate {
  code: string;
  date: string;
  camera: number;
  senato: number;
  provenance: string;
}
export type LegalGenerationSettings = Pick<
  ReformGenerationScenario,
  | 'kind'
  | 'algorithmId'
  | 'seed'
  | 'electionDate'
  | 'renewal'
  | 'coverage'
  | 'candidateGeneration'
  | 'candidateTemplates'
  | 'suppliedPreferences'
  | 'suppliedVotes'
  | 'bonusLinks'
  | 'preferenceProfileId'
  | 'chamberProfileId'
>;
export interface FastGenerationScenario extends LegalGenerationSettings {
  schemaVersion: 4;
  referenceDate: string;
  fitSeed: string;
  models: { statistical: string; numerical: string; references: string; chambers: string; rng: string };
  data: { sourceBundleId: string; fittedStateId: string; profileStateId: string; geographyVersion: string };
  components: FastComponent[];
  lists: { camera: FastList[]; senato: FastList[] };
  references: FastReference[];
  nonValidReferences: FastNonValidReference[];
}
export interface PhysicalCell {
  id: string;
  municipality: string;
  region: string;
  province: string;
  collegeId: string;
  chamber: Chamber;
  electors: number;
}
export interface SenateAdapter {
  version: typeof SENATE_ADAPTER_VERSION;
  referenceDate: string;
  pairedDate: string;
  componentIds: string[];
  nationalLogRatios: Float64Array;
  nonValidLogOdds: number;
  profile: GeographicProfile;
  sourceVersions: string[];
  assumptions: string[];
  contrasts: {
    component: string;
    method: 'separate' | 'pooled' | 'fallback';
    group: string[];
    camera: number | null;
    senato: number | null;
  }[];
}
export interface FastVoteAssets {
  fit: ReferenceFit;
  cameraProfile: GeographicProfile;
  senateAdapter?: SenateAdapter;
  modelId: string;
  numericalVersion: string;
  fitSeed: string;
  profileReferenceDate: string;
  geography: SourceGeography;
  electorates: TargetElectorate[];
  sourceBundleId: string;
  fittedStateId: string;
  profileStateId: string;
}
