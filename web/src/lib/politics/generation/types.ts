/** Statistical contracts are independent of law-specific candidates and seats. */
import type { ReformGenerationScenario, ReformGenerationSnapshot, GenerationTrace } from '../reform-2026/generation-types';
import type { SourceGeography } from '../reform-2026/vote-share/source-types';
export interface VoteGeography { version: string; colleges: readonly SourceGeography['colleges'][number][] }
export type M01Settings = Pick<ReformGenerationScenario,
  'seed' | 'electionDate' | 'projectionDate' | 'renewal' | 'lists' | 'coverage' |
  'correspondences' | 'shares' | 'abstention' | 'projectionPolicy' | 'chamberProfileId' | 'pollReferences'>;
export type M01Data = Pick<ReformGenerationSnapshot,
  'version' | 'historicalDate' | 'municipalities' | 'memberships'> & {
  geography: VoteGeography;
  models: Pick<ReformGenerationSnapshot['models'], 'version' | 'chambers'>;
};
export interface CollegeCounts { electors: number; votes: Map<string, number> }
export interface GeneratedVotes { counts: Map<string, CollegeCounts>; trace: GenerationTrace }
export function m01Data(snapshot: ReformGenerationSnapshot): M01Data {
  return { ...snapshot, geography: snapshot.seats.geography };
}
