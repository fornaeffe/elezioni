/** Compatibility facade for retained reform callers and fixed regression inputs. */
import type { ReformGenerationScenario, ReformGenerationSnapshot } from './generation-types';
import { m01Data } from '../generation/types';
import { preparePairedVoteReferences as prepare, generatePairedMunicipalVotes as generate } from '../generation/m01';
export { pairedStandardShock, deriveCommonReferences, renewedChamberIds } from '../generation/m01';
export type { ProjectedMunicipality, GeneratedMunicipalVotes } from '../generation/m01';
export function preparePairedVoteReferences(s: ReformGenerationScenario, snapshot: ReformGenerationSnapshot) {
  return prepare(s, m01Data(snapshot));
}
export function generatePairedMunicipalVotes(s: ReformGenerationScenario, snapshot: ReformGenerationSnapshot,
  units: Parameters<typeof generate>[2], simulation: number) {
  return generate(s, m01Data(snapshot), units, simulation);
}
