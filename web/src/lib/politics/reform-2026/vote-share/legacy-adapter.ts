/** Compatibility envelope for existing reform releases; arithmetic lives in the shared compiler. */
import type { ReleasedScenario } from './release';
import type { CompactSource } from './source-types';
import type { FastVoteAssets } from './forecast-types';
import type { ReformAssemblyContext, ReformGenerationScenario, ReformGenerationSnapshot } from '../generation-types';
import { REFORM_MODEL_IDS } from '../generation-types';
import { sourceKey, type AssetBytes } from './source-codec';
import { reformPresentation } from '../presentation-metadata';
import { compileM01, latestLegacyPair, type CompiledM01 } from '../../generation/m01-compiler';
export { latestLegacyPair } from '../../generation/m01-compiler';
export interface CompiledLegacy { inputKey: string; scenario: ReformGenerationScenario; snapshot: ReformGenerationSnapshot; audit: CompiledM01['audit'] }
export const isM01Generation = (s: Pick<ReleasedScenario, 'generation'>) =>
  s.generation?.model === 'M01';
export function legacyInputKey(s: ReleasedScenario) {
  // SDs, fitting seeds, candidate-only profile scales and pooling do not acquire
  // unsupported legacy meanings. They remain in the common editable scenario.
  return sourceKey('unified-legacy-input-v1', {
    source: s.data.sourceBundleId,
    referenceDate: s.referenceDate,
    electionDate: s.electionDate,
    generation: s.generation,
    sourceMemberships: s.sourceMemberships,
    componentMap: s.componentMap,
    polls: s.polls,
    lists: s.lists,
    coverage: s.coverage,
    references: s.references.map(({ sdPercentagePoints, ...r }) => r),
    nonValid: s.nonValidReferences.map(({ sdPercentagePoints, ...r }) => r),
    renewal: s.renewal,
    seed: s.seed,
    candidateGeneration: s.candidateGeneration,
    candidateTemplates: s.candidateTemplates,
    suppliedVotes: s.suppliedVotes,
    suppliedPreferences: s.suppliedPreferences,
    bonusLinks: s.bonusLinks,
    chamberProfileId: s.chamberProfileId,
    preferenceProfileId: s.preferenceProfileId,
  });
}
export async function compileLegacy(s: ReleasedScenario, source: CompactSource,
  read: (path: string) => Promise<AssetBytes>, assets: FastVoteAssets, context: ReformAssemblyContext): Promise<CompiledLegacy> {
  const compiled = await compileM01(s, source, read, assets,
    { geography: context.seats.geography, models: context.models }, await legacyInputKey(s));
  const pair = latestLegacyPair(source, s.referenceDate);
  const snapshot: ReformGenerationSnapshot = {
    ...structuredClone(context),
    schemaVersion: 1,
    kind: 'politics-reform-generation',
    version: compiled.inputKey,
    historicalDate: compiled.snapshot.historicalDate,
    historicalLists: source.payload.choices.filter(c => c.eventId === pair.camera.id || c.eventId === pair.senato.id)
      .map(c => ({ id: c.id, name: c.label, kind: c.opaque ? 'special-ballot-group' : 'list' })),
    historicalCoalitions: [],
    memberships: compiled.snapshot.memberships,
    municipalities: compiled.snapshot.municipalities,
  };
  snapshot.assumptions.push(
    'Original latest eligible common-18+ Camera/Senato pair; target electorates are conditional, missing municipal history uses labelled original regional proportions.',
    'Political mapping/filing is conditioned on available choices, preserving historical turnout; post-FV-08 policy audit remains planned.',
  );
  const scenario: ReformGenerationScenario = {
    schemaVersion: compiled.scenario.pollReferences ? 3 : 2,
    kind: s.kind,
    algorithmId: s.algorithmId,
    snapshotVersion: snapshot.version,
    modelVersion: context.models.version,
    modelIds: REFORM_MODEL_IDS,
    preferenceProfileId: s.preferenceProfileId,
    chamberProfileId: s.chamberProfileId,
    seed: s.seed,
    electionDate: s.electionDate,
    projectionDate: compiled.scenario.projectionDate,
    mode: 'mean',
    renewal: s.renewal,
    lists: {
      camera: s.lists.camera.map(
        ({
          componentIds,
          nationalUncertaintyMultiplier,
          localUncertaintyMultiplier,
          ...l
        }) => l,
      ),
      senato: s.lists.senato.map(
        ({
          componentIds,
          nationalUncertaintyMultiplier,
          localUncertaintyMultiplier,
          ...l
        }) => l,
      ),
    },
    coverage: s.coverage,
    correspondences: compiled.scenario.correspondences,
    shares: compiled.scenario.shares,
    abstention: compiled.scenario.abstention,
    candidateGeneration: s.candidateGeneration,
    candidateTemplates: s.candidateTemplates,
    suppliedPreferences: s.suppliedPreferences,
    suppliedVotes: s.suppliedVotes,
    bonusLinks: s.bonusLinks,
    conversion: null,
    projectionPolicy: 'preserve-historical-turnout-v1',
    presentation: reformPresentation(s),
  };
  if (compiled.scenario.pollReferences) scenario.pollReferences = compiled.scenario.pollReferences;
  return { ...compiled, scenario, snapshot };
}
