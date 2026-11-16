/** Existing reform envelope around the shared statistical M02 boundary. */
import { prepareM02Votes } from '../../generation/m02';
export type { FastVoteAudit, FastGenerationOptions } from '../../generation/m02';
import type { FastGenerationOptions } from '../../generation/m02';
import type { FastGenerationScenario, FastVoteAssets } from './forecast-types';
import { FAST_GENERATION_VERSION } from './forecast-types';
import type { ReformAssemblyContext, ReformLegalSettings } from '../generation-types';
import { requireGenerationModel } from './model-registry';
import { FAST_SHARE_MODEL } from './types';
import { prepareReferenceBonusPlan } from '../reference-bonus-plan';
import { assembleReformSimulation, type AssemblySource } from '../assembly';
export function m02AssemblySource(s: Pick<FastGenerationScenario, 'models'>, assets: FastVoteAssets): AssemblySource {
  const definition = requireGenerationModel({ model: 'M02', modelVersion: '3.0.0' });
  return {
          voteParameterVersion: definition.id + "@" + definition.version + "/" + FAST_GENERATION_VERSION,
          chamberModelId: "fast-share-paired-v1",
          chamberParameterVersion:
            assets.senateAdapter?.version ?? "camera-only",
          sourceVersions: [
            assets.sourceBundleId,
            assets.fittedStateId,
            assets.profileStateId,
            FAST_SHARE_MODEL,
            ...(assets.senateAdapter?.sourceVersions ?? []),
          ],
          assumptions: [
            ...(assets.senateAdapter?.assumptions.filter(a => !a.startsWith('One paired election') && !a.startsWith('Senate filtering/')) ?? []),
            "M02 3.0.0: one unrestricted common Camera field. Descriptive paired and explicit reference contrasts stay fixed through future movement, followed by separate physical filing conditioning and independent multinomial counts. No temporal contrast innovations are identified by one paired election.",
            "Conditional municipal 18+ electorates are apportioned to physical cells using census population weights before generation; this is a labelled reconstruction, not observed submunicipal turnout. Electorate dates: " +
              [...new Set(assets.electorates.map((e) => e.date))]
                .sort()
                .join(", ") +
              ".",
            ...[...new Set(assets.electorates.map((e) => e.provenance))],
            s.models.forecast
              ? "Filing-aware forecast field " +
                s.models.forecast +
                "; national totals are derived from feasible physical probabilities before multinomial counts. Reference construction: " +
                (s.models.forecastReference ?? "conditional") +
                "; reference-profile regional variance share " +
                (s.models.profileRegionCorrelation ?? 0) +
                "; innovation scale " +
                (s.models.forecastScale ?? 1) +
                ". Explicit list-reference interventions preserve turnout."
              : "Global physical-cell probability projection before independent multinomial counts; fixed-turnout reference interventions. Central legal reference metadata reports effective probabilities after scoped overrides; sampled targets are retained separately in the fast audit.",
            "The legal ballot representation remains final-attributed-list-totals-v1; statistical model " +
              FAST_SHARE_MODEL +
              ".",
          ],
        };
}
export function prepareFastGeneration(s: FastGenerationScenario, assets: FastVoteAssets,
  context: ReformAssemblyContext, options: FastGenerationOptions = {}) {
  if (![4, 5].includes(s.schemaVersion) || s.kind !== 'politics-reform-scenario' || s.algorithmId !== 'politiche-reform-2026-v1')
    throw new Error('FAST_GENERATION_MODEL_MISMATCH');
  s = structuredClone(s); context = structuredClone(context);
  const prepared = prepareM02Votes(s, assets, { geography: context.seats.geography, models: context.models }, options);
  const bonusPlan = s.candidateGeneration.bonusPolicy ? prepareReferenceBonusPlan(s, context, [], {
    referenceDate: s.referenceDate, support: prepared.support,
  }) : undefined;
  const legalLists = (ch: 'camera' | 'senato') => s.lists[ch].map(({ componentIds, nationalUncertaintyMultiplier, localUncertaintyMultiplier, ...l }) => l);
  const legalScenario: ReformLegalSettings = { ...s, lists: { camera: legalLists('camera'), senato: legalLists('senato') } };
  const source = m02AssemblySource(s, assets);
  return { ...prepared, scenario: s, bonusPlan, run: (simulation: number) => {
    const { counts, trace, audit } = prepared.run(simulation);
    if (bonusPlan) trace.bonusSelection = { policy: bonusPlan.policy, referenceDate: bonusPlan.referenceDate,
      slates: bonusPlan.slates.map(({ chamber, territoryId, subject, allocations }) => ({ chamber, territoryId, subject, allocations })) };
    const result = assembleReformSimulation({ scenario: legalScenario, snapshot: context, bonusPlan }, simulation, counts, trace, source);
    return { ...result, audit };
  } };
}
