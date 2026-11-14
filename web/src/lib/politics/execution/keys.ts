import { sourceKey } from '../reform-2026/vote-share/source-codec';
import type { PoliticsScenario } from '../scenario/schema';
import { activeGenerator } from '../scenario/registry';
import type { M02Settings } from '../generation/m02';

/** Hash parents, rather than retransmitting the paired observations merely to
 * reproduce a key in every worker. This is a dependency identity, not model/RNG. */
export function m02PreparationKey(s: PoliticsScenario, parents: {
  nationalInputKey: string; profileInputKey: string; pairedInputKey: string;
}) {
  return sourceKey('politics-m02-preparation-v2', { model: activeGenerator(s), fitSeed: s.execution.fittingSeed,
    national: parents.nationalInputKey, profiles: parents.profileInputKey, paired: parents.pairedInputKey,
    policies: s.modelSettings.M02.policies, source: s.data.historical.sha256, electorate: s.data.electorate.sha256 });
}
export function m02GenerationKey(preparationKey: string, settings: M02Settings) {
  return sourceKey('politics-m02-generation-v1', { preparationKey, inputs: { ...settings,
    lists: Object.fromEntries((['camera', 'senato'] as const).map(ch => [ch,
      settings.lists[ch].map(({ name, coalitionId, protectedMinority, kind, ...list }) => list)])) } });
}
