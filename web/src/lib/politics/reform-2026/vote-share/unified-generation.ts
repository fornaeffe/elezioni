/** Registry boundary: the same scenario and legal assembly, distinct vote laws. */
import { prepareFastGeneration } from './generation';
import type { FastVoteAssets } from './forecast-types';
import type { ReformAssemblyContext } from '../generation-types';
import type { ReleasedScenario } from './release';
import { isM01Generation, type CompiledLegacy } from './legacy-adapter';
export { generationModels } from './correspondences';
export function prepareScenarioGeneration(
  s: ReleasedScenario,
  assets: FastVoteAssets,
  context: ReformAssemblyContext,
  legacy?: CompiledLegacy,
) {
  // Enforce the shared ballot contract before choosing either statistical law.
  // The derived Camera TAA proportional view is deliberately not a new ballot.
  for (const ch of s.renewal === 'joint'
    ? (['camera', 'senato'] as const)
    : [s.renewal]) {
    const physical = new Set(
      context.seats.geography.colleges
        .filter((c) => c.chamber === ch && c.kind !== 'taa-proportional')
        .map((c) => c.id),
    );
    for (const college of physical) {
      const filed = new Set(
          s.coverage.find((c) => c.chamber === ch && c.collegeId === college)
            ?.listIds ?? [],
        ),
        seen = new Set<string>();
      for (const list of s.lists[ch].filter((l) => filed.has(l.id)))
        for (const id of list.componentIds) {
          if (seen.has(id))
            throw new Error(
              'Component competes in multiple future choices: ' +
                ch +
                '/' +
                college +
                '/' +
                id,
            );
          seen.add(id);
        }
    }
  }
  if (!isM01Generation(s)) return prepareFastGeneration(s, assets, context);
  if (!legacy) throw new Error('Prepare legacy references before simulation');
  return prepareFastGeneration(s, assets, context, { legacy });
}
