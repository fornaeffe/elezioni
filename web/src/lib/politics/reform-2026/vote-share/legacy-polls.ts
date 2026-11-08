import type { ReformGenerationScenario, ReformGenerationSnapshot } from '../generation-types';
import { m01Data } from '../../generation/types';
import { refreshM01Polls } from '../../generation/m01-polls';
export { latestLegacyPoints } from '../../generation/m01-polls';
export type { LegacyPollObservation } from '../../generation/m01-polls';
export function refreshLegacyPolls(s: ReformGenerationScenario, snapshot: ReformGenerationSnapshot,
  ...args: Parameters<typeof refreshM01Polls> extends [unknown, unknown, ...infer R] ? R : never) {
  const result = refreshM01Polls(s, m01Data(snapshot), ...args);
  if (s.pollReferences) s.schemaVersion = 3;
  return result;
}
