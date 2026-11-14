import type { PreparedPoliticsExecution, PoliticsNumericStates } from './prepare';
import type { PoliticsScenario } from '../scenario/schema';
import type { PoliticsCheckpoint } from '../scenario/envelopes';
import { createPoliticsCheckpoint, validatePoliticsCheckpoint } from '../files/checkpoint';
import { encodeState, decodeState } from '../reform-2026/vote-share/state-codec';
import { fittedStateShapes, profileStateShapes } from '../reform-2026/vote-share/state-shapes';
import { implementationSha256 } from './build';

export async function executionCheckpoint(prepared: PreparedPoliticsExecution): Promise<PoliticsCheckpoint | null> {
  const states = prepared.numericStates;
  if (!states) return null;
  const metadata = { sourceVersions: [prepared.scenario.data.historical.version],
    methodVersions: [prepared.implementationId, implementationSha256(prepared.implementationId)], assumptions: [...prepared.scenario.provenance.assumptions] };
  const fit = await encodeState('fast-share-fit', states.fit, metadata, fittedStateShapes(states.fit.fit));
  const profiles = await encodeState('fast-share-profiles', states.profiles, metadata, profileStateShapes(states.profiles.cameraProfile));
  return createPoliticsCheckpoint(prepared.scenario,
    { implementationSha256: implementationSha256(prepared.implementationId), preparationKey: prepared.keys.preparationKey }, [
      { ...fit, envelope: { ...fit.envelope, kind: 'fast-share-fit' } },
      { ...profiles, envelope: { ...profiles.envelope, kind: 'fast-share-profiles' } },
    ]);
}
export async function checkpointStates(value: unknown, scenario: PoliticsScenario, implementationId: string) {
  const file = value as PoliticsCheckpoint;
  await validatePoliticsCheckpoint(value, scenario, { implementationSha256: implementationSha256(implementationId), preparationKey: file.preparationKey });
  const read = async (path: string) => {
    const asset = file.assets.find(a => a.path === path);
    if (!asset) throw new Error(`MISSING_POLITICS_CHECKPOINT_ASSET: ${path}`);
    const raw = asset.chunks.map(c => atob(c)).join('');
    return Uint8Array.from(raw, c => c.charCodeAt(0));
  };
  const fit = await decodeState(file.states.find(s => s.kind === 'fast-share-fit')!.descriptor, read, 'fast-share-fit');
  const profiles = await decodeState(file.states.find(s => s.kind === 'fast-share-profiles')!.descriptor, read, 'fast-share-profiles');
  return { file, states: { fit: fit.state, profiles: profiles.state } as PoliticsNumericStates };
}
