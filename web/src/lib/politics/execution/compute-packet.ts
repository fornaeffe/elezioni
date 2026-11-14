/** Ephemeral, exact-build-bound transport, distinct from portable checkpoints. */
import { structuredHash } from '../../core/structured-hash';
import { parsePoliticsScenario } from '../scenario/validation';
import type { PoliticsScenario } from '../scenario/schema';
import { commonPoliticsImplementation } from './build';
import { preparePoliticsComputeInputs, type PoliticsExecutionKeys, type PoliticsComputeInputs } from './compute-inputs';
import type { PreparedPoliticsExecution } from './prepare';
import { m02PreparationKey, m02GenerationKey } from './keys';

export interface PoliticsComputePacket {
  schemaVersion: 1;
  implementationId: string;
  scenario: PoliticsScenario;
  keys: PoliticsExecutionKeys;
  inputs: PoliticsComputeInputs;
  sha256: string;
}
export async function createPoliticsComputePacket(prepared: PreparedPoliticsExecution): Promise<PoliticsComputePacket> {
  const content = { schemaVersion: 1 as const, implementationId: prepared.implementationId, scenario: prepared.scenario,
    keys: prepared.keys, inputs: prepared.computeInputs };
  return { ...content, sha256: await structuredHash(content) };
}
export async function preparePoliticsCompute(packet: PoliticsComputePacket, implementationId = commonPoliticsImplementation) {
  if (!implementationId || packet.implementationId !== implementationId) throw new Error('POLITICS_IMPLEMENTATION_MISMATCH');
  if (packet.schemaVersion !== 1 || Object.keys(packet).sort().join('|') !== 'implementationId|inputs|keys|scenario|schemaVersion|sha256')
    throw new Error('INVALID_POLITICS_COMPUTE_PACKET');
  const { sha256, ...content } = packet;
  if (sha256 !== await structuredHash(content)) throw new Error('FROZEN_POLITICS_BINDING_MISMATCH');
  const scenario = parsePoliticsScenario(packet.scenario);
  const g = packet.inputs.generator;
  if (g.id === 'M02' && (packet.keys.preparationKey !== await m02PreparationKey(scenario, {
    nationalInputKey: g.parents.fit.nationalInputKey, profileInputKey: g.parents.profiles.profileInputKey,
    pairedInputKey: g.parents.profiles.pairedInputKey,
  }) || packet.keys.generationKey !== await m02GenerationKey(packet.keys.preparationKey, g.settings)))
    throw new Error('POLITICS_NUMERIC_STATE_DEPENDENCY_MISMATCH');
  return preparePoliticsComputeInputs(scenario, implementationId, packet.keys, packet.inputs);
}
