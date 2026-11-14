import { expect, it } from 'vitest';
import { commonPoliticsFixture } from '../../../../tests/fixtures/common-politics-input';
import { REFORM, ROSATELLUM } from '../scenario/schema';
import { preparePoliticsExecution } from './prepare';
import { createPoliticsComputePacket, preparePoliticsCompute } from './compute-packet';
import { ownedBuffers } from '../reform-2026/vote-share/fit-resources';
import { structuredHash } from '../../core/structured-hash';
import { politicsCapacity, politicsRunResources } from './resources';

it.each([[ROSATELLUM, 'M01'], [REFORM, 'M01'], [REFORM, 'M02']] as const)(
  'transfers prepared %s/%s inputs with exact deterministic outcomes and no mutable numeric cache', async (law, model) => {
    const f = await commonPoliticsFixture(law, model), prepared = await preparePoliticsExecution(f.scenario, f.readers);
    const before = prepared.numericStates ? await structuredHash(prepared.numericStates) : undefined;
    const packet = await createPoliticsComputePacket(prepared);
    expect(Object.keys(packet).sort()).toEqual(['implementationId', 'inputs', 'keys', 'scenario', 'schemaVersion', 'sha256']);
    if (packet.inputs.generator.id === 'M02') expect(packet.inputs.generator.assets.fit).toBe(prepared.numericStates!.fit.fit);
    const copy = structuredClone(packet), buffers = ownedBuffers(copy);
    const moved = structuredClone(copy, { transfer: buffers });
    expect(buffers.every(b => b.byteLength === 0)).toBe(true);
    const worker = await preparePoliticsCompute(moved, prepared.implementationId);
    expect('numericStates' in worker).toBe(false); expect('computeInputs' in worker).toBe(false);
    for (const id of [7, 1, 7]) expect(worker.generate(id)).toEqual(prepared.generate(id));
    if (prepared.numericStates) expect(await structuredHash(prepared.numericStates)).toBe(before);
    const capacity = politicsCapacity(packet, { hardwareConcurrency: 28, deviceMemory: 32 });
    expect(capacity.recommended).toBeLessThanOrEqual(4); expect(capacity.maximum).toBe(26);
    expect(politicsCapacity(packet, { hardwareConcurrency: 28 }).recommended).toBe(1);
    const s = { ...prepared.scenario, execution: { ...prepared.scenario.execution, workers: { kind: 'fixed' as const, count: 6 }, simulations: 10 } };
    expect(politicsRunResources(s, capacity)).toEqual({ requested: 6, actual: 6 });
    const bad = structuredClone(packet); bad.keys.generationKey += '-stale';
    await expect(preparePoliticsCompute(bad, prepared.implementationId)).rejects.toThrow('BINDING_MISMATCH');
    if (packet.inputs.generator.id === 'M02') {
      const corrupt = structuredClone(packet); if (corrupt.inputs.generator.id !== 'M02') throw new Error('Expected M02');
      corrupt.inputs.generator.assets.fit.political.values[0] += 1;
      await expect(preparePoliticsCompute(corrupt, prepared.implementationId)).rejects.toThrow('BINDING_MISMATCH');
      const parents = structuredClone(packet); if (parents.inputs.generator.id !== 'M02') throw new Error('Expected M02');
      parents.inputs.generator.parents.fit.nationalInputKey = 'different-parent';
      const { sha256: _old, ...content } = parents; parents.sha256 = await structuredHash(content);
      await expect(preparePoliticsCompute(parents, prepared.implementationId)).rejects.toThrow('DEPENDENCY_MISMATCH');
    }
  }, 60000);
