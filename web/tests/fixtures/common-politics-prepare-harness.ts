/** Browser-only worker test setup. Original observations stop at this boundary. */
import { preparePoliticsExecution } from '../../src/lib/politics/execution/prepare';
import { createPoliticsComputePacket } from '../../src/lib/politics/execution/compute-packet';
export { ownedBuffers } from '../../src/lib/politics/reform-2026/vote-share/fit-resources';

export async function prepare(input: { scenario: unknown; implementationId: string;
  assets: { id: string; bytes: number[] }[]; sourceChunks: { path: string; bytes: number[] }[] }) {
  const assets = new Map(input.assets.map(a => [a.id, Uint8Array.from(a.bytes)]));
  const chunks = new Map(input.sourceChunks.map(a => [a.path, Uint8Array.from(a.bytes)]));
  const prepared = await preparePoliticsExecution(input.scenario, { implementationId: input.implementationId,
    asset: async pin => { const b = assets.get(pin.id); if (!b) throw new Error('Missing test asset'); return b; },
    sourceChunk: async path => { const b = chunks.get(path); if (!b) throw new Error('Missing test chunk'); return b; } });
  return createPoliticsComputePacket(prepared);
}
