import { loadVerifiedJson } from '../core/runtime-data';
import catalog from '../politics/reform-2026/data/generation-catalog.generated.json';
import { prepareReformGeneration, type PreparedReformGeneration } from '../politics/reform-2026/generation';
import type { ReformGenerationSnapshot } from '../politics/reform-2026/generation-types';
import { computeReformRun, encodeReformRun, prepareFastCompute, type PreparedFastCompute, type ReformRunChunk } from './reform-compute';
import type { GenerationPacket } from '../politics/reform-2026/vote-share/fit-types';

export type ReformComputeRequest = { requestId: number } & (
  { action: 'init'; scenario: unknown; basePath: string } |
  { action:'init-fast';packet:GenerationPacket } |
  { action: 'compute'; simulation: number } | { action: 'pull' }
);
export interface ReformComputeReply { requestId: number; ok: boolean; data?: unknown; error?: string }

let prepared: PreparedReformGeneration | PreparedFastCompute | undefined;
let chunks: Generator<ReformRunChunk> | undefined;
let busy = false;
self.onmessage = async (event: MessageEvent<ReformComputeRequest>) => {
  const request = event.data;
  try {
    if (busy) throw new Error('Compute worker already handling a request');
    busy = true;
    let data: unknown;
    if(request.action==='init-fast'){
      if(prepared)throw new Error('Compute worker already initialized');prepared=await prepareFastCompute(request.packet);data=null;
    }else if (request.action === 'init') {
      if (prepared) throw new Error('Compute worker already initialized');
      const snapshot = await loadVerifiedJson(request.basePath, catalog) as ReformGenerationSnapshot;
      prepared = prepareReformGeneration(request.scenario, snapshot);
      data = null;
    } else if (request.action === 'compute') {
      if (!prepared || chunks) throw new Error('Compute worker is not ready for another simulation');
      const run = computeReformRun(prepared, request.simulation);
      chunks = encodeReformRun(run.batch);
      data = run.ready;
    } else {
      if (!chunks) throw new Error('No simulation available to drain');
      const next = chunks.next();
      if (next.done) { chunks = undefined; data = null; }
      else {
        self.postMessage({ requestId: request.requestId, ok: true, data: next.value } satisfies ReformComputeReply, { transfer: [next.value.buffer] });
        return;
      }
    }
    self.postMessage({ requestId: request.requestId, ok: true, data } satisfies ReformComputeReply);
  } catch (error) {
    self.postMessage({ requestId: request.requestId, ok: false, error: error instanceof Error ? `${error.name}: ${error.message}` : String(error) } satisfies ReformComputeReply);
  } finally { busy = false; }
};
