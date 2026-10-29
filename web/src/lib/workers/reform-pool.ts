import type { ReformComputeReply, ReformComputeRequest } from './reform-compute.worker';
import type { ReformRunChunk, ReformRunReady } from './reform-compute';
import type { GenerationPacket } from '../politics/reform-2026/vote-share/fit-types';
import { ownedBuffers } from '../politics/reform-2026/vote-share/fit-resources';

type Request = ReformComputeRequest extends infer T ? T extends { requestId: number } ? Omit<T, 'requestId'> : never : never;
export interface ReformComputeConnection {
  compute(simulation: number): Promise<ReformRunReady>;
  pull(): Promise<ReformRunChunk | null>;
  close(): void;
}

export class ReformComputeClient implements ReformComputeConnection {
  private sequence = 0;
  private pending?: { id: number; resolve: (value: unknown) => void; reject: (error: Error) => void };
  private failure?: Error;
  constructor(private worker: Worker) {
    worker.onmessage = (event: MessageEvent<ReformComputeReply>) => {
      const pending = this.pending;
      if (!pending || event.data.requestId !== pending.id) { this.fail(new Error('Unexpected compute worker reply')); return; }
      this.pending = undefined;
      if (event.data.ok) pending.resolve(event.data.data);
      else { const error = new Error(event.data.error ?? 'Compute worker failed'); this.failure = error; pending.reject(error); }
    };
    worker.onerror = event => { event.preventDefault(); this.fail(new Error(event.message || 'Compute worker crashed')); };
    worker.onmessageerror = () => this.fail(new Error('Unable to decode compute worker message'));
  }
  private fail(error: Error) { this.failure = error; this.pending?.reject(error); this.pending = undefined; }
  private call<T>(request: Request,transfer:Transferable[]=[]): Promise<T> {
    if (this.failure) return Promise.reject(this.failure);
    if (this.pending) return Promise.reject(new Error('Concurrent compute worker requests'));
    return new Promise<T>((resolve, reject) => {
      const requestId = ++this.sequence;
      this.pending = { id: requestId, resolve: value => resolve(value as T), reject };
      try { this.worker.postMessage({ ...request, requestId },transfer); }
      catch (error) { this.fail(error instanceof Error ? error : new Error(String(error))); }
    });
  }
  initialize(scenario: unknown, basePath: string) { return this.call<void>({ action: 'init', scenario, basePath }); }
  initializeFast(packet:GenerationPacket){return this.call<void>({action:'init-fast',packet},ownedBuffers(packet));}
  compute(simulation: number) { return this.call<ReformRunReady>({ action: 'compute', simulation }); }
  pull() { return this.call<ReformRunChunk | null>({ action: 'pull' }); }
  close() { this.fail(new Error('Compute worker closed')); this.worker.terminate(); }
}

/** One outstanding simulation per worker. A slot is reused only after durable
 * ordered commit: even a slow first run cannot create an unbounded reorder queue.
 * Cancellation drains the contiguous assigned prefix, without new dispatch.
 */
export async function runOrderedReformPool(workers: ReformComputeConnection[], total: number,
  cancelled: () => boolean, commit: (simulation: number, worker: ReformComputeConnection, ready: ReformRunReady) => Promise<void>) {
  type Settled = { ready: ReformRunReady } | { error: unknown };
  const pending = new Map<number, { worker: ReformComputeConnection; ready: Promise<Settled> }>();
  let dispatched = 0;
  const dispatch = (worker: ReformComputeConnection) => {
    if (cancelled() || dispatched >= total) return;
    const simulation = ++dispatched;
    // Observe rejections immediately, including future slots and shutdown races.
    const ready = worker.compute(simulation).then(ready => ({ ready }), error => ({ error }));
    pending.set(simulation, { worker, ready });
  };
  try {
    for (const worker of workers) dispatch(worker);
    for (let simulation = 1; pending.has(simulation); simulation++) {
      const slot = pending.get(simulation)!, settled = await slot.ready;
      if ('error' in settled) throw settled.error;
      if (settled.ready.simulation !== simulation) throw new Error('Compute worker returned a different simulation ID');
      await commit(simulation, slot.worker, settled.ready);
      pending.delete(simulation);
      dispatch(slot.worker);
    }
  } finally { for (const worker of workers) worker.close(); }
}
