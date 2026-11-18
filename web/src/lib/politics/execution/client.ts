import { ResultServiceClient } from '../../core/result-service-client';
import type { StoredResult } from '../../core/result-store';
import type { PoliticsScenario } from '../scenario/schema';
import type { PoliticsCheckpoint } from '../scenario/envelopes';
import type { PreparedPoliticsExecution } from './prepare';
import type { politicsCapacity } from './resources';
import type { SimulationProgress } from '../../core/types';

export interface PreparationSummary {
  note?: string;
  keys: PreparedPoliticsExecution['keys']; reuse: PreparedPoliticsExecution['reuse'];
  capacity: ReturnType<typeof politicsCapacity>;
}
export interface PoliticsRuntime {
  prepare(scenario: PoliticsScenario, force?: boolean, checkpoint?: PoliticsCheckpoint): Promise<PreparationSummary>;
  run(scenario: PoliticsScenario, id: string, owner: string, progress: (p: SimulationProgress) => void): Promise<StoredResult>;
  cancel(): Promise<void>;
  checkpoint(): Promise<PoliticsCheckpoint | null>;
  close(): void;
}
export class PoliticsClient implements PoliticsRuntime {
  readonly results: ResultServiceClient;
  private sequence = 0;
  private pending = new Map<number, { resolve: (v: unknown) => void; reject: (e: Error) => void; progress?: (p: SimulationProgress) => void }>();
  onPreparationProgress?: (detail: string, completed: number, total: number) => void;
  private preparationRevision = 0;
  private closing = false;
  constructor(readonly worker: Worker, private basePath: string) {
    this.results = new ResultServiceClient(worker);
    worker.addEventListener('message', this.receive);
    worker.addEventListener('error', this.failure);
    worker.addEventListener('messageerror', this.failure);
  }
  private receive = (event: MessageEvent) => {
    const m = event.data; if (m.type !== 'politics-task') return;
    const pending = this.pending.get(m.requestId); if (!pending) return;
    if (m.progress) { pending.progress?.(m.progress); return; }
    this.pending.delete(m.requestId);
    if (m.ok) pending.resolve(m.data); else pending.reject(new Error(m.error));
  };
  private failure = () => { for (const p of this.pending.values()) p.reject(new Error('Politics worker failed')); this.pending.clear(); this.results.close(); };
  private call<T>(action: string, fields: object = {}, progress?: (p: SimulationProgress) => void): Promise<T> {
    const requestId = ++this.sequence;
    return new Promise<T>((resolve, reject) => {
      this.pending.set(requestId, { resolve: v => resolve(v as T), reject, progress });
      try { this.worker.postMessage({ type: 'politics-task', requestId, action, basePath: this.basePath, ...fields }); }
      catch (e) { this.pending.delete(requestId); reject(e); }
    });
  }
  prepare(scenario: PoliticsScenario, force = false, checkpoint?: PoliticsCheckpoint) {
    const revision = ++this.preparationRevision;
    return this.call<PreparationSummary>('prepare', { scenario, force, checkpoint }, p =>
      { if (revision === this.preparationRevision) this.onPreparationProgress?.((p as SimulationProgress & { detail?: string }).detail ?? p.phase, p.completed, p.total); });
  }
  run(scenario: PoliticsScenario, id: string, owner: string, progress: (p: SimulationProgress) => void) { return this.call<StoredResult>('run', { scenario, id, owner }, progress); }
  cancel() { ++this.preparationRevision; return this.call<void>('cancel'); }
  checkpoint() { return this.call<PoliticsCheckpoint | null>('checkpoint'); }
  close() {
    if (this.closing) return; this.closing = true;
    for (const p of this.pending.values()) p.reject(new Error('Politics worker closed')); this.pending.clear();
    const stop = () => {
      this.worker.removeEventListener('message', this.receive); this.worker.removeEventListener('error', this.failure);
      this.worker.removeEventListener('messageerror', this.failure); this.results.close();
    };
    // Give the coordinator a chance to close every nested compute worker.
    const timeout = setTimeout(stop, 1000);
    void this.call<void>('dispose').catch(() => undefined).finally(() => { clearTimeout(timeout); stop(); });
  }
}
