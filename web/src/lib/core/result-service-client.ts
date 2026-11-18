import type { ResultServiceReply, ResultServiceRequest, ResultServiceProgress } from '../workers/result-service';
import type { ArchiveProgressListener } from './archive-progress';

export class ResultServiceClient {
  private sequence = 0;
  private pending = new Map<number, { resolve: (value: unknown) => void; reject: (error: Error) => void; progress?: ArchiveProgressListener }>();
  constructor(readonly worker: Worker) {
    worker.addEventListener('message', this.receive);
    worker.addEventListener('error', this.failure);
    worker.addEventListener('messageerror', this.failure);
  }
  private failure = () => { for (const p of this.pending.values()) p.reject(new Error('Result worker failed')); this.pending.clear(); };
  private receive = (event: MessageEvent<ResultServiceReply | ResultServiceProgress>) => {
    if (event.data.type !== 'result-service' && event.data.type !== 'result-service-progress') return;
    const pending = this.pending.get(event.data.requestId); if (!pending) return;
    if (event.data.type === 'result-service-progress') { pending.progress?.(event.data.progress); return; }
    this.pending.delete(event.data.requestId);
    if (event.data.ok) pending.resolve(event.data.data);
    else pending.reject(new Error(`${event.data.error?.code}: ${event.data.error?.message}`));
  };
  call<T>(action: ResultServiceRequest['action'], fields: Omit<ResultServiceRequest, 'action' | 'type' | 'requestId'>, progress?: ArchiveProgressListener): Promise<T> {
    const requestId = ++this.sequence;
    return new Promise<T>((resolve, reject) => {
      this.pending.set(requestId, { resolve: value => resolve(value as T), reject, progress });
      try { this.worker.postMessage({ ...fields, type: 'result-service', requestId, action } satisfies ResultServiceRequest); }
      catch (error) { this.pending.delete(requestId); reject(error); }
    });
  }
  close() {
    this.worker.removeEventListener('message', this.receive);
    this.worker.removeEventListener('error', this.failure); this.worker.removeEventListener('messageerror', this.failure);
    for (const pending of this.pending.values()) pending.reject(new Error('Result worker closed'));
    this.pending.clear(); this.worker.terminate();
  }
}
