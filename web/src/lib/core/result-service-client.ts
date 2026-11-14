import type { ResultServiceReply, ResultServiceRequest } from '../workers/result-service';

export class ResultServiceClient {
  private sequence = 0;
  private pending = new Map<number, { resolve: (value: unknown) => void; reject: (error: Error) => void }>();
  constructor(readonly worker: Worker) {
    worker.addEventListener('message', this.receive);
    worker.addEventListener('error', this.failure);
    worker.addEventListener('messageerror', this.failure);
  }
  private failure = () => { for (const p of this.pending.values()) p.reject(new Error('Result worker failed')); this.pending.clear(); };
  private receive = (event: MessageEvent<ResultServiceReply>) => {
    if (event.data.type !== 'result-service') return;
    const pending = this.pending.get(event.data.requestId); if (!pending) return;
    this.pending.delete(event.data.requestId);
    if (event.data.ok) pending.resolve(event.data.data);
    else pending.reject(new Error(`${event.data.error?.code}: ${event.data.error?.message}`));
  };
  call<T>(action: ResultServiceRequest['action'], fields: Omit<ResultServiceRequest, 'action' | 'type' | 'requestId'>): Promise<T> {
    const requestId = ++this.sequence;
    return new Promise<T>((resolve, reject) => {
      this.pending.set(requestId, { resolve: value => resolve(value as T), reject });
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
