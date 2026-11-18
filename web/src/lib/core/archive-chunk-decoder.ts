import { gunzipSync } from 'fflate';

// Two workers captured nearly all the end-to-end benefit on this machine.
const archiveCodecWorkers = 2;

/** Owned by a single archive operation. No model inputs or checkpoint arrays
 * enter these workers. The caller supplies at most one bounded prefetch batch. */
export class ArchiveChunkDecoder {
  private workers: Worker[] = [];
  private sequence = 0;
  private pending = new Map<number, { resolve: (bytes: Uint8Array) => void; reject: (error: Error) => void }>();
  private disabled = false;
  constructor(private readonly compressedBytes: number) {}
  private stop = () => {
    this.disabled = true;
    for (const worker of this.workers) worker.terminate(); this.workers = [];
    for (const job of this.pending.values()) job.reject(new Error('Archive decoder closed')); this.pending.clear();
  };
  close() { this.stop(); }
  async decode(chunks: readonly Uint8Array[]): Promise<Uint8Array[]> {
    if (!this.disabled && archiveCodecWorkers > 0 && this.compressedBytes >= 2 * 1024 * 1024 &&
      typeof Worker !== 'undefined' && navigator.hardwareConcurrency >= 4) {
      try {
        if (!this.workers.length) for (let i = 0; i < archiveCodecWorkers; i++) {
          const worker = new Worker(new URL('../workers/archive-codec.worker.ts', import.meta.url), { type: 'module' });
          this.workers.push(worker);
          worker.onmessage = event => {
            const job = this.pending.get(event.data.id); if (!job) return;
            this.pending.delete(event.data.id);
            if (event.data.error) job.reject(new Error(event.data.error)); else job.resolve(event.data.bytes);
          };
          worker.onerror = event => { event.preventDefault(); this.stop(); };
          worker.onmessageerror = this.stop;
        }
        const timeout = setTimeout(this.stop, 10000);
        try { return await Promise.all(chunks.map((bytes, i) => new Promise<Uint8Array>((resolve, reject) => {
          const id = ++this.sequence; this.pending.set(id, { resolve, reject });
          // Retain the compressed source for a complete serial retry after any
          // worker/transfer failure. Only this bounded copy is transferred.
          const copy = bytes.slice();
          this.workers[i % this.workers.length].postMessage({ id, bytes: copy }, [copy.buffer]);
        }))); } finally { clearTimeout(timeout); }
      } catch { this.stop(); }
    }
    return chunks.map(bytes => gunzipSync(bytes));
  }
}
