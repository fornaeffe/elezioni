/** Transport policy for pinned source GETs. Byte verification stays in sources.ts. */
import { Agent } from 'undici';
import type { Source } from './sources';

const attempts = 3, deadlineMs = 180_000;
const retryStatuses = new Set([408, 429, 500, 502, 503, 504]);
// getaddrinfo may report ENOTFOUND for intermittent resolver/OS lookup failures,
// even after this hostname succeeded. A persistent failure still exhausts the
// same bounded attempts/deadline; source byte verification remains separate.
const retryCodes = new Set(['UND_ERR_CONNECT_TIMEOUT', 'UND_ERR_HEADERS_TIMEOUT', 'UND_ERR_BODY_TIMEOUT',
  'UND_ERR_SOCKET', 'ECONNRESET', 'ECONNREFUSED', 'ETIMEDOUT', 'EAI_AGAIN', 'ENOTFOUND', 'ENETUNREACH', 'EHOSTUNREACH']);

class HttpFailure extends Error {
  readonly code: string;
  constructor(readonly status: number, readonly retryAfterMs: number) {
    super(`HTTP ${status}`); this.code = `HTTP_${status}`;
  }
}
function failureCode(error: unknown): string {
  const seen = new Set<unknown>();
  while (error && typeof error === 'object' && !seen.has(error)) {
    seen.add(error);
    const e = error as { code?: unknown; cause?: unknown };
    if (typeof e.code === 'string') return e.code;
    error = e.cause;
  }
  return 'UNKNOWN';
}
function retryAfter(value: string | null): number {
  if (value === null) return 0;
  const milliseconds = /^\d+(?:\.\d+)?$/.test(value) ? Number(value) * 1000 : Date.parse(value) - Date.now();
  return Number.isFinite(milliseconds) ? Math.max(0, milliseconds) : 0;
}
function backoff(milliseconds: number, signal: AbortSignal) {
  return new Promise<void>((resolve, reject) => {
    signal.throwIfAborted();
    const aborted = () => { clearTimeout(timer); reject(signal.reason); };
    const timer = setTimeout(() => { signal.removeEventListener('abort', aborted); resolve(); }, milliseconds);
    signal.addEventListener('abort', aborted, { once: true });
  });
}

export async function downloadSource(source: Source): Promise<Buffer> {
  // Scoped dispatcher: do not change global fetch or browser networking. The
  // 30s connection limit is separate from the shared 180s source deadline.
  const host = new URL(source.url).host, started = Date.now();
  const agent = new Agent({ connect: { timeout: 30_000 } }), controller = new AbortController();
  const timer = setTimeout(() => controller.abort(new Error('Source download deadline exceeded')), deadlineMs);
  const elapsed = () => `${((Date.now() - started) / 1000).toFixed(1)}s`;
  let attempt = 0;
  try {
    for (attempt = 1; attempt <= attempts; attempt++) {
      controller.signal.throwIfAborted();
      console.log(`SOURCE start: id=${source.id}; host=${host}; attempt=${attempt}/${attempts}; elapsed=${elapsed()}`);
      try {
        const init: RequestInit & { dispatcher: Agent } = { headers: source.headers, signal: controller.signal, dispatcher: agent };
        const response = await fetch(source.url, init);
        if (!response.ok) {
          const wait = retryAfter(response.headers.get('retry-after'));
          await response.body?.cancel();
          throw new HttpFailure(response.status, wait);
        }
        const bytes = Buffer.from(await response.arrayBuffer());
        controller.signal.throwIfAborted();
        console.log(`SOURCE received: id=${source.id}; host=${host}; attempt=${attempt}/${attempts}; elapsed=${elapsed()}; bytes=${bytes.length}`);
        return bytes;
      } catch (error) {
        const code = controller.signal.aborted ? 'SOURCE_DEADLINE_EXCEEDED' : failureCode(error);
        const transient = error instanceof HttpFailure ? retryStatuses.has(error.status) : retryCodes.has(code);
        const retry = transient && attempt < attempts && !controller.signal.aborted;
        console.warn(`SOURCE failed: id=${source.id}; host=${host}; attempt=${attempt}/${attempts}; elapsed=${elapsed()}; code=${code}; retry=${retry}`);
        if (!retry) throw error;
        const wait = Math.max(attempt === 1 ? 2000 : 5000, error instanceof HttpFailure ? error.retryAfterMs : 0);
        // A server-requested wait must never extend the source deadline or
        // overflow Node's timer range. Expiry aborts this wait and ends retries.
        await backoff(Math.min(wait, deadlineMs), controller.signal);
      }
    }
    throw new Error('Source download attempts exhausted');
  } catch (error) {
    const code = controller.signal.aborted ? 'SOURCE_DEADLINE_EXCEEDED' : failureCode(error);
    throw new Error(`Source ${source.id} download failed; host=${host}; attempts=${attempt}/${attempts}; elapsed=${elapsed()}; code=${code}: ${error instanceof Error ? error.message : String(error)}`, { cause: error });
  } finally {
    clearTimeout(timer);
    // Includes failed/aborted bodies; no sockets or partial downloads survive.
    await agent.destroy();
  }
}
