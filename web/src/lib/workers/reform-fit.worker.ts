/** One ephemeral fitting task. The client owns accepted buffers and terminates
 * this worker after result/error/cancel, releasing all decoded source histories. */
import { runtimeDataUrl } from '../core/runtime-data';
import { runFitTask } from '../politics/reform-2026/vote-share/fit-service';
import { ownedBuffers } from '../politics/reform-2026/vote-share/fit-resources';
import {
  MAX_SOURCE_BYTES,
  type AssetBytes,
} from '../politics/reform-2026/vote-share/source-codec';
import { throwIfCancelled } from '../politics/reform-2026/vote-share/execution';
import type {
  FitWorkerRequest,
  FitWorkerReply,
} from '../politics/reform-2026/vote-share/fit-types';
const scope = self as unknown as {
  onmessage: ((event: MessageEvent<FitWorkerRequest>) => void) | null;
  postMessage(message: FitWorkerReply, transfer?: Transferable[]): void;
};
let running: { id: number; controller: AbortController } | undefined;
scope.onmessage = async ({ data: r }) => {
  if (r.action === 'cancel') {
    if (running?.id === r.requestId) running.controller.abort();
    return;
  }
  const binding = {
    requestId: r.requestId,
    revision: r.revision,
    fingerprint: r.fingerprint,
  };
  if (running) {
    scope.postMessage({
      ...binding,
      action: 'error',
      error: 'Concurrent fit task rejected',
      cancelled: false,
    });
    return;
  }
  const controller = new AbortController();
  running = { id: r.requestId, controller };
  const started = performance.now();
  let yieldedAt = started,
    reportedAt = -Infinity,
    phase = '',
    maximumNumericBatchMs = 0,
    progressMessages = 0;
  try {
    const read = async (path: string): Promise<AssetBytes> => {
      throwIfCancelled(controller.signal);
      if (!/^[\w.-]+$/.test(path) || path === '.' || path === '..')
        throw new Error('Unsafe fit asset path');
      const response = await fetch(
        runtimeDataUrl(
          r.configuration.base,
          r.configuration.pointer.assetRoot + path,
        ),
        { signal: controller.signal },
      );
      if (!response.ok)
        throw new Error(
          'Missing fit asset: HTTP ' + response.status + ' ' + path,
        );
      const reader = response.body?.getReader();
      if (!reader) throw new Error('Missing fit asset body');
      const parts: Uint8Array[] = [];
      let size = 0;
      try {
        for (;;) {
          const part = await reader.read();
          if (part.done) break;
          size += part.value.length;
          if (size > MAX_SOURCE_BYTES)
            throw new Error('FIT_ASSET_MEMORY_LIMIT');
          parts.push(part.value);
        }
      } finally {
        await reader.cancel();
      }
      const body = new Uint8Array(size);
      let at = 0;
      for (const p of parts) {
        body.set(p, at);
        at += p.length;
      }
      return { transport: 'http', body };
    };
    const result = await runFitTask(r, read, {
      signal: controller.signal,
      checkpoint: async (progress) => {
        throwIfCancelled(controller.signal);
        const now = performance.now();
        maximumNumericBatchMs = Math.max(
          maximumNumericBatchMs,
          progress.numericBatchMs ?? 0,
        );
        if (progress.phase !== phase || now - reportedAt >= 32) {
          scope.postMessage({ ...binding, action: 'progress', progress });
          phase = progress.phase;
          reportedAt = now;
          progressMessages++;
        }
        if (now - yieldedAt >= 8) {
          await new Promise<void>((done) => setTimeout(done, 0));
          yieldedAt = performance.now();
        }
        throwIfCancelled(controller.signal);
      },
    });
    throwIfCancelled(controller.signal);
    result.audit = {
      ...result.audit,
      elapsedMs: performance.now() - started,
      maximumNumericBatchMs,
      progressMessages,
    };
    scope.postMessage(
      { ...binding, action: 'result', result },
      ownedBuffers(result),
    );
  } catch (error) {
    scope.postMessage({
      ...binding,
      action: 'error',
      error: error instanceof Error ? error.message : String(error),
      cancelled: controller.signal.aborted,
    });
  } finally {
    running = undefined;
  }
};
