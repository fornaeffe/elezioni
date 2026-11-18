import { onDestroy } from 'svelte';
import type { SimulationProgress, SimulationRequest, SimulationResult, SimulationWorkerMessage } from '../core/types';
import { validateWorkspaceRequest } from './registry';

export interface WorkspaceWorker {
  onmessage: ((event: MessageEvent<SimulationWorkerMessage>) => void) | null;
  onerror: ((event: ErrorEvent) => void) | null;
  postMessage(request: SimulationRequest): void;
  terminate(): void;
}

/** One owned execution; teardown and late messages cannot mutate a new page. */
export function createWorkspaceExecution(factory: () => WorkspaceWorker) {
  let running = $state(false), phase = $state('idle'), elapsedMs = $state(0);
  let worker: WorkspaceWorker | undefined, revision = 0;
  function cancel() {
    revision++;
    worker?.terminate(); worker = undefined;
    running = false; phase = 'cancelled';
  }
  onDestroy(cancel);
  return {
    get running() { return running; }, get phase() { return phase; }, get elapsedMs() { return elapsedMs; },
    cancel,
    run(request: SimulationRequest, completed: (result: SimulationResult) => void, failed: (error: Error) => void,
      progress?: (message: SimulationProgress) => void) {
      cancel();
      const token = revision;
      try {
        validateWorkspaceRequest(request);
        const active = factory(); worker = active;
        running = true; phase = 'validate'; elapsedMs = 0;
        active.onmessage = event => {
          if (revision !== token) return;
          const message = event.data;
          if (message.type === 'progress') {
            phase = message.phase; elapsedMs = message.elapsedMs; progress?.(message); return;
          }
          revision++; active.terminate(); worker = undefined; running = false;
          phase = message.status; elapsedMs = message.benchmark.elapsedMs;
          completed(message);
        };
        active.onerror = event => {
          if (revision !== token) return;
          cancel(); phase = 'error'; failed(new Error(event.message));
        };
        active.postMessage(request);
      } catch (e) { cancel(); phase = 'error'; failed(e instanceof Error ? e : new Error(String(e))); }
    },
  };
}
