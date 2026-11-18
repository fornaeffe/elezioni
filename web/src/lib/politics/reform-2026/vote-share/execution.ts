/** Shared arithmetic steppers: synchronous Node and cooperative worker scheduling. */
export interface FitProgress {
  phase: string;
  completed: number;
  total: number;
  numericBatchMs?: number;
}
export type FitSteps<T> = Generator<FitProgress, T, void>;
export function completeSteps<T>(steps: FitSteps<T>): T {
  let result = steps.next();
  while (!result.done) result = steps.next();
  return result.value;
}
export function throwIfCancelled(signal?: AbortSignal) {
  if (signal?.aborted)
    throw new DOMException('Parameter estimation cancelled', 'AbortError');
}
export async function cooperativeSteps<T>(
  steps: FitSteps<T>,
  checkpoint: (p: FitProgress) => Promise<void>,
): Promise<T> {
  try {
    for (;;) {
      const started = performance.now(),
        result = steps.next(),
        numericBatchMs = performance.now() - started;
      if (result.done) {
        await checkpoint({
          phase: 'numeric-complete',
          completed: 1,
          total: 1,
          numericBatchMs,
        });
        return result.value;
      }
      await checkpoint({ ...result.value, numericBatchMs });
    }
  } finally {
    steps.return(undefined as never);
  }
}
