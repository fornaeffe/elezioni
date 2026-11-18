/** Conservative planning budget, not a measurement of available/free RAM.
 * See docs/politics-reform-2026-application.md#parallel-execution for evidence.
 * Includes transient generation/scrutiny allocations and one pending run.
 */
export const reformWorkerMemoryMiB = 1536;
export const reformCoordinatorMemoryMiB = 512;
export const reformWorkerEstimateVersion = 'r26-worker-memory-v1';

export interface WorkerResourceHints {
  hardwareConcurrency?: number;
  deviceMemory?: number;
}

export function estimateReformWorkerCapacity(hints: WorkerResourceHints) {
  const cores = Number.isFinite(hints.hardwareConcurrency) && hints.hardwareConcurrency! >= 1
    ? Math.floor(hints.hardwareConcurrency!) : 2;
  const reportedMemoryGiB = Number.isFinite(hints.deviceMemory) && hints.deviceMemory! > 0
    ? hints.deviceMemory! : null;
  // Leave half the reported RAM to the OS/other tabs, plus a coordinator budget.
  // Unsupported/private device-memory reporting uses a conservative 2 GiB hint.
  const budgetMiB = Math.floor((reportedMemoryGiB ?? 2) * 1024 / 2);
  const cpuMaximum = Math.max(1, cores - (cores >= 4 ? 2 : 1));
  const memoryMaximum = Math.max(1, Math.floor((budgetMiB - reformCoordinatorMemoryMiB) / reformWorkerMemoryMiB));
  return { maximum: cpuMaximum, recommended: Math.min(cpuMaximum, memoryMaximum), cpuMaximum, memoryMaximum,
    reportedMemoryGiB, budgetMiB, workerMemoryMiB: reformWorkerMemoryMiB, estimateVersion: reformWorkerEstimateVersion };
}

/** CPU is the selectable ceiling; RAM determines only the default and warning. */
export function planReformWorkers(hints: WorkerResourceHints, coordinatorBytes: number, perWorkerBytes: number,
  heap?: { budgetBytes: number; coordinatorBytes: number; perWorkerBytes: number }) {
  if (![coordinatorBytes, perWorkerBytes].every(n => Number.isSafeInteger(n) && n > 0)) throw new Error('Invalid worker memory estimate');
  const legacy = estimateReformWorkerCapacity(hints), MiB = 1024 * 1024;
  const deviceBytes = legacy.reportedMemoryGiB === null ? null : Math.floor(legacy.reportedMemoryGiB * 1024 * MiB);
  const budgetBytes = Math.floor((deviceBytes ?? 2 * 1024 * MiB) / 2);
  if (heap && !Object.values(heap).every(n => Number.isSafeInteger(n) && n > 0)) throw new Error('Invalid browser heap estimate');
  const heapRecommended = heap ? Math.max(1, Math.floor((heap.budgetBytes - heap.coordinatorBytes) / heap.perWorkerBytes)) : legacy.cpuMaximum;
  const recommended = deviceBytes === null ? 1 : Math.max(1, Math.min(legacy.cpuMaximum, heapRecommended, Math.floor((budgetBytes - coordinatorBytes) / perWorkerBytes)));
  return { maximum: legacy.cpuMaximum, recommended, budgetBytes, deviceBytes, coordinatorBytes, perWorkerBytes,
    heap, heapRecommended, estimateVersion: 'r26-cpu-ceiling-half-declared-memory-browser-v3' };
}

export function workerMemoryWarning(count: number, capacity: ReturnType<typeof planReformWorkers>) {
  return capacity.deviceBytes !== null && capacity.coordinatorBytes + count * capacity.perWorkerBytes >= capacity.deviceBytes;
}
export function workerHeapWarning(count: number, capacity: ReturnType<typeof planReformWorkers>) {
  return !!capacity.heap && capacity.heap.coordinatorBytes + count * capacity.heap.perWorkerBytes > capacity.heap.budgetBytes;
}

export function requireReformWorkerCount(value: number, maximum: number): number {
  if (!Number.isSafeInteger(value) || value < 1 || value > maximum) {
    throw new RangeError(`Compute worker count must be an integer from 1 to ${maximum}`);
  }
  return value;
}
