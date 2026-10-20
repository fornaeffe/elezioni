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
  const cpuMaximum = Math.max(1, cores - 1);
  const memoryMaximum = Math.max(1, Math.floor((budgetMiB - reformCoordinatorMemoryMiB) / reformWorkerMemoryMiB));
  return { maximum: Math.min(cpuMaximum, memoryMaximum), cpuMaximum, memoryMaximum,
    reportedMemoryGiB, budgetMiB, workerMemoryMiB: reformWorkerMemoryMiB, estimateVersion: reformWorkerEstimateVersion };
}

export function requireReformWorkerCount(value: number, maximum: number): number {
  if (!Number.isSafeInteger(value) || value < 1 || value > maximum) {
    throw new RangeError(`Compute worker count must be an integer from 1 to ${maximum}`);
  }
  return value;
}
