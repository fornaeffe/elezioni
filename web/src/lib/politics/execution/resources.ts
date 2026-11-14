import { planReformWorkers, requireReformWorkerCount, type WorkerResourceHints } from '../../core/reform-worker-capacity';
import { retainedBytes } from '../reform-2026/vote-share/fit-resources';
import type { PoliticsComputePacket } from '../../workers/politics-compute';
import type { PoliticsScenario } from '../scenario/schema';
import { activeGenerator, requirePair } from '../scenario/registry';

/** Packet copies plus generation/legal scratch. Rosatellum's full national
 * candidate matrices need the original 1536 MiB scrutiny budget until CP-08
 * measures the common adapter; estimates are labelled, never free-RAM claims. */
export function politicsCapacity(packet: PoliticsComputePacket, hints: WorkerResourceHints) {
  const s = packet.scenario as PoliticsScenario, bytes = retainedBytes(packet), MiB = 1024 * 1024;
  const scratch = s.selection.scrutiny.id === 'politiche-r-parity-v1' ? 1536 : activeGenerator(s).id === 'M01' ? 384 : 320;
  return planReformWorkers(hints, 128 * MiB + 2 * bytes, scratch * MiB + 2 * bytes,
    { budgetBytes: 3072 * MiB, coordinatorBytes: 512 * MiB, perWorkerBytes: Math.max(256 * MiB, bytes) });
}
export function politicsRunResources(s: PoliticsScenario, capacity: ReturnType<typeof politicsCapacity>) {
  requirePair(s.selection.scrutiny.id, activeGenerator(s).id);
  if (!Number.isSafeInteger(s.execution.simulations) || s.execution.simulations < 1) throw new Error('Expected a positive safe simulation count');
  if (s.execution.simulations * 64 > 64 * 1024 * 1024) throw new Error('Simulation membership exceeds the 64 MiB session metadata budget');
  const requested = s.execution.workers.kind === 'fixed'
    ? requireReformWorkerCount(s.execution.workers.count, capacity.maximum) : capacity.recommended;
  return { requested, actual: Math.min(requested, s.execution.simulations) };
}
