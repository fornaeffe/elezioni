import type { CompactSource } from './source-types';
import type { AcceptedFit, GenerationPacket } from './fit-types';
import type { SourceSelection } from './source-compiler';
import { planReformWorkers, type WorkerResourceHints } from '../../../core/reform-worker-capacity';
export const MiB = 1024 * 1024;
/** Collect unique OWNED transferable buffers; never transfer an accepted state directly. */
export function ownedBuffers(value: unknown): ArrayBuffer[] {
  const buffers = new Set<ArrayBuffer>(),
    seen = new Set<object>();
  const visit = (x: unknown) => {
    if (!x || typeof x !== 'object' || seen.has(x)) return;
    seen.add(x);
    if (ArrayBuffer.isView(x)) {
      if (!(x.buffer instanceof ArrayBuffer))
        throw new Error('Shared numeric buffers are unsupported');
      buffers.add(x.buffer);
      return;
    }
    if (x instanceof ArrayBuffer) {
      buffers.add(x);
      return;
    }
    for (const v of Object.values(x)) visit(v);
  };
  visit(value);
  return [...buffers];
}
/** Conservative object/buffer accounting, not browser heap instrumentation. */
export function retainedBytes(value: unknown): number {
  const seen = new Set<object>();
  let bytes = 0;
  const visit = (x: unknown) => {
    if (typeof x === 'string') {
      bytes += 32 + 2 * x.length;
      return;
    }
    if (!x || typeof x !== 'object') {
      bytes += 8;
      return;
    }
    if (seen.has(x)) return;
    seen.add(x);
    if (ArrayBuffer.isView(x)) {
      bytes += 64;
      visit(x.buffer);
      return;
    }
    if (x instanceof ArrayBuffer) {
      bytes += x.byteLength;
      return;
    }
    bytes += 64;
    for (const [k, v] of Object.entries(x)) {
      bytes += 16 + k.length * 2;
      visit(v);
    }
  };
  visit(value);
  return bytes;
}
export function fitMemoryPlan(
  previous: AcceptedFit,
  source: CompactSource | undefined,
  selection: SourceSelection,
  limits: {
    maximumAggregateBytes?: number;
    maximumWorkspaceBytes?: number;
    resultPipelineBytes?: number;
  } = {},
) {
  const aggregate = limits.maximumAggregateBytes ?? Number.MAX_SAFE_INTEGER,
    workspace = limits.maximumWorkspaceBytes ?? 256 * MiB,
    results = limits.resultPipelineBytes ?? 64 * MiB;
  if (
    [aggregate, workspace].some((n) => !Number.isSafeInteger(n) || n <= 0) ||
    !Number.isSafeInteger(results) ||
    results < 0 ||
    workspace > 256 * MiB
  )
    throw new Error('INVALID_FIT_MEMORY_BUDGET');
  const selected =
    source?.payload.events.filter((e) =>
      [
        ...selection.electionIds,
        previous.scenario.historySelection.pairedCamera,
        previous.scenario.historySelection.pairedSenato,
      ].includes(e.id),
    ) ?? [];
  const k = selection.components.length,
    n = selection.targets.length;
  // Includes decoded columns, original row/group objects, pooled histories and
  // retained diagnostic draws at the largest accepted retention (8,000/chain).
  const history = selected.reduce(
    (s, e) =>
      s + e.chunk.rows * 192 + e.chunk.values * 160 + e.chunk.decodedBytes * 2,
    0,
  );
  const observationRows =
    (selection.electionIds.length + selection.pollIds.length) *
    Math.max(1, k - 1);
  const dense =
    8 *
    (3 * observationRows ** 2 + 12 * (2 * k) ** 2 + 64 * observationRows * k);
  const numerical = Math.max(
    8 * (2 * k) ** 2 + 4 * 8000 * (2 * k) * 16 + 4 * 8000 * 192,
    selected.reduce((s, e) => s + e.chunk.rows * 64, 0) + n * (k + 1) * 64,
    Math.min(dense, workspace),
  );
  const estimatedAggregateBytes =
    3 * retainedBytes(previous) + history + numerical + results + 32 * MiB;
  if (estimatedAggregateBytes > aggregate)
    throw new Error(
      'FIT_AGGREGATE_MEMORY_LIMIT: estimated ' +
        estimatedAggregateBytes +
        '; limit ' +
        aggregate,
    );
  return {
    estimatedAggregateBytes,
    maximumWorkspaceBytes: workspace,
    solver: dense <= workspace ? ('dense' as const) : ('matrix-free' as const),
  };
}
/** Account for the existing scrutiny/results worker budget, plus authoritative
 * fitted buffers and all prospective handoff copies; no forced unsafe minimum. */
export function generationCapacity(
  packet: GenerationPacket,
  hints: WorkerResourceHints,
  additionalReservedBytes = 0,
) {
  if (
    !Number.isSafeInteger(additionalReservedBytes) ||
    additionalReservedBytes < 0
  )
    throw new Error('Invalid reserved memory');
  const coordinatorBytes =
      128 * MiB + 2 * retainedBytes(packet) + additionalReservedBytes,
    // Conservative scratch plus explicit owned packet copies. The old CDP-only
    // samples did not enumerate nested workers; FV-08 renderer-process samples
    // provide the complete process-footprint check. The compact legacy adapter
    // measured ~4.3 GiB across nine workers and the coordinator in FV-08;
    // 384 MiB scratch plus actual packet copies replaces the old 1,536 MiB
    // estimate for the substantially larger original-workspace data structure.
    perWorkerBytes = (packet.scenario.generation?.model === 'legacy-poll-informed-v1' ? 384 : 320) * MiB + 2 * retainedBytes(packet);
  // V8 can share a 4 GiB pointer-compression cage across workers in one
  // renderer. A 3 GiB planning budget leaves headroom for startup/GC and UI.
  // The 10-worker browser probe and Luca's manual trial passed; 26 failed.
  // RAM estimates include off-heap buffers and remain separate from this guard.
  return planReformWorkers(hints, coordinatorBytes, perWorkerBytes, {
    budgetBytes: 3072 * MiB, coordinatorBytes: 512 * MiB,
    perWorkerBytes: Math.max(256 * MiB, retainedBytes(packet)),
  });
}
export function copyGenerationPacket(accepted: AcceptedFit): GenerationPacket {
  return structuredClone({
    scenario: accepted.scenario,
    assets: accepted.assets,
    context: accepted.context,
    keys: accepted.keys,
    ...(accepted.legacy ? { legacy: accepted.legacy } : {}),
  });
}
