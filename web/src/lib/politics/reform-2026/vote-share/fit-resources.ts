import type { CompactSource } from './source-types';
import type { AcceptedFit, GenerationPacket } from './fit-types';
import type { SourceSelection } from './source-compiler';
import { type WorkerResourceHints } from '../../../core/reform-worker-capacity';
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
  const aggregate = limits.maximumAggregateBytes ?? 512 * MiB,
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
  const cores = Number.isFinite(hints.hardwareConcurrency)
      ? Math.max(1, Math.floor(hints.hardwareConcurrency!))
      : 2,
    memory =
      Number.isFinite(hints.deviceMemory) && hints.deviceMemory! > 0
        ? hints.deviceMemory!
        : 2,
    // Fast packets omit the legacy historical snapshot. Reserve the measured
    // legal/result scratch plus two retained-packet copies per owner, and cap
    // the whole active pipeline at the frozen FV-08 512 MiB target.
    budgetBytes = Math.min(512 * MiB, Math.floor((memory * 1024 * MiB) / 2)),
    coordinatorBytes =
      128 * MiB + 2 * retainedBytes(packet) + additionalReservedBytes,
    perWorkerBytes = 112 * MiB + 2 * retainedBytes(packet),
    maximum = Math.max(
      0,
      Math.min(
        Math.max(1, cores - 1),
        Math.floor((budgetBytes - coordinatorBytes) / perWorkerBytes),
      ),
    );
  return {
    maximum,
    budgetBytes,
    coordinatorBytes,
    perWorkerBytes,
    estimateVersion: 'fast-share-aggregate-512-v2',
  };
}
export function copyGenerationPacket(accepted: AcceptedFit): GenerationPacket {
  return structuredClone({
    scenario: accepted.scenario,
    assets: accepted.assets,
    context: accepted.context,
    keys: accepted.keys,
  });
}
