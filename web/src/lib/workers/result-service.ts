import { loadVerifiedJson, requireSimulationCount } from '../core/runtime-data';
import { appendResultBatch, deleteStoredResult, getStoredResult, newResultStorage, saveStoredResult, storedTablePage, storedWarningPage, ResultChunkWriter, warningCells, type StoredResult } from '../core/result-store';
import { importResultStream, streamStoredResult, type ExportScenario } from '../core/result-export';
import { jsonFileStream } from '../core/json-stream';
import type { SimulationProgress, SimulationRequest, SimulationResult } from '../core/types';
import catalog from '../politics/reform-2026/data/generation-catalog.generated.json';
import { generateReformSimulation, prepareReformGeneration } from '../politics/reform-2026/generation';
import type { ReformGenerationSnapshot } from '../politics/reform-2026/generation-types';
import { createReformPresentation } from '../politics/reform-2026/result-presentation';
import { runPoliticsSimulation, resolvePoliticsSimulationAlgorithm } from '../politics/simulation-scrutiny';
import { REFORM_ALGORITHM_ID } from '../politics/reform-2026/input';

export interface ResultServiceRequest {
  type: 'result-service'; requestId: number;
  action: 'page' | 'warnings' | 'manifest' | 'delete' | 'cancel' | 'import' | 'export';
  resultId: string; owner: string;
  table?: number; offset?: number; limit?: number; simulation?: number;
  file?: File; format?: 'json' | 'csv';
}
export interface ResultServiceReply {
  type: 'result-service'; requestId: number; ok: boolean; data?: unknown;
  error?: { code: string; message: string };
}
const cancelled = new Set<string>();
const active = new Set<string>();
const describe = (error: unknown) => error instanceof Error ? `${error.name}: ${error.message}` : String(error);

export async function runStoredReform(request: SimulationRequest, emit: (progress: SimulationProgress) => void): Promise<SimulationResult> {
  requireSimulationCount(request.simulations);
  resolvePoliticsSimulationAlgorithm(REFORM_ALGORITHM_ID, 'production');
  if (!('kind' in request.scenario)) throw new Error('A reform request requires an explicit reform scenario');
  const started = performance.now(), startedAt = new Date().toISOString();
  const progress = (phase: SimulationProgress['phase'], completed: number) => emit({ type: 'progress', phase, completed, total: request.simulations, elapsedMs: performance.now() - started });
  progress('prepare', 0);
  const snapshot = await loadVerifiedJson(request.basePath ?? '', catalog) as ReformGenerationSnapshot;
  const prepared = prepareReformGeneration(request.scenario, snapshot);
  if (prepared.scenario.seed !== request.seed || prepared.scenario.electionDate !== request.electionDate) throw new Error('Request seed/date must match the reform scenario');
  const id = request.resultId ?? crypto.randomUUID(), owner = request.resultOwner ?? 'direct-worker';
  const record: StoredResult = { id, owner, scenario: prepared.scenario, exports: [], result: {
    type: 'result', status: 'not_implemented', tables: [], warnings: [], storage: newResultStorage(id),
    benchmark: { startedAt, elapsedMs: 0, simulations: request.simulations, dataVersion: snapshot.version, scrutinyAlgorithmId: REFORM_ALGORITHM_ID, seed: request.seed }
  } };
  await saveStoredResult(record);
  const presentation = createReformPresentation(prepared.scenario, snapshot, request.simulations);
  active.add(id);
  try {
    for (let simulation = 1; simulation <= request.simulations; simulation++) {
      if (cancelled.has(id)) break;
      let generated: ReturnType<typeof generateReformSimulation> | undefined;
      let output: ReturnType<typeof runPoliticsSimulation> | undefined;
      try {
        generated = generateReformSimulation(prepared, simulation);
        progress('simulate', simulation);
        output = runPoliticsSimulation({ algorithmId: REFORM_ALGORITHM_ID, simulationId: simulation, input: generated.input });
      } catch (error) { presentation.failure(simulation, error); }
      if (output && generated) {
        if (output.algorithmId !== REFORM_ALGORITHM_ID) throw new Error('Unexpected reform result');
        presentation.add(generated.input, output.result, generated.trace);
      }
      const batch = presentation.drain();
      await appendResultBatch(record, batch.tables, batch.warnings, simulation);
      record.result.warnings.push(...batch.warnings.filter(w => w.severity === 'info').slice(0, Math.max(0, 20 - record.result.warnings.length)));
      record.result.storage!.completedSimulations = simulation;
      record.result.benchmark.elapsedMs = performance.now() - started;
      record.result.tables = record.result.storage!.tables.map(t => ({ name: t.name, columns: t.columns, rows: [] }));
      // Publishing this manifest is the commit marker for the complete run.
      // Readers ignore chunks beyond its counters after any interrupted write.
      await saveStoredResult(record);
      if (simulation % 10 === 0 || simulation === request.simulations) progress('scrutinize', simulation);
    }
    if (cancelled.has(id)) {
      record.result.storage!.state = 'interrupted';
      record.result.warnings.push({ code: 'RESULT_CANCELLED', electionKind: 'politiche', severity: 'warning', message: `Calcolo annullato; conservate ${record.result.storage!.completedSimulations} simulazioni su ${request.simulations} richieste.` });
    } else {
      progress('summarize', request.simulations);
      const final = presentation.finish();
      await appendResultBatch(record, final.tables, final.warnings, 0);
      record.result.tables = final.tables.map(t => ({ ...t, rows: t.name === 'Reform overview' || t.name === 'Conditional profile sensitivity' ? t.rows : [] }));
      record.result.storage!.state = 'complete'; record.result.status = 'completed';
    }
    record.result.benchmark.elapsedMs = performance.now() - started;
    await saveStoredResult(record);
    return record.result;
  } catch (error) {
    // Storage/transport failures never become legal non-computability or cause
    // a reroll. Recover only the last durably published manifest.
    const saved = await getStoredResult(id, owner);
    saved.result.storage!.state = 'interrupted'; saved.result.status = 'not_implemented';
    saved.result.warnings.push({ code: 'RESULT_STORAGE_ERROR', electionKind: 'politiche', severity: 'error', message: `${describe(error)}. Conservate ${saved.result.storage!.completedSimulations} simulazioni; il denominatore richiesto resta ${request.simulations}.` });
    await saveStoredResult(saved).catch(() => undefined);
    return saved.result;
  } finally { active.delete(id); cancelled.delete(id); }
}

async function importStoredFile(file: File, id: string, owner: string): Promise<StoredResult> {
  const record: StoredResult = { id, owner, exports: [], scenario: {} as ExportScenario, result: {
    type: 'result', status: 'not_implemented', tables: [], warnings: [], storage: newResultStorage(id),
    benchmark: { startedAt: '', elapsedMs: 0, simulations: 0, dataVersion: '' }
  } };
  await saveStoredResult(record);
  let writer: ResultChunkWriter | undefined, currentIndex = -2;
  const warningWriter = new ResultChunkWriter(record, -1);
  try {
    const parsed = await importResultStream(jsonFileStream(file), {
      async table(index, table) {
        const storage = record.result.storage!;
        storage.tables[index] ??= { ...table, rows: 0, chunks: 0 };
        Object.assign(storage.tables[index], table);
      },
      async row(index, cells) {
        // JSON object property order is arbitrary: rows can precede their
        // descriptor. Keep counters now; a later table callback fills labels.
        record.result.storage!.tables[index] ??= { name: '', columns: [], rows: 0, chunks: 0 };
        if (currentIndex !== index) { await writer?.flush(); writer = new ResultChunkWriter(record, index); currentIndex = index; }
        const columns = record.result.storage!.tables[index]?.columns ?? [];
        await writer!.append(cells, Number(cells[columns.indexOf('Simulation')] ?? 0));
      },
      async warning(w) {
        await warningWriter.append(warningCells(w), w.simulationId ?? 0);
        record.result.storage!.warningCounts[w.severity ?? 'warning']++;
        if (w.severity === 'info' && record.result.warnings.length < 20) record.result.warnings.push(w);
      }
    });
    await writer?.flush(); await warningWriter.flush();
    const storage = record.result.storage!, notes = record.result.warnings;
    record.scenario = parsed.scenario;
    record.result = { ...parsed.result, warnings: notes, storage, tables: storage.tables.map(t => ({ name: t.name, columns: t.columns, rows: [] })) };
    storage.state = parsed.result.status === 'completed' ? 'complete' : 'interrupted';
    storage.completedSimulations = storage.tables.find(t => t.name === 'Reform runs')?.rows ?? parsed.result.benchmark.simulations;
    await saveStoredResult(record); return record;
  } catch (error) {
    // Delete only this newly-created staging import, never an existing result.
    await deleteStoredResult(id, owner).catch(() => undefined); throw error;
  }
}

function encodedStream(values: AsyncGenerator<string>): ReadableStream<Uint8Array<ArrayBuffer>> {
  return new ReadableStream<string>({
    async pull(controller) { try { const next = await values.next(); if (next.done) controller.close(); else controller.enqueue(next.value); } catch (e) { controller.error(e); } },
    async cancel() { await values.return(undefined); }
  }).pipeThrough(new TextEncoderStream());
}
async function exportStoredFile(record: StoredResult, format: 'json' | 'csv'): Promise<File> {
  if (!navigator.storage.getDirectory) throw new Error('Questo browser richiede il supporto ai file temporanei OPFS per esportare risultati grandi. I risultati locali sono conservati.');
  const root = await navigator.storage.getDirectory();
  const directory = await root.getDirectoryHandle('elezioni-result-exports', { create: true });
  const filename = `${record.id}-${crypto.randomUUID()}.${format === 'json' ? 'json.gz' : 'csv'}`;
  const handle = await directory.getFileHandle(filename, { create: true });
  try {
    let stream = encodedStream(streamStoredResult(record, format));
    if (format === 'json') stream = stream.pipeThrough(new CompressionStream('gzip'));
    await stream.pipeTo(await handle.createWritable());
    record.exports.push(filename); await saveStoredResult(record);
    // File is a disk-backed Blob; sending it does not materialize its contents
    // as a giant JS buffer. Retain it until explicit result-session deletion.
    return await handle.getFile();
  } catch (error) { await directory.removeEntry(filename).catch(() => undefined); throw error; }
}
export async function handleResultService(request: ResultServiceRequest): Promise<ResultServiceReply> {
  try {
    let data: unknown;
    if (request.action === 'cancel') { cancelled.add(request.resultId); data = null; }
    else if (request.action === 'import') {
      if (!request.file) throw new Error('Missing result file');
      data = await importStoredFile(request.file, request.resultId, request.owner);
    } else {
      const record = await getStoredResult(request.resultId, request.owner);
      if (request.action === 'manifest') {
        if (record.result.storage?.state === 'writing' && !active.has(record.id)) {
          record.result.storage.state = 'interrupted'; record.result.status = 'not_implemented';
          record.result.warnings.push({ code: 'RESULT_INTERRUPTED', severity: 'warning', electionKind: 'politiche', message: 'Calcolo interrotto: sono disponibili solo le simulazioni conservate. Nessuna simulazione viene riestratta.' });
          await saveStoredResult(record).catch(() => undefined);
        }
        data = record;
      }
      else if (request.action === 'page') data = await storedTablePage(record, request.table ?? 0, request.offset ?? 0, request.limit ?? 50, request.simulation);
      else if (request.action === 'warnings') data = await storedWarningPage(record, request.offset ?? 0);
      else if (request.action === 'export') {
        if (active.has(record.id)) throw new Error('Wait for the calculation or cancel it before export');
        data = await exportStoredFile(record, request.format ?? 'json');
      } else if (request.action === 'delete') {
        if (active.has(record.id)) throw new Error('Cancel the calculation before deleting its result');
        await deleteStoredResult(record.id, request.owner); data = null;
      } else throw new Error('Unknown result operation');
    }
    return { type: 'result-service', requestId: request.requestId, ok: true, data };
  } catch (error) { return { type: 'result-service', requestId: request.requestId, ok: false, error: { code: 'RESULT_SERVICE_ERROR', message: describe(error) } }; }
}
