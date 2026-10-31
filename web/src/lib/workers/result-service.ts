import { storedPositionProbabilities, storedBonusProbabilities } from '../politics/reform-2026/stored-probabilities';
import { loadVerifiedJson, requireSimulationCount } from '../core/runtime-data';
import { appendResultBatch, cellsWarning, deleteStoredResult, getStoredResult, newResultStorage, registerStoredExport, saveStoredResult, storedRows, storedTablePage, storedWarningPage, ResultChunkWriter, warningCells, type Cell, type StoredResult } from '../core/result-store';
import { createDiagnosticFrequency } from '../core/result-diagnostics';
import { importResultStream, streamStoredResult, streamDiagnosticIdsCsv, type ExportScenario } from '../core/result-export';
import { storedMessageSummary, storedMessageContext, storedMessageIdPage, storedMessageIds } from '../core/stored-diagnostics';
import type { DiagnosticFilter } from '../core/diagnostic-messages';
import { diagnosticEvent, diagnosticScope, diagnosticTableName } from '../core/diagnostic-messages';
import { jsonFileStream } from '../core/json-stream';
import type { SimulationProgress, SimulationRequest, SimulationResult } from '../core/types';
import catalog from '../politics/reform-2026/data/generation-catalog.generated.json';
import { requireReformScenario, validateReformSnapshot } from '../politics/reform-2026/generation-scenario';
import type { ReformGenerationSnapshot } from '../politics/reform-2026/generation-types';
import { createReformPresentation } from '../politics/reform-2026/result-presentation';
import { resolvePoliticsSimulationAlgorithm } from '../politics/simulation-scrutiny';
import { REFORM_ALGORITHM_ID } from '../politics/reform-2026/input';
import { estimateReformWorkerCapacity, requireReformWorkerCount } from '../core/reform-worker-capacity';
import { ReformComputeClient, runOrderedReformPool } from './reform-pool';
import { ensureStatistics, requireSeries, seriesStatistics, statisticalDistribution, storedSeatEndowments, storedSummaryTable } from '../politics/reform-2026/stored-statistics';
import { summaryTableName, type BonusFilter, type SeatScope, type CollegeMeasure } from '../results/statistics';
import { requireFastScenario } from '../politics/reform-2026/vote-share/scenario';
import { generationCapacity } from '../politics/reform-2026/vote-share/fit-resources';
import { loadCandidate } from '../politics/reform-2026/vote-share/release';
import { checkpointReader, requirePortableFitFile, sameCheckpointScenario } from '../politics/reform-2026/vote-share/checkpoint';
import { samplePage } from '../core/statistics-cache';

export interface ResultServiceRequest {
  type: 'result-service'; requestId: number;
  action: 'page' | 'warnings' | 'diagnostics' | 'messages-summary' | 'messages-general' | 'messages-context' | 'messages-ids' | 'messages-export' | 'manifest' | 'fit-session' | 'delete' | 'cancel' | 'import' | 'export' | 'statistics' | 'statistics-series' | 'statistics-points' | 'statistics-box' | 'statistics-positions' | 'statistics-bonus' | 'statistics-endowments';
  resultId: string; owner: string;
  table?: number; offset?: number; limit?: number; simulation?: number;
  file?: File; format?: 'json' | 'csv';
  list?: string; chamber?: string;
  series?: string; seatScope?: SeatScope; measure?: CollegeMeasure;
  bonusFilter?: BonusFilter;
  filter?: DiagnosticFilter;
}
export interface ResultServiceReply {
  type: 'result-service'; requestId: number; ok: boolean; data?: unknown;
  error?: { code: string; message: string };
}
const cancelled = new Set<string>();
const active = new Set<string>();
const deleting = new Set<string>();
const resultJobs = new Map<string, Set<Promise<unknown>>>();
const describe = (error: unknown) => error instanceof Error ? `${error.name}: ${error.message}` : String(error);

export async function runStoredReform(request: SimulationRequest, emit: (progress: SimulationProgress) => void): Promise<SimulationResult> {
  requireSimulationCount(request.simulations);
  resolvePoliticsSimulationAlgorithm(REFORM_ALGORITHM_ID, 'production');
  if (!('kind' in request.scenario)) throw new Error('A reform request requires an explicit reform scenario');
  const fast=request.fast;
  if(fast && !fast.packets.length) throw new Error('Missing generation packet');
  const fastCapacity=fast?generationCapacity(fast.packets[0],navigator as Navigator&{deviceMemory?:number}):null;
  const capacity = fastCapacity?{...estimateReformWorkerCapacity(navigator),maximum:fastCapacity.maximum,
    workerMemoryMiB:Math.ceil(fastCapacity.perWorkerBytes/(1024*1024)),estimateVersion:fastCapacity.estimateVersion}:estimateReformWorkerCapacity(navigator as Navigator & { deviceMemory?: number });
  const requestedWorkers = requireReformWorkerCount(request.computeWorkers ?? capacity.maximum, capacity.maximum);
  const computeWorkers = Math.min(requestedWorkers, request.simulations);
  const started = performance.now(), startedAt = new Date().toISOString();
  const progress = (phase: SimulationProgress['phase'], completed: number) => emit({ type: 'progress', phase, completed, total: request.simulations, elapsedMs: performance.now() - started });
  progress('prepare', 0);
  if(fast&&fast.packets.length!==computeWorkers)throw new Error('Frozen worker packets do not match requested workers');
  const snapshot = fast?fast.packets[0].context:await loadVerifiedJson(request.basePath ?? '', catalog) as ReformGenerationSnapshot;
  const errors = fast?[]:validateReformSnapshot(snapshot);
  if (errors.length) throw new RangeError(`Invalid reform snapshot: ${errors.join('; ')}`);
  const scenario = fast?requireFastScenario(request.scenario):requireReformScenario(request.scenario, snapshot as ReformGenerationSnapshot);
  if(fast){requirePortableFitFile(fast.file);const restored=await loadCandidate(fast.file.pointer,checkpointReader(fast.file),{allowEditedEvidence:true,expectedGenerationImplementationId:fast.packets[0].scenario.models.generationImplementationId});
    if(!sameCheckpointScenario(restored.scenario,scenario as typeof restored.scenario)||fast.packets.some(p=>!sameCheckpointScenario(p.scenario,restored.scenario)))throw new Error('Frozen scenario/checkpoint mismatch');}
    if (scenario.seed !== request.seed || scenario.electionDate !== request.electionDate) throw new Error('Request seed/date must match the reform scenario');
    if (scenario.schemaVersion===4 && request.dataVersion!==scenario.data.sourceBundleId) throw new Error('Request source must match the reform scenario');
  const id = request.resultId ?? crypto.randomUUID(), owner = request.resultOwner ?? 'direct-worker';
  const execution = { requestedWorkers, computeWorkers, maximumWorkers: capacity.maximum,
    estimatedWorkerMemoryMiB: capacity.workerMemoryMiB, estimateVersion: capacity.estimateVersion,
    generationMs: 0, scrutinyMs: 0, presentationMs: 0, storageMs: 0 };
  const record: StoredResult = { id, owner, scenario, fitFile:fast?.file,exports: [], result: {
    type: 'result', status: 'not_implemented', completedSimulations: 0, tables: [], warnings: [], storage: newResultStorage(id),
    benchmark: { startedAt, elapsedMs: 0, simulations: request.simulations, dataVersion:scenario.schemaVersion===4?scenario.data.sourceBundleId:snapshot.version, scrutinyAlgorithmId: REFORM_ALGORITHM_ID, seed: request.seed, execution }
  } };
  await saveStoredResult(record);
  // Simulation 1 supplies shared context once, independent of pool size. Only
  // small aggregate counters live here; run details stay in compute workers.
  const presentation = createReformPresentation(scenario, snapshot, request.simulations, { includeContext: false });
  const workers: ReformComputeClient[] = [];
  let failureCode = 'RESULT_EXECUTION_ERROR';
  active.add(id);
  try {
    for (let index = 0; index < computeWorkers; index++) {
      workers.push(new ReformComputeClient(new Worker(new URL('./reform-compute.worker.ts', import.meta.url), { type: 'module', name: `reform-compute-${index + 1}` })));
    }
    await Promise.all(workers.map((worker,index) => fast?worker.initializeFast(fast.packets[index]):worker.initialize(scenario, request.basePath ?? '')));
    await runOrderedReformPool(workers, request.simulations, () => cancelled.has(id), async (simulation, worker, ready) => {
      execution.generationMs += ready.timings.generationMs;
      execution.scrutinyMs += ready.timings.scrutinyMs;
      execution.presentationMs += ready.timings.presentationMs;
      for (;;) {
        const chunk = await worker.pull();
        if (!chunk) break;
        const storageStarted = performance.now();
        const cells = JSON.parse(new TextDecoder().decode(chunk.buffer)) as Cell[][];
        const tables = ready.tables.map(table => ({ ...table, rows: [] as SimulationResult['tables'][number]['rows'] }));
        const warnings = chunk.table === -1 ? cells.map(cellsWarning) : [];
        if (chunk.table !== -1) {
          const table = tables[chunk.table];
          if (!table) throw new Error('Unknown compute result table');
          table.rows = cells.map(row => Object.fromEntries(table.columns.map((column, index) => [column, row[index]])));
        }
        try { await appendResultBatch(record, tables, warnings, simulation); }
        catch (error) { failureCode = 'RESULT_STORAGE_ERROR'; throw error; }
        record.result.warnings.push(...warnings.filter(w => w.severity === 'info').slice(0, Math.max(0, 20 - record.result.warnings.length)));
        execution.storageMs += performance.now() - storageStarted;
      }
      record.result.storage!.completedSimulations = simulation;
      record.result.completedSimulations = simulation;
      record.result.benchmark.elapsedMs = performance.now() - started;
      record.result.tables = record.result.storage!.tables.map(t => ({ name: t.name, columns: t.columns, rows: [] }));
      // Publishing this manifest is the commit marker for the complete run.
      // Readers ignore chunks beyond its counters after any interrupted write.
      const storageStarted = performance.now();
      try { await saveStoredResult(record); }
      catch (error) { failureCode = 'RESULT_STORAGE_ERROR'; throw error; }
      execution.storageMs += performance.now() - storageStarted;
      presentation.accumulate(ready.counts);
      progress('simulate', simulation);
      if (simulation % 10 === 0 || simulation === request.simulations) progress('scrutinize', simulation);
    });
    if (cancelled.has(id)) {
      record.result.storage!.state = 'interrupted';
      record.result.warnings.push({ code: 'RESULT_CANCELLED', electionKind: 'politiche', severity: 'warning', message: `Calcolo annullato; conservate ${record.result.storage!.completedSimulations} simulazioni su ${request.simulations} richieste.` });
    } else {
      progress('summarize', request.simulations);
      const final = presentation.finish();
      // The same table contract is used by charts and CSV. Canonical rows are
      // already committed; rebuildable point/value indexes are never exported.
      final.tables[final.tables.findIndex(t => t.name === summaryTableName)] = await storedSummaryTable(record);
      try { await appendResultBatch(record, final.tables, final.warnings, 0); }
      catch (error) { failureCode = 'RESULT_STORAGE_ERROR'; throw error; }
      record.result.tables = final.tables.map(t => ({ ...t, rows: t.name === 'Reform overview' || t.name === 'Conditional profile sensitivity' ? t.rows : [] }));
      record.result.storage!.state = 'complete'; record.result.status = 'completed';
    }
    record.result.benchmark.elapsedMs = performance.now() - started;
    try { await saveStoredResult(record); }
    catch (error) { failureCode = 'RESULT_STORAGE_ERROR'; throw error; }
    return record.result;
  } catch (error) {
    // Storage/transport failures never become legal non-computability or cause
    // a reroll. Recover only the last durably published manifest.
    const saved = await getStoredResult(id, owner);
    saved.result.storage!.state = 'interrupted'; saved.result.status = 'not_implemented';
    saved.result.warnings.push({ code: failureCode, electionKind: 'politiche', severity: 'error', message: `${describe(error)}. Conservate ${saved.result.storage!.completedSimulations} simulazioni; il denominatore richiesto resta ${request.simulations}.` });
    await saveStoredResult(saved).catch(() => undefined);
    return saved.result;
  } finally { for (const worker of workers) worker.close(); active.delete(id); cancelled.delete(id); }
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
    record.fitFile=parsed.fitFile;
    record.result = { ...parsed.result, warnings: notes, storage, tables: storage.tables.map(t => ({ name: t.name, columns: t.columns, rows: [] })) };
    storage.state = parsed.result.status === 'completed' ? 'complete' : 'interrupted';
    const runs = storage.tables.find(t => t.name === 'Reform runs')?.rows;
    if (parsed.result.completedSimulations !== undefined && runs !== undefined && parsed.result.completedSimulations !== runs) throw new Error('Saved simulation coverage disagrees with run ledger');
    storage.completedSimulations = parsed.result.completedSimulations ?? runs ?? parsed.result.benchmark.simulations;
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
async function exportStoredFile(record: StoredResult, format: 'json' | 'csv', values = streamStoredResult(record, format)): Promise<File> {
  if (!navigator.storage?.getDirectory) throw new Error('Questo browser richiede il supporto ai file temporanei OPFS per esportare risultati grandi. I risultati locali sono conservati.');
  const root = await navigator.storage.getDirectory();
  const directory = await root.getDirectoryHandle('elezioni-result-exports', { create: true });
  const filename = `${record.id}-${crypto.randomUUID()}.${format === 'json' ? 'json.gz' : 'csv'}`;
  const handle = await directory.getFileHandle(filename, { create: true });
  try {
    let stream = encodedStream(values);
    if (format === 'json') stream = stream.pipeThrough(new CompressionStream('gzip'));
    await stream.pipeTo(await handle.createWritable());
    await registerStoredExport(record.id, record.owner, filename);
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
        data = record.fitFile?{...record,fitFile:undefined}:record;
      }
      else if(request.action==='fit-session'){if(!record.fitFile)throw new Error('No fitted state is stored for this result');data=record.fitFile;}
      else if (request.action === 'diagnostics') {
        if (active.has(record.id)) throw new Error('Wait for calculation completion before querying diagnostic frequencies');
        const events = record.result.storage!.tables.findIndex(t => t.name === diagnosticTableName);
        const index = events >= 0 ? events : record.result.storage!.tables.findIndex(t => t.name === 'Warnings and errors');
        if (index < 0) throw new Error('Diagnostic ledger is unavailable');
        const descriptor = record.result.storage!.tables[index];
        // Older/imported archives can contain a whole-run aggregation with only
        // its first simulation ID. Do not invent distinct-run frequencies.
        if (events < 0 && !descriptor.runIndexed) throw new Error('Per-simulation diagnostic ledger is unavailable for this imported result');
        const frequencies = createDiagnosticFrequency(record.result.benchmark.simulations, record.result.storage!.completedSimulations);
        for await (const cells of storedRows(record.id, index)) {
          if (events < 0) frequencies.add(Object.fromEntries(descriptor.columns.map((c, i) => [c, cells[i]])));
          else {
            const w = diagnosticEvent(cells, descriptor.columns);
            frequencies.add({ Code: w.code, Severity: w.severity ?? 'warning', 'First simulation': diagnosticScope(w) === 'simulation' ? w.simulationId ?? null : null, Message: w.message, Occurrences: w.occurrences ?? 1 });
          }
        }
        data = frequencies.finish();
      }
      else if (request.action.startsWith('messages-')) {
        if (active.has(record.id)) throw new Error('Wait for calculation completion before querying messages');
        if (deleting.has(record.id)) throw new Error('Result session is being deleted');
        const job = (async () => {
          if (request.action === 'messages-summary') return storedMessageSummary(record, request.filter, request.offset);
          if (request.action === 'messages-general') return storedMessageContext(record, {}, request.offset, true);
          if (request.action === 'messages-context') return storedMessageContext(record, request.filter, request.offset);
          if (request.action === 'messages-ids') return storedMessageIdPage(record, request.filter, request.offset);
          return exportStoredFile(record, 'csv', streamDiagnosticIdsCsv(storedMessageIds(record, request.filter)));
        })();
        const jobs = resultJobs.get(record.id) ?? new Set<Promise<unknown>>(); resultJobs.set(record.id, jobs); jobs.add(job);
        try { data = await job; } finally { jobs.delete(job); if (!jobs.size) resultJobs.delete(record.id); }
      }
      else if (request.action.startsWith('statistics')) {
        if (active.has(record.id)) throw new Error('Wait for calculation completion before querying statistics');
        if (deleting.has(record.id)) throw new Error('Result session is being deleted');
        const job = (async () => {
          if (request.action === 'statistics-endowments') return storedSeatEndowments(record);
          if (request.action === 'statistics-positions') return await storedPositionProbabilities(record, request.chamber ?? '', request.list ?? '');
          if (request.action === 'statistics-bonus') return await storedBonusProbabilities(record);
          if (request.action === 'statistics') return (await ensureStatistics(record)).overview;
          if (!request.series) throw new Error('Missing statistical series');
          const summary = await requireSeries(record, request.series);
          if (request.action === 'statistics-points') return samplePage(record.id, request.series, request.offset ?? 0, request.limit ?? 256);
          if (request.action === 'statistics-box') return statisticalDistribution(record, summary);
          return seriesStatistics(record, request.series, request.measure, request.bonusFilter, request.seatScope);
        })();
        const jobs = resultJobs.get(record.id) ?? new Set<Promise<unknown>>(); resultJobs.set(record.id, jobs); jobs.add(job);
        try { data = await job; } finally { jobs.delete(job); if (!jobs.size) resultJobs.delete(record.id); }
      }
      else if (request.action === 'page') data = await storedTablePage(record, request.table ?? 0, request.offset ?? 0, request.limit ?? 50, request.simulation);
      else if (request.action === 'warnings') data = await storedWarningPage(record, request.offset ?? 0);
      else if (request.action === 'export') {
        if (active.has(record.id)) throw new Error('Wait for the calculation or cancel it before export');
        if (deleting.has(record.id)) throw new Error('Result session is being deleted');
        const job = exportStoredFile(record, request.format ?? 'json');
        const jobs = resultJobs.get(record.id) ?? new Set<Promise<unknown>>(); resultJobs.set(record.id, jobs); jobs.add(job);
        try { data = await job; } finally { jobs.delete(job); if (!jobs.size) resultJobs.delete(record.id); }
      } else if (request.action === 'delete') {
        if (active.has(record.id)) throw new Error('Cancel the calculation before deleting its result');
        deleting.add(record.id);
        try { await Promise.allSettled([...(resultJobs.get(record.id) ?? [])]); await deleteStoredResult(record.id, request.owner); data = null; }
        finally { deleting.delete(record.id); }
      } else throw new Error('Unknown result operation');
    }
    return { type: 'result-service', requestId: request.requestId, ok: true, data };
  } catch (error) { return { type: 'result-service', requestId: request.requestId, ok: false, error: { code: 'RESULT_SERVICE_ERROR', message: describe(error) } }; }
}
