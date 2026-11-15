import { storedPositionProbabilities, storedBonusProbabilities } from '../politics/reform-2026/stored-probabilities';
import { deleteStoredResult, getStoredResult, withStoredFittedState, registerStoredExport, saveStoredResult, storedRows, storedTablePage, storedWarningPage, type StoredResult } from '../core/result-store';
import { createDiagnosticFrequency } from '../core/result-diagnostics';
import { streamStoredResult, streamDiagnosticIdsCsv } from '../core/result-export';
import { storedMessageSummary, storedMessageContext, storedMessageIdPage, storedMessageIds } from '../core/stored-diagnostics';
import type { DiagnosticFilter } from '../core/diagnostic-messages';
import { diagnosticEvent, diagnosticScope, diagnosticTableName } from '../core/diagnostic-messages';
import type { SimulationProgress, SimulationRequest, SimulationResult } from '../core/types';
import type { ReformGenerationSnapshot } from '../politics/reform-2026/generation-types';
import { ensureStatistics, requireSeries, seriesStatistics, statisticalDistribution, storedSeatEndowments } from '../politics/reform-2026/stored-statistics';
import { type BonusFilter, type SeatScope, type CollegeMeasure } from '../results/statistics';
import { samplePage } from '../core/statistics-cache';
import { importStoredPolitics } from '../politics/files/stored-archive';

export interface ResultServiceRequest {
  type: 'result-service'; requestId: number;
  action: 'page' | 'warnings' | 'diagnostics' | 'messages-summary' | 'messages-general' | 'messages-context' | 'messages-ids' | 'messages-export' | 'manifest' | 'fit-session' | 'delete' | 'cancel' | 'import' | 'export' | 'statistics' | 'statistics-series' | 'statistics-points' | 'statistics-box' | 'statistics-positions' | 'statistics-bonus' | 'statistics-endowments';
  resultId: string; owner: string;
  table?: number; offset?: number; limit?: number; simulation?: number;
  file?: File; format?: 'json' | 'csv'; commonPolitics?: boolean;
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
/** Common coordinator shares the same query/export/deletion locks. */
export function setResultActive(id: string, running: boolean) { if (running) active.add(id); else active.delete(id); }
const deleting = new Set<string>();
const resultJobs = new Map<string, Set<Promise<unknown>>>();
const describe = (error: unknown) => error instanceof Error ? `${error.name}: ${error.message}` : String(error);

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
      data = await importStoredPolitics(request.file, request.resultId, request.owner);
    } else {
      const record = await getStoredResult(request.resultId, request.owner);
      if (request.action === 'manifest') {
        if (record.result.storage?.state === 'writing' && !active.has(record.id)) {
          record.result.storage.state = 'interrupted'; record.result.status = 'not_implemented';
          record.result.warnings.push({ code: 'RESULT_INTERRUPTED', severity: 'warning', electionKind: 'politiche', message: 'Calcolo interrotto: sono disponibili solo le simulazioni conservate. Nessuna simulazione viene riestratta.' });
          if (record.politics) { const run = record.politics.run; record.politics.run = { ...run,
            session: { ...run.session, state: 'interrupted' }, result: { ...run.result, status: 'not_implemented' } }; }
          await saveStoredResult(record).catch(() => undefined);
        }
        data = { ...record, fitFile: undefined, ...(record.politics ? { politics: { ...record.politics, checkpoint: null } } : {}) };
      }
      else if(request.action==='fit-session'){const fitted=await withStoredFittedState(record);if(!fitted.fitFile)throw new Error('No fitted state is stored for this result');data=fitted.fitFile;}
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
        const format = request.format ?? 'json';
        const job = exportStoredFile(record, format);
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
