/** Test-only pre-common transport and archived-table regressions. Never imported by application code. */
import { storedPositionProbabilities, storedBonusProbabilities } from '../../src/lib/politics/reform-2026/stored-probabilities';
import { loadVerifiedJson, requireSimulationCount } from '../../src/lib/core/runtime-data';
import { appendResultBatch, cellsWarning, deleteStoredResult, getStoredResult, withStoredFittedState, newResultStorage, registerStoredExport, saveStoredResult, storedRows, storedTablePage, storedWarningPage, ResultChunkWriter, warningCells, type Cell, type StoredResult } from '../../src/lib/core/result-store';
import { createDiagnosticFrequency } from '../../src/lib/core/result-diagnostics';
import { importResultStream, streamStoredResult, streamDiagnosticIdsCsv, type ExportScenario } from '../../src/lib/core/result-export';
import { storedMessageSummary, storedMessageContext, storedMessageIdPage, storedMessageIds } from '../../src/lib/core/stored-diagnostics';
import type { DiagnosticFilter } from '../../src/lib/core/diagnostic-messages';
import { diagnosticEvent, diagnosticScope, diagnosticTableName } from '../../src/lib/core/diagnostic-messages';
import { jsonFileStream } from '../../src/lib/core/json-stream';
import type { SimulationProgress, SimulationRequest, SimulationResult } from '../../src/lib/core/types';
import catalog from '../../src/lib/politics/reform-2026/data/generation-catalog.generated.json';
import type { ReformGenerationSnapshot } from '../../src/lib/politics/reform-2026/generation-types';
import { createReformPresentation } from '../../src/lib/politics/reform-2026/result-presentation';
import { resolvePoliticsSimulationAlgorithm } from '../../src/lib/politics/simulation-scrutiny';
import { REFORM_ALGORITHM_ID } from '../../src/lib/politics/reform-2026/input';
import { estimateReformWorkerCapacity, requireReformWorkerCount } from '../../src/lib/core/reform-worker-capacity';
import { ReformComputeClient, runOrderedReformPool } from '../../src/lib/workers/reform-pool';
import { ensureStatistics, requireSeries, seriesStatistics, statisticalDistribution, storedSeatEndowments, storedSummaryTable } from '../../src/lib/politics/reform-2026/stored-statistics';
import { summaryTableName, type BonusFilter, type SeatScope, type CollegeMeasure } from '../../src/lib/results/statistics';
import { requireFastScenario } from '../../src/lib/politics/reform-2026/vote-share/scenario';
import { generationCapacity } from '../../src/lib/politics/reform-2026/vote-share/fit-resources';
import { loadCandidate } from '../../src/lib/politics/reform-2026/vote-share/release';
import { checkpointReader, requirePortableFitFile, sameCheckpointScenario } from '../../src/lib/politics/reform-2026/vote-share/checkpoint';
import { samplePage } from '../../src/lib/core/statistics-cache';
import { importStoredPolitics } from '../../src/lib/politics/files/stored-archive';

import { handleResultService as currentService, setResultActive, type ResultServiceRequest, type ResultServiceReply } from '../../src/lib/workers/result-service';
const cancelled=new Set<string>();
const describe=(error:unknown)=>error instanceof Error?`${error.name}: ${error.message}`:String(error);
export async function runStoredReform(request: SimulationRequest, emit: (progress: SimulationProgress) => void): Promise<SimulationResult> {
  requireSimulationCount(request.simulations);
  resolvePoliticsSimulationAlgorithm(REFORM_ALGORITHM_ID, 'production');
  if (!('kind' in request.scenario)) throw new Error('A reform request requires an explicit reform scenario');
  const fast=request.fast;
  if(!fast) throw new Error('Only latest fitted reform sessions can execute in this application');
  if(fast && !fast.packets.length) throw new Error('Missing generation packet');
  const fastCapacity=fast?generationCapacity(fast.packets[0],navigator as Navigator&{deviceMemory?:number}):null;
  const capacity = fastCapacity?{...estimateReformWorkerCapacity(navigator),maximum:fastCapacity.maximum,
    workerMemoryMiB:Math.ceil(fastCapacity.perWorkerBytes/(1024*1024)),estimateVersion:fastCapacity.estimateVersion}:estimateReformWorkerCapacity(navigator as Navigator & { deviceMemory?: number });
  const requestedWorkers = requireReformWorkerCount(request.computeWorkers ?? fastCapacity?.recommended ?? capacity.recommended, capacity.maximum);
  const computeWorkers = Math.min(requestedWorkers, request.simulations);
  const started = performance.now(), startedAt = new Date().toISOString();
  const progress = (phase: SimulationProgress['phase'], completed: number) => emit({ type: 'progress', phase, completed, total: request.simulations, elapsedMs: performance.now() - started });
  progress('prepare', 0);
  if(fast&&fast.packets.length!==1&&fast.packets.length!==computeWorkers)throw new Error('Frozen worker packets do not match requested workers');
  const snapshot = fast.packets[0].context;
  const scenario = requireFastScenario(request.scenario);
  if(fast){requirePortableFitFile(fast.file);const restored=await loadCandidate(fast.file.pointer,checkpointReader(fast.file),{allowEditedEvidence:true,expectedGenerationImplementationId:fast.packets[0].scenario.models.generationImplementationId});
    if(!sameCheckpointScenario(restored.scenario,scenario as typeof restored.scenario)||fast.packets.some(p=>!sameCheckpointScenario(p.scenario,restored.scenario)))throw new Error('Frozen scenario/checkpoint mismatch');}
    if (scenario.seed !== request.seed || scenario.electionDate !== request.electionDate) throw new Error('Request seed/date must match the reform scenario');
    if ((scenario.schemaVersion === 4 || scenario.schemaVersion === 5) && request.dataVersion!==scenario.data.sourceBundleId) throw new Error('Request source must match the reform scenario');
  const id = request.resultId ?? crypto.randomUUID(), owner = request.resultOwner ?? 'direct-worker';
  const execution = { requestedWorkers, computeWorkers, maximumWorkers: capacity.maximum,
    estimatedWorkerMemoryMiB: capacity.workerMemoryMiB, estimateVersion: capacity.estimateVersion,
    generationMs: 0, scrutinyMs: 0, presentationMs: 0, storageMs: 0 };
  const record: StoredResult = { id, owner, scenario, fitFile:fast?.file,exports: [], result: {
    type: 'result', status: 'not_implemented', completedSimulations: 0, tables: [], warnings: [], storage: newResultStorage(id),
    benchmark: { startedAt, elapsedMs: 0, simulations: request.simulations, dataVersion:(scenario.schemaVersion === 4 || scenario.schemaVersion === 5)?scenario.data.sourceBundleId:snapshot.version, scrutinyAlgorithmId: REFORM_ALGORITHM_ID, seed: request.seed, execution }
  } };
  await saveStoredResult(record);
  // Simulation 1 supplies shared context once, independent of pool size. Only
  // small aggregate counters live here; run details stay in compute workers.
  const presentation = createReformPresentation(scenario, snapshot, request.simulations, { includeContext: false });
  const workers: ReformComputeClient[] = [];
  let failureCode = 'RESULT_EXECUTION_ERROR';
  setResultActive(id, true);
  try {
    for (let index = 0; index < computeWorkers; index++) {
      workers.push(new ReformComputeClient(new Worker(new URL('../../src/lib/workers/reform-compute.worker.ts', import.meta.url), { type: 'module', name: `reform-compute-${index + 1}` })));
    }
    if (fast) {
      // Keep each initialization message bounded independently of CPU count.
      // Retain the coordinator's packet/context; transfer only owned copies.
      for (let index = 0; index < workers.length; index++)
        await workers[index].initializeFast(fast.packets.length === 1
          ? structuredClone(fast.packets[0]) : fast.packets[index]);
    } else await Promise.all(workers.map(worker => worker.initialize(scenario, request.basePath ?? '')));
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
  } finally { for (const worker of workers) worker.close(); setResultActive(id, false); cancelled.delete(id); }
}

export async function importStoredFile(file: File, id: string, owner: string): Promise<StoredResult> {
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

export async function handleRetiredResultService(request:ResultServiceRequest):Promise<ResultServiceReply>{
 if(request.action==='cancel')cancelled.add(request.resultId);
 if(request.action==='import'&&!request.commonPolitics){
  try{return {type:'result-service',requestId:request.requestId,ok:true,data:await importStoredFile(request.file!,request.resultId,request.owner)}}
  catch(error){return {type:'result-service',requestId:request.requestId,ok:false,error:{code:'RESULT_SERVICE_ERROR',message:describe(error)}}}
 }
 return currentService(request);
}
