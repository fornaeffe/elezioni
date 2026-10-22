import { appendResultBatch, deleteStoredResult, getStoredResult, newResultStorage, registerStoredExport, saveStoredResult, storedTablePage, type StoredResult } from '../../src/lib/core/result-store';
import { importResultStream, resultTablesToCsv, streamStoredResult } from '../../src/lib/core/result-export';
import { createDefaultPoliticsScenario } from '../../src/lib/scenario/politics';
import type { ResultTable } from '../../src/lib/core/types';
import { handleResultService } from '../../src/lib/workers/result-service';

self.onmessage = async () => {
  try {
    const record: StoredResult = { id: crypto.randomUUID(), owner: 'test-owner', scenario: createDefaultPoliticsScenario(), exports: [], result: {
      type: 'result', status: 'completed', tables: [], warnings: [], storage: newResultStorage(crypto.randomUUID()),
      benchmark: { startedAt: '2026-10-20T00:00:00Z', elapsedMs: 1, simulations: 2, dataVersion: 'v1', scrutinyAlgorithmId: 'politiche-r-parity-v1' }
    } };
    record.result.storage!.id = record.id;
    await saveStoredResult(record);
    const tables: ResultTable[] = [{ name: 'Details', columns: ['Simulation', 'Name', 'Value'], rows: Array.from({ length: 600 }, (_, i) => ({ Simulation: 1, Name: `Città "Europa" ${i} \\ 😀`, Value: i % 2 ? null : i })) },
      { name: 'Models, references and conditions', columns: ['Kind', 'Value', 'Occurrences'], rows: [{ Kind: 'source', Value: 'shared', Occurrences: 1 }] }];
    await appendResultBatch(record, tables, [{ code: 'TEST', severity: 'warning', electionKind: 'politiche', simulationId: 1, message: 'Test "warning"' }], 1);
    record.result.storage!.completedSimulations = 1; await saveStoredResult(record);
    const boundaryPage = await storedTablePage(record, 0, 250, 50);
    await appendResultBatch(record, [{ ...tables[0], rows: [] }, tables[1]], [], 2);
    record.result.storage!.completedSimulations = 2; await saveStoredResult(record);
    await Promise.all([registerStoredExport(record.id, record.owner, 'export-A.csv'), registerStoredExport(record.id, record.owner, 'export-B.json.gz')]);
    const registered = (await getStoredResult(record.id, record.owner)).exports;
    if (registered.length !== 2 || !registered.includes('export-A.csv') || !registered.includes('export-B.json.gz')) throw new Error('Concurrent export registration lost a file');
    const metadata = await storedTablePage(record, 1, 0);
    let json = '', csv = '';
    for await (const text of streamStoredResult(record, 'json')) json += text;
    for await (const text of streamStoredResult(record, 'csv')) csv += text;
    const importedRows: unknown[][] = [], importedNames: string[] = [], importedWarnings: unknown[] = [];
    const bytes = new TextEncoder().encode(json); let at = 0;
    const parsed = await importResultStream(new ReadableStream<Uint8Array<ArrayBuffer>>({ pull(c) { if (at === bytes.length) c.close(); else { c.enqueue(bytes.slice(at, at + 17)); at = Math.min(at + 17, bytes.length); } } }), {
      async table(i, t) { importedNames[i] = t.name; },
      async row(i, row) { (importedRows[i] ??= []).push(row); },
      async warning(w) { importedWarnings.push(w); }
    });
    const csvExpected = resultTablesToCsv([tables[0], { ...tables[1], rows: [{ Kind: 'source', Value: 'shared', Occurrences: 2 }] }]);
    const invalid = JSON.parse(json); invalid.result.tables[0].rows[0].push('extra-cell');
    const invalidId = crypto.randomUUID();
    const invalidImport = await handleResultService({ type: 'result-service', requestId: 1, action: 'import', resultId: invalidId, owner: record.owner,
      file: new File([JSON.stringify(invalid)], 'invalid.json') });
    let invalidImportRemoved = false;
    try { await getStoredResult(invalidId, record.owner); } catch { invalidImportRemoved = true; }
    const reordered = JSON.parse(json);
    reordered.result.tables = reordered.result.tables.map((table: { rows: unknown; columns: unknown; name: string }) => ({ rows: table.rows, columns: table.columns, name: table.name }));
    const reorderedId = crypto.randomUUID();
    const reorderedImport = await handleResultService({ type: 'result-service', requestId: 2, action: 'import', resultId: reorderedId, owner: record.owner,
      file: new File([JSON.stringify(reordered)], 'reordered.json') });
    if (!reorderedImport.ok) throw new Error(reorderedImport.error!.message);
    const reorderedPage = await storedTablePage(await getStoredResult(reorderedId, record.owner), 0, 250, 50);
    await deleteStoredResult(reorderedId, record.owner);
    // Inject a quota failure after one unpublished chunk. Published counters
    // must hide every row from that incomplete batch.
    const published = await getStoredResult(record.id, record.owner), previousCount = published.result.storage!.tables[0].rows;
    const original = IDBObjectStore.prototype.add; let adds = 0, quotaError = '';
    IDBObjectStore.prototype.add = function(value, key) {
      if (this.name === 'chunks' && value.resultId === record.id && ++adds === 2) throw new DOMException('Injected quota exhaustion', 'QuotaExceededError');
      return key === undefined ? original.call(this, value) : original.call(this, value, key);
    };
    try { await appendResultBatch(record, [tables[0]], [], 3); } catch (e) { quotaError = (e as Error).name; }
    finally { IDBObjectStore.prototype.add = original; }
    const retained = await getStoredResult(record.id, record.owner);
    const afterFailure = await storedTablePage(retained, 0, previousCount, 50);
    let foreignDenied = false;
    try { await deleteStoredResult(record.id, 'another-owner'); } catch { foreignDenied = true; }
    const stillPresent = (await getStoredResult(record.id, record.owner)).id;
    await deleteStoredResult(record.id, record.owner);
    self.postMessage({ ok: true, boundaryPage, metadata, importedNames, importedRows, importedWarnings,
      parsedScenario: 'id' in parsed.scenario ? parsed.scenario.id : parsed.scenario.kind, csvMatches: csv === csvExpected, quotaError, retainedRows: retained.result.storage!.tables[0].rows,
      afterFailureRows: afterFailure.rows.length, foreignDenied, stillPresent: stillPresent === record.id, invalidImportRejected: !invalidImport.ok, invalidImportRemoved, reorderedPage });
  } catch (e) { self.postMessage({ ok: false, error: String(e) }); }
};
