import { newResultStorage, saveStoredResult, deleteStoredResult, ResultChunkWriter, warningCells, storedRows, cellsWarning, type StoredResult } from '../../core/result-store';
import { importPoliticsArchiveFile, streamPoliticsArchive, readPoliticsResultFile, parsePoliticsResult } from './results';
import { inspectPoliticsFile } from './metadata';
import type { PoliticsArchiveEnvelope, PoliticsResultEnvelope } from '../scenario/envelopes';
import { diagnosticEvent, diagnosticTableName } from '../../core/diagnostic-messages';

/** Staging records have fresh local IDs and are published to the session index
 * only after EOF and strict validation. No import action touches the draft. */
export async function importStoredPolitics(file: File, id: string, owner: string): Promise<StoredResult> {
  const metadata = await inspectPoliticsFile(file);
  if (metadata.kind === 'politics-result') {
    const run = await readPoliticsResultFile(file);
    if (run.execution.generator.id === 'M02') throw new Error('M02 results require an archive with portable fitting state');
    const archive: PoliticsArchiveEnvelope = { kind: 'politics-result-archive', schemaVersion: 1, exportedAt: new Date().toISOString(), run, portableFit: null };
    return importStoredPolitics(new File([JSON.stringify(archive)], file.name), id, owner);
  }
  if (metadata.kind !== 'politics-result-archive') throw new Error('Expected a new-format result archive');
  const record: StoredResult = { id, owner, exports: [], scenario: {} as StoredResult['scenario'], result: {
    type: 'result', status: 'not_implemented', tables: [], warnings: [], storage: newResultStorage(id),
    benchmark: { startedAt: '', elapsedMs: 0, simulations: 1, dataVersion: '' } } };
  await saveStoredResult(record);
  let writer: ResultChunkWriter | undefined, index = -2;
  const warningWriter = new ResultChunkWriter(record, -1);
  await importPoliticsArchiveFile(file, {
    async row(i, cells) {
      record.result.storage!.tables[i] ??= { name: '', columns: [], rows: 0, chunks: 0, runIndexed: false };
      if (i !== index) { await writer?.flush(); writer = new ResultChunkWriter(record, i); index = i; }
      await writer!.append([...cells]);
    },
    async table(i, t) {
      record.result.storage!.tables[i] ??= { name: '', columns: [], rows: 0, chunks: 0, runIndexed: false };
      Object.assign(record.result.storage!.tables[i], { name: t.name, columns: [...t.columns] });
    },
    async warning(w) { await warningWriter.append(warningCells(w), w.simulationId ?? 0); record.result.storage!.warningCounts[w.severity]++; },
    async commit(header) {
      await writer?.flush(); await warningWriter.flush();
      const storage = record.result.storage!;
      record.scenario = header.run.executedScenario;
      record.politics = { run: header.run, checkpoint: header.portableFit };
      record.result = { ...header.run.result, tables: storage.tables.map(t => ({ name: t.name, columns: t.columns, rows: [] })),
        warnings: [], storage };
      storage.completedSimulations = header.run.result.completedSimulations;
      storage.state = header.run.session.state;
      // Publish only to this unselected staging ID so bounded readers can
      // validate row membership even when rows preceded their JSON descriptor.
      await saveStoredResult(record);
      const ids = new Set(header.run.session.simulations.map(s => s.id));
      for (const [i, table] of storage.tables.entries()) {
        const column = table.columns.findIndex(c => c === 'Simulation' || c === 'Sim');
        if (column < 0) continue;
        for await (const cells of storedRows(record.id, i)) {
          if (table.name === diagnosticTableName) {
            const event = diagnosticEvent(cells, table.columns);
            if (event.simulationId !== undefined && !ids.has(event.simulationId)) throw new Error('INVALID_POLITICS_DIAGNOSTIC_MEMBERSHIP');
          } else if (!Number.isSafeInteger(cells[column]) || !ids.has(Number(cells[column]))) throw new Error('INVALID_POLITICS_TABLE_MEMBERSHIP');
        }
      }
      await saveStoredResult(record);
    },
    async abort() { await deleteStoredResult(id, owner).catch(() => undefined); },
  });
  return record;
}
export function storedPoliticsRun(record: StoredResult): PoliticsResultEnvelope {
  if (!record.politics) throw new Error('Missing common politics session identity');
  const run = structuredClone(record.politics.run);
  return parsePoliticsResult({ ...run, result: { ...run.result,
    tables: record.result.storage!.tables.map(t => ({ name: t.name, columns: t.columns, rows: [] })) } });
}
export function streamStoredPolitics(record: StoredResult) {
  return streamPoliticsArchive({ kind: 'politics-result-archive', schemaVersion: 1, exportedAt: new Date().toISOString(),
    run: storedPoliticsRun(record), portableFit: record.politics!.checkpoint }, {
      rows: i => storedRows(record.id, i),
      async *warnings() {
        for await (const cells of storedRows(record.id, -1)) {
          const w = cellsWarning(cells);
          yield { ...w, electionKind: 'politiche', severity: w.severity ?? 'warning' } as PoliticsResultEnvelope['result']['warnings'][number];
        }
        for (const w of record.result.warnings.filter(w => w.code.startsWith('RESULT_')))
          yield { ...w, electionKind: 'politiche', severity: w.severity ?? 'warning' } as PoliticsResultEnvelope['result']['warnings'][number];
      },
    });
}
