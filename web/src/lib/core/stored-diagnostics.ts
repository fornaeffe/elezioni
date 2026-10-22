import { cellsWarning, storedRows, type StoredResult } from './result-store';
import { createMessageSummary, diagnosticCells, diagnosticColumns, diagnosticEvent, diagnosticMatches, diagnosticScope, diagnosticTableName, type DiagnosticFilter } from './diagnostic-messages';
import type { ScrutinyWarning } from './types';

/** Versioned events prove exact import membership. Legacy native chunks can
 * prove run membership, but legacy imports retain only occurrence evidence. */
function source(record: StoredResult) {
  const tables = record.result.storage!.tables;
  let index = tables.findIndex(t => t.name === diagnosticTableName);
  const versioned = index >= 0;
  if (!versioned) index = tables.findIndex(t => t.name === 'Warnings and errors');
  const empty = record.result.completedSimulations === 0 && record.result.storage!.completedSimulations === 0;
  return { index, versioned, empty, exact: empty || versioned || index >= 0 && tables[index].runIndexed === true };
}
async function* messages(record: StoredResult) {
  const { index, versioned } = source(record);
  if (index >= 0) {
    const descriptor = record.result.storage!.tables[index];
    for await (const cells of storedRows(record.id, index)) {
      if (versioned) yield diagnosticEvent(cells, descriptor.columns);
      else {
        const row = Object.fromEntries(descriptor.columns.map((c, i) => [c, cells[i]]));
        const legacy = { code: row.Code as string, severity: (row.Severity ?? 'warning') as ScrutinyWarning['severity'],
          message: row.Message as string, occurrences: Number(row.Occurrences), electionKind: 'politiche',
          ...(row['First simulation'] == null ? {} : { simulationId: Number(row['First simulation']) }) } satisfies ScrutinyWarning;
        yield diagnosticEvent(diagnosticCells(legacy), diagnosticColumns);
      }
    }
  }
  // Raw warnings preserve execution failures (including imported ones) and are
  // independent of the manifest's deliberately short information preview.
  const seenExecution = new Set<string>();
  for await (const cells of storedRows(record.id, -1)) {
    const w = cellsWarning(cells), scope = diagnosticScope(w);
    if (scope === 'execution') { seenExecution.add(JSON.stringify(w)); yield w; }
    else if (index < 0) yield w;
  }
  for (const w of record.result.warnings) if (diagnosticScope(w) === 'execution' && !seenExecution.has(JSON.stringify(w))) yield w;
}
const pageOffset = (offset = 0) => {
  if (!Number.isSafeInteger(offset) || offset < 0) throw new Error('Invalid diagnostic page');
  return offset;
};
export async function storedMessageSummary(record: StoredResult, filter: DiagnosticFilter = {}, offset = 0) {
  const summary = createMessageSummary(record.result.benchmark.simulations, record.result.storage!.completedSimulations, source(record).exact, filter);
  for await (const w of messages(record)) summary.add(w);
  const capability = source(record);
  return { ...summary.finish(pageOffset(offset)), contextAvailable: capability.versioned || capability.empty };
}
export async function storedMessageContext(record: StoredResult, filter: DiagnosticFilter = {}, offset = 0, general = false) {
  const rows: ScrutinyWarning[] = []; let total = 0;
  pageOffset(offset);
  // Three streaming passes give general failures/warnings priority over compact
  // information notes without retaining every condition in an array.
  for (const severity of general ? ['error', 'warning', 'info'] : ['']) {
    for await (const w of messages(record)) {
      if (general ? diagnosticScope(w) === 'simulation' || (w.severity ?? 'warning') !== severity : diagnosticScope(w) !== 'simulation' || !diagnosticMatches(w, filter)) continue;
      if (total >= offset && rows.length < 50) rows.push(w);
      total++;
    }
  }
  return { rows, total, exact: source(record).exact };
}
/** No all-ID Set or array: committed event order is ascending by simulation.
 * Duplicates in one run do not advance the cursor. */
export async function* storedMessageIds(record: StoredResult, filter: DiagnosticFilter = {}) {
  if (!source(record).exact) throw new Error('Gli ID esatti non sono ricostruibili da questo archivio.');
  let last = 0, previous = 0;
  for await (const w of messages(record)) {
    if (diagnosticScope(w) !== 'simulation' || w.simulationId === undefined) continue;
    if (!Number.isSafeInteger(w.simulationId) || w.simulationId < 1 || w.simulationId < previous || w.simulationId > record.result.storage!.completedSimulations) throw new Error('Invalid diagnostic simulation order/coverage');
    previous = w.simulationId;
    if (!diagnosticMatches(w, filter)) continue;
    if (w.simulationId !== last) yield w.simulationId;
    last = w.simulationId;
  }
}
export async function storedMessageIdPage(record: StoredResult, filter: DiagnosticFilter = {}, offset = 0) {
  pageOffset(offset); const ids: number[] = []; let total = 0;
  for await (const id of storedMessageIds(record, filter)) { if (total >= offset && ids.length < 50) ids.push(id); total++; }
  return { ids, total };
}
