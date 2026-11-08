import { array } from '../../core/json-shape';
import { JsonStreamReader, jsonFileStream } from '../../core/json-stream';
import { canonicalJson, requiredAssets } from '../scenario/dependencies';
import { activeGenerator, requirePair } from '../scenario/registry';
import { parsePoliticsScenario } from '../scenario/validation';
import { resultEnvelopeShape, archiveEnvelopeShape, tableShape, cellShape, warningShape,
  type PoliticsResultEnvelope, type PoliticsArchiveEnvelope } from '../scenario/envelopes';
import { validatePoliticsCheckpoint } from './checkpoint';
import { checkShape, OLD_POLITICS_FORMAT, requireHeader } from './validation';

type Cell = string | number | boolean | null;
type Warning = PoliticsResultEnvelope['result']['warnings'][number];

/** Detached immutable-execution snapshot; does not read or replace any live draft. */
export function parsePoliticsResult(value: unknown): PoliticsResultEnvelope {
  requireHeader(value, 'politics-result');
  const run = checkShape(resultEnvelopeShape, value);
  const s = parsePoliticsScenario(run.executedScenario), e = run.execution, b = run.result.benchmark;
  const model = activeGenerator(s), pair = requirePair(s.selection.scrutiny.id, model.id);
  if (canonicalJson(e.generator) !== canonicalJson(model) || canonicalJson(e.scrutiny) !== canonicalJson(s.selection.scrutiny) ||
    e.adapter.id !== pair.adapterId || e.adapter.version !== '1.0.0' ||
    canonicalJson(e.data) !== canonicalJson(requiredAssets(s)) ||
    e.numericalVersion !== s.modelSettings[model.id].policies.numerical || e.rngVersion !== s.modelSettings[model.id].policies.rng ||
    canonicalJson(e.requestedWorkers) !== canonicalJson(s.execution.workers) ||
    s.execution.workers.kind === 'fixed' && e.actualWorkers > s.execution.workers.count ||
    e.fittingSeed !== s.execution.fittingSeed || e.simulationSeed !== s.execution.simulationSeed ||
    b.scrutinyAlgorithmId !== e.scrutiny.id || b.seed !== e.simulationSeed || b.simulations !== s.execution.simulations ||
    b.dataVersion !== s.data.historical.version || b.startedAt !== e.startedAt || b.elapsedMs !== e.elapsedMs)
    throw new Error('POLITICS_RESULT_EXECUTION_IDENTITY_MISMATCH');
  if (b.execution && (b.execution.computeWorkers !== e.actualWorkers || b.execution.computeWorkers > b.execution.maximumWorkers ||
    e.requestedWorkers.kind === 'fixed' && b.execution.requestedWorkers !== e.requestedWorkers.count))
    throw new Error('POLITICS_RESULT_WORKER_IDENTITY_MISMATCH');
  const ids = new Set<number>();
  for (const simulation of run.session.simulations) {
    if (ids.has(simulation.id) || simulation.id > s.execution.simulations) throw new Error('INVALID_POLITICS_SESSION_MEMBERSHIP');
    ids.add(simulation.id);
  }
  if (run.session.state === 'complete' && ids.size !== s.execution.simulations || run.result.completedSimulations !== ids.size)
    throw new Error('INVALID_POLITICS_SESSION_COVERAGE');
  for (const warning of run.result.warnings) validateWarningMembership(warning, ids);
  for (const table of run.result.tables) validateTable(table);
  return structuredClone(run);
}
function validateWarningMembership(warning: Warning, ids: Set<number>) {
  if (warning.simulationId !== undefined && !ids.has(warning.simulationId) || warning.scope === 'simulation' && warning.simulationId === undefined)
    throw new Error('INVALID_POLITICS_DIAGNOSTIC_MEMBERSHIP');
}
function validateTable(table: PoliticsResultEnvelope['result']['tables'][number]) {
  if (new Set(table.columns).size !== table.columns.length || !table.columns.length || table.columns.length > 4096 ||
    table.rows.some(row => row.length !== table.columns.length)) throw new Error('INVALID_POLITICS_RESULT_TABLE');
}
export async function parsePoliticsArchive(value: unknown): Promise<PoliticsArchiveEnvelope> {
  requireHeader(value, 'politics-result-archive');
  const archive = checkShape(archiveEnvelopeShape, value);
  const run = parsePoliticsResult(archive.run);
  if (activeGenerator(run.executedScenario).id === 'M02' && archive.portableFit === null)
    throw new Error('MISSING_POLITICS_PORTABLE_FIT');
  const portableFit = archive.portableFit === null ? null : await validatePoliticsCheckpoint(archive.portableFit, run.executedScenario, run.execution);
  return { ...archive, run, portableFit };
}
export function serializePoliticsResult(value: PoliticsResultEnvelope): string {
  const run = parsePoliticsResult(value);
  return JSON.stringify({ kind: run.kind, schemaVersion: run.schemaVersion, executedScenario: run.executedScenario,
    execution: run.execution, session: run.session, result: run.result });
}

/** A sink owns a separate staging area. Only commit may publish/select it;
 * abort must discard staging, including after truncated JSON or late identity errors. */
export interface PoliticsArchiveSink {
  table(index: number, descriptor: { name: string; columns: readonly string[] }): Promise<void>;
  row(index: number, cells: readonly Cell[]): Promise<void>;
  warning(warning: Warning): Promise<void>;
  commit(header: PoliticsArchiveEnvelope): Promise<void>;
  abort(): Promise<void>;
}

export async function importPoliticsArchiveStream(stream: ReadableStream<Uint8Array<ArrayBuffer>>, sink: PoliticsArchiveSink): Promise<PoliticsArchiveEnvelope> {
  const reader = new JsonStreamReader(stream), root: Record<string, unknown> = {}, run: Record<string, unknown> = {}, result: Record<string, unknown> = {};
  const warningIds = new Set<number>();
  try {
    await reader.object(async key => {
      if (['schema_version', 'scenario', 'fitFile'].includes(key)) throw new Error(OLD_POLITICS_FORMAT);
      if (!['kind', 'schemaVersion', 'exportedAt', 'run', 'portableFit'].includes(key)) throw new Error(`UNKNOWN_POLITICS_ARCHIVE_FIELD: ${key}`);
      if (key !== 'run') { root[key] = await reader.value(); return; }
      await reader.object(async field => {
        if (!['kind', 'schemaVersion', 'executedScenario', 'execution', 'session', 'result'].includes(field)) throw new Error(`UNKNOWN_POLITICS_RESULT_FIELD: ${field}`);
        if (field !== 'result') { run[field] = await reader.value(); return; }
        await reader.object(async property => {
          if (property === 'tables') {
            result.tables = [];
            await reader.array(async index => {
              const table: Record<string, unknown> = {};
              // Row width is checked after reading the descriptor, allowing JSON key reordering.
              const widths = new Set<number>();
              await reader.object(async name => {
                if (name === 'rows') {
                  table.rows = [];
                  await reader.array(async () => {
                    const cells: Cell[] = [];
                    await reader.array(async index => {
                      if (index >= 4096) throw new Error('POLITICS_RESULT_ROW_LIMIT');
                      const token = await reader.take();
                      if (!token || token.kind === 'punctuation') throw new Error('INVALID_POLITICS_RESULT_CELL');
                      cells.push(checkShape(cellShape, token.value));
                    });
                    widths.add(cells.length); await sink.row(index, cells);
                  });
                } else if (name === 'name' || name === 'columns') table[name] = await reader.value();
                else throw new Error(`UNKNOWN_POLITICS_TABLE_FIELD: ${name}`);
              });
              const parsed = checkShape(tableShape, table); validateTable(parsed);
              if ([...widths].some(width => width !== parsed.columns.length)) throw new Error('INVALID_POLITICS_RESULT_TABLE_WIDTH');
              (result.tables as unknown[]).push(parsed);
              await sink.table(index, parsed);
            });
          } else if (property === 'warnings') {
            result.warnings = [];
            await reader.array(async () => {
              const warning = checkShape(warningShape, await reader.value());
              if (warning.scope === 'simulation' && warning.simulationId === undefined) throw new Error('INVALID_POLITICS_DIAGNOSTIC_MEMBERSHIP');
              if (warning.simulationId !== undefined) warningIds.add(warning.simulationId);
              await sink.warning(warning);
            });
          } else if (['type', 'status', 'completedSimulations', 'benchmark'].includes(property)) result[property] = await reader.value();
          else throw new Error(`UNKNOWN_POLITICS_WORKER_RESULT_FIELD: ${property}`);
        });
        run.result = result;
      });
      root.run = run;
    });
    await reader.finish();
    const archive = await parsePoliticsArchive(root);
    const ids = new Set(archive.run.session.simulations.map(s => s.id));
    if ([...warningIds].some(id => !ids.has(id))) throw new Error('INVALID_POLITICS_DIAGNOSTIC_MEMBERSHIP');
    await sink.commit(archive);
    return archive;
  } catch (error) { await sink.abort(); throw error; }
  finally { await reader.close(); }
}

/** File transport detection is shared with independent regional imports. */
export const importPoliticsArchiveFile = (file: File, sink: PoliticsArchiveSink) => importPoliticsArchiveStream(jsonFileStream(file), sink);

export interface PoliticsArchiveRows {
  rows(index: number): AsyncIterable<readonly Cell[]>;
  warnings(): AsyncIterable<Warning>;
}
/** Header first, then one row/diagnostic at a time. The caller can pipe UTF-8
 * chunks through CompressionStream('gzip') without making a full archive copy. */
export async function* streamPoliticsArchive(value: PoliticsArchiveEnvelope, source?: PoliticsArchiveRows): AsyncGenerator<string> {
  const archive = await parsePoliticsArchive(value), run = archive.run;
  const { result } = run, { tables, warnings, ...resultHeader } = result;
  const header = { kind: run.kind, schemaVersion: run.schemaVersion, executedScenario: run.executedScenario, execution: run.execution, session: run.session };
  yield `{"kind":"politics-result-archive","schemaVersion":1,"exportedAt":${JSON.stringify(archive.exportedAt)},"run":{`;
  yield `${JSON.stringify(header).slice(1, -1)},"result":{${JSON.stringify(resultHeader).slice(1, -1)},"tables":[`;
  for (let i = 0; i < tables.length; i++) {
    const table = tables[i];
    yield `${i ? ',' : ''}{"name":${JSON.stringify(table.name)},"columns":${JSON.stringify(table.columns)},"rows":[`;
    let first = true;
    for await (const row of source ? source.rows(i) : table.rows) {
      checkShape(array(cellShape), row);
      if (row.length !== table.columns.length) throw new Error('INVALID_POLITICS_RESULT_TABLE_WIDTH');
      yield `${first ? '' : ','}${JSON.stringify(row)}`; first = false;
    }
    yield ']}';
  }
  yield '],"warnings":[';
  let first = true;
  const ids = new Set(run.session.simulations.map(s => s.id));
  for await (const warning of source ? source.warnings() : warnings) {
    checkShape(warningShape, warning); validateWarningMembership(warning, ids);
    yield `${first ? '' : ','}${JSON.stringify(warning)}`; first = false;
  }
  yield ']}},"portableFit":';
  if (archive.portableFit === null) yield 'null';
  else {
    const { assets, ...fitHeader } = archive.portableFit;
    yield `{${JSON.stringify(fitHeader).slice(1, -1)},"assets":[`;
    for (let i = 0; i < assets.length; i++) {
      yield `${i ? ',' : ''}{"path":${JSON.stringify(assets[i].path)},"chunks":[`;
      for (let j = 0; j < assets[i].chunks.length; j++) yield `${j ? ',' : ''}${JSON.stringify(assets[i].chunks[j])}`;
      yield ']}';
    }
    yield ']}';
  }
  yield '}';
}

/** Small JSON/JSON.gz result documents use the same duplicate-key rejecting reader. */
export async function readPoliticsResultFile(file: File): Promise<PoliticsResultEnvelope> {
  const reader = new JsonStreamReader(jsonFileStream(file));
  try { const value = await reader.value(); await reader.finish(); return parsePoliticsResult(value); }
  finally { await reader.close(); }
}
