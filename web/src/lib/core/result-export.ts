import { requireReformScenario } from '../politics/reform-2026/generation-scenario';
import type { ReformGenerationScenario } from '../politics/reform-2026/generation-types';
import type { ReleasedScenario } from '../politics/reform-2026/vote-share/release';
import { loadCandidate } from '../politics/reform-2026/vote-share/release';
import { requireFastScenario } from '../politics/reform-2026/vote-share/scenario';
import { requirePortableFitFile, checkpointReader, sameCheckpointScenario, type PortableFitFile } from '../politics/reform-2026/vote-share/checkpoint';
import type { ResultTable, Scenario, SimulationResult } from './types';
import { JsonStreamReader } from './json-stream';
import { cellsWarning, storedRows, type Cell, type StoredResult, type StoredTable } from './result-store';

export type ExportScenario = Scenario | ReformGenerationScenario | ReleasedScenario;

export interface SimulationResultExport {
  schema_version: 2|3;
  fitFile?:PortableFitFile;
  exportedAt: string;
  scenario: ExportScenario;
  result: SimulationResult;
}

interface ColumnarResultTable {
  name: string;
  columns: string[];
  rows: JsonCell[][];
}

interface ColumnarSimulationResult extends Omit<SimulationResult, 'tables'> {
  tables: ColumnarResultTable[];
}

export interface SerializedSimulationResultExportV2<T extends ExportScenario = ExportScenario> {
  schema_version: 2|3;
  fitFile?:PortableFitFile;
  exportedAt: string;
  scenario: T;
  result: ColumnarSimulationResult;
}

type JsonRecord = Record<string, unknown>;
type JsonCell = string | number | boolean | null;

function jsonClone<T>(value: T): T {
  return JSON.parse(JSON.stringify(value)) as T;
}

function isRecord(value: unknown): value is JsonRecord {
  return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function describeError(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

function requireRecord(value: unknown, path: string): JsonRecord {
  if (!isRecord(value)) throw new Error(`${path} must be an object.`);
  return value;
}

function requireString(value: unknown, path: string): string {
  if (typeof value !== 'string') throw new Error(`${path} must be a string.`);
  return value;
}

function requireFiniteNumber(value: unknown, path: string): number {
  if (typeof value !== 'number' || !Number.isFinite(value)) {
    throw new Error(`${path} must be a finite number.`);
  }
  return value;
}

function requireBoolean(value: unknown, path: string): boolean {
  if (typeof value !== 'boolean') throw new Error(`${path} must be a boolean.`);
  return value;
}

function requireArray(value: unknown, path: string): unknown[] {
  if (!Array.isArray(value)) throw new Error(`${path} must be an array.`);
  return value;
}

function requireStringArray(value: unknown, path: string): string[] {
  return requireArray(value, path).map((item, index) => requireString(item, `${path}[${index}]`));
}

function requireOptionalString(value: unknown, path: string): string | undefined {
  if (value === undefined) return undefined;
  return requireString(value, path);
}

function requireOptionalFiniteNumber(value: unknown, path: string): number | undefined {
  if (value === undefined) return undefined;
  return requireFiniteNumber(value, path);
}

function requireEnum<T extends string>(value: unknown, path: string, allowed: readonly T[]): T {
  if (typeof value !== 'string' || !allowed.includes(value as T)) {
    throw new Error(`${path} must be one of: ${allowed.join(', ')}.`);
  }
  return value as T;
}

function requireOptionalEnum<T extends string>(
  value: unknown,
  path: string,
  allowed: readonly T[]
): T | undefined {
  if (value === undefined) return undefined;
  if (typeof value !== 'string' || !allowed.includes(value as T)) {
    throw new Error(`${path} must be one of: ${allowed.join(', ')}.`);
  }
  return value as T;
}

function requireCell(value: unknown, path: string): JsonCell {
  if (value === null || typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean') {
    if (typeof value === 'number' && !Number.isFinite(value)) {
      throw new Error(`${path} must be a finite number.`);
    }
    return value;
  }

  throw new Error(`${path} must be a string, number, boolean, or null.`);
}

function tableCell(value: string | number | boolean | null | undefined): JsonCell {
  return value ?? null;
}

function parseExportedScenario(value: unknown): ExportScenario {
  if(isRecord(value)&&value.kind==='politics-reform-scenario'&&value.schemaVersion===4)return requireFastScenario(value);
  const scenario = requireRecord(value, 'scenario');
  if (scenario.kind === 'politics-reform-scenario') return requireReformScenario(scenario);
  const defaultSource = requireRecord(scenario.defaultSource, 'scenario.defaultSource');

  requireString(scenario.id, 'scenario.id');
  requireString(scenario.name, 'scenario.name');
  requireString(scenario.electionDate, 'scenario.electionDate');
  requireEnum(scenario.globalShareMode, 'scenario.globalShareMode', ['mean']);
  requireFiniteNumber(scenario.abstentionShare, 'scenario.abstentionShare');
  requireBoolean(scenario.abstentionOverride, 'scenario.abstentionOverride');

  requireEnum(defaultSource.kind, 'scenario.defaultSource.kind', ['bundled', 'last-election', 'manual']);
  requireEnum(defaultSource.electionKind, 'scenario.defaultSource.electionKind', [
    'politiche',
    'regionali-er',
    'comunali'
  ]);
  requireString(defaultSource.territory, 'scenario.defaultSource.territory');
  requireString(defaultSource.dataVersion, 'scenario.defaultSource.dataVersion');
  requireOptionalString(defaultSource.snapshotId, 'scenario.defaultSource.snapshotId');

  requireArray(scenario.coalitions, 'scenario.coalitions');
  requireArray(scenario.lists, 'scenario.lists');
  requireArray(scenario.listCorrespondences, 'scenario.listCorrespondences');
  requireArray(scenario.localShareOverrides, 'scenario.localShareOverrides');
  requireArray(scenario.candidateTemplates, 'scenario.candidateTemplates');

  return scenario as unknown as Scenario;
}

function parseColumnarResultTable(value: unknown, index: number): ResultTable {
  const table = requireRecord(value, `result.tables[${index}]`);
  const columns = requireStringArray(table.columns, `result.tables[${index}].columns`);
  const rows = requireArray(table.rows, `result.tables[${index}].rows`).map((row, rowIndex) => {
    const rowValues = requireArray(row, `result.tables[${index}].rows[${rowIndex}]`);
    if (rowValues.length !== columns.length) {
      throw new Error(
        `result.tables[${index}].rows[${rowIndex}] must have ${columns.length} cells, found ${rowValues.length}.`
      );
    }

    return Object.fromEntries(
      columns.map((column, columnIndex) => [
        column,
        requireCell(rowValues[columnIndex], `result.tables[${index}].rows[${rowIndex}][${columnIndex}]`)
      ])
    ) as Record<string, JsonCell>;
  });

  return {
    name: requireString(table.name, `result.tables[${index}].name`),
    columns,
    rows
  };
}

/** Numeric IDs are streamed separately from complete result archives. */
export async function* streamDiagnosticIdsCsv(ids: AsyncIterable<number>): AsyncGenerator<string> {
  yield 'Simulation\r\n';
  for await (const id of ids) {
    if (!Number.isSafeInteger(id) || id < 1) throw new Error('Invalid simulation ID');
    yield `${id}\r\n`;
  }
}

function parseWarning(value: unknown, index: number): SimulationResult['warnings'][number] {
  const warning = requireRecord(value, `result.warnings[${index}]`);
  const simulationId = requireOptionalFiniteNumber(warning.simulationId, `result.warnings[${index}].simulationId`);
  const severity = requireOptionalEnum(warning.severity, `result.warnings[${index}].severity`, [
    'info',
    'warning',
    'error'
  ]);
  const lawReference = requireOptionalString(warning.lawReference, `result.warnings[${index}].lawReference`);
  const todoReference = requireOptionalString(warning.todoReference, `result.warnings[${index}].todoReference`);
  const parsedWarning: SimulationResult['warnings'][number] = {
    code: requireString(warning.code, `result.warnings[${index}].code`),
    electionKind: requireEnum(warning.electionKind, `result.warnings[${index}].electionKind`, [
      'politiche',
      'regionali-er',
      'comunali'
    ]),
    message: requireString(warning.message, `result.warnings[${index}].message`)
  };

  if (simulationId !== undefined) parsedWarning.simulationId = simulationId;
  if (severity !== undefined) parsedWarning.severity = severity;
  if (lawReference !== undefined) parsedWarning.lawReference = lawReference;
  if (todoReference !== undefined) parsedWarning.todoReference = todoReference;
  const chamber = requireOptionalEnum(warning.chamber, `result.warnings[${index}].chamber`, ['camera', 'senato']);
  if (chamber !== undefined) parsedWarning.chamber = chamber;
  const scope = requireOptionalEnum(warning.scope, `result.warnings[${index}].scope`, ['global', 'simulation', 'execution']);
  if (scope !== undefined) parsedWarning.scope = scope;
  const occurrences = requireOptionalFiniteNumber(warning.occurrences, `result.warnings[${index}].occurrences`);
  if (occurrences !== undefined) {
    if (!Number.isSafeInteger(occurrences) || occurrences < 1) throw new Error('Invalid diagnostic occurrence count');
    parsedWarning.occurrences = occurrences;
  }
  for (const key of ['interpretationId', 'seatId', 'drawContextId', 'reason'] as const) {
    const value = requireOptionalString(warning[key], `result.warnings[${index}].${key}`);
    if (value !== undefined) parsedWarning[key] = value;
  }

  return parsedWarning;
}

function parseSimulationResult(value: unknown): SimulationResult {
  const result = requireRecord(value, 'result');
  const benchmark = requireRecord(result.benchmark, 'result.benchmark');
  const status = requireEnum(result.status, 'result.status', ['completed', 'not_implemented']);
  const scrutinyAlgorithmId = requireOptionalString(benchmark.scrutinyAlgorithmId, 'result.benchmark.scrutinyAlgorithmId');

  if (result.type !== 'result') throw new Error('result.type must be "result".');

  const parsedResult: SimulationResult = {
    type: 'result',
    status,
    tables: requireArray(result.tables, 'result.tables').map(parseColumnarResultTable),
    warnings: requireArray(result.warnings, 'result.warnings').map(parseWarning),
    benchmark: {
      startedAt: requireString(benchmark.startedAt, 'result.benchmark.startedAt'),
      elapsedMs: requireFiniteNumber(benchmark.elapsedMs, 'result.benchmark.elapsedMs'),
      simulations: requireFiniteNumber(benchmark.simulations, 'result.benchmark.simulations'),
      dataVersion: requireString(benchmark.dataVersion, 'result.benchmark.dataVersion')
    }
  };

  const completed = requireOptionalFiniteNumber(result.completedSimulations, 'result.completedSimulations');
  if (completed !== undefined) {
    if (!Number.isSafeInteger(completed) || completed < 0 || completed > parsedResult.benchmark.simulations) throw new Error('Invalid completed simulation coverage');
    parsedResult.completedSimulations = completed;
  }

  if (scrutinyAlgorithmId !== undefined) parsedResult.benchmark.scrutinyAlgorithmId = scrutinyAlgorithmId;
  const seed = requireOptionalString(benchmark.seed, 'result.benchmark.seed');
  if (seed !== undefined) parsedResult.benchmark.seed = seed;
  if (benchmark.execution !== undefined) {
    const execution = requireRecord(benchmark.execution, 'result.benchmark.execution');
    const positiveInteger = (key: string) => {
      const value = requireFiniteNumber(execution[key], `result.benchmark.execution.${key}`);
      if (!Number.isSafeInteger(value) || value < 1) throw new Error(`result.benchmark.execution.${key} must be a positive integer.`);
      return value;
    };
    const nonnegative = (key: string) => {
      const value = requireFiniteNumber(execution[key], `result.benchmark.execution.${key}`);
      if (value < 0) throw new Error(`result.benchmark.execution.${key} must be nonnegative.`);
      return value;
    };
    const parsed = { requestedWorkers: positiveInteger('requestedWorkers'), computeWorkers: positiveInteger('computeWorkers'),
      maximumWorkers: positiveInteger('maximumWorkers'), estimatedWorkerMemoryMiB: positiveInteger('estimatedWorkerMemoryMiB'),
      estimateVersion: requireString(execution.estimateVersion, 'result.benchmark.execution.estimateVersion'),
      generationMs: nonnegative('generationMs'), scrutinyMs: nonnegative('scrutinyMs'),
      presentationMs: nonnegative('presentationMs'), storageMs: nonnegative('storageMs') };
    if (parsed.computeWorkers > parsed.requestedWorkers || parsed.requestedWorkers > parsed.maximumWorkers) throw new Error('Inconsistent compute worker counts.');
    parsedResult.benchmark.execution = parsed;
  }

  return parsedResult;
}

function csvCell(value: string | number | boolean | null | undefined): string {
  if (value === null || value === undefined) return '';

  const text = String(value);
  if (!/[",\r\n]/.test(text)) return text;

  return `"${text.replaceAll('"', '""')}"`;
}

function csvRow(values: ReadonlyArray<string | number | boolean | null | undefined>): string {
  return values.map(csvCell).join(',');
}

export function createSimulationResultExport<T extends ExportScenario>(params: {
  fitFile?:PortableFitFile;
  result: SimulationResult;
  scenario: T;
  exportedAt: string;
}): SerializedSimulationResultExportV2<T> {
  if (params.result.storage) throw new Error('Stored results must use the streaming export boundary.');
  if ('schemaVersion' in params.scenario && params.scenario.schemaVersion===4 && (!params.fitFile || !sameCheckpointScenario(params.scenario,params.fitFile.scenario))) throw new Error('Fast results require their frozen fitted session.');
  const result = jsonClone(params.result);

  return {
    schema_version: 'schemaVersion' in params.scenario&&params.scenario.schemaVersion===4?3:2,
    ...(params.fitFile?{fitFile:requirePortableFitFile(params.fitFile)}:{}),
    exportedAt: params.exportedAt,
    scenario: jsonClone(params.scenario),
    result: {
      ...result,
      tables: result.tables.map((table) => ({
        name: table.name,
        columns: [...table.columns],
        rows: table.rows.map((row) => table.columns.map((column) => tableCell(row[column])))
      }))
    }
  };
}

export function parseSimulationResultExport(text: string): SimulationResultExport {
  let parsed: unknown;

  try {
    parsed = JSON.parse(text) as unknown;
  } catch (error) {
    throw new Error(`Result export JSON is not valid JSON: ${describeError(error)}`);
  }

  const payload = requireRecord(parsed, 'Result export JSON');

  if (!('scenario' in payload)) {
    throw new Error('Result export JSON must include the scenario that generated the results.');
  }
  if (!('result' in payload)) {
    throw new Error('Result export JSON must include a result.');
  }
  if (payload.schema_version !== 2 && payload.schema_version!==3) {
    throw new Error(`Unsupported result export schema_version: ${String(payload.schema_version)}.`);
  }

  const exportedAt = requireString(payload.exportedAt, 'exportedAt');
  if (Number.isNaN(Date.parse(exportedAt))) throw new Error('exportedAt must be a valid date string.');

  const scenario = parseExportedScenario(payload.scenario), result = parseSimulationResult(payload.result);
  const fast='kind' in scenario&&scenario.schemaVersion===4;
  const fitFile=payload.schema_version===3?requirePortableFitFile(payload.fitFile):undefined;
  if(fast!==(payload.schema_version===3)||fitFile&&!sameCheckpointScenario(fitFile.scenario,scenario as ReleasedScenario))throw new Error('Fitted scenario/result file identity mismatch.');
  if ('kind' in scenario && scenario.kind === 'politics-reform-scenario') {
    if (result.benchmark.scrutinyAlgorithmId !== scenario.algorithmId || result.benchmark.seed !== scenario.seed || result.benchmark.dataVersion !== (scenario.schemaVersion===4?scenario.data.sourceBundleId:scenario.snapshotVersion)) {
      throw new Error('Reform scenario and result algorithm/seed/data identity mismatch.');
    }
  } else {
    if (result.benchmark.scrutinyAlgorithmId === 'politiche-reform-2026-v1') throw new Error('Reform result requires an explicit reform scenario.');
    if ('defaultSource' in scenario && scenario.defaultSource.electionKind === 'politiche' && !result.benchmark.scrutinyAlgorithmId) result.benchmark.scrutinyAlgorithmId = 'politiche-r-parity-v1';
  }
  return {
    schema_version: payload.schema_version,
    ...(fitFile?{fitFile}:{}),
    exportedAt,
    scenario,
    result
  };
}

export function resultTablesToCsv(tables: readonly ResultTable[]): string {
  const lines: string[] = [];

  for (const table of tables) {
    if (lines.length > 0) lines.push('');

    lines.push(csvRow(['Table', table.name]));
    lines.push(csvRow(table.columns));

    for (const row of table.rows) {
      lines.push(csvRow(table.columns.map((column) => row[column])));
    }
  }

  return lines.join('\r\n');
}

/** Same schema-v2 columnar JSON as the synchronous small-result API, emitted
 * a row at a time. No full-result copy or whole-file string is constructed. */
export async function* streamStoredResult(record: StoredResult, format: 'json' | 'csv'): AsyncGenerator<string> {
  const tables = record.result.storage!.tables;
  if (format === 'csv') {
    let first = true;
    for (let i = 0; i < tables.length; i++) {
      const table = tables[i];
      if (!first) yield '\r\n\r\n'; first = false;
      yield `${csvRow(['Table', table.name])}\r\n${csvRow(table.columns)}`;
      for await (const cells of storedRows(record.id, i)) yield `\r\n${csvRow(cells)}`;
    }
    return;
  }
  const coverage = record.result.completedSimulations === undefined ? '' : `,"completedSimulations":${record.result.storage!.completedSimulations}`;
  const fast='schemaVersion'in record.scenario&&record.scenario.schemaVersion===4;
  if(fast&&!record.fitFile)throw new Error('Missing frozen fitted state for export');
  yield `{"schema_version":${fast?3:2},"exportedAt":${JSON.stringify(new Date().toISOString())},"scenario":${JSON.stringify(record.scenario)}${fast?`,"fitFile":${JSON.stringify(record.fitFile)}`:''},"result":{"type":"result","status":${JSON.stringify(record.result.status)}${coverage},"benchmark":${JSON.stringify(record.result.benchmark)},"warnings":[`;
  let first = true;
  for await (const cells of storedRows(record.id, -1)) {
    if (!first) yield ','; first = false;
    yield JSON.stringify(cellsWarning(cells));
  }
  for (const warning of record.result.warnings.filter(w => w.code.startsWith('RESULT_'))) {
    if (!first) yield ','; first = false; yield JSON.stringify(warning);
  }
  yield '],"tables":[';
  for (let i = 0; i < tables.length; i++) {
    const table = tables[i];
    if (i) yield ',';
    yield `{"name":${JSON.stringify(table.name)},"columns":${JSON.stringify(table.columns)},"rows":[`;
    let firstRow = true;
    for await (const cells of storedRows(record.id, i)) {
      if (!firstRow) yield ','; firstRow = false;
      yield JSON.stringify(cells);
    }
    yield ']}';
  }
  yield ']}}';
}

export interface StreamingResultSink {
  table(index: number, descriptor: Pick<StoredTable, 'name' | 'columns'>): Promise<void>;
  row(index: number, cells: Cell[]): Promise<void>;
  warning(warning: SimulationResult['warnings'][number]): Promise<void>;
}
export async function importResultStream(stream: ReadableStream<Uint8Array<ArrayBuffer>>, sink: StreamingResultSink): Promise<SimulationResultExport> {
  const reader = new JsonStreamReader(stream);
  const root: JsonRecord = {}, result: JsonRecord = {};
  let sawTables = false, sawWarnings = false;
  try {
    await reader.object(async key => {
      if (key !== 'result') {
        if (['schema_version', 'exportedAt', 'scenario','fitFile'].includes(key)) root[key] = await reader.value();
        else await reader.skip();
        return;
      }
      await reader.object(async field => {
        if (field === 'warnings') {
          sawWarnings = true;
          await reader.array(async index => sink.warning(parseWarning(await reader.value(), index)));
        } else if (field === 'tables') {
          sawTables = true;
          await reader.array(async index => {
            let name = '', columns: string[] | undefined, sawRows = false;
            const widths = new Set<number>();
            await reader.object(async property => {
              if (property === 'name') {
                name = requireString(await reader.value(), `result.tables[${index}].name`);
                await sink.table(index, { name, columns: columns ?? [] });
              } else if (property === 'columns') {
                columns = requireStringArray(await reader.value(), `result.tables[${index}].columns`);
                await sink.table(index, { name, columns });
              } else if (property === 'rows') {
                sawRows = true;
                await reader.array(async rowIndex => {
                  const values = requireArray(await reader.value(), `result.tables[${index}].rows[${rowIndex}]`);
                  if (values.length > 4096) throw new Error('Result row has too many cells');
                  const cells = values.map((v, c) => requireCell(v, `result.tables[${index}].rows[${rowIndex}][${c}]`));
                  widths.add(cells.length); await sink.row(index, cells);
                });
              } else await reader.skip();
            });
            if (!name || !columns || !sawRows || [...widths].some(n => n !== columns!.length)) throw new Error(`Invalid columnar result table ${index}`);
            await sink.table(index, { name, columns });
          });
        } else if (['type', 'status', 'benchmark', 'completedSimulations'].includes(field)) result[field] = await reader.value();
        else await reader.skip();
      });
    });
    await reader.finish();
    if (!sawTables || !sawWarnings) throw new Error('Result export must include tables and warnings');
    // Reuse the existing strict envelope and identity validation on small headers.
    const parsed=parseSimulationResultExport(JSON.stringify({ ...root, result: { ...result, tables: [], warnings: [] } }));
    if(parsed.fitFile){const restored=await loadCandidate(parsed.fitFile.pointer,checkpointReader(parsed.fitFile),{allowEditedEvidence:true,
      expectedGenerationImplementationId:(parsed.scenario as ReleasedScenario).models.generationImplementationId});
      if(!sameCheckpointScenario(restored.scenario,parsed.scenario as ReleasedScenario))throw new Error('Saved fitted state/scenario mismatch.');}
    return parsed;
  } finally { await reader.close(); }
}
