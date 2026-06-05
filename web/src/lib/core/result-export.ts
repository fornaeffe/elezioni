import type { ResultTable, Scenario, SimulationResult } from './types';

export interface SimulationResultExport {
  schema_version: 1 | 2;
  exportedAt: string;
  scenario: Scenario;
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

export interface SerializedSimulationResultExportV2 {
  schema_version: 2;
  exportedAt: string;
  scenario: Scenario;
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

function parseExportedScenario(value: unknown): Scenario {
  const scenario = requireRecord(value, 'scenario');
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

function parseRowObjectResultTable(value: unknown, index: number): ResultTable {
  const table = requireRecord(value, `result.tables[${index}]`);
  const columns = requireStringArray(table.columns, `result.tables[${index}].columns`);
  const rows = requireArray(table.rows, `result.tables[${index}].rows`).map((row, rowIndex) => {
    const rowRecord = requireRecord(row, `result.tables[${index}].rows[${rowIndex}]`);
    const parsedRow: Record<string, JsonCell> = {};

    for (const column of columns) {
      if (!(column in rowRecord)) {
        throw new Error(`result.tables[${index}].rows[${rowIndex}].${column} is missing.`);
      }
      parsedRow[column] = requireCell(rowRecord[column], `result.tables[${index}].rows[${rowIndex}].${column}`);
    }

    for (const [key, cell] of Object.entries(rowRecord)) {
      if (key in parsedRow) continue;
      parsedRow[key] = requireCell(cell, `result.tables[${index}].rows[${rowIndex}].${key}`);
    }

    return parsedRow;
  });

  return {
    name: requireString(table.name, `result.tables[${index}].name`),
    columns,
    rows
  };
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

  return parsedWarning;
}

function parseSimulationResult(value: unknown, tableFormat: 'row-object' | 'columnar'): SimulationResult {
  const result = requireRecord(value, 'result');
  const benchmark = requireRecord(result.benchmark, 'result.benchmark');
  const status = requireEnum(result.status, 'result.status', ['completed', 'not_implemented']);
  const scrutinyAlgorithmId = requireOptionalString(benchmark.scrutinyAlgorithmId, 'result.benchmark.scrutinyAlgorithmId');

  if (result.type !== 'result') throw new Error('result.type must be "result".');

  const parsedResult: SimulationResult = {
    type: 'result',
    status,
    tables: requireArray(result.tables, 'result.tables').map(
      tableFormat === 'columnar' ? parseColumnarResultTable : parseRowObjectResultTable
    ),
    warnings: requireArray(result.warnings, 'result.warnings').map(parseWarning),
    benchmark: {
      startedAt: requireString(benchmark.startedAt, 'result.benchmark.startedAt'),
      elapsedMs: requireFiniteNumber(benchmark.elapsedMs, 'result.benchmark.elapsedMs'),
      simulations: requireFiniteNumber(benchmark.simulations, 'result.benchmark.simulations'),
      dataVersion: requireString(benchmark.dataVersion, 'result.benchmark.dataVersion')
    }
  };

  if (scrutinyAlgorithmId !== undefined) parsedResult.benchmark.scrutinyAlgorithmId = scrutinyAlgorithmId;

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

export function createSimulationResultExport(params: {
  result: SimulationResult;
  scenario: Scenario;
  exportedAt: string;
}): SerializedSimulationResultExportV2 {
  const result = jsonClone(params.result);

  return {
    schema_version: 2,
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
  if (payload.schema_version !== 1 && payload.schema_version !== 2) {
    throw new Error(`Unsupported result export schema_version: ${String(payload.schema_version)}.`);
  }

  const exportedAt = requireString(payload.exportedAt, 'exportedAt');
  if (Number.isNaN(Date.parse(exportedAt))) throw new Error('exportedAt must be a valid date string.');

  return {
    schema_version: payload.schema_version,
    exportedAt,
    scenario: parseExportedScenario(payload.scenario),
    result: parseSimulationResult(payload.result, payload.schema_version === 2 ? 'columnar' : 'row-object')
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
