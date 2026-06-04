import type { ResultTable, Scenario, SimulationResult } from './types';

export interface SimulationResultExport {
  schema_version: 1;
  exportedAt: string;
  scenario: Scenario;
  result: SimulationResult;
}

function jsonClone<T>(value: T): T {
  return JSON.parse(JSON.stringify(value)) as T;
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
}): SimulationResultExport {
  return {
    schema_version: 1,
    exportedAt: params.exportedAt,
    scenario: jsonClone(params.scenario),
    result: jsonClone(params.result)
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
