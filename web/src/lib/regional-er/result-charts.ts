import type { ResultTable, Scenario } from '$lib/core/types';

export interface RegionalErChartItem {
  label: string;
  value: number;
  color: string;
}

export interface RegionalErResultChart {
  id: string;
  title: string;
  unit: string;
  items: RegionalErChartItem[];
}

function colorByList(scenario: Scenario): Map<string, string> {
  return new Map(scenario.lists.map((row) => [row.name, row.color]));
}

function colorByCoalition(scenario: Scenario): Map<string, string> {
  return new Map(scenario.coalitions.map((row) => [row.name, row.color]));
}

function numberCell(row: Record<string, string | number | boolean | null>, column: string): number {
  const value = row[column];
  return typeof value === 'number' && Number.isFinite(value) ? value : 0;
}

function stringCell(row: Record<string, string | number | boolean | null>, column: string): string {
  const value = row[column];
  return typeof value === 'string' ? value : '';
}

function tableByName(tables: readonly ResultTable[], name: string): ResultTable | null {
  return tables.find((table) => table.name === name) ?? null;
}

export function buildRegionalErResultCharts(
  tables: readonly ResultTable[],
  scenario: Scenario
): RegionalErResultChart[] {
  const listColors = colorByList(scenario);
  const coalitionColors = colorByCoalition(scenario);
  const charts: RegionalErResultChart[] = [];
  const seatsByList = tableByName(tables, 'Regional average seats by list');
  const voteShareByList = tableByName(tables, 'Regional vote share by list');
  const coalitionOutcomes = tableByName(tables, 'Regional coalition outcomes');

  if (seatsByList) {
    charts.push({
      id: 'regional-list-seats',
      title: 'Seggi medi per lista',
      unit: 'seggi',
      items: seatsByList.rows
        .map((row) => ({
          label: stringCell(row, 'Lista'),
          value: numberCell(row, 'Media'),
          color: listColors.get(stringCell(row, 'Lista')) ?? '#6f7f8f'
        }))
        .sort((left, right) => right.value - left.value)
    });
  }

  if (voteShareByList) {
    charts.push({
      id: 'regional-list-votes',
      title: 'Percentuali medie sui voti validi',
      unit: '%',
      items: voteShareByList.rows
        .map((row) => ({
          label: stringCell(row, 'Lista'),
          value: numberCell(row, 'Media %'),
          color: listColors.get(stringCell(row, 'Lista')) ?? '#6f7f8f'
        }))
        .sort((left, right) => right.value - left.value)
    });
  }

  if (coalitionOutcomes) {
    charts.push({
      id: 'regional-coalition-win',
      title: 'Probabilita presidente',
      unit: '%',
      items: coalitionOutcomes.rows
        .map((row) => ({
          label: stringCell(row, 'Coalizione'),
          value: numberCell(row, 'Prob. presidente %'),
          color: coalitionColors.get(stringCell(row, 'Coalizione')) ?? '#6f7f8f'
        }))
        .sort((left, right) => right.value - left.value)
    });
  }

  return charts.filter((chart) => chart.items.length > 0);
}
