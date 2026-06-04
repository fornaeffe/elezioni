import type { ResultTable, Scenario } from '$lib/core/types';
import type { Ramo } from './types';

export interface PoliticsResultChartBar {
  label: string;
  value: number;
  displayValue: string;
  color: string;
  widthPercent: number;
}

export interface PoliticsResultChartGroup {
  label: Ramo;
  bars: PoliticsResultChartBar[];
}

export interface PoliticsResultChart {
  id: string;
  title: string;
  groups: PoliticsResultChartGroup[];
}

type NumericRow = Record<string, string | number | boolean | null>;

const fallbackColor = '#5f7f92';

function safeColor(value: string | undefined): string {
  if (!value) return fallbackColor;
  return /^#[0-9a-f]{6}$/i.test(value) ? value : fallbackColor;
}

function asString(value: string | number | boolean | null | undefined): string {
  if (value === null || value === undefined) return '';
  return String(value);
}

function asNumber(value: string | number | boolean | null | undefined): number {
  if (typeof value === 'number' && Number.isFinite(value)) return value;
  if (typeof value === 'string' && value.trim() !== '') {
    const parsed = Number(value);
    return Number.isFinite(parsed) ? parsed : 0;
  }
  return 0;
}

function tableByName(tables: readonly ResultTable[], name: string): ResultTable | undefined {
  return tables.find((table) => table.name === name);
}

function listColorMap(scenario: Scenario): Map<string, string> {
  return new Map(scenario.lists.map((list) => [list.name, safeColor(list.color)]));
}

function formatNumber(value: number, suffix = ''): string {
  return `${Number(value.toFixed(2))}${suffix}`;
}

function buildGroupedBars(params: {
  table: ResultTable;
  labelColumn: string;
  valueColumn: string;
  colors: ReadonlyMap<string, string>;
  valueSuffix?: string;
}): PoliticsResultChartGroup[] {
  const byRamo = new Map<Ramo, NumericRow[]>();

  for (const row of params.table.rows) {
    const ramo = asString(row.Ramo);
    if (ramo !== 'camera' && ramo !== 'senato') continue;

    const rows = byRamo.get(ramo) ?? [];
    rows.push(row);
    byRamo.set(ramo, rows);
  }

  return (['camera', 'senato'] as Ramo[])
    .map((ramo) => {
      const rows = byRamo.get(ramo) ?? [];
      const sorted = [...rows].sort(
        (left, right) => asNumber(right[params.valueColumn]) - asNumber(left[params.valueColumn])
      );
      const max = Math.max(0, ...sorted.map((row) => asNumber(row[params.valueColumn])));

      return {
        label: ramo,
        bars: sorted.map((row) => {
          const label = asString(row[params.labelColumn]);
          const value = asNumber(row[params.valueColumn]);
          return {
            label,
            value,
            displayValue: formatNumber(value, params.valueSuffix),
            color: params.colors.get(label) ?? fallbackColor,
            widthPercent: max === 0 ? 0 : (value / max) * 100
          };
        })
      };
    })
    .filter((group) => group.bars.length > 0);
}

export function buildPoliticsResultCharts(tables: readonly ResultTable[], scenario: Scenario): PoliticsResultChart[] {
  const colors = listColorMap(scenario);
  const charts: PoliticsResultChart[] = [];
  const plurinominalSeats = tableByName(tables, 'Average plurinominal seats by list');
  const voteShares = tableByName(tables, 'Vote share by list');

  if (plurinominalSeats) {
    charts.push({
      id: 'plurinominal-seats-by-list',
      title: 'Seggi plurinominali medi',
      groups: buildGroupedBars({
        table: plurinominalSeats,
        labelColumn: 'Lista',
        valueColumn: 'Media',
        colors
      })
    });
  }

  if (voteShares) {
    charts.push({
      id: 'vote-share-by-list',
      title: 'Percentuali medie sui voti validi',
      groups: buildGroupedBars({
        table: voteShares,
        labelColumn: 'Lista',
        valueColumn: 'Media %',
        colors,
        valueSuffix: '%'
      })
    });
  }

  return charts.filter((chart) => chart.groups.some((group) => group.bars.length > 0));
}
