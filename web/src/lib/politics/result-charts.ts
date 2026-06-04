import type { ResultTable, Scenario } from '$lib/core/types';
import {
  politicsCoalitionSeatVotePlotTableName,
  politicsListSeatVotePlotTableName,
  politicsPlurinominalSeatVotePlotTableName
} from './result-presentation';
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

export interface PoliticsChartTick {
  value: number;
  label: string;
  positionPercent: number;
}

export interface PoliticsBarResultChart {
  kind: 'bar';
  id: string;
  title: string;
  groups: PoliticsResultChartGroup[];
}

export interface PoliticsBoxPlotItem {
  label: string;
  color: string;
  whiskerLow: number;
  q1: number;
  median: number;
  q3: number;
  whiskerHigh: number;
  average: number;
  whiskerLowPercent: number;
  q1Percent: number;
  medianPercent: number;
  q3Percent: number;
  whiskerHighPercent: number;
  averagePercent: number;
  outlierPercents: number[];
}

export interface PoliticsBoxPlotPanel {
  label: Ramo;
  ticks: PoliticsChartTick[];
  items: PoliticsBoxPlotItem[];
}

export interface PoliticsBoxPlotResultChart {
  kind: 'boxplot';
  id: string;
  title: string;
  xLabel: string;
  panels: PoliticsBoxPlotPanel[];
}

export interface PoliticsScatterPoint {
  x: number;
  y: number;
  xPercent: number;
  yPercent: number;
}

export interface PoliticsScatterPanel {
  label: string;
  color: string;
  xTicks: PoliticsChartTick[];
  yTicks: PoliticsChartTick[];
  points: PoliticsScatterPoint[];
}

export interface PoliticsScatterResultChart {
  kind: 'scatter';
  id: string;
  title: string;
  xLabel: string;
  yLabel: string;
  panels: PoliticsScatterPanel[];
}

export interface PoliticsSpineCell {
  seats: number;
  count: number;
  label: string;
  color: string;
  yPercent: number;
  heightPercent: number;
}

export interface PoliticsSpineBin {
  label: string;
  xPercent: number;
  widthPercent: number;
  cells: PoliticsSpineCell[];
}

export interface PoliticsSpinePanel {
  label: string;
  bins: PoliticsSpineBin[];
}

export interface PoliticsSpineResultChart {
  kind: 'spine';
  id: string;
  title: string;
  xLabel: string;
  panels: PoliticsSpinePanel[];
}

export type PoliticsResultChart =
  | PoliticsBarResultChart
  | PoliticsBoxPlotResultChart
  | PoliticsScatterResultChart
  | PoliticsSpineResultChart;

export interface PoliticsPlurinominalChartOption {
  id: string;
  ramo: Ramo;
  list: string;
  circoscrizione: string;
  collegioPlurinominale: string;
  label: string;
}

type NumericRow = Record<string, string | number | boolean | null>;

interface SeatVoteRow {
  ramo: Ramo;
  label: string;
  voteShare: number;
  seats: number;
}

interface PlurinominalSeatVoteRow extends SeatVoteRow {
  circoscrizione: string;
  collegioPlurinominale: string;
}

const fallbackColor = '#5f7f92';
const chartRami: Ramo[] = ['camera', 'senato'];

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

function coalitionColorMap(scenario: Scenario): Map<string, string> {
  return new Map(scenario.coalitions.map((coalition) => [coalition.name, safeColor(coalition.color)]));
}

function listOrderMap(scenario: Scenario): Map<string, number> {
  return new Map(scenario.lists.map((list, index) => [list.name, index]));
}

function coalitionOrderMap(scenario: Scenario): Map<string, number> {
  return new Map(scenario.coalitions.map((coalition, index) => [coalition.name, index]));
}

function subjectColor(label: string, scenario: Scenario): string {
  return listColorMap(scenario).get(label) ?? coalitionColorMap(scenario).get(label) ?? fallbackColor;
}

function formatNumber(value: number, suffix = ''): string {
  const rounded = Number(value.toFixed(2));
  return `${rounded}${suffix}`;
}

function formatPercent(value: number): string {
  return formatNumber(value, '%');
}

function round(value: number, digits = 2): number {
  return Number(value.toFixed(digits));
}

function quantile(values: readonly number[], probability: number): number {
  if (values.length === 0) return 0;

  const sorted = [...values].sort((left, right) => left - right);
  const index = (sorted.length - 1) * probability;
  const lowerIndex = Math.floor(index);
  const upperIndex = Math.ceil(index);
  if (lowerIndex === upperIndex) return sorted[lowerIndex];

  const fraction = index - lowerIndex;
  return sorted[lowerIndex] * (1 - fraction) + sorted[upperIndex] * fraction;
}

function scalePercent(value: number, min: number, max: number): number {
  if (max <= min) return 50;
  return ((value - min) / (max - min)) * 100;
}

function axisBounds(values: readonly number[], includeZero: boolean): { min: number; max: number } {
  if (values.length === 0) return { min: 0, max: 1 };

  let min = Math.min(...values);
  let max = Math.max(...values);
  if (includeZero) min = Math.min(0, min);

  if (min === max) {
    const padding = Math.max(1, Math.abs(max) * 0.1);
    min -= padding;
    max += padding;
  }

  return {
    min: Math.max(0, Math.floor(min)),
    max: Math.ceil(max)
  };
}

function ticksForBounds(min: number, max: number, count = 4, suffix = ''): PoliticsChartTick[] {
  if (max <= min) {
    return [{ value: min, label: formatNumber(min, suffix), positionPercent: 50 }];
  }

  return Array.from({ length: count }, (_, index) => {
    const value = min + ((max - min) * index) / (count - 1);
    return {
      value: round(value),
      label: formatNumber(value, suffix),
      positionPercent: scalePercent(value, min, max)
    };
  });
}

function sortLabels(labels: Iterable<string>, order: ReadonlyMap<string, number>): string[] {
  return [...labels].sort((left, right) => {
    const leftOrder = order.get(left) ?? Number.MAX_SAFE_INTEGER;
    const rightOrder = order.get(right) ?? Number.MAX_SAFE_INTEGER;
    if (leftOrder !== rightOrder) return leftOrder - rightOrder;
    return left.localeCompare(right);
  });
}

function buildGroupedBars(params: {
  table: ResultTable;
  labelColumn: string;
  valueColumn: string;
  colors: ReadonlyMap<string, string>;
  order: ReadonlyMap<string, number>;
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

  return chartRami
    .map((ramo) => {
      const rows = byRamo.get(ramo) ?? [];
      const sorted = [...rows].sort((left, right) => {
        const valueDiff = asNumber(right[params.valueColumn]) - asNumber(left[params.valueColumn]);
        if (valueDiff !== 0) return valueDiff;

        const leftLabel = asString(left[params.labelColumn]);
        const rightLabel = asString(right[params.labelColumn]);
        return (params.order.get(leftLabel) ?? 9999) - (params.order.get(rightLabel) ?? 9999);
      });
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

function seatVoteRows(
  table: ResultTable | undefined,
  labelColumn: string,
  voteShareColumn: string,
  seatsColumn: string
): SeatVoteRow[] {
  if (!table) return [];

  return table.rows
    .map((row) => {
      const ramo = asString(row.Ramo);
      if (ramo !== 'camera' && ramo !== 'senato') return null;

      return {
        ramo,
        label: asString(row[labelColumn]),
        voteShare: asNumber(row[voteShareColumn]),
        seats: asNumber(row[seatsColumn])
      };
    })
    .filter((row): row is SeatVoteRow => row !== null && row.label.length > 0);
}

function plurinominalSeatVoteRows(table: ResultTable | undefined): PlurinominalSeatVoteRow[] {
  if (!table) return [];

  return table.rows
    .map((row) => {
      const ramo = asString(row.Ramo);
      if (ramo !== 'camera' && ramo !== 'senato') return null;

      return {
        ramo,
        label: asString(row.Lista),
        circoscrizione: asString(row.Circoscrizione),
        collegioPlurinominale: asString(row['Collegio pluri']),
        voteShare: asNumber(row['Percentuale %']),
        seats: asNumber(row['Numero max'])
      };
    })
    .filter((row): row is PlurinominalSeatVoteRow => row !== null && row.label.length > 0);
}

function rowsByRamoAndLabel(rows: readonly SeatVoteRow[]): Map<Ramo, Map<string, SeatVoteRow[]>> {
  const result = new Map<Ramo, Map<string, SeatVoteRow[]>>();

  for (const row of rows) {
    const byLabel = result.get(row.ramo) ?? new Map<string, SeatVoteRow[]>();
    const group = byLabel.get(row.label) ?? [];
    group.push(row);
    byLabel.set(row.label, group);
    result.set(row.ramo, byLabel);
  }

  return result;
}

function boxStats(values: readonly number[]): Omit<PoliticsBoxPlotItem, 'label' | 'color'> {
  const sorted = [...values].sort((left, right) => left - right);
  const q1 = quantile(sorted, 0.25);
  const median = quantile(sorted, 0.5);
  const q3 = quantile(sorted, 0.75);
  const iqr = q3 - q1;
  const lowerFence = q1 - iqr * 1.5;
  const upperFence = q3 + iqr * 1.5;
  const inlierValues = sorted.filter((value) => value >= lowerFence && value <= upperFence);
  const whiskerLow = inlierValues[0] ?? sorted[0] ?? 0;
  const whiskerHigh = inlierValues[inlierValues.length - 1] ?? sorted[sorted.length - 1] ?? 0;
  const average = sorted.length === 0 ? 0 : sorted.reduce((sum, value) => sum + value, 0) / sorted.length;
  const outliers = sorted.filter((value) => value < whiskerLow || value > whiskerHigh);

  return {
    whiskerLow,
    q1,
    median,
    q3,
    whiskerHigh,
    average,
    whiskerLowPercent: 0,
    q1Percent: 0,
    medianPercent: 0,
    q3Percent: 0,
    whiskerHighPercent: 0,
    averagePercent: 0,
    outlierPercents: outliers
  };
}

function buildVoteShareBoxPlotChart(
  rows: readonly SeatVoteRow[],
  scenario: Scenario
): PoliticsBoxPlotResultChart | null {
  if (rows.length === 0) return null;

  const grouped = rowsByRamoAndLabel(rows);
  const order = listOrderMap(scenario);
  const colors = listColorMap(scenario);
  const panels: PoliticsBoxPlotPanel[] = [];

  for (const ramo of chartRami) {
    const byLabel = grouped.get(ramo);
    if (!byLabel) continue;

    const items = sortLabels(byLabel.keys(), order).map((label) => ({
      label,
      color: colors.get(label) ?? fallbackColor,
      ...boxStats((byLabel.get(label) ?? []).map((row) => row.voteShare))
    }));
    const bounds = axisBounds(
      items.flatMap((item) => [item.whiskerLow, item.whiskerHigh, ...item.outlierPercents]),
      true
    );

    panels.push({
      label: ramo,
      ticks: ticksForBounds(bounds.min, bounds.max, 5, '%'),
      items: items.map((item) => ({
        ...item,
        whiskerLowPercent: scalePercent(item.whiskerLow, bounds.min, bounds.max),
        q1Percent: scalePercent(item.q1, bounds.min, bounds.max),
        medianPercent: scalePercent(item.median, bounds.min, bounds.max),
        q3Percent: scalePercent(item.q3, bounds.min, bounds.max),
        whiskerHighPercent: scalePercent(item.whiskerHigh, bounds.min, bounds.max),
        averagePercent: scalePercent(item.average, bounds.min, bounds.max),
        outlierPercents: item.outlierPercents.map((value) => scalePercent(value, bounds.min, bounds.max))
      }))
    });
  }

  if (panels.length === 0) return null;

  return {
    kind: 'boxplot',
    id: 'vote-share-boxplot',
    title: 'Percentuali simulate per lista',
    xLabel: 'Percentuale sui voti validi',
    panels
  };
}

function buildScatterChart(params: {
  id: string;
  title: string;
  rows: readonly SeatVoteRow[];
  scenario: Scenario;
  order: ReadonlyMap<string, number>;
  colorForLabel: (label: string) => string;
}): PoliticsScatterResultChart | null {
  if (params.rows.length === 0) return null;

  const grouped = rowsByRamoAndLabel(params.rows);
  const panels: PoliticsScatterPanel[] = [];

  for (const ramo of chartRami) {
    const byLabel = grouped.get(ramo);
    if (!byLabel) continue;

    for (const label of sortLabels(byLabel.keys(), params.order)) {
      const rows = byLabel.get(label) ?? [];
      const xBounds = axisBounds(rows.map((row) => row.voteShare), true);
      const yBounds = axisBounds(rows.map((row) => row.seats), true);

      panels.push({
        label: `${ramo} - ${label}`,
        color: params.colorForLabel(label),
        xTicks: ticksForBounds(xBounds.min, xBounds.max, 4, '%'),
        yTicks: ticksForBounds(yBounds.min, yBounds.max, 4),
        points: rows.map((row) => ({
          x: row.voteShare,
          y: row.seats,
          xPercent: scalePercent(row.voteShare, xBounds.min, xBounds.max),
          yPercent: scalePercent(row.seats, yBounds.min, yBounds.max)
        }))
      });
    }
  }

  if (panels.length === 0) return null;

  return {
    kind: 'scatter',
    id: params.id,
    title: params.title,
    xLabel: 'Percentuale sui voti validi',
    yLabel: 'Eletti',
    panels
  };
}

function parseHexColor(color: string): [number, number, number] {
  const safe = safeColor(color).slice(1);
  return [0, 2, 4].map((index) => Number.parseInt(safe.slice(index, index + 2), 16)) as [number, number, number];
}

function toHex(value: number): string {
  return Math.round(Math.max(0, Math.min(255, value)))
    .toString(16)
    .padStart(2, '0');
}

function mixColor(left: string, right: string, amount: number): string {
  const l = parseHexColor(left);
  const r = parseHexColor(right);
  const mixed = l.map((value, index) => value + (r[index] - value) * amount);
  return `#${mixed.map(toHex).join('')}`;
}

function spineColor(baseColor: string, index: number, total: number): string {
  if (total <= 1) return mixColor(baseColor, '#ffffff', 0.35);
  const amount = index / (total - 1);
  return amount < 0.5
    ? mixColor('#000000', baseColor, amount * 2)
    : mixColor(baseColor, '#ffffff', (amount - 0.5) * 2);
}

function binBoundaries(values: readonly number[]): number[] {
  if (values.length === 0) return [0, 1];

  const rounded = Array.from(
    new Set(Array.from({ length: 11 }, (_, index) => round(quantile(values, index / 10), 2)))
  ).sort((left, right) => left - right);

  if (rounded.length >= 2) return rounded;

  const value = rounded[0];
  return [Math.max(0, value - 0.5), value + 0.5];
}

function binIndexForValue(value: number, boundaries: readonly number[]): number {
  for (let index = 0; index < boundaries.length - 1; index += 1) {
    const isLast = index === boundaries.length - 2;
    if (value >= boundaries[index] && (value < boundaries[index + 1] || (isLast && value <= boundaries[index + 1]))) {
      return index;
    }
  }
  return Math.max(0, boundaries.length - 2);
}

function buildSpinePanel(label: string, rows: readonly SeatVoteRow[], color: string): PoliticsSpinePanel {
  const total = Math.max(1, rows.length);
  const boundaries = binBoundaries(rows.map((row) => row.voteShare));
  const seatValues = [...new Set(rows.map((row) => row.seats))].sort((left, right) => left - right);
  const seatIndex = new Map(seatValues.map((seats, index) => [seats, index]));
  const binRows = Array.from({ length: boundaries.length - 1 }, () => [] as SeatVoteRow[]);

  for (const row of rows) {
    binRows[binIndexForValue(row.voteShare, boundaries)].push(row);
  }

  let xCursor = 0;
  const bins = binRows
    .map((rowsInBin, index) => {
      const widthPercent = (rowsInBin.length / total) * 100;
      const countBySeats = new Map<number, number>();
      for (const row of rowsInBin) countBySeats.set(row.seats, (countBySeats.get(row.seats) ?? 0) + 1);

      let yCursor = 100;
      const cells = seatValues
        .filter((seats) => (countBySeats.get(seats) ?? 0) > 0)
        .map((seats) => {
          const count = countBySeats.get(seats) ?? 0;
          const heightPercent = rowsInBin.length === 0 ? 0 : (count / rowsInBin.length) * 100;
          yCursor -= heightPercent;
          return {
            seats,
            count,
            label: count > total * 0.005 ? String(seats) : '',
            color: spineColor(color, seatIndex.get(seats) ?? 0, seatValues.length),
            yPercent: yCursor,
            heightPercent
          };
        });

      const bin = {
        label: `${formatPercent(boundaries[index])}-${formatPercent(boundaries[index + 1])}`,
        xPercent: xCursor,
        widthPercent,
        cells
      };
      xCursor += widthPercent;
      return bin;
    })
    .filter((bin) => bin.widthPercent > 0);

  return {
    label,
    bins
  };
}

function buildSpineChart(params: {
  id: string;
  title: string;
  rows: readonly SeatVoteRow[];
  order: ReadonlyMap<string, number>;
  colorForLabel: (label: string) => string;
}): PoliticsSpineResultChart | null {
  if (params.rows.length === 0) return null;

  const grouped = rowsByRamoAndLabel(params.rows);
  const panels: PoliticsSpinePanel[] = [];

  for (const ramo of chartRami) {
    const byLabel = grouped.get(ramo);
    if (!byLabel) continue;

    for (const label of sortLabels(byLabel.keys(), params.order)) {
      panels.push(buildSpinePanel(`${ramo} - ${label}`, byLabel.get(label) ?? [], params.colorForLabel(label)));
    }
  }

  if (panels.length === 0) return null;

  return {
    kind: 'spine',
    id: params.id,
    title: params.title,
    xLabel: 'Percentuale sui voti validi',
    panels
  };
}

export function buildPoliticsPlurinominalChartOptions(
  tables: readonly ResultTable[],
  scenario: Scenario
): PoliticsPlurinominalChartOption[] {
  const rows = plurinominalSeatVoteRows(tableByName(tables, politicsPlurinominalSeatVotePlotTableName));
  const order = listOrderMap(scenario);
  const optionsById = new Map<string, { option: PoliticsPlurinominalChartOption; maxSeats: number }>();

  for (const row of rows) {
    const id = `${row.ramo}\u001f${row.label}\u001f${row.circoscrizione}\u001f${row.collegioPlurinominale}`;
    const existing = optionsById.get(id);
    if (existing) {
      existing.maxSeats = Math.max(existing.maxSeats, row.seats);
      continue;
    }

    optionsById.set(id, {
      option: {
        id,
        ramo: row.ramo,
        list: row.label,
        circoscrizione: row.circoscrizione,
        collegioPlurinominale: row.collegioPlurinominale,
        label: `${row.ramo} - ${row.label} - ${row.collegioPlurinominale}`
      },
      maxSeats: row.seats
    });
  }

  return [...optionsById.values()]
    .filter((entry) => entry.maxSeats > 0)
    .map((entry) => entry.option)
    .sort((left, right) => {
      const listDiff = (order.get(left.list) ?? 9999) - (order.get(right.list) ?? 9999);
      if (listDiff !== 0) return listDiff;
      const ramoDiff = left.ramo.localeCompare(right.ramo);
      if (ramoDiff !== 0) return ramoDiff;
      const circDiff = left.circoscrizione.localeCompare(right.circoscrizione);
      if (circDiff !== 0) return circDiff;
      return left.collegioPlurinominale.localeCompare(right.collegioPlurinominale);
    });
}

export function buildPoliticsPlurinominalChart(
  tables: readonly ResultTable[],
  scenario: Scenario,
  option: PoliticsPlurinominalChartOption | null
): PoliticsSpineResultChart | null {
  if (!option) return null;

  const rows = plurinominalSeatVoteRows(tableByName(tables, politicsPlurinominalSeatVotePlotTableName)).filter(
    (row) =>
      row.ramo === option.ramo &&
      row.label === option.list &&
      row.circoscrizione === option.circoscrizione &&
      row.collegioPlurinominale === option.collegioPlurinominale
  );
  if (rows.length === 0) return null;

  return {
    kind: 'spine',
    id: 'plurinominal-college-spine',
    title: 'Eletti nel collegio plurinominale',
    xLabel: 'Percentuale sui voti validi',
    panels: [buildSpinePanel(option.label, rows, listColorMap(scenario).get(option.list) ?? fallbackColor)]
  };
}

export function buildPoliticsResultCharts(tables: readonly ResultTable[], scenario: Scenario): PoliticsResultChart[] {
  const listColors = listColorMap(scenario);
  const listOrder = listOrderMap(scenario);
  const coalitionColors = coalitionColorMap(scenario);
  const coalitionOrder = coalitionOrderMap(scenario);
  const charts: Array<PoliticsResultChart | null> = [];
  const plurinominalSeats = tableByName(tables, 'Average plurinominal seats by list');
  const voteShares = tableByName(tables, 'Vote share by list');
  const listSeatVoteRows = seatVoteRows(
    tableByName(tables, politicsListSeatVotePlotTableName),
    'Lista',
    'Percentuale %',
    'Seggi'
  );
  const coalitionRows = seatVoteRows(
    tableByName(tables, politicsCoalitionSeatVotePlotTableName),
    'Soggetto',
    'Percentuale liste %',
    'Seggi'
  );

  if (plurinominalSeats) {
    charts.push({
      kind: 'bar',
      id: 'plurinominal-seats-by-list',
      title: 'Seggi plurinominali medi',
      groups: buildGroupedBars({
        table: plurinominalSeats,
        labelColumn: 'Lista',
        valueColumn: 'Media',
        colors: listColors,
        order: listOrder
      })
    });
  }

  if (voteShares) {
    charts.push({
      kind: 'bar',
      id: 'vote-share-by-list',
      title: 'Percentuali medie sui voti validi',
      groups: buildGroupedBars({
        table: voteShares,
        labelColumn: 'Lista',
        valueColumn: 'Media %',
        colors: listColors,
        order: listOrder,
        valueSuffix: '%'
      })
    });
  }

  charts.push(buildVoteShareBoxPlotChart(listSeatVoteRows, scenario));
  charts.push(
    buildScatterChart({
      id: 'seat-vote-scatter-by-list',
      title: 'Eletti e percentuali per lista',
      rows: listSeatVoteRows,
      scenario,
      order: listOrder,
      colorForLabel: (label) => listColors.get(label) ?? fallbackColor
    })
  );
  charts.push(
    buildScatterChart({
      id: 'seat-vote-scatter-by-coalition',
      title: 'Eletti e percentuali per coalizione',
      rows: coalitionRows,
      scenario,
      order: coalitionOrder,
      colorForLabel: (label) => coalitionColors.get(label) ?? subjectColor(label, scenario)
    })
  );
  charts.push(
    buildSpineChart({
      id: 'seat-vote-spine-by-list',
      title: 'Spinogrammi degli eletti',
      rows: listSeatVoteRows,
      order: listOrder,
      colorForLabel: (label) => listColors.get(label) ?? fallbackColor
    })
  );

  return charts.filter((chart): chart is PoliticsResultChart => {
    if (!chart) return false;
    if (chart.kind === 'bar') return chart.groups.some((group) => group.bars.length > 0);
    return chart.panels.length > 0;
  });
}
