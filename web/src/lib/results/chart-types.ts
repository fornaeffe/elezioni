import type { Ramo } from '../politics/types';

export interface ResultChartBar {
  label: string;
  value: number;
  displayValue: string;
  color: string;
  widthPercent: number;
}

export interface ResultChartGroup {
  label: Ramo;
  bars: ResultChartBar[];
}

export interface ChartTick {
  value: number;
  label: string;
  positionPercent: number;
}

export interface BarResultChart {
  kind: 'bar';
  id: string;
  title: string;
  groups: ResultChartGroup[];
}

export interface BoxPlotItem {
  streamId?: string;
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

export interface BoxPlotPanel {
  label: Ramo;
  ticks: ChartTick[];
  items: BoxPlotItem[];
}

export interface BoxPlotResultChart {
  kind: 'boxplot';
  id: string;
  title: string;
  xLabel: string;
  panels: BoxPlotPanel[];
}

export interface ScatterPoint {
  x: number;
  y: number;
  xPercent: number;
  yPercent: number;
}

export interface ScatterPanel {
  label: string;
  color: string;
  xTicks: ChartTick[];
  yTicks: ChartTick[];
  points: ScatterPoint[];
}

export interface ScatterResultChart {
  kind: 'scatter';
  id: string;
  title: string;
  xLabel: string;
  yLabel: string;
  panels: ScatterPanel[];
}

export interface SpineCell {
  seats: number;
  count: number;
  label: string;
  color: string;
  textColor: string;
  yPercent: number;
  heightPercent: number;
}

export interface SpineBin {
  label: string;
  xPercent: number;
  widthPercent: number;
  cells: SpineCell[];
}

export interface SpinePanel {
  label: string;
  ticks: ChartTick[];
  bins: SpineBin[];
}

export interface SpineResultChart {
  kind: 'spine';
  id: string;
  title: string;
  xLabel: string;
  panels: SpinePanel[];
}

export type ResultChart =
  | BarResultChart
  | BoxPlotResultChart
  | ScatterResultChart
  | SpineResultChart;

export interface CollegeChartOption {
  id: string;
  ramo: Ramo;
  list: string;
  circoscrizione: string;
  collegioPlurinominale: string;
  label: string;
}

