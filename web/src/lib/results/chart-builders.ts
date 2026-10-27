import type { BoxPlotItem, ChartTick, SpineResultChart } from './chart-types';
import type { Distribution, SeriesDescriptor, SeriesStatistics } from './statistics';
import { readableTextColor, spineColor } from './chart-colors';

export const percentPosition = (value: number, min: number, max: number) => max > min ? (value - min) / (max - min) * 100 : 50;
export const displayNumber = (value: number | null, digits = 2) => value === null ? 'Non disponibile' : value.toLocaleString('it-IT', { maximumFractionDigits: digits });
export function chartTicks(min: number, max: number, suffix = '', count = 5): ChartTick[] {
  return Array.from({ length: count }, (_, i) => {
    const value = min + (max - min) * i / (count - 1);
    return { value, label: `${displayNumber(value)}${suffix}`, positionPercent: i / (count - 1) * 100 };
  });
}
export function boxPlotItem(d: SeriesDescriptor, s: Distribution, requested: number, max: number): BoxPlotItem | null {
  if (!s.count) return null;
  const pct = (n: number | null) => percentPosition(n!, 0, max);
  return { label: `${d.name} (${s.count}/${requested})`, color: d.color, streamId: d.id, whiskerLow: s.whiskerLow!, whiskerHigh: s.whiskerHigh!,
    q1: s.q1!, median: s.median!, q3: s.q3!, average: s.mean!, whiskerLowPercent: pct(s.whiskerLow), whiskerHighPercent: pct(s.whiskerHigh),
    q1Percent: pct(s.q1), medianPercent: pct(s.median), q3Percent: pct(s.q3), averagePercent: pct(s.mean), outlierPercents: [] };
}
export function spineChart(s: SeriesStatistics, title: string): SpineResultChart {
  const total = s.distribution.count, values = [...new Set(s.bins.flatMap(b => b.outcomes.flatMap(o => o.seats === null ? [] : [o.seats])))].sort((a, b) => a - b);
  let x = 0;
  const ticks: ChartTick[] = s.bins.length ? [{ value: s.bins[0].low, label: `${displayNumber(s.bins[0].low)}%`, positionPercent: 0 }] : [];
  const bins = s.bins.filter(b => b.count).map(b => {
    const width = b.count / total * 100; let y = 100;
    const cells = b.outcomes.map(o => {
      const height = o.count / b.count * 100; y -= height;
      const color = o.seats === null ? '#9ca3af' : spineColor(s.descriptor.color, values.indexOf(o.seats), values.length);
      return { seats: o.seats ?? -1, count: o.count, label: o.seats === null ? '?' : String(o.seats), color, textColor: readableTextColor(color), yPercent: y, heightPercent: height };
    });
    const bin = { label: `${displayNumber(b.low)}–${displayNumber(b.high)}% · ${b.count}/${s.requested} richieste`, xPercent: x, widthPercent: width, cells };
    x += width; ticks.push({ value: b.high, label: `${displayNumber(b.high)}%`, positionPercent: x }); return bin;
  });
  const shareLabel = s.xDescriptor && s.xDescriptor.id !== s.descriptor.id ? ` · ${s.xDescriptor.chamber} · ${s.xDescriptor.name}` : '';
  return { kind: 'spine', id: `spine-${s.descriptor.id}`, title, xLabel: `Percentuale sui voti validi${shareLabel} · larghezza proporzionale alle osservazioni con percentuale nota`,
    panels: [{ label: `${s.descriptor.chamber} · ${s.descriptor.name}`, ticks, bins }] };
}
