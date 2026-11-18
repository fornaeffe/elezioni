import type { Distribution, SeriesSummary } from '../results/statistics';
import type { BoxPlotResultChart, BoxPlotItem } from '../results/chart-types';
import { boxPlotItem, chartTicks } from '../results/chart-builders';

export function buildPoliticsBoxplots(
  distributions: { summary: SeriesSummary; distribution: Distribution }[],
  requested: number,
) {
  const bySeries = new Map<string, { distribution: Distribution; max: number }>();
  const charts: BoxPlotResultChart[] = [];
  for (const kind of ['list', 'subject']) {
    const chart: BoxPlotResultChart = {
      kind: 'boxplot',
      id: `reform-vote-boxplot-${kind}`,
      title:
        kind === 'list'
          ? 'Percentuali simulate per lista'
          : 'Percentuali simulate per coalizione / lista singola',
      xLabel: 'Percentuale sui voti validi · distribuzione delle osservazioni disponibili',
      panels: [],
    };
    for (const chamber of ['camera', 'senato'] as const) {
      const entries = distributions.filter((e) => e.summary.chamber === chamber && e.summary.kind === kind);
      if (!entries.length) continue;
      const max = Math.max(1, Math.ceil(Math.max(...entries.map((e) => e.distribution.max ?? 0))));
      const items: BoxPlotItem[] = [];
      for (const e of entries) {
        const item = boxPlotItem(e.summary, e.distribution, requested, max);
        if (item) {
          items.push(item);
          bySeries.set(e.summary.id, { distribution: e.distribution, max });
        }
      }
      chart.panels.push({ label: chamber, ticks: chartTicks(0, max, '%'), items });
    }
    if (chart.panels.length) charts.push(chart);
  }
  return { charts, bySeries };
}
