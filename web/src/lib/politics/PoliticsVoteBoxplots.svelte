<script lang="ts">
  import { onMount } from 'svelte';
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { Distribution, SeriesSummary, StatisticsOverview } from '../results/statistics';
  import type { BoxPlotResultChart } from '../results/chart-types';
  import ResultSection from '../results/ResultSection.svelte';
  import ResultCharts from '../results/ResultCharts.svelte';
  import StreamedPlot from '../results/StreamedPlot.svelte';
  import { buildPoliticsBoxplots } from './result-boxplots';

  let {
    client,
    resultId,
    owner,
    overview,
  }: {
    client: ResultServiceClient;
    resultId: string;
    owner: string;
    overview: StatisticsOverview;
  } = $props();
  let boxes = $state.raw<BoxPlotResultChart[]>([]);
  let boxBySeries = $state.raw(new Map<string, { distribution: Distribution; max: number }>());
  let boxKind = $state('list'),
    boxChamber = $state<'camera' | 'senato'>('camera');
  let error = $state('');
  const selectedBoxes = $derived(
    boxes
      .filter((chart) => chart.id.endsWith(boxKind))
      .map((chart) => ({ ...chart, panels: chart.panels.filter((panel) => panel.label === boxChamber) })),
  );

  onMount(() => {
    let alive = true;
    async function load() {
      try {
        const distributions: { summary: SeriesSummary; distribution: Distribution }[] = [];
        for (const summary of overview.series.filter(
          (s) => (s.kind === 'list' || s.kind === 'subject') && s.votesKnown,
        )) {
          const distribution = await client.call<Distribution>('statistics-box', {
            resultId,
            owner,
            series: summary.id,
          });
          if (!alive) return;
          distributions.push({ summary, distribution });
        }
        if (!alive) return;
        const built = buildPoliticsBoxplots(distributions, overview.requested);
        boxBySeries = built.bySeries;
        boxes = built.charts;
      } catch (e) {
        if (alive) error = String(e);
      }
    }
    void load();
    return () => {
      alive = false;
    };
  });
</script>

{#if error}<p role="alert">{error}</p>{/if}
{#if boxes.length}
  <ResultSection
    title={boxKind === 'list'
      ? 'Percentuali simulate per lista'
      : 'Percentuali simulate per coalizione / lista singola'}
  >
    {#snippet children(open)}
      {#if open}
        <label>
          Raggruppamento dei boxplot
          <select bind:value={boxKind}>
            <option value="list">Liste</option>
            <option value="subject">Coalizioni / liste singole</option>
          </select>
        </label>
        <label>
          Ramo dei boxplot
          <select bind:value={boxChamber}>
            <option value="camera">Camera</option>
            <option value="senato">Senato</option>
          </select>
        </label>
        {#key boxChamber + boxKind}
          <ResultCharts charts={selectedBoxes} collapsible={false}>
            {#snippet outliers(item)}
              {@const b = boxBySeries.get(item.streamId!)!}
              <StreamedPlot
                {client}
                {resultId}
                {owner}
                series={item.streamId!}
                label={`Valori anomali ${item.label}`}
                color={item.color}
                variant="outliers"
                maxX={b.max}
                whiskerLow={b.distribution.whiskerLow!}
                whiskerHigh={b.distribution.whiskerHigh!}
              />
            {/snippet}
          </ResultCharts>
          {#if !selectedBoxes[0]?.panels.length}<p role="status">
              Nessuna percentuale disponibile per questo ramo.
            </p>{/if}
        {/key}
      {/if}
    {/snippet}
  </ResultSection>
{/if}

<style>
  label {
    display: grid;
    gap: 0.5rem;
    margin: 1rem 0;
  }
  select {
    box-sizing: border-box;
    width: 100%;
    min-width: 0;
    font: inherit;
    padding: 0.5rem;
  }
  p {
    font-size: 0.85rem;
    line-height: 1.5;
    color: #4d5963;
  }
</style>
