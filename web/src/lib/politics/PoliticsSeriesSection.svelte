<script lang="ts">
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { SeriesSummary } from '../results/statistics';
  import ResultSection from '../results/ResultSection.svelte';
  import ReformSeriesChart from './reform-2026/ReformSeriesChart.svelte';

  let {
    client,
    resultId,
    owner,
    series,
    variant,
  }: {
    client: ResultServiceClient;
    resultId: string;
    owner: string;
    series: SeriesSummary[];
    variant: 'scatter' | 'spine';
  } = $props();
</script>

<ResultSection title={variant === 'scatter' ? 'Eletti e percentuali' : 'Spinogrammi degli eletti'}>
  {#snippet children(open)}
    {#if open}<ReformSeriesChart {client} {resultId} {owner} {series} {variant} />{/if}
  {/snippet}
</ResultSection>
