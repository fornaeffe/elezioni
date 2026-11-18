<script lang="ts">
  import type { ResultServiceClient } from '../../core/result-service-client';
  import type { SeriesSummary } from '../../results/statistics';
  import ResultSection from '../../results/ResultSection.svelte';
  import ElectionProbabilityTable from './ElectionProbabilityTable.svelte';

  let {
    client,
    resultId,
    owner,
    series,
  }: {
    client: ResultServiceClient;
    resultId: string;
    owner: string;
    series: SeriesSummary[];
  } = $props();
</script>

<ResultSection title="Probabilità di elezione">
  {#snippet children(open)}
    {#if open}<ElectionProbabilityTable {client} {resultId} {owner} {series} />{/if}
  {/snippet}
</ResultSection>
