<script lang="ts">
  import { onMount } from 'svelte';
  import type { ResultServiceClient } from '../../core/result-service-client';
  import type { BonusFilter, SeriesDescriptor, SeriesStatistics } from '../../results/statistics';
  import { spineChart } from '../../results/chart-builders';
  import ResultCharts from '../../results/ResultCharts.svelte';
  import StreamedPlot from '../../results/StreamedPlot.svelte';
  let { client, resultId, owner, descriptor, variant, measure = 'elected', collapsible = true, bonusFilter = 'all' }: {
    client: ResultServiceClient; resultId: string; owner: string; descriptor: SeriesDescriptor;
    variant: 'scatter' | 'spine'; measure?: 'elected' | 'entitled'; collapsible?: boolean; bonusFilter?: BonusFilter;
  } = $props();
  let element: HTMLDivElement;
  let statistics = $state.raw<SeriesStatistics | null>(null), error = $state('');
  onMount(() => {
    let alive = true;
    const observer = new IntersectionObserver(entries => {
      if (!entries.some(e => e.isIntersecting)) return;
      observer.disconnect();
      void client.call<SeriesStatistics>('statistics-series', { resultId, owner, series: descriptor.id, measure, bonusFilter })
        .then(s => { if (alive) statistics = s; }).catch(e => { if (alive) error = String(e); });
    }, { rootMargin: '200px' });
    observer.observe(element); return () => { alive = false; observer.disconnect(); };
  });
</script>

<div bind:this={element} class="series-panel" aria-label={`${descriptor.chamber} · ${descriptor.name}`}>
  {#if statistics}
    {@const s = statistics}
    {#if descriptor.kind === 'college'}<p class="coverage">Simulazioni nel grafico: {s.distribution.count} · Simulazioni selezionate: {s.requested}</p>{/if}
    {#if variant === 'scatter'}
      <h4>{descriptor.chamber} · {descriptor.name}</h4>
      <StreamedPlot {client} {resultId} {owner} series={descriptor.id} label={`${descriptor.chamber} · ${descriptor.name}`} color={descriptor.color}
        maxX={Math.max(1, Math.ceil(s.distribution.max ?? 1))} maxY={Math.max(1, s.yMax)} {measure} />
    {:else if s.distribution.count}
      <ResultCharts charts={[spineChart(s, `${descriptor.chamber} · ${descriptor.name}`)]} initialOpen={true} {collapsible} />
    {:else}
      <p role="status">Nessun dato disponibile per il filtro selezionato.</p>
    {/if}
    <p class="coverage">{s.known}/{s.requested} coppie note · {s.missingY} esiti ignoti con percentuale nota · {s.missingX} percentuali non disponibili. Il grigio (?) indica esiti ignoti.</p>
  {:else if error}<p role="alert">{error}</p>
  {:else}<p role="status">{descriptor.chamber} · {descriptor.name} — caricamento…</p>{/if}
</div>

<style>
  .series-panel { min-height: 5rem; min-width: 0; }
  h4 { font-size: .85rem; margin: .75rem 0; color: #697681; }
  .coverage { font-size: .8rem; color: #697681; padding: 0 1rem; }
</style>
