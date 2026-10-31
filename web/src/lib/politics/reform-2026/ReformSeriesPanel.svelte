<script lang="ts">
  import { onMount } from 'svelte';
  import type { ResultServiceClient } from '../../core/result-service-client';
  import type { BonusFilter, SeatScope, CollegeMeasure, SeriesDescriptor, SeriesStatistics } from '../../results/statistics';
  import { spineChart } from '../../results/chart-builders';
  import ResultCharts from '../../results/ResultCharts.svelte';
  import StreamedPlot from '../../results/StreamedPlot.svelte';
  let { client, resultId, owner, descriptor, variant, measure = 'elected', collapsible = true, bonusFilter = 'all', seatScope = 'ordinary-and-bonus', eager = false }: {
    client: ResultServiceClient; resultId: string; owner: string; descriptor: SeriesDescriptor;
    variant: 'scatter' | 'spine'; measure?: CollegeMeasure; collapsible?: boolean; bonusFilter?: BonusFilter;
    seatScope?: SeatScope; eager?: boolean;
  } = $props();
  let element: HTMLDivElement;
  let statistics = $state.raw<SeriesStatistics | null>(null), error = $state('');
  onMount(() => {
    let alive = true;
    const load = () => {
      void client.call<SeriesStatistics>('statistics-series', { resultId, owner, series: descriptor.id, measure, bonusFilter, seatScope })
        .then(s => { if (alive) statistics = s; }).catch(e => { if (alive) error = String(e); });
    };
    // An explicitly opened territorial view requests its chosen data immediately.
    // Other chart families still defer their queries until near the viewport.
    if (eager) { load(); return () => { alive = false; }; }
    const observer = new IntersectionObserver(entries => {
      if (!entries.some(e => e.isIntersecting)) return;
      observer.disconnect(); load();
    }, { rootMargin: '200px' });
    observer.observe(element); return () => { alive = false; observer.disconnect(); };
  });
</script>

<div bind:this={element} class="series-panel" aria-label={`${descriptor.chamber} · ${descriptor.name}`}>
  {#if statistics}
    {@const s = statistics}
    {#if s.unavailableReason}<p role="status">{s.unavailableReason}</p>
    {:else}
      {#if descriptor.kind === 'college' || descriptor.kind.startsWith('constituency-')}<p class="coverage">Simulazioni nel grafico: {s.distribution.count} · Simulazioni selezionate: {s.requested}</p>{/if}
      {#if variant === 'scatter' && measure !== 'last-position'}
        <h4>{descriptor.chamber} · {descriptor.name}</h4>
        <StreamedPlot {client} {resultId} {owner} series={descriptor.id} label={`${descriptor.chamber} · ${descriptor.name}`} color={descriptor.color}
          maxX={Math.max(1, Math.ceil(s.distribution.max ?? 1))} maxY={Math.max(1, s.yMax)} {measure} />
      {:else if s.distribution.count}
        <ResultCharts charts={[spineChart(s, `${measure === 'last-position' ? 'Ultima posizione eletta · ' : measure === 'entitled' ? descriptor.kind.startsWith('constituency-') ? 'Seggi ordinari spettanti · ' : 'Seggi spettanti · ' : descriptor.kind.startsWith('constituency-') ? seatScope === 'ordinary' ? 'Eletti ordinari · ' : 'Eletti ordinari + premio · ' : ''}${descriptor.chamber} · ${descriptor.name}`)]} initialOpen={true} {collapsible} />
      {:else}
        <p role="status">Nessun dato disponibile per il filtro selezionato.</p>
      {/if}
      <p class="coverage">{s.known}/{s.requested} coppie note · {s.missingY} esiti ignoti con percentuale nota · {s.missingX} percentuali non disponibili. Il grigio (?) indica esiti ignoti.</p>
    {/if}
  {:else if error}<p role="alert">{error}</p>
  {:else}<p role="status">{descriptor.chamber} · {descriptor.name} — caricamento…</p>{/if}
</div>

<style>
  .series-panel { min-height: 5rem; min-width: 0; }
  h4 { font-size: .85rem; margin: .75rem 0; color: #697681; }
  .coverage { font-size: .8rem; color: #697681; padding: 0 1rem; }
</style>
