<script lang="ts">
  import type { ResultServiceClient } from '../../core/result-service-client';
  import type { SeriesSummary } from '../../results/statistics';
  import ReformSeriesPanel from './ReformSeriesPanel.svelte';

  let { client, resultId, owner, series, variant }: {
    client: ResultServiceClient; resultId: string; owner: string; series: SeriesSummary[];
    variant: 'scatter' | 'spine';
  } = $props();

  let chamber = $state<'camera' | 'senato'>('camera');
  let kind = $state<'list' | 'subject'>('list');
  let preferredSeries = $state('');
  const options = $derived(series.filter(s => s.chamber === chamber && s.kind === kind && s.votesKnown));
  // A chamber/group change always resolves to an available series, even in sparse archives.
  const selected = $derived(options.find(s => s.id === preferredSeries) ?? options[0]);
</script>

<div class="controls">
  <label>Raggruppamento
    <select bind:value={kind}>
      <option value="list">Liste</option>
      <option value="subject">Coalizioni / liste singole</option>
    </select>
  </label>
  <label>Ramo
    <select bind:value={chamber}>
      <option value="camera">Camera</option>
      <option value="senato">Senato</option>
    </select>
  </label>
  <label>{kind === 'list' ? 'Lista' : 'Coalizione / lista singola'}
    <select value={selected?.id ?? ''} onchange={e => { preferredSeries = e.currentTarget.value; }} disabled={!options.length}>
      {#each options as option (option.id)}<option value={option.id}>{option.name}</option>{/each}
    </select>
  </label>
</div>

{#if selected}
  {#key selected.id}
    <ReformSeriesPanel {client} {resultId} {owner} descriptor={selected} {variant} />
  {/key}
{:else}
  <p role="status">Nessuna percentuale disponibile per questo ramo e raggruppamento.</p>
{/if}

<style>
  .controls { display: flex; flex-wrap: wrap; gap: 1rem; margin: 1rem 0; }
  label { display: grid; gap: .5rem; flex: 1 1 12rem; min-width: 0; }
  select { box-sizing: border-box; width: 100%; min-width: 0; font: inherit; padding: .5rem; }
  p { font-size: .85rem; color: #4d5963; }
</style>
