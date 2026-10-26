<script lang="ts">
  import { displayNumber } from './chart-builders';
  let { rows, entityLabel = 'Lista' }: { entityLabel?: string; rows: { id: string; label: string; color: string; elected: number | null; minimum: number; share: number | null }[] } = $props();
  const maxElected = $derived(Math.max(1, ...rows.map(r => r.elected ?? r.minimum)));
  const maxShare = $derived(Math.max(1, ...rows.map(r => r.share ?? 0)));
</script>

<div class="paired-bars">
  <div class="heading"><span>{entityLabel}</span><span>Eletti medi</span><span>Voti validi medi %</span></div>
  {#each rows as row (row.id)}
    <div class="row">
      <span class="label" title={row.label}>{row.label}</span>
      <div class="value"><span class="track"><span style:width={`${(row.elected ?? row.minimum) / maxElected * 100}%`} style:background={row.color}></span></span><strong>{row.elected === null ? '≥ ' : ''}{displayNumber(row.elected ?? row.minimum)}</strong></div>
      <div class="value"><span class="track"><span style:width={`${(row.share ?? 0) / maxShare * 100}%`} style:background={row.color}></span></span><strong>{row.share === null ? 'n.d.' : displayNumber(row.share) + '%'}</strong></div>
    </div>
  {/each}
</div>

<style>
  .paired-bars { display: grid; gap: .5rem; margin: 1rem 0; font-size: .85rem; }
  .heading, .row { display: grid; grid-template-columns: minmax(8rem, 1fr) 2fr 2fr; gap: 1rem; align-items: center; }
  .heading { font-weight: 700; color: #4d5963; }
  .label { overflow-wrap: anywhere; }
  .value { display: grid; grid-template-columns: 1fr 5rem; gap: .5rem; align-items: center; }
  strong { font-variant-numeric: tabular-nums; text-align: right; font-weight: 600; }
  .track { background: #e8edf1; height: .75rem; border-radius: 1rem; overflow: hidden; }
  .track > span { display: block; height: 100%; }
  @media (max-width: 600px) { .heading, .row { gap: .5rem; grid-template-columns: minmax(5rem, 1fr) 1.5fr 1.5fr; } .value { grid-template-columns: 1fr; gap: .2rem; } strong { text-align: left; } }
</style>
