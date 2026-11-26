<script lang="ts">
  import type { Snippet } from 'svelte';
  let { simulations = $bindable(10), seed = $bindable(''), maximum = null, showSeed = true,
    disabled = false, canRun = false, running = false, label = 'Esegui', onrun, oncancel, children }:
    { simulations: number; seed?: string; maximum?: number | null; showSeed?: boolean;
      disabled?: boolean; canRun?: boolean; running?: boolean; label?: string;
      onrun: () => void; oncancel?: () => void; children?: Snippet } = $props();
</script>
<div class="run-controls">
  <label>Simulazioni<input type="number" min="1" max={maximum ?? undefined} step="1" bind:value={simulations} disabled={disabled || running} /></label>
  {#if showSeed}<label>Seed<input type="text" bind:value={seed} disabled={disabled || running} /></label>{/if}
  {#if children}{@render children()}{/if}
  <button data-variant="primary" type="button" onclick={onrun} disabled={disabled || running || !canRun || !Number.isSafeInteger(simulations) || simulations < 1 || maximum !== null && simulations > maximum}>{label}</button>
  {#if running && oncancel}<button type="button" onclick={oncancel}>Annulla calcolo</button>{/if}
</div>
<style>
  .run-controls { display: flex; gap: var(--space-3); flex-wrap: wrap; align-items: end; }
  label { display: grid; gap: var(--space-1); min-inline-size: 0; }
  input[type='number'] { width: 7rem; }
</style>
