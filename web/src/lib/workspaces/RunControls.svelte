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
  <button class="primary" type="button" onclick={onrun} disabled={disabled || running || !canRun || !Number.isSafeInteger(simulations) || simulations < 1 || maximum !== null && simulations > maximum}>{label}</button>
  {#if running && oncancel}<button type="button" onclick={oncancel}>Annulla calcolo</button>{/if}
</div>
<style>
  .run-controls { display: flex; gap: .75rem; flex-wrap: wrap; align-items: end; }
  label { display: grid; gap: .35rem; }
  input, button { font: inherit; padding: .5rem; border: 1px solid #a8bac6; border-radius: 4px; }
  input[type='number'] { width: 7rem; }
  button { cursor: pointer; background: #edf3f7; }
  button:disabled { cursor: default; opacity: .6; }
  .primary { background: #24536e; color: white; }
</style>
