<script lang="ts">
  import type { Snippet } from 'svelte';
  let { label, children }: { label: string; children?: Snippet } = $props();
</script>
<details class="activity-overlay" aria-label="Stato applicazione">
  <summary><span role="status">{label}</span></summary>
  <div>{#if children}{@render children()}{/if}</div>
</details>
<style>
  .activity-overlay { position: fixed; bottom: 1rem; left: 1rem; z-index: 40; max-width: min(23rem, calc(100vw - 2rem)); border: 1px solid #a8bac6; border-radius: .6rem; background: #f1f6f8; color: #244f40; box-shadow: 0 3px 16px #18202626; font: .85rem system-ui, sans-serif; }
  summary { cursor: pointer; padding: .7rem 1rem; }
  div { padding: 0 1rem .7rem; }
</style>
