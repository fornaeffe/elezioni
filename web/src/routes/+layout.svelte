<script lang="ts">
  import type { Snippet } from 'svelte';
  let { children }: { children: Snippet } = $props();
</script>
{@render children()}
<style>
  :global(body) { margin: 0; min-height: 100vh; background: #f4f7f8; color: #15232d; font-family: system-ui, sans-serif; }
  :global(*), :global(*::before), :global(*::after) { box-sizing: border-box; }
</style>
