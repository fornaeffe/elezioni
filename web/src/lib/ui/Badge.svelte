<script lang="ts">
  import type { Snippet } from 'svelte';
  let { children }: { children: Snippet } = $props();
</script>

<span>{@render children()}</span>

<style>
  span {
    display: inline-block;
    font-size: var(--text-small);
    background: var(--color-surface-muted);
    padding: var(--space-1) var(--space-2);
    border-radius: var(--radius-control);
  }
</style>
