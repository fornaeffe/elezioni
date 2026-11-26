<script lang="ts">
  import type { Snippet } from 'svelte';
  let { title, count, children }: {
    title: string;
    count?: { value: number; label: string };
    children: Snippet;
  } = $props();
  let open = $state(false);
</script>
<details ontoggle={event => open = event.currentTarget.open}>
  <summary>
    {title}{#if count && count.value > 0}
      <span class="summary-count">{` · ${count.value} ${count.label}`}</span>
    {/if}
  </summary>
  {#if open}{@render children()}{/if}
</details>

<style>
  details {
    min-inline-size: 0;
    margin-block: var(--space-2);
    padding: var(--space-3);
    border: 1px solid var(--color-border);
    border-radius: var(--radius-panel);
    background: var(--color-surface);
  }

  summary { font-weight: 600; overflow-wrap: anywhere; }
  :global(details) details { background: var(--color-surface-muted); }

  .summary-count {
    color: var(--color-text-muted);
    font-size: 0.875em;
    font-weight: 400;
  }
</style>
