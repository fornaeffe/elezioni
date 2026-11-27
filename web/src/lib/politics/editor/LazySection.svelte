<script lang="ts">
  import type { Snippet } from 'svelte';
  let { title, count, headerControl, children }: {
    title: string;
    count?: { value: number; label: string };
    headerControl?: Snippet;
    children: Snippet;
  } = $props();
  let open = $state(false);
</script>

{#snippet heading()}
  {title}{#if count && count.value > 0}<span class="summary-count">{` · ${count.value} ${count.label}`}</span>{/if}
{/snippet}

{#snippet disclosure()}
  <details ontoggle={event => open = event.currentTarget.open}>
    <summary class={{ 'with-control': !!headerControl }}>{#if headerControl}
      <span class="marker" aria-hidden="true"></span><span class="summary-title">{@render heading()}</span>
    {:else}{@render heading()}{/if}</summary>
    {#if open}{@render children()}{/if}
  </details>
{/snippet}

{#if headerControl}
  <div class="section-with-control">
    <div class="header-control">{@render headerControl()}</div>
    {@render disclosure()}
  </div>
{:else}
  {@render disclosure()}
{/if}

<style>
  details, .section-with-control {
    --section-padding: var(--space-3);
    --section-border-width: 1px;
    margin-block: var(--space-2);
  }

  details {
    min-inline-size: 0;
    padding: var(--section-padding);
    border: var(--section-border-width) solid var(--color-border);
    border-radius: var(--radius-panel);
    background: var(--color-surface);
  }

  summary { font-weight: 600; overflow-wrap: anywhere; }
  :global(details) details { background: var(--color-surface-muted); }

  .section-with-control {
    --summary-marker-size: 0.75rem;
    --header-control-size: 1.5rem;
    --header-gap: var(--space-1);
    --header-inset: calc(var(--section-padding) + var(--section-border-width));
    position: relative;
  }

  .section-with-control > details { margin-block: 0; }

  /* Align the sibling control with the reserved column in the native summary. */
  .header-control {
    position: absolute;
    inset-block-start: var(--header-inset);
    inset-inline-start: calc(var(--header-inset) + var(--summary-marker-size) + var(--header-gap));
    inline-size: var(--header-control-size);
    block-size: var(--header-control-size);
  }

  .with-control {
    display: grid;
    grid-template-columns: var(--summary-marker-size) var(--header-control-size) minmax(0, 1fr);
    column-gap: var(--header-gap);
    line-height: var(--header-control-size);
    list-style: none;
  }

  .with-control::-webkit-details-marker { display: none; }
  .with-control .summary-title { grid-column: 3; }
  .marker::before { content: '▸'; }
  details[open] > summary > .marker::before { content: '▾'; }

  .summary-count {
    color: var(--color-text-muted);
    font-size: 0.875em;
    font-weight: 400;
  }
</style>
