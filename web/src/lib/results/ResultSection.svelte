<script lang="ts">
  import type { Snippet } from 'svelte';

  let {
    title,
    open = $bindable(false),
    compact = false,
    children,
    onToggle,
  }: {
    title: string;
    open?: boolean;
    compact?: boolean;
    children: Snippet<[boolean]>;
    onToggle?: (open: boolean) => void;
  } = $props();
</script>

<details
  class={{ compact }}
  bind:open
  ontoggle={(event) => onToggle?.(event.currentTarget.open)}
  role={compact ? undefined : 'region'}
  aria-label={title}
>
  <summary>{title}</summary>
  {@render children(open)}
</details>

<style>
  details {
    min-width: 0;
    margin: 0;
    border-block-end: 1px solid var(--color-border);
    padding-block: var(--space-4);
  }
  summary {
    color: var(--color-text-muted);
    font-size: var(--text-small);
    font-weight: 750;
  }
  details.compact {
    border: 0;
    padding-block: var(--space-1);
  }
  .compact > summary {
    font-size: inherit;
    font-weight: 600;
  }
</style>
