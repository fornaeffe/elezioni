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
    border-bottom: 1px solid #e5e9ed;
    padding: 1rem 0;
  }
  summary {
    cursor: pointer;
    color: #4d5963;
    font-size: 0.9rem;
    font-weight: 750;
  }
  summary:focus-visible {
    outline: 2px solid #3878a0;
    outline-offset: 0.25rem;
  }
  details.compact {
    border: 0;
    padding: 0.25rem 0;
  }
  .compact > summary {
    font-size: inherit;
    font-weight: 600;
  }
</style>
