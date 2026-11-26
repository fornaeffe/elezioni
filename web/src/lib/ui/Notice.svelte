<script lang="ts">
  import type { Snippet } from 'svelte';

  let { severity, as = 'div', role, 'aria-label': label, 'data-testid': testId, children }: {
    severity: 'info' | 'warning' | 'error';
    as?: 'div' | 'article' | 'section' | 'p';
    role?: 'alert' | 'status';
    'aria-label'?: string;
    'data-testid'?: string;
    children: Snippet;
  } = $props();
</script>

<svelte:element this={as} class="notice" data-severity={severity} {role} aria-label={label} data-testid={testId}>
  {@render children()}
</svelte:element>

<style>
  .notice {
    --notice-text: var(--color-text);
    padding: var(--space-3) var(--space-4);
    border-inline-start: 4px solid var(--notice-border);
    border-radius: var(--radius-control);
    background: var(--notice-surface);
    color: var(--notice-text);
    overflow-wrap: anywhere;
  }
  [data-severity=info] {
    --notice-border: var(--color-info-border);
    --notice-surface: var(--color-info-surface);
  }
  [data-severity=warning] {
    --notice-border: var(--color-warning-border);
    --notice-surface: var(--color-warning-surface);
    --notice-text: var(--color-warning-text);
  }
  [data-severity=error] {
    --notice-border: var(--color-error-border);
    --notice-surface: var(--color-error-surface);
    --notice-text: var(--color-error-text);
  }
</style>
