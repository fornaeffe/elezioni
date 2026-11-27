<script lang="ts">
  import type { Snippet } from 'svelte';
  import type { HTMLDialogAttributes } from 'svelte/elements';

  let { element = $bindable(), title, description, children, actions, onclose }: {
    element?: HTMLDialogElement;
    title: string;
    description?: Snippet;
    children?: Snippet;
    actions?: Snippet;
    onclose?: HTMLDialogAttributes['onclose'];
  } = $props();

  const titleId = $props.id();
  const descriptionId = `${titleId}-description`;
</script>

<dialog bind:this={element} aria-labelledby={titleId} aria-describedby={description ? descriptionId : undefined} {onclose}>
  <h2 id={titleId}>{title}</h2>
  {#if description}
    <div id={descriptionId} class="description">{@render description()}</div>
  {/if}
  {@render children?.()}
  {#if actions}
    <div class="actions">{@render actions()}</div>
  {/if}
</dialog>

<style>
  dialog {
    inline-size: var(--dialog-width, 28rem);
    max-inline-size: calc(100% - 2 * var(--space-4));
    max-block-size: calc(100% - 2 * var(--space-4));
    padding: var(--space-4);
    border: 1px solid var(--color-border);
    border-radius: var(--radius-panel);
    color: var(--color-text);
    background: var(--color-surface);
    box-shadow: var(--shadow-overlay);
    overflow: auto;
    overflow-wrap: anywhere;
    overscroll-behavior: contain;
  }

  dialog[open] { display: grid; gap: var(--space-3); }
  dialog::backdrop { background: var(--color-backdrop); }
  h2 { margin: 0; font-size: 1.25rem; }
  .description > :global(p) { margin: 0; }
  .description > :global(p + p) { margin-block-start: var(--space-2); }
  .actions { display: flex; flex-wrap: wrap; justify-content: end; gap: var(--space-2); }
</style>
