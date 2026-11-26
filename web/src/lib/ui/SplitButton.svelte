<script lang="ts">
  import { DropdownMenu } from 'bits-ui';
  import { ChevronDown } from '@lucide/svelte';

  type Action = {
    label: string;
    run: () => void | Promise<void>;
  };

  let { label, optionsLabel, primary, options, disabled = false }: {
    label: string;
    optionsLabel: string;
    primary: Action;
    options: readonly Action[];
    disabled?: boolean;
  } = $props();
</script>

<div class="split-button" role="group" aria-label={label}>
  <button type="button" class="primary" {disabled} onclick={() => void primary.run()}>{primary.label}</button>
  {#key disabled}
    <DropdownMenu.Root>
      <DropdownMenu.Trigger aria-label={optionsLabel} {disabled}>
        <ChevronDown size={16} aria-hidden="true" />
      </DropdownMenu.Trigger>
      <DropdownMenu.Portal>
        <DropdownMenu.Content data-split-button-menu="" align="end" sideOffset={4} preventScroll={false}>
          {#each options as option (option)}
            <DropdownMenu.Item onSelect={() => void option.run()}>{option.label}</DropdownMenu.Item>
          {/each}
        </DropdownMenu.Content>
      </DropdownMenu.Portal>
    </DropdownMenu.Root>
  {/key}
</div>

<style>
  .split-button { display: inline-flex; max-inline-size: 100%; }
  .primary { border-start-end-radius: 0; border-end-end-radius: 0; }
  .split-button :global([data-dropdown-menu-trigger]) {
    display: inline-grid;
    place-items: center;
    flex-shrink: 0;
    border-inline-start: 0;
    border-start-start-radius: 0;
    border-end-start-radius: 0;
    padding-inline: var(--space-3);
  }

  .split-button :global([data-dropdown-menu-trigger][data-state=open]) {
    background: var(--color-action-hover);
  }

  .split-button :is(.primary, :global([data-dropdown-menu-trigger])):focus-visible { z-index: 1; }

  :global([data-split-button-menu][data-dropdown-menu-content]) {
    max-inline-size: var(--bits-dropdown-menu-content-available-width);
    padding: var(--space-1);
    border: 1px solid var(--color-border);
    border-radius: var(--radius-control);
    background: var(--color-surface);
    box-shadow: var(--shadow-overlay);
    z-index: 101;
  }

  :global([data-split-button-menu] [data-dropdown-menu-item]) {
    padding: var(--space-2) var(--space-3);
    border-radius: var(--radius-control);
    cursor: pointer;
  }

  :global([data-split-button-menu] [data-dropdown-menu-item][data-highlighted]) {
    background: var(--color-action-hover);
  }
</style>
