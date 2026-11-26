<script lang="ts">
  import { ChevronDown } from '@lucide/svelte';
  import { Combobox } from 'bits-ui';

  type Option = {
    value: string;
    label: string;
    detail?: string;
    disabledReason?: string | null;
  };

  let { label, value, options, onValueChange, action }: {
    label: string;
    value: string;
    options: readonly Option[];
    onValueChange: (value: string) => void;
    action?: { label: string; run: (input: HTMLInputElement) => void };
  } = $props();

  const id = $props.id();
  const itemPrefix = 'option:';
  const actionValue = 'action';
  let open = $state(false);
  let query = $state<string | null>(null);
  let input = $state<HTMLInputElement | null>(null);
  let anchor = $state<HTMLDivElement | null>(null);

  const selected = $derived(options.find(option => option.value === value));
  const search = $derived(query?.trim().toLowerCase() ?? '');
  const matches = $derived(search
    ? options.filter(option => option.label.toLowerCase().includes(search))
    : options);

  function select(item: string) {
    if (item === actionValue) {
      if (input) action?.run(input);
    } else if (item.startsWith(itemPrefix)) {
      onValueChange(item.slice(itemPrefix.length));
    }
  }
</script>

<div data-field>
  <label for={id}>{label}</label>
  <Combobox.Root type="single" allowDeselect={false}
    bind:value={() => itemPrefix + value, select} bind:open
    onOpenChange={next => { if (!next) query = null; }}>
    <div class="control" bind:this={anchor}>
      <Combobox.Input {id} bind:ref={input}
        aria-describedby={selected?.disabledReason ? `${id}-reason` : undefined}
        oninput={event => { query = event.currentTarget.value; open = true; }}>
        {#snippet child({ props })}
          <!-- Display the saved option after a dismissed query, an action, or a rejected edit. -->
          <input {...props} value={query ?? selected?.label ?? ''} />
        {/snippet}
      </Combobox.Input>
      <Combobox.Trigger aria-label={`Mostra opzioni per ${label}`}>
        <ChevronDown size={16} aria-hidden="true" />
      </Combobox.Trigger>
    </div>
    <Combobox.Portal>
      <Combobox.Content data-search-combobox-popup="" customAnchor={anchor} align="start" sideOffset={4}>
        <Combobox.Viewport>
          {#each matches as option (option.value)}
            <Combobox.Item value={itemPrefix + option.value} label={option.label}
              disabled={!!option.disabledReason} aria-disabled={option.disabledReason ? true : undefined}>
              {option.label}{option.detail ? ` · ${option.detail}` : ''}
              {#if option.disabledReason}<small>{option.disabledReason}</small>{/if}
            </Combobox.Item>
          {:else}
            <div role="status">Nessuna opzione trovata</div>
          {/each}
          {#if action}
            <Combobox.Item value={actionValue} label={action.label}>{action.label}</Combobox.Item>
          {/if}
        </Combobox.Viewport>
      </Combobox.Content>
    </Combobox.Portal>
  </Combobox.Root>
  {#if selected?.disabledReason}<small id={`${id}-reason`}>{selected.disabledReason}</small>{/if}
</div>

<style>
  .control { display: flex; inline-size: 16rem; max-inline-size: 100%; }
  input {
    flex: 1;
    min-inline-size: 0;
    border-start-end-radius: 0;
    border-end-end-radius: 0;
  }
  .control :global([data-combobox-trigger]) {
    display: grid;
    place-items: center;
    border-inline-start: 0;
    border-start-start-radius: 0;
    border-end-start-radius: 0;
  }
  .control :is(input, :global([data-combobox-trigger])):focus-visible { z-index: 1; }
  small { color: var(--color-text-muted); }

  :global([data-search-combobox-popup][data-combobox-content]) {
    inline-size: max-content;
    min-inline-size: min(var(--bits-combobox-anchor-width), var(--bits-combobox-content-available-width));
    max-inline-size: min(24rem, var(--bits-combobox-content-available-width));
    max-block-size: min(18rem, var(--bits-combobox-content-available-height));
    overflow-wrap: anywhere;
    padding: var(--space-1);
    border: 1px solid var(--color-border);
    border-radius: var(--radius-control);
    background: var(--color-surface);
    box-shadow: var(--shadow-overlay);
    z-index: 101;
  }
  :global([data-search-combobox-popup] :is([data-combobox-item], [role=status])) {
    padding: var(--space-2) var(--space-3);
    border-radius: var(--radius-control);
  }
  :global([data-search-combobox-popup] [data-combobox-item]:not([data-disabled])) { cursor: pointer; }
  :global([data-search-combobox-popup] [data-combobox-item][data-highlighted]) { background: var(--color-action-hover); }
  :global([data-search-combobox-popup] [data-combobox-item][data-selected]) { font-weight: 600; }
  :global([data-search-combobox-popup] :is([data-disabled], [role=status])) { color: var(--color-text-muted); }
  :global([data-search-combobox-popup] small) { display: block; }
</style>
