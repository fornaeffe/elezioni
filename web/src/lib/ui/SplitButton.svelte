<script lang="ts">
  type Action = {
    label: string;
    run: () => void | Promise<void>;
  };

  let { label, optionsLabel, primary, options }: {
    label: string;
    optionsLabel: string;
    primary: Action;
    options: readonly Action[];
  } = $props();

  const optionsId = $props.id();
  let open = $state(false);
  let toggle: HTMLButtonElement;

  function closeOptionsAndFocusToggle() {
    toggle.focus();
    open = false;
  }

  function handleKeydown(event: KeyboardEvent) {
    if (event.key !== 'Escape' || !open) return;
    event.preventDefault();
    closeOptionsAndFocusToggle();
  }
</script>

<div
  class="split-button"
  role="group"
  aria-label={label}
  onfocusout={event => {
    const next = event.relatedTarget;
    if (!(next instanceof Node) || !event.currentTarget.contains(next)) open = false;
  }}
>
  <button
    type="button"
    class="primary"
    onkeydown={handleKeydown}
    onclick={() => {
      open = false;
      void primary.run();
    }}
  >{primary.label}</button>
  <button
    type="button"
    class="toggle"
    aria-label={optionsLabel}
    aria-expanded={open}
    aria-controls={optionsId}
    bind:this={toggle}
    onkeydown={handleKeydown}
    onclick={() => {
      toggle.focus();
      open = !open;
    }}
  ><span aria-hidden="true">▾</span></button>
  <div class="options" id={optionsId} hidden={!open}>
    {#each options as option (option)}
      <button
        type="button"
        onkeydown={handleKeydown}
        onclick={() => {
          closeOptionsAndFocusToggle();
          void option.run();
        }}
      >{option.label}</button>
    {/each}
  </div>
</div>

<style>
  .split-button {
    position: relative;
    display: inline-flex;
    margin: .25rem;
  }

  .split-button button {
    font: inherit;
    max-width: 100%;
    margin: 0;
    border: 1px solid #9aaebf;
    border-radius: .3rem;
    padding: .5rem;
    background: #edf5fa;
    cursor: pointer;
  }

  .split-button button:focus-visible {
    z-index: 1;
    outline: 3px solid #007baa;
    outline-offset: 3px;
  }

  .split-button button:disabled {
    opacity: .6;
    cursor: default;
  }

  .split-button .primary {
    border-start-end-radius: 0;
    border-end-end-radius: 0;
  }

  .split-button .toggle {
    border-inline-start: 0;
    border-start-start-radius: 0;
    border-end-start-radius: 0;
    padding-inline: .75rem;
  }

  .options {
    position: absolute;
    inset-block-start: calc(100% + .25rem);
    inset-inline-start: 0;
    width: 100%;
    z-index: 2;
    border-radius: .3rem;
    box-shadow: 0 2px 8px #17304240;
  }

  .options button {
    display: block;
    width: 100%;
  }
</style>
