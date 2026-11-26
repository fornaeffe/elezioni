<script lang="ts">
  import { Info } from '@lucide/svelte';

  let { label, text }: { label: string; text: string } = $props();
  const id = $props.id();
  let open = $state(false);
</script>

<button type="button" class="trigger" aria-label={`Aiuto per ${label}`} popovertarget={id}>
  <Info size={16} aria-hidden="true" />
</button>
<span id={id} popover="auto" class="panel" ontoggle={event => open = event.newState === 'open'}>
  {text}
</span>
<span class="visually-hidden" role="status">{open ? text : ''}</span>

<style>
  button.trigger {
    display: inline-grid;
    place-items: center;
    flex: none;
    width: 1.5rem;
    height: 1.5rem;
    padding: 0;
    margin: 0;
    border: 0;
    border-radius: 50%;
    background: transparent;
    color: var(--color-accent);
    cursor: pointer;
  }

  button.trigger:hover, button.trigger:focus-visible {
    background: var(--color-action-hover);
  }

  .panel {
    width: min(22rem, calc(100vw - 2rem));
    max-height: min(24rem, calc(100dvh - 2rem));
    overflow: auto;
    padding: .65rem .8rem;
    border: 1px solid var(--color-border);
    border-radius: var(--radius-panel);
    background: var(--color-surface);
    color: var(--color-text);
    box-shadow: var(--shadow-overlay);
    font-size: var(--text-small);
    font-weight: 400;
    line-height: 1.45;
  }

  @supports (position-area: block-end span-all) and (justify-self: anchor-center) {
    .panel {
      inset: auto;
      margin: .35rem .5rem;
      position-area: block-end span-all;
      justify-self: anchor-center;
      position-try-fallbacks: flip-block;
    }
  }

</style>
