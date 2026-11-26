<script lang="ts">
  import type { Snippet } from 'svelte';
  import Toggletip from './Toggletip.svelte';

  let { id, label, help, hideCaptionOnWide = false, leading = false, children }: {
    id: string;
    label: string;
    help?: string;
    hideCaptionOnWide?: boolean;
    leading?: boolean;
    children: Snippet;
  } = $props();
</script>

<span class="field" data-field>
  {#if leading}{@render children()}{/if}
  <span class={['caption', { 'hide-on-wide': hideCaptionOnWide }]}>
    <label for={id}>{label}</label>
    {#if help}<Toggletip {label} text={help} />{/if}
  </span>
  {#if !leading}{@render children()}{/if}
</span>

<style>
  .field { display: var(--field-display, inline-flex); margin-block: var(--field-margin, var(--space-1)); }
  .caption { display: inline-flex; align-items: center; gap: var(--space-1); overflow-wrap: anywhere; }
  @media (width > 650px) { .hide-on-wide { display: none; } }
  @media (width <= 650px) { .field { display: flex; } }
</style>
