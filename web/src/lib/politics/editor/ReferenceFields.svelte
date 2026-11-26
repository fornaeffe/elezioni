<script lang="ts">
  import FieldWithHelp from '$lib/ui/FieldWithHelp.svelte';
  import Toggletip from '$lib/ui/Toggletip.svelte';
  import type { NonValidReference, PoliticsScenario, ReferenceScope, Reference } from '../scenario/schema';
  import { capabilities } from '../scenario/registry';
  import { referenceRows, scopeKey, setReference, type ApplyEdit, type ReferenceTarget } from './model';
  import { commonReferenceHelp, referenceQuotaHelp, referenceSdHelp } from './reference-help';

  type Row = Reference | NonValidReference;
  type Channel = Reference['channel'];

  const channels = [
    { id: 'common', heading: 'Entrambe' },
    { id: 'camera', heading: 'Camera' },
    { id: 'senato', heading: 'Senato' }
  ] as const;

  let { scenario, target, scope, label, edit, layout = 'basic', hideCaptionOnWide = false, showCommonRemove = true }: {
    scenario: PoliticsScenario; target: ReferenceTarget; scope: ReferenceScope; label: string; edit: ApplyEdit;
    layout?: 'basic' | 'grid'; hideCaptionOnWide?: boolean; showCommonRemove?: boolean;
  } = $props();

  const id = $props.id();
  const generator = $derived(capabilities(scenario).generator);
  const national = $derived(scope.kind === 'national');
  const quotaHelp = $derived(referenceQuotaHelp(generator.id, target?.kind ?? 'non-valid', scope.kind));
  const scopeAddress = $derived(scopeKey(scope));
  const rows = $derived(referenceRows(scenario, target).filter(row =>
    scopeKey(row.scope) === scopeAddress && row.appliesTo.includes(generator.id)));

  function editReference(channel: Channel, percent: string, sd?: string) {
    return edit(s => setReference(s, target, scope, channel, percent, sd));
  }

  function focusQuota(inputId: string) {
    (document.getElementById(inputId) ?? document.getElementById(`${id}-camera-quota`))?.focus();
  }
</script>

{#snippet quota(inputId: string, name: string, channel: Channel, row: Row | undefined)}
  <input class="quota" id={inputId} aria-label={name} type="number" min="0" max="100" step="any"
    placeholder="% auto" value={row ? Number((row.mean * 100).toPrecision(12)) : ''}
    onchange={event => editReference(channel, event.currentTarget.value)} />
{/snippet}

{#snippet remove(channel: Channel, name: string, inputId: string)}
  <button class="remove" type="button" aria-label={`Rimuovi ${name}`} onclick={() => {
    if (editReference(channel, '')) focusQuota(inputId);
  }}>{layout === 'grid' ? 'Rimuovi' : `Rimuovi ${name}`}</button>
{/snippet}

{#if layout === 'basic'}
  {@const row = rows.find(row => row.channel === 'common')}
  {@const name = `${label} (%)`}
  <div class="basic">
    <FieldWithHelp {id} label={name} help={quotaHelp} {hideCaptionOnWide}>
      {@render quota(id, name, 'common', row)}
    </FieldWithHelp>
    {#if showCommonRemove && row}{@render remove('common', label, id)}{/if}
  </div>
{:else}
  <div class="reference-scope" role="group" aria-label={national ? 'Riferimenti nazionali' : `Riferimenti per ${label}`}>
    <div class="reference-grid" data-sd={generator.referenceSd || undefined}>
      <div class="heading quota-heading">Quota (%) <Toggletip label="Quota (%)" text={quotaHelp} /></div>
      {#if generator.referenceSd}
        <div class="heading sd-heading">SD (p.p.) <Toggletip label="SD (p.p.)" text={referenceSdHelp} /></div>
      {/if}
      {#each channels as { id: channel, heading } (channel)}
        {@const common = channel === 'common'}
        {@const row = rows.find(row => row.channel === channel)}
        {@const name = national && common ? label : `${label} ${common ? 'comune' : heading}`}
        {@const inputId = `${id}-${channel}-quota`}
        <div class="channel" data-channel={channel}>
          <div class="heading channel-heading">
            {heading}
            {#if common}
              <Toggletip label={heading} text={commonReferenceHelp(national)} />
            {/if}
          </div>
          {#if national && common}
            <span class="quota unavailable" aria-hidden="true">—</span>
          {:else}
            {@render quota(inputId, `${name} (%)`, channel, row)}
          {/if}
          {#if generator.referenceSd}
            {#if row}
              {@const boundary = row.mean === 0 || row.mean === 1}
              {@const explicit = row.uncertainty?.origin === 'explicit'}
              {@const sdId = `${id}-${channel}-sd`}
              <div class="sd">
                <input id={sdId} aria-label={`SD ${name} (punti percentuali)`} aria-describedby={`${sdId}-status`}
                  type="number" min="0" step="any" disabled={boundary}
                  value={row.uncertainty?.sdPercentagePoints ?? ''} placeholder="Predefinita"
                  onchange={event => editReference(channel, String(row.mean * 100), event.currentTarget.value)} />
                <div class="sd-status">
                  <small id={`${sdId}-status`}>{explicit ? 'Esplicita' : 'Predefinita'}{boundary ? ' · quota al limite' : ''}</small>
                  {#if explicit}
                    <button type="button" class="reset" aria-label={`Usa SD predefinita per ${name}`} onclick={() => {
                      if (editReference(channel, String(row.mean * 100), '')) {
                        if (boundary) focusQuota(inputId);
                        else document.getElementById(sdId)?.focus();
                      }
                    }}>Ripristina</button>
                  {/if}
                </div>
              </div>
            {:else}
              <span class="sd unavailable" aria-hidden="true">—</span>
            {/if}
          {/if}
          {#if row && (!common || showCommonRemove)}{@render remove(channel, name, inputId)}{/if}
        </div>
      {/each}
    </div>
  </div>
{/if}

<style>
  .basic {
    display: var(--reference-display, flex);
    flex-wrap: wrap;
    align-items: center;
    gap: var(--space-2);
  }
  .basic input { inline-size: var(--reference-input-width, 8rem); }
  @media (width <= 650px) { .basic input { inline-size: 100%; } }

  .reference-scope { container-type: inline-size; }
  .reference-grid {
    display: grid;
    grid-template-columns: minmax(max-content, .7fr) repeat(3, minmax(0, 1fr));
    grid-template-rows: [heading] auto [quota] auto [actions] auto;
    gap: var(--space-1) var(--space-2);
    margin-block: var(--space-2);
    align-items: start;
  }
  .reference-grid[data-sd] {
    grid-template-rows: [heading] auto [quota] auto [sd] auto [actions] auto;
  }
  .channel {
    display: grid;
    grid-template-rows: subgrid;
    grid-row: 1 / -1;
    min-inline-size: 0;
  }
  .channel[data-channel='common'] { grid-column: 2; }
  .channel[data-channel='camera'] { grid-column: 3; }
  .channel[data-channel='senato'] { grid-column: 4; }

  .heading {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    gap: 0 var(--space-1);
    min-block-size: 1.5rem;
    color: var(--color-text-muted);
    font-size: var(--text-small);
    font-weight: 600;
    overflow-wrap: anywhere;
  }
  .channel-heading { grid-row: heading; color: var(--color-text); }
  .quota-heading { grid-area: quota / 1; }
  .sd-heading { grid-area: sd / 1; }
  .channel > .quota { grid-row: quota; }
  .sd { grid-row: sd; min-inline-size: 0; }
  .channel input {
    inline-size: 100%;
    min-inline-size: 0;
    padding: var(--space-1) var(--space-2);
    font-variant-numeric: tabular-nums;
  }
  .unavailable { padding: var(--space-1); color: var(--color-text-muted); }
  .sd-status {
    display: flex;
    flex-wrap: wrap;
    align-items: baseline;
    gap: 0 var(--space-1);
    margin-block-start: var(--space-1);
    color: var(--color-text-muted);
  }
  .channel > .remove { grid-row: actions; justify-self: start; }
  .channel :is(.reset, .remove) {
    padding: var(--space-1);
    border: 0;
    background: transparent;
    color: var(--color-accent);
    font-size: var(--text-small);
    overflow-wrap: anywhere;
  }
  .channel :is(.reset, .remove):hover { background: var(--color-action-hover); }

  @container (width < 36rem) {
    .reference-grid, .reference-grid[data-sd] {
      grid-template-columns: minmax(0, .9fr) minmax(0, 1fr);
      grid-template-rows: auto;
    }
    .reference-grid[data-sd] {
      grid-template-columns: minmax(0, .9fr) repeat(2, minmax(0, 1fr));
    }
    .quota-heading { grid-area: 1 / 2; }
    .sd-heading { grid-area: 1 / 3; }
    .channel[data-channel] {
      grid-area: auto / 1 / auto / -1;
      grid-template-columns: subgrid;
      grid-template-rows: auto;
      row-gap: 0;
    }
    .channel-heading { grid-area: 1 / 1; }
    .channel > .quota { grid-area: 1 / 2; }
    .sd { grid-area: 1 / 3; }
    .channel > .remove { grid-area: 2 / 2 / auto / -1; }
  }
  @container (width < 15rem) {
    .reference-grid { column-gap: var(--space-1); }
    .channel input { padding-inline: var(--space-1); }
  }
</style>
