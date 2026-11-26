<script lang="ts">
  import FieldWithHelp from '$lib/ui/FieldWithHelp.svelte';
  import type { PoliticsScenario, ReferenceScope, Reference } from '../scenario/schema';
  import { capabilities, activeGenerator } from '../scenario/registry';
  import { referenceRows, scopeKey, setReference, type ApplyEdit, type ReferenceTarget } from './model';
  let { scenario, target, scope, channel = 'common', label, edit, meanHelp, showSd = true, showMean = true, hideCaptionOnWide = false, showRemove = true }: {
    scenario: PoliticsScenario; target: ReferenceTarget; scope: ReferenceScope; channel?: Reference['channel']; label: string; edit: ApplyEdit; meanHelp?: string; showSd?: boolean; showMean?: boolean; hideCaptionOnWide?: boolean; showRemove?: boolean;
  } = $props();
  const meanId = $props.id();
  const meanLabel = $derived(`${label} (%)`);
  let row = $derived(referenceRows(scenario, target).find(r => scopeKey(r.scope) === scopeKey(scope) && r.channel === channel && r.appliesTo.includes(activeGenerator(scenario).id)));
  let usesSd = $derived(capabilities(scenario).generator.referenceSd);
</script>
<div class="reference-fields">
  {#if showMean}
    <FieldWithHelp id={meanId} label={meanLabel} help={meanHelp} {hideCaptionOnWide}>
      <input id={meanId} aria-label={hideCaptionOnWide ? meanLabel : undefined} type="number" min="0" max="100" step="any" placeholder="% auto" value={row ? Number((row.mean * 100).toPrecision(12)) : ''}
        onchange={e => edit(s => setReference(s, target, scope, channel, e.currentTarget.value))} />
    </FieldWithHelp>
  {/if}
  {#if usesSd && showSd && row}
    <label data-field>SD {label} (punti percentuali)
      <input type="number" min="0" step="any" disabled={row.mean === 0 || row.mean === 1} value={row.uncertainty?.sdPercentagePoints ?? ''}
        placeholder="Predefinita" onchange={e => edit(s => setReference(s, target, scope, channel, String(row!.mean * 100), e.currentTarget.value))} />
    </label>
    <small>{row.uncertainty?.origin === 'explicit' ? 'SD esplicita' : 'SD predefinita: convenzione dello scenario'}{row.mean === 0 || row.mean === 1 ? ' · al limite la SD deve essere zero' : ''}</small>
    {#if row.uncertainty?.origin === 'explicit'}<button onclick={() => edit(s => setReference(s, target, scope, channel, String(row!.mean * 100), ''))}>Usa SD predefinita</button>{/if}
  {/if}
  {#if row && showRemove}<button onclick={() => edit(s => setReference(s, target, scope, channel, ''))}>Rimuovi {label}</button>{/if}
</div>

<style>
  .reference-fields {
    display: var(--reference-display, flex);
    flex-wrap: wrap;
    align-items: center;
    gap: var(--space-2);
  }

  input { inline-size: var(--reference-input-width, 8rem); }
  small { color: var(--color-text-muted); }
  @media (width <= 650px) { input { inline-size: 100%; } }
</style>
