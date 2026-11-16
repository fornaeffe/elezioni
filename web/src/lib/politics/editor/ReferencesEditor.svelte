<script lang="ts">
  import type { PoliticsScenario } from '../scenario/schema';
  import { capabilities } from '../scenario/registry';
  import type { EditorAssets } from './assets';
  import { referenceRows, editScenario, type ReferenceTarget, type ApplyEdit } from './model';
  import LazySection from './LazySection.svelte';
  import ReferenceFields from './ReferenceFields.svelte';
  import ReferenceTree from './ReferenceTree.svelte';
  import { effectiveUncertaintyMultipliers } from '../scenario/references';
  let { scenario, target, label, assets, edit, mode = 'all' }: { scenario: PoliticsScenario; target: ReferenceTarget; label: string; assets: EditorAssets | null; edit: ApplyEdit; mode?: 'all' | 'basic' | 'advanced' } = $props();
  let rows = $derived(referenceRows(scenario, target));
  let different = $derived(rows.some(r => r.scope.kind === 'national' && r.channel !== 'common'));
  function multiplier(key: 'national' | 'local', value: string) {
    edit(s => editScenario(s, n => {
      if (target?.kind !== 'list') return;
      for (const chamber of ['camera', 'senato'] as const) {
        let row = n.modelSettings.M02.uncertaintyMultipliers.find(r => r.listId === target.id && r.chamber === chamber);
        if (!row) { row = { listId: target.id, chamber, national: null, local: null }; n.modelSettings.M02.uncertaintyMultipliers.push(row); }
        row[key] = value.trim() === '' ? null : Number(value);
      }
    }));
  }
</script>
{#if mode !== 'advanced'}
<ReferenceFields {scenario} {target} scope={{ kind: 'national' }} {label} {edit} showSd={false} compact={mode === 'basic'} showRemove={mode !== 'basic'} />
{/if}
{#if different && mode !== 'advanced'}<span class="badge">Riferimenti distinti per camera</span>{/if}
{#if mode !== 'basic'}
<LazySection title={`Riferimenti avanzati · ${rows.length} dichiarazioni`}>
  <p>{target === null ? 'Astensioni e schede non valide: percentuali degli elettori.' : 'Percentuali dei voti validi.'} Campo vuoto: nessuna sostituzione. M01 ignora le SD salvate; M02 le utilizza.</p>
  <ReferenceFields {scenario} {target} scope={{ kind: 'national' }} {label} {edit} showMean={false} showRemove={mode === 'advanced'} />
  {#each ['camera', 'senato'] as channel (channel)}
    <ReferenceFields {scenario} {target} scope={{ kind: 'national' }} channel={channel as 'camera' | 'senato'} label={`${label} ${channel === 'camera' ? 'Camera' : 'Senato'}`} {edit} />
  {/each}
  {#if target?.kind === 'list' && capabilities(scenario).generator.uncertaintyMultipliers}
      {@const values = effectiveUncertaintyMultipliers(scenario, target.id, 'camera')}
      {#each ['national', 'local'] as k (k)}
        {@const key = k as 'national' | 'local'}
        <label>Moltiplicatore {key === 'national' ? 'nazionale' : 'locale'} comune (adimensionale)
          <input type="number" min="0" step="any" value={values[key]} onchange={e => multiplier(key, e.currentTarget.value)} />
        </label>
      {/each}
  {/if}
  {#if assets}<ReferenceTree {scenario} {target} nodes={assets.references} {edit} />{:else}<p>Dati territoriali in caricamento.</p>{/if}
</LazySection>

{/if}
