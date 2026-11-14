<script lang="ts">
  import type { PoliticsScenario } from '../scenario/schema';
  import { capabilities } from '../scenario/registry';
  import type { EditorAssets } from './assets';
  import { referenceRows, editScenario, type ReferenceTarget, type ApplyEdit } from './model';
  import LazySection from './LazySection.svelte';
  import ReferenceFields from './ReferenceFields.svelte';
  import ReferenceTree from './ReferenceTree.svelte';
  import { effectiveUncertaintyMultipliers } from '../scenario/references';
  let { scenario, target, label, assets, edit }: { scenario: PoliticsScenario; target: ReferenceTarget; label: string; assets: EditorAssets | null; edit: ApplyEdit } = $props();
  let rows = $derived(referenceRows(scenario, target));
  let different = $derived(rows.some(r => r.scope.kind === 'national' && r.channel !== 'common'));
  function multiplier(chamber: 'camera' | 'senato', key: 'national' | 'local', value: string) {
    edit(s => editScenario(s, n => {
      if (target?.kind !== 'list') return;
      let row = n.modelSettings.M02.uncertaintyMultipliers.find(r => r.listId === target.id && r.chamber === chamber);
      if (!row) { row = { listId: target.id, chamber, national: null, local: null }; n.modelSettings.M02.uncertaintyMultipliers.push(row); }
      row[key] = value.trim() === '' ? null : Number(value);
    }));
  }
</script>
<ReferenceFields {scenario} {target} scope={{ kind: 'national' }} {label} {edit} showSd={false} />
{#if different}<span class="badge">Riferimenti distinti per camera</span>{/if}
<LazySection title={`Riferimenti avanzati · ${rows.length} dichiarazioni`}>
  <p>{target === null ? 'Astensioni e schede non valide: percentuali degli elettori.' : 'Percentuali dei voti validi.'} Campo vuoto: nessuna sostituzione. M01 ignora le SD salvate; M02 le utilizza.</p>
  <ReferenceFields {scenario} {target} scope={{ kind: 'national' }} {label} {edit} showMean={false} />
  {#each ['camera', 'senato'] as channel (channel)}
    <ReferenceFields {scenario} {target} scope={{ kind: 'national' }} channel={channel as 'camera' | 'senato'} label={`${label} ${channel === 'camera' ? 'Camera' : 'Senato'}`} {edit} />
  {/each}
  {#if target?.kind === 'list' && capabilities(scenario).generator.uncertaintyMultipliers}
    {#each ['camera', 'senato'] as ch (ch)}
      {@const chamber = ch as 'camera' | 'senato'}
      {@const values = effectiveUncertaintyMultipliers(scenario, target.id, chamber)}
      {#each ['national', 'local'] as k (k)}
        {@const key = k as 'national' | 'local'}
        <label>Moltiplicatore {key === 'national' ? 'nazionale' : 'locale'} {chamber} (adimensionale)
          <input type="number" min="0" step="any" value={values[key]} onchange={e => multiplier(chamber, key, e.currentTarget.value)} />
        </label>
      {/each}
    {/each}
  {/if}
  {#if assets}<ReferenceTree {scenario} {target} nodes={assets.references} {edit} />{:else}<p>Dati territoriali in caricamento.</p>{/if}
</LazySection>
