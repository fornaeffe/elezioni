<script lang="ts">
  import Toggletip from '$lib/ui/Toggletip.svelte';
  import type { PoliticsScenario } from '../scenario/schema';
  import { capabilities } from '../scenario/registry';
  import type { EditorAssets } from './assets';
  import { referenceRows, editScenario, type ReferenceTarget, type ApplyEdit } from './model';
  import LazySection from './LazySection.svelte';
  import ReferenceFields from './ReferenceFields.svelte';
  import ReferenceTree from './ReferenceTree.svelte';
  import { effectiveUncertaintyMultipliers } from '../scenario/references';
  import { nationalReferenceHelp } from './reference-help';

  const multiplierFields = [
    { key: 'national', label: 'Moltiplicatore nazionale comune (adimensionale)',
      help: 'Regola quanto può variare il sostegno alla lista a livello nazionale nelle simulazioni M02, per Camera e Senato. Usa 1 per la variabilità predefinita, 0 per azzerare questa fonte di variabilità e un valore maggiore di 1 per aumentarla.' },
    { key: 'local', label: 'Moltiplicatore locale comune (adimensionale)',
      help: 'Regola quanto può variare il sostegno alla lista da comune a comune nelle simulazioni M02, per Camera e Senato. Usa 1 per la variabilità predefinita, 0 per azzerare questa fonte di variabilità e un valore maggiore di 1 per aumentarla.' }
  ] as const;
  let { scenario, target, label, assets, edit, mode = 'all' }: { scenario: PoliticsScenario; target: ReferenceTarget; label: string; assets: EditorAssets | null; edit: ApplyEdit; mode?: 'all' | 'basic' | 'advanced' } = $props();
  const multiplierId = $props.id();
  const generator = $derived(capabilities(scenario).generator);
  const meanHelp = $derived(nationalReferenceHelp(generator.id, target?.kind ?? 'non-valid'));
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
<ReferenceFields {scenario} {target} scope={{ kind: 'national' }} {label} {edit} {meanHelp} showSd={false} compact={mode === 'basic'} showRemove={mode !== 'basic'} />
{/if}
{#if different && mode !== 'advanced'}<span class="badge">Riferimenti distinti per camera</span>{/if}
{#if mode !== 'basic'}
<LazySection title="Riferimenti avanzati" count={{ value: rows.length, label: 'dichiarazioni' }}>
  <ReferenceFields {scenario} {target} scope={{ kind: 'national' }} {label} {edit} showMean={false} showRemove={mode === 'advanced'} />
  {#each ['camera', 'senato'] as channel (channel)}
    <ReferenceFields {scenario} {target} scope={{ kind: 'national' }} channel={channel as 'camera' | 'senato'} label={`${label} ${channel === 'camera' ? 'Camera' : 'Senato'}`} {edit} {meanHelp} />
  {/each}
  {#if target?.kind === 'list' && generator.uncertaintyMultipliers}
      {@const values = effectiveUncertaintyMultipliers(scenario, target.id, 'camera')}
      {#each multiplierFields as { key, label: fieldLabel, help } (key)}
        {@const id = `${multiplierId}-${key}`}
        <span class="field-with-tip">
          <span class="field-caption"><label for={id}>{fieldLabel}</label><Toggletip label={fieldLabel} text={help} /></span>
          <input {id} type="number" min="0" step="any" value={values[key]} onchange={e => multiplier(key, e.currentTarget.value)} />
        </span>
      {/each}
  {/if}
  {#if assets}<ReferenceTree {scenario} {target} nodes={assets.references} {edit} />{:else}<p>Dati territoriali in caricamento.</p>{/if}
</LazySection>

{/if}
