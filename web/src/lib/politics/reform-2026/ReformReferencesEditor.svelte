<script lang="ts">
  import PercentageInput from '../../scenario/PercentageInput.svelte';
  import SearchSelect from '../../scenario/SearchSelect.svelte';
  import type { ReferenceScope } from './input';
  import type { ReformGenerationScenario } from './generation-types';
  import type { ReformEditorCatalog } from './editor-catalog';
  import { editableLists, referenceKey, referenceValue, setReformReference, type EditingScope } from './scenario-editor';
  let { scenario, catalog, disabled, onaction }: {
    scenario: ReformGenerationScenario; catalog: ReformEditorCatalog; disabled: boolean;
    onaction: (action: () => ReformGenerationScenario) => void;
  } = $props();
  let scope = $state<EditingScope>('common');
  let kind = $state<ReferenceScope['kind']>('national');
  let location = $state('');
  const options = $derived(kind === 'region' ? catalog.regions.map(r => ({ id: r.id, label: r.name })) : kind === 'province'
    ? [...new Set(catalog.municipalities.map(m => m.provinceId))].sort().map(id => ({ id, label: `Provincia ${id}` }))
    : catalog.municipalities.map(m => ({ id: m.code, label: `${m.name} (${m.provinceId})` })));
  const reference = $derived<ReferenceScope | null>(kind === 'national' ? { kind: 'national' } : location ? { kind, code: location } : null);
  const lists = $derived(editableLists(scenario, scope));
  const localGroups = $derived([...new Map([...scenario.shares, ...scenario.abstention].filter(r => r.scope.kind !== 'national').map(r => [referenceKey(r.scope), r.scope])).values()]);
  function set(list: string | null, value: number | undefined) { if (reference) { const r = reference; onaction(() => setReformReference(scenario, r, list, scope, value)); } }
  function label(r: ReferenceScope) { return r.kind === 'national' ? 'Italia' : r.kind === 'municipality' ? catalog.municipalities.find(m => m.code === r.code)?.name ?? r.code : r.kind === 'region' ? catalog.regions.find(m => m.id === r.code)?.name ?? r.code : `Provincia ${r.code}`; }
</script>
<section id="r26-references" aria-label="Quote e astensione">
  <h3>Quote e astensione</h3>
  <label>Ramo riferimenti<select bind:value={scope} {disabled}><option value="common">Comune ai rami</option><option value="camera">Camera</option><option value="senato">Senato</option></select></label>
  <p>Quote sui voti validi, in percentuale. Vuoto = eredita; 0 = zero esplicito. La quota comune è un'ancora: il modello deriva quote Camera/Senato diverse; un valore esplicito per ramo ha precedenza.</p>
  <div class="editor-row">
    <label>Ambito geografico<select bind:value={kind} onchange={() => { location = ''; }} {disabled}>
      <option value="national">Italia</option><option value="region">Regione</option><option value="province">Provincia</option><option value="municipality">Comune</option>
    </select></label>
    {#if kind !== 'national'}<SearchSelect label="area riferimento" {options} value={location} onchange={v => { location = v; }} {disabled} />{/if}
  </div>
  <p>Precedenza: Italia → regione → provincia → comune. I campi mostrano l'override di quest'area; i valori ereditati e derivati sono disponibili in Verifica e anteprima.</p>
  {#if reference}
    <h4>{label(reference)} — {scope === 'common' ? 'Comune ai rami' : scope === 'camera' ? 'Camera' : 'Senato'}</h4>
    <div class="editor-reference-grid">
      {#each lists as list (list.id)}
        <label>{list.name}<PercentageInput label={`Quota ${list.name}`} value={referenceValue(scenario.shares, reference, scope, list.id)} onchange={v => set(list.id, v)} {disabled} /></label>
      {/each}
      <label>Astensione elettori<PercentageInput label="Astensione elettori" value={referenceValue(scenario.abstention, reference, scope)} onchange={v => set(null, v)} {disabled} /></label>
    </div>
    <p>L'astensione comprende anche schede non valide e voti storici non mappati, secondo la convenzione dello scenario.</p>
  {/if}
  {#if localGroups.length}<details><summary>Aree con riferimenti ({localGroups.length})</summary>
    {#each localGroups as group (referenceKey(group))}<button type="button" {disabled} onclick={() => { kind = group.kind; location = group.kind === 'national' ? '' : group.code; }}>{label(group)}</button>{/each}
  </details>{/if}
</section>
