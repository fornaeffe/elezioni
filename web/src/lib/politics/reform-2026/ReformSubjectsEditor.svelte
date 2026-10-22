<script lang="ts">
  import { Trash2 } from '@lucide/svelte';
  import type { PoliticalList } from './input';
  import type { ReformGenerationScenario } from './generation-types';
  import type { ReformEditorCatalog } from './editor-catalog';
  import { addReformCoalition, addReformList, copyReformPoliticalConfiguration, editableLists, removeReformCoalition, removeReformList, updateReformList, type EditingScope } from './scenario-editor';
  import { upgradeReformDraft } from './presentation-metadata';
  let { scenario, scope, catalog, disabled, onaction }: {
    scenario: ReformGenerationScenario; scope: EditingScope; catalog: ReformEditorCatalog; disabled: boolean;
    onaction: (action: () => ReformGenerationScenario) => void;
  } = $props();
  const lists = $derived(editableLists(scenario, scope));
  let search = $state('');
  let page = $state(0);
  const matching = $derived(lists.filter(l => `${l.name} ${l.id}`.toLocaleLowerCase('it-IT').includes(search.toLocaleLowerCase('it-IT'))));
  const safePage = $derived(Math.min(page, Math.max(0, Math.ceil(matching.length / 10) - 1)));
  function metadata(change: (s: ReformGenerationScenario) => void) { onaction(() => { const n = upgradeReformDraft(scenario); change(n); return n; }); }
  function patch(id: string, value: Partial<Omit<PoliticalList, 'id'>>) { onaction(() => updateReformList(scenario, scope, id, value)); }
</script>
<section id="r26-subjects" aria-label="Liste e coalizioni">
  <h3>Coalizioni</h3>
  <p>Nomi e colori sono comuni. L'appartenenza delle liste segue l'ambito di modifica selezionato.</p>
  {#each scenario.presentation?.coalitions ?? [] as coalition (coalition.id)}
    <div class="editor-row subject-row coalition-row">
      <input type="color" aria-label={`Colore coalizione ${coalition.name}`} value={coalition.color} {disabled} onchange={e => metadata(n => { n.presentation!.coalitions.find(c => c.id === coalition.id)!.color = e.currentTarget.value; })} />
      <label class="subject-name">Nome coalizione<input value={coalition.name} {disabled} onchange={e => { const name = e.currentTarget.value; metadata(n => { n.presentation!.coalitions.find(c => c.id === coalition.id)!.name = name; }); }} /></label>
      <button type="button" title={`Rimuovi coalizione ${coalition.name}`} aria-label={`Rimuovi coalizione ${coalition.name}`} {disabled} onclick={() => onaction(() => removeReformCoalition(scenario, coalition.id))}><Trash2 size={18} aria-hidden="true" /></button>
    </div>
  {/each}
  <button type="button" {disabled} onclick={() => onaction(() => addReformCoalition(scenario))}>Aggiungi coalizione</button>
  <h3>Liste</h3>
  <p>Le nuove liste iniziano senza depositi: seleziona i collegi nella sezione Depositi. I nomi non cambiano le identità salvate.</p>
  <label>Cerca lista<input type="search" bind:value={search} oninput={() => { page = 0; }} {disabled} /></label>
  <p>{matching.length} liste; fino a 10 per pagina, compresi i riferimenti storici speciali.</p>
  {#each matching.slice(safePage * 10, (safePage + 1) * 10) as list (list.id)}
    <div class="editor-card" data-testid="reform-list-row">
      <div class="editor-row subject-row list-row">
        <input type="color" aria-label={`Colore lista ${list.name}`} value={scenario.presentation?.lists.find(l => l.id === list.id)?.color ?? '#64748b'} {disabled}
          onchange={e => { const color = e.currentTarget.value; metadata(n => { n.presentation!.lists.find(l => l.id === list.id)!.color = color; }); }} />
        <label class="subject-name">Nome lista<input value={list.name} {disabled} onchange={e => patch(list.id, { name: e.currentTarget.value })} /></label>
        <label class="subject-coalition">Coalizione<select value={list.coalitionId ?? ''} {disabled} onchange={e => patch(list.id, { coalitionId: e.currentTarget.value || null })}>
          <option value="">Lista autonoma</option>
          {#each scenario.presentation?.coalitions ?? [] as coalition (coalition.id)}<option value={coalition.id}>{coalition.name}</option>{/each}
        </select></label>
        <button type="button" title={`Rimuovi lista ${list.name}`} aria-label={`Rimuovi lista ${list.name}`} {disabled} onclick={() => onaction(() => removeReformList(scenario, scope, list.id))}><Trash2 size={18} aria-hidden="true" /></button>
      </div>
      {#if scope === 'common'}
        {@const camera = scenario.lists.camera.find(l => l.id === list.id)}
        {@const senate = scenario.lists.senato.find(l => l.id === list.id)}
        {#if !camera || !senate || JSON.stringify(camera) !== JSON.stringify(senate)}<p class="editor-notice">Configurazione diversa tra Camera e Senato. Una modifica comune aggiorna entrambi; gli altri campi restano distinti.</p>{/if}
      {/if}
      <details><summary>Minoranza linguistica protetta — {list.name}</summary>
        <label><input type="checkbox" checked={list.protectedMinority !== null} {disabled} onchange={e => patch(list.id, { protectedMinority: e.currentTarget.checked ? { regionId: '04', legalReference: '' } : null })} />Dichiara tutela</label>
        {#if list.protectedMinority}
          <label>Regione tutela<select value={list.protectedMinority.regionId} {disabled} onchange={e => patch(list.id, { protectedMinority: { ...list.protectedMinority!, regionId: e.currentTarget.value } })}>
            {#each catalog.regions as region (region.id)}<option value={region.id}>{region.name}</option>{/each}
          </select></label>
          <label>Riferimento tutela<input value={list.protectedMinority.legalReference} {disabled} onchange={e => patch(list.id, { protectedMinority: { ...list.protectedMinority!, legalReference: e.currentTarget.value } })} /></label>
          <p>La presenza storica sul territorio non implica automaticamente una tutela.</p>
        {/if}
      </details>
    </div>
  {/each}
  <div class="editor-row"><button type="button" disabled={disabled || safePage === 0} onclick={() => { page = safePage - 1; }}>Liste precedenti</button><span>{safePage + 1} / {Math.max(1, Math.ceil(matching.length / 10))}</span><button type="button" disabled={disabled || (safePage + 1) * 10 >= matching.length} onclick={() => { page = safePage + 1; }}>Liste successive</button>
  <button type="button" {disabled} onclick={() => { search = ''; page = Math.floor(lists.length / 10); onaction(() => addReformList(scenario, scope)); }}>Aggiungi lista</button></div>
  {#if scope !== 'common'}<details><summary>Copia configurazione politica</summary>
    <p>Copia liste e corrispondenze dall'altro ramo. I depositi già esistenti restano solo per le liste mantenute; le nuove liste richiedono depositi espliciti.</p>
    <button type="button" {disabled} onclick={() => onaction(() => copyReformPoliticalConfiguration(scenario, scope === 'camera' ? 'senato' : 'camera', scope))}>Copia liste e corrispondenze {scope === 'camera' ? 'Senato → Camera' : 'Camera → Senato'}</button>
  </details>{/if}
</section>
