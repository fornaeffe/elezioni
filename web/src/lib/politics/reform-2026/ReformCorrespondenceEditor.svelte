<script lang="ts">
  import SearchSelect from '../../scenario/SearchSelect.svelte';
  import type { ReformGenerationScenario } from './generation-types';
  import type { ReformEditorCatalog } from './editor-catalog';
  import { editableLists, editingChambers, resetReformCorrespondences, setReformCorrespondence, type EditingScope } from './scenario-editor';
  let { scenario, defaults, catalog, disabled, onaction }: {
    scenario: ReformGenerationScenario; defaults: ReformGenerationScenario; catalog: ReformEditorCatalog; disabled: boolean;
    onaction: (action: () => ReformGenerationScenario) => void;
  } = $props();
  let scope = $state<EditingScope>('common');
  let past = $state('');
  let destination = $state('');
  let factor = $state<number | undefined>(1);
  const destinations = $derived(editableLists(scenario, scope));
  const options = $derived(catalog.historicalLists.map(l => ({ id: l.id, label: `${l.name}${l.kind === 'special-ballot-group' ? ' (gruppo speciale)' : ''}` })));
  const chambers = $derived(editingChambers(scope));
  const unmapped = $derived(chambers.map(ch => ({ ch, count: catalog.historicalLists.filter(l => !scenario.correspondences.some(c => c.chamber === ch && c.pastListId === l.id)).length })));
</script>
<section aria-label="Corrispondenze storiche">
  <h3>Corrispondenze storiche</h3>
  <label>Ramo corrispondenze<select bind:value={scope} {disabled}><option value="common">Comune ai rami</option><option value="camera">Camera</option><option value="senato">Senato</option></select></label>
  <p>Base {catalog.historicalDate}. I pesi si normalizzano per lista storica e ramo. Più origini possono confluire nella stessa lista; le origini senza destinazioni confluiscono nell'astensione. I gruppi speciali non vengono scomposti in liste inventate.</p>
  {#each unmapped as row (row.ch)}<p>{row.ch}: {row.count} origini senza corrispondenza.</p>{/each}
  <SearchSelect label="lista storica" {options} value={past} onchange={v => { past = v; }} {disabled} />
  {#if past}
    {#each chambers as ch (ch)}
      {@const rows = scenario.correspondences.filter(c => c.chamber === ch && c.pastListId === past)}
      {@const total = rows.reduce((n, r) => n + r.factor, 0)}
      <h4>{ch === 'camera' ? 'Camera' : 'Senato'}</h4>
      {#if !rows.length}<p>Nessuna destinazione: astensione.</p>{/if}
      {#each rows as row (row.futureListId)}
        <div class="editor-row">
          <span>{scenario.lists[ch].find(l => l.id === row.futureListId)?.name ?? row.futureListId}</span>
          <label>Peso {ch}<input type="number" min="0" step="any" value={row.factor} {disabled} onchange={e => { const value = e.currentTarget.valueAsNumber; onaction(() => setReformCorrespondence(scenario, scope, past, row.futureListId, value)); }} /></label>
          <span>{total > 0 ? `${(row.factor / total * 100).toFixed(2)}%` : 'Somma pesi nulla'}</span>
          <button type="button" {disabled} onclick={() => onaction(() => setReformCorrespondence(scenario, scope, past, row.futureListId, undefined))}>Rimuovi destinazione {ch}</button>
        </div>
      {/each}
    {/each}
    <div class="editor-row">
      <label>Lista destinazione<select bind:value={destination} {disabled}><option value="">Seleziona</option>{#each destinations as list (list.id)}<option value={list.id}>{list.name}</option>{/each}</select></label>
      <label>Peso nuovo<input type="number" min="0" step="any" bind:value={factor} {disabled} /></label>
      <button type="button" disabled={disabled || !destination || factor === undefined} onclick={() => onaction(() => setReformCorrespondence(scenario, scope, past, destination, factor))}>Applica corrispondenza</button>
      <button type="button" {disabled} onclick={() => onaction(() => resetReformCorrespondences(scenario, defaults, scope, past))}>Ripristina origine dai valori iniziali</button>
    </div>
  {/if}
</section>
