<script lang="ts">
  import type { ReformGenerationScenario } from './generation-types';
  import type { ReformEditorCatalog } from './editor-catalog';
  import { editableLists, setReformCoverage } from './scenario-editor';
  let { scenario, catalog, disabled, onaction }: {
    scenario: ReformGenerationScenario; catalog: ReformEditorCatalog; disabled: boolean;
    onaction: (action: () => ReformGenerationScenario) => void;
  } = $props();
  let chamber = $state<'' | 'camera' | 'senato'>('');
  let listId = $state('');
  let regionId = $state('');
  let kind = $state('ordinary');
  let page = $state(0);
  const lists = $derived(editableLists(scenario, 'common'));
  const selected = $derived(lists.find(l => l.id === listId)?.id ?? lists[0]?.id ?? '');
  const colleges = $derived(catalog.colleges.filter(c => (!chamber || chamber === c.chamber) && scenario.lists[c.chamber].some(l => l.id === selected) && (!kind || c.kind === kind)
    && (!regionId || catalog.territories.find(t => t.id === c.territoryId)?.regionId === regionId)));
  const safePage = $derived(Math.min(page, Math.max(0, Math.ceil(colleges.length / 25) - 1)));
  const rows = $derived(colleges.slice(safePage * 25, (safePage + 1) * 25));
  function apply(filed: boolean, collegeId?: string) { onaction(() => {
    let n = scenario;
    for (const ch of ['camera', 'senato'] as const) {
      if (!n.lists[ch].some(l => l.id === selected)) continue;
      n = setReformCoverage(n, ch, colleges.filter(c => c.chamber === ch && (!collegeId || c.id === collegeId)).map(c => c.id), selected, filed);
    }
    return n;
  }); }
</script>
<section aria-label="Depositi liste">
  <h3>Depositi</h3>
  <p>Per Altre liste la copertura indica dove simulare i voti e non genera candidature.</p>
  <p>Assente dal collegio e depositata con zero voti sono condizioni diverse. In modalità comune le azioni filtrate riguardano entrambi i rami; per modificare un singolo collegio abilita Camera/Senato.</p>
  <div class="editor-row">
    <label>Ramo deposito<select bind:value={chamber} onchange={() => { page = 0; }} {disabled}><option value="">Tutti i rami</option><option value="camera">Camera</option><option value="senato">Senato</option></select></label>
    <label>Lista deposito<select value={selected} {disabled} onchange={e => { listId = e.currentTarget.value; }}>{#each lists as list (list.id)}<option value={list.id}>{list.name}</option>{/each}</select></label>
    <label>Regione deposito<select bind:value={regionId} onchange={() => { page = 0; }} {disabled}><option value="">Tutte</option>{#each catalog.regions as r (r.id)}<option value={r.id}>{r.name}</option>{/each}</select></label>
    <label>Tipo collegio<select bind:value={kind} onchange={() => { page = 0; }} {disabled}><option value="ordinary">Ordinario</option><option value="special-uninominal">Uninominale speciale</option><option value="taa-proportional">Proporzionale TAA</option><option value="">Tutti</option></select></label>
    <button type="button" disabled={disabled || !selected} onclick={() => apply(true)}>Deposita nei collegi filtrati ({colleges.length})</button>
    <button type="button" disabled={disabled || !selected} onclick={() => apply(false)}>Ritira dai collegi filtrati ({colleges.length})</button>
  </div>
  <div class="editor-table-wrap"><table><caption>Depositi della lista selezionata</caption><thead><tr><th>Ramo</th><th>Collegio</th><th>Depositata</th></tr></thead><tbody>
    {#each rows as college (college.id)}
      {@const filed = scenario.coverage.some(c => c.chamber === college.chamber && c.collegeId === college.id && c.listIds.includes(selected))}
      <tr><td>{college.chamber}</td><td>{college.name}</td><td>
      <input type="checkbox" aria-label={`Deposito ${college.name}`} checked={filed} {disabled} onchange={e => apply(e.currentTarget.checked, college.id)} />
    </td></tr>{/each}
  </tbody></table></div>
  <button type="button" disabled={safePage === 0} onclick={() => { page = safePage - 1; }}>Depositi precedenti</button>
  <span>{safePage + 1} / {Math.max(1, Math.ceil(colleges.length / 25))}</span>
  <button type="button" disabled={(safePage + 1) * 25 >= colleges.length} onclick={() => { page = safePage + 1; }}>Depositi successivi</button>
</section>
