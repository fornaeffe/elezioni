<script lang="ts">
  import { Trash2, Settings2 } from '@lucide/svelte';
  import type { PoliticalList } from './input';
  import type { ReformGenerationScenario } from './generation-types';
  import type { ReformEditorCatalog } from './editor-catalog';
  import { addReformCoalition, addReformList, editableLists, removeReformCoalition, removeReformList, updatePresentList, coalitionChambers, setCoalitionPresence, setListPresence, setReformOtherListEnabled } from './scenario-editor';
  import { upgradeReformDraft } from './presentation-metadata';
  let { scenario, catalog, disabled, onaction }: {
    scenario: ReformGenerationScenario; catalog: ReformEditorCatalog; disabled: boolean;
    onaction: (action: () => ReformGenerationScenario) => void;
  } = $props();
  const lists = $derived(editableLists(scenario, 'common'));
  let openDetails = $state<string[]>([]);
  function toggle(id: string) { openDetails = openDetails.includes(id) ? openDetails.filter(v => v !== id) : [...openDetails, id]; }
  let search = $state('');
  let page = $state(0);
  const matching = $derived(lists.filter(l => l.kind !== 'other').filter(l => `${l.name} ${l.id}`.toLocaleLowerCase('it-IT').includes(search.toLocaleLowerCase('it-IT'))));
  const safePage = $derived(Math.min(page, Math.max(0, Math.ceil(matching.length / 10) - 1)));
  function metadata(change: (s: ReformGenerationScenario) => void) { onaction(() => { const n = upgradeReformDraft(scenario); change(n); return n; }); }
  function patch(id: string, value: Partial<Omit<PoliticalList, 'id'>>) { onaction(() => updatePresentList(scenario, id, value)); }
</script>
<section id="r26-subjects" aria-label="Liste e coalizioni">
  <h3>Coalizioni</h3>
  <p>Nomi e colori sono condivisi. Apri le impostazioni per dichiarare i rami di presenza; le coalizioni presenti in entrambi sono collegate automaticamente quando hanno liste in entrambi.</p>
  {#each scenario.presentation?.coalitions ?? [] as coalition (coalition.id)}
    <div class="editor-row subject-row coalition-row">
      <button class="subject-settings" type="button" title={`Presenza Camera/Senato — ${coalition.name}`} aria-label={`Impostazioni coalizione ${coalition.name}`} aria-expanded={openDetails.includes(coalition.id)} aria-controls={`details-${coalition.id}`} {disabled} onclick={() => toggle(coalition.id)}><Settings2 size={18} aria-hidden="true" />{#if coalitionChambers(scenario, coalition.id).length !== 2}<span class="subject-badge" aria-label="Presenza limitata">!</span>{/if}</button>
      <input type="color" aria-label={`Colore coalizione ${coalition.name}`} value={coalition.color} {disabled} onchange={e => metadata(n => { n.presentation!.coalitions.find(c => c.id === coalition.id)!.color = e.currentTarget.value; })} />
      <label class="subject-name">Nome coalizione<input value={coalition.name} {disabled} onchange={e => { const name = e.currentTarget.value; metadata(n => { n.presentation!.coalitions.find(c => c.id === coalition.id)!.name = name; }); }} /></label>
      <button type="button" title={`Rimuovi coalizione ${coalition.name}`} aria-label={`Rimuovi coalizione ${coalition.name}`} {disabled} onclick={() => onaction(() => removeReformCoalition(scenario, coalition.id))}><Trash2 size={18} aria-hidden="true" /></button>
    </div>
    <div id={`details-${coalition.id}`} hidden={!openDetails.includes(coalition.id)} class="subject-details">
      {#each ['camera', 'senato'] as ch (ch)}<label><input type="checkbox" aria-label={`Presenza coalizione ${coalition.name} ${ch}`} checked={coalitionChambers(scenario, coalition.id).includes(ch as 'camera' | 'senato')} {disabled} onchange={e => onaction(() => setCoalitionPresence(scenario, coalition.id, ch as 'camera' | 'senato', e.currentTarget.checked))} />{ch === 'camera' ? 'Camera' : 'Senato'}</label>{/each}
      <p>Il collegamento automatico diventa operativo quando la coalizione ha almeno una lista in ciascun ramo.</p>
    </div>
  {/each}
  <button type="button" {disabled} onclick={() => onaction(() => addReformCoalition(scenario))}>Aggiungi coalizione</button>
  <h3>Liste</h3>
  <label><input type="checkbox" role="switch" aria-label="Includi Altre liste" checked={lists.some(l => l.kind === 'other')} {disabled} onchange={e => onaction(() => setReformOtherListEnabled(scenario, e.currentTarget.checked))} />Includi Altre liste</label>
  {#each lists.filter(l => l.kind === 'other') as other (other.id)}
    <div class="editor-card" data-testid="other-list-row"><strong><span style:color="#9ca3af" aria-hidden="true">●</span> Altre liste</strong><p>Categoria di voti validi sempre esclusa dai seggi, senza candidati e coalizione. Quote, copertura geografica e corrispondenze sono nelle rispettive sezioni.</p>
      {#each ['camera', 'senato'] as ch (ch)}<label><input type="checkbox" aria-label={`Presenza lista Altre liste ${ch}`} checked={scenario.lists[ch as 'camera' | 'senato'].some(l => l.id === other.id)} {disabled} onchange={e => onaction(() => setListPresence(scenario, other.id, ch as 'camera' | 'senato', e.currentTarget.checked))} />{ch === 'camera' ? 'Camera' : 'Senato'}</label>{/each}
    </div>
  {/each}
  <p>Le nuove liste iniziano senza depositi: seleziona i collegi nella sezione Depositi. I nomi non cambiano le identità salvate.</p>
  <label>Cerca lista<input type="search" bind:value={search} oninput={() => { page = 0; }} {disabled} /></label>
  <p>{matching.length} liste; fino a 10 per pagina, compresi i riferimenti storici speciali.</p>
  {#each matching.slice(safePage * 10, (safePage + 1) * 10) as list (list.id)}
    {@const minority = [...scenario.lists.camera, ...scenario.lists.senato].find(l => l.id === list.id && l.protectedMinority)?.protectedMinority ?? null}
    <div class="editor-card" data-testid="reform-list-row">
      <div class="editor-row subject-row list-row">
        <button class="subject-settings" type="button" title={`Presenza Camera/Senato e tutela — ${list.name}`} aria-label={`Impostazioni lista ${list.name}`} aria-expanded={openDetails.includes(list.id)} aria-controls={`details-${list.id}`} {disabled} onclick={() => toggle(list.id)}><Settings2 size={18} aria-hidden="true" />{#if !scenario.lists.camera.some(l => l.id === list.id) || !scenario.lists.senato.some(l => l.id === list.id) || [...scenario.lists.camera, ...scenario.lists.senato].some(l => l.id === list.id && l.protectedMinority)}<span class="subject-badge" aria-label="Presenza limitata o tutela dichiarata">!</span>{/if}</button>
        <input type="color" aria-label={`Colore lista ${list.name}`} value={scenario.presentation?.lists.find(l => l.id === list.id)?.color ?? '#64748b'} {disabled}
          onchange={e => { const color = e.currentTarget.value; metadata(n => { n.presentation!.lists.find(l => l.id === list.id)!.color = color; }); }} />
        <label class="subject-name">Nome lista<input value={list.name} {disabled} onchange={e => patch(list.id, { name: e.currentTarget.value })} /></label>
        <label class="subject-coalition">Coalizione<select value={list.coalitionId ?? ''} {disabled} onchange={e => patch(list.id, { coalitionId: e.currentTarget.value || null })}>
          <option value="">Lista autonoma</option>
          {#each scenario.presentation?.coalitions ?? [] as coalition (coalition.id)}<option value={coalition.id}>{coalition.name}</option>{/each}
        </select></label>
        <button type="button" title={`Rimuovi lista ${list.name}`} aria-label={`Rimuovi lista ${list.name}`} {disabled} onclick={() => onaction(() => removeReformList(scenario, 'common', list.id))}><Trash2 size={18} aria-hidden="true" /></button>
      </div>
      <div id={`details-${list.id}`} hidden={!openDetails.includes(list.id)} class="subject-details">
        {#each ['camera', 'senato'] as ch (ch)}<label><input type="checkbox" aria-label={`Presenza lista ${list.name} ${ch}`} checked={scenario.lists[ch as 'camera' | 'senato'].some(l => l.id === list.id)} {disabled} onchange={e => onaction(() => setListPresence(scenario, list.id, ch as 'camera' | 'senato', e.currentTarget.checked))} />{ch === 'camera' ? 'Camera' : 'Senato'}</label>{/each}
        <h4>Minoranza linguistica protetta</h4>
        <label><input type="checkbox" checked={minority !== null} {disabled} onchange={e => patch(list.id, { protectedMinority: e.currentTarget.checked ? { regionId: '04', legalReference: '' } : null })} />Dichiara tutela</label>
        {#if minority}
          <label>Regione tutela<select value={minority.regionId} {disabled} onchange={e => patch(list.id, { protectedMinority: { ...minority!, regionId: e.currentTarget.value } })}>
            {#each catalog.regions as region (region.id)}<option value={region.id}>{region.name}</option>{/each}
          </select></label>
          <label>Riferimento tutela<input value={minority.legalReference} {disabled} onchange={e => patch(list.id, { protectedMinority: { ...minority!, legalReference: e.currentTarget.value } })} /></label>
          <p>La presenza storica sul territorio non implica automaticamente una tutela.</p>
        {/if}
      </div>
    </div>
  {/each}
  <div class="editor-row"><button type="button" disabled={disabled || safePage === 0} onclick={() => { page = safePage - 1; }}>Liste precedenti</button><span>{safePage + 1} / {Math.max(1, Math.ceil(matching.length / 10))}</span><button type="button" disabled={disabled || (safePage + 1) * 10 >= matching.length} onclick={() => { page = safePage + 1; }}>Liste successive</button>
  <button type="button" {disabled} onclick={() => { search = ''; page = Math.floor(lists.length / 10); onaction(() => addReformList(scenario, 'common')); }}>Aggiungi lista</button></div>
</section>
