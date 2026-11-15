<script lang="ts">
  import type { PoliticsScenario } from '../scenario/schema';
  import { chambers, REFORM } from '../scenario/schema';
  import { capabilities } from '../scenario/registry';
  import { createCoalition, createList, updateCoalition, updateList, deleteSubject, setMembers } from '../scenario/edits';
  import { OTHER_LIST_ID, OTHER_LIST_NAME, OTHER_LIST_COLOR } from '../../core/other-list';
  import type { EditorAssets } from './assets';
  import type { ApplyEdit } from './model';
  import LazySection from './LazySection.svelte';
  import BonusLinksEditor from './BonusLinksEditor.svelte';
  import FilingTree from './FilingTree.svelte';
  import ReferencesEditor from './ReferencesEditor.svelte';
  let { scenario, assets, edit }: { scenario: PoliticsScenario; assets: EditorAssets | null; edit: ApplyEdit } = $props();
  let openCoalitions = $state<Record<string, boolean>>({}), openLists = $state<Record<string, boolean>>({});
  let coalitionName = $state(''), listName = $state(''), otherMembers = $state<string[]>([]);
  const other = $derived(scenario.subjects.lists.find(l => l.kind === 'other'));
  const otherOptions = $derived([
    ...(assets?.components.filter(c => !scenario.history.pooling.some(p => p.members.includes(c.id))) ?? []),
    ...scenario.history.pooling.map(p => ({ id: p.id, name: scenario.subjects.components.find(c => c.id === p.id)?.name ?? p.id }))
  ].filter(c => !scenario.memberships.some(m => m.camera.includes(c.id) || m.senato.includes(c.id))));
  function newList(s: PoliticsScenario, id: string, name: string, kind: 'list' | 'other') {
    const componentId = kind === 'other' ? `new:${id}:${crypto.randomUUID()}` : `new:${id}`;
    return createList(s, { id, name, color: kind === 'other' ? OTHER_LIST_COLOR : '#64748b', coalitionId: null, minority: null, kind },
      { id: componentId, name, support: 'Ipotesi esplicita senza conteggi storici', provenance: 'Nuova lista creata nello scenario',
        fallback: { M01: { policy: 'regional-or-equal-list-v1', provenance: 'M01: fallback regionale o uniforme' },
          M02: { geographicLogProfile: 0, profileSd: 0.45, localInnovationSd: 0.25, nationalInnovationSd: 0.35, provenance: 'Convenzione di creazione; parametri modificabili' } } }, assets!.context);
  }
  function addList() {
    if (!assets) return;
    const id = `list-${crypto.randomUUID()}`;
    edit(s => newList(s, id, listName, 'list'));
    listName = '';
  }
  function addOther() {
    if (!assets) return;
    edit(s => {
      if (!otherMembers.length) throw new Error('Scegli esplicitamente i componenti del residuo.');
      return setMembers(newList(s, OTHER_LIST_ID, OTHER_LIST_NAME, 'other'), OTHER_LIST_ID, 'camera', otherMembers, assets!.context);
    });
  }
</script>
<section aria-labelledby="coalitions-title">
  <h2 id="coalitions-title">Coalizioni</h2>
  <div class="subject-header coalition-grid" aria-hidden="true"><span>Colore</span><span>Nome coalizione</span><span></span></div>
  {#each scenario.subjects.coalitions as coalition (coalition.id)}
    <article aria-label={coalition.name}>
      <div class="subject-row coalition-grid">
        <label class="subject-color"><span class="visually-hidden">Colore {coalition.name}</span><input type="color" value={coalition.color} onchange={e => edit(s => updateCoalition(s, { ...coalition, color: e.currentTarget.value }))} /></label>
        <label class="subject-name"><span class="mobile-label" aria-hidden="true">Nome</span><span class="visually-hidden">Nome coalizione</span><input value={coalition.name} onchange={e => edit(s => updateCoalition(s, { ...coalition, name: e.currentTarget.value }))} /></label>
        <button class="subject-options" type="button" aria-label={`Opzioni coalizione ${coalition.name}`} aria-expanded={!!openCoalitions[coalition.id]} aria-controls={`coalition-options-${coalition.id}`} onclick={() => openCoalitions[coalition.id] = !openCoalitions[coalition.id]}><span aria-hidden="true">⚙</span></button>
      </div>
      <div id={`coalition-options-${coalition.id}`} hidden={!openCoalitions[coalition.id]}>
        {#if openCoalitions[coalition.id]}
        <p>Camere: {coalition.chambers.map(ch => ch === 'camera' ? 'Camera' : 'Senato').join(', ') || 'nessuna camera'}</p>
        {#each chambers as chamber (chamber)}
          <label><input type="checkbox" checked={coalition.chambers.includes(chamber)} onchange={e => edit(s => updateCoalition(s, { ...coalition,
            chambers: e.currentTarget.checked ? [...coalition.chambers, chamber] : coalition.chambers.filter(c => c !== chamber) }))} />{chamber === 'camera' ? 'Camera' : 'Senato'}</label>
        {/each}
        {#if capabilities(scenario).scrutiny.bonusLinks}<BonusLinksEditor {scenario} subject={{ kind: 'coalition', id: coalition.id }} {edit} />{/if}
        {#if assets}<LazySection title="Candidati della coalizione"><FilingTree {scenario} owner={{ kind: 'coalition', id: coalition.id }} {assets} {edit} /></LazySection>{/if}
        <button onclick={() => edit(s => deleteSubject(s, { kind: 'coalition', id: coalition.id }))}>Elimina coalizione</button>
        {/if}
      </div>
    </article>
  {/each}
  <div class="row"><label>Nuova coalizione<input bind:value={coalitionName} /></label><button disabled={!coalitionName.trim()} onclick={() => { const name = coalitionName; edit(s => createCoalition(s, { id: `coalition-${crypto.randomUUID()}`, name, color: '#64748b' })); coalitionName = ''; }}>Aggiungi coalizione</button></div>
</section>
<section aria-labelledby="lists-title">
  <h2 id="lists-title">Liste</h2>
  <div class="subject-header list-grid" aria-hidden="true"><span>Colore</span><span>Nome lista</span><span>Coalizione</span><span>Riferimento nazionale (%)</span><span></span></div>
  {#each scenario.subjects.lists as list (list.id)}
    <article aria-label={list.name}>
      <div class="subject-row list-grid">
        <label class="subject-color"><span class="visually-hidden">Colore {list.name}</span><input type="color" disabled={list.kind === 'other'} value={list.color} onchange={e => edit(s => updateList(s, { ...list, color: e.currentTarget.value }))} /></label>
        <label class="subject-name"><span class="mobile-label" aria-hidden="true">Nome</span><span class="visually-hidden">Nome lista</span><input disabled={list.kind === 'other'} value={list.name} onchange={e => edit(s => updateList(s, { ...list, name: e.currentTarget.value }))} /></label>
        <label class="subject-coalition"><span class="mobile-label" aria-hidden="true">Coalizione</span><span class="visually-hidden">Coalizione</span><select disabled={list.kind === 'other'} value={list.coalitionId ?? ''} onchange={e => edit(s => updateList(s, { ...list, coalitionId: e.currentTarget.value || null }))}>
          <option value="">Lista autonoma</option>{#each scenario.subjects.coalitions as c (c.id)}<option value={c.id}>{c.name}</option>{/each}
        </select></label>
        <div class="subject-reference"><ReferencesEditor {scenario} target={{ kind: 'list', id: list.id }} label="Riferimento nazionale" {assets} {edit} mode="basic" /></div>
        <button class="subject-options" type="button" aria-label={`Opzioni lista ${list.name}`} aria-expanded={!!openLists[list.id]} aria-controls={`list-options-${list.id}`} onclick={() => openLists[list.id] = !openLists[list.id]}><span aria-hidden="true">⚙</span></button>
      </div>
      <div id={`list-options-${list.id}`} hidden={!openLists[list.id]}>
        {#if openLists[list.id]}
        <p>{scenario.lawSettings[scenario.selection.scrutiny.id].filings.find(f => f.listId === list.id)?.locations.length ?? 0} depositi salvati</p>
        <ReferencesEditor {scenario} target={{ kind: 'list', id: list.id }} label="Riferimento nazionale" {assets} {edit} mode="advanced" />
        <p>La partecipazione è determinata dai depositi. Nome, coalizione e minoranza sono comuni alle camere.</p>
        {#if list.kind !== 'other'}
          <label>Regione di minoranza<select value={list.minority?.regionId ?? ''} onchange={e => edit(s => updateList(s, { ...list, minority: e.currentTarget.value ? {
            regionId: e.currentTarget.value, namespace: s.data.referenceGeography.version, legalReference: list.minority?.legalReference ?? null } : null }))}>
            <option value="">Nessuna dichiarazione</option>{#each assets?.references ?? [] as r (JSON.stringify(r.scope))}{#if r.scope.kind === 'region'}<option value={r.scope.code}>{r.name}</option>{/if}{/each}
          </select></label>
          {#if list.minority}<label>Riferimento normativo minoranza<input value={list.minority.legalReference ?? ''} onchange={e => edit(s => updateList(s, { ...list, minority: { ...list.minority!, legalReference: e.currentTarget.value || null } }))} /></label>{/if}
        {/if}
        {#if capabilities(scenario).scrutiny.bonusLinks}<BonusLinksEditor {scenario} subject={{ kind: 'list', id: list.id }} {edit} />{/if}
        {#if assets}<LazySection title={`Depositi e candidati · ${scenario.lawSettings[scenario.selection.scrutiny.id].placements.filter(p => p.owner.kind === 'list' && p.owner.id === list.id).length} candidati fissati`}><FilingTree {scenario} owner={{ kind: 'list', id: list.id }} {assets} {edit} /></LazySection>{/if}
        <button onclick={() => edit(s => deleteSubject(s, { kind: 'list', id: list.id }))}>Elimina lista</button>
        {/if}
      </div>
    </article>
  {/each}
  <div class="row"><label>Nuova lista<input bind:value={listName} /></label><button disabled={!listName.trim() || !assets} onclick={addList}>Aggiungi lista</button></div>
  <fieldset aria-label="Altre liste"><legend>Altre liste</legend>
    <p>Categoria residuale autonoma, senza candidati né seggi. Scegli componenti liberi da altre liste; Camera e Senato sono inizialmente collegati.</p>
    {#if other}
      <button onclick={() => { edit(s => deleteSubject(s, { kind: 'list', id: other.id })); otherMembers = []; }}>Disattiva Altre liste</button>
    {:else}
      <label>Componenti del residuo<select multiple size="4" bind:value={otherMembers}>
        {#each otherOptions as c (c.id)}<option value={c.id}>{c.name}</option>{/each}
      </select></label>
      {#if !otherOptions.length}<p>Nessun componente libero: modifica prima le appartenenze nelle impostazioni avanzate.</p>{/if}
      <button disabled={!assets} onclick={addOther}>Attiva Altre liste</button>
    {/if}
  </fieldset>
</section>
