<script lang="ts">
  import type { PoliticsScenario } from '../scenario/schema';
  import { chambers, REFORM } from '../scenario/schema';
  import { capabilities } from '../scenario/registry';
  import { createCoalition, createList, updateCoalition, updateList, deleteSubject } from '../scenario/edits';
  import type { EditorAssets } from './assets';
  import type { ApplyEdit } from './model';
  import LazySection from './LazySection.svelte';
  import BonusLinksEditor from './BonusLinksEditor.svelte';
  import FilingTree from './FilingTree.svelte';
  import ReferencesEditor from './ReferencesEditor.svelte';
  let { scenario, assets, edit }: { scenario: PoliticsScenario; assets: EditorAssets | null; edit: ApplyEdit } = $props();
  let coalitionName = $state(''), listName = $state('');
  function addList() {
    if (!assets) return;
    const id = `list-${crypto.randomUUID()}`;
    edit(s => createList(s, { id, name: listName, color: '#64748b', coalitionId: null, minority: null, kind: 'list' },
      { id: `new:${id}`, name: listName, support: 'Ipotesi esplicita senza conteggi storici', provenance: 'Nuova lista creata nello scenario',
        fallback: { M01: { policy: 'regional-or-equal-list-v1', provenance: 'M01: fallback regionale o uniforme' },
          M02: { geographicLogProfile: 0, profileSd: 0.45, localInnovationSd: 0.25, nationalInnovationSd: 0.35, provenance: 'Convenzione di creazione; parametri modificabili' } } }, assets.context));
    listName = '';
  }
</script>
<section aria-labelledby="coalitions-title">
  <h2 id="coalitions-title">Coalizioni</h2>
  {#each scenario.subjects.coalitions as coalition (coalition.id)}
    <article aria-label={coalition.name}>
      <div class="row">
        <label>Colore {coalition.name}<input type="color" value={coalition.color} onchange={e => edit(s => updateCoalition(s, { ...coalition, color: e.currentTarget.value }))} /></label>
        <label>Nome coalizione<input value={coalition.name} onchange={e => edit(s => updateCoalition(s, { ...coalition, name: e.currentTarget.value }))} /></label>
      </div>
      <LazySection title={`Opzioni coalizione · ${coalition.chambers.map(ch => ch === 'camera' ? 'Camera' : 'Senato').join(', ') || 'nessuna camera'}`}>
        {#each chambers as chamber (chamber)}
          <label><input type="checkbox" checked={coalition.chambers.includes(chamber)} onchange={e => edit(s => updateCoalition(s, { ...coalition,
            chambers: e.currentTarget.checked ? [...coalition.chambers, chamber] : coalition.chambers.filter(c => c !== chamber) }))} />{chamber === 'camera' ? 'Camera' : 'Senato'}</label>
        {/each}
        {#if capabilities(scenario).scrutiny.bonusLinks}<BonusLinksEditor {scenario} subject={{ kind: 'coalition', id: coalition.id }} {edit} />{/if}
        {#if assets}<LazySection title="Candidati della coalizione"><FilingTree {scenario} owner={{ kind: 'coalition', id: coalition.id }} {assets} {edit} /></LazySection>{/if}
        <button onclick={() => edit(s => deleteSubject(s, { kind: 'coalition', id: coalition.id }))}>Elimina coalizione</button>
      </LazySection>
    </article>
  {/each}
  <div class="row"><label>Nuova coalizione<input bind:value={coalitionName} /></label><button disabled={!coalitionName.trim()} onclick={() => { const name = coalitionName; edit(s => createCoalition(s, { id: `coalition-${crypto.randomUUID()}`, name, color: '#64748b' })); coalitionName = ''; }}>Aggiungi coalizione</button></div>
</section>
<section aria-labelledby="lists-title">
  <h2 id="lists-title">Liste</h2>
  {#each scenario.subjects.lists as list (list.id)}
    <article aria-label={list.name}>
      <div class="row">
        <label>Colore {list.name}<input type="color" disabled={list.kind === 'other'} value={list.color} onchange={e => edit(s => updateList(s, { ...list, color: e.currentTarget.value }))} /></label>
        <label>Nome lista<input disabled={list.kind === 'other'} value={list.name} onchange={e => edit(s => updateList(s, { ...list, name: e.currentTarget.value }))} /></label>
        <label>Coalizione<select disabled={list.kind === 'other'} value={list.coalitionId ?? ''} onchange={e => edit(s => updateList(s, { ...list, coalitionId: e.currentTarget.value || null }))}>
          <option value="">Lista autonoma</option>{#each scenario.subjects.coalitions as c (c.id)}<option value={c.id}>{c.name}</option>{/each}
        </select></label>
      </div>
      <ReferencesEditor {scenario} target={{ kind: 'list', id: list.id }} label="Riferimento nazionale" {assets} {edit} />
      <LazySection title={`Opzioni lista · ${scenario.lawSettings[scenario.selection.scrutiny.id].filings.find(f => f.listId === list.id)?.locations.length ?? 0} depositi salvati`}>
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
      </LazySection>
    </article>
  {/each}
  <div class="row"><label>Nuova lista<input bind:value={listName} /></label><button disabled={!listName.trim() || !assets} onclick={addList}>Aggiungi lista</button></div>
</section>
