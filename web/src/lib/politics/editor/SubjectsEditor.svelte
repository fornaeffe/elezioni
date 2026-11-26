<script lang="ts">
  import Toggletip from '$lib/ui/Toggletip.svelte';
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
  import MembershipsEditor from './MembershipsEditor.svelte';
  import { referenceQuotaHelp } from './reference-help';
  let { scenario, assets, edit }: { scenario: PoliticsScenario; assets: EditorAssets | null; edit: ApplyEdit } = $props();
  let openCoalitions = $state<Record<string, boolean>>({}), openLists = $state<Record<string, boolean>>({});
  let coalitionName = $state(''), listName = $state(''), otherMembers = $state<string[]>([]);
  let otherSetup = $state(false), otherOpen = $state(false), otherSearch = $state(''), otherConfirm = $state(false);
  const other = $derived(scenario.subjects.lists.find(l => l.kind === 'other'));
  const otherMembership = $derived(scenario.memberships.find(m => m.listId === other?.id));
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
        <label data-field class="subject-color"><span class="visually-hidden">Colore {coalition.name}</span><input type="color" value={coalition.color} onchange={e => edit(s => updateCoalition(s, { ...coalition, color: e.currentTarget.value }))} /></label>
        <label data-field class="subject-name"><span class="mobile-label" aria-hidden="true">Nome</span><span class="visually-hidden">Nome coalizione</span><input value={coalition.name} onchange={e => edit(s => updateCoalition(s, { ...coalition, name: e.currentTarget.value }))} /></label>
        <button class="subject-options" type="button" aria-label={`Opzioni coalizione ${coalition.name}`} title={`Opzioni avanzate per ${coalition.name}`} aria-expanded={!!openCoalitions[coalition.id]} aria-controls={`coalition-options-${coalition.id}`} onclick={() => openCoalitions[coalition.id] = !openCoalitions[coalition.id]}><span aria-hidden="true">⚙</span></button>
      </div>
      <div id={`coalition-options-${coalition.id}`} hidden={!openCoalitions[coalition.id]}>
        {#if openCoalitions[coalition.id]}
        <p>Coalizione presente in:</p>
        {#each chambers as chamber (chamber)}
          <label data-field><input type="checkbox" checked={coalition.chambers.includes(chamber)} onchange={e => edit(s => updateCoalition(s, { ...coalition,
            chambers: e.currentTarget.checked ? [...coalition.chambers, chamber] : coalition.chambers.filter(c => c !== chamber) }))} />{chamber === 'camera' ? 'Camera' : 'Senato'}</label>
        {/each}
        {#if capabilities(scenario).scrutiny.bonusLinks}<BonusLinksEditor {scenario} subject={{ kind: 'coalition', id: coalition.id }} {edit} />{/if}
        {#if assets}<LazySection title="Candidati della coalizione"><FilingTree {scenario} owner={{ kind: 'coalition', id: coalition.id }} {assets} {edit} /></LazySection>{/if}
        <button onclick={() => edit(s => deleteSubject(s, { kind: 'coalition', id: coalition.id }))}>Elimina coalizione</button>
        {/if}
      </div>
    </article>
  {/each}
  <div class="row"><label data-field>Nuova coalizione<input bind:value={coalitionName} /></label><button disabled={!coalitionName.trim()} onclick={() => { const name = coalitionName; edit(s => createCoalition(s, { id: `coalition-${crypto.randomUUID()}`, name, color: '#64748b' })); coalitionName = ''; }}>Aggiungi coalizione</button></div>
</section>
<section aria-labelledby="lists-title">
  <h2 id="lists-title">Liste</h2>
  <div class="subject-header list-grid">
    <span aria-hidden="true">Colore</span><span aria-hidden="true">Nome lista</span><span aria-hidden="true">Coalizione</span>
    <span class="field-caption">
      <span aria-hidden="true">Riferimento nazionale (%)</span>
      <Toggletip label="Riferimento nazionale (%)" text={referenceQuotaHelp(capabilities(scenario).generator.id, 'list')} />
    </span>
  </div>
  {#each scenario.subjects.lists.filter(l => l.kind !== 'other') as list (list.id)}
    <article aria-label={list.name}>
      <div class="subject-row list-grid">
        <label data-field class="subject-color"><span class="visually-hidden">Colore {list.name}</span><input type="color" disabled={list.kind === 'other'} value={list.color} onchange={e => edit(s => updateList(s, { ...list, color: e.currentTarget.value }))} /></label>
        <label data-field class="subject-name"><span class="mobile-label" aria-hidden="true">Nome</span><span class="visually-hidden">Nome lista</span><input disabled={list.kind === 'other'} value={list.name} onchange={e => edit(s => updateList(s, { ...list, name: e.currentTarget.value }))} /></label>
        <label data-field class="subject-coalition"><span class="mobile-label" aria-hidden="true">Coalizione</span><span class="visually-hidden">Coalizione</span><select disabled={list.kind === 'other'} value={list.coalitionId ?? ''} onchange={e => edit(s => updateList(s, { ...list, coalitionId: e.currentTarget.value || null }))}>
          <option value="">Lista autonoma</option>{#each scenario.subjects.coalitions as c (c.id)}<option value={c.id}>{c.name}</option>{/each}
        </select></label>
        <div class="subject-reference"><ReferencesEditor {scenario} target={{ kind: 'list', id: list.id }} label="Riferimento nazionale" {assets} {edit} mode="basic" hideCaptionOnWide /></div>
        <button class="subject-options" type="button" aria-label={`Opzioni lista ${list.name}`} title={`Opzioni avanzate per ${list.name}`} aria-expanded={!!openLists[list.id]} aria-controls={`list-options-${list.id}`} onclick={() => openLists[list.id] = !openLists[list.id]}><span aria-hidden="true">⚙</span></button>
      </div>
      <div id={`list-options-${list.id}`} hidden={!openLists[list.id]}>
        {#if openLists[list.id]}
          <ReferencesEditor {scenario} target={{ kind: 'list', id: list.id }} label="Riferimento nazionale" {assets} {edit} mode="advanced" />
          {#if list.kind !== 'other'}
            <label data-field>Regione di minoranza<select value={list.minority?.regionId ?? ''} onchange={e => edit(s => updateList(s, { ...list, minority: e.currentTarget.value ? {
              regionId: e.currentTarget.value, namespace: s.data.referenceGeography.version, legalReference: list.minority?.legalReference ?? null } : null }))}>
              <option value="">Nessuna dichiarazione</option>{#each assets?.references ?? [] as r (JSON.stringify(r.scope))}{#if r.scope.kind === 'region'}<option value={r.scope.code}>{r.name}</option>{/if}{/each}
            </select></label>
            {#if list.minority}<label data-field>Riferimento normativo minoranza<input value={list.minority.legalReference ?? ''} onchange={e => edit(s => updateList(s, { ...list, minority: { ...list.minority!, legalReference: e.currentTarget.value || null } }))} /></label>{/if}
          {/if}
          {#if capabilities(scenario).scrutiny.bonusLinks}<BonusLinksEditor {scenario} subject={{ kind: 'list', id: list.id }} {edit} />{/if}
          <p>Lista depositata in {scenario.lawSettings[scenario.selection.scrutiny.id].filings.find(f => f.listId === list.id)?.locations.length ?? 0} collegi</p>
          {#if assets}
            <LazySection title="Depositi e candidati" count={{
              value: scenario.lawSettings[scenario.selection.scrutiny.id].placements.filter(p => p.owner.kind === 'list' && p.owner.id === list.id).length,
              label: 'candidati fissati'
            }}>
              <FilingTree {scenario} owner={{ kind: 'list', id: list.id }} {assets} {edit} />
            </LazySection>
          {/if}
          <button onclick={() => edit(s => deleteSubject(s, { kind: 'list', id: list.id }))}>Elimina lista</button>
        {/if}
      </div>
    </article>
  {/each}
  <div class="row"><label data-field>Nuova lista<input bind:value={listName} /></label><button disabled={!listName.trim() || !assets} onclick={addList}>Aggiungi lista</button></div>
  <article aria-label="Altre liste" class="other-card">
    <div class="other-row">
      <span class="other-dot" aria-hidden="true" style:color={OTHER_LIST_COLOR}>{other ? '●' : '○'}</span>
      <div class="other-identity"><strong>Altre liste</strong><small>Categoria residuale · senza candidati né seggi</small></div>
      {#if other}
        <div class="other-reference"><ReferencesEditor {scenario} target={{ kind: 'list', id: other.id }} label="Riferimento nazionale" {assets} {edit} mode="basic" /></div>
        <button type="button" aria-expanded={otherOpen} aria-controls="other-options" onclick={() => otherOpen = !otherOpen}>Opzioni {otherOpen ? '▴' : '▾'}</button>
      {:else}
        <button type="button" aria-expanded={otherSetup} aria-controls={otherSetup ? 'other-setup' : undefined} disabled={!assets} onclick={() => otherSetup = !otherSetup}>Attiva</button>
      {/if}
    </div>
    {#if other}
      <div id="other-options" hidden={!otherOpen}>
        {#if otherOpen}
          {#if assets}<LazySection title={`Componenti · ${otherMembership?.locked ? 'Camera e Senato collegati' : 'Camere separate'} · Camera ${otherMembership?.camera.length ?? 0}, Senato ${otherMembership?.senato.length ?? 0}`}><MembershipsEditor {scenario} {assets} {edit} listId={other.id} /></LazySection>{/if}
          <ReferencesEditor {scenario} target={{ kind: 'list', id: other.id }} label="Riferimento nazionale" {assets} {edit} mode="advanced" />
          {#if assets}<LazySection title="Copertura territoriale"><p>La copertura determina dove la categoria riceve voti nella legge selezionata.</p><FilingTree {scenario} owner={{ kind: 'list', id: other.id }} {assets} {edit} /></LazySection>{/if}
          {#if otherConfirm}
            <div class="other-confirm"><p>La disattivazione rimuove Altre liste e i suoi riferimenti e depositi. La riattivazione non ripristina i riferimenti precedenti.</p><button onclick={() => otherConfirm = false}>Annulla</button><button onclick={() => { edit(s => deleteSubject(s, { kind: 'list', id: other.id })); otherMembers = []; otherConfirm = false; otherOpen = false; }}>Conferma disattivazione</button></div>
          {:else}<button onclick={() => otherConfirm = true}>Disattiva Altre liste</button>{/if}
        {/if}
      </div>
    {:else if otherSetup}
      <div id="other-setup" class="other-setup">
        <fieldset><legend>Componenti del residuo</legend>
          <p>Seleziona almeno un componente libero. Camera e Senato saranno inizialmente collegati.</p>
          {#if !assets}<p>Dati in caricamento.</p>
          {:else if !otherOptions.length}<p>Nessun componente libero: modifica prima le appartenenze nelle impostazioni avanzate.</p>
          {:else}
            <label data-field>Cerca componente<input type="search" bind:value={otherSearch} /></label>
            <div class="bounded-list other-choices">
              {#each otherOptions.filter(c => c.name.toLocaleLowerCase('it').includes(otherSearch.toLocaleLowerCase('it'))) as c (c.id)}
                <label data-field><input type="checkbox" value={c.id} checked={otherMembers.includes(c.id)} onchange={e => otherMembers = e.currentTarget.checked ? [...otherMembers, c.id] : otherMembers.filter(id => id !== c.id)} />{c.name}</label>
              {:else}<p>Nessun componente corrisponde alla ricerca.</p>{/each}
            </div>
          {/if}
        </fieldset>
        <div class="other-actions"><span>{otherMembers.length} selezionati</span><button onclick={() => { otherSetup = false; otherMembers = []; otherSearch = ''; }}>Annulla</button><button disabled={!assets || !otherMembers.length || otherMembers.some(id => !otherOptions.some(c => c.id === id))} onclick={() => { addOther(); otherSetup = false; otherSearch = ''; }}>Attiva Altre liste</button></div>
      </div>
    {/if}
  </article>
</section>

<style>
  section + section { margin-block-start: 1.5rem; }
  article {
    border: 1px solid var(--color-border);
    border-radius: var(--radius-panel);
    padding: var(--space-3);
    margin-block: var(--space-2);
    background: var(--color-surface);
  }

  .row { display: flex; flex-wrap: wrap; align-items: center; gap: var(--space-2); }
  .subject-header, .subject-row, .other-row { display: grid; align-items: center; gap: var(--space-2); }
  .coalition-grid { grid-template-columns: 4rem minmax(0, 1fr) 3rem; }
  .list-grid { grid-template-columns: 4rem repeat(2, minmax(8rem, 1fr)) minmax(8rem, 12rem) 3rem; }
  .subject-header { padding-inline: var(--space-3); font-weight: 600; font-size: var(--text-small); }
  .field-caption { display: inline-flex; align-items: center; gap: var(--space-1); }
  .subject-name input, .subject-row select { inline-size: 100%; min-inline-size: 0; }
  .subject-reference, .other-reference {
    --reference-display: block;
    --reference-input-width: 100%;
    --field-display: flex;
    --field-margin: 0;
  }
  .subject-options { inline-size: 2.6rem; block-size: 2.6rem; font-size: 1.3rem; line-height: 1; }
  .mobile-label { display: none; }

  .other-row { grid-template-columns: 4rem minmax(12rem, 1fr) minmax(8rem, 12rem) auto; }
  .other-dot { text-align: center; font-size: 1.5rem; }
  .other-identity small { display: block; color: var(--color-text-muted); }
  .other-reference { grid-column: 3; }
  .other-row > button { grid-column: 4; }
  .other-setup, .other-confirm {
    border-block-start: 1px solid var(--color-border);
    margin-block-start: var(--space-3);
    padding-block-start: var(--space-3);
  }
  fieldset { min-inline-size: 0; border: 0; margin: 0; padding: 0; }
  legend { font-weight: 600; }
  .other-choices {
    max-block-size: 18rem;
    overflow: auto;
    border: 1px solid var(--color-border);
    border-radius: var(--radius-control);
    padding: var(--space-2) var(--space-3);
    background: var(--color-surface-muted);
  }
  .other-choices label { display: flex; }
  .other-actions {
    display: flex;
    align-items: center;
    justify-content: flex-end;
    flex-wrap: wrap;
    gap: var(--space-2);
    margin-block-start: var(--space-2);
  }
  .other-actions span { margin-inline-end: auto; }

  @media (width <= 650px) {
    .row > label { inline-size: 100%; }
    .subject-header { display: none; }
    .coalition-grid, .list-grid { grid-template-columns: 3rem minmax(0, 1fr) 3rem; }
    .mobile-label { display: inline; }
    .subject-coalition, .subject-reference { grid-column: 2; }
    .subject-options { grid-column: 3; grid-row: 1; }
    .other-row { grid-template-columns: 3rem minmax(0, 1fr) auto; }
    .other-reference { grid-column: 2 / 4; }
    .other-row > button { grid-column: 3; grid-row: 1; }
  }
</style>
