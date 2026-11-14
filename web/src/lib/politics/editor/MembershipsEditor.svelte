<script lang="ts">
  import type { PoliticsScenario, Chamber } from '../scenario/schema';
  import { chambers } from '../scenario/schema';
  import { capabilities } from '../scenario/registry';
  import { setMembers, setMembershipLock } from '../scenario/edits';
  import type { EditorAssets } from './assets';
  import { editScenario, type ApplyEdit } from './model';
  import LazySection from './LazySection.svelte';
  let { scenario, assets, edit }: { scenario: PoliticsScenario; assets: EditorAssets; edit: ApplyEdit } = $props();
  let view = $state('future'), search = $state(''), source = $state(''), choice = $state('');
  let additions = $state<Record<string, string>>({});
  let options = $derived([...assets.components.filter(c => !scenario.history.pooling.some(p => p.members.includes(c.id))), ...scenario.history.pooling.map(p => ({ id: p.id, name: scenario.subjects.components.find(c => c.id === p.id)?.name ?? p.id }))]);
  let sources = $derived([...new Set(assets.sources.map(s => s.sourceId))]);
  let selected = $derived(assets.sources.find(s => s.sourceId === source && s.choiceId === choice));
  let declaration = $derived(scenario.history.sourceMemberships.find(s => s.sourceId === source && s.choiceId === choice));
  let composition = $derived(scenario.modelSettings.M01.compositions.find(s => s.sourceId === source && s.choiceId === choice));
  function conflict(listId: string, chamber: Chamber, id: string) {
    const m = scenario.memberships.find(m => m.listId === listId)!;
    return scenario.memberships.some(other => other.listId !== listId && (other[chamber].includes(id) || m.locked && other.senato.includes(id)));
  }
  function historicalConflict(id: string) { return scenario.history.sourceMemberships.some(r => r.sourceId === source && r.questionId === selected?.questionId && r.choiceId !== choice && r.members.includes(id)); }
  function sourceMembers(members: readonly string[]) {
    if (!selected) return;
    edit(s => editScenario(s, n => { n.history.sourceMemberships = [...n.history.sourceMemberships.filter(r => !(r.sourceId === source && r.choiceId === choice)),
      { sourceId: source, questionId: selected!.questionId, choiceId: choice, members: [...members], provenance: 'Dichiarazione utente; conteggi originali invariati' }]; }, assets.context));
  }
  function fraction(id: string, value: string) {
    if (!selected) return;
    edit(s => editScenario(s, n => {
      let row = n.modelSettings.M01.compositions.find(r => r.sourceId === source && r.choiceId === choice);
      if (!row) { row = { sourceId: source, questionId: selected!.questionId, choiceId: choice, fractions: {}, provenance: 'Composizione dichiarata, non conteggi osservati', proxy: false }; n.modelSettings.M01.compositions.push(row); }
      if (value.trim() === '') delete row.fractions[id]; else row.fractions[id] = Number(value) / 100;
    }, assets.context));
  }
</script>
<label>Vista appartenenze<select bind:value={view}><option value="future">Liste future</option><option value="source">Fonte e opzione storica</option></select></label>
{#if view === 'future'}
  <label>Cerca lista<input type="search" bind:value={search} /></label>
  {#each scenario.memberships.filter(m => scenario.subjects.lists.find(l => l.id === m.listId)?.name.toLocaleLowerCase('it').includes(search.toLocaleLowerCase('it'))) as membership (membership.listId)}
    <LazySection title={`${scenario.subjects.lists.find(l => l.id === membership.listId)?.name} · Camera ${membership.camera.length}, Senato ${membership.senato.length}`}>
      <label><input type="checkbox" checked={membership.locked} onchange={e => edit(s => setMembershipLock(s, membership.listId, e.currentTarget.checked, assets.context))} />Collega Camera e Senato</label>
      <p>Ricollegare sostituisce interamente Senato con Camera, anche se vuota. I conflitti bloccano l’intera modifica.</p>
      <div class="chamber-grid">
        {#each chambers as chamber (chamber)}
          {@const locked = membership.locked && chamber === 'senato'}
          <fieldset disabled={locked}><legend>{chamber === 'camera' ? 'Camera' : 'Senato'}{locked ? ' · eredita Camera' : ''}</legend>
            {#each membership[chamber] as id (id)}
              <div>{options.find(c => c.id === id)?.name ?? id}<button onclick={() => edit(s => setMembers(s, membership.listId, chamber, membership[chamber].filter(c => c !== id), assets.context))}>Rimuovi componente {options.find(c => c.id === id)?.name ?? id}</button></div>
            {/each}
            {#if !membership[chamber].length}<p>Nessun membro storico: attivo il componente proprio con ipotesi salvate.</p>{/if}
            <label>Aggiungi componente {chamber}<select value={additions[`${membership.listId}:${chamber}`] ?? ''} onchange={e => additions[`${membership.listId}:${chamber}`] = e.currentTarget.value}>
              <option value="">Scegli componente</option>
              {#each options.filter(c => !membership[chamber].includes(c.id)) as component (component.id)}
                {@const used = conflict(membership.listId, chamber, component.id)}
                <option value={component.id} disabled={used}>{component.name}{used ? ' — appartiene a un’altra lista nella camera' : ''}</option>
              {/each}
            </select></label>
            <button disabled={!additions[`${membership.listId}:${chamber}`]} onclick={() => { const id = additions[`${membership.listId}:${chamber}`]; edit(s => setMembers(s, membership.listId, chamber, [...membership[chamber], id], assets.context)); additions[`${membership.listId}:${chamber}`] = ''; }}>Conferma aggiunta {chamber}</button>
          </fieldset>
        {/each}
      </div>
    </LazySection>
  {/each}
{:else}
  <label>Fonte<select bind:value={source} onchange={() => choice = ''}><option value="">Scegli fonte</option>{#each sources as id (id)}<option value={id}>{id}</option>{/each}</select></label>
  <label>Opzione<select bind:value={choice}><option value="">Scegli opzione</option>{#each assets.sources.filter(s => s.sourceId === source) as row (row.choiceId)}<option value={row.choiceId}>{row.name}</option>{/each}</select></label>
  {#if selected}
    {#if selected.other}<p>“Altri” è il complemento delle opzioni offerte; i membri non sono modificabili.</p>
    {:else}
      {@const members = declaration?.members ?? selected.members}
      {#each members as id (id)}<p>{assets.components.find(c => c.id === id)?.name ?? id}<button onclick={() => sourceMembers(members.filter(c => c !== id))}>Rimuovi membro {id}</button></p>{/each}
      <label>Aggiungi membro storico<select value="" onchange={e => { if (e.currentTarget.value) sourceMembers([...members, e.currentTarget.value]); }}>
        <option value="">Scegli componente</option>
        {#each assets.components.filter(c => !members.includes(c.id)) as c (c.id)}<option value={c.id} disabled={historicalConflict(c.id)}>{c.name}{historicalConflict(c.id) ? ' — usato in altra opzione della fonte' : ''}</option>{/each}
      </select></label>
      <p>Provenienza: {declaration?.provenance ?? 'Fonte originale vincolata'}</p>
      <button onclick={() => edit(s => editScenario(s, n => { n.history.sourceMemberships = n.history.sourceMemberships.filter(r => !(r.sourceId === source && r.choiceId === choice)); }, assets.context))}>Ripristina membri della fonte</button>
      {#if capabilities(scenario).generator.sourceCompositions}
        <p>Composizione M01: frazioni percentuali, somma richiesta 100%. Non sono conteggi osservati. Totale: {Object.values(composition?.fractions ?? {}).reduce((a, b) => a + b, 0) * 100}%.</p>
        {#each [...new Set([...members, ...Object.keys(composition?.fractions ?? {})])] as id (id)}
          <label>Frazione {id} (%)<input type="number" min="0" max="100" step="any" value={composition?.fractions[id] === undefined ? '' : composition.fractions[id] * 100} onchange={e => fraction(id, e.currentTarget.value)} /></label>
        {/each}
        {#if composition}
          <label>Provenienza composizione<input value={composition.provenance} onchange={e => edit(s => editScenario(s, n => { n.modelSettings.M01.compositions.find(r => r.sourceId === source && r.choiceId === choice)!.provenance = e.currentTarget.value; }))} /></label>
          <button onclick={() => edit(s => editScenario(s, n => { n.modelSettings.M01.compositions = n.modelSettings.M01.compositions.filter(r => !(r.sourceId === source && r.choiceId === choice)); }, assets.context))}>Rimuovi composizione esplicita</button>
          <p>Senza composizione esplicita, M01 usa l’allocazione uguale tra i membri dichiarati; non sono conteggi osservati.</p>
        {/if}
      {/if}
    {/if}
  {/if}
{/if}
