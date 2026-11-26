<script lang="ts">
  import Toggletip from '$lib/ui/Toggletip.svelte';
  import type { PoliticsScenario } from '../scenario/schema';
  import { capabilities } from '../scenario/registry';
  import type { EditorAssets } from './assets';
  import { editScenario, type ApplyEdit } from './model';
  import LazySection from './LazySection.svelte';
  import MembershipsEditor from './MembershipsEditor.svelte';
  import ReferencesEditor from './ReferencesEditor.svelte';

  const fallbackFields = [
    { key: 'geographicLogProfile', label: 'Profilo geografico (log-rapporto)',
      help: 'Correzione al peso del componente in ciascun comune rispetto al suo peso nazionale, espressa come log-rapporto. Per un componente senza storico si usa lo stesso valore in tutti i comuni. Zero lascia invariato il peso prima della normalizzazione locale.' },
    { key: 'profileSd', label: 'SD profilo (log-rapporto)',
      help: 'Deviazione standard che esprime l’incertezza sul profilo geografico del componente alla data di riferimento. Valori più alti permettono maggiore variabilità tra i profili di partenza simulati.' },
    { key: 'localInnovationSd', label: 'SD innovazione locale (log-rapporto)',
      help: 'Deviazione standard delle variazioni comunali del sostegno al componente dalla data di riferimento all’elezione. Valori più alti consentono ai comuni di discostarsi maggiormente dall’andamento nazionale.' },
    { key: 'nationalInnovationSd', label: 'SD innovazione nazionale (log-rapporto)',
      help: 'Deviazione standard delle variazioni nazionali del sostegno al componente dalla data di riferimento all’elezione. Valori più alti consentono uno spostamento comune a tutti i comuni più ampio.' }
  ] as const;
  let { scenario, assets, edit }: { scenario: PoliticsScenario; assets: EditorAssets; edit: ApplyEdit } = $props();
  const fallbackId = $props.id();
  let poolName = $state(''), poolMembers = $state<string[]>([]), poolProvenance = $state(''), query = $state('');
  let componentId = $state('');
  let components = $derived(capabilities(scenario).generator.id === 'M02' ? scenario.newComponents : [...new Map([
    ...assets.components, ...scenario.subjects.components, ...scenario.history.pooling.map(p => ({ id: p.id, name: p.id })), ...scenario.newComponents
  ].map(c => [c.id, c])).values()]);
  let component = $derived(scenario.newComponents.find(c => c.id === componentId));
  function fallback(key: 'geographicLogProfile' | 'profileSd' | 'localInnovationSd' | 'nationalInnovationSd' | 'provenance', value: string) {
    edit(s => editScenario(s, n => { const row = n.newComponents.find(c => c.id === componentId)!; if (key === 'provenance') row.fallback.M02[key] = value; else row.fallback.M02[key] = Number(value); }));
  }
  function pool() {
    edit(s => editScenario(s, n => {
      const id = `pool-${crypto.randomUUID()}`;
      n.history.pooling.push({ id, members: [...poolMembers], provenance: poolProvenance });
      n.subjects.components.push({ id, name: poolName, support: 'fitted' });
    }, assets.context));
    poolName = ''; poolMembers = []; poolProvenance = '';
  }
  function removePool(id: string) {
    edit(s => {
      if (s.memberships.some(m => m.camera.includes(id) || m.senato.includes(id)) || s.references.some(r => r.subject.kind === 'component' && r.subject.id === id))
        throw new Error('Rimuovi prima le appartenenze future e i riferimenti del raggruppamento. Non è possibile distribuire automaticamente il suo supporto.');
      return editScenario(s, n => { n.history.pooling = n.history.pooling.filter(p => p.id !== id); n.subjects.components = n.subjects.components.filter(c => c.id !== id); }, assets.context);
    });
  }
</script>
<LazySection title="Appartenenze dei componenti"><MembershipsEditor {scenario} {assets} {edit} /></LazySection>
{#if capabilities(scenario).generator.pooling}
  <LazySection title={`Raggruppamento dei componenti · ${scenario.history.pooling.length} dichiarazioni`}>
    <p>Seleziona componenti originali interi. Il raggruppamento definisce la base identificabile; non divide né inventa conteggi. Le frazioni M01 e le appartenenze future restano distinte.</p>
    {#each scenario.history.pooling as group (group.id)}
      <LazySection title={`${scenario.subjects.components.find(c => c.id === group.id)?.name ?? group.id} · ${group.members.length} componenti`}>
        <p>Base risultante: {group.members.map(id => assets.components.find(c => c.id === id)?.name ?? id).join(', ')}</p>
        <label>Provenienza raggruppamento<input value={group.provenance} onchange={e => edit(s => editScenario(s, n => { n.history.pooling.find(p => p.id === group.id)!.provenance = e.currentTarget.value; }))} /></label>
        <button onclick={() => removePool(group.id)}>Rimuovi raggruppamento</button>
      </LazySection>
    {/each}
    <label>Nome raggruppamento<input bind:value={poolName} /></label>
    <label>Cerca componente originale<input type="search" bind:value={query} /></label>
    <div class="bounded-list">
      {#each assets.components.filter(c => c.name.toLocaleLowerCase('it').includes(query.toLocaleLowerCase('it'))) as c (c.id)}
        {@const used = scenario.history.pooling.some(p => p.members.includes(c.id))}
        <label><input type="checkbox" disabled={used} checked={poolMembers.includes(c.id)} onchange={e => poolMembers = e.currentTarget.checked ? [...poolMembers, c.id] : poolMembers.filter(id => id !== c.id)} />{c.name}{used ? ' · già raggruppato' : ''}</label>
      {/each}
    </div>
    <p>Nuova base: {poolMembers.join(', ') || 'Nessun componente selezionato'}</p>
    <label>Provenienza<input bind:value={poolProvenance} /></label>
    <button disabled={!poolName.trim() || !poolProvenance.trim() || poolMembers.length < 2} onclick={pool}>Crea raggruppamento</button>
  </LazySection>
{/if}
<LazySection title="Ipotesi senza storico e riferimenti dei componenti">
  <label>Componente<select bind:value={componentId}><option value="">Scegli componente</option>{#each components as c (c.id)}<option value={c.id}>{c.name}</option>{/each}</select></label>
  {#if component}
    <p>Componente proprio: attivo solo nelle camere dove la lista non ha membri storici. Le ipotesi restano salvate quando inattivo.</p>
    {#if capabilities(scenario).generator.id === 'M02'}
      {#each fallbackFields as { key, label, help } (key)}
        {@const id = `${fallbackId}-${key}`}
        <span class="field-with-tip">
          <span class="field-caption"><label for={id}>{label}</label><Toggletip {label} text={help} /></span>
          <input {id} type="number" min={key === 'geographicLogProfile' ? undefined : 0} step="any" value={component.fallback.M02[key]} onchange={e => fallback(key, e.currentTarget.value)} />
        </span>
      {/each}
      <label>Provenienza ipotesi M02<input value={component.fallback.M02.provenance} onchange={e => fallback('provenance', e.currentTarget.value)} /></label>
    {:else}<p>M01 usa il profilo regionale o, in assenza, l’allocazione uguale tra liste. I parametri M02 salvati non hanno effetto su M01. Provenienza: {component.fallback.M01.provenance}</p>{/if}
    <label>Assunzioni di supporto<input value={component.support} onchange={e => edit(s => editScenario(s, n => { n.newComponents.find(c => c.id === componentId)!.support = e.currentTarget.value; }))} /></label>
  {/if}
  {#if componentId}<ReferencesEditor {scenario} target={{ kind: 'component', id: componentId }} label="Riferimento componente" {assets} {edit} />{/if}
</LazySection>
<LazySection title="Elettorato · dati in sola lettura">
  <p>Fonte vincolata: {scenario.data.electorate.version}. Data: {assets.electorate.targets[0]?.date} · età minima {assets.electorate.minimumAge} anni.</p>
  <p>Camera: {assets.electorate.targets.reduce((n, t) => n + t.camera, 0).toLocaleString('it')}; Senato: {assets.electorate.targets.reduce((n, t) => n + t.senato, 0).toLocaleString('it')}.</p>
  {#each [...new Set(assets.electorate.targets.map(t => t.provenance))] as provenance (provenance)}<p>{provenance}</p>{/each}
  {#each scenario.provenance.assumptions as assumption (assumption)}<p>{assumption}</p>{/each}
</LazySection>
