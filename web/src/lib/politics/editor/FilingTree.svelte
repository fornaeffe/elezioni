<script lang="ts">
  import type { PoliticsScenario, Subject } from '../scenario/schema';
  import { chambers } from '../scenario/schema';
  import { locationKey } from '../scenario/validation';
  import { isListFilingLocation, renewed } from '../scenario/effective';
  import { setFilings } from '../scenario/edits';
  import { filingNodes, type EditorAssets, type FilingNode } from './assets';
  import type { ApplyEdit } from './model';
  import LazySection from './LazySection.svelte';
  import CandidateSlots from './CandidateSlots.svelte';
  let { scenario, owner, assets, edit }: { scenario: PoliticsScenario; owner: Subject; assets: EditorAssets; edit: ApplyEdit } = $props();
  let law = $derived(scenario.selection.scrutiny.id);
  let list = $derived(owner.kind === 'list' ? scenario.subjects.lists.find(l => l.id === owner.id) : null);
  let filed = $derived(scenario.lawSettings[law].filings.find(f => f.listId === owner.id)?.locations ?? []);
  let keys = $derived(new Set(filed.map(locationKey)));
  function leaves(nodes: readonly FilingNode[]) { return nodes.flatMap(filingNodes).map(n => n.location).filter(l => isListFilingLocation(l, assets.context.geographyRelations)); }
  function state(nodes: readonly FilingNode[]) { const locations = leaves(nodes); const selected = locations.filter(l => keys.has(locationKey(l))).length; return { checked: locations.length > 0 && selected === locations.length, mixed: selected > 0 && selected < locations.length }; }
  function toggle(nodes: readonly FilingNode[], checked: boolean) {
    const locations = leaves(nodes), remove = new Set(locations.map(locationKey));
    edit(s => setFilings(s, law, owner.id, [...filed.filter(l => !remove.has(locationKey(l))), ...(checked ? locations : [])], assets.context));
  }
  function permitted(chamber: 'camera' | 'senato') { return !list?.coalitionId || !!scenario.subjects.coalitions.find(c => c.id === list.coalitionId)?.chambers.includes(chamber); }
  function pins(nodes: readonly FilingNode[]) { const codes = new Set(nodes.flatMap(filingNodes).map(n => locationKey(n.location))); return scenario.lawSettings[law].placements.filter(p => p.owner.id === owner.id && p.owner.kind === owner.kind && codes.has(locationKey(p.location))).length; }
  function candidateNode(node: FilingNode) { return list ? node.role !== 'bonus' && !(list.coalitionId && node.role === 'uninominal') : node.role === 'bonus' || node.role === 'uninominal'; }
</script>
{#snippet tree(nodes: readonly FilingNode[], disabled: boolean)}
  {#each nodes as node (locationKey(node.location))}
    <LazySection title={`${node.name}${pins([node]) ? ` · ${pins([node])} candidati fissati` : ''}`}>
      {#if list && leaves([node]).length}
        <label><input type="checkbox" checked={state([node]).checked} indeterminate={state([node]).mixed}
          aria-label={`Deposita ${node.name}`} {disabled} onchange={e => toggle([node], e.currentTarget.checked)} />Deposita in {node.name}</label>
      {/if}
      {#if node.role && candidateNode(node) && list?.kind !== 'other'}
        <LazySection title={`Candidati · ${pins([node])} fissati`}><CandidateSlots {scenario} {owner} {node} {assets} {edit} /></LazySection>
      {/if}
      {#if node.children.length}{@render tree(node.children, disabled)}{/if}
    </LazySection>
  {/each}
{/snippet}
{#each chambers as chamber (chamber)}
  {@const nodes = assets.filings[law].filter(n => n.location.chamber === chamber)}
  <LazySection title={`${chamber === 'camera' ? 'Camera' : 'Senato'} · ${pins(nodes)} candidati fissati${!renewed(scenario, chamber) ? ' · rinnovo inattivo' : ''}`}>
    {#if !permitted(chamber)}<p>Camera esclusa dalla coalizione: depositi e candidati conservati, inattivi.</p>{/if}
    {#if list}<label><input type="checkbox" aria-label={`Deposita tutta ${chamber === 'camera' ? 'Camera' : 'Senato'}`} checked={state(nodes).checked} indeterminate={state(nodes).mixed}
      disabled={!permitted(chamber)} onchange={e => toggle(nodes, e.currentTarget.checked)} />Deposita in tutti i collegi</label>{/if}
    {@render tree(nodes, !permitted(chamber))}
  </LazySection>
{/each}
