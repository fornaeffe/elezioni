<script lang="ts">
  import type { Location, PoliticsScenario, Subject } from '../scenario/schema';
  import { chambers } from '../scenario/schema';
  import { locationKey } from '../scenario/validation';
  import { isListFilingLocation, renewed, sameSubject } from '../scenario/effective';
  import { setFilings } from '../scenario/edits';
  import { filingNodes, type EditorAssets, type FilingNode } from './assets';
  import type { ApplyEdit } from './model';
  import LazySection from './LazySection.svelte';
  import CandidateSlots from './CandidateSlots.svelte';

  interface FilingBranch {
    node: FilingNode;
    children: FilingBranch[];
    filingLocations: Location[];
    showCandidates: boolean;
  }

  let { scenario, owner, assets, edit }: { scenario: PoliticsScenario; owner: Subject; assets: EditorAssets; edit: ApplyEdit } = $props();
  let law = $derived(scenario.selection.scrutiny.id);
  let list = $derived(owner.kind === 'list' ? scenario.subjects.lists.find(l => l.id === owner.id) : null);
  let filed = $derived(scenario.lawSettings[law].filings.find(f => f.listId === owner.id)?.locations ?? []);
  let keys = $derived(new Set(filed.map(locationKey)));
  const placements = $derived(scenario.lawSettings[law].placements.filter(p => sameSubject(p.owner, owner)));
  const chamberTrees = $derived(chambers.map(chamber => {
    const nodes = assets.filings[law].filter(node => node.location.chamber === chamber);
    const branches = visibleBranches(nodes);
    const filingLocations = branches.flatMap(branch => branch.filingLocations);
    return { chamber, nodes, branches, filingLocations };
  }).filter(tree => tree.branches.length > 0));

  function visibleBranches(nodes: readonly FilingNode[]): FilingBranch[] {
    return nodes.flatMap(node => {
      const children = visibleBranches(node.children);
      const filingLocations = children.flatMap(child => child.filingLocations);
      if (list && isListFilingLocation(node.location, assets.context.geographyRelations)) {
        filingLocations.unshift(node.location);
      }
      const showCandidates = candidateNode(node);
      return filingLocations.length || showCandidates || children.length
        ? [{ node, children, filingLocations, showCandidates }]
        : [];
    });
  }

  function state(locations: readonly Location[]) {
    const selected = locations.filter(location => keys.has(locationKey(location))).length;
    return { checked: locations.length > 0 && selected === locations.length, mixed: selected > 0 && selected < locations.length };
  }
  function toggle(locations: readonly Location[], checked: boolean) {
    const remove = new Set(locations.map(locationKey));
    edit(s => setFilings(s, law, owner.id, [...filed.filter(l => !remove.has(locationKey(l))), ...(checked ? locations : [])], assets.context));
  }
  function permitted(chamber: 'camera' | 'senato') { return !list?.coalitionId || !!scenario.subjects.coalitions.find(c => c.id === list.coalitionId)?.chambers.includes(chamber); }
  function pins(nodes: readonly FilingNode[]) {
    if (!placements.length) return 0;
    const codes = new Set(nodes.flatMap(filingNodes).map(n => locationKey(n.location)));
    return placements.filter(p => codes.has(locationKey(p.location))).length;
  }
  function pinsAt(node: FilingNode) {
    const key = locationKey(node.location);
    return placements.filter(p => p.role === node.role && locationKey(p.location) === key).length;
  }
  function candidateNode(node: FilingNode) {
    if (!node.role || list?.kind === 'other') return false;
    return list
      ? node.role !== 'bonus' && !(list.coalitionId && node.role === 'uninominal')
      : node.role === 'bonus' || node.role === 'uninominal';
  }
</script>
{#snippet tree(branches: readonly FilingBranch[], disabled: boolean)}
  {#each branches as { node, children, filingLocations, showCandidates } (locationKey(node.location))}
    <LazySection title={node.name} count={{ value: pins([node]), label: 'candidati fissati' }}>
      {#if filingLocations.length}
        {@const filingState = state(filingLocations)}
        <label data-field><input type="checkbox" checked={filingState.checked} indeterminate={filingState.mixed}
          aria-label={`Deposita ${node.name}`} {disabled} onchange={e => toggle(filingLocations, e.currentTarget.checked)} />Deposita in {node.name}</label>
      {/if}
      {#if showCandidates}
        <LazySection title="Candidati" count={{ value: pinsAt(node), label: 'fissati' }}>
          <CandidateSlots {scenario} {owner} {node} {assets} {edit} />
        </LazySection>
      {/if}
      {#if children.length}{@render tree(children, disabled)}{/if}
    </LazySection>
  {/each}
{/snippet}
{#each chamberTrees as { chamber, nodes, branches, filingLocations } (chamber)}
  {@const disabled = !permitted(chamber)}
  <LazySection
    title={`${chamber === 'camera' ? 'Camera' : 'Senato'}${!renewed(scenario, chamber) ? ' · rinnovo inattivo' : ''}`}
    count={{ value: pins(nodes), label: 'candidati fissati' }}
  >
    {#if disabled}<p>Camera esclusa dalla coalizione: depositi e candidati conservati, inattivi.</p>{/if}
    {#if filingLocations.length}
      {@const filingState = state(filingLocations)}
      <label data-field><input type="checkbox" aria-label={`Deposita tutta ${chamber === 'camera' ? 'Camera' : 'Senato'}`} checked={filingState.checked}
        indeterminate={filingState.mixed} {disabled} onchange={e => toggle(filingLocations, e.currentTarget.checked)} />Deposita in tutti i collegi</label>
    {/if}
    {@render tree(branches, disabled)}
  </LazySection>
{/each}
