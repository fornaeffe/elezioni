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
    return { chamber, nodes, branches, filingLocations, disabled: !permitted(chamber) };
  }).filter(tree => tree.branches.length > 0));
  const editableLocations = $derived(chamberTrees.flatMap(tree => tree.disabled ? [] : tree.filingLocations));

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

{#snippet filingCheckbox(locations: readonly Location[], name: string, description: string, disabled: boolean)}
  {@const filingState = state(locations)}
  <label class="filing-checkbox" title={description}>
    <input type="checkbox" aria-label={name} checked={filingState.checked} indeterminate={filingState.mixed}
      {disabled} onchange={event => toggle(locations, event.currentTarget.checked)} />
  </label>
{/snippet}

{#snippet tree(branches: readonly FilingBranch[], disabled: boolean)}
  {#each branches as { node, children, filingLocations, showCandidates } (locationKey(node.location))}
    {#snippet branchCheckbox()}
      {@render filingCheckbox(filingLocations, `Deposita ${node.name}`, `Deposita in ${node.name}`, disabled)}
    {/snippet}
    <LazySection title={node.name} count={{ value: pins([node]), label: 'candidati fissati' }}
      headerControl={filingLocations.length ? branchCheckbox : undefined}>
      {#if showCandidates}
        <LazySection title="Candidati" count={{ value: pinsAt(node), label: 'fissati' }}>
          <CandidateSlots {scenario} {owner} {node} {assets} {edit} />
        </LazySection>
      {/if}
      {#if children.length}{@render tree(children, disabled)}{/if}
    </LazySection>
  {/each}
{/snippet}

{#snippet chamberSections()}
  {#each chamberTrees as { chamber, nodes, branches, filingLocations, disabled } (chamber)}
    {@const chamberName = chamber === 'camera' ? 'Camera' : 'Senato'}
    {#snippet chamberCheckbox()}
      {@render filingCheckbox(filingLocations, `Deposita tutta ${chamberName}`, 'Deposita in tutti i collegi', disabled)}
    {/snippet}
    <LazySection
      title={`${chamberName}${!renewed(scenario, chamber) ? ' · rinnovo inattivo' : ''}`}
      count={{ value: pins(nodes), label: 'candidati fissati' }}
      headerControl={filingLocations.length ? chamberCheckbox : undefined}
    >
      {#if disabled}<p>Camera esclusa dalla coalizione: depositi e candidati conservati, inattivi.</p>{/if}
      {@render tree(branches, disabled)}
    </LazySection>
  {/each}
{/snippet}

{#if list && list.kind !== 'other'}
  <LazySection title="Depositi e candidati" count={{ value: placements.length, label: 'candidati fissati' }}>
    {#snippet headerControl()}
      {@const description = 'Deposita in tutti i collegi modificabili'}
      {@render filingCheckbox(editableLocations, description, description, !editableLocations.length)}
    {/snippet}
    {@render chamberSections()}
  </LazySection>
{:else}
  {@render chamberSections()}
{/if}

<style>
  .filing-checkbox {
    --filing-border: var(--color-error-border);
    display: grid;
    place-items: center;
    box-sizing: border-box;
    inline-size: 100%;
    block-size: 100%;
    border: 1px solid var(--filing-border);
    border-radius: var(--radius-control);
    background: var(--color-error-surface);
    cursor: pointer;
  }

  .filing-checkbox:has(:checked) {
    --filing-border: var(--color-success-border);
    background: var(--color-success-surface);
  }

  .filing-checkbox:has(:indeterminate) {
    --filing-border: var(--color-warning-border);
    background: var(--color-warning-surface);
  }

  .filing-checkbox:has(:disabled) { cursor: default; }

  .filing-checkbox input {
    margin: 0;
    accent-color: var(--filing-border);
    cursor: inherit;
  }
</style>
