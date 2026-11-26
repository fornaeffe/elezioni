<script lang="ts">
  import type { PoliticsScenario } from '../scenario/schema';
  import type { ReferenceNode } from './assets';
  import { referenceScopes } from './assets';
  import { referenceRows, removeReferences, scopeKey, type ApplyEdit, type ReferenceTarget } from './model';
  import LazySection from './LazySection.svelte';
  import ReferenceFields from './ReferenceFields.svelte';
  import ReferenceTree from './ReferenceTree.svelte';
  let { scenario, target, nodes, edit }: { scenario: PoliticsScenario; target: ReferenceTarget; nodes: readonly ReferenceNode[]; edit: ApplyEdit } = $props();
  let query = $state(''), limit = $state(40);
  let filtered = $derived(nodes.filter(n => n.name.toLocaleLowerCase('it').includes(query.toLocaleLowerCase('it'))));
  function count(node: ReferenceNode) { const keys = new Set(referenceScopes(node).map(scopeKey)); return referenceRows(scenario, target).filter(r => keys.has(scopeKey(r.scope))).length; }
</script>
{#if nodes.length > 30}<label data-field>Cerca territorio<input type="search" bind:value={query} oninput={() => limit = 40} /></label>{/if}
{#each filtered.slice(0, limit) as node (scopeKey(node.scope))}
  <LazySection title={node.name} count={{ value: count(node), label: 'dichiarazioni' }}>
    <ReferenceFields {scenario} {target} scope={node.scope} label={node.name} {edit} layout="grid" />
    <button type="button" class="remove-subtree" onclick={() => edit(s => removeReferences(s, target, referenceScopes(node)))}>
      {node.children.length ? 'Rimuovi riferimenti nel territorio e discendenti' : 'Rimuovi riferimenti nel territorio'}
    </button>
    {#if node.children.length}<ReferenceTree {scenario} {target} nodes={node.children} {edit} />{/if}
  </LazySection>
{/each}
{#if filtered.length > limit}<button onclick={() => limit += 40}>Mostra altri territori</button>{/if}

<style>
  .remove-subtree {
    padding: var(--space-1);
    border: 0;
    background: transparent;
    color: var(--color-text-muted);
    font-size: var(--text-small);
    text-align: start;
  }
  .remove-subtree:hover { background: var(--color-action-hover); }
</style>
