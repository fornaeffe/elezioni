<script lang="ts">
  import { resolve } from '$app/paths';
  import type { Snippet } from 'svelte';
  import { electionWorkspaces, workspace, type WorkspaceId } from './registry';
  let { id, busy = false, children }: { id: WorkspaceId; busy?: boolean; children: Snippet } = $props();
  const definition = $derived(workspace(id));
</script>

<div class="workspace-shell" data-workspace={id} aria-busy={busy}>
  <nav aria-label="Spazi di simulazione">
    <a href={resolve('/')}>Simulatore elezioni</a>
    {#each electionWorkspaces as item (item.id)}
      <a href={resolve(item.route)} aria-current={item.kind === definition.kind ? 'page' : undefined}>{item.name}</a>
    {/each}
  </nav>
  {@render children()}
</div>

<style>
  nav { padding-block: var(--space-3); display: flex; flex-wrap: wrap; gap: 1.25rem; border-bottom: 1px solid var(--color-border); }
  a { color: var(--color-accent); text-underline-offset: .2rem; }
  [aria-current] { font-weight: 700; }
</style>
