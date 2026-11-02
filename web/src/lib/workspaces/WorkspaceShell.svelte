<script lang="ts">
  import { base } from '$app/paths';
  import type { Snippet } from 'svelte';
  import { electionWorkspaces, workspace, type WorkspaceId } from './registry';
  let { id, busy = false, children }: { id: WorkspaceId; busy?: boolean; children: Snippet } = $props();
  const definition = $derived(workspace(id));
</script>

<div class="workspace-shell" data-workspace={id} aria-busy={busy}>
  <nav aria-label="Spazi di simulazione">
    <a href={`${base}/`}>Simulatore elezioni</a>
    {#each electionWorkspaces as item (item.id)}
      <a href={`${base}${item.route}`} aria-current={item.kind === definition.kind ? 'page' : undefined}>{item.name}</a>
    {/each}
  </nav>
  {@render children()}
</div>

<style>
  .workspace-shell { --workspace-border: #d7e0e5; color: #15232d; font-family: system-ui, sans-serif; }
  nav { max-width: 1500px; margin: auto; padding: .8rem 1.5rem; display: flex; flex-wrap: wrap; gap: 1.25rem; border-bottom: 1px solid var(--workspace-border); }
  a { color: #24536e; text-underline-offset: .2rem; }
  [aria-current] { font-weight: 700; }
</style>
