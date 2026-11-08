<script lang="ts">
  import { authoredPoliticsDefault } from '$lib/politics/scenario/defaults';
  import { takeWorkspaceImport } from '$lib/workspaces/handoff';
  import { navigateWorkspaceImport } from '$lib/workspaces/navigation';
  import LegacyPoliticsWorkspace from '$lib/workspaces/LegacyPoliticsWorkspace.svelte';
  import ReformWorkspace from '$lib/politics/reform-2026/ReformWorkspace.svelte';
  import { politicsWorkspaces } from '$lib/workspaces/registry';
  import { transitionalFileWorkspace as fileWorkspace, transitionalScenarioWorkspace as scenarioWorkspace } from '$lib/workspaces/import-routing';
  import type { SimulationResultExport } from '$lib/core/result-export';
  import type { ReleasedScenario } from '$lib/politics/reform-2026/vote-share/release';
  const legacyInput = takeWorkspaceImport('politics-legacy'), reformInput = takeWorkspaceImport('politics-reform');
  let algorithmId = $state<string>(legacyInput ? politicsWorkspaces[0].algorithm : authoredPoliticsDefault.selection.scrutiny.id);
  let busy = $state(false), error = $state('');
  let input = $state.raw<{ file?: File; payload?: SimulationResultExport; scenario?: ReleasedScenario }>(legacyInput ?? reformInput ?? {});
  async function routeImport(next: typeof input) {
    try {
      const definition = next.file ? await fileWorkspace(next.file) : scenarioWorkspace(next.scenario ?? next.payload?.scenario);
      if (definition.kind !== 'politiche') { await navigateWorkspaceImport(next); return; }
      input = next; algorithmId = definition.algorithm; busy = false;
    } catch (e) { error = e instanceof Error ? e.message : String(e); }
  }
</script>
<svelte:head><title>Elezioni politiche</title></svelte:head>
<nav class="algorithm-selection" aria-label="Legge elettorale">
  <label>Algoritmo di scrutinio
    <select bind:value={algorithmId} disabled={busy}>
      {#each politicsWorkspaces as item (item.id)}<option value={item.algorithm}>{item.label}</option>{/each}
    </select>
  </label>
  <p>I file politiche precedenti non sono più supportati. Il nuovo editor userà una bozza comune alle due leggi.</p>
</nav>
{#if error}<p role="alert">{error}</p>{/if}
{#key algorithmId}
  {#if algorithmId === politicsWorkspaces[0].algorithm}
    <LegacyPoliticsWorkspace imported={input.payload} importedFile={input.file} onRouteImport={routeImport}
      onImportConsumed={() => input = {}} onBusyChange={value => busy = value} />
  {:else}
    <ReformWorkspace importedFile={input.file ?? null} imported={input.payload ?? null} importedScenario={input.scenario ?? null}
      onLegacyImport={payload => { void routeImport({ payload }); }} onExternalFile={file => { void routeImport({ file }); }}
      onImportConsumed={() => input = {}} onBusyChange={value => busy = value} />
  {/if}
{/key}
<style>
  .algorithm-selection { max-width: 1500px; margin: auto; padding: 1rem 1.5rem 0; font-family: system-ui, sans-serif; }
  label { display: flex; flex-wrap: wrap; align-items: center; gap: .75rem; }
  select { font: inherit; padding: .5rem; border: 1px solid #a8bac6; border-radius: 4px; }
  p { color: #465c69; }
</style>
