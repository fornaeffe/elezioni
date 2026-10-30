<script lang="ts">
  import { onMount } from 'svelte';
  import { fastReformCandidate } from './vote-share/build';
  import {
    archiveWorkflow,
    scenarioWorkflow,
    type ReformWorkflow,
  } from './archive-routing';
  import { isReformFile } from '../../core/json-stream';
  import InstalledReformWorkspace from './InstalledReformWorkspace.svelte';
  import FastReformWorkspace from './vote-share/FastReformWorkspace.svelte';
  import type { SimulationResultExport } from '../../core/result-export';
  import type { ReformGenerationScenario } from './generation-types';
  let {
    imported = null,
    importedScenario = null,
    importedFile = null,
    onLegacyImport,
    onImportConsumed,
  }: {
    imported?: SimulationResultExport | null;
    importedScenario?: ReformGenerationScenario | null;
    importedFile?: File | null;
    onLegacyImport: (p: SimulationResultExport) => void;
    onImportConsumed: () => void;
  } = $props();
  let workflow = $state<ReformWorkflow>(
      fastReformCandidate ? 'fast' : 'installed',
    ),
    ready = $state(false),
    error = $state('');
  let input = $state.raw<{
    file: File | null;
    payload: SimulationResultExport | null;
    scenario: ReformGenerationScenario | null;
  }>({ file: null, payload: null, scenario: null });
  onMount(() => {
    void initialize();
  });
  async function initialize() {
    try {
      input = {
        file: importedFile,
        payload: imported,
        scenario: importedScenario,
      };
      if (importedFile) workflow = await archiveWorkflow(importedFile);
      else if (importedScenario || imported)
        workflow = scenarioWorkflow(importedScenario ?? imported?.scenario);
      ready = true;
    } catch (e) {
      error = e instanceof Error ? e.message : String(e);
    }
  }
  async function routeFile(file: File) {
    if (!(await isReformFile(file))) return false;
    const target = await archiveWorkflow(file);
    if (target === workflow) return false;
    input = { file, payload: null, scenario: null };
    workflow = target;
    return true;
  }
  function consumed() {
    input = { file: null, payload: null, scenario: null };
    onImportConsumed();
  }
</script>

{#if error}<p role="alert">{error}</p>
  <button
    onclick={() => {
      input = { file: null, payload: null, scenario: null };
      error = '';
      ready = true;
      onImportConsumed();
    }}>Apri scenario predefinito</button
  >
{:else if !ready}<p role="status">Verifica versione dello scenario…</p>
{:else}
  {#if workflow === 'installed' && fastReformCandidate}<p role="status">
      Questo archivio usa il generatore precedente. Modello, scenario e
      risultati conservano la loro versione originale.
    </p>{/if}
  {#key workflow}
    {#if workflow === 'fast'}
      <FastReformWorkspace
        imported={input.payload}
        importedFile={input.file}
        importedScenario={input.scenario}
        {onLegacyImport}
        onImportConsumed={consumed}
        onRouteFile={routeFile}
      />
    {:else}
      <InstalledReformWorkspace
        imported={input.payload}
        importedFile={input.file}
        importedScenario={input.scenario}
        {onLegacyImport}
        onImportConsumed={consumed}
        onRouteFile={routeFile}
      />
    {/if}
  {/key}
{/if}
