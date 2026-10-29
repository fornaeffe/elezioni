<script lang="ts">
  import { fastReformCandidate } from './vote-share/build';
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
</script>

{#if fastReformCandidate}
  <FastReformWorkspace
    {imported}
    {importedFile}
    {importedScenario}
    {onLegacyImport}
    {onImportConsumed}
  />
{:else}
  <InstalledReformWorkspace
    {imported}
    {importedFile}
    {importedScenario}
    {onLegacyImport}
    {onImportConsumed}
  />
{/if}
