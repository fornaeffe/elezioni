<script lang="ts">
  import type { PoliticsScenario } from '../scenario/schema';
  import { REFORM } from '../scenario/schema';
  import { activeGenerator } from '../scenario/registry';
  import { editScenario, type ApplyEdit } from './model';
  import LazySection from './LazySection.svelte';
  /** CP-07 supplies task ownership. Presentation never starts or randomizes execution implicitly. */
  export interface ExecutionControls {
    busy: boolean; preparing?: boolean; status: string; progress?: string; onRun: () => void; onCancel: () => void;
    onFit?: () => void; onImportCheckpoint?: (file: File) => void; onExportCheckpoint?: () => void;
    workerInfo?: { requested: string; recommended: number; maximum: number; memoryMiB: number; warning: boolean };
  }
  let { scenario, edit, execution }: { scenario: PoliticsScenario; edit: ApplyEdit; execution?: ExecutionControls } = $props();
  let model = $derived(activeGenerator(scenario));
  function seed(key: 'fittingSeed' | 'simulationSeed') { edit(s => editScenario(s, n => { n.execution[key] = crypto.randomUUID(); })); }
</script>
<section aria-labelledby="run-title">
  <h2 id="run-title">Simulazione</h2>
  <fieldset disabled={execution?.busy}>
    <label>Numero di simulazioni<input type="number" min="1" step="1" value={scenario.execution.simulations} onchange={e => edit(s => editScenario(s, n => { n.execution.simulations = Number(e.currentTarget.value); }))} /></label>
    <details class="advanced">
      <summary>Impostazioni avanzate della simulazione</summary>
      <div class="advanced-content">
        <div class="row"><label>Seed del fitting<input value={scenario.execution.fittingSeed} onchange={e => edit(s => editScenario(s, n => { n.execution.fittingSeed = e.currentTarget.value; }))} /></label><button aria-label="Genera seed casuale del fitting" onclick={() => seed('fittingSeed')}>Genera seed casuale</button></div>
        <div class="row"><label>Seed delle simulazioni<input value={scenario.execution.simulationSeed} onchange={e => edit(s => editScenario(s, n => { n.execution.simulationSeed = e.currentTarget.value; }))} /></label><button aria-label="Genera seed casuale delle simulazioni" onclick={() => seed('simulationSeed')}>Genera seed casuale</button></div>
        <label>Worker richiesti<select value={scenario.execution.workers.kind} onchange={e => edit(s => editScenario(s, n => { n.execution.workers = e.currentTarget.value === 'auto' ? { kind: 'auto', policy: 'model-aware-v1' } : { kind: 'fixed', count: 1 }; }))}><option value="auto">Automatici · in base al modello</option><option value="fixed">Numero esplicito</option></select></label>
        {#if scenario.execution.workers.kind === 'fixed'}<label>Numero di worker richiesti<input type="number" min="1" step="1" value={scenario.execution.workers.count} onchange={e => edit(s => editScenario(s, n => { n.execution.workers = { kind: 'fixed', count: Number(e.currentTarget.value) }; }))} /></label>{/if}
        {#if execution?.workerInfo}
          <p class="worker-info">Worker richiesti: {execution.workerInfo.requested} · consigliati {execution.workerInfo.recommended} · massimo CPU {execution.workerInfo.maximum}. Stima per worker: {execution.workerInfo.memoryMiB} MiB.</p>
          {#if execution.workerInfo.warning}<p class="worker-warning" role="status">Se il browser dovesse esaurire la memoria disponibile, riprova riducendo il numero di worker.</p>{/if}
        {/if}
        <LazySection title="Generazione delle candidature">
          {#if scenario.selection.scrutiny.id === REFORM}
            <label>Probabilità di riuso candidato<input type="number" min="0" max="1" step="any" value={scenario.lawSettings[REFORM].candidateGeneration.reuseProbability} onchange={e => edit(s => editScenario(s, n => { n.lawSettings[REFORM].candidateGeneration.reuseProbability = Number(e.currentTarget.value); }))} /></label>
            <label>Massimo candidature ordinarie<input type="number" min="1" max="5" step="1" value={scenario.lawSettings[REFORM].candidateGeneration.maximumOrdinaryCandidacies} onchange={e => edit(s => editScenario(s, n => { n.lawSettings[REFORM].candidateGeneration.maximumOrdinaryCandidacies = Number(e.currentTarget.value); }))} /></label>
          {:else}
            <label>Quota riuso uninominale nel plurinominale<input type="number" min="0" max="1" step="any" value={scenario.lawSettings['politiche-r-parity-v1'].candidateGeneration.uninominalToPlurinominalShare} onchange={e => edit(s => editScenario(s, n => { n.lawSettings['politiche-r-parity-v1'].candidateGeneration.uninominalToPlurinominalShare = Number(e.currentTarget.value); }))} /></label>
            {#each scenario.lawSettings['politiche-r-parity-v1'].candidateGeneration.plurinominalCandidacyCountShares as share, i (i)}
              <label>Probabilità di {i + 1} candidature<input type="number" min="0" max="1" step="any" value={share} onchange={e => edit(s => editScenario(s, n => { n.lawSettings['politiche-r-parity-v1'].candidateGeneration.plurinominalCandidacyCountShares[i] = Number(e.currentTarget.value); }))} /></label>
            {/each}
            <p>Le cinque probabilità devono sommare a uno.</p>
          {/if}
        </LazySection>
        {#if model.id === 'M02'}
          <LazySection title="Fitting e checkpoint">
            <p>Il modello selezionato viene caricato automaticamente. Il checkpoint deve corrispondere esattamente allo scenario e alle dipendenze.</p>
            <button disabled={!execution?.onFit} onclick={() => execution?.onFit?.()}>Ricalcola fitting</button>
            <label>Carica checkpoint<input type="file" accept=".json,.gz" disabled={!execution?.onImportCheckpoint} onchange={e => { const file = e.currentTarget.files?.[0]; if (file) execution?.onImportCheckpoint?.(file); e.currentTarget.value = ''; }} /></label>
            <button disabled={!execution?.onExportCheckpoint} onclick={() => execution?.onExportCheckpoint?.()}>Scarica checkpoint</button>
          </LazySection>
        {/if}
      </div>
    </details>
    <button disabled={!execution} onclick={() => execution?.onRun()}>Avvia simulazione</button>
  </fieldset>
</section>
<style>
  .advanced { margin: 1rem 0; padding: .75rem; border: 1px solid #cbd5e1; border-radius: .4rem; }
  .advanced > summary { cursor: pointer; font-weight: 600; }
  .advanced-content { padding-top: .5rem; }
  .worker-info, .worker-warning { overflow-wrap: anywhere; }
  .worker-warning { color: #8a4b08; }
</style>
