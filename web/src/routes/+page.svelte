<script lang="ts">
  import { ChevronDown, ChevronUp, Download, Plus, Play, RotateCcw, Trash2, Upload } from '@lucide/svelte';
  import { browser } from '$app/environment';
  import { onMount } from 'svelte';
  import SimulationWorker from '$lib/workers/simulation.worker?worker';
  import type {
    ResultTable,
    Scenario,
    ScenarioListCorrespondence,
    SimulationRequest,
    SimulationResult,
    SimulationWorkerMessage
  } from '$lib/core/types';
  import { createSimulationResultExport, resultTablesToCsv } from '$lib/core/result-export';
  import {
    cloneScenario,
    createDefaultPoliticsScenario,
    createScenarioCoalition,
    createScenarioList,
    createScenarioListCorrespondence,
    defaultPoliticsSourceModelListNames,
    parseScenario,
    politicsScenarioStorageKey,
    serializeScenario,
    validateScenario
  } from '$lib/scenario/politics';

  const dataVersion = 'v1';
  const diagnosticTableNames = new Set(['Generated pipeline runs']);

  let simulations = $state(10);
  let seed = $state('politiche-2027');
  let running = $state(false);
  let phase = $state('idle');
  let elapsedMs = $state(0);
  let tables = $state<ResultTable[]>([]);
  let warnings = $state<string[]>([]);
  let lastResult = $state<SimulationResult | null>(null);
  let showDiagnostics = $state(false);
  let showAdvancedScenario = $state(false);
  let scenarioDraft = $state<Scenario>(createDefaultPoliticsScenario());
  let scenarioStorageReady = $state(false);
  let fileInput: HTMLInputElement | undefined;

  const scenario = $derived(cloneScenario(scenarioDraft));
  const validationMessages = $derived(validateScenario(scenario));
  const canRun = $derived(!running && validationMessages.length === 0);
  const runButtonLabel = $derived(running ? phase : 'Esegui');
  const elapsedLabel = $derived(`${elapsedMs.toFixed(0)} ms`);
  const hasResult = $derived(lastResult !== null);
  const primaryTables = $derived(
    tables
      .filter((table) => !diagnosticTableNames.has(table.name))
      .sort((left, right) => resultTablePriority(left.name) - resultTablePriority(right.name))
  );
  const diagnosticTables = $derived(tables.filter((table) => diagnosticTableNames.has(table.name)));
  const diagnosticsToggleLabel = $derived(showDiagnostics ? 'Nascondi dettagli' : 'Mostra dettagli');
  const manualListCorrespondences = $derived(
    scenarioDraft.listCorrespondences.filter((correspondence) => correspondence.source === 'manual')
  );
  const bundledListCorrespondenceCount = $derived(
    scenarioDraft.listCorrespondences.filter((correspondence) => correspondence.source === 'bundled').length
  );

  onMount(() => {
    if (!browser) return;

    const stored = localStorage.getItem(politicsScenarioStorageKey);
    if (stored) {
      try {
        scenarioDraft = parseScenario(stored);
      } catch (error) {
        warnings = [`SCENARIO_STORAGE_ERROR: ${error instanceof Error ? error.message : String(error)}`];
      }
    }

    scenarioStorageReady = true;
  });

  $effect(() => {
    if (!browser || !scenarioStorageReady) return;
    localStorage.setItem(politicsScenarioStorageKey, serializeScenario(scenario));
  });

  function addList(): void {
    scenarioDraft.lists = [...scenarioDraft.lists, createScenarioList(scenarioDraft.coalitions, scenarioDraft.lists)];
  }

  function removeList(id: string): void {
    const removed = scenarioDraft.lists.find((row) => row.id === id);
    scenarioDraft.lists = scenarioDraft.lists.filter((row) => row.id !== id);
    if (removed) {
      scenarioDraft.listCorrespondences = scenarioDraft.listCorrespondences.filter(
        (correspondence) => correspondence.source !== 'manual' || correspondence.futureList !== removed.name
      );
    }
  }

  function addCoalition(): void {
    scenarioDraft.coalitions = [...scenarioDraft.coalitions, createScenarioCoalition(scenarioDraft.coalitions)];
  }

  function updateCoalitionName(id: string, name: string): void {
    const coalition = scenarioDraft.coalitions.find((row) => row.id === id);
    const previousName = coalition?.name;
    scenarioDraft.coalitions = scenarioDraft.coalitions.map((row) => (row.id === id ? { ...row, name } : row));

    if (previousName !== undefined) {
      scenarioDraft.lists = scenarioDraft.lists.map((row) =>
        row.coalition === previousName ? { ...row, coalition: name } : row
      );
    }
  }

  function removeCoalition(id: string): void {
    const removed = scenarioDraft.coalitions.find((row) => row.id === id);
    const coalitions = scenarioDraft.coalitions.filter((row) => row.id !== id);
    const fallback = coalitions[0]?.name ?? null;

    scenarioDraft.coalitions = coalitions;
    if (removed) {
      scenarioDraft.lists = scenarioDraft.lists.map((row) =>
        row.coalition === removed.name ? { ...row, coalition: fallback } : row
      );
    }
  }

  function addListCorrespondence(): void {
    scenarioDraft.listCorrespondences = [
      ...scenarioDraft.listCorrespondences,
      createScenarioListCorrespondence(scenarioDraft)
    ];
  }

  function updateListCorrespondence(id: string, patch: Partial<ScenarioListCorrespondence>): void {
    scenarioDraft.listCorrespondences = scenarioDraft.listCorrespondences.map((row) =>
      row.id === id && row.source === 'manual' ? { ...row, ...patch } : row
    );
  }

  function removeListCorrespondence(id: string): void {
    scenarioDraft.listCorrespondences = scenarioDraft.listCorrespondences.filter((row) => row.id !== id);
  }

  function resetScenario(): void {
    scenarioDraft = createDefaultPoliticsScenario();
    tables = [];
    warnings = [];
    lastResult = null;
    showDiagnostics = false;
    showAdvancedScenario = false;
  }

  function scenarioFilename(): string {
    const slug = scenario.name
      .trim()
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-|-$/g, '');
    return `${slug || 'scenario'}.json`;
  }

  function downloadScenario(): void {
    if (!browser) return;

    downloadText(serializeScenario(scenario), 'application/json', scenarioFilename());
  }

  function downloadText(content: string, type: string, filename: string): void {
    const blob = new Blob([content], { type });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = filename;
    link.click();
    URL.revokeObjectURL(url);
  }

  function resultFilename(extension: 'csv' | 'json'): string {
    return `${scenarioFilename().replace(/\.json$/, '')}-risultati.${extension}`;
  }

  function downloadResultsJson(): void {
    if (!browser || !lastResult) return;

    const payload = createSimulationResultExport({
      result: lastResult,
      scenario,
      exportedAt: new Date().toISOString()
    });
    downloadText(JSON.stringify(payload, null, 2), 'application/json', resultFilename('json'));
  }

  function downloadResultsCsv(): void {
    if (!browser || !lastResult) return;

    downloadText(resultTablesToCsv(lastResult.tables), 'text/csv;charset=utf-8', resultFilename('csv'));
  }

  function chooseScenarioFile(): void {
    fileInput?.click();
  }

  async function loadScenarioFile(event: Event): Promise<void> {
    const input = event.currentTarget as HTMLInputElement;
    const file = input.files?.[0];
    if (!file) return;

    try {
      scenarioDraft = parseScenario(await file.text());
      tables = [];
      warnings = [];
      lastResult = null;
      showDiagnostics = false;
      showAdvancedScenario = false;
    } catch (error) {
      warnings = [`SCENARIO_LOAD_ERROR: ${error instanceof Error ? error.message : String(error)}`];
    } finally {
      input.value = '';
    }
  }

  function runSimulation(): void {
    if (validationMessages.length > 0) {
      warnings = validationMessages.map((message) => `SCENARIO_VALIDATION: ${message}`);
      return;
    }

    const worker = new SimulationWorker();
    const scenarioSnapshot = cloneScenario(scenarioDraft);
    const request: SimulationRequest = {
      kind: 'politiche',
      scenario: scenarioSnapshot,
      electionDate: scenarioSnapshot.electionDate,
      simulations,
      seed,
      dataVersion
    };

    running = true;
    phase = 'validate';
    elapsedMs = 0;
    tables = [];
    warnings = [];
    lastResult = null;
    showDiagnostics = false;

    worker.onmessage = (event: MessageEvent<SimulationWorkerMessage>) => {
      const message = event.data;
      if (message.type === 'progress') {
        phase = message.phase;
        elapsedMs = message.elapsedMs;
        return;
      }

      lastResult = message;
      tables = message.tables;
      warnings = message.warnings.map((warning) => `${warning.code}: ${warning.message}`);
      elapsedMs = message.benchmark.elapsedMs;
      running = false;
      phase = message.status;
      worker.terminate();
    };

    worker.onerror = (error) => {
      warnings = [`WORKER_ERROR: ${error.message}`];
      elapsedMs = performance.now();
      running = false;
      phase = 'error';
      lastResult = null;
      worker.terminate();
    };

    try {
      worker.postMessage(request);
    } catch (error) {
      warnings = [`WORKER_POST_ERROR: ${error instanceof Error ? error.message : String(error)}`];
      elapsedMs = 0;
      running = false;
      phase = 'error';
      lastResult = null;
      worker.terminate();
    }
  }

  function resultTablePriority(name: string): number {
    if (name === 'Election overview') return 0;
    if (name === 'Average plurinominal seats by list') return 1;
    if (name === 'Vote share by list') return 2;
    if (name === 'Uninominal winners by support') return 3;
    if (name === 'Scenario projection') return 4;
    return 10;
  }
</script>

<svelte:head>
  <title>Elezioni</title>
</svelte:head>

<main class="workspace">
  <section class="toolbar" aria-label="Simulazione">
    <div>
      <h1>{scenarioDraft.name || 'Scenario politiche'}</h1>
      <p>Snapshot {dataVersion}</p>
    </div>
    <label>
      Simulazioni
      <input type="number" min="1" max="1000" step="1" bind:value={simulations} />
    </label>
    <label>
      Seed
      <input type="text" bind:value={seed} />
    </label>
    <button class="primary" type="button" onclick={runSimulation} disabled={!canRun}>
      <Play size={18} aria-hidden="true" />
      <span>{runButtonLabel}</span>
    </button>
  </section>

  <section class="grid">
    <div class="panel scenario-panel">
      <div class="panel-heading">
        <h2>Scenario</h2>
        <div class="panel-actions">
          <button
            type="button"
            class="icon-button"
            onclick={resetScenario}
            title="Ripristina scenario"
            aria-label="Ripristina scenario"
          >
            <RotateCcw size={18} aria-hidden="true" />
          </button>
          <button
            type="button"
            class="icon-button"
            onclick={chooseScenarioFile}
            title="Carica scenario"
            aria-label="Carica scenario"
          >
            <Upload size={18} aria-hidden="true" />
          </button>
          <button
            type="button"
            class="icon-button"
            onclick={downloadScenario}
            title="Scarica scenario"
            aria-label="Scarica scenario"
          >
            <Download size={18} aria-hidden="true" />
          </button>
        </div>
      </div>

      <input
        class="hidden-file"
        type="file"
        accept="application/json,.json"
        bind:this={fileInput}
        onchange={loadScenarioFile}
      />

      <div class="scenario-meta">
        <label>
          Nome scenario
          <input type="text" bind:value={scenarioDraft.name} />
        </label>
        <label>
          Data elezione
          <input type="date" bind:value={scenarioDraft.electionDate} />
        </label>
      </div>

      {#if validationMessages.length > 0}
        <div class="validation" data-testid="scenario-validation">
          {#each validationMessages as message}
            <p>{message}</p>
          {/each}
        </div>
      {/if}

      <div class="advanced">
        <button
          class="advanced-toggle"
          type="button"
          onclick={() => (showAdvancedScenario = !showAdvancedScenario)}
          aria-expanded={showAdvancedScenario}
          aria-controls="scenario-advanced"
        >
          {#if showAdvancedScenario}
            <ChevronUp size={18} aria-hidden="true" />
          {:else}
            <ChevronDown size={18} aria-hidden="true" />
          {/if}
          <span>Impostazioni avanzate</span>
        </button>

        {#if showAdvancedScenario}
          <div id="scenario-advanced" class="advanced-content">
            <div class="setting-row">
              <label>
                Astensione elettori
                <input
                  type="number"
                  min="0"
                  max="99.9"
                  step="0.1"
                  bind:value={scenarioDraft.abstentionShare}
                  oninput={() => (scenarioDraft.abstentionOverride = true)}
                  aria-label="Astensione elettori"
                />
              </label>
              <label class="override-toggle">
                <input
                  type="checkbox"
                  bind:checked={scenarioDraft.abstentionOverride}
                  aria-label="Usa astensione"
                />
                <span>Usa</span>
              </label>
            </div>

            <div class="correspondence-block">
              <div class="correspondence-heading">
                <div>
                  <span class="setting-label">Corrispondenze liste</span>
                  <span class="setting-meta">{bundledListCorrespondenceCount} bundled</span>
                </div>
                <button
                  type="button"
                  class="icon-button"
                  onclick={addListCorrespondence}
                  title="Aggiungi corrispondenza"
                  aria-label="Aggiungi corrispondenza"
                  disabled={scenarioDraft.lists.length === 0 || defaultPoliticsSourceModelListNames.length === 0}
                >
                  <Plus size={18} aria-hidden="true" />
                </button>
              </div>

              {#if manualListCorrespondences.length === 0}
                <p class="advanced-note">Nessuna corrispondenza manuale</p>
              {:else}
                <div class="correspondence-editor">
                  {#each manualListCorrespondences as correspondence (correspondence.id)}
                    <div class="correspondence-row">
                      <label>
                        Lista scenario
                        <select
                          value={correspondence.futureList}
                          onchange={(event) =>
                            updateListCorrespondence(correspondence.id, {
                              futureList: (event.currentTarget as HTMLSelectElement).value
                            })}
                          aria-label="Lista scenario corrispondenza"
                        >
                          {#each scenarioDraft.lists as list}
                            <option value={list.name}>{list.name}</option>
                          {/each}
                        </select>
                      </label>
                      <label>
                        Modello sorgente
                        <select
                          value={correspondence.pastList}
                          onchange={(event) =>
                            updateListCorrespondence(correspondence.id, {
                              pastList: (event.currentTarget as HTMLSelectElement).value
                            })}
                          aria-label="Lista modello corrispondenza"
                        >
                          {#each defaultPoliticsSourceModelListNames as listName}
                            <option value={listName}>{listName}</option>
                          {/each}
                        </select>
                      </label>
                      <button
                        type="button"
                        class="icon-button danger"
                        onclick={() => removeListCorrespondence(correspondence.id)}
                        title="Rimuovi corrispondenza"
                        aria-label="Rimuovi corrispondenza"
                      >
                        <Trash2 size={18} aria-hidden="true" />
                      </button>
                    </div>
                  {/each}
                </div>
              {/if}
            </div>
          </div>
        {/if}
      </div>

      <div class="section-heading">
        <h3>Coalizioni</h3>
        <button
          type="button"
          class="icon-button"
          onclick={addCoalition}
          title="Aggiungi coalizione"
          aria-label="Aggiungi coalizione"
        >
          <Plus size={18} aria-hidden="true" />
        </button>
      </div>

      <div class="coalition-editor">
        {#each scenarioDraft.coalitions as coalition (coalition.id)}
          <div class="coalition-row">
            <input class="color" type="color" bind:value={coalition.color} aria-label="Colore coalizione" />
            <input
              type="text"
              value={coalition.name}
              oninput={(event) => updateCoalitionName(coalition.id, (event.currentTarget as HTMLInputElement).value)}
              aria-label="Nome coalizione"
            />
            <button
              type="button"
              class="icon-button danger"
              onclick={() => removeCoalition(coalition.id)}
              disabled={scenarioDraft.coalitions.length <= 1}
              title="Rimuovi coalizione"
              aria-label="Rimuovi coalizione"
            >
              <Trash2 size={18} aria-hidden="true" />
            </button>
          </div>
        {/each}
      </div>

      <div class="section-heading">
        <h3>Liste</h3>
        <button
          type="button"
          class="icon-button"
          onclick={addList}
          title="Aggiungi lista"
          aria-label="Aggiungi lista"
        >
          <Plus size={18} aria-hidden="true" />
        </button>
      </div>

      <div class="list-editor">
        {#each scenarioDraft.lists as list (list.id)}
          <div class="list-row">
            <input class="color" type="color" bind:value={list.color} aria-label="Colore lista" />
            <input type="text" bind:value={list.name} aria-label="Nome lista" />
            <select bind:value={list.coalition} aria-label="Coalizione">
              {#each scenarioDraft.coalitions as coalition}
                <option value={coalition.name}>{coalition.name}</option>
              {/each}
            </select>
            <input
              class="share"
              type="number"
              min="0"
              max="100"
              step="0.1"
              bind:value={list.startingShare}
              oninput={() => (list.shareOverride = true)}
              aria-label="Quota iniziale"
            />
            <label class="override-toggle">
              <input
                type="checkbox"
                bind:checked={list.shareOverride}
                aria-label={`Usa quota ${list.name || 'lista'}`}
              />
              <span>Usa</span>
            </label>
            <button
              type="button"
              class="icon-button danger"
              onclick={() => removeList(list.id)}
              title="Rimuovi lista"
              aria-label="Rimuovi lista"
            >
              <Trash2 size={18} aria-hidden="true" />
            </button>
          </div>
        {/each}
      </div>
    </div>

    <div class="panel result-panel">
      <div class="panel-heading">
        <h2>Risultati</h2>
        <div class="result-heading-actions">
          {#if hasResult}
            <button type="button" class="text-button" onclick={downloadResultsJson} aria-label="Scarica risultati JSON">
              <Download size={16} aria-hidden="true" />
              <span>JSON</span>
            </button>
            <button type="button" class="text-button" onclick={downloadResultsCsv} aria-label="Scarica risultati CSV">
              <Download size={16} aria-hidden="true" />
              <span>CSV</span>
            </button>
          {/if}
          <span data-testid="elapsed-ms" data-phase={phase}>{elapsedLabel}</span>
        </div>
      </div>

      {#if warnings.length > 0}
        <div class="warnings">
          {#each warnings as warning}
            <p>{warning}</p>
          {/each}
        </div>
      {/if}

      {#each primaryTables as table}
        <table>
          <caption>{table.name}</caption>
          <thead>
            <tr>
              {#each table.columns as column}
                <th>{column}</th>
              {/each}
            </tr>
          </thead>
          <tbody>
            {#each table.rows as row}
              <tr>
                {#each table.columns as column}
                  <td>{row[column]}</td>
                {/each}
              </tr>
            {/each}
          </tbody>
        </table>
      {/each}

      {#if diagnosticTables.length > 0}
        <div class="diagnostics">
          <button
            class="diagnostic-toggle"
            type="button"
            onclick={() => (showDiagnostics = !showDiagnostics)}
            aria-expanded={showDiagnostics}
            aria-controls="diagnostic-tables"
          >
            {#if showDiagnostics}
              <ChevronUp size={18} aria-hidden="true" />
            {:else}
              <ChevronDown size={18} aria-hidden="true" />
            {/if}
            <span>{diagnosticsToggleLabel}</span>
          </button>

          {#if showDiagnostics}
            <div id="diagnostic-tables">
              {#each diagnosticTables as table}
                <table>
                  <caption>{table.name}</caption>
                  <thead>
                    <tr>
                      {#each table.columns as column}
                        <th>{column}</th>
                      {/each}
                    </tr>
                  </thead>
                  <tbody>
                    {#each table.rows as row}
                      <tr>
                        {#each table.columns as column}
                          <td>{row[column]}</td>
                        {/each}
                      </tr>
                    {/each}
                  </tbody>
                </table>
              {/each}
            </div>
          {/if}
        </div>
      {/if}
    </div>
  </section>
</main>

<style>
  :global(body) {
    margin: 0;
    background: #f4f6f7;
    color: #182026;
    font-family:
      Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  }

  button,
  input,
  select {
    font: inherit;
  }

  .workspace {
    min-height: 100vh;
    padding: 24px;
  }

  .toolbar {
    display: grid;
    grid-template-columns: minmax(220px, 1fr) 160px 220px auto;
    gap: 16px;
    align-items: end;
    margin: 0 auto 20px;
    max-width: 1180px;
  }

  h1,
  h2,
  p {
    margin: 0;
  }

  h1 {
    font-size: 28px;
    font-weight: 750;
  }

  h2 {
    font-size: 16px;
    font-weight: 700;
  }

  h3 {
    margin: 0;
    color: #4d5963;
    font-size: 13px;
    font-weight: 750;
  }

  .toolbar p,
  .panel-heading span {
    color: #697681;
    font-size: 13px;
  }

  label {
    display: grid;
    gap: 6px;
    color: #4d5963;
    font-size: 13px;
    font-weight: 650;
  }

  input,
  select {
    min-height: 38px;
    border: 1px solid #c9d0d6;
    border-radius: 6px;
    background: #ffffff;
    color: #182026;
    padding: 0 10px;
  }

  .grid {
    display: grid;
    grid-template-columns: minmax(0, 1.1fr) minmax(320px, 0.9fr);
    gap: 20px;
    max-width: 1180px;
    margin: 0 auto;
  }

  .panel {
    border: 1px solid #d8dee3;
    border-radius: 8px;
    background: #ffffff;
    min-width: 0;
  }

  .panel-heading {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    min-height: 52px;
    border-bottom: 1px solid #e5e9ed;
    padding: 0 16px;
  }

  .panel-actions {
    display: flex;
    gap: 8px;
  }

  .result-heading-actions {
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .hidden-file {
    display: none;
  }

  .scenario-meta {
    display: grid;
    grid-template-columns: minmax(160px, 1fr) 160px;
    gap: 12px;
    padding: 16px 16px 0;
  }

  .validation {
    margin: 16px 16px 0;
    border-left: 4px solid #9d2d2d;
    background: #fff1f1;
    padding: 10px 12px;
    color: #7c2020;
    font-size: 13px;
  }

  .validation p + p {
    margin-top: 4px;
  }

  .advanced {
    padding: 16px 16px 0;
  }

  .advanced-toggle {
    width: 100%;
    justify-content: flex-start;
    background: #f7f9fa;
    color: #4d5963;
    font-weight: 700;
  }

  .advanced-toggle span {
    flex: 1;
    text-align: left;
  }

  .advanced-content {
    display: grid;
    gap: 12px;
    border-bottom: 1px solid #e5e9ed;
    padding: 12px 0 16px;
  }

  .setting-row {
    display: grid;
    grid-template-columns: minmax(140px, 1fr) auto;
    gap: 12px;
    align-items: center;
  }

  .setting-label {
    color: #4d5963;
    font-size: 13px;
    font-weight: 700;
  }

  .setting-meta {
    display: block;
    margin-top: 3px;
    color: #697681;
    font-size: 12px;
    font-weight: 650;
  }

  .correspondence-block {
    display: grid;
    gap: 10px;
  }

  .correspondence-heading {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
  }

  .advanced-note {
    color: #697681;
    font-size: 13px;
  }

  .correspondence-editor {
    display: grid;
    gap: 10px;
  }

  .correspondence-row {
    display: grid;
    grid-template-columns: minmax(130px, 1fr) minmax(130px, 1fr) 40px;
    gap: 8px;
    align-items: end;
  }

  .section-heading {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    padding: 16px 16px 0;
  }

  .coalition-editor {
    display: grid;
    gap: 10px;
    padding: 12px 16px 0;
  }

  .coalition-row {
    display: grid;
    grid-template-columns: 40px minmax(130px, 1fr) 40px;
    gap: 8px;
    align-items: center;
  }

  .list-editor {
    display: grid;
    gap: 10px;
    padding: 12px 16px 16px;
  }

  .list-row {
    display: grid;
    grid-template-columns: 40px minmax(130px, 1fr) minmax(130px, 0.8fr) 86px 64px 40px;
    gap: 8px;
    align-items: center;
  }

  .color {
    width: 40px;
    padding: 3px;
  }

  .share {
    text-align: right;
  }

  .override-toggle {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 6px;
    min-height: 38px;
    color: #4d5963;
    font-size: 12px;
    font-weight: 700;
  }

  .override-toggle input {
    min-height: auto;
    width: 16px;
    height: 16px;
    margin: 0;
    padding: 0;
  }

  button {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    gap: 8px;
    min-height: 38px;
    border: 1px solid #bdc7d0;
    border-radius: 6px;
    background: #ffffff;
    color: #182026;
    cursor: pointer;
  }

  button:disabled {
    cursor: not-allowed;
    opacity: 0.7;
  }

  .primary {
    min-width: 116px;
    border-color: #2f6f57;
    background: #2f6f57;
    color: #ffffff;
    font-weight: 700;
  }

  .icon-button {
    width: 38px;
    padding: 0;
  }

  .text-button {
    min-height: 32px;
    padding: 0 10px;
    color: #4d5963;
    font-size: 12px;
    font-weight: 700;
  }

  .text-button span {
    color: inherit;
    font-size: inherit;
  }

  .danger {
    color: #9d2d2d;
  }

  .result-panel {
    overflow: hidden;
  }

  .warnings {
    margin: 16px;
    border-left: 4px solid #c4822e;
    background: #fff7ec;
    padding: 10px 12px;
    color: #6f4315;
    font-size: 13px;
  }

  .diagnostics {
    border-top: 1px solid #e5e9ed;
    padding: 12px 16px 0;
  }

  .diagnostic-toggle {
    width: 100%;
    justify-content: flex-start;
    background: #f7f9fa;
    color: #4d5963;
    font-weight: 700;
  }

  .diagnostic-toggle span {
    flex: 1;
    text-align: left;
  }

  table {
    width: calc(100% - 32px);
    margin: 16px;
    border-collapse: collapse;
    font-size: 13px;
  }

  .diagnostics table {
    width: 100%;
    margin: 12px 0 16px;
  }

  caption {
    margin-bottom: 8px;
    text-align: left;
    color: #4d5963;
    font-weight: 700;
  }

  th,
  td {
    border-bottom: 1px solid #e5e9ed;
    padding: 8px 6px;
    text-align: left;
  }

  th {
    color: #5c6872;
    font-weight: 700;
  }

  @media (max-width: 860px) {
    .workspace {
      padding: 16px;
    }

    .toolbar,
    .grid,
    .scenario-meta {
      grid-template-columns: 1fr;
    }

    .list-row {
      grid-template-columns: 40px minmax(0, 1fr) 78px 58px 40px;
    }

    .setting-row,
    .correspondence-row {
      grid-template-columns: 1fr;
    }

    .list-row select {
      grid-column: 2 / -1;
    }
  }
</style>
