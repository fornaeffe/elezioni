<script lang="ts">
  import { Download, Play, Plus, RotateCcw, Trash2, Upload } from '@lucide/svelte';
  import { browser } from '$app/environment';
  import { onMount, tick } from 'svelte';
  import SimulationWorker from '$lib/workers/simulation.worker?worker';
  import RegionalErResultCharts from '$lib/regional-er/RegionalErResultCharts.svelte';
  import type {
    ResultTable,
    Scenario,
    ScenarioListCorrespondence,
    ScenarioLocalShareOverride,
    ScenarioLocalShareOverrideScope,
    ScrutinyWarning,
    SimulationRequest,
    SimulationResult,
    SimulationWorkerMessage
  } from '$lib/core/types';
  import {
    createSimulationResultExport,
    parseSimulationResultExport,
    resultTablesToCsv,
    type SimulationResultExport
  } from '$lib/core/result-export';
  import { buildRegionalErResultCharts } from '$lib/regional-er/result-charts';
  import { regionalErResultPlotTableNames } from '$lib/regional-er/result-presentation';
  import {
    buildRegionalErScenarioLocalShareOverrideGroups,
    cloneRegionalErScenario,
    createDefaultRegionalErScenario,
    createRegionalErScenarioCoalition,
    createRegionalErScenarioList,
    parseRegionalErScenario,
    regionalErAbstentionListName,
    regionalErScenarioStorageKey,
    removeRegionalErScenarioCoalition,
    removeRegionalErScenarioList,
    removeRegionalErScenarioLocalShareOverride,
    renameRegionalErScenarioCoalition,
    renameRegionalErScenarioList,
    serializeRegionalErScenario,
    updateRegionalErScenarioHistoricalCorrespondence,
    updateRegionalErScenarioLocalShareOverride,
    upsertRegionalErScenarioLocalShareOverride,
    validateRegionalErScenario
  } from '$lib/scenario/regional-er';
  import { generatedPoliticsMunicipalities } from '$lib/scenario/politics-municipalities.generated';

  type UiMessageSeverity = 'info' | 'warning' | 'error';

  interface UiMessage {
    code: string;
    message: string;
    severity: UiMessageSeverity;
  }

  interface LocationOption {
    scope: ScenarioLocalShareOverrideScope;
    code: string;
    label: string;
    sublabel: string;
    searchText: string;
  }

  const dataVersion = 'v1';
  const diagnosticTableNames = new Set(['Generated regional runs']);
  const internalTableNames = new Set<string>(regionalErResultPlotTableNames);
  const regionalMunicipalities = generatedPoliticsMunicipalities.filter((row) => row.REGIONE === 'Emilia-Romagna');

  let simulations = $state(10);
  let seed = $state('regionali-er-2027');
  let running = $state(false);
  let busyAction = $state('');
  let phase = $state('idle');
  let elapsedMs = $state(0);
  let tables = $state<ResultTable[]>([]);
  let messages = $state<UiMessage[]>([]);
  let lastResult = $state<SimulationResult | null>(null);
  let showDiagnostics = $state(false);
  let showAdvancedScenario = $state(false);
  let scenarioDraft = $state<Scenario>(createDefaultRegionalErScenario());
  let scenarioStorageReady = $state(false);
  let compressionSupported = $state(false);
  let selectedLocalOverrideScope = $state<ScenarioLocalShareOverrideScope>('municipality');
  let localOverrideSearch = $state('');
  let selectedLocalOverrideLocationCode = $state('');
  let selectedLocalOverrideList = $state('');
  let localOverrideShare = $state(0);
  let fileInput: HTMLInputElement | undefined;
  let resultFileInput: HTMLInputElement | undefined;

  const scenario = $derived(cloneRegionalErScenario(scenarioDraft));
  const validationMessages = $derived(validateRegionalErScenario(scenario));
  const isBusy = $derived(running || busyAction !== '');
  const canRun = $derived(!isBusy && validationMessages.length === 0);
  const runButtonLabel = $derived(running ? phase : 'Esegui');
  const elapsedLabel = $derived(`${elapsedMs.toFixed(0)} ms`);
  const infoMessages = $derived(messages.filter((message) => message.severity === 'info'));
  const warningMessages = $derived(messages.filter((message) => message.severity !== 'info'));
  const primaryTables = $derived(
    tables
      .filter((table) => !diagnosticTableNames.has(table.name) && !internalTableNames.has(table.name))
      .sort((left, right) => resultTablePriority(left.name) - resultTablePriority(right.name))
  );
  const diagnosticTables = $derived(tables.filter((table) => diagnosticTableNames.has(table.name)));
  const resultCharts = $derived(buildRegionalErResultCharts(tables, scenario));
  const localOverrideGroups = $derived(buildRegionalErScenarioLocalShareOverrideGroups(scenarioDraft));
  const localOverrideLocationOptions = $derived(
    buildLocationOptions().filter((option) => option.scope === selectedLocalOverrideScope)
  );
  const localOverrideSearchResults = $derived(searchLocations(localOverrideSearch, localOverrideLocationOptions));
  const selectedLocalOverrideListName = $derived(selectedLocalOverrideList || scenarioDraft.lists[0]?.name || '');

  onMount(() => {
    if (!browser) return;
    compressionSupported = 'CompressionStream' in globalThis && 'DecompressionStream' in globalThis;

    const stored = localStorage.getItem(regionalErScenarioStorageKey);
    if (stored) {
      try {
        scenarioDraft = parseRegionalErScenario(stored);
      } catch (error) {
        messages = [uiMessage('SCENARIO_STORAGE_ERROR', error instanceof Error ? error.message : String(error), 'warning')];
      }
    }
    scenarioStorageReady = true;
  });

  $effect(() => {
    if (!browser || !scenarioStorageReady) return;
    localStorage.setItem(regionalErScenarioStorageKey, serializeRegionalErScenario(scenario));
  });

  function clearDisplayedResults(): void {
    tables = [];
    lastResult = null;
    showDiagnostics = false;
  }

  function addList(): void {
    scenarioDraft.lists = [...scenarioDraft.lists, createRegionalErScenarioList(scenarioDraft.coalitions, scenarioDraft.lists)];
    clearDisplayedResults();
  }

  function removeList(id: string): void {
    scenarioDraft = removeRegionalErScenarioList(scenarioDraft, id);
    clearDisplayedResults();
  }

  function addCoalition(): void {
    scenarioDraft.coalitions = [...scenarioDraft.coalitions, createRegionalErScenarioCoalition(scenarioDraft.coalitions)];
    clearDisplayedResults();
  }

  function updateCoalitionName(id: string, name: string): void {
    scenarioDraft = renameRegionalErScenarioCoalition(scenarioDraft, id, name);
    clearDisplayedResults();
  }

  function removeCoalition(id: string): void {
    scenarioDraft = removeRegionalErScenarioCoalition(scenarioDraft, id);
    clearDisplayedResults();
  }

  function updateCorrespondence(id: string, patch: Partial<ScenarioListCorrespondence>): void {
    scenarioDraft = updateRegionalErScenarioHistoricalCorrespondence(scenarioDraft, id, patch);
    clearDisplayedResults();
  }

  function resetScenario(): void {
    scenarioDraft = createDefaultRegionalErScenario();
    messages = [uiMessage('SCENARIO_RESET', 'Scenario Emilia-Romagna ripristinato.', 'info')];
    clearDisplayedResults();
  }

  function catalogCode(value: string | number): string {
    return String(value).trim();
  }

  function normalizeSearchText(value: string): string {
    return value
      .trim()
      .toLocaleLowerCase('it-IT')
      .normalize('NFKD')
      .replace(/[\u0300-\u036f]/g, '');
  }

  function locationKey(scope: ScenarioLocalShareOverrideScope, code: string): string {
    return `${scope}\u001f${code}`;
  }

  function scopeLabel(scope: ScenarioLocalShareOverrideScope): string {
    if (scope === 'region') return 'Regione';
    if (scope === 'province') return 'Provincia';
    return 'Comune';
  }

  function buildLocationOptions(): LocationOption[] {
    const municipalityOptions = regionalMunicipalities.map((row) => {
      const code = catalogCode(row.CODICE_COMUNE);
      const label = `${row.COMUNE} (${code})`;
      const sublabel = `${row.PROVINCIA} - ${row.REGIONE}`;
      return {
        scope: 'municipality' as const,
        code,
        label,
        sublabel,
        searchText: normalizeSearchText(`${label} ${sublabel}`)
      };
    });
    const provinceGroups = new Map<string, typeof regionalMunicipalities>();
    const regionGroups = new Map<string, typeof regionalMunicipalities>();

    for (const row of regionalMunicipalities) {
      const provinceCode = catalogCode(row.CODICE_PROVINCIA);
      const regionCode = catalogCode(row.CODICE_REGIONE);
      provinceGroups.set(provinceCode, [...(provinceGroups.get(provinceCode) ?? []), row]);
      regionGroups.set(regionCode, [...(regionGroups.get(regionCode) ?? []), row]);
    }

    const provinceOptions = [...provinceGroups.entries()].map(([code, rows]) => {
      const first = rows[0];
      const label = `${first?.PROVINCIA ?? code} (${code})`;
      const sublabel = `${first?.REGIONE ?? ''} - ${rows.length} comuni`;
      return {
        scope: 'province' as const,
        code,
        label,
        sublabel,
        searchText: normalizeSearchText(`${label} ${sublabel}`)
      };
    });
    const regionOptions = [...regionGroups.entries()].map(([code, rows]) => {
      const first = rows[0];
      const label = `${first?.REGIONE ?? code} (${code})`;
      const sublabel = `${rows.length} comuni`;
      return {
        scope: 'region' as const,
        code,
        label,
        sublabel,
        searchText: normalizeSearchText(`${label} ${sublabel}`)
      };
    });

    return [...municipalityOptions, ...provinceOptions, ...regionOptions].sort((left, right) =>
      `${left.scope}\u001f${left.label}`.localeCompare(`${right.scope}\u001f${right.label}`, 'it')
    );
  }

  function searchLocations(query: string, options: LocationOption[]): LocationOption[] {
    const normalized = normalizeSearchText(query);
    if (!normalized) return options.slice(0, 10);
    const tokens = normalized.split(/\s+/).filter(Boolean);
    return options.filter((option) => tokens.every((token) => option.searchText.includes(token))).slice(0, 10);
  }

  function selectLocation(option: LocationOption): void {
    selectedLocalOverrideScope = option.scope;
    selectedLocalOverrideLocationCode = option.code;
    localOverrideSearch = option.label;
  }

  function selectedLocationLabel(scope: ScenarioLocalShareOverrideScope, code: string): string {
    return buildLocationOptions().find((option) => locationKey(option.scope, option.code) === locationKey(scope, code))?.label ?? code;
  }

  function addSelectedLocalOverride(): void {
    if (!selectedLocalOverrideLocationCode || !selectedLocalOverrideListName) return;
    scenarioDraft = upsertRegionalErScenarioLocalShareOverride(scenarioDraft, {
      scope: selectedLocalOverrideScope,
      locationCode: selectedLocalOverrideLocationCode,
      list: selectedLocalOverrideListName,
      startingShare: localOverrideShare
    });
    clearDisplayedResults();
  }

  function updateLocalOverride(id: string, patch: Partial<ScenarioLocalShareOverride>): void {
    scenarioDraft = updateRegionalErScenarioLocalShareOverride(scenarioDraft, id, patch);
    clearDisplayedResults();
  }

  function removeLocalOverride(id: string): void {
    scenarioDraft = removeRegionalErScenarioLocalShareOverride(scenarioDraft, id);
    clearDisplayedResults();
  }

  function scenarioFilename(): string {
    const slug = scenario.name
      .toLocaleLowerCase('it-IT')
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-|-$/g, '');
    return `${slug || 'scenario-regionali-er'}.json`;
  }

  async function withBusy<T>(label: string, action: () => T | Promise<T>): Promise<T> {
    busyAction = label;
    try {
      await tick();
      return await action();
    } finally {
      busyAction = '';
    }
  }

  function downloadBlob(blob: Blob, filename: string): void {
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = filename;
    link.click();
    URL.revokeObjectURL(url);
  }

  function downloadText(content: string, type: string, filename: string): void {
    downloadBlob(new Blob([content], { type }), filename);
  }

  async function downloadScenario(): Promise<void> {
    await withBusy('download-scenario', () =>
      downloadText(serializeRegionalErScenario(scenario), 'application/json', scenarioFilename())
    );
  }

  function resultFilename(extension: 'csv' | 'json.gz'): string {
    const slug = scenario.name
      .toLocaleLowerCase('it-IT')
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-|-$/g, '');
    return `${slug || 'risultati-regionali-er'}-${lastResult?.benchmark.simulations ?? simulations}.${extension}`;
  }

  function currentResultExport() {
    if (!lastResult) return null;
    return createSimulationResultExport({
      result: lastResult,
      scenario,
      exportedAt: new Date().toISOString()
    });
  }

  async function gzipText(text: string): Promise<Blob> {
    if (!compressionSupported) return new Blob([text], { type: 'application/json' });
    const stream = new Blob([text], { type: 'application/json' }).stream().pipeThrough(new CompressionStream('gzip'));
    return await new Response(stream).blob();
  }

  function isGzipFile(file: File): boolean {
    return file.name.toLocaleLowerCase().endsWith('.gz') || file.type === 'application/gzip';
  }

  async function readJsonFile(file: File): Promise<string> {
    if (!isGzipFile(file)) return await file.text();
    const stream = file.stream().pipeThrough(new DecompressionStream('gzip'));
    return await new Response(stream).text();
  }

  async function downloadResultsGzip(): Promise<void> {
    if (!lastResult) return;
    await withBusy('download-results', async () => {
      const payload = currentResultExport();
      if (!payload) return;
      const blob = await gzipText(JSON.stringify(payload));
      downloadBlob(blob, resultFilename('json.gz'));
    });
  }

  async function downloadResultsCsv(): Promise<void> {
    const result = lastResult;
    if (!result) return;
    await withBusy('download-csv', () =>
      downloadText(resultTablesToCsv(result.tables), 'text/csv;charset=utf-8', resultFilename('csv'))
    );
  }

  function uiMessage(code: string, message: string, severity: UiMessageSeverity = 'warning'): UiMessage {
    return { code, message, severity };
  }

  function workerMessage(warning: ScrutinyWarning): UiMessage {
    return uiMessage(warning.code, warning.message, warning.severity ?? 'warning');
  }

  function looksLikeResultJson(text: string): boolean {
    try {
      const parsed = JSON.parse(text) as unknown;
      return parsed !== null && typeof parsed === 'object' && 'result' in parsed && 'scenario' in parsed;
    } catch {
      return false;
    }
  }

  function applyResultExport(payload: SimulationResultExport): void {
    const importedScenario = parseRegionalErScenario(JSON.stringify({ scenario: payload.scenario }));
    scenarioDraft = importedScenario;
    lastResult = payload.result;
    tables = payload.result.tables;
    messages = [uiMessage('RESULT_IMPORT', `Risultati importati dal JSON esportato il ${payload.exportedAt}.`, 'info')];
    elapsedMs = payload.result.benchmark.elapsedMs;
  }

  function chooseScenarioFile(): void {
    fileInput?.click();
  }

  function chooseResultFile(): void {
    resultFileInput?.click();
  }

  async function loadScenarioFile(event: Event): Promise<void> {
    const input = event.currentTarget as HTMLInputElement;
    const file = input.files?.[0];
    if (!file) return;

    try {
      const text = await readJsonFile(file);
      if (looksLikeResultJson(text)) {
        applyResultExport(parseSimulationResultExport(text));
      } else {
        scenarioDraft = parseRegionalErScenario(text);
        messages = [uiMessage('SCENARIO_IMPORT', `Scenario importato da ${file.name}.`, 'info')];
        clearDisplayedResults();
      }
    } catch (error) {
      messages = [uiMessage('SCENARIO_LOAD_ERROR', error instanceof Error ? error.message : String(error), 'error')];
    } finally {
      input.value = '';
    }
  }

  async function loadResultFile(event: Event): Promise<void> {
    const input = event.currentTarget as HTMLInputElement;
    const file = input.files?.[0];
    if (!file) return;

    try {
      if (!isGzipFile(file)) throw new Error('Carica risultati regionali come file .json.gz.');
      applyResultExport(parseSimulationResultExport(await readJsonFile(file)));
    } catch (error) {
      messages = [uiMessage('RESULT_LOAD_ERROR', error instanceof Error ? error.message : String(error), 'error')];
    } finally {
      input.value = '';
    }
  }

  function runSimulation(): void {
    if (!canRun) return;

    running = true;
    phase = 'avvio';
    elapsedMs = 0;
    messages = [];
    tables = [];
    lastResult = null;
    showDiagnostics = false;

    const worker = new SimulationWorker();
    const scenarioSnapshot = cloneRegionalErScenario(scenarioDraft);
    const request: SimulationRequest = {
      kind: 'regionali-er',
      scenario: scenarioSnapshot,
      electionDate: scenarioSnapshot.electionDate,
      simulations,
      seed,
      dataVersion
    };

    worker.onmessage = (event: MessageEvent<SimulationWorkerMessage>) => {
      const message = event.data;
      if (message.type === 'progress') {
        phase = message.phase;
        elapsedMs = message.elapsedMs;
        return;
      }

      running = false;
      worker.terminate();
      elapsedMs = message.benchmark.elapsedMs;
      lastResult = message;
      tables = message.tables;
      messages = message.warnings.map(workerMessage);
    };

    worker.onerror = (error) => {
      running = false;
      worker.terminate();
      messages = [uiMessage('WORKER_ERROR', error.message, 'error')];
    };

    worker.postMessage(request);
  }

  function resultTablePriority(name: string): number {
    const order = [
      'Regional election overview',
      'Regional coalition outcomes',
      'Regional average seats by list',
      'Regional vote share by list',
      'Regional province seats by list',
      'Scenario projection'
    ];
    const index = order.indexOf(name);
    return index === -1 ? 100 : index;
  }
</script>

<svelte:head>
  <title>Regionali Emilia-Romagna</title>
</svelte:head>

<main>
  <section class="toolbar" aria-label="Simulazione">
    <div>
      <a class="back-link" href="/">Simulatore elezioni</a>
      <h1>{scenarioDraft.name || 'Regionali Emilia-Romagna'}</h1>
      <p>Workflow regionale Emilia-Romagna senza generazione di candidati individuali.</p>
    </div>
    <div class="run-controls">
      <label>
        Simulazioni
        <input type="number" min="1" max="1000" bind:value={simulations} />
      </label>
      <label>
        Seed
        <input type="text" bind:value={seed} />
      </label>
      <button class="primary" type="button" disabled={!canRun} onclick={runSimulation}>
        <Play size={16} aria-hidden="true" />
        {runButtonLabel}
      </button>
      <span class="elapsed" data-testid="elapsed-ms">{elapsedLabel}</span>
    </div>
  </section>

  {#if validationMessages.length > 0}
    <section class="message-list warning" aria-label="Validazione scenario">
      {#each validationMessages as message}
        <p>{message}</p>
      {/each}
    </section>
  {/if}

  {#if infoMessages.length > 0}
    <section class="message-list info" aria-label="Note simulazione">
      {#each infoMessages as message}
        <p><strong>{message.code}</strong>: {message.message}</p>
      {/each}
    </section>
  {/if}

  {#if warningMessages.length > 0}
    <section class="message-list warning" aria-label="Avvisi simulazione">
      {#each warningMessages as message}
        <p><strong>{message.code}</strong>: {message.message}</p>
      {/each}
    </section>
  {/if}

  <section class="panel">
    <div class="section-heading">
      <h2>Scenario</h2>
      <div class="button-row">
        <button type="button" onclick={chooseScenarioFile}>
          <Upload size={16} aria-hidden="true" />
          Importa
        </button>
        <button type="button" onclick={downloadScenario}>
          <Download size={16} aria-hidden="true" />
          Scarica
        </button>
        <button type="button" onclick={resetScenario}>
          <RotateCcw size={16} aria-hidden="true" />
          Reset
        </button>
      </div>
      <input
        data-testid="regional-scenario-file-input"
        bind:this={fileInput}
        class="hidden-input"
        type="file"
        accept=".json,.json.gz,application/json,application/gzip"
        onchange={loadScenarioFile}
      />
    </div>

    <div class="form-grid">
      <label>
        Nome scenario
        <input type="text" bind:value={scenarioDraft.name} />
      </label>
      <label>
        Data elezione
        <input type="date" bind:value={scenarioDraft.electionDate} />
      </label>
      <label>
        Astensione elettori
        <input
          type="number"
          min="0"
          max="99"
          step="0.1"
          bind:value={scenarioDraft.abstentionShare}
          oninput={() => (scenarioDraft.abstentionOverride = true)}
        />
      </label>
      <label class="checkbox">
        <input type="checkbox" bind:checked={scenarioDraft.abstentionOverride} />
        Usa astensione
      </label>
    </div>

    <div class="section-heading compact">
      <h3>Coalizioni</h3>
      <button type="button" onclick={addCoalition}>
        <Plus size={16} aria-hidden="true" />
        Aggiungi
      </button>
    </div>
    <div class="rows">
      {#each scenarioDraft.coalitions as coalition (coalition.id)}
        <div class="coalition-row">
          <input
            aria-label="Nome coalizione"
            type="text"
            value={coalition.name}
            oninput={(event) => updateCoalitionName(coalition.id, (event.currentTarget as HTMLInputElement).value)}
          />
          <input aria-label="Colore coalizione" type="color" bind:value={coalition.color} />
          <button type="button" aria-label={`Rimuovi coalizione ${coalition.name}`} disabled={scenarioDraft.coalitions.length <= 1} onclick={() => removeCoalition(coalition.id)}>
            <Trash2 size={16} aria-hidden="true" />
          </button>
        </div>
      {/each}
    </div>

    <div class="section-heading compact">
      <h3>Liste</h3>
      <button type="button" onclick={addList}>
        <Plus size={16} aria-hidden="true" />
        Aggiungi
      </button>
    </div>
    <div class="rows">
      {#each scenarioDraft.lists as list (list.id)}
        <div class="list-row">
          <input
            aria-label="Nome lista"
            type="text"
            value={list.name}
            oninput={(event) => (scenarioDraft = renameRegionalErScenarioList(scenarioDraft, list.id, (event.currentTarget as HTMLInputElement).value))}
          />
          <select bind:value={list.coalition} aria-label={`Coalizione ${list.name}`}>
            {#each scenarioDraft.coalitions as coalition}
              <option value={coalition.name}>{coalition.name}</option>
            {/each}
          </select>
          <input aria-label={`Colore ${list.name}`} type="color" bind:value={list.color} />
          <input
            aria-label="Quota iniziale"
            type="number"
            min="0"
            max="100"
            step="0.1"
            bind:value={list.startingShare}
            oninput={() => (list.shareOverride = true)}
          />
          <label class="checkbox small">
            <input aria-label={`Usa quota ${list.name}`} type="checkbox" bind:checked={list.shareOverride} />
            Usa
          </label>
          <button type="button" aria-label={`Rimuovi lista ${list.name}`} disabled={scenarioDraft.lists.length <= 1} onclick={() => removeList(list.id)}>
            <Trash2 size={16} aria-hidden="true" />
          </button>
        </div>
      {/each}
    </div>

    <button class="link-button" type="button" onclick={() => (showAdvancedScenario = !showAdvancedScenario)}>
      {showAdvancedScenario ? 'Nascondi impostazioni avanzate' : 'Impostazioni avanzate'}
    </button>

    {#if showAdvancedScenario}
      <div class="advanced">
        <details>
          <summary>Corrispondenze storiche ({scenarioDraft.listCorrespondences.length})</summary>
          <div class="correspondence-list">
            {#each scenarioDraft.listCorrespondences as correspondence (correspondence.id)}
              <div class="correspondence-row">
                <span>{correspondence.pastElection} / {correspondence.pastList || '(vuota)'}</span>
                <select
                  aria-label="Destinazione corrispondenza"
                  value={correspondence.futureList}
                  onchange={(event) => updateCorrespondence(correspondence.id, { futureList: (event.currentTarget as HTMLSelectElement).value })}
                >
                  {#each [...scenarioDraft.lists.map((row) => row.name), regionalErAbstentionListName] as destination}
                    <option value={destination}>{destination}</option>
                  {/each}
                </select>
                <input
                  aria-label="Fattore corrispondenza"
                  type="number"
                  min="0.001"
                  step="0.001"
                  value={correspondence.factor}
                  oninput={(event) => updateCorrespondence(correspondence.id, { factor: Number((event.currentTarget as HTMLInputElement).value) })}
                />
              </div>
            {/each}
          </div>
        </details>

        <details open>
          <summary>Quote locali ({scenarioDraft.localShareOverrides.length})</summary>
          <div class="local-editor">
            <label>
              Ambito quota locale
              <select bind:value={selectedLocalOverrideScope} aria-label="Ambito quota locale">
                <option value="municipality">Comune</option>
                <option value="province">Provincia</option>
                <option value="region">Regione</option>
              </select>
            </label>
            <label>
              Cerca localita
              <input type="text" bind:value={localOverrideSearch} aria-label="Cerca localita" />
            </label>
            {#if localOverrideSearchResults.length > 0}
              <div class="location-results" role="listbox" aria-label="Localita trovate">
                {#each localOverrideSearchResults as option (locationKey(option.scope, option.code))}
                  <button type="button" onclick={() => selectLocation(option)}>
                    <span>{option.label}</span>
                    <span>{option.sublabel}</span>
                  </button>
                {/each}
              </div>
            {/if}
            <label>
              Lista quota locale
              <select bind:value={selectedLocalOverrideList} aria-label="Lista quota locale">
                {#each scenarioDraft.lists as list}
                  <option value={list.name}>{list.name}</option>
                {/each}
              </select>
            </label>
            <label>
              Quota locale da aggiungere
              <input type="number" min="0" max="100" step="0.1" bind:value={localOverrideShare} aria-label="Quota locale da aggiungere" />
            </label>
            <button type="button" disabled={!selectedLocalOverrideLocationCode || scenarioDraft.lists.length === 0} onclick={addSelectedLocalOverride}>
              <Plus size={16} aria-hidden="true" />
              Aggiungi quota locale
            </button>
          </div>

          <div class="local-groups">
            {#each localOverrideGroups as group (group.key)}
              <div class="local-group">
                <strong>{scopeLabel(group.scope)} {selectedLocationLabel(group.scope, group.locationCode)}</strong>
                <span>{group.totalShare.toFixed(1)}%</span>
                {#each group.overrides as override (override.id)}
                  <div class="local-row">
                    <select
                      aria-label={`Lista quota locale ${override.list}`}
                      value={override.list}
                      onchange={(event) => updateLocalOverride(override.id, { list: (event.currentTarget as HTMLSelectElement).value })}
                    >
                      {#each scenarioDraft.lists as list}
                        <option value={list.name}>{list.name}</option>
                      {/each}
                    </select>
                    <input
                      aria-label={`Quota locale ${override.list}`}
                      type="number"
                      min="0"
                      max="100"
                      step="0.1"
                      value={override.startingShare}
                      oninput={(event) => updateLocalOverride(override.id, { startingShare: Number((event.currentTarget as HTMLInputElement).value) })}
                    />
                    <button type="button" aria-label={`Rimuovi quota locale ${override.list}`} onclick={() => removeLocalOverride(override.id)}>
                      <Trash2 size={16} aria-hidden="true" />
                    </button>
                  </div>
                {/each}
              </div>
            {/each}
          </div>
        </details>
      </div>
    {/if}
  </section>

  <section class="panel">
    <div class="section-heading">
      <h2>Risultati</h2>
      <div class="button-row">
        <button type="button" disabled={!lastResult} onclick={downloadResultsGzip}>
          <Download size={16} aria-hidden="true" />
          Scarica risultati compressi
        </button>
        <button type="button" disabled={!lastResult} onclick={downloadResultsCsv}>
          <Download size={16} aria-hidden="true" />
          Scarica risultati CSV
        </button>
        <button type="button" onclick={chooseResultFile}>
          <Upload size={16} aria-hidden="true" />
          Importa risultati
        </button>
      </div>
      <input
        data-testid="regional-result-file-input"
        bind:this={resultFileInput}
        class="hidden-input"
        type="file"
        accept=".json.gz,application/gzip"
        onchange={loadResultFile}
      />
    </div>

    <RegionalErResultCharts charts={resultCharts} />

    {#if primaryTables.length === 0}
      <p class="empty">Esegui una simulazione regionale per vedere i risultati.</p>
    {:else}
      <div class="tables">
        {#each primaryTables as table (table.name)}
          <section class="table-block">
            <h3>{table.name}</h3>
            <div class="table-wrap">
              <table aria-label={table.name}>
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
            </div>
          </section>
        {/each}
      </div>
    {/if}

    {#if diagnosticTables.length > 0}
      <button class="link-button" type="button" onclick={() => (showDiagnostics = !showDiagnostics)}>
        {showDiagnostics ? 'Nascondi dettagli' : 'Mostra dettagli'}
      </button>
      {#if showDiagnostics}
        <div class="tables">
          {#each diagnosticTables as table (table.name)}
            <section class="table-block">
              <h3>{table.name}</h3>
              <div class="table-wrap">
                <table aria-label={table.name}>
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
              </div>
            </section>
          {/each}
        </div>
      {/if}
    {/if}
  </section>
</main>

<style>
  :global(body) {
    margin: 0;
    font-family:
      Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    color: #182733;
    background: #f4f7f8;
  }

  main {
    width: min(1440px, calc(100% - 2rem));
    margin: 0 auto;
    padding: 1rem 0 3rem;
    display: grid;
    gap: 1rem;
  }

  .toolbar,
  .panel,
  .message-list {
    background: #ffffff;
    border: 1px solid #dce5e9;
    border-radius: 8px;
    padding: 1rem;
  }

  .toolbar {
    display: flex;
    justify-content: space-between;
    gap: 1rem;
    align-items: end;
  }

  .back-link {
    color: #49687a;
    font-size: 0.85rem;
    text-decoration: none;
  }

  h1,
  h2,
  h3,
  p {
    margin: 0;
  }

  h1 {
    margin-top: 0.35rem;
    font-size: 1.8rem;
    letter-spacing: 0;
  }

  .toolbar p,
  .empty {
    color: #60727e;
    margin-top: 0.35rem;
  }

  .run-controls,
  .button-row,
  .section-heading,
  .form-grid,
  .coalition-row,
  .list-row,
  .local-editor,
  .local-row,
  .correspondence-row {
    display: flex;
    gap: 0.75rem;
    align-items: end;
    flex-wrap: wrap;
  }

  .section-heading {
    justify-content: space-between;
    margin-bottom: 1rem;
  }

  .section-heading.compact {
    margin: 1.25rem 0 0.75rem;
  }

  .form-grid {
    align-items: center;
  }

  label {
    display: grid;
    gap: 0.25rem;
    font-size: 0.8rem;
    color: #4b5f6c;
  }

  input,
  select,
  button {
    font: inherit;
  }

  input,
  select {
    min-height: 2.2rem;
    border: 1px solid #cfdce3;
    border-radius: 6px;
    padding: 0.35rem 0.5rem;
    background: #ffffff;
    color: #182733;
  }

  input[type='color'] {
    width: 2.5rem;
    padding: 0.15rem;
  }

  input[type='number'] {
    width: 7rem;
  }

  button {
    min-height: 2.2rem;
    border: 1px solid #c5d4dc;
    border-radius: 6px;
    padding: 0.4rem 0.7rem;
    display: inline-flex;
    align-items: center;
    gap: 0.4rem;
    color: #182733;
    background: #ffffff;
    cursor: pointer;
  }

  button:disabled {
    opacity: 0.55;
    cursor: not-allowed;
  }

  button.primary {
    background: #245d73;
    border-color: #245d73;
    color: #ffffff;
  }

  .link-button {
    margin-top: 1rem;
    border: 0;
    padding: 0;
    min-height: auto;
    color: #245d73;
    background: transparent;
  }

  .checkbox {
    display: flex;
    align-items: center;
    gap: 0.45rem;
  }

  .checkbox.small {
    font-size: 0.75rem;
  }

  .rows,
  .advanced,
  .tables,
  .local-groups,
  .correspondence-list {
    display: grid;
    gap: 0.65rem;
  }

  .list-row {
    display: grid;
    grid-template-columns: minmax(13rem, 1.2fr) minmax(9rem, 0.8fr) auto auto auto auto;
    align-items: center;
  }

  .coalition-row {
    display: grid;
    grid-template-columns: minmax(13rem, 1fr) auto auto;
    align-items: center;
    justify-content: start;
  }

  .message-list {
    display: grid;
    gap: 0.4rem;
  }

  .message-list.info {
    border-color: #b7d4df;
    background: #eef8fb;
  }

  .message-list.warning {
    border-color: #e4c785;
    background: #fff7df;
  }

  .hidden-input {
    display: none;
  }

  details {
    border-top: 1px solid #e3ebef;
    padding-top: 1rem;
  }

  summary {
    cursor: pointer;
    font-weight: 700;
  }

  .correspondence-list {
    margin-top: 0.75rem;
    max-height: 22rem;
    overflow: auto;
  }

  .correspondence-row {
    display: grid;
    grid-template-columns: minmax(16rem, 1.6fr) minmax(12rem, 1fr) 6rem;
    align-items: center;
  }

  .local-editor {
    margin-top: 0.75rem;
  }

  .location-results {
    display: grid;
    min-width: min(30rem, 100%);
    border: 1px solid #d6e1e6;
    border-radius: 6px;
    overflow: hidden;
  }

  .location-results button {
    justify-content: space-between;
    border: 0;
    border-radius: 0;
    border-bottom: 1px solid #e5edf1;
  }

  .location-results button:last-child {
    border-bottom: 0;
  }

  .location-results span:last-child {
    color: #667984;
    font-size: 0.78rem;
  }

  .local-group {
    border: 1px solid #dce5e9;
    border-radius: 8px;
    padding: 0.75rem;
    display: grid;
    gap: 0.5rem;
  }

  .local-row {
    display: grid;
    grid-template-columns: minmax(12rem, 1fr) 7rem auto;
    align-items: center;
  }

  .table-wrap {
    overflow: auto;
    border: 1px solid #dce5e9;
    border-radius: 8px;
  }

  table {
    width: 100%;
    border-collapse: collapse;
    font-size: 0.86rem;
  }

  th,
  td {
    padding: 0.55rem 0.65rem;
    border-bottom: 1px solid #e5edf1;
    text-align: left;
    white-space: nowrap;
  }

  th {
    background: #f1f6f8;
    font-weight: 700;
  }

  tr:last-child td {
    border-bottom: 0;
  }

  .elapsed {
    color: #637783;
    font-variant-numeric: tabular-nums;
  }

  @media (max-width: 900px) {
    .toolbar {
      display: grid;
    }

    .list-row,
    .coalition-row,
    .correspondence-row,
    .local-row {
      grid-template-columns: 1fr;
    }
  }
</style>
