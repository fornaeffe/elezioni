<script lang="ts">
  import { base } from '$app/paths';
  import { onMount, onDestroy } from 'svelte';
  import SimulationWorker from '../../workers/simulation.worker?worker';
  import { createSimulationResultExport, parseSimulationResultExport, type SimulationResultExport } from '../../core/result-export';
  import { isReformFile, jsonFileStream } from '../../core/json-stream';
  import { ResultServiceClient } from '../../core/result-service-client';
  import type { StoredResult } from '../../core/result-store';
  import type { ResultServiceReply } from '../../workers/result-service';
  import type { ResultTable, ScrutinyWarning, SimulationRequest, SimulationResult, SimulationWorkerMessage } from '../../core/types';
  import { requireSimulationCount } from '../../core/runtime-data';
  import { estimateReformWorkerCapacity, requireReformWorkerCount } from '../../core/reform-worker-capacity';
  import { requireReformScenario, serializeReformScenario, parseReformScenario } from './generation-scenario';
  import type { ReformGenerationScenario } from './generation-types';
  import { bundledReformScenario, bundledReformModels } from './application-defaults';
  import { buildReformSeatChart } from './result-presentation';

  let { imported = null, importedScenario = null, importedFile = null, onLegacyImport, onImportConsumed }: {
    imported?: SimulationResultExport | null;
    importedScenario?: ReformGenerationScenario | null;
    importedFile?: File | null;
    onLegacyImport: (payload: SimulationResultExport) => void;
    onImportConsumed: () => void;
  } = $props();
  function copyScenario(value: unknown) { return requireReformScenario($state.snapshot(value)); }
  const models = bundledReformModels();
  const storageKey = 'elezioni:politics-reform-2026:scenario:v1';
  const resultKey = 'elezioni:politics-reform-2026:results:v1';
  let scenario = $state<ReformGenerationScenario>(bundledReformScenario());
  let resultScenario = $state<ReformGenerationScenario | null>(null);
  let result = $state.raw<SimulationResult | null>(null);
  let table = $state.raw<ResultTable | undefined>();
  let chartTable = $state.raw<ResultTable | undefined>();
  let shownWarnings = $state.raw<ScrutinyWarning[]>([]);
  let savedResults = $state<{ id: string; label: string }[]>([]);
  let selectedSavedResult = $state('');
  let simulations = $state(10);
  let workerCapacity = $state(estimateReformWorkerCapacity({}));
  let computeWorkers = $state(1);
  let running = $state(false);
  let busyAction = $state('');
  let phase = $state('idle');
  let error = $state('');
  let ready = $state(false);
  let advanced = $state(false);
  let draft = $state('');
  let tableIndex = $state(0);
  let pageIndex = $state(0);
  let warningPage = $state(0);
  let selectedRun = $state(1);
  let selectedChamber = $state('camera');
  let referenceList = $state('');
  let commonShare = $state<number | undefined>();
  let cameraShare = $state<number | undefined>();
  let senateShare = $state<number | undefined>();
  let client: ResultServiceClient | undefined;
  let owner = '', currentRun = '', alive = true;
  let tableRequest = 0, chartRequest = 0, warningRequest = 0;
  const pageSize = 50;
  const isBusy = $derived(running || busyAction !== '');
  const descriptor = $derived(result?.storage?.tables[tableIndex]);
  const warningCount = $derived(result?.storage ? result.storage.warningCounts.error + result.storage.warningCounts.warning : result?.warnings.filter(w => w.severity !== 'info').length ?? 0);
  const pageCount = $derived(Math.max(1, Math.ceil((descriptor?.rows ?? table?.rows.length ?? 0) / pageSize)));
  const safePage = $derived(Math.min(pageIndex, pageCount - 1));
  const rows = $derived(table?.rows ?? []);
  const chart = $derived(buildReformSeatChart(chartTable, selectedChamber, selectedRun, resultScenario ?? scenario));
  const fingerprint = $derived(JSON.stringify(scenario));
  const stale = $derived(resultScenario !== null && fingerprint !== JSON.stringify(resultScenario));

  onMount(() => { void initialize(); });
  onDestroy(() => { alive = false; client?.close(); });
  $effect(() => { if (ready) localStorage.setItem(storageKey, fingerprint); });
  $effect(() => {
    const id = result?.storage?.id, index = tableIndex, page = safePage;
    if (id) void fetchTable(id, index, page);
    else table = result?.tables[index];
  });
  $effect(() => {
    const id = result?.storage?.id, simulation = selectedRun;
    const index = result?.storage?.tables.findIndex(t => t.name === 'Seat entitlement and election locations') ?? -1;
    if (id && index >= 0) void fetchChart(id, index, simulation);
    else chartTable = result?.tables.find(t => t.name === 'Seat entitlement and election locations');
  });
  $effect(() => {
    const id = result?.storage?.id, page = warningPage;
    if (id) void fetchWarnings(id, page);
    else shownWarnings = result?.warnings.filter(w => w.severity !== 'info').slice(page * 10, page * 10 + 10) ?? [];
  });
  function describe(e: unknown) { return e instanceof Error ? e.message : String(e); }
  function service() {
    if (!client) client = new ResultServiceClient(new SimulationWorker());
    return client;
  }
  function remember(id: string, label: string) {
    savedResults = [...savedResults.filter(r => r.id !== id), { id, label }];
    selectedSavedResult = id;
    sessionStorage.setItem(resultKey, JSON.stringify(savedResults));
  }
  async function initialize() {
    try {
      workerCapacity = estimateReformWorkerCapacity(navigator as Navigator & { deviceMemory?: number });
      const savedWorkers = localStorage.getItem(`${storageKey}:compute-workers`);
      const previousWorkers = savedWorkers === null ? workerCapacity.maximum : Number(savedWorkers);
      computeWorkers = Number.isSafeInteger(previousWorkers) && previousWorkers >= 1
        ? Math.min(previousWorkers, workerCapacity.maximum) : workerCapacity.maximum;
      const ownerKey = `${resultKey}:owner`;
      owner = sessionStorage.getItem(ownerKey) ?? crypto.randomUUID(); sessionStorage.setItem(ownerKey, owner);
      savedResults = JSON.parse(sessionStorage.getItem(resultKey) ?? '[]');
      if (importedFile) await importFile(importedFile);
      else if (imported) {
        const payload = createSimulationResultExport({ scenario: imported.scenario, result: imported.result, exportedAt: imported.exportedAt });
        await importFile(new File([JSON.stringify(payload)], 'import.json'));
      } else {
        const saved = localStorage.getItem(storageKey);
        if (importedScenario) scenario = copyScenario(importedScenario);
        else if (saved) scenario = parseReformScenario(saved);
        if (!importedScenario && savedResults.length) await restoreSaved(savedResults.at(-1)!.id);
      }
    } catch (e) { error = describe(e); }
    finally { if (alive) { onImportConsumed(); ready = true; } }
  }
  async function fetchTable(id: string, index: number, page: number) {
    const version = ++tableRequest;
    try {
      const loaded = await service().call<ResultTable>('page', { resultId: id, owner, table: index, offset: page * pageSize });
      if (alive && version === tableRequest) table = loaded;
    } catch (e) { if (alive && version === tableRequest) error = describe(e); }
  }
  async function fetchChart(id: string, index: number, simulation: number) {
    const version = ++chartRequest;
    if (!Number.isSafeInteger(simulation) || simulation < 1) { chartTable = undefined; return; }
    try {
      const loaded = await service().call<ResultTable>('page', { resultId: id, owner, table: index, offset: 0, limit: 1000, simulation });
      if (alive && version === chartRequest) chartTable = loaded;
    } catch (e) { if (alive && version === chartRequest) error = describe(e); }
  }
  async function fetchWarnings(id: string, page: number) {
    const version = ++warningRequest;
    try {
      const loaded = await service().call<ScrutinyWarning[]>('warnings', { resultId: id, owner, offset: page * 10 });
      if (alive && version === warningRequest) shownWarnings = loaded;
    } catch (e) { if (alive && version === warningRequest) error = describe(e); }
  }
  function attach(record: StoredResult, label: string) {
    if (!('kind' in record.scenario)) throw new Error('Expected an explicit reform scenario');
    scenario = copyScenario(record.scenario); resultScenario = copyScenario(record.scenario);
    result = record.result; simulations = record.result.benchmark.simulations;
    tableIndex = 0; pageIndex = 0; warningPage = 0; phase = label;
    if (record.result.storage?.state === 'interrupted') error = record.result.warnings.filter(w => w.code.startsWith('RESULT_')).map(w => w.message).join('\n') || 'Risultato parziale: il calcolo precedente e stato interrotto.';
    remember(record.id, `${record.result.benchmark.startedAt} · ${record.result.storage?.completedSimulations}/${simulations}`);
  }
  async function restoreSaved(id: string) {
    error = '';
    try { attach(await service().call<StoredResult>('manifest', { resultId: id, owner }), 'imported'); }
    catch (e) { error = describe(e); }
  }
  function download(blob: Blob, filename: string) {
    const url = URL.createObjectURL(blob), link = document.createElement('a');
    link.href = url; link.download = filename; link.click();
    setTimeout(() => URL.revokeObjectURL(url), 60000);
  }
  function exportScenario() {
    try { download(new Blob([serializeReformScenario($state.snapshot(scenario))], { type: 'application/json' }), 'r26-scenario.json'); }
    catch (e) { error = describe(e); }
  }
  async function exportResult(format: 'json' | 'csv') {
    if (!result?.storage) { error = 'Nessun risultato locale da esportare.'; return; }
    busyAction = 'Preparo il file dai risultati locali…'; error = '';
    try {
      const file = await service().call<File>('export', { resultId: result.storage.id, owner, format });
      download(file, format === 'json' ? 'r26-risultati.json.gz' : 'r26-risultati.csv');
    } catch (e) { error = describe(e); }
    finally { if (alive) busyAction = ''; }
  }
  async function importFile(file: File) {
    busyAction = 'Importo i risultati in blocchi…'; error = '';
    try {
      if (!await isReformFile(file)) {
        onLegacyImport(parseSimulationResultExport(await new Response(jsonFileStream(file)).text())); return;
      }
      // Standalone scenarios remain small input envelopes, not result archives.
      const header = await inspectStandalone(file);
      if (header) { scenario = header; return; }
      const id = crypto.randomUUID();
      const record = await service().call<StoredResult>('import', { resultId: id, owner, file });
      attach(record, 'imported');
    } finally { if (alive) busyAction = ''; }
  }
  async function inspectStandalone(file: File): Promise<ReformGenerationScenario | null> {
    const { JsonStreamReader } = await import('../../core/json-stream');
    const reader = new JsonStreamReader(jsonFileStream(file)); const stop = Symbol('header'); let standalone = false;
    try { await reader.object(async key => { if (key === 'kind') { standalone = await reader.value() === 'politics-reform-scenario'; throw stop; } if (key === 'scenario' || key === 'result') throw stop; await reader.skip(); }); }
    catch (e) { if (e !== stop) throw e; }
    finally { await reader.close(); }
    return standalone ? parseReformScenario(await new Response(jsonFileStream(file)).text()) : null;
  }
  async function loadFile(event: Event) {
    const input = event.currentTarget as HTMLInputElement, file = input.files?.[0]; if (!file) return;
    try { await importFile(file); } catch (e) { error = describe(e); } finally { input.value = ''; }
  }
  async function clearResult() {
    if (!result?.storage) return;
    const id = result.storage.id;
    try {
      await service().call('delete', { resultId: id, owner });
      savedResults = savedResults.filter(r => r.id !== id); sessionStorage.setItem(resultKey, JSON.stringify(savedResults));
      result = null; resultScenario = null; table = undefined; chartTable = undefined; selectedSavedResult = '';
    } catch (e) { error = describe(e); }
  }
  function applyReference() {
    try {
      if (!referenceList) throw new Error('Seleziona una lista');
      const share = (v: number | undefined) => v === undefined ? null : v / 100;
      const next = copyScenario(scenario);
      next.shares = next.shares.filter(r => r.scope.kind !== 'national' || r.listId !== referenceList);
      next.shares.push({ scope: { kind: 'national' }, listId: referenceList, common: share(commonShare), camera: share(cameraShare), senato: share(senateShare) });
      scenario = copyScenario(next); error = '';
    } catch (e) { error = describe(e); }
  }
  function applyDraft() { try { scenario = parseReformScenario(draft); advanced = false; error = ''; } catch (e) { error = describe(e); } }
  async function cancel() {
    try { if (currentRun) await service().call('cancel', { resultId: currentRun, owner }); }
    catch (e) { if (alive) error = describe(e); }
  }
  function run() {
    try {
      error = ''; requireSimulationCount(simulations); requireReformWorkerCount(computeWorkers, workerCapacity.maximum);
      localStorage.setItem(`${storageKey}:compute-workers`, String(computeWorkers));
      const captured = copyScenario(scenario);
      const worker = service().worker; currentRun = crypto.randomUUID();
      const id = currentRun; remember(id, `${new Date().toISOString()} · in corso`);
      running = true; phase = 'prepare';
      const request: SimulationRequest = { basePath: base, resultId: id, resultOwner: owner, kind: 'politiche', scenario: captured,
        scrutinyAlgorithmId: captured.algorithmId, electionDate: captured.electionDate, seed: captured.seed, dataVersion: captured.snapshotVersion, simulations, computeWorkers };
      worker.onmessage = (event: MessageEvent<SimulationWorkerMessage | ResultServiceReply>) => {
        const m = event.data; if (m.type === 'result-service') return;
        if (m.type === 'progress') { phase = `${m.phase} ${m.completed}/${m.total}`; return; }
        running = false; phase = m.status; currentRun = '';
        if (m.storage) {
          if (m.storage.state === 'interrupted') error = m.warnings.filter(w => w.code.startsWith('RESULT_')).map(w => w.message).join('\n');
          result = m; resultScenario = captured; tableIndex = 0; pageIndex = 0; warningPage = 0;
          remember(id, `${m.benchmark.startedAt} · ${m.storage.completedSimulations}/${m.benchmark.simulations}`);
        } else if (!result) { result = m; resultScenario = captured; }
        else error = m.warnings.map(w => `${w.code}: ${w.message}`).join('\n');
      };
      worker.onerror = event => { error = event.message; running = false; phase = 'error'; };
      worker.postMessage(request);
    } catch (e) { error = describe(e); running = false; }
  }
</script>

<main class="reform-workspace">
  <h1>Politiche — Riforma 2026</h1>
  <p>Scrutinio iniziale dei seggi nazionali: Camera 392, Senato 196; estero escluso. Il flusso precedente rimane il valore predefinito e i suoi file conservano la legge precedente.</p>
  <section aria-label="Scenario riforma">
    <h2>Scenario riforma</h2>
    <div class="controls">
      <label>Simulazioni<input type="number" min="1" step="1" bind:value={simulations} disabled={isBusy} /></label>
      <label>Processi di calcolo<select aria-label="Processi di calcolo" bind:value={computeWorkers} disabled={isBusy || !ready} aria-describedby="reform-worker-memory">
        {#each Array.from({ length: workerCapacity.maximum }, (_, index) => index + 1) as count (count)}<option value={count}>{count}</option>{/each}
      </select></label>
      <label>Seed<input bind:value={scenario.seed} disabled={isBusy} /></label>
      <label>Data proiezione<input type="date" bind:value={scenario.projectionDate} disabled={isBusy} /></label>
      <label>Data elezione<input type="date" bind:value={scenario.electionDate} disabled={isBusy} /></label>
      <label>Profilo preferenze<select bind:value={scenario.preferenceProfileId} disabled={isBusy}>{#each models.preferences.profiles as p (p.id)}<option value={p.id}>{p.id}</option>{/each}</select></label>
      <label>Profilo dipendenza Camera-Senato<select bind:value={scenario.chamberProfileId} disabled={isBusy}>{#each models.chambers.profiles as p (p.id)}<option value={p.id}>{p.id}</option>{/each}</select></label>
      <button onclick={run} disabled={isBusy}>Esegui riforma</button>
      {#if running}<button onclick={cancel}>Annulla</button>{/if}
    </div>
    <p id="reform-worker-memory">Massimo stimato: {workerCapacity.maximum}; circa {(workerCapacity.workerMemoryMiB / 1024).toLocaleString('it-IT')} GiB per processo.
      {#if workerCapacity.reportedMemoryGiB === null}Il browser non comunica la memoria: limite prudenziale.{:else}Memoria del dispositivo indicata dal browser: {workerCapacity.reportedMemoryGiB} GiB.{/if}
      La memoria libera effettiva può essere inferiore. Ridurre i processi lascia più risorse alle altre attività.</p>
    <p>I profili sono ipotesi di sensibilità. Le probabilità su candidati, copertura e recuperi sono condizionate alle preferenze sintetiche. Ripetere con gli stessi seed e altri profili per esplorare la sensibilità del premio congiunto.</p>
    <p>I totali attribuiti non specificano la composizione dei voti di sola lista del premio. La quota comune ancora entrambi i rami; le quote esplicite per ramo hanno precedenza e la modalità media conserva incertezza.</p>
    <div class="controls">
      <label>Lista riferimento nazionale<select bind:value={referenceList}><option value="">Seleziona</option>{#each scenario.lists.camera as l (l.id)}<option value={l.id}>{l.name}</option>{/each}</select></label>
      <label>Quota comune %<input type="number" min="0" max="100" bind:value={commonShare} /></label>
      <label>Quota Camera %<input type="number" min="0" max="100" bind:value={cameraShare} /></label>
      <label>Quota Senato %<input type="number" min="0" max="100" bind:value={senateShare} /></label>
      <button onclick={applyReference} disabled={isBusy}>Applica riferimento</button>
    </div>
    <p>Lasciare vuote le quote non specificate. Il file scenario contiene liste, coalizioni, candidature, riferimenti geografici, dati forniti e tutte le ipotesi riproducibili.</p>
    <div class="controls">
      <label>Carica scenario o risultati<input type="file" accept=".json,.gz" onchange={loadFile} disabled={isBusy} data-testid="reform-file-input" /></label>
      <button onclick={exportScenario}>Scarica scenario</button>
      <button onclick={() => { draft = JSON.stringify(scenario, null, 2); advanced = !advanced; }}>Modifica scenario completo</button>
    </div>
    {#if advanced}
      <label>Scenario completo JSON<textarea bind:value={draft} rows="18"></textarea></label>
      <button onclick={applyDraft} disabled={isBusy}>Applica scenario completo</button>
    {/if}
  </section>
  <p role="status" data-testid="reform-phase">{phase}</p>
  {#if busyAction}<p role="status">{busyAction}</p>{/if}
  {#if savedResults.length}<label>Risultati locali<select bind:value={selectedSavedResult} onchange={() => restoreSaved(selectedSavedResult)} disabled={isBusy}>{#each savedResults as saved (saved.id)}<option value={saved.id}>{saved.label}</option>{/each}</select></label>{/if}
  {#if error}<p role="alert">{error}</p>{/if}
  {#if result}
    <section aria-label="Risultati riforma">
      <h2>Risultati riforma</h2>
      {#if result.storage}<p data-testid="stored-result-status">{result.storage.completedSimulations}/{result.benchmark.simulations} simulazioni conservate · stato {result.storage.state} · {(result.storage.compressedBytes / 1024 / 1024).toFixed(1)} MiB in blocchi compressi</p>{/if}
      <p>{result.benchmark.scrutinyAlgorithmId} · {result.benchmark.dataVersion} · {result.benchmark.simulations} simulazioni richieste · {result.benchmark.elapsedMs.toFixed(0)} ms</p>
      {#if stale}<p role="alert">Scenario modificato. I risultati e gli export si riferiscono allo scenario eseguito.</p><button onclick={() => { if (resultScenario) scenario = copyScenario(resultScenario); }}>Ripristina scenario dei risultati</button>{/if}
      <button onclick={() => exportResult('json')} disabled={isBusy}>Scarica risultati JSON.gz</button>
      <button onclick={() => exportResult('csv')} disabled={isBusy}>Scarica risultati CSV</button>
      <button onclick={clearResult} disabled={isBusy}>Elimina risultato locale</button>
      <div aria-label="Note riforma">{#each result.warnings.filter(w => w.severity === 'info') as w (w.code + w.message)}<p><strong>{w.code}</strong>: {w.message}</p>{/each}</div>
      <div aria-label="Avvisi riforma">
        <p>{warningCount} avvisi/errori. Visualizzati al massimo 10 alla volta.</p>
        {#each shownWarnings as w, i (warningPage * 10 + i)}<p><strong>{w.severity}: {w.code}</strong>: {w.message}</p>{/each}
        {#if warningCount > 10}
          <button disabled={warningPage === 0} onclick={() => { warningPage--; }}>Avvisi precedenti</button>
          <button disabled={(warningPage + 1) * 10 >= warningCount} onclick={() => { warningPage++; }}>Avvisi successivi</button>
        {/if}
      </div>
      <div class="controls">
        <label>Simulazione grafico<input type="number" min="1" max={result.benchmark.simulations} step="1" bind:value={selectedRun} /></label>
        <label>Ramo grafico<select bind:value={selectedChamber}><option value="camera">Camera</option><option value="senato">Senato</option></select></label>
      </div>
      <figure aria-label="Seggi per categoria e titolare">
        <figcaption>Seggi per categoria e titolare — simulazione {selectedRun}, {selectedChamber}</figcaption>
        {#each chart as bar (bar.id)}<div class="bar-row"><span>{bar.label}</span><meter min="0" max={selectedChamber === 'camera' ? 392 : 196} value={bar.seats}>{bar.seats}</meter><span>{bar.seats}</span></div>{:else}<p>Seggi non disponibili per questa simulazione. Consultare gli stadi e i blocchi; nessun risultato è ricostruito o riestratto.</p>{/each}
      </figure>
      <label>Tabella riforma<select bind:value={tableIndex} onchange={() => { pageIndex = 0; }}>
        {#each result.tables as t, i (t.name)}<option value={i}>{t.name} ({result.storage?.tables[i]?.rows ?? t.rows.length})</option>{/each}
      </select></label>
      <div class="controls">
        <button disabled={safePage === 0} onclick={() => { pageIndex = safePage - 1; }}>Pagina precedente</button>
        <span>Pagina {safePage + 1} di {pageCount} · massimo {pageSize} righe visualizzate</span>
        <button disabled={safePage + 1 >= pageCount} onclick={() => { pageIndex = safePage + 1; }}>Pagina successiva</button>
      </div>
      {#if table}
        <div class="table-scroll"><table><caption>{table.name}</caption><thead><tr>{#each table.columns as c (c)}<th>{c}</th>{/each}</tr></thead>
          <tbody>{#each rows as row, i (safePage * pageSize + i)}<tr>{#each table.columns as c (c)}<td>{row[c] === null ? 'Non specificato' : row[c]}</td>{/each}</tr>{/each}</tbody>
        </table></div>
      {/if}
    </section>
  {/if}
</main>

<style>
  .reform-workspace { max-width: 1500px; margin: auto; padding: 1.5rem; color: #15232d; font-family: system-ui, sans-serif; }
  section { background: white; border: 1px solid #d7e0e5; border-radius: 8px; padding: 1.25rem; margin: 1rem 0; }
  .controls { display: flex; flex-wrap: wrap; gap: 1rem; align-items: end; margin: 1rem 0; }
  label { display: grid; gap: .4rem; }
  input, select, button, textarea { font: inherit; padding: .5rem; border: 1px solid #a8bac6; border-radius: 4px; }
  button { cursor: pointer; background: #edf3f7; }
  textarea { width: 100%; box-sizing: border-box; }
  .table-scroll { overflow: auto; }
  table { border-collapse: collapse; width: 100%; }
  th, td { text-align: left; border-bottom: 1px solid #d7e0e5; padding: .5rem; max-width: 40rem; overflow-wrap: anywhere; }
  caption { text-align: left; font-weight: bold; padding: 1rem 0; }
  .bar-row { display: grid; grid-template-columns: minmax(10rem, 1fr) 2fr 3rem; gap: .5rem; margin: .35rem 0; }
  meter { width: 100%; }
  [role='alert'] { color: #9f2519; }
</style>
