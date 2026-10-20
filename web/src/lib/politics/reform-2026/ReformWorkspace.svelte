<script lang="ts">
  import { base } from '$app/paths';
  import { onMount, onDestroy } from 'svelte';
  import SimulationWorker from '../../workers/simulation.worker?worker';
  import { createSimulationResultExport, parseSimulationResultExport, resultTablesToCsv, type SimulationResultExport } from '../../core/result-export';
  import type { SimulationRequest, SimulationResult, SimulationWorkerMessage } from '../../core/types';
  import { requireSimulationCount } from '../../core/runtime-data';
  import { requireReformScenario, serializeReformScenario, parseReformScenario } from './generation-scenario';
  import type { ReformGenerationScenario } from './generation-types';
  import { bundledReformScenario, bundledReformModels } from './application-defaults';
  import { buildReformSeatChart } from './result-presentation';

  let { imported = null, importedScenario = null, onLegacyImport, onImportConsumed }: {
    imported?: SimulationResultExport | null;
    importedScenario?: ReformGenerationScenario | null;
    onLegacyImport: (payload: SimulationResultExport) => void;
    onImportConsumed: () => void;
  } = $props();
  function copyScenario(value: unknown) { return requireReformScenario($state.snapshot(value)); }
  const models = bundledReformModels();
  const storageKey = 'elezioni:politics-reform-2026:scenario:v1';
  let scenario = $state<ReformGenerationScenario>(bundledReformScenario());
  let resultScenario = $state<ReformGenerationScenario | null>(null);
  let result = $state.raw<SimulationResult | null>(null);
  let simulations = $state(10);
  let running = $state(false);
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
  let worker: Worker | null = null;
  const pageSize = 50;
  const warnings = $derived((result?.warnings ?? []).filter(w => w.severity !== 'info').toSorted((a, b) => Number(b.severity === 'error') - Number(a.severity === 'error')));
  const shownWarnings = $derived(warnings.slice(warningPage * 10, warningPage * 10 + 10));
  const table = $derived(result?.tables[tableIndex]);
  const pageCount = $derived(Math.max(1, Math.ceil((table?.rows.length ?? 0) / pageSize)));
  const safePage = $derived(Math.min(pageIndex, pageCount - 1));
  const rows = $derived(table?.rows.slice(safePage * pageSize, (safePage + 1) * pageSize) ?? []);
  const chart = $derived(buildReformSeatChart(result?.tables.find(t => t.name === 'Seat entitlement and election locations'), selectedChamber, selectedRun, resultScenario ?? scenario));
  // Drafts may temporarily contain empty/invalid form values. Validate only at
  // action boundaries, so editing a seed/date cannot throw during rendering.
  const fingerprint = $derived(JSON.stringify(scenario));
  const stale = $derived(resultScenario !== null && fingerprint !== JSON.stringify(resultScenario));

  onMount(() => {
    try {
      if (imported) applyImport(imported);
      else if (importedScenario) scenario = copyScenario(importedScenario);
      else {
        const saved = localStorage.getItem(storageKey);
        if (saved) scenario = parseReformScenario(saved);
      }
    } catch (e) { error = describe(e); }
    onImportConsumed();
    ready = true;
  });
  onDestroy(() => worker?.terminate());
  $effect(() => { if (ready) localStorage.setItem(storageKey, fingerprint); });

  function describe(e: unknown) { return e instanceof Error ? e.message : String(e); }
  function download(content: BlobPart, filename: string, type: string) {
    const url = URL.createObjectURL(new Blob([content], { type }));
    const a = document.createElement('a'); a.href = url; a.download = filename; a.click(); URL.revokeObjectURL(url);
  }
  function exportScenario() {
    try { download(serializeReformScenario($state.snapshot(scenario)), 'r26-scenario.json', 'application/json'); }
    catch (e) { error = describe(e); }
  }
  async function exportResult() {
    if (!result || !resultScenario) return;
    try {
      const payload = createSimulationResultExport({ scenario: resultScenario, result, exportedAt: new Date().toISOString() });
      const stream = new Blob([JSON.stringify(payload)]).stream().pipeThrough(new CompressionStream('gzip'));
      download(await new Response(stream).arrayBuffer(), 'r26-risultati.json.gz', 'application/gzip');
    } catch (e) { error = describe(e); }
  }
  function applyImport(payload: SimulationResultExport) {
    if (!('kind' in payload.scenario)) { onLegacyImport(payload); return; }
    scenario = copyScenario(payload.scenario);
    resultScenario = copyScenario(payload.scenario);
    result = payload.result; simulations = payload.result.benchmark.simulations;
    phase = 'imported'; tableIndex = 0; pageIndex = 0; warningPage = 0;
  }
  async function loadFile(event: Event) {
    const input = event.currentTarget as HTMLInputElement, file = input.files?.[0];
    if (!file) return;
    try {
      error = '';
      const text = file.name.endsWith('.gz') ? await new Response(file.stream().pipeThrough(new DecompressionStream('gzip'))).text() : await file.text();
      const value = JSON.parse(text);
      if ('result' in value) applyImport(parseSimulationResultExport(text));
      else if (value.kind === 'politics-reform-scenario') { scenario = parseReformScenario(text); result = null; resultScenario = null; }
      else throw new Error('Scenario precedente: caricarlo nel flusso Legge precedente. La conversione alla riforma richiede una mappatura esplicita.');
    } catch (e) { error = describe(e); }
    input.value = '';
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
  function applyDraft() {
    try { scenario = parseReformScenario(draft); advanced = false; error = ''; } catch (e) { error = describe(e); }
  }
  function run() {
    try {
      error = ''; requireSimulationCount(simulations);
      const captured = copyScenario(scenario);
      worker?.terminate(); worker = new SimulationWorker(); running = true; phase = 'prepare';
      const request: SimulationRequest = { basePath: base, kind: 'politiche', scenario: captured,
        scrutinyAlgorithmId: captured.algorithmId, electionDate: captured.electionDate, seed: captured.seed,
        dataVersion: captured.snapshotVersion, simulations };
      worker.onmessage = (event: MessageEvent<SimulationWorkerMessage>) => {
        const m = event.data;
        if (m.type === 'progress') { phase = `${m.phase} ${m.completed}/${m.total}`; return; }
        result = m; resultScenario = captured; phase = m.status; running = false; tableIndex = 0; pageIndex = 0; warningPage = 0;
        worker?.terminate(); worker = null;
      };
      worker.onerror = e => { error = e.message; running = false; worker?.terminate(); worker = null; };
      worker.postMessage(request);
    } catch (e) { error = describe(e); running = false; worker?.terminate(); worker = null; }
  }
</script>

<main class="reform-workspace">
  <h1>Politiche — Riforma 2026</h1>
  <p>Scrutinio iniziale dei seggi nazionali: Camera 392, Senato 196; estero escluso. Il flusso precedente rimane il valore predefinito e i suoi file conservano la legge precedente.</p>
  <section aria-label="Scenario riforma">
    <h2>Scenario riforma</h2>
    <div class="controls">
      <label>Simulazioni<input type="number" min="1" step="1" bind:value={simulations} disabled={running} /></label>
      <label>Seed<input bind:value={scenario.seed} disabled={running} /></label>
      <label>Data proiezione<input type="date" bind:value={scenario.projectionDate} disabled={running} /></label>
      <label>Data elezione<input type="date" bind:value={scenario.electionDate} disabled={running} /></label>
      <label>Profilo preferenze<select bind:value={scenario.preferenceProfileId} disabled={running}>{#each models.preferences.profiles as p (p.id)}<option value={p.id}>{p.id}</option>{/each}</select></label>
      <label>Profilo dipendenza Camera-Senato<select bind:value={scenario.chamberProfileId} disabled={running}>{#each models.chambers.profiles as p (p.id)}<option value={p.id}>{p.id}</option>{/each}</select></label>
      <button onclick={run} disabled={running}>Esegui riforma</button>
      {#if running}<button onclick={() => { worker?.terminate(); worker = null; running = false; phase = 'cancelled'; }}>Annulla</button>{/if}
    </div>
    <p>I profili sono ipotesi di sensibilità. Le probabilità su candidati, copertura e recuperi sono condizionate alle preferenze sintetiche. Ripetere con gli stessi seed e altri profili per esplorare la sensibilità del premio congiunto.</p>
    <p>I totali attribuiti non specificano la composizione dei voti di sola lista del premio. La quota comune ancora entrambi i rami; le quote esplicite per ramo hanno precedenza e la modalità media conserva incertezza.</p>
    <div class="controls">
      <label>Lista riferimento nazionale<select bind:value={referenceList}><option value="">Seleziona</option>{#each scenario.lists.camera as l (l.id)}<option value={l.id}>{l.name}</option>{/each}</select></label>
      <label>Quota comune %<input type="number" min="0" max="100" bind:value={commonShare} /></label>
      <label>Quota Camera %<input type="number" min="0" max="100" bind:value={cameraShare} /></label>
      <label>Quota Senato %<input type="number" min="0" max="100" bind:value={senateShare} /></label>
      <button onclick={applyReference} disabled={running}>Applica riferimento</button>
    </div>
    <p>Lasciare vuote le quote non specificate. Il file scenario contiene liste, coalizioni, candidature, riferimenti geografici, dati forniti e tutte le ipotesi riproducibili.</p>
    <div class="controls">
      <label>Carica scenario o risultati<input type="file" accept=".json,.gz" onchange={loadFile} disabled={running} data-testid="reform-file-input" /></label>
      <button onclick={exportScenario}>Scarica scenario</button>
      <button onclick={() => { draft = JSON.stringify(scenario, null, 2); advanced = !advanced; }}>Modifica scenario completo</button>
    </div>
    {#if advanced}
      <label>Scenario completo JSON<textarea bind:value={draft} rows="18"></textarea></label>
      <button onclick={applyDraft} disabled={running}>Applica scenario completo</button>
    {/if}
  </section>
  <p role="status" data-testid="reform-phase">{phase}</p>
  {#if error}<p role="alert">{error}</p>{/if}
  {#if result}
    <section aria-label="Risultati riforma">
      <h2>Risultati riforma</h2>
      <p>{result.benchmark.scrutinyAlgorithmId} · {result.benchmark.dataVersion} · {result.benchmark.simulations} simulazioni richieste · {result.benchmark.elapsedMs.toFixed(0)} ms</p>
      {#if stale}<p role="alert">Scenario modificato. I risultati e gli export si riferiscono allo scenario eseguito.</p><button onclick={() => { if (resultScenario) scenario = copyScenario(resultScenario); }}>Ripristina scenario dei risultati</button>{/if}
      <button onclick={exportResult}>Scarica risultati JSON.gz</button>
      <button onclick={() => { if (result) download(resultTablesToCsv(result.tables), 'r26-risultati.csv', 'text/csv'); }}>Scarica risultati CSV</button>
      <div aria-label="Note riforma">{#each result.warnings.filter(w => w.severity === 'info') as w (w.code + w.message)}<p><strong>{w.code}</strong>: {w.message}</p>{/each}</div>
      <div aria-label="Avvisi riforma">
        <p>{warnings.length} avvisi/errori. Visualizzati al massimo 10 alla volta.</p>
        {#each shownWarnings as w (w.code + w.message)}<p><strong>{w.severity}: {w.code}</strong>: {w.message}</p>{/each}
        {#if warnings.length > 10}
          <button disabled={warningPage === 0} onclick={() => { warningPage--; }}>Avvisi precedenti</button>
          <button disabled={(warningPage + 1) * 10 >= warnings.length} onclick={() => { warningPage++; }}>Avvisi successivi</button>
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
        {#each result.tables as t, i (t.name)}<option value={i}>{t.name} ({t.rows.length})</option>{/each}
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
