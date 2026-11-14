<script lang="ts">
  import { onMount } from 'svelte';
  import { base } from '$app/paths';
  import CommonPoliticsEditor from './editor/CommonPoliticsEditor.svelte';
  import PoliticsSessionResults from './PoliticsSessionResults.svelte';
  import { createDefaultCommonPoliticsScenario } from './scenario/defaults';
  import { PoliticsWorkspaceController } from './workspace-controller';
  import { PoliticsClient } from './execution/client';
  import { loadEditorAssets } from './editor/assets';
  import { parsePoliticsScenario } from './scenario/validation';
  import { parsePoliticsCheckpoint } from './files/checkpoint';
  import { inspectPoliticsFile } from './files/metadata';
  import { decodeScenario } from './scenario/serialization';
  import { JsonStreamReader, jsonFileStream } from '../core/json-stream';
  import { takeWorkspaceImport } from '../workspaces/handoff';
  import { fileWorkspace } from '../workspaces/import-routing';
  import { navigateWorkspaceImport } from '../workspaces/navigation';
  import { workerMemoryWarning, workerHeapWarning } from '../core/reform-worker-capacity';
  import { OLD_POLITICS_FORMAT } from './files/validation';
  let controller: PoliticsWorkspaceController;
  let client = $state.raw<PoliticsClient>()!;
  let render = $state(0), ready = $state(false), editorEpoch = $state(0), fileError = $state(''), exporting = $state(false);
  let lawFilter = $state(''), modelFilter = $state('');
  const view = $derived.by(() => { render; return ready ? { scenario: controller.scenario, sessions: controller.sessions, selected: controller.selected,
    viewed: controller.viewed, busy: controller.busy, preparing: controller.preparing, status: controller.status, progress: controller.progress,
    error: controller.error, storageError: controller.storageError, stale: controller.stale, recommendation: controller.recommend1000,
    preparation: controller.preparation } : null; });
  const execution = $derived(view ? { busy: view.busy, preparing: view.preparing, status: view.status, progress: view.progress,
    onRun: () => { void controller.run(); }, onCancel: () => { void controller.cancel(); }, onFit: () => controller.prepare(true),
    onImportCheckpoint: (file: File) => { void importCheckpoint(file); },
    onExportCheckpoint: view.preparation && !view.preparing ? () => { void exportCheckpoint(); } : undefined } : undefined);
  function download(file: Blob, name: string) { const url = URL.createObjectURL(file), a = document.createElement('a'); a.href = url; a.download = name; a.click(); setTimeout(() => URL.revokeObjectURL(url), 1000); }
  async function exportCheckpoint() {
    try { const checkpoint = await client.checkpoint(); if (!checkpoint) throw new Error('Checkpoint non applicabile a M01'); download(new Blob([JSON.stringify(checkpoint)]), 'politiche-checkpoint.json'); }
    catch (e) { fileError = String(e); }
  }
  async function importCheckpoint(file: File) {
    const reader = new JsonStreamReader(jsonFileStream(file));
    try { const checkpoint = parsePoliticsCheckpoint(await reader.value()); await reader.finish(); controller.prepare(false, checkpoint); }
    catch (e) { fileError = `Checkpoint non caricato: ${String(e)}`; } finally { await reader.close(); }
  }
  async function exportResult(format: 'json' | 'csv') {
    if (!view?.viewed) return; exporting = true;
    try { const file = await client.results.call<File>('export', { resultId: view.viewed.id, owner: controller.owner, format }); download(file, `politiche-risultati.${format === 'json' ? 'json.gz' : 'csv'}`); }
    catch (e) { fileError = String(e); } finally { exporting = false; }
  }
  async function loadViewedScenario() { if (await controller.loadViewedScenario()) editorEpoch++; }
  async function stageExternalFile(file: File) {
    const target = await fileWorkspace(file);
    if (target.kind === 'regionali-er') return () => { void navigateWorkspaceImport({ file }).catch(e => { fileError = String(e); }); };
    const metadata = await inspectPoliticsFile(file);
    return metadata.kind === 'politics-scenario' ? null : () => { void controller.importResult(file); };
  }
  onMount(() => {
    let alive = true;
    const validation = new AbortController();
    client = new PoliticsClient(new Worker(new URL('../workers/politics-workspace.worker.ts', import.meta.url), { type: 'module' }), base);
    controller = new PoliticsWorkspaceController(createDefaultCommonPoliticsScenario(), { runtime: client, storage: localStorage,
      changed: () => { render++; }, manifest: (id, owner) => client.results.call('manifest', { resultId: id, owner }),
      importResult: (file, id, owner) => client.results.call('import', { file, resultId: id, owner, commonPolitics: true }),
      validateScenario: async s => { const assets = await loadEditorAssets(s, validation.signal); parsePoliticsScenario(s, assets.context); } });
    client.onPreparationProgress = (detail, completed, total) => controller.preparationProgress(detail, completed, total);
    void controller.initialize().then(async () => {
      if (!alive) return; ready = true;
      const input = takeWorkspaceImport('politics-legacy') ?? takeWorkspaceImport('politics-reform');
      if (!input) return;
      if (input.payload) throw new Error(OLD_POLITICS_FORMAT);
      const draftAtImport = controller.scenario;
      if (input.file) {
        const metadata = await inspectPoliticsFile(input.file);
        if (metadata.kind !== 'politics-scenario') { await controller.importResult(input.file); return; }
        const scenario = decodeScenario(new Uint8Array(await input.file.arrayBuffer()));
        const assets = await loadEditorAssets(scenario, validation.signal); parsePoliticsScenario(scenario, assets.context);
        if (alive && controller.scenario === draftAtImport && !controller.busy) { controller.change(scenario); editorEpoch++; }
      } else if (input.scenario) {
        const scenario = parsePoliticsScenario(input.scenario), assets = await loadEditorAssets(scenario, validation.signal);
        parsePoliticsScenario(scenario, assets.context);
        if (alive && controller.scenario === draftAtImport && !controller.busy) { controller.change(scenario); editorEpoch++; }
      }
    }).catch(e => { if (alive) fileError = String(e); });
    return () => { alive = false; validation.abort(); controller.dispose(); };
  });
</script>
<svelte:head><title>Elezioni politiche</title></svelte:head>
{#if view}
  {#if view.error}<p class="workspace-error" role="alert">{view.error}</p>{/if}
  {#if view.storageError}<p class="workspace-error" role="alert">{view.storageError}</p>{/if}
  {#if fileError}<p class="workspace-error" role="alert">{fileError}</p>{/if}
  {#key editorEpoch}<CommonPoliticsEditor scenario={view.scenario} onchange={s => { controller.change(s); }} {execution} {stageExternalFile} />{/key}
  {#if view.preparation}<p class="worker-plan">Worker richiesti: {view.scenario.execution.workers.kind === 'auto' ? 'automatici' : view.scenario.execution.workers.count} · consigliati {view.preparation.capacity.recommended} · massimo CPU {view.preparation.capacity.maximum}. Stima per worker: {Math.ceil(view.preparation.capacity.perWorkerBytes / 1048576)} MiB.</p>{/if}
  {#if view.preparation && view.scenario.execution.workers.kind === 'fixed' && (workerMemoryWarning(view.scenario.execution.workers.count, view.preparation.capacity) || workerHeapWarning(view.scenario.execution.workers.count, view.preparation.capacity))}
    <p class="workspace-error" role="alert">I worker richiesti superano la stima prudenziale di memoria del browser. Riduci il numero di worker prima di avviare.</p>
  {/if}
  <section class="sessions" aria-label="Sessioni dei risultati">
    <h2>Risultati delle elezioni politiche</h2>
    <label>Importa risultati<input type="file" accept=".json,.gz" onchange={e => { const file = e.currentTarget.files?.[0]; if (file) void controller.importResult(file); e.currentTarget.value = ''; }} /></label>
    <div class="controls"><label>Filtro legge<select bind:value={lawFilter}><option value="">Tutte</option><option value="politiche-r-parity-v1">Rosatellum</option><option value="politiche-reform-2026-v1">Riforma 2026</option></select></label>
      <label>Filtro modello<select bind:value={modelFilter}><option value="">Tutti</option><option value="M01">M01</option><option value="M02">M02</option></select></label></div>
    <label>Sessione dei risultati<select value={view.selected} onchange={e => { void controller.selectSession(e.currentTarget.value); }}>
      <option value="" disabled>Seleziona una sessione</option>
      {#each view.sessions.filter(s => s.id === view.selected || (!lawFilter || s.law === lawFilter) && (!modelFilter || s.model.startsWith(modelFilter))) as s (s.id)}
        <option value={s.id}>{s.law === 'politiche-r-parity-v1' ? 'Rosatellum' : 'Riforma 2026'} · {s.model} · {s.createdAt} · {s.renewal} · {s.completed}/{s.requested}</option>
      {/each}
    </select></label>
    {#if view.viewed?.politics}
      {@const record = view.viewed}
      <p>Scenario eseguito: {record.politics!.run.executedScenario.name} · {record.politics!.run.execution.generator.id}@{record.politics!.run.execution.generator.version} · {record.result.completedSimulations}/{record.result.benchmark.simulations} simulazioni conservate · worker effettivi {record.politics!.run.execution.actualWorkers}.</p>
      {#if view.stale}<p role="status">Scenario modificato. Risultati ed export conservano lo scenario eseguito.</p>{/if}
      {#if view.recommendation}<p role="status">Le 10 simulazioni sono riuscite. Per risultati significativi consigliamo 1.000 simulazioni: modifica il numero e avvia una nuova esecuzione.</p>{/if}
      <div class="controls"><button disabled={view.busy} onclick={loadViewedScenario}>Carica lo scenario di questa sessione</button>
        <button disabled={exporting || view.busy} onclick={() => exportResult('json')}>Scarica archivio risultati JSON.gz</button>
        <button disabled={exporting || view.busy} onclick={() => exportResult('csv')}>Scarica risultati CSV</button></div>
      {#key `${record.id}:${record.result.completedSimulations}:${record.result.storage?.state}`}<PoliticsSessionResults {record} client={client.results} />{/key}
    {/if}
  </section>
{/if}
<style>
  .sessions, .worker-plan, .workspace-error { max-width: 1460px; margin: 1rem auto; padding: 1rem; font-family: system-ui, sans-serif; }
  .sessions { border-top: 1px solid #cbd5e1; } label { display: grid; gap: .4rem; margin: .75rem 0; } .controls { display: flex; flex-wrap: wrap; gap: .75rem; }
  input, select, button { max-width: 100%; box-sizing: border-box; padding: .5rem; font: inherit; } .workspace-error { color: #9f2519; overflow-wrap: anywhere; }
</style>
