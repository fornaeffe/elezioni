<script lang="ts">
  import Notice from '$lib/ui/Notice.svelte';
  import { onMount, tick } from 'svelte';
  import { useStatus } from '$lib/app-status.svelte';
  const appStatus = useStatus();
  import { resolve } from '$app/paths';
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
  import { archiveProgressText, type ArchiveProgressListener } from '../core/archive-progress';
  let controller: PoliticsWorkspaceController;
  let client = $state.raw<PoliticsClient>()!;
  let render = $state(0), ready = $state(false), editorEpoch = $state(0), fileError = $state(''), exporting = $state(false);
  let lawFilter = $state(''), modelFilter = $state('');
  const view = $derived.by(() => { render; return ready ? { scenario: controller.scenario, sessions: controller.sessions, selected: controller.selected,
    viewed: controller.viewed, busy: controller.busy, preparing: controller.preparing, status: controller.status, progress: controller.progress,
    error: controller.error, storageError: controller.storageError, stale: controller.stale, recommendation: controller.recommend1000,
    preparation: controller.preparation } : null; });
  const execution = $derived(view ? { busy: view.busy, preparing: view.preparing, status: view.status, progress: view.progress,
    workerInfo: view.preparation ? { requested: view.scenario.execution.workers.kind === 'auto' ? 'automatici' : String(view.scenario.execution.workers.count),
      recommended: view.preparation.capacity.recommended, maximum: view.preparation.capacity.maximum,
      memoryMiB: Math.ceil(view.preparation.capacity.perWorkerBytes / 1048576),
      warning: view.scenario.execution.workers.kind === 'fixed' && (workerMemoryWarning(view.scenario.execution.workers.count, view.preparation.capacity) || workerHeapWarning(view.scenario.execution.workers.count, view.preparation.capacity)) } : undefined,
    onRun: () => { void controller.run(); }, onCancel: () => { void controller.cancel(); }, onFit: () => controller.prepare(true),
    onImportCheckpoint: (file: File) => { void importCheckpoint(file); },
    onExportCheckpoint: view.preparation && !view.preparing ? () => { void exportCheckpoint(); } : undefined } : undefined);
  $effect(() => { const v = view; appStatus.set('politics-execution', !v ? { state: 'working', label: 'Inizializzazione' } : v.busy || v.preparing ? { state: 'working', label: v.status, detail: v.progress, cancel: () => void controller.cancel() } : { state: 'ready', label: v.status, detail: v.progress }); });
  $effect(() => { if (view?.error) appStatus.set('politics-error', { state: 'error', label: 'Operazione non riuscita', detail: view.error }); else appStatus.clear('politics-error'); });
  $effect(() => { if (view?.storageError) appStatus.set('politics-storage', { state: 'error', label: 'Errore di salvataggio', detail: view.storageError }); else appStatus.clear('politics-storage'); });
  $effect(() => { if (fileError) appStatus.set('politics-file-error', { state: 'error', label: 'Errore file', detail: fileError }); else appStatus.clear('politics-file-error'); });
  async function track<T>(label: string, task: (progress: ArchiveProgressListener) => Promise<T>): Promise<T> {
    const id = crypto.randomUUID(); appStatus.set(id, { state: 'working', label });
    try {
      await tick(); await new Promise<void>(resolve => setTimeout(resolve, 0));
      return await task(progress => appStatus.set(id, { state: 'working', label, detail: archiveProgressText(progress) }));
    } finally { appStatus.clear(id); }
  }
  function download(file: Blob, name: string) { const url = URL.createObjectURL(file), a = document.createElement('a'); a.href = url; a.download = name; a.click(); setTimeout(() => URL.revokeObjectURL(url), 1000); }
  async function exportCheckpoint() {
    fileError = '';
    try { await track('Preparazione checkpoint per il download', async () => { const checkpoint = await client.checkpoint(); if (!checkpoint) throw new Error('Checkpoint non applicabile a M01'); download(new Blob([JSON.stringify(checkpoint)]), 'politiche-checkpoint.json'); }); }
    catch (e) { fileError = String(e); }
  }
  async function importCheckpoint(file: File) {
    fileError = '';
    const reader = new JsonStreamReader(jsonFileStream(file));
    try { const checkpoint = parsePoliticsCheckpoint(await track('Caricamento checkpoint', () => reader.value())); await reader.finish(); controller.prepare(false, checkpoint); }
    catch (e) { fileError = `Checkpoint non caricato: ${String(e)}`; } finally { await reader.close(); }
  }
  async function exportResult(format: 'json' | 'csv') {
    if (!view?.viewed) return; fileError = ''; exporting = true;
    try { const resultId = view.viewed.id; const file = await track('Preparazione risultati per il download', progress => client.results.call<File>('export', { resultId, owner: controller.owner, format }, progress)); download(file, `politiche-risultati.${format === 'json' ? 'json.gz' : 'csv'}`); }
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
    client = new PoliticsClient(new Worker(new URL('../workers/politics-workspace.worker.ts', import.meta.url), { type: 'module' }), resolve('/').replace(/\/$/, ''));
    controller = new PoliticsWorkspaceController(createDefaultCommonPoliticsScenario(), { runtime: client, storage: localStorage,
      changed: () => { render++; }, manifest: (id, owner) => client.results.call('manifest', { resultId: id, owner }),
      importResult: (file, id, owner) => track('Caricamento risultati', progress => client.results.call('import', { file, resultId: id, owner, commonPolitics: true }, progress)),
      validateScenario: async s => { const assets = await loadEditorAssets(s, validation.signal); parsePoliticsScenario(s, assets.context); } });
    client.onPreparationProgress = (detail, completed, total) => controller.preparationProgress(detail, completed, total);
    void controller.initialize().then(async () => {
      if (!alive) return; ready = true;
      const input = takeWorkspaceImport('politics-legacy') ?? takeWorkspaceImport('politics-reform');
      if (!input) return;
      await track('Caricamento file', async () => {
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
      });
    }).catch(e => { if (alive) fileError = String(e); });
    return () => { alive = false; validation.abort(); controller.dispose(); appStatus.clear('politics-execution'); appStatus.clear('politics-error'); appStatus.clear('politics-storage'); appStatus.clear('politics-file-error'); };
  });
</script>
<svelte:head><title>Elezioni politiche</title></svelte:head>
{#if view}
  {#key editorEpoch}<CommonPoliticsEditor scenario={view.scenario} onchange={s => { controller.change(s); }} {execution} {stageExternalFile} />{/key}
  <section class="sessions" aria-label="Sessioni dei risultati">
    <h2>Risultati delle elezioni politiche</h2>
    {#if view.viewed?.politics}
      {@const record = view.viewed}
      <p>Scenario eseguito: {record.politics!.run.executedScenario.name} · {record.politics!.run.execution.generator.id}@{record.politics!.run.execution.generator.version} · {record.result.completedSimulations}/{record.result.benchmark.simulations} simulazioni conservate · worker effettivi {record.politics!.run.execution.actualWorkers}.</p>
      {#if view.stale}<Notice as="p" severity="warning" role="status">Lo scenario è stato modificato: questi
        risultati si riferiscono ad uno scenario precedente, che è conservato 
        insieme ai risultati ed è possibile ripristinare. Per ottenere i risultati
        relativi allo scenario modificato occorre avviare una nuova simulazione.</Notice>{/if}
      {#if view.recommendation}<Notice as="p" severity="info" role="status">Le 10 simulazioni sono riuscite. Per risultati significativi consigliamo 1.000 simulazioni: modifica il numero e avvia una nuova esecuzione.</Notice>{/if}
    {/if}
    <details class="result-transfer">
      <summary>Caricamento, selezione e download dei risultati</summary>
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
        <div class="controls"><button disabled={view.busy} onclick={loadViewedScenario}>Carica lo scenario di questa sessione</button>
          <button disabled={exporting || view.busy} onclick={() => exportResult('json')}>Scarica archivio risultati JSON.gz</button>
          <button disabled={exporting || view.busy} onclick={() => exportResult('csv')}>Scarica risultati CSV</button></div>
      {/if}
    </details>
    {#if view.viewed?.politics}
      {@const record = view.viewed}
      {#key `${record.id}:${record.result.completedSimulations}:${record.result.storage?.state}`}<PoliticsSessionResults {record} client={client.results} />{/key}
    {/if}
  </section>
{/if}
<style>
  .sessions { margin-block-start: var(--space-4); padding-block-start: var(--space-4); border-top: 1px solid var(--color-border); }
  .result-transfer { margin-block: var(--space-3) var(--space-4); }
  .result-transfer > summary { font-weight: 600; }
  label { display: grid; gap: var(--space-1); margin-block: var(--space-3); }
  .controls { display: flex; flex-wrap: wrap; gap: var(--space-3); }
</style>
