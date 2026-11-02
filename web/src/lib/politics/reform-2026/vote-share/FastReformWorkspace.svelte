<script lang="ts">
  import { confirmWorkspaceReset } from '../../../workspaces/draft.svelte';
  import WorkspaceShell from '../../../workspaces/WorkspaceShell.svelte';
  import RunControls from '../../../workspaces/RunControls.svelte';
  import ResultTableView from '../../../workspaces/ResultTable.svelte';
  import { readResultPage } from '../../../workspaces/result-access';
  import { base } from '$app/paths';
  import { onMount, onDestroy } from 'svelte';
  import SimulationWorker from '../../../workers/simulation.worker?worker';
  import { ResultServiceClient } from '../../../core/result-service-client';
  import { requireSimulationCount } from '../../../core/runtime-data';
  import { requireReformWorkerCount, workerMemoryWarning, workerHeapWarning } from '../../../core/reform-worker-capacity';
  import {isM01Generation} from './legacy-adapter';
  import { JsonStreamReader, jsonFileStream } from '../../../core/json-stream';
  import {
    parseSimulationResultExport,
    type SimulationResultExport,
  } from '../../../core/result-export';
  import type { StoredResult } from '../../../core/result-store';
  import type {
    SimulationRequest,
    SimulationResult,
    SimulationWorkerMessage,
    ResultTable,
  } from '../../../core/types';
  import type { ResultServiceReply } from '../../../workers/result-service';
  import type { ReformGenerationScenario } from '../generation-types';
  import type { ReleasedScenario, CandidatePointer } from './release';
  import type { CompactSource } from './source-types';
  import type { FitClientState } from './fit-client';
  import type {
    PreviewSelection,
    ReformEditorPreview,
  } from '../editor-preview';
  import { ReformFitClient } from './fit-client';
  import { labelFastDraft, futureEvidenceSignature } from './editor-model';
  import { validateFastScenario, requireFastScenario } from './scenario';
  import {
    requirePortableFitFile,
    sameCheckpointScenario,
    type PortableFitFile,
  } from './checkpoint';
  import { fastReformImplementations } from './build';
  import { loadEditorDefault } from './editor-loading';
  import FastScenarioEditor from './FastScenarioEditor.svelte';
  import ReformResultCharts from '../ReformResultCharts.svelte';
  import { buildReformSeatChart } from '../result-presentation';
  import DiagnosticMessages from '../../../results/DiagnosticMessages.svelte';
  let {
    imported = null,
    importedScenario = null,
    importedFile = null,
    onLegacyImport,
    onImportConsumed,
    onRouteFile,
    onBusyChange,
  }: {
    imported?: SimulationResultExport | null;
    importedScenario?: ReformGenerationScenario | import('./release').ReleasedScenario | null;
    importedFile?: File | null;
    onLegacyImport: (p: SimulationResultExport) => void;
    onImportConsumed: () => void;
    onRouteFile?: (file: File) => Promise<boolean>;
    onBusyChange?: (busy: boolean) => void;
  } = $props();
  const storageKey = 'elezioni:politics-reform-2026:fast-draft:v1',
    resultKey = 'elezioni:politics-reform-2026:fast-results:v1';
  let scenario = $state<ReleasedScenario | null>(null);
  let defaultScenario = $state.raw<ReleasedScenario | undefined>();
  let pointer = $state.raw<CandidatePointer | undefined>();
  let fitting: ReformFitClient | undefined,
    resultClient: ResultServiceClient | undefined;
  let fitState = $state<FitClientState>({
    phase: 'empty',
    revision: 0,
    acceptedRevision: null,
  });
  let metadataLoading = $state(false),
    dataError = $state(''),
    error = $state(''),
    busy = $state(''),
    sourceLoading = $state(false),
    loaded = $state(false),
    jsonPending = $state(false),
    editorRevision = $state(0);
  let editorData = $state.raw<ReturnType<ReformFitClient['editorData']> | null>(
      null,
    ),
    source = $state.raw<CompactSource | null>(null);
  let preview = $state.raw<Pick<
      ReformEditorPreview,
      'placements' | 'bonusSelection'
    > | null>(null),
    previewSelection = $state<PreviewSelection | undefined>();
  let simulations = $state(10),
    computeWorkers = $state(1),
    maximumWorkers = $state(0),
    running = $state(false),
    runPhase = $state('Pronto');
  let workerCapacity=$state.raw<ReturnType<ReformFitClient['generationCapacity']>>();
  const memoryWarning=$derived(workerCapacity ? workerMemoryWarning(computeWorkers,workerCapacity) : false);
  let result = $state.raw<SimulationResult | null>(null),
    resultScenario = $state<ReleasedScenario | null>(null),
    owner = $state(''),
    savedResults = $state<{ id: string; label: string }[]>([]),
    selectedSaved = $state('');
  let table = $state.raw<ResultTable | undefined>(),
    tableIndex = $state(0),
    page = $state(0),
    alive = true,
    currentRun = '',
    tableRequest = 0;
  let singleOpen = $state(false),
    selectedRun = $state(1),
    selectedChamber = $state('camera'),
    chartTable = $state.raw<ResultTable | undefined>(),
    chartRequest = 0;
  let leases: { release: () => void }[] = [],
    timer: ReturnType<typeof setTimeout> | undefined,
    controller = new AbortController();
  let evidenceChanged = false;
  const errors = $derived(scenario ? validateFastScenario(scenario) : []);
  const fittingBusy = $derived(
    ['loading', 'validating', 'fitting'].includes(fitState.phase),
  );
  const canRun = $derived(
    loaded &&
      fitState.phase === 'ready' &&
      fitState.acceptedRevision === fitState.revision &&
      !jsonPending &&
      errors.length === 0,
  );
  const stale = $derived(
    scenario &&
      resultScenario &&
      !sameCheckpointScenario(scenario, resultScenario),
  );
  const pageCount = $derived(
    Math.max(
      1,
      Math.ceil((result?.storage?.tables[tableIndex]?.rows ?? 0) / 50),
    ),
  );
  const chart = $derived(
    resultScenario
      ? buildReformSeatChart(
          chartTable,
          selectedChamber,
          selectedRun,
          resultScenario,
        )
      : [],
  );
  const fitLabel = $derived(
    {
      empty: 'Modello non caricato',
      loading: 'Caricamento modello',
      validating: 'Verifica compatibilità',
      stale: 'Stima da aggiornare o scenario da verificare',
      fitting: 'Stima in corso',
      ready: 'Modello pronto',
      cancelled: 'Operazione annullata',
      error: 'Errore del modello',
      disposed: 'Chiuso',
    }[fitState.phase],
  );
  $effect(() => onBusyChange?.(running || !!busy || fittingBusy));
  onMount(() => {
    void initialize();
  });
  onDestroy(() => {
    alive = false;
    controller.abort();
    clearTimeout(timer);
    fitting?.dispose();
    resultClient?.close();
    release();
  });
  $effect(() => {
    const id = result?.storage?.id,
      index = tableIndex,
      p = Math.min(page, pageCount - 1);
    if (id && result?.storage?.tables[index]) void fetchTable(id, index, p);
  });
  $effect(() => {
    const id = result?.storage?.id,
      simulation = selectedRun;
    if (singleOpen && id) void fetchChart(id, simulation);
  });
  function describe(e: unknown) {
    return e instanceof Error ? e.message : String(e);
  }
  function report(e: unknown) {
    if (alive && !(e instanceof DOMException && e.name === 'AbortError'))
      error = describe(e);
  }
  function service() {
    return (resultClient ??= new ResultServiceClient(new SimulationWorker()));
  }
  function release() {
    for (const l of leases) l.release();
    leases = [];
  }
  function remember(id: string, label: string) {
    savedResults = [...savedResults.filter((r) => r.id !== id), { id, label }];
    selectedSaved = id;
    sessionStorage.setItem(resultKey, JSON.stringify(savedResults));
  }
  function rememberDraft() {
    if (!scenario || validateFastScenario(scenario).length) return;
    try {
      localStorage.setItem(
        storageKey,
        JSON.stringify($state.snapshot(scenario)),
      );
    } catch (e) {
      report(e);
    }
  }
  async function initialize() {
    try {
      owner =
        sessionStorage.getItem(resultKey + ':owner') ?? crypto.randomUUID();
      sessionStorage.setItem(resultKey + ':owner', owner);
      savedResults = JSON.parse(sessionStorage.getItem(resultKey) ?? '[]');
      await loadMetadata();
      const savedDraft = localStorage.getItem(storageKey);
      if (savedDraft)
        scenario = labelFastDraft(requireFastScenario(JSON.parse(savedDraft)));
      if (importedFile) await importFile(importedFile);
      else if (imported) {
        if (
          'schemaVersion' in imported.scenario &&
          imported.scenario.schemaVersion === 5 &&
          imported.fitFile
        )
          await importFile(
            new File(
              [JSON.stringify({ ...imported, schema_version: 3 })],
              'results.json',
            ),
          );
        else onLegacyImport(imported);
      } else if (importedScenario)
        scenario = labelFastDraft(requireFastScenario(importedScenario));
      else {
        const saved = localStorage.getItem(storageKey);
        if (saved)
          scenario = labelFastDraft(requireFastScenario(JSON.parse(saved)));
        if (savedResults.length) await restoreResult(savedResults.at(-1)!.id);
      }
    } catch (e) {
      report(e);
    } finally {
      if (alive) onImportConsumed();
    }
  }
  async function loadMetadata() {
    metadataLoading = true;
    dataError = '';
    try {
      const data = await loadEditorDefault(base, controller.signal);
      if (!alive) return;
      pointer = data.pointer;
      defaultScenario = labelFastDraft(data.scenario);
      scenario ??= structuredClone(defaultScenario);
    } catch (e) {
      if (alive) dataError = describe(e);
    } finally {
      if (alive) metadataLoading = false;
    }
  }
  function createClient(pin = pointer) {
    if (!pin || !fastReformImplementations)
      throw new Error('Catalogo / identità della build non disponibili');
    fitting?.dispose();
    fitting = new ReformFitClient(
      { base, pointer: pin, implementations: fastReformImplementations },
      {
        onChange: (s) => {
          if (alive) fitState = s;
        },
      },
    );
    loaded = false;
    return fitting;
  }
  function refreshWorkers() {
    workerCapacity=fitting?.generationCapacity(navigator);
    maximumWorkers=workerCapacity?.maximum??0;
    const saved=Number(localStorage.getItem(storageKey+':workers:'+(scenario?.generation?.model??'M02')));
    computeWorkers=Number.isSafeInteger(saved)&&saved>=1?Math.min(saved,maximumWorkers):workerCapacity?.recommended??1;
  }
  async function loadModel() {
    error = '';
    try {
      const c = fitting ?? createClient();
      await c.load();
      if (!alive) return;
      loaded = true;
      editorData = c.editorData();
      refreshWorkers();
      const draft = scenario
        ? structuredClone($state.snapshot(scenario))
        : c.copyDraft()!;
      // A lightweight local draft may name a previous custom fit. Loading the
      // default gives this draft a new accepted parent; its evidence stays stale.
      draft.data.fittedStateId = c.copyDraft()!.data.fittedStateId;
      draft.data.profileStateId = c.copyDraft()!.data.profileStateId;
      if (!sameCheckpointScenario(draft, c.copyDraft()!)) {
        evidenceChanged =
          futureEvidenceSignature(draft) !==
          futureEvidenceSignature(c.copyDraft()!);
        c.setDraft(draft);
        if (!evidenceChanged) await validate();
      } else scenario = c.copyDraft()!;
    } catch (e) {
      report(e);
    }
  }
  function change(next: ReleasedScenario) {
    const previous = scenario;
    scenario = next;
    preview = null;
    rememberDraft();
    clearTimeout(timer);
    if (!loaded || !fitting) return;
    // setDraft cancels a pending worker immediately; accepted arrays remain private.
    evidenceChanged =
      evidenceChanged ||
      (!!previous &&
        futureEvidenceSignature(previous) !== futureEvidenceSignature(next));
    fitting.setDraft($state.snapshot(next));
    if (previous?.generation?.model !== next.generation?.model) refreshWorkers();
    if (!evidenceChanged && !running && validateFastScenario(next).length === 0)
      timer = setTimeout(() => {
        void validate();
      }, 250);
  }
  async function validate() {
    if (!fitting || running || jsonPending) return;
    try {
      await fitting.validate();
      if (alive && fitting.canRun) {
        scenario = fitting.copyDraft()!;
        refreshWorkers();
        rememberDraft();
      }
    } catch (e) {
      report(e);
    }
  }
  async function refit() {
    if (!fitting || running) return;
    error = '';
    try {
      await fitting.fit();
      if (alive) {
        scenario = fitting.copyDraft()!;
        evidenceChanged = false;
        editorData = fitting.editorData();
        refreshWorkers();
        rememberDraft();
      }
    } catch (e) {
      report(e);
    }
  }
  async function loadSource() {
    if (!fitting || !loaded) {
      error =
        'Carica prima il modello per consultare il catalogo delle osservazioni.';
      return;
    }
    if (running) {
      error =
        'Attendi il termine del calcolo per caricare il catalogo delle osservazioni.';
      return;
    }
    sourceLoading = true;
    try {
      source = (await fitting.source()).source ?? null;
    } catch (e) {
      report(e);
    } finally {
      if (alive) sourceLoading = false;
    }
  }
  async function showPreview(selection: PreviewSelection) {
    if (!fitting || !canRun || running) return;
    try {
      const p = await fitting.preview(selection);
      preview = p.preview ?? null;
      previewSelection = selection;
    } catch (e) {
      report(e);
    }
  }
  async function reset() {
    if (running || busy) return;
    if (!confirmWorkspaceReset()) return;
    fitting?.dispose();
    fitting = undefined;
    loaded = false;
    editorData = null;
    source = null;
    scenario = structuredClone(defaultScenario!);
    evidenceChanged = false;
    jsonPending = false;
    preview = null;
    editorRevision++;
    rememberDraft();
    error = '';
  }
  function download(blob: Blob, name: string) {
    const url = URL.createObjectURL(blob),
      a = document.createElement('a');
    a.href = url;
    a.download = name;
    a.click();
    setTimeout(() => URL.revokeObjectURL(url), 60000);
  }
  async function saveSession() {
    if (!fitting || running) return;
    busy = 'Preparazione scenario con stato stimato';
    try {
      const file = (await fitting.checkpoint()).file;
      if (!file) throw new Error('Stato assente');
      download(
        new Blob([JSON.stringify(file)], { type: 'application/json' }),
        'r26-scenario-stimato.json',
      );
    } catch (e) {
      report(e);
    } finally {
      if (alive) busy = '';
    }
  }
  async function restoreSession(file: PortableFitFile) {
    const c = fitting ?? createClient(file.pointer);
    await c.restoreCheckpoint(file);
    if (!alive) return;
    loaded = true;
    editorData = c.editorData();
    source = null;
    const accepted = c.copyDraft()!;
    scenario = labelFastDraft(file.scenario);
    refreshWorkers();
    evidenceChanged =
      futureEvidenceSignature(accepted) !== futureEvidenceSignature(scenario);
    if (!sameCheckpointScenario(scenario, accepted)) {
      c.setDraft($state.snapshot(scenario));
      if (!evidenceChanged) await validate();
    }
    jsonPending = false;
    editorRevision++;
    preview = null;
    rememberDraft();
  }
  async function importFile(file: File) {
    if (running) return;
    busy = 'Caricamento file';
    try {
      if (await onRouteFile?.(file)) return;
      const reader = new JsonStreamReader(jsonFileStream(file));
      let kind = '',
        legacyArchive = false;
      const stop = Symbol();
      try {
        await reader.object(async (key) => {
          if (key === 'kind') {
            kind = String(await reader.value());
            throw stop;
          }
          if (key === 'schema_version') {
            legacyArchive = (await reader.value()) === 2;
            if (legacyArchive) throw stop;
            return;
          }
          if (key === 'scenario' || key === 'result') throw stop;
          await reader.skip();
        });
      } catch (e) {
        if (e !== stop) throw e;
      } finally {
        await reader.close();
      }
      if (legacyArchive)
        throw new Error(
          'Questo file usa il generatore precedente. È supportato solo il formato corrente; non esiste una conversione statistica automatica.',
        );
      if (kind === 'politics-reform-session') {
        if (file.size > 90 * 1024 * 1024)
          throw new Error('File scenario troppo grande');
        const raw = await new Response(jsonFileStream(file)).text();
        if (raw.length > 90 * 1024 * 1024)
          throw new Error('File scenario troppo grande');
        await restoreSession(requirePortableFitFile(JSON.parse(raw)));
        return;
      }
      if (kind === 'politics-reform-scenario') {
        const parsed = JSON.parse(
          await new Response(jsonFileStream(file)).text(),
        );
        if (parsed.schemaVersion !== 5)
          throw new Error(
            'Questo scenario usa il generatore precedente. È supportato solo il formato corrente; non esiste una conversione statistica automatica.',
          );
        change(requireFastScenario(parsed));
        editorRevision++;
        jsonPending = false;
        return;
      }
      const record = await service().call<StoredResult>('import', {
        resultId: crypto.randomUUID(),
        owner,
        file,
      });
      if (
        !('schemaVersion' in record.scenario) ||
        record.scenario.schemaVersion !== 5
      ) {
        onLegacyImport(
          parseSimulationResultExport(
            await new Response(jsonFileStream(file)).text(),
          ),
        );
        return;
      }
      attach(record); // Viewing results does not overwrite the separate scenario draft.
    } finally {
      if (alive) busy = '';
    }
  }
  async function loadFile(event: Event) {
    const input = event.currentTarget as HTMLInputElement,
      file = input.files?.[0];
    if (!file) return;
    try {
      await importFile(file);
    } catch (e) {
      report(e);
    } finally {
      input.value = '';
    }
  }
  function attach(record: StoredResult) {
    tableRequest++;
    result = record.result;
    resultScenario = requireFastScenario(record.scenario);
    tableIndex = 0;
    page = 0;
    table = undefined;
    remember(
      record.id,
      `${record.result.benchmark.startedAt} · ${record.result.storage?.completedSimulations ?? 0}/${record.result.benchmark.simulations}`,
    );
  }
  async function restoreResult(id: string) {
    try {
      attach(
        await service().call<StoredResult>('manifest', { resultId: id, owner }),
      );
    } catch (e) {
      report(e);
    }
  }
  async function restoreResultScenario() {
    if (!result?.storage || running) return;
    busy = 'Ripristino scenario dei risultati';
    try {
      const file = await service().call<PortableFitFile>('fit-session', {
        resultId: result.storage.id,
        owner,
      });
      await restoreSession(requirePortableFitFile(file));
    } catch (e) {
      report(e);
    } finally {
      if (alive) busy = '';
    }
  }
  async function exportResult(format: 'json' | 'csv') {
    if (!result?.storage) return;
    busy = 'Preparazione export';
    try {
      download(
        await service().call<File>('export', {
          resultId: result.storage.id,
          owner,
          format,
        }),
        format === 'json' ? 'r26-risultati.json.gz' : 'r26-risultati.csv',
      );
    } catch (e) {
      report(e);
    } finally {
      if (alive) busy = '';
    }
  }
  async function clearResult() {
    if (!result?.storage) return;
    const id = result.storage.id;
    try {
      await service().call('delete', { resultId: id, owner });
      savedResults = savedResults.filter((r) => r.id !== id);
      sessionStorage.setItem(resultKey, JSON.stringify(savedResults));
      result = null;
      resultScenario = null;
      table = undefined;
    } catch (e) {
      report(e);
    }
  }
  async function fetchTable(id: string, index: number, p: number) {
    const version = ++tableRequest;
    try {
      if (!result || result.storage?.id !== id) return;
      const t = await readResultPage(result, { table: index, offset: p * 50 },
        request => service().call<ResultTable>('page', { ...request, owner }));
      if (alive && version === tableRequest) table = t;
    } catch (e) {
      if (version === tableRequest) report(e);
    }
  }
  async function fetchChart(id: string, simulation: number) {
    const version = ++chartRequest,
      index =
        result?.storage?.tables.findIndex(
          (t) => t.name === 'Seat entitlement and election locations',
        ) ?? -1;
    if (index < 0 || !Number.isSafeInteger(simulation) || simulation < 1) {
      chartTable = undefined;
      return;
    }
    try {
      if (!result || result.storage?.id !== id) return;
      const t = await readResultPage(result, { table: index, offset: 0, limit: 1000, simulation },
        request => service().call<ResultTable>('page', { ...request, owner }));
      if (alive && version === chartRequest) chartTable = t;
    } catch (e) {
      if (version === chartRequest) report(e);
    }
  }
  async function cancelRun() {
    if (currentRun)
      try {
        await service().call('cancel', { resultId: currentRun, owner });
      } catch (e) {
        report(e);
      }
  }
  async function run() {
    if (!fitting || !canRun || running || busy) return;
    clearTimeout(timer);
    busy = 'Congelamento dello scenario';
    error = '';
    try {
      requireSimulationCount(simulations);
      requireReformWorkerCount(computeWorkers, maximumWorkers);
      const file = (await fitting.checkpoint()).file;
      if (!file) throw new Error('Stato stimato mancante');
      // An edit while the checkpoint worker was running cancels that operation.
      if (!fitting.canRun)
        throw new Error('Scenario modificato durante la preparazione');
      // Send one owned packet across the page boundary. The coordinator copies
      // it separately to each compute worker: a wide pool must not serialize all
      // worker packets into one browser IPC message.
      const reserved = fitting.reserveGenerationCopies(1, navigator);
      leases = reserved.leases;
      const captured = file.scenario,
        id = crypto.randomUUID(),
        worker = service().worker;
      currentRun = id;
      running = true;
      runPhase = 'Preparazione';
      remember(id, 'Calcolo in corso');
      const request: SimulationRequest = {
        kind: 'politiche',
        basePath: base,
        resultId: id,
        resultOwner: owner,
        scenario: captured,
        fast: { file, packets: reserved.leases.map((l) => l.packet) },
        scrutinyAlgorithmId: captured.algorithmId,
        electionDate: captured.electionDate,
        seed: captured.seed,
        dataVersion: captured.data.sourceBundleId,
        simulations,
        computeWorkers,
      };
      worker.onmessage = (
        e: MessageEvent<SimulationWorkerMessage | ResultServiceReply>,
      ) => {
        const m = e.data;
        if (m.type === 'result-service') return;
        if (m.type === 'progress') {
          runPhase = `${m.phase} ${m.completed}/${m.total}`;
          return;
        }
        release();
        running = false;
        currentRun = '';
        runPhase = m.status;
        if (m.storage)
          attach({ id, owner, scenario: captured, result: m, exports: [] });
        else report(m.warnings.map((w) => w.message).join('\n'));
        if (!evidenceChanged && fitState.phase === 'stale') void validate();
      };
      worker.onerror = (e) => {
        release();
        running = false;
        currentRun = '';
        runPhase = 'Errore';
        report(e.message);
      };
      worker.postMessage(
        request,
        reserved.leases.flatMap((l) => l.transfer),
      );
    } catch (e) {
      release();
      running = false;
      report(e);
    } finally {
      if (alive) busy = '';
    }
  }
</script>

<WorkspaceShell id="politics-reform" busy={running || !!busy}>
<main class="fast-workspace" data-testid="fast-reform-workspace">
  <h1>Politiche — Riforma 2026</h1>
  <p>Modelli di generazione con dati, scenario e scrutinio condivisi.</p>
  <section aria-label="Dati e stima">
    <h2>Dati e stima</h2>
    {#if metadataLoading}<p role="status">
        Caricamento scenario iniziale…
      </p>{/if}
    {#if dataError}<p role="alert">{dataError}</p>
      <button onclick={loadMetadata} disabled={metadataLoading}
        >Riprova caricamento scenario</button
      >{/if}
    <p role="status" data-testid="fit-status">
      {fitLabel}{#if fitState.progress}
        · {fitState.progress.phase}
        {fitState.progress.completed}/{fitState.progress.total}{/if}
    </p>
    {#if fitState.error && fitState.phase !== 'cancelled'}<p role="alert">
        {fitState.error}
      </p>{/if}
    <label
      >Carica scenario o risultati<input
        type="file"
        accept=".json,.gz"
        onchange={loadFile}
        disabled={running || fittingBusy || !!busy}
        data-testid="reform-file-input"
      /></label
    >
    <div class="controls">
      <button
        onclick={loadModel}
        disabled={!pointer || fittingBusy || running || !!busy}
        >{loaded
          ? 'Ricarica modello predefinito'
          : 'Carica modello predefinito'}</button
      >
      <button
        onclick={refit}
        disabled={!loaded ||
          fittingBusy ||
          running ||
          !!busy ||
          jsonPending ||
          errors.length > 0}>{scenario && isM01Generation(scenario) ? 'Prepara riferimenti legacy' : 'Ricalcola parametri'}</button
      >
      <button
        onclick={validate}
        disabled={!loaded ||
          fittingBusy ||
          running ||
          !!busy ||
          jsonPending ||
          errors.length > 0}>Verifica scenario</button
      >
      {#if fittingBusy}<button onclick={() => fitting?.cancel()}
          >Annulla operazione modello</button
        >{/if}
    </div>
    {#if !loaded}<p>
        Puoi preparare lo scenario prima di caricare i file del modello. Il
        caricamento avviene su richiesta; nessun prefetch in background.
      </p>{/if}
    {#if fitState.plan && scenario && !isM01Generation(scenario)}<p>
        Riutilizzo: nazionale {fitState.plan.reused.national ? 'sì' : 'no'},
        profilo Camera {fitState.plan.reused.cameraProfile ? 'sì' : 'no'},
        adattamento Senato {fitState.plan.reused.senateAdapter ? 'sì' : 'no'}.
      </p>{/if}
  </section>
  {#if scenario}
    <p>
      {scenario.components.filter((c) => c.kind === 'fitted').length} componenti stimati;
      {scenario.components.filter((c) => c.kind === 'new').length} componenti da riferimento
      con ipotesi di fallback esplicite.
    </p>
    {#if jsonPending}<p role="status">
        Bozza JSON in attesa: applicala o scartala per eseguire un calcolo.
      </p>{/if}
    <section aria-label="Scenario riforma">
      <h2>Scenario riforma</h2>
      <div class="controls">
        <button
          onclick={saveSession}
          disabled={!loaded ||
            running ||
            fittingBusy ||
            !!busy ||
            jsonPending ||
            errors.length > 0}>Scarica scenario con stima</button
        ><button
          onclick={reset}
          disabled={!defaultScenario || running || !!busy}
          >Ripristina scenario predefinito</button
        >
      </div>
      {#key editorRevision}<FastScenarioEditor
          {scenario}
          {editorData}
          {source}
          {sourceLoading}
          onloadsource={loadSource}
          onchange={change}
          onerror={(m) => (error = m)}
          onpreview={showPreview}
          {preview}
          {previewSelection}
          canPreview={canRun && !running && !busy}
          onpending={(p) => (jsonPending = p)}
        />{/key}
      {#if errors.length}<details>
          <summary>Correzioni necessarie ({errors.length})</summary>
          <ul>
            {#each errors as e, i (i)}<li>{e}</li>{/each}
          </ul>
        </details>{/if}
    </section>
    <section aria-label="Simulazione">
      <h2>Simulazione</h2>
      <RunControls bind:simulations showSeed={false} {canRun} {running} disabled={!!busy || maximumWorkers === 0}
        label="Esegui riforma" onrun={run} oncancel={cancelRun}>
<label
          >Processi di calcolo<select
            aria-label="Processi di calcolo"
            bind:value={computeWorkers}
            onchange={e=>localStorage.setItem(storageKey+':workers:'+(scenario?.generation?.model??'M02'),e.currentTarget.value)}
            disabled={running || !!busy || maximumWorkers === 0}
            >{#each Array.from({ length: maximumWorkers }, (_, i) => i + 1) as n (n)}<option
                value={n}>{n}</option
              >{/each}</select
          ></label
        >      </RunControls>
      <p>
        Massimo CPU: {maximumWorkers} processi; sono riservati due processori logici
        per interfaccia e coordinamento sui dispositivi con almeno quattro processori.
        {#if workerCapacity}Consigliati: {workerCapacity.recommended}; memoria stimata totale con la selezione corrente:
          {((workerCapacity.coordinatorBytes+computeWorkers*workerCapacity.perWorkerBytes)/1024**3).toFixed(2)} GiB.
          {#if workerCapacity.deviceBytes===null}Il browser non comunica la memoria: predefinito un processo.{:else}Memoria dichiarata dal browser: {(workerCapacity.deviceBytes/1024**3).toFixed(1)} GiB; il predefinito mira a usarne al massimo metà.{/if}
          Il consiglio considera anche un margine prudenziale per la memoria del browser.
        {/if} Le stime includono copie dei dati e memoria di lavoro; non misurano la RAM libera.
      </p>
      {#if memoryWarning}<p role="alert">Attenzione: il numero scelto di processi è stimato usare tutta la memoria dichiarata del dispositivo o superarla. Puoi ridurlo prima del calcolo.</p>{/if}
      {#if workerCapacity && workerHeapWarning(computeWorkers, workerCapacity)}<p role="alert">Il numero scelto supera il margine prudenziale di memoria del browser. Il calcolo può esaurire la memoria anche con RAM libera nel dispositivo. Riduci i processi se il browser si interrompe.</p>{/if}
      <p role="status" data-testid="reform-phase">{runPhase}</p>
    </section>
  {/if}
  {#if busy}<p role="status">{busy}…</p>{/if}
  {#if error}<div role="alert">
      <p>{error}</p>
      <button onclick={() => (error = '')}>Chiudi messaggio</button>
    </div>{/if}
  {#if savedResults.length}<label
      >Risultati locali<select
        bind:value={selectedSaved}
        onchange={() => restoreResult(selectedSaved)}
        disabled={running || !!busy}
        >{#each savedResults as r (r.id)}<option value={r.id}>{r.label}</option
          >{/each}</select
      ></label
    >{/if}
  {#if result}
    <section aria-label="Risultati riforma">
      <h2>Risultati riforma</h2>
      <p data-testid="stored-result-status">
        {result.storage?.completedSimulations ?? 0}/{result.benchmark
          .simulations} simulazioni conservate · {result.status} · {result.benchmark.elapsedMs.toFixed(
          0,
        )} ms
      </p>
      {#if stale}<p role="alert">
          Scenario modificato. Risultati ed export conservano lo scenario e la
          stima eseguiti.
        </p>{/if}
      <div class="controls">
        <button
          onclick={restoreResultScenario}
          disabled={running || fittingBusy || !!busy}
          >Ripristina scenario dei risultati</button
        ><button
          onclick={() => exportResult('json')}
          disabled={running || !!busy}>Scarica risultati JSON.gz</button
        ><button
          onclick={() => exportResult('csv')}
          disabled={running || !!busy}>Scarica risultati CSV</button
        ><button onclick={clearResult} disabled={running || !!busy}
          >Elimina risultato locale</button
        >
      </div>
      {#key result.storage?.id}<DiagnosticMessages
          client={service()}
          {result}
          {owner}
          onInspect={(simulation, chamber) => {
            selectedRun = simulation;
            if (chamber) selectedChamber = chamber;
            singleOpen = true;
          }}
        />{#if result.storage}<ReformResultCharts
            client={service()}
            resultId={result.storage.id}
            {owner}
          />{/if}{/key}
      <details bind:open={singleOpen}>
        <summary>Seggi della singola simulazione</summary>
        <div class="controls">
          <label
            >Simulazione grafico<input
              type="number"
              min="1"
              max={result.benchmark.simulations}
              step="1"
              bind:value={selectedRun}
            /></label
          ><label
            >Ramo grafico<select bind:value={selectedChamber}
              ><option value="camera">Camera</option><option value="senato"
                >Senato</option
              ></select
            ></label
          >
        </div>
        <figure aria-label="Seggi per categoria e titolare">
          <figcaption>Simulazione {selectedRun}, {selectedChamber}</figcaption>
          {#each chart as bar (bar.id)}<div>
              {bar.label}
              <meter
                min="0"
                max={selectedChamber === 'camera' ? 392 : 196}
                value={bar.seats}>{bar.seats}</meter
              >
              {bar.seats}
            </div>{:else}<p>
              Seggi non disponibili: consultare gli stadi e i blocchi della
              simulazione.
            </p>{/each}
        </figure>
      </details>
      <label
        >Tabella riforma<select
          bind:value={tableIndex}
          onchange={() => (page = 0)}
          >{#each result.tables as t, i (t.name)}<option value={i}
              >{t.name} ({result.storage?.tables[i]?.rows ?? 0})</option
            >{/each}</select
        ></label
      >
      <div class="controls">
        <button disabled={page === 0} onclick={() => page--}
          >Pagina precedente</button
        ><span
          >Pagina {Math.min(page + 1, pageCount)} di {pageCount} · massimo 50 righe</span
        ><button disabled={page >= pageCount - 1} onclick={() => page++}
          >Pagina successiva</button
        >
      </div>
      {#if table}<div class="table-scroll">
          <ResultTableView {table} />
        </div>{/if}
    </section>
  {/if}
</main>
</WorkspaceShell>

<style>
  .fast-workspace {
    max-width: 1500px;
    margin: auto;
    padding: 1.5rem;
    color: #17344a;
    font-family: system-ui, sans-serif;
  }
  section {
    border: 1px solid #cbd5e1;
    border-radius: 0.5rem;
    padding: 1rem;
    margin: 1rem 0;
    background: white;
  }
  .controls {
    display: flex;
    flex-wrap: wrap;
    gap: 0.75rem;
    align-items: end;
    margin: 1rem 0;
  }
  label {
    display: grid;
    gap: 0.3rem;
    max-width: 100%;
  }
  input,
  select,
  button {
    font: inherit;
    padding: 0.5rem;
    max-width: 100%;
    box-sizing: border-box;
  }
  button {
    cursor: pointer;
  }
  .table-scroll {
    overflow: auto;
  }
  [role='alert'] {
    color: #9f2519;
    overflow-wrap: anywhere;
  }
  @media (max-width: 600px) {
    .fast-workspace {
      padding: 0.5rem;
    }
    section {
      padding: 0.65rem;
    }
    .controls > label {
      width: 100%;
    }
  }
</style>
