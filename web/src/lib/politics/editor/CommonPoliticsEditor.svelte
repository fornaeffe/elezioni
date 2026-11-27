<script lang="ts">
  import { onMount, tick } from 'svelte';
  import { useStatus, type StatusError } from '$lib/app-status.svelte';
  import { errorDiagnostic, errorMessage } from '$lib/core/operation-error';
  import SplitButton from '$lib/ui/SplitButton.svelte';
  import FieldWithHelp from '$lib/ui/FieldWithHelp.svelte';
  const appStatus = useStatus();
  import { type PoliticsScenario, type GeneratorId, type LawId, laws, REFORM } from '../scenario/schema';
  import { activeGenerator, capabilities, supportedPairs } from '../scenario/registry';
  import { modelRegistry, generationModels } from '../reform-2026/vote-share/model-registry';
  import { selectLaw, selectGenerator, setRenewal } from '../scenario/edits';
  import { createDefaultCommonPoliticsScenario } from '../scenario/defaults';
  import { encodeScenario, decodeScenario } from '../scenario/serialization';
  import { parsePoliticsScenario, validateActiveScenario } from '../scenario/validation';
  import { editScenario, type ApplyEdit } from './model';
  import { loadEditorAssets, type EditorAssets } from './assets';
  import SubjectsEditor from './SubjectsEditor.svelte';
  import ReferencesEditor from './ReferencesEditor.svelte';
  import AdvancedEditor from './AdvancedEditor.svelte';
  import LazySection from './LazySection.svelte';
  import RunControls, { type ExecutionControls } from './RunControls.svelte';
  import CandidateForm from './CandidateForm.svelte';
  let { scenario, onchange, execution, stageExternalFile }: { scenario: PoliticsScenario; onchange: (next: PoliticsScenario) => void;
    execution?: ExecutionControls; stageExternalFile?: (file: File) => Promise<(() => void) | null> } = $props();
  const referenceDateId = $props.id();
  let assets = $state.raw<EditorAssets | null>(null), status = $state('Caricamento del modello selezionato…');
  let error = $state.raw<StatusError | null>(null);
  let controller: AbortController | null = null, importController: AbortController | null = null, disposed = false, epoch = 0, editRevision = 0;
  $effect(() => { appStatus.set('politics-editor', error ? { ...error, retry: !assets ? () => void load(scenario) : undefined } : !assets ? { state: status === 'Caricamento annullato' ? 'ready' : 'working', label: status, cancel: () => { controller?.abort(); ++epoch; status = 'Caricamento annullato'; }, retry: () => void load(scenario) } : { state: 'ready', label: status }); });
  let generator = $derived(activeGenerator(scenario));
  let issues = $derived(assets ? validateActiveScenario(scenario, assets.context) : []);
  const lawName = (law: LawId) => law === REFORM ? 'Riforma 2026' : 'Rosatellum';
  async function load(next: PoliticsScenario) {
    controller?.abort(); controller = new AbortController();
    const revision = ++epoch;
    error = null; assets = null; status = `Caricamento ${activeGenerator(next).id} · ${lawName(next.selection.scrutiny.id)}…`;
    try {
      const loaded = await loadEditorAssets(next, controller.signal);
      parsePoliticsScenario(next, loaded.context);
      if (disposed || revision !== epoch) return;
      assets = loaded; status = `${activeGenerator(next).id} · dati verificati`;
    } catch (e) {
      if (disposed || revision !== epoch || controller.signal.aborted) return;
      status = 'Caricamento non riuscito';
      error = { state: 'error', label: 'Dati dell’editor non caricati', diagnostics: [errorDiagnostic(e, 'EDITOR_LOAD_FAILED')] };
    }
  }
  const apply: ApplyEdit = (edit, onError) => {
    if (execution?.busy) {
      onError?.('Attendi il termine della simulazione per modificare lo scenario.');
      return false;
    }
    try {
      const before = scenario;
      const next = parsePoliticsScenario(edit(scenario), assets?.context);
      onchange(next); ++editRevision; importController?.abort(); error = null;
      if (before.selection.scrutiny.id !== next.selection.scrutiny.id || activeGenerator(before).id !== activeGenerator(next).id || JSON.stringify(before.data) !== JSON.stringify(next.data)) void load(next);
      return true;
    } catch (e) {
      if (onError) onError(errorMessage(e));
      else error = { state: 'error', label: 'Modifica non applicata', diagnostics: [errorDiagnostic(e, 'EDITOR_EDIT_FAILED')] };
      return false;
    }
  };
  async function importFile(file: File) {
    const revision = editRevision;
    error = null; importController?.abort();
    const staged = new AbortController(); importController = staged;
    const task = `scenario-import-${crypto.randomUUID()}`; appStatus.set(task, { state: 'working', label: 'Caricamento file' });
    try {
      const external = await stageExternalFile?.(file);
      if (disposed || staged.signal.aborted || execution?.busy || revision !== editRevision) return;
      if (external) { external(); return; }
      const next = decodeScenario(new Uint8Array(await file.arrayBuffer()));
      // Validate every exact dependency before replacing either draft or UI.
      const loaded = await loadEditorAssets(next, staged.signal);
      parsePoliticsScenario(next, loaded.context);
      if (disposed || staged.signal.aborted || execution?.busy || revision !== editRevision) return;
      onchange(next); ++editRevision; controller?.abort(); ++epoch; assets = loaded; error = null; status = `${activeGenerator(next).id} · dati verificati`;
    } catch (e) {
      if (!disposed && !staged.signal.aborted) error = { state: 'error', label: 'Scenario non caricato', diagnostics: [errorDiagnostic(e, 'SCENARIO_IMPORT_FAILED')] };
    } finally { appStatus.clear(task); }
  }
  async function download(gzip: boolean) {
    const task = `scenario-export-${crypto.randomUUID()}`; appStatus.set(task, { state: 'working', label: 'Preparazione scenario per il download' });
    await tick(); await new Promise<void>(resolve => setTimeout(resolve, 0));
    try { const url = URL.createObjectURL(new Blob([encodeScenario(scenario, gzip)])); const a = document.createElement('a'); a.href = url; a.download = `scenario-politiche.json${gzip ? '.gz' : ''}`; a.click(); setTimeout(() => URL.revokeObjectURL(url), 1000); }
    catch (e) { error = { state: 'error', label: 'Scenario non scaricato', diagnostics: [errorDiagnostic(e, 'SCENARIO_EXPORT_FAILED')] }; }
    finally { appStatus.clear(task); }
  }
  onMount(() => { void load(scenario); return () => { disposed = true; ++epoch; controller?.abort(); importController?.abort(); appStatus.clear('politics-editor'); }; });
</script>
<div class="common-politics-editor">
  <h1>Simulatore delle elezioni politiche</h1>
  {#if issues.length}<LazySection title={`Verifica dello scenario · ${issues.length} problemi da correggere`}><ul>{#each issues as issue, i (`${issue.code}:${issue.path}:${i}`)}<li>{issue.code}: {issue.message}</li>{/each}</ul></LazySection>{/if}
  <fieldset disabled={execution?.busy}>
    <div class="row">
      <label data-field>Carica scenario<input type="file" accept=".json,.gz" onchange={e => { const file = e.currentTarget.files?.[0]; if (file) void importFile(file); e.currentTarget.value = ''; }} /></label>
      <SplitButton
        label="Download dello scenario"
        optionsLabel="Opzioni di download dello scenario"
        disabled={execution?.busy}
        primary={{ label: 'Scarica scenario JSON.gz', run: () => download(true) }}
        options={[{ label: 'Scarica scenario JSON', run: () => download(false) }]}
      />
      <button onclick={() => { if (confirm('Ripristinare lo scenario predefinito? Le modifiche della bozza saranno sostituite.')) apply(() => createDefaultCommonPoliticsScenario()); }}>Ripristina scenario predefinito</button>
    </div>
    <div class="row">
      <label data-field>Nome scenario<input value={scenario.name} onchange={e => apply(s => editScenario(s, n => { n.name = e.currentTarget.value; }))} /></label>
      <label data-field>Data elezione<input type="date" value={scenario.electionDate} onchange={e => apply(s => editScenario(s, n => { n.electionDate = e.currentTarget.value; }))} /></label>
      <label data-field>Legge elettorale<select value={scenario.selection.scrutiny.id} onchange={e => apply(s => selectLaw(s, e.currentTarget.value as LawId))}>{#each laws as law (law)}<option value={law}>{lawName(law)}</option>{/each}</select></label>
      <label data-field>Camere da rinnovare<select value={scenario.renewal} onchange={e => apply(s => setRenewal(s, e.currentTarget.value as PoliticsScenario['renewal']))}><option value="joint">Camera e Senato</option><option value="camera">Solo Camera</option><option value="senato">Solo Senato</option></select></label>
    </div>
    <div class="subjects"><SubjectsEditor {scenario} {assets} edit={apply} /></div>
    <LazySection title="Impostazioni avanzate dello scenario">
      <div>
        <FieldWithHelp id={referenceDateId} label="Data di riferimento" help="È la data a cui si riferiscono le quote inserite nello scenario e l’ultimo giorno utile per i risultati storici e i sondaggi. Il modello proietta il voto da questa data fino all’elezione. Può coincidere con la data dell’elezione o precederla.">
          <input id={referenceDateId} type="date" value={scenario.referenceDate} onchange={e => apply(s => editScenario(s, n => { n.referenceDate = e.currentTarget.value; }))} />
        </FieldWithHelp>
      </div>
      <label data-field>Generatore dei voti<select aria-label="Generatore dei voti" value={generator.id} onchange={e => { const model = modelRegistry[e.currentTarget.value as GeneratorId]; apply(s => selectGenerator(s, model.id, model.version)); }}>
        {#each generationModels.filter(m => supportedPairs.some(p => p.law === scenario.selection.scrutiny.id && p.generator === m.id)) as model (model.id)}<option value={model.id}>{model.label}</option>{/each}
      </select></label>
      <p>{capabilities(scenario).generator.referenceSd ? 'M02: campo politico condiviso, incertezza dei riferimenti e raggruppamento di componenti. Le differenze fra Camera e Senato restano quelle della storia abbinata e dei riferimenti espliciti; le loro variazioni future non sono stimate.' : 'M01: composizioni storiche e shock di lista. SD e moltiplicatori M02 restano salvati, inattivi.'}</p>
      <ReferencesEditor {scenario} target={null} label="Astensione e voti non validi" {assets} edit={apply} />
      {#if assets}<AdvancedEditor {scenario} {assets} edit={apply} />{/if}
      <LazySection title={`Identità dei candidati · ${scenario.candidates.length}`}>
        {#each scenario.candidates as candidate (candidate.id)}<LazySection title={candidate.name}><CandidateForm {scenario} {candidate} edit={apply} /></LazySection>{/each}
        <LazySection title="Nuovo candidato"><CandidateForm {scenario} edit={apply} /></LazySection>
      </LazySection>
    </LazySection>
  </fieldset>
  <div class="run"><RunControls {scenario} {execution} edit={apply} /></div>
</div>

<style>
  .common-politics-editor { line-height: 1.5; }
  fieldset { min-inline-size: 0; border: 0; padding: 0; margin: 0; }
  .row { display: flex; flex-wrap: wrap; align-items: center; gap: var(--space-2); }
  .row + .row { margin-block-start: var(--space-3); }
  .subjects, .run { margin-block: 1.5rem; }

  @media (width <= 650px) { .row > label { inline-size: 100%; } }
</style>
