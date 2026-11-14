<script lang="ts">
  import { onMount } from 'svelte';
  import { base } from '$app/paths';
  import { type PoliticsScenario, type GeneratorId, type LawId, laws, REFORM } from '../scenario/schema';
  import { activeGenerator, capabilities, supportedPairs } from '../scenario/registry';
  import { selectLaw, selectGenerator, setRenewal } from '../scenario/edits';
  import { createDefaultCommonPoliticsScenario } from '../scenario/defaults';
  import { encodeScenario, decodeScenario } from '../scenario/serialization';
  import { parsePoliticsScenario, validateActiveScenario } from '../scenario/validation';
  import { editScenario, type Edit } from './model';
  import { loadEditorAssets, type EditorAssets } from './assets';
  import SubjectsEditor from './SubjectsEditor.svelte';
  import ReferencesEditor from './ReferencesEditor.svelte';
  import AdvancedEditor from './AdvancedEditor.svelte';
  import LazySection from './LazySection.svelte';
  import RunControls, { type ExecutionControls } from './RunControls.svelte';
  import CandidateForm from './CandidateForm.svelte';
  import './editor.css';
  let { scenario, onchange, execution }: { scenario: PoliticsScenario; onchange: (next: PoliticsScenario) => void; execution?: ExecutionControls } = $props();
  let assets = $state.raw<EditorAssets | null>(null), error = $state(''), status = $state('Caricamento del modello selezionato…');
  let controller: AbortController | null = null, importController: AbortController | null = null, disposed = false, epoch = 0, editRevision = 0;
  let generator = $derived(activeGenerator(scenario));
  let issues = $derived(assets ? validateActiveScenario(scenario, assets.context) : []);
  const lawName = (law: LawId) => law === REFORM ? 'Riforma 2026' : 'Rosatellum';
  async function load(next: PoliticsScenario) {
    controller?.abort(); controller = new AbortController();
    const revision = ++epoch;
    assets = null; status = `Caricamento ${activeGenerator(next).id} · ${lawName(next.selection.scrutiny.id)}…`;
    try {
      const loaded = await loadEditorAssets(next, controller.signal);
      parsePoliticsScenario(next, loaded.context);
      if (disposed || revision !== epoch) return;
      assets = loaded; status = `${activeGenerator(next).id} · dati verificati`;
    } catch (e) {
      if (disposed || revision !== epoch || controller.signal.aborted) return;
      status = 'Caricamento non riuscito'; error = e instanceof Error ? e.message : String(e);
    }
  }
  function apply(edit: Edit) {
    if (execution?.busy) return false;
    try {
      const before = scenario;
      const next = parsePoliticsScenario(edit(scenario), assets?.context);
      onchange(next); ++editRevision; importController?.abort(); error = '';
      if (before.selection.scrutiny.id !== next.selection.scrutiny.id || activeGenerator(before).id !== activeGenerator(next).id || JSON.stringify(before.data) !== JSON.stringify(next.data)) void load(next);
      return true;
    } catch (e) { error = `Modifica non applicata: ${e instanceof Error ? e.message : String(e)}`; return false; }
  }
  async function importFile(file: File) {
    const revision = editRevision;
    importController?.abort();
    const staged = new AbortController(); importController = staged;
    try {
      const next = decodeScenario(new Uint8Array(await file.arrayBuffer()));
      // Validate every exact dependency before replacing either draft or UI.
      const loaded = await loadEditorAssets(next, staged.signal);
      parsePoliticsScenario(next, loaded.context);
      if (disposed || staged.signal.aborted || execution?.busy || revision !== editRevision) return;
      onchange(next); ++editRevision; controller?.abort(); ++epoch; assets = loaded; error = ''; status = `${activeGenerator(next).id} · dati verificati`;
    } catch (e) { if (!disposed && !staged.signal.aborted) error = `Scenario non caricato. ${e instanceof Error ? e.message : String(e)}`; }
  }
  function download(gzip: boolean) {
    try { const url = URL.createObjectURL(new Blob([encodeScenario(scenario, gzip)])); const a = document.createElement('a'); a.href = url; a.download = `scenario-politiche.json${gzip ? '.gz' : ''}`; a.click(); setTimeout(() => URL.revokeObjectURL(url), 1000); }
    catch (e) { error = String(e); }
  }
  onMount(() => { void load(scenario); return () => { disposed = true; ++epoch; controller?.abort(); importController?.abort(); }; });
</script>
<div class="common-politics-editor">
  <nav aria-label="Percorso"><a href={`${base}/`}>Home</a> / <a href={`${base}/politics`}>Elezioni politiche</a> / Editor comune</nav>
  <h1>Scenario delle elezioni politiche</h1>
  <aside class="editor-status"><details><summary>{execution?.status ?? status}</summary><p>{execution?.progress ?? status}</p>{#if execution?.busy}<button onclick={execution.onCancel}>Annulla esecuzione</button>{:else if !assets}<button onclick={() => { controller?.abort(); ++epoch; status = 'Caricamento annullato'; }}>Annulla caricamento</button><button onclick={() => void load(scenario)}>Riprova caricamento</button>{/if}</details></aside>
  {#if error}<p role="alert">{error}</p>{/if}
  {#if issues.length}<LazySection title={`Verifica dello scenario · ${issues.length} problemi da correggere`}><ul>{#each issues as issue, i (`${issue.code}:${issue.path}:${i}`)}<li>{issue.code}: {issue.message}</li>{/each}</ul></LazySection>{/if}
  <fieldset disabled={execution?.busy}>
    <div class="row">
      <label>Carica scenario<input type="file" accept=".json,.gz" onchange={e => { const file = e.currentTarget.files?.[0]; if (file) void importFile(file); e.currentTarget.value = ''; }} /></label>
      <button onclick={() => download(false)}>Scarica scenario JSON</button><button onclick={() => download(true)}>Scarica scenario JSON.gz</button>
      <button onclick={() => { if (confirm('Ripristinare lo scenario predefinito? Le modifiche della bozza saranno sostituite.')) apply(() => createDefaultCommonPoliticsScenario()); }}>Ripristina scenario predefinito</button>
    </div>
    <div class="row">
      <label>Nome scenario<input value={scenario.name} onchange={e => apply(s => editScenario(s, n => { n.name = e.currentTarget.value; }))} /></label>
      <label>Data elezione<input type="date" value={scenario.electionDate} onchange={e => apply(s => editScenario(s, n => { n.electionDate = e.currentTarget.value; }))} /></label>
      <label>Data di riferimento<input type="date" value={scenario.referenceDate} onchange={e => apply(s => editScenario(s, n => { n.referenceDate = e.currentTarget.value; }))} /></label>
      <label>Algoritmo di scrutinio<select value={scenario.selection.scrutiny.id} onchange={e => apply(s => selectLaw(s, e.currentTarget.value as LawId))}>{#each laws as law (law)}<option value={law}>{lawName(law)}</option>{/each}</select></label>
      <label>Camere da rinnovare<select value={scenario.renewal} onchange={e => apply(s => setRenewal(s, e.currentTarget.value as PoliticsScenario['renewal']))}><option value="joint">Camera e Senato</option><option value="camera">Solo Camera</option><option value="senato">Solo Senato</option></select></label>
    </div>
    <SubjectsEditor {scenario} {assets} edit={apply} />
    <section aria-label="Non valido e astensione"><h2>Non valido / astensione</h2><ReferencesEditor {scenario} target={null} label="Non valido / astensione" {assets} edit={apply} /></section>
    <LazySection title="Impostazioni avanzate dello scenario">
      <label>Generatore dei voti<select aria-label="Generatore dei voti" value={generator.id} onchange={e => apply(s => selectGenerator(s, e.currentTarget.value as GeneratorId, '2.0.0'))}>
        {#each ['M01', 'M02'] as model (model)}<option value={model} disabled={!supportedPairs.some(p => p.law === scenario.selection.scrutiny.id && p.generator === model)}>{model} · 2.0.0{!supportedPairs.some(p => p.law === scenario.selection.scrutiny.id && p.generator === model) ? ' — non disponibile con questa legge' : ''}</option>{/each}
      </select></label>
      <p>{capabilities(scenario).generator.referenceSd ? 'M02: incertezza dei riferimenti e raggruppamento di componenti.' : 'M01: composizioni storiche e shock di lista. SD e moltiplicatori M02 restano salvati, inattivi.'}</p>
      {#if assets}<AdvancedEditor {scenario} {assets} edit={apply} />{/if}
      <LazySection title={`Identità dei candidati · ${scenario.candidates.length}`}>
        {#each scenario.candidates as candidate (candidate.id)}<LazySection title={candidate.name}><CandidateForm {scenario} {candidate} edit={apply} /></LazySection>{/each}
        <LazySection title="Nuovo candidato"><CandidateForm {scenario} edit={apply} /></LazySection>
      </LazySection>
    </LazySection>
  </fieldset>
  <RunControls {scenario} {execution} edit={apply} />
</div>
