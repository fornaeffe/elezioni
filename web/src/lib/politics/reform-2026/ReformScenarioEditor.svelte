<script lang="ts">
  import { base } from '$app/paths';
  import { onDestroy } from 'svelte';
  import EditorWorker from '../../workers/reform-editor.worker?worker';
  import ReformSubjectsEditor from './ReformSubjectsEditor.svelte';
  import ReformReferencesEditor from './ReformReferencesEditor.svelte';
  import ReformCorrespondenceEditor from './ReformCorrespondenceEditor.svelte';
  import ReformCoverageEditor from './ReformCoverageEditor.svelte';
  import ReformBonusLinksEditor from './ReformBonusLinksEditor.svelte';
  import ReformCandidatesEditor from './ReformCandidatesEditor.svelte';
  import { bundledReformEditorCatalog, bundledReformModels, bundledReformScenario } from './application-defaults';
  import { parseReformScenario, validateReformScenario } from './generation-scenario';
  import { upgradeReformDraft } from './presentation-metadata';
  import { removedDependencies, type EditingScope } from './scenario-editor';
  import type { ReformGenerationScenario } from './generation-types';
  import type { PreviewSelection, ReformEditorPreview } from './editor-preview';
  import './scenario-editor.css';

  let { scenario, onchange, disabled = false }: { scenario: ReformGenerationScenario; onchange: (scenario: ReformGenerationScenario) => void; disabled?: boolean } = $props();
  const catalog = bundledReformEditorCatalog(), models = bundledReformModels(), defaults = bundledReformScenario();
  let scope = $state<EditingScope>('common');
  let chamberEditing = $state(false);
  let error = $state('');
  let pending = $state<ReformGenerationScenario | null>(null);
  let dependencies = $state<string[]>([]);
  let jsonOpen = $state(false);
  let json = $state('');
  let jsonBase = $state('');
  let preview = $state.raw<ReformEditorPreview | null>(null);
  let previewSelection = $state<PreviewSelection | undefined>();
  let previewFingerprint = $state('');
  let previewPage = $state(0);
  let abstentionPage = $state(0);
  let previewBusy = $state(false);
  let correspondencesOpen = $state(false);
  let coverageOpen = $state(false);
  let linksOpen = $state(false);
  let candidatesOpen = $state(false);
  let worker: Worker | undefined;
  const fingerprint = $derived(JSON.stringify(scenario));
  const validation = $derived(validateReformScenario(scenario));
  const blocked = $derived(disabled || previewBusy || pending !== null);
  const previewCurrent = $derived(preview !== null && previewFingerprint === fingerprint);
  const chamberOverrideCount = $derived([...scenario.shares, ...scenario.abstention].filter(r => r.camera !== null || r.senato !== null).length);
  const previewRows = $derived(previewCurrent ? preview!.references.slice(previewPage * 30, (previewPage + 1) * 30) : []);
  const preference = $derived(models.preferences.profiles.find(p => p.id === scenario.preferenceProfileId));
  const chamber = $derived(models.chambers.profiles.find(p => p.id === scenario.chamberProfileId));
  onDestroy(() => worker?.terminate());
  function change(action: () => ReformGenerationScenario) {
    if (blocked) return;
    try {
      error = ''; const next = action();
      dependencies = removedDependencies(scenario, next);
      if (dependencies.length) pending = next;
      else onchange(next);
    } catch (e) { error = e instanceof Error ? e.message : String(e); }
  }
  function setting(changeSetting: (s: ReformGenerationScenario) => void) { change(() => { const n = upgradeReformDraft(scenario); changeSetting(n); return n; }); }
  function runPreview(selection?: PreviewSelection) {
    worker?.terminate(); error = ''; previewBusy = true;
    const captured = fingerprint; previewSelection = selection;
    worker = new EditorWorker();
    worker.onmessage = (event: MessageEvent<{ preview?: ReformEditorPreview; error?: string }>) => {
      previewBusy = false; worker?.terminate(); worker = undefined;
      if (captured !== JSON.stringify(scenario)) return;
      if (event.data.error) { error = event.data.error; preview = null; }
      else { preview = event.data.preview!; previewFingerprint = captured; previewPage = 0; abstentionPage = 0; }
    };
    worker.onerror = e => { previewBusy = false; error = e.message; worker?.terminate(); worker = undefined; };
    worker.postMessage({ base, scenario: JSON.parse(captured), selection: selection ? { ...selection } : undefined });
  }
  function applyJson() {
    try {
      if (jsonBase !== fingerprint) throw new Error('Lo scenario è cambiato dopo l’apertura del JSON. Riapri il JSON corrente prima di applicarlo.');
      const next = upgradeReformDraft(parseReformScenario(json)); change(() => next); jsonOpen = false;
    } catch (e) { error = e instanceof Error ? e.message : String(e); }
  }
  function errorTarget(message: string) {
    return /share|reference|abstention/i.test(message) ? '#r26-references' : /list|coalition|presentation/i.test(message) ? '#r26-subjects' : '#r26-settings';
  }
</script>
<div class="scenario-editor" aria-label="Editor scenario riforma">
  <fieldset id="r26-settings" disabled={blocked}>
    <legend>Impostazioni dello scenario</legend>
    <div class="editor-row">
      <label>Nome scenario<input value={scenario.presentation?.name ?? ''} onchange={e => { const value = e.currentTarget.value; setting(n => { n.presentation!.name = value; }); }} /></label>
      <label>Seed<input value={scenario.seed} oninput={e => { const value = e.currentTarget.value; setting(n => { n.seed = value; }); }} /></label>
      <label>Data proiezione<input type="date" value={scenario.projectionDate} onchange={e => { const value = e.currentTarget.value; setting(n => { n.projectionDate = value; }); }} /></label>
      <label>Data elezione<input type="date" value={scenario.electionDate} onchange={e => { const value = e.currentTarget.value; setting(n => { n.electionDate = value; }); }} /></label>
      <label>Rinnovo<select value={scenario.renewal} onchange={e => { const value = e.currentTarget.value as ReformGenerationScenario['renewal']; setting(n => { n.renewal = value; }); }}><option value="joint">Camera e Senato</option><option value="camera">Solo Camera</option><option value="senato">Solo Senato</option></select></label>
    </div>
    <p>Quote in modalità media: descrivono il riferimento alla data di proiezione e conservano l'incertezza fino all'elezione.</p>
    <details><summary>Profili del modello</summary>
      <div class="editor-row">
        <label>Profilo preferenze<select value={scenario.preferenceProfileId} onchange={e => { const value = e.currentTarget.value; setting(n => { n.preferenceProfileId = value; }); }}>{#each models.preferences.profiles as p (p.id)}<option value={p.id}>{p.id}</option>{/each}</select></label>
        <label>Profilo dipendenza Camera-Senato<select value={scenario.chamberProfileId} onchange={e => { const value = e.currentTarget.value; setting(n => { n.chamberProfileId = value; }); }}>{#each models.chambers.profiles as p (p.id)}<option value={p.id}>{p.id}</option>{/each}</select></label>
      </div>
      {#if preference}<p>Preferenze: intensità ×{preference.intensityMultiplier.toFixed(2)}, concentrazione ×{preference.concentrationMultiplier.toFixed(2)}, persistenza {preference.persistence.toFixed(2)}, peso triple {preference.tripleWeight.toFixed(2)}.</p>{/if}
      {#if chamber}<p>Dipendenza: quota varianza comune {chamber.sharedVariance.toFixed(2)}, correlazione residua {chamber.residualCorrelation.toFixed(2)}, contrasto storico ×{chamber.gapMultiplier.toFixed(2)}.</p>{/if}
      <p>Ipotesi di sensibilità, non intervalli di confidenza calibrati. Gli esiti dei candidati sono condizionati alle preferenze sintetiche.</p>
    </details>
  </fieldset>
  <label><input type="checkbox" bind:checked={chamberEditing} disabled={blocked} onchange={() => { scope = 'common'; }} />Abilita modifiche avanzate per Camera/Senato</label>
  <div class="editor-row" aria-label="Ambito modifica">
    <button type="button" aria-pressed={scope === 'common'} disabled={blocked} onclick={() => { scope = 'common'; }}>Comune ai rami</button>
    {#if chamberEditing}<button type="button" aria-pressed={scope === 'camera'} disabled={blocked} onclick={() => { scope = 'camera'; }}>Camera</button><button type="button" aria-pressed={scope === 'senato'} disabled={blocked} onclick={() => { scope = 'senato'; }}>Senato</button>{/if}
  </div>
  <p>Ambito attivo: <strong>{scope === 'common' ? 'Comune ai rami' : scope === 'camera' ? 'Camera' : 'Senato'}</strong>. Le modifiche comuni aggiornano entrambi i rami. Le quote esplicite per ramo restano prioritarie: {chamberOverrideCount} riferimenti con override per ramo.</p>
  {#if error}<p role="alert" class="editor-error">{error}</p>{/if}
  {#if pending}<div role="alert" class="editor-notice">
    <p>La modifica rimuove anche: {dependencies.join(', ')}. Controlla questi effetti prima di applicarla.</p>
    <button type="button" disabled={disabled} onclick={() => { onchange(pending!); pending = null; }}>Applica modifica e rimuovi dipendenze</button>
    <button type="button" onclick={() => { pending = null; }}>Mantieni scenario</button>
  </div>{/if}
  {#if validation.length}<div role="alert" class="editor-error" data-testid="reform-scenario-validation"><p>Correggi lo scenario prima di eseguire:</p><ul>{#each validation as message (message)}<li><a href={errorTarget(message)}>{message}</a></li>{/each}</ul></div>{/if}
  <ReformSubjectsEditor {scenario} {scope} {catalog} disabled={blocked} onaction={change} />
  <ReformReferencesEditor {scenario} {scope} {catalog} disabled={blocked} onaction={change} />
  <details bind:open={correspondencesOpen}><summary>Corrispondenze storiche</summary>{#if correspondencesOpen}<ReformCorrespondenceEditor {scenario} {scope} {catalog} {defaults} disabled={blocked} onaction={change} />{/if}</details>
  <details bind:open={coverageOpen}><summary>Depositi delle liste</summary>{#if coverageOpen}<ReformCoverageEditor {scenario} {scope} {catalog} disabled={blocked} onaction={change} />{/if}</details>
  <details bind:open={linksOpen}><summary>Collegamenti per il premio</summary>{#if linksOpen}<ReformBonusLinksEditor {scenario} disabled={blocked} onaction={change} />{/if}</details>
  <details bind:open={candidatesOpen}><summary>Candidature e anteprima del premio</summary>{#if candidatesOpen}<ReformCandidatesEditor {scenario} {catalog} disabled={blocked} onaction={change} onpreview={runPreview} preview={previewCurrent ? preview : null} {previewSelection} />{/if}</details>
  <section aria-label="Verifica scenario riforma">
    <h3>Verifica e anteprima</h3>
    <button type="button" disabled={blocked || validation.length > 0} onclick={() => runPreview()}>Verifica scenario e riferimenti</button>
    {#if previewBusy}<p role="status">Verifica in corso nel worker…</p><button type="button" onclick={() => { worker?.terminate(); worker = undefined; previewBusy = false; }}>Annulla verifica</button>{/if}
    {#if preview && !previewCurrent}<p>Scenario modificato: ripeti la verifica per aggiornare l'anteprima.</p>{/if}
    {#if previewCurrent}
      <p role="status">Scenario verificato con un campione di generazione (simulazione 1). Nessuno scrutinio eseguito; altre simulazioni possono incontrare percorsi non computabili.</p>
      {#each preview!.warnings as warning, i (`${warning.code}-${i}`)}<p class="editor-notice">{warning.code}: {warning.message}</p>{/each}
      <div class="editor-table-wrap"><table><caption>Riferimenti risolti — quote sui voti validi</caption><thead><tr><th>Area</th><th>Lista</th><th>Comune</th><th>Camera</th><th>Senato</th></tr></thead><tbody>
        {#each previewRows as row, index (index)}<tr><td>{row.scope.kind === 'national' ? 'Italia' : row.scope.code}</td><td>{[...scenario.lists.camera, ...scenario.lists.senato].find(l => l.id === (row.listIds.camera ?? row.listIds.senato))?.name}</td><td>{row.common ? `${(row.common.share * 100).toFixed(3)}%` : '—'}</td><td>{row.camera ? `${(row.camera.share * 100).toFixed(3)}% (${row.camera.origin})` : '—'}</td><td>{row.senato ? `${(row.senato.share * 100).toFixed(3)}% (${row.senato.origin})` : '—'}</td></tr>{/each}
      </tbody></table></div>
      <button type="button" disabled={previewPage === 0} onclick={() => { previewPage--; }}>Riferimenti precedenti</button><button type="button" disabled={(previewPage + 1) * 30 >= preview!.references.length} onclick={() => { previewPage++; }}>Riferimenti successivi</button>
      <div class="editor-table-wrap"><table><caption>Astensione risolta — media ponderata per elettori dopo i riferimenti locali</caption><thead><tr><th>Area</th><th>Camera</th><th>Senato</th></tr></thead><tbody>
        {#each preview!.abstention.slice(abstentionPage * 30, (abstentionPage + 1) * 30) as row (JSON.stringify(row.scope))}<tr><td>{row.scope.kind === 'national' ? 'Italia' : row.scope.code}</td><td>{row.camera === null ? '—' : `${(row.camera * 100).toFixed(3)}%`}</td><td>{row.senato === null ? '—' : `${(row.senato * 100).toFixed(3)}%`}</td></tr>{/each}
      </tbody></table></div>
      <button type="button" disabled={abstentionPage === 0} onclick={() => { abstentionPage--; }}>Astensioni precedenti</button><button type="button" disabled={(abstentionPage + 1) * 30 >= preview!.abstention.length} onclick={() => { abstentionPage++; }}>Astensioni successive</button>
    {/if}
  </section>
  <details><summary>Scenario completo e dati forniti (JSON)</summary>
    <p>Voti forniti: {scenario.suppliedVotes.length}; preferenze fornite: {scenario.suppliedPreferences.length}. Modificali nel JSON conservando provenienza, identità dei candidati e certificati di validità. I voti Camera TAA si forniscono sui collegi speciali, una sola volta.</p>
    <button type="button" disabled={blocked} onclick={() => { json = JSON.stringify(scenario, null, 2); jsonBase = fingerprint; jsonOpen = true; }}>Modifica scenario completo</button>
    {#if jsonOpen}<label>Scenario completo JSON<textarea rows="18" bind:value={json} disabled={blocked}></textarea></label><button type="button" disabled={blocked} onclick={applyJson}>Applica scenario completo</button>{/if}
  </details>
</div>
