<script lang="ts">
  import SearchSelect from '../../scenario/SearchSelect.svelte';
  import type { Candidate, Chamber } from './input';
  import type { ReformCandidateTemplate, ReformGenerationScenario } from './generation-types';
  import type { ReformEditorCatalog } from './editor-catalog';
  import type { PreviewSelection, ReformEditorPreview } from './editor-preview';
  import { newReformCandidate, reformSubjects, saveReformCandidate, subjectLabel } from './scenario-editor';
  import { upgradeReformDraft, cloneReformValue } from './presentation-metadata';
  let { scenario, catalog, disabled, onaction, onpreview, preview, previewSelection }: {
    scenario: ReformGenerationScenario; catalog: ReformEditorCatalog; disabled: boolean;
    onaction: (action: () => ReformGenerationScenario) => void; onpreview: (selection: PreviewSelection) => void;
    preview: ReformEditorPreview | null; previewSelection: PreviewSelection | undefined;
  } = $props();
  let chamber = $state<Chamber>('camera');
  let role = $state<ReformCandidateTemplate['role']>('ordinary');
  let locationId = $state('');
  let ownerId = $state('');
  let position = $state<number | undefined>(1);
  let affiliation = $state('');
  let person = $state<Candidate>({ id: '', name: '', birthDate: '', gender: 'female', availability: { kind: 'available' }, externalCandidacies: [] });
  let page = $state(0);
  const places = $derived(role === 'bonus' ? catalog.territories.filter(t => t.chamber === chamber && t.kind === 'ordinary').map(t => ({ id: t.id, label: t.name }))
    : catalog.colleges.filter(c => c.chamber === chamber && (role === 'special' ? c.kind === 'special-uninominal' : c.kind !== 'special-uninominal')).map(c => ({ id: c.id, label: c.name })));
  const owners = $derived(role === 'ordinary' ? scenario.lists[chamber].filter(l => scenario.coverage.some(c => c.chamber === chamber && c.collegeId === locationId && c.listIds.includes(l.id))).map(l => ({ id: l.id, label: l.name }))
    : reformSubjects(scenario, chamber).filter(sub => scenario.coverage.some(c => c.chamber === chamber && (role === 'special' ? c.collegeId === locationId : catalog.colleges.find(college => college.id === c.collegeId)?.territoryId === locationId)
      && c.listIds.some(id => { const l = scenario.lists[chamber].find(l => l.id === id); return l && (l.coalitionId ?? l.id) === sub.id; }))).map(sub => ({ id: sub.id, label: subjectLabel(scenario, sub, chamber) })));
  const selection = $derived<PreviewSelection>({ chamber, role, locationId, ownerId });
  const placements = $derived(JSON.stringify(selection) === JSON.stringify(previewSelection) ? preview?.placements ?? [] : []);
  const people = $derived([...new Map(scenario.candidateTemplates.map(t => [t.candidate.id, t.candidate])).values()]);
  const templates = $derived(scenario.candidateTemplates.filter(t => t.chamber === chamber && t.role === role));
  const safePage = $derived(Math.min(page, Math.max(0, Math.ceil(templates.length / 20) - 1)));
  function fresh() { person = newReformCandidate(scenario); }
  function edit(t: ReformCandidateTemplate) {
    chamber = t.chamber; role = t.role; locationId = t.locationId; ownerId = t.ownerId; position = t.position;
    affiliation = t.affiliationListId ?? ''; person = cloneReformValue(t.candidate);
  }
  function save() { onaction(() => {
    if (!locationId || !owners.some(o => o.id === ownerId) || !person.id || !person.name.trim() || !person.birthDate || !position) throw new Error('Completa posizione, persona, nome e data di nascita.');
    if (person.id.startsWith('synthetic:')) throw new Error('Usa una nuova identità stabile per fissare un candidato sintetico.');
    return saveReformCandidate(scenario, { chamber, role, locationId, ownerId, position: role === 'special' ? 1 : position,
      affiliationListId: role === 'ordinary' ? ownerId : affiliation || null, candidate: person });
  }); }
  function external(ch: Chamber, kind: 'domestic' | 'foreign', checked: boolean) {
    person = { ...person, externalCandidacies: [...person.externalCandidacies.filter(c => c.chamber !== ch || c.kind !== kind), ...(checked ? [{ chamber: ch, kind }] : [])] };
  }
</script>
<section aria-label="Candidature riforma">
  <h3>Candidature e planner del premio</h3>
  <p>I collegi e le posizioni sono propri di ciascun ramo. I template fissano persone; le posizioni rimanenti sono generate. La stessa identità mantiene gli stessi dati in tutte le sue candidature.</p>
  {#if scenario.candidateGeneration.bonusPolicy}
    <p>Le quote di candidati del premio seguono il sostegno stimato alla data di proiezione e restano uguali in tutte le simulazioni. I collegi sono ordinati casualmente per ogni simulazione. Un candidato ordinario fornito resta escluso dal premio finché non viene inserito anche come candidato del premio con la stessa identità.</p>
  {:else}
    <p>Questo scenario conserva la selezione originaria del premio.</p>
    <button type="button" {disabled} onclick={() => onaction(() => { const n = upgradeReformDraft(scenario); n.candidateGeneration.bonusPolicy = 'reference-proportional-v1'; return n; })}>Aggiorna selezione del premio</button>
  {/if}
  <div class="editor-row">
    <label>Probabilità riuso candidati<input type="number" min="0" max="1" step="0.01" value={scenario.candidateGeneration.reuseProbability} {disabled} onchange={e => { const value = e.currentTarget.valueAsNumber; onaction(() => { const n = upgradeReformDraft(scenario); n.candidateGeneration.reuseProbability = value; return n; }); }} /></label>
    <label>Massimo candidature ordinarie<select value={scenario.candidateGeneration.maximumOrdinaryCandidacies} {disabled} onchange={e => { const value = Number(e.currentTarget.value); onaction(() => { const n = upgradeReformDraft(scenario); n.candidateGeneration.maximumOrdinaryCandidacies = value; return n; }); }}>{#each [1, 2, 3, 4, 5] as value (value)}<option {value}>{value}</option>{/each}</select></label>
  </div>
  <div class="editor-row">
    <label>Ramo candidature<select bind:value={chamber} {disabled} onchange={() => { locationId = ''; ownerId = ''; page = 0; }}><option value="camera">Camera</option><option value="senato">Senato</option></select></label>
    <label>Ruolo candidatura<select bind:value={role} {disabled} onchange={() => { locationId = ''; ownerId = ''; position = 1; page = 0; }}><option value="ordinary">Ordinaria</option><option value="bonus">Premio</option><option value="special">Uninominale speciale</option></select></label>
  </div>
  {#if role === 'bonus'}<p>I candidati forniti occupano le posizioni indicate. Il planner completa il premio rispettando le candidature ordinarie, i prefissi e i vincoli di genere. Le quote possono adattarsi ai candidati forniti; non sono diritti ai seggi delle liste coalizzate.</p>{/if}
  <div class="editor-row">
    <SearchSelect label="collegio o territorio candidatura" options={places} value={locationId} onchange={v => { locationId = v; ownerId = ''; }} {disabled} />
    <label>Titolare candidatura<select bind:value={ownerId} {disabled}><option value="">Seleziona</option>{#each owners as owner (owner.id)}<option value={owner.id}>{owner.label}</option>{/each}</select></label>
    <button type="button" disabled={disabled || !locationId || !ownerId} onclick={() => onpreview(selection)}>Anteprima posizioni</button>
  </div>
  <p>L'anteprima usa la simulazione 1 e il seed corrente. Le persone sintetiche possono cambiare tra simulazioni; per fissarle crea un'identità stabile. Non è un risultato di scrutinio.</p>
  {#if placements.length}
    <div class="editor-table-wrap"><table><caption>Posizioni pianificate</caption><thead><tr><th>Posizione</th><th>Persona</th><th>Genere</th><th>Lista</th><th>Prefisso ordinario</th><th>Azione</th></tr></thead><tbody>
      {#each placements as p (p.position)}<tr><td>{p.position}</td><td>{p.candidateId.startsWith('synthetic:') ? 'Candidato sintetico' : p.name}</td><td>{p.gender === 'female' ? 'F' : 'M'}</td><td>{scenario.lists[chamber].find(l => l.id === p.listId)?.name ?? 'Indipendente'}</td><td>{p.ordinaryLocation ? `${p.ordinaryLocation} / ${p.ordinaryPosition}` : '—'}</td><td><button type="button" {disabled} onclick={() => {
        const existing = scenario.candidateTemplates.find(t => t.chamber === chamber && t.role === role && t.locationId === locationId && t.ownerId === ownerId && t.position === p.position);
        if (existing) edit(existing); else { fresh(); person = { ...person, gender: p.gender as Candidate['gender'] }; position = p.position; affiliation = p.listId ?? ''; }
      }}>Usa posizione {p.position}</button></td></tr>{/each}
    </tbody></table></div>
  {/if}
  <fieldset {disabled}><legend>Persona e posizione</legend>
    <div class="editor-row">
      <button type="button" onclick={fresh}>Nuova persona</button>
      <label>Persona esistente<select value={people.some(p => p.id === person.id) ? person.id : ''} onchange={e => { const p = people.find(p => p.id === e.currentTarget.value); if (p) person = cloneReformValue(p); }}><option value="">Seleziona</option>{#each people as p (p.id)}<option value={p.id}>{p.name} ({p.id})</option>{/each}</select></label>
      <label>Identità persona<input value={person.id} readonly /></label>
      <label>Nome candidato<input bind:value={person.name} /></label>
      <label>Data nascita<input type="date" bind:value={person.birthDate} /></label>
      <label>Genere candidato<select bind:value={person.gender}><option value="female">Femminile</option><option value="male">Maschile</option></select></label>
      <label>Posizione candidatura<input type="number" min="1" max={role === 'ordinary' ? 7 : role === 'special' ? 1 : undefined} bind:value={position} /></label>
      {#if role !== 'ordinary'}<label>Affiliazione candidatura<select bind:value={affiliation}><option value="">{role === 'special' ? 'Indipendente' : 'Automatica'}</option>{#each scenario.lists[chamber] as list (list.id)}<option value={list.id}>{list.name}</option>{/each}</select></label>{/if}
    </div>
    <label><input type="checkbox" checked={person.availability.kind === 'unavailable'} onchange={e => { person = { ...person, availability: e.currentTarget.checked ? { kind: 'unavailable', reason: '' } : { kind: 'available' } }; }} />Indisponibile</label>
    {#if person.availability.kind === 'unavailable'}<label>Motivo indisponibilità<input value={person.availability.reason} onchange={e => { person = { ...person, availability: { kind: 'unavailable', reason: e.currentTarget.value } }; }} /></label>{/if}
    <details><summary>Altre candidature dichiarate</summary>
      {#each ['camera', 'senato'] as ch (ch)}{#each ['domestic', 'foreign'] as kind (kind)}<label><input type="checkbox" checked={person.externalCandidacies.some(c => c.chamber === ch && c.kind === kind)} onchange={e => external(ch as Chamber, kind as 'domestic' | 'foreign', e.currentTarget.checked)} />{ch} — {kind === 'domestic' ? 'Italia' : 'Estero'}</label>{/each}{/each}
    </details>
    <p>Salvare una persona esistente aggiorna tutti i suoi template. Le incompatibilità di genere, età, prefisso e pluricandidatura vengono controllate anche nella verifica in worker.</p>
    <button type="button" onclick={save}>Salva template candidato</button>
  </fieldset>
  <h4>Template salvati ({templates.length})</h4>
  {#each templates.slice(safePage * 20, (safePage + 1) * 20) as t (JSON.stringify([t.locationId, t.ownerId, t.position]))}<div class="editor-row"><span>{t.candidate.name} — {t.locationId} / {t.position}</span>
    <button type="button" {disabled} onclick={() => edit(t)}>Modifica {t.candidate.name}</button>
    <button type="button" {disabled} onclick={() => onaction(() => { const n = upgradeReformDraft(scenario); n.candidateTemplates = n.candidateTemplates.filter(v => !(v.chamber === t.chamber && v.role === t.role && v.locationId === t.locationId && v.ownerId === t.ownerId && v.position === t.position)); return n; })}>Rimuovi {t.candidate.name}</button>
  </div>{/each}
  <button type="button" disabled={safePage === 0} onclick={() => { page = safePage - 1; }}>Template precedenti</button><span>{safePage + 1} / {Math.max(1, Math.ceil(templates.length / 20))}</span><button type="button" disabled={(safePage + 1) * 20 >= templates.length} onclick={() => { page = safePage + 1; }}>Template successivi</button>
</section>
