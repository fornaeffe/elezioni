<script lang="ts">
  import { untrack } from 'svelte';
  import FieldWithHelp from '$lib/ui/FieldWithHelp.svelte';
  import type { Candidate, PoliticsScenario } from '../scenario/schema';
  import { assumedCandidateBirthDate, createCandidate, updateCandidate, deleteCandidate } from '../scenario/edits';
  import type { ApplyEdit } from './model';
  let { scenario, candidate = null, edit, afterCreate, done = () => {}, closeLabel = 'Chiudi candidato' }: {
    scenario: PoliticsScenario;
    candidate?: Candidate | null;
    edit: ApplyEdit;
    afterCreate?: (scenario: PoliticsScenario, candidateId: string) => PoliticsScenario;
    done?: () => void;
    closeLabel?: string;
  } = $props();
  // This form is keyed by the identity in its parent, so these are intentionally initial form values.
  const initial = untrack(() => candidate);
  const formId = $props.id();
  const birthDateId = `${formId}-birth-date`;
  const availableId = `${formId}-available`;
  const birthDateHelp = $derived(candidate
    ? 'La data serve per calcolare l’età il giorno dell’elezione e per risolvere gli spareggi che dipendono dall’età.'
    : `Se lasci il campo vuoto, il simulatore registra il 1º gennaio ${assumedCandidateBirthDate(scenario.electionDate).slice(0, 4)} come data di nascita presunta. La data viene salvata e usata per calcolare l’età e risolvere gli spareggi che dipendono dall’età.`);
  let name = $state(initial?.name ?? ''), gender = $state(initial?.gender ?? ''), birthDate = $state(initial?.birthDate ?? '');
  let available = $state(initial?.availability.kind !== 'unavailable'), reason = $state(initial?.availability.kind === 'unavailable' ? initial.availability.reason : '');
  let error = $state('');
  function save(e: SubmitEvent) {
    e.preventDefault();
    const availability: Candidate['availability'] = available ? { kind: 'available' } : { kind: 'unavailable', reason };
    const saved = edit(s => {
      if (candidate) return updateCandidate(s, { ...candidate, name, gender: gender as Candidate['gender'], birthDate,
        birthDateOrigin: birthDate === candidate.birthDate ? candidate.birthDateOrigin : 'declared', availability });

      const id = `candidate-${crypto.randomUUID()}`;
      const next = createCandidate(s, { id, name, gender: gender as Candidate['gender'], birthDate, availability });
      return afterCreate ? afterCreate(next, id) : next;
    }, afterCreate ? message => { error = message; } : undefined);
    if (saved) { if (!candidate) { name = ''; gender = ''; birthDate = ''; available = true; reason = ''; } done(); }
  }
</script>
<form onsubmit={save} class="candidate-form">
  <label data-field>Nome candidato<input required bind:value={name} /></label>
  <label data-field>Genere<select aria-label="Genere" required bind:value={gender}><option value="">Seleziona il genere</option><option value="female">Donna</option><option value="male">Uomo</option></select></label>
  <FieldWithHelp id={birthDateId} label="Data di nascita" help={birthDateHelp}>
    <input id={birthDateId} type="date" required={!!candidate} bind:value={birthDate} />
  </FieldWithHelp>
  {#if candidate?.birthDateOrigin === 'assumed'}<p>Data di nascita assunta.</p>{/if}
  <FieldWithHelp id={availableId} label="Disponibile" leading help="Togli la spunta se la persona non è disponibile a candidarsi e indica il motivo. Non sarà selezionabile per nuove candidature. Per simulare, rimuovi o sostituisci le candidature già fissate che sono attive.">
    <input id={availableId} type="checkbox" bind:checked={available} />
  </FieldWithHelp>
  {#if !available}<label data-field>Motivo indisponibilità<input required bind:value={reason} /></label>{/if}
  {#if error}<p class="error" role="alert">{error}</p>{/if}
  <button type="submit">Salva candidato</button><button type="button" onclick={done}>{closeLabel}</button>
  {#if candidate}<button type="button" onclick={() => { edit(s => deleteCandidate(s, candidate.id)); done(); }}>Elimina identità e tutte le candidature</button>{/if}
</form>

<style>
  form { display: flex; flex-wrap: wrap; align-items: center; gap: var(--space-2); }
  p { flex-basis: 100%; margin-block: 0; color: var(--color-text-muted); }
  .error { color: var(--color-error-text); }
  @media (width <= 650px) { label { inline-size: 100%; } }
</style>
