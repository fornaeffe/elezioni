<script lang="ts">
  import { untrack } from 'svelte';
  import type { Candidate, PoliticsScenario } from '../scenario/schema';
  import { createCandidate, updateCandidate, deleteCandidate } from '../scenario/edits';
  import type { ApplyEdit } from './model';
  let { scenario, candidate = null, edit, done = () => {} }: { scenario: PoliticsScenario; candidate?: Candidate | null; edit: ApplyEdit; done?: () => void } = $props();
  // This form is keyed by the identity in its parent, so these are intentionally initial form values.
  const initial = untrack(() => candidate);
  let name = $state(initial?.name ?? ''), gender = $state(initial?.gender ?? ''), birthDate = $state(initial?.birthDate ?? '');
  let available = $state(initial?.availability.kind !== 'unavailable'), reason = $state(initial?.availability.kind === 'unavailable' ? initial.availability.reason : '');
  function save(e: SubmitEvent) {
    e.preventDefault();
    const availability: Candidate['availability'] = available ? { kind: 'available' } : { kind: 'unavailable', reason };
    const saved = edit(s => candidate ? updateCandidate(s, { ...candidate, name, gender: gender as Candidate['gender'], birthDate,
      birthDateOrigin: birthDate === candidate.birthDate ? candidate.birthDateOrigin : 'declared', availability }) :
      createCandidate(s, { id: `candidate-${crypto.randomUUID()}`, name, gender: gender as Candidate['gender'], birthDate, availability }));
    if (saved) { if (!candidate) { name = ''; gender = ''; birthDate = ''; available = true; reason = ''; } done(); }
  }
</script>
<form onsubmit={save} class="candidate-form">
  <label>Nome candidato<input required bind:value={name} /></label>
  <label>Genere<select aria-label="Genere" required bind:value={gender}><option value="">Seleziona il genere</option><option value="female">Donna</option><option value="male">Uomo</option></select></label>
  <label>Data di nascita<input type="date" required={!!candidate} bind:value={birthDate} /></label>
  <p>Se vuota alla creazione: 1 gennaio {Number(scenario.electionDate.slice(0, 4)) - 50}. La data assunta influenza età e spareggi e resta salvata.</p>
  {#if candidate?.birthDateOrigin === 'assumed'}<p>Data di nascita assunta.</p>{/if}
  <label><input type="checkbox" bind:checked={available} />Disponibile</label>
  {#if !available}<label>Motivo indisponibilità<input required bind:value={reason} /></label>{/if}
  <button type="submit">Salva candidato</button><button type="button" onclick={done}>Chiudi candidato</button>
  {#if candidate}<button type="button" onclick={() => { edit(s => deleteCandidate(s, candidate.id)); done(); }}>Elimina identità e tutte le candidature</button>{/if}
</form>
