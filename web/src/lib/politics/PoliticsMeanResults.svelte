<script lang="ts">
  import type { StatisticsOverview } from '../results/statistics';
  import PairedBars from '../results/PairedBars.svelte';
  import ResultSection from '../results/ResultSection.svelte';

  let { overview, isReform }: { overview: StatisticsOverview; isReform: boolean } = $props();
  let meanKind = $state('list'),
    meanChamber = $state<'camera' | 'senato'>('camera');
  const meanLists = $derived(overview.series.filter((s) => s.chamber === meanChamber && s.kind === meanKind));
</script>

<ResultSection title="Eletti medi e percentuali sui voti validi" open>
  <p>
    {overview.saved}/{overview.requested} simulazioni conservate. Eletti per affiliazione personale finale; coalizioni
    per titolarità del seggio. Sono inclusi ordinari, premio, uninominali speciali e proporzionali TAA; Estero escluso.
  </p>
  <p>
    Nel caso siano presenti 
    simulazioni dove non è stato possibile completare lo scrutinio, ≥ 
    indica la percentuale minima.
  </p>
  {#if isReform}<p>Identità e recuperi restano condizionati ai dati/modelli delle preferenze.</p>
  {:else}<p>
      Rosatellum: le liste riportano i soli seggi plurinominali; le coalizioni e i sostegni comprendono anche
      gli eletti uninominali.
    </p>{/if}
  {#if overview.legacy}<p role="note">
      Archivio precedente: alcune affiliazioni non sono ricostruibili; i grafici locali richiedono una nuova
      esecuzione. L’apertura dell’archivio non esegue simulazioni.
    </p>{/if}
  <div class="mean-controls">
    <label>
      Raggruppamento
      <select bind:value={meanKind}>
        <option value="list">Liste</option>
        <option value="subject">Coalizioni / liste singole</option>
      </select>
    </label>
    <label>
      Ramo
      <select bind:value={meanChamber}>
        <option value="camera">Camera</option>
        <option value="senato">Senato</option>
      </select>
    </label>
  </div>
  {#if meanLists.length}
    <h4>{meanChamber === 'camera' ? 'Camera' : 'Senato'}</h4>
    <PairedBars
      entityLabel={meanKind === 'list' ? 'Lista' : 'Coalizione / lista singola'}
      rows={meanLists.map((s) => ({
        id: s.id,
        label: s.name,
        color: s.color,
        elected: s.electedKnown === overview.requested ? s.electedSum / overview.requested : null,
        minimum: s.confirmed / overview.requested,
        share: s.votesKnown === overview.requested ? s.voteSum / overview.requested : null,
      }))}
    />
  {:else}<p role="status">Nessuna lista disponibile per questo ramo.</p>{/if}
</ResultSection>

<style>
  p {
    font-size: 0.85rem;
    line-height: 1.5;
    color: #4d5963;
  }
  label {
    display: grid;
    gap: 0.5rem;
    margin: 1rem 0;
  }
  select {
    font: inherit;
    padding: 0.5rem;
  }
  select {
    box-sizing: border-box;
    width: 100%;
    min-width: 0;
  }
  @media (min-width: 768px) {
    .mean-controls {
      display: flex;
      align-items: end;
      gap: 1rem;
    }
    .mean-controls label {
      flex: 0 1 auto;
    }
    .mean-controls select {
      width: auto;
    }
  }
</style>
