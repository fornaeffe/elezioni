<script lang="ts">
  import { OTHER_LIST_ID } from '../../core/other-list';
  import { errorMessage } from '../../core/operation-error';
  import type { ResultServiceClient } from '../../core/result-service-client';
  import type { SeriesSummary } from '../../results/statistics';
  import { probabilityCellColors, probabilityLabel, type PositionMetric, type PositionProbabilities } from './election-probability';
  let { client, resultId, owner, series }: { client: ResultServiceClient; resultId: string; owner: string; series: SeriesSummary[] } = $props();
  let chamber = $state('camera'), list = $state('');
  let metric = $state<PositionMetric>('all');
  const lists = $derived(series.filter(s => s.chamber === chamber && s.kind === 'list' && s.owner.startsWith('list:') && s.owner !== `list:${OTHER_LIST_ID}`));
  const selected = $derived(lists.find(s => s.owner === list) ?? lists[0]);
</script>
<div aria-label="Probabilità di elezione nel collegio">
  <label>Ramo delle probabilità<select bind:value={chamber}><option value="camera">Camera</option><option value="senato">Senato</option></select></label>
  <label>Lista delle probabilità<select value={selected?.owner ?? ''} onchange={e => { list = e.currentTarget.value; }}>{#each lists as item (item.id)}<option value={item.owner}>{item.name}</option>{/each}</select></label>
  <label>Metrica delle probabilità<select bind:value={metric}>
    <option value="all">Tutte le simulazioni</option>
    <option value="capolista-bonus">Capolista presente in una lista bonus</option>
    <option value="capolista-no-bonus">Capolista non presente in alcuna lista bonus</option>
  </select></label>
  <p>Ogni cella indica la probabilità che sia eletto da questa lista in questo 
    collegio il candidato in quella posizione oppure almeno un candidato 
    successivo. La prima colonna rappresenta i capolista, le colonne seguenti 
    gli altri sei candidati in ordine di preferenze decrescenti (non, quindi, 
    l'ordine di presentazione sulla scheda). Sono inclusi nella percentuale i 
    candidati eletti perché un ripescaggio li ha eletti 
    ripescandoli da questo collegio. Non sono inclusi nella percentuale i candidati eletti in 
    un altro collegio o nel listino premio.</p>
  {#if metric === 'all'}
    <p>Denominatore: tutte le simulazioni richieste.</p>
  {:else}
    <p>Le simulazioni sono divise in due gruppi, a seconda della presenza del 
      capolista in qualsiasi lista bonus 
      dello stesso ramo, indipendentemente dall’attribuzione del premio. 
      Il denominatore, per ciascun collegio, è il numero di simulazioni del 
      gruppo scelto (capolista candidato anche nel listino premio o capolista 
      non candidato in listini premio). Le simulazioni senza dati della lista non sono incluse: 
      se presenti, la percentuale riguarda solo le simulazioni con gruppo noto.</p>
  {/if}
  <p>Nel caso di esiti mancanti, ≥ indica una percentuale minima; 
    n.d. indica nessun esito noto o denominatore nullo; 
    — indica una posizione assente. 
    Ogni cella riporta gli esiti noti nel tooltip. 
    I risultati dipendono dal modello di generazione casuale dei voti, delle 
    preferenze e delle candidature.</p>
  {#if selected}
    {#await client.call<PositionProbabilities>('statistics-positions', {
      resultId, owner, chamber, list: selected.owner.slice(5), positionMetric: metric,
    })}
      <p role="status">Preparazione delle probabilità…</p>
    {:then data}
      {#if !data.available}<p>Dati delle posizioni non disponibili nell’archivio. È necessaria una nuova esecuzione.</p>
      {:else if !data.rows.length}<p>Nessun collegio con questa lista depositata.</p>
      {:else}
        <div class="table-scroll"><table><caption>Probabilità di elezione in questa posizione o in una successiva — {selected.name}, {chamber} · {data.requested} simulazioni richieste</caption>
          <thead><tr><th scope="col">Collegio</th>{#if metric !== 'all'}<th scope="col">Simulazioni nel gruppo</th><th scope="col">Gruppo non noto</th>{/if}<th scope="col">Capolista</th>{#each [1, 2, 3, 4, 5, 6] as rank (rank)}<th scope="col">{rank}ª preferenza</th>{/each}</tr></thead>
          <tbody>{#each data.rows as row (row.id)}{@const denominator = metric === 'all' ? data.requested : row.denominator}<tr><th scope="row">{row.name}</th>{#if metric !== 'all'}<td>{row.denominator}</td><td>{data.requested - row.observed}</td>{/if}{#each row.positions as cell, i (i)}{@const colors = probabilityCellColors(selected.color, denominator ? cell.wins / denominator : 0)}<td style:background-color={colors.background} style:color={colors.foreground} title={`${cell.wins} simulazioni con elezione confermata in questa posizione o in una successiva; ${cell.known} esiti noti; ${cell.absent} posizioni assenti su ${denominator}`}>{probabilityLabel(cell, denominator)}</td>{/each}</tr>{/each}</tbody>
        </table></div>
      {/if}
    {:catch error}
      <p role="alert">{errorMessage(error)}</p>
    {/await}
  {:else}
    <p>Nessuna lista disponibile per questo ramo.</p>
  {/if}
</div>
<style>
  label { display: grid; gap: .5rem; margin: 1rem 0; }
  select { width: 100%; min-width: 0; }
  p { font-size: .85rem; line-height: 1.5; color: var(--color-text-muted); }
  .table-scroll { overflow: auto; }
  table { border-collapse: collapse; font-size: .8rem; width: 100%; }
  th, td { padding: .5rem; border-bottom: 1px solid var(--color-border); text-align: left; }
</style>
