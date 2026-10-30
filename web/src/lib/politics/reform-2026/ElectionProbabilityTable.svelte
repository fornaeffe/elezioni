<script lang="ts">
  import { OTHER_LIST_ID } from '../../core/other-list';
  import type { ResultServiceClient } from '../../core/result-service-client';
  import type { SeriesSummary } from '../../results/statistics';
  import { probabilityLabel, type PositionProbabilities } from './election-probability';
  let { client, resultId, owner, series }: { client: ResultServiceClient; resultId: string; owner: string; series: SeriesSummary[] } = $props();
  let chamber = $state('camera'), list = $state('');
  const lists = $derived(series.filter(s => s.chamber === chamber && s.kind === 'list' && s.owner.startsWith('list:') && s.owner !== `list:${OTHER_LIST_ID}`));
  const selected = $derived(lists.find(s => s.owner === list) ?? lists[0]);
  let data = $state.raw<PositionProbabilities | null>(null), error = $state('');
  $effect(() => {
    const current = selected;
    data = null; error = '';
    if (!current) return;
    let active = true;
    client.call<PositionProbabilities>('statistics-positions', { resultId, owner, chamber, list: current.owner.slice(5) }).then(value => { if (active) data = value; }).catch(e => { if (active) error = String(e); });
    return () => { active = false; };
  });
</script>
<div aria-label="Probabilità di elezione nel collegio">
  <label>Ramo delle probabilità<select bind:value={chamber}><option value="camera">Camera</option><option value="senato">Senato</option></select></label>
  <label>Lista delle probabilità<select value={selected?.owner ?? ''} onchange={e => { list = e.currentTarget.value; }}>{#each lists as item (item.id)}<option value={item.owner}>{item.name}</option>{/each}</select></label>
  <p>Ogni cella indica la probabilità che sia eletto da questa lista in questo collegio il candidato in quella posizione oppure almeno un candidato successivo. Ordine: capolista, poi sei candidati per preferenze decrescenti in ciascuna simulazione. Sono inclusi i recuperi nella sede finale, senza richiedere un rapporto di causa tra le elezioni. Un’elezione altrove, da sola, non conta. Non è una probabilità individuale per una persona fissa.</p>
  <p>Denominatore: tutte le simulazioni richieste. ≥ indica un minimo confermato con esiti mancanti; n.d. indica nessun esito noto; — indica una posizione assente. Ogni cella riporta gli esiti noti nel suggerimento. Risultati condizionati ai modelli e ai profili delle preferenze.</p>
  {#if error}<p role="alert">{error}</p>
  {:else if !selected}<p>Nessuna lista disponibile per questo ramo.</p>
  {:else if !data}<p role="status">Preparazione delle probabilità…</p>
  {:else if !data.available}<p>Dati delle posizioni non disponibili nell’archivio. È necessaria una nuova esecuzione.</p>
  {:else if !data.rows.length}<p>Nessun collegio con questa lista depositata.</p>
  {:else}
    <div class="table-scroll"><table><caption>Probabilità di elezione in questa posizione o in una successiva — {selected.name}, {chamber} · {data.requested} simulazioni richieste</caption>
      <thead><tr><th scope="col">Collegio</th><th scope="col">Capolista</th>{#each [1, 2, 3, 4, 5, 6] as rank (rank)}<th scope="col">{rank}ª preferenza</th>{/each}</tr></thead>
      <tbody>{#each data.rows as row (row.id)}<tr><th scope="row">{row.name}</th>{#each row.positions as cell, i (i)}<td title={`${cell.wins} simulazioni con elezione confermata in questa posizione o in una successiva; ${cell.known} esiti noti; ${cell.absent} posizioni assenti su ${data.requested}`}>{probabilityLabel(cell, data.requested)}</td>{/each}</tr>{/each}</tbody>
    </table></div>
  {/if}
</div>
<style>
  label { display: grid; gap: .5rem; margin: 1rem 0; }
  select { box-sizing: border-box; width: 100%; min-width: 0; padding: .5rem; font: inherit; }
  p { font-size: .85rem; line-height: 1.5; color: #4d5963; }
  .table-scroll { overflow: auto; }
  table { border-collapse: collapse; font-size: .8rem; width: 100%; }
  th, td { padding: .5rem; border-bottom: 1px solid #e5e9ed; text-align: left; }
</style>
