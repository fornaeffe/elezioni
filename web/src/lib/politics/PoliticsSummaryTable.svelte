<script lang="ts">
  import { summaryTable, type StatisticsOverview } from '../results/statistics';
  import { displayNumber } from '../results/chart-builders';
  import ResultSection from '../results/ResultSection.svelte';

  let { overview, isReform }: { overview: StatisticsOverview; isReform: boolean } = $props();
  let summaryPage = $state(0);
  const table = $derived(summaryTable(overview));
  const headings: Record<string, string> = {
    Chamber: 'Ramo',
    Name: 'Lista',
    Requested: 'Richieste',
    'Vote observations': 'Percentuali note',
    'Election observations': 'Esiti noti',
    'Mean vote share %': 'Percentuale media %',
    'Mean elected': 'Eletti medi',
    'Confirmed elected minimum / requested': 'Eletti confermati minimi / richieste',
    'Ordinary minimum / requested': 'Ordinari minimi / richieste',
    'Bonus minimum / requested': 'Premio minimo / richieste',
    'Special minimum / requested': 'Uninominali speciali minimi / richieste',
    'TAA minimum / requested': 'Proporzionali TAA minimi / richieste',
  };
</script>

<ResultSection title="Tabella riepilogativa degli eletti e delle percentuali">
  {#snippet children(open)}
    {#if open}
      <p>
        Richieste: tutte le simulazioni previste, incluse quelle fallite o interrotte. Percentuali note ed
        esiti noti: numero di osservazioni disponibili per la lista e il ramo. Percentuale media ed eletti
        medi sono disponibili solo con tutte le osservazioni note.
      </p>
      <p>
        Le colonne «minimi / richieste» dividono gli eletti confermati per tutte le simulazioni richieste:
        sono medie minime, non percentuali né probabilità. Estero escluso.
      </p>
      {#if isReform}<p>
          Ordinari, premio, uninominali speciali e proporzionali TAA suddividono quel minimo per categoria;
          possono includere recuperi nella sede finale.
        </p>
      {:else}<p>
          Rosatellum: le liste riportano i soli seggi plurinominali; le coalizioni e i sostegni comprendono
          anche gli eletti uninominali.
        </p>{/if}
      <div class="table-scroll">
        <table>
          <caption>Eletti e percentuali medi per lista</caption>
          <thead>
            <tr>
              {#each table.columns.filter((c) => c !== 'List') as column (column)}<th>
                  {headings[column] ?? column}
                </th>{/each}
            </tr>
          </thead>
          <tbody>
            {#each table.rows.slice(summaryPage * 50, summaryPage * 50 + 50) as row (String(row.Chamber) + String(row.List))}<tr
              >
                {#each table.columns.filter((c) => c !== 'List') as column (column)}<td>
                    {typeof row[column] === 'number'
                      ? displayNumber(row[column] as number)
                      : (row[column] ?? 'Non disponibile')}
                  </td>{/each}
              </tr>{/each}
          </tbody>
        </table>
      </div>
      {#if table.rows.length > 50}<button disabled={summaryPage === 0} onclick={() => summaryPage--}>
          Riepilogo precedente
        </button>
        <button disabled={(summaryPage + 1) * 50 >= table.rows.length} onclick={() => summaryPage++}>
          Riepilogo successivo
        </button>{/if}
    {/if}
  {/snippet}
</ResultSection>

<style>
  p {
    font-size: 0.85rem;
    line-height: 1.5;
    color: #4d5963;
  }
  button {
    font: inherit;
    padding: 0.5rem;
  }
  .table-scroll {
    overflow: auto;
  }
  table {
    border-collapse: collapse;
    font-size: 0.8rem;
  }
  th,
  td {
    padding: 0.5rem;
    border-bottom: 1px solid #e5e9ed;
    text-align: left;
  }
</style>
