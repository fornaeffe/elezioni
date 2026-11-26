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
    'Confirmed elected minimum / requested': 'Eletti confermati medi',
    'Ordinary minimum / requested': 'Eletti ordinari medi',
    'Bonus minimum / requested': 'Eletti premio medi',
    'Special minimum / requested': 'Eletti uninominali speciali medi',
    'TAA minimum / requested': 'Eletti proporzionali TAA medi',
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
      {#if !isReform}<p>
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
    color: var(--color-text-muted);
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
    border-bottom: 1px solid var(--color-border);
    text-align: left;
  }
</style>
