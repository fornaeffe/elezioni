<script lang="ts">
  import { onMount } from 'svelte';
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { StatisticsOverview } from '../results/statistics';
  import type { BonusPairProbability } from './reform-2026/stored-probabilities';
  import type { ReformSeatEndowments } from './reform-2026/stored-statistics';
  import { probabilityLabel } from './reform-2026/election-probability';
  import ResultSection from '../results/ResultSection.svelte';
  import PoliticsMeanResults from './PoliticsMeanResults.svelte';
  import PoliticsSummaryTable from './PoliticsSummaryTable.svelte';
  import PoliticsVoteBoxplots from './PoliticsVoteBoxplots.svelte';
  import PoliticsSeriesSection from './PoliticsSeriesSection.svelte';
  import ReformElectionProbabilitySection from './reform-2026/ReformElectionProbabilitySection.svelte';
  import ReformTerritorialSpinogram from './reform-2026/ReformTerritorialSpinogram.svelte';

  let {
    client,
    resultId,
    owner,
    scrutinyId,
  }: {
    client: ResultServiceClient;
    resultId: string;
    owner: string;
    scrutinyId: string;
  } = $props();
  const isReform = $derived(scrutinyId === 'politiche-reform-2026-v1');
  let overview = $state.raw<StatisticsOverview | null>(null);
  let endowments = $state.raw<ReformSeatEndowments | null>(null);
  let bonus = $state.raw<BonusPairProbability[]>([]);
  let error = $state(''),
    bonusError = $state('');

  onMount(() => {
    let alive = true;
    async function load() {
      try {
        const value = await client.call<StatisticsOverview>('statistics', { resultId, owner });
        if (!alive) return;
        overview = value;
        const seats = await client.call<ReformSeatEndowments>('statistics-endowments', { resultId, owner });
        if (!alive) return;
        endowments = seats;
        if (isReform) {
          try {
            const winners = await client.call<BonusPairProbability[]>('statistics-bonus', {
              resultId,
              owner,
            });
            if (alive) bonus = winners;
          } catch (e) {
            if (alive) bonusError = String(e);
          }
        }
      } catch (e) {
        if (alive) error = String(e);
      }
    }
    void load();
    return () => {
      alive = false;
    };
  });
</script>

<div class="reform-statistics" aria-label="Statistiche riforma">
  {#if error}<p role="alert">{error}</p>{/if}
  {#if overview}
    {@const o = overview}
    {#if isReform}<ResultSection title="Vincitori del premio" open>
        <p>
          Percentuale delle simulazioni richieste in cui il premio congiunto è 
          assegnato alla coppia collegata
          Camera–Senato, indipendentemente dai seggi poi coperti. Nel caso siano presenti 
          simulazioni dove non è stato possibile completare lo scrutinio, ≥ 
          indica la percentuale minima.
        </p>
        {#if bonusError}<p role="alert">Impossibile caricare i vincitori del premio: {bonusError}</p>
        {:else}<div class="bonus-bars">
            {#each bonus as row (row.id)}<div class="bonus-row">
                <span>{row.name}</span>
                <span class="bonus-track">
                  <span
                    style:width={`${(100 * row.wins) / o.requested}%`}
                    style:background={row.color}
                  ></span>
                </span>
                <strong>{probabilityLabel(row, o.requested)}</strong>
              </div>{:else}<p>Nessuna assegnazione del premio disponibile.</p>{/each}
          </div>{/if}
      </ResultSection>{/if}
    <PoliticsMeanResults {overview} {isReform} />
    {#if endowments && (endowments.colleges.length || endowments.bonusSlates.length)}
      <ResultSection title="Dotazioni dei seggi per collegio e liste del premio">
        {#if endowments.colleges.length}
          <h4>Seggi per collegio</h4>
          <div class="table-scroll">
            <table>
              <caption>Dotazione di ogni collegio, con e senza premio</caption>
              <thead>
                <tr>
                  <th>Ramo</th>
                  <th>Circoscrizione</th>
                  <th>Collegio</th>
                  <th>Tipo</th>
                  <th>Senza premio</th>
                  <th>Con premio</th>
                </tr>
              </thead>
              <tbody>
                {#each endowments.colleges as row (`${row.chamber}:${row.college}`)}
                  <tr>
                    <td>{row.chamber === 'camera' ? 'Camera' : 'Senato'}</td>
                    <td>{row.territoryName}</td>
                    <td>{row.collegeName}</td>
                    <td>{row.kind}</td>
                    <td>{row.seatsWithoutBonus}</td>
                    <td>{row.seatsWithBonus}</td>
                  </tr>
                {/each}
              </tbody>
            </table>
          </div>
        {/if}
        {#if endowments.bonusSlates.length}
          <h4>Seggi delle liste del premio</h4>
          <div class="table-scroll">
            <table>
              <caption>Dotazione della lista del premio per circoscrizione</caption>
              <thead>
                <tr>
                  <th>Ramo</th>
                  <th>Circoscrizione</th>
                  <th>Seggi della lista del premio</th>
                </tr>
              </thead>
              <tbody>
                {#each endowments.bonusSlates as row (`${row.chamber}:${row.territory}`)}
                  <tr>
                    <td>{row.chamber === 'camera' ? 'Camera' : 'Senato'}</td>
                    <td>{row.territoryName}</td>
                    <td>{row.seats}</td>
                  </tr>
                {/each}
              </tbody>
            </table>
          </div>
        {/if}
      </ResultSection>
    {/if}
    <PoliticsSummaryTable {overview} {isReform} />
    <PoliticsVoteBoxplots {client} {resultId} {owner} {overview} />
    {#if isReform}<ReformElectionProbabilitySection
        {client}
        {resultId}
        {owner}
        series={overview.series}
      />{/if}
    <PoliticsSeriesSection {client} {resultId} {owner} series={overview.series} variant="scatter" />
    <PoliticsSeriesSection {client} {resultId} {owner} series={overview.series} variant="spine" />
    {#if isReform}<ReformTerritorialSpinogram {client} {resultId} {owner} {overview} />{/if}
  {:else if !error}<p role="status">Preparazione delle statistiche…</p>{/if}
</div>

<style>
  .reform-statistics {
    margin: 1.5rem 0;
  }
  p {
    font-size: 0.85rem;
    line-height: 1.5;
    color: var(--color-text-muted);
  }
  .bonus-bars {
    display: grid;
    gap: 0.6rem;
    margin: 1rem 0;
    font-size: 0.85rem;
  }
  .bonus-row {
    display: grid;
    grid-template-columns: minmax(5rem, 1fr) 2fr 5rem;
    gap: 0.5rem;
    align-items: center;
  }
  .bonus-track {
    background: var(--color-track);
    height: 0.75rem;
    border-radius: 1rem;
    overflow: hidden;
  }
  .bonus-track > span {
    display: block;
    height: 100%;
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
