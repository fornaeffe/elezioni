<script lang="ts">
  import { onMount, untrack } from 'svelte';
  import type { StoredResult } from '../../core/result-store';
  import type { ResultServiceClient } from '../../core/result-service-client';
  import type { ResultTable } from '../../core/types';
  import type { ResultInspection } from '../result-inspection';
  import ResultSection from '../../results/ResultSection.svelte';
  import { buildReformSeatChart } from './result-presentation';

  let {
    record,
    client,
    inspection,
  }: {
    record: StoredResult;
    client: ResultServiceClient;
    inspection?: ResultInspection;
  } = $props();
  let singleOpen = $state(false),
    selectedRun = $state<number | undefined>(1),
    selectedChamber = $state('camera');
  let chartTable = $state.raw<ResultTable>(),
    error = $state('');
  const chart = $derived(
    buildReformSeatChart(
      chartTable,
      selectedChamber,
      selectedRun ?? 0,
      record.politics!.run.executedScenario,
    ),
  );

  let sequence = 0,
    alive = true;
  async function loadChart() {
    const revision = ++sequence;
    const index =
      record.result.storage?.tables.findIndex((t) => t.name === 'Seat entitlement and election locations') ??
      -1;
    const requested = record.result.benchmark.simulations,
      run = selectedRun;
    chartTable = undefined;
    error = '';
    if (index < 0 || run === undefined || !Number.isSafeInteger(run) || run < 1 || run > requested) return;
    try {
      const next = await client.call<ResultTable>('page', {
        resultId: record.id,
        owner: record.owner,
        table: index,
        offset: 0,
        limit: 1000,
        simulation: run,
      });
      if (alive && revision === sequence) chartTable = next;
    } catch (e) {
      if (alive && revision === sequence) error = String(e);
    }
  }

  // Navigation can open this view without binding its independently editable controls to its sibling.
  $effect(() => {
    const request = inspection;
    if (!request) return;
    untrack(() => {
      selectedRun = request.simulation;
      if (request.chamber) selectedChamber = request.chamber;
      if (singleOpen) void loadChart();
      else singleOpen = true;
    });
  });
  onMount(() => () => {
    alive = false;
    ++sequence;
  });
</script>

<ResultSection
  title="Seggi della singola simulazione"
  bind:open={singleOpen}
  onToggle={(open) => {
    if (open) void loadChart();
  }}
>
  {#if singleOpen}
    <div class="controls">
      <label>
        Simulazione grafico
        <input
          type="number"
          min="1"
          max={record.result.benchmark.simulations}
          step="1"
          bind:value={selectedRun}
          onchange={loadChart}
        />
      </label>
      <label>
        Ramo grafico
        <select bind:value={selectedChamber}>
          <option value="camera">Camera</option>
          <option value="senato">Senato</option>
        </select>
      </label>
    </div>
    <figure aria-label="Seggi per categoria e titolare">
      <figcaption>Simulazione {selectedRun}, {selectedChamber}</figcaption>
      {#each chart as bar (bar.id)}<div>
          {bar.label}
          <span
            class="seat-track"
            role="meter"
            aria-label={bar.label}
            aria-valuemin="0"
            aria-valuemax={selectedChamber === 'camera' ? 392 : 196}
            aria-valuenow={bar.seats}
          >
            <span
              style:width={`${(100 * bar.seats) / (selectedChamber === 'camera' ? 392 : 196)}%`}
              style:background={bar.color}
            ></span>
          </span>
          {bar.seats}
        </div>
      {:else}<p>Seggi non disponibili: consultare gli stadi e i blocchi della simulazione.</p>{/each}
    </figure>
    {#if error}<p role="alert">{error}</p>{/if}
  {/if}
</ResultSection>

<style>
  label {
    display: grid;
    gap: 0.4rem;
    margin: 0.75rem 0;
  }
  .controls {
    display: flex;
    flex-wrap: wrap;
    gap: 0.5rem;
    align-items: center;
  }
  input,
  select {
    max-width: 100%;
    box-sizing: border-box;
    padding: 0.5rem;
    font: inherit;
  }
  .seat-track {
    display: inline-block;
    width: 10rem;
    max-width: 100%;
    height: 0.75rem;
    background: #e8edf1;
    border-radius: 1rem;
    overflow: hidden;
    vertical-align: middle;
  }
  .seat-track > span {
    display: block;
    height: 100%;
  }
</style>
