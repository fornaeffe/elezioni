<script lang="ts">
  import { onMount } from 'svelte';
  import type { StoredResult } from '../core/result-store';
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { ResultTable } from '../core/types';
  import ResultTableView from '../workspaces/ResultTable.svelte';
  import DiagnosticMessages from '../results/DiagnosticMessages.svelte';
  import ReformResultCharts from './reform-2026/ReformResultCharts.svelte';
  import { buildReformSeatChart } from './reform-2026/result-presentation';
  let { record, client }: { record: StoredResult; client: ResultServiceClient } = $props();
  let table = $state.raw<ResultTable | null>(null), error = $state(''), index = $state(0), offset = $state(0), simulation = $state<number | undefined>();
  let sequence = 0, alive = true;
  let singleOpen = $state(false), selectedRun = $state(1), selectedChamber = $state('camera');
  let chartTable = $state.raw<ResultTable>(), chartSequence = 0;
  const chart = $derived(buildReformSeatChart(chartTable, selectedChamber, selectedRun, record.politics!.run.executedScenario));
  const descriptor = $derived(record.result.storage?.tables[index]);
  async function load() {
    const revision = ++sequence; error = '';
    if (!descriptor) { table = null; return; }
    try { const next = await client.call<ResultTable>('page', { resultId: record.id, owner: record.owner, table: index, offset, simulation });
      if (alive && revision === sequence) table = next;
    } catch (e) { if (alive && revision === sequence) error = String(e); }
  }
  async function loadChart() {
    const revision = ++chartSequence;
    const index = record.result.storage?.tables.findIndex(t => t.name === 'Seat entitlement and election locations') ?? -1;
    if (index < 0 || !Number.isSafeInteger(selectedRun) || selectedRun < 1 || selectedRun > record.result.benchmark.simulations) { chartTable = undefined; return; }
    try {
      const next = await client.call<ResultTable>('page', { resultId: record.id, owner: record.owner, table: index, offset: 0, limit: 1000, simulation: selectedRun });
      if (alive && revision === chartSequence) chartTable = next;
    } catch (e) { if (alive && revision === chartSequence) error = String(e); }
  }
  function inspect(id: number, chamber?: string) {
    simulation = selectedRun = id; offset = 0; singleOpen = true;
    if (chamber) selectedChamber = chamber;
    void load(); void loadChart();
  }
  onMount(() => { void load(); return () => { alive = false; ++sequence; ++chartSequence; }; });
</script>
<div class="session-results">
  <DiagnosticMessages {client} result={record.result} owner={record.owner} onInspect={inspect} />
  <ReformResultCharts {client} resultId={record.id} owner={record.owner} scrutinyId={record.politics!.run.execution.scrutiny.id} />
  {#if record.politics!.run.execution.scrutiny.id === 'politiche-reform-2026-v1'}
    <details bind:open={singleOpen} ontoggle={e => { if (e.currentTarget.open) void loadChart(); }}>
      <summary>Seggi della singola simulazione</summary>
      {#if singleOpen}
        <div class="controls"><label>Simulazione grafico<input type="number" min="1" max={record.result.benchmark.simulations} step="1" bind:value={selectedRun} onchange={loadChart} /></label>
          <label>Ramo grafico<select bind:value={selectedChamber}><option value="camera">Camera</option><option value="senato">Senato</option></select></label></div>
        <figure aria-label="Seggi per categoria e titolare"><figcaption>Simulazione {selectedRun}, {selectedChamber}</figcaption>
          {#each chart as bar (bar.id)}<div>{bar.label} <meter min="0" max={selectedChamber === 'camera' ? 392 : 196} value={bar.seats}>{bar.seats}</meter> {bar.seats}</div>
          {:else}<p>Seggi non disponibili: consultare gli stadi e i blocchi della simulazione.</p>{/each}
        </figure>
      {/if}
    </details>
  {/if}
  <label>Tabella dei risultati<select bind:value={index} onchange={() => { offset = 0; void load(); }}>
    {#each record.result.storage?.tables ?? [] as t, i (i)}<option value={i}>{t.name} ({t.rows})</option>{/each}
  </select></label>
  <label>Simulazione (vuoto = tutte)<input type="number" min="1" max={record.result.benchmark.simulations} bind:value={simulation} onchange={() => { offset = 0; void load(); }} /></label>
  <div class="controls"><button disabled={offset === 0} onclick={() => { offset = Math.max(0, offset - 50); void load(); }}>Pagina precedente</button>
    <span>Righe {offset + 1}–{offset + (table?.rows.length ?? 0)} di {descriptor?.rows ?? 0}</span>
    <button disabled={(table?.rows.length ?? 0) < 50} onclick={() => { offset += 50; void load(); }}>Pagina successiva</button></div>
  {#if error}<p role="alert">{error}</p>{/if}
  {#if table}<div class="table-scroll"><ResultTableView {table} /></div>{/if}
</div>
<style>
  .session-results { min-width: 0; } label { display: grid; gap: .4rem; margin: .75rem 0; } .controls { display: flex; flex-wrap: wrap; gap: .5rem; align-items: center; }
  input, select, button { max-width: 100%; box-sizing: border-box; padding: .5rem; font: inherit; } .table-scroll { overflow: auto; }
</style>
