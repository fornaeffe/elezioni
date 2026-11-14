<script lang="ts">
  import { onMount } from 'svelte';
  import type { StoredResult } from '../core/result-store';
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { ResultTable } from '../core/types';
  import ResultTableView from '../workspaces/ResultTable.svelte';
  import DiagnosticMessages from '../results/DiagnosticMessages.svelte';
  import ReformResultCharts from './reform-2026/ReformResultCharts.svelte';
  let { record, client }: { record: StoredResult; client: ResultServiceClient } = $props();
  let table = $state.raw<ResultTable | null>(null), error = $state(''), index = $state(0), offset = $state(0), simulation = $state<number | undefined>();
  let sequence = 0, alive = true;
  const descriptor = $derived(record.result.storage?.tables[index]);
  async function load() {
    const revision = ++sequence; error = '';
    try { const next = await client.call<ResultTable>('page', { resultId: record.id, owner: record.owner, table: index, offset, simulation });
      if (alive && revision === sequence) table = next;
    } catch (e) { if (alive && revision === sequence) error = String(e); }
  }
  function inspect(id: number) { simulation = id; offset = 0; void load(); }
  onMount(() => { void load(); return () => { alive = false; ++sequence; }; });
</script>
<div class="session-results">
  <DiagnosticMessages {client} result={record.result} owner={record.owner} onInspect={inspect} />
  <ReformResultCharts {client} resultId={record.id} owner={record.owner} scrutinyId={record.politics!.run.execution.scrutiny.id} />
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
