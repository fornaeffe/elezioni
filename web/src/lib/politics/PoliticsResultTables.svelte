<script lang="ts">
  import { onMount, untrack } from 'svelte';
  import type { StoredResult } from '../core/result-store';
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { ResultTable } from '../core/types';
  import type { ResultInspection } from './result-inspection';
  import ResultTableView from '../workspaces/ResultTable.svelte';
  import ResultSection from '$lib/results/ResultSection.svelte';

  let {
    record,
    client,
    inspection,
  }: {
    record: StoredResult;
    client: ResultServiceClient;
    inspection?: ResultInspection;
  } = $props();
  let table = $state.raw<ResultTable | null>(null);
  let error = $state(''),
    index = $state(0),
    offset = $state(0);
  let simulation = $state<number | undefined>();
  const descriptor = $derived(record.result.storage?.tables[index]);

  let sequence = 0,
    alive = true;
  async function load() {
    const revision = ++sequence;
    error = '';
    if (!descriptor) {
      table = null;
      return;
    }
    try {
      const next = await client.call<ResultTable>('page', {
        resultId: record.id,
        owner: record.owner,
        table: index,
        offset,
        simulation,
      });
      if (alive && revision === sequence) table = next;
    } catch (e) {
      if (alive && revision === sequence) error = String(e);
    }
  }

  // Apply navigation once per request; subsequent control edits remain local.
  $effect(() => {
    const request = inspection;
    if (!request) return;
    untrack(() => {
      simulation = request.simulation;
      offset = 0;
      void load();
    });
  });
  onMount(() => {
    void load();
    return () => {
      alive = false;
      ++sequence;
    };
  });
</script>

<ResultSection title="Tabelle">
  <label>
    Tabella dei risultati
    <select
      bind:value={index}
      onchange={() => {
        offset = 0;
        void load();
      }}
    >
      {#each record.result.storage?.tables ?? [] as t, i (i)}<option value={i}>
          {t.name} ({t.rows})
        </option>{/each}
    </select>
  </label>
  <label>
    Simulazione (vuoto = tutte)
    <input
      type="number"
      min="1"
      max={record.result.benchmark.simulations}
      bind:value={simulation}
      onchange={() => {
        offset = 0;
        void load();
      }}
    />
  </label>
  <div class="controls">
    <button
      disabled={offset === 0}
      onclick={() => {
        offset = Math.max(0, offset - 50);
        void load();
      }}
    >
      Pagina precedente
    </button>
    <span>Righe {offset + 1}–{offset + (table?.rows.length ?? 0)} di {descriptor?.rows ?? 0}</span>
    <button
      disabled={(table?.rows.length ?? 0) < 50}
      onclick={() => {
        offset += 50;
        void load();
      }}
    >
      Pagina successiva
    </button>
  </div>
  {#if error}<p role="alert">{error}</p>{/if}
  {#if table}<ResultTableView {table} />{/if}
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
</style>
