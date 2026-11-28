<script lang="ts">
  import Notice from '$lib/ui/Notice.svelte';
  import { errorMessage } from '../core/operation-error';
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { ScrutinyWarning, SimulationResult } from '../core/types';
  import {
    diagnosticTitle,
    diagnosticSeverityLabel,
    type DiagnosticFilter,
  } from '../core/diagnostic-messages';
  let {
    client,
    result,
    owner,
    filter,
    selected,
    exact,
    onInspect,
    onClose,
  }: {
    client: ResultServiceClient;
    result: SimulationResult;
    owner: string;
    filter: DiagnosticFilter;
    selected: { code: string; severity: string };
    exact: boolean;
    onInspect: (simulation: number, chamber?: string) => void;
    onClose: () => void;
  } = $props();
  type IdPage = { ids: number[]; total: number };
  type ContextPage = { rows: ScrutinyWarning[]; total: number };
  let idOffset = $state(0),
    contextOffset = $state(0),
    reason = $state('');
  let idSearch = $state<number | undefined>(),
    inspecting = $state<number>();
  let detailFilter = $state.raw<DiagnosticFilter>({});
  let exportError = $state(''),
    exporting = $state(false);
  const resultId = $derived(result.storage?.id);
  const selectedFilter = $derived({ ...filter, ...selected, ...detailFilter });
  const processed = $derived(result.storage?.completedSimulations ?? 0);

  async function readPage<T>(query: Promise<T>) {
    try {
      return { data: await query };
    } catch (error) {
      return { error: errorMessage(error) };
    }
  }

  function applyDetails(event: SubmitEvent) {
    event.preventDefault();
    detailFilter = { reason, simulation: idSearch };
    idOffset = 0;
    contextOffset = 0;
    inspecting = undefined;
    exportError = '';
  }
  async function exportIds() {
    if (!result.storage) return;
    exporting = true;
    exportError = '';
    try {
      const file = await client.call<File>('messages-export', {
        resultId: result.storage.id,
        owner,
        filter: selectedFilter,
      });
      const url = URL.createObjectURL(file),
        link = document.createElement('a');
      link.href = url;
      link.download = 'r26-simulazioni-interessate.csv';
      link.click();
      setTimeout(() => URL.revokeObjectURL(url), 60000);
    } catch (e) {
      exportError = errorMessage(e);
    } finally {
      exporting = false;
    }
  }
</script>

{#snippet loadingIds()}<p role="status">Caricamento ID…</p>{/snippet}
{#snippet loadingContext()}<p role="status">Caricamento messaggi originali…</p>{/snippet}

<section aria-label={exact ? 'Simulazioni interessate' : 'Dettagli messaggio'}>
  <h3>{exact ? 'Simulazioni interessate' : 'Dettagli messaggio'}</h3>
  {#if exact}<button onclick={onClose}>Chiudi elenco delle simulazioni</button>{/if}
  <p>
    {diagnosticTitle(selected.code)} ·
    <code>{selected.code}</code>
    · {diagnosticSeverityLabel(selected.severity)}
  </p>
  {#if exact}
    <form onsubmit={applyDetails}>
      <label>
        Cerca ID simulazione
        <input type="number" min="1" max={processed} step="1" bind:value={idSearch} />
      </label>
      <label>
        Filtra motivo originale
        <input type="search" bind:value={reason} />
      </label>
      <button>Filtra simulazioni</button>
    </form>
    {#if resultId}
      <svelte:boundary pending={loadingIds}>
        {@const response = await readPage(client.call<IdPage>('messages-ids', {
          resultId, owner, filter: selectedFilter, offset: idOffset,
        }))}
        {@const ids = response.data}
        {@const loading = $effect.pending() > 0}
        <button onclick={exportIds} disabled={exporting || loading || !ids}>
          {exporting ? 'Preparazione CSV…' : 'Scarica tutti gli ID filtrati CSV'}
        </button>
        {#if loading}{@render loadingIds()}
        {:else if ids}<p>{ids.total} simulazioni interessate</p>
          <ul class="ids">
            {#each ids.ids as id (id)}<li>
                <strong>{id}</strong>
                <button
                  onclick={() => {
                    inspecting = id;
                    contextOffset = 0;
                  }}
                >
                  Messaggi di {id}
                </button>
                <button onclick={() => onInspect(id, filter.chamber)}>Risultato di {id}</button>
              </li>{:else}<li>Nessuna simulazione corrisponde ai filtri.</li>{/each}
          </ul>
          {#if ids.total > 50}<nav aria-label="Pagine ID">
              <button
                disabled={idOffset === 0}
                onclick={() => {
                  idOffset -= 50;
                }}
              >
                ID precedenti
              </button>
              <span>{idOffset + 1}–{Math.min(idOffset + 50, ids.total)} di {ids.total}</span>
              <button
                disabled={idOffset + 50 >= ids.total}
                onclick={() => {
                  idOffset += 50;
                }}
              >
                ID successivi
              </button>
            </nav>{/if}
        {:else}<Notice as="p" severity="error" role="status">{response.error}</Notice>{/if}
      </svelte:boundary>
    {/if}
  {/if}
  {#if resultId && (inspecting !== undefined || !exact)}<h4>
      {inspecting !== undefined
        ? `Messaggi originali della simulazione ${inspecting}`
        : 'Testi originali salvati (ID esatti non disponibili)'}
    </h4>
    <svelte:boundary pending={loadingContext}>
      {@const response = await readPage(client.call<ContextPage>('messages-context', {
        resultId, owner, filter: { ...selectedFilter, simulation: inspecting }, offset: contextOffset,
      }))}
      {@const context = response.data}
      {#if $effect.pending()}{@render loadingContext()}
      {:else if context}<div class="contexts">{#each context.rows as w, i (contextOffset + i)}<Notice as="article" severity={w.severity ?? 'warning'}>
            <p>{w.message}</p>
            <p>
              {w.occurrences ?? 1} occorrenze ·
              <code>{w.code}</code>
              {#if w.chamber}
                · {w.chamber}{/if}
            </p>
            {#if w.reason}<p>Motivo: {w.reason}</p>{/if}{#if w.lawReference}<p>
                Riferimento: {w.lawReference}
              </p>{/if}{#if w.interpretationId}<p>Interpretazione: {w.interpretationId}</p>{/if}{#if w.seatId}<p
              >
                Seggio: {w.seatId}
              </p>{/if}{#if w.drawContextId}<p>Sorteggio: {w.drawContextId}</p>{/if}{#if w.todoReference}<p>
                {w.todoReference}
              </p>{/if}
          </Notice>{/each}</div>
        {#if context.total > 50}<nav aria-label="Pagine contesti">
            <button
              disabled={contextOffset === 0}
              onclick={() => {
                contextOffset -= 50;
              }}
            >
              Contesti precedenti
            </button>
            <span>{contextOffset + 1}–{Math.min(contextOffset + 50, context.total)} di {context.total}</span>
            <button
              disabled={contextOffset + 50 >= context.total}
              onclick={() => {
                contextOffset += 50;
              }}
            >
              Contesti successivi
            </button>
          </nav>{/if}
      {:else}<Notice as="p" severity="error" role="status">{response.error}</Notice>{/if}
    </svelte:boundary>
  {/if}
  {#if exportError}<Notice as="p" severity="error" role="status">{exportError}</Notice>{/if}
</section>

<style>
  form, nav { display: flex; flex-wrap: wrap; gap: var(--space-3); margin-block: var(--space-3); }
  form { align-items: end; }
  nav { align-items: center; }
  label { display: grid; gap: var(--space-1); max-inline-size: 100%; }
  .ids { padding-inline-start: 1.3rem; }
  .ids li { margin-block: var(--space-2); }
  .contexts { display: grid; gap: var(--space-2); }
</style>
