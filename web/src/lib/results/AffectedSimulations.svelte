<script lang="ts">
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
  type Context = { rows: ScrutinyWarning[]; total: number; exact: boolean };
  let ids = $state.raw<{ ids: number[]; total: number }>();
  let context = $state.raw<Context>();
  let idOffset = $state(0),
    contextOffset = $state(0),
    reason = $state('');
  let idSearch = $state<number | undefined>(),
    inspecting = $state<number>();
  let detailFilter = $state.raw<DiagnosticFilter>({});
  let issue = $state(''),
    exporting = $state(false);
  const selectedFilter = $derived({ ...filter, ...selected, ...detailFilter });
  const processed = $derived(result.storage?.completedSimulations ?? 0);
  const describe = (error: unknown) => (error instanceof Error ? error.message : String(error));
  $effect(() => {
    const resultId = result.storage?.id,
      applied = selectedFilter,
      offset = idOffset,
      selection = selected;
    ids = undefined;
    if (!resultId || !selection || !exact) return;
    let alive = true;
    void client
      .call<{ ids: number[]; total: number }>('messages-ids', { resultId, owner, filter: applied, offset })
      .then((value) => {
        if (alive) ids = value;
      })
      .catch((e) => {
        if (alive) issue = describe(e);
      });
    return () => {
      alive = false;
    };
  });
  $effect(() => {
    const resultId = result.storage?.id,
      applied = selectedFilter,
      simulation = inspecting,
      offset = contextOffset;
    context = undefined;
    if (!resultId || (exact && simulation === undefined)) return;
    let alive = true;
    void client
      .call<Context>('messages-context', { resultId, owner, filter: { ...applied, simulation }, offset })
      .then((value) => {
        if (alive) context = value;
      })
      .catch((e) => {
        if (alive) issue = describe(e);
      });
    return () => {
      alive = false;
    };
  });
  function applyDetails(event: SubmitEvent) {
    event.preventDefault();
    detailFilter = { reason, simulation: idSearch };
    idOffset = 0;
    contextOffset = 0;
    inspecting = undefined;
    issue = '';
  }
  async function exportIds() {
    if (!result.storage) return;
    exporting = true;
    issue = '';
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
      issue = describe(e);
    } finally {
      exporting = false;
    }
  }
</script>

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
    <p>
      ID unici in ordine crescente, massimo 50 per pagina. Il filtro del ramo del riepilogo resta applicato.
    </p>
    <button onclick={exportIds} disabled={exporting || !ids}>
      {exporting ? 'Preparazione CSV…' : 'Scarica tutti gli ID filtrati CSV'}
    </button>
    {#if ids}<p>{ids.total} simulazioni interessate</p>
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
    {:else}<p role="status">Caricamento ID…</p>{/if}
  {/if}
  {#if inspecting !== undefined || exact === false}<h4>
      {inspecting !== undefined
        ? `Messaggi originali della simulazione ${inspecting}`
        : 'Testi originali salvati (ID esatti non disponibili)'}
    </h4>
    {#if context}{#each context.rows as w, i (contextOffset + i)}<article>
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
        </article>{/each}
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
    {:else}<p role="status">Caricamento messaggi originali…</p>{/if}
  {/if}
</section>
{#if issue}<p role="status" class="error">{issue}</p>{/if}
