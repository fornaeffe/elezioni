<script lang="ts">
  import Notice from '$lib/ui/Notice.svelte';
  import { errorMessage } from '../core/operation-error';
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { ScrutinyWarning, SimulationResult } from '../core/types';
  import { diagnosticScope, diagnosticTitle, diagnosticSeverityLabel } from '../core/diagnostic-messages';
  import ResultSection from './ResultSection.svelte';
  let { client, result, owner }: { client: ResultServiceClient; result: SimulationResult; owner: string } =
    $props();
  type MessagePage = { rows: ScrutinyWarning[]; total: number };
  const pageSize = 50;
  let generalOffset = $state(0);
  const resultId = $derived(result.storage?.id);
  const requested = $derived(result.benchmark.simulations);
  const processed = $derived(result.storage?.completedSimulations ?? 0);
  const localRows = $derived(result.warnings.filter((warning) => diagnosticScope(warning) !== 'simulation'));
  const localPage = $derived({
    rows: localRows.slice(generalOffset, generalOffset + pageSize),
    total: localRows.length,
    offset: generalOffset,
  });

  async function loadPage(service: ResultServiceClient, resultId: string, owner: string, offset: number) {
    const page = await service.call<MessagePage>('messages-general', { resultId, owner, offset });
    return { ...page, offset };
  }
</script>

{#snippet loadingGeneral()}<p role="status">Caricamento messaggi generali…</p>{/snippet}
{#snippet failedGeneral(error: unknown, reset: () => void)}
  <Notice as="p" severity="error" role="status">{errorMessage(error)}</Notice>
  <button type="button" onclick={reset}>Riprova a caricare i messaggi generali</button>
{/snippet}

{#snippet messagePage(general: MessagePage & { offset: number })}
  {#each general.rows as w, i (general.offset + i)}
    <Notice as="article" severity={w.severity ?? 'warning'}>
      <p class="scope">
        {diagnosticSeverityLabel(w.severity)} · {diagnosticScope(w) === 'global'
          ? 'Tutte le simulazioni'
          : diagnosticScope(w) === 'execution'
            ? 'Esecuzione'
            : 'Ambito non documentato nell’archivio'}
      </p>
      {#if w.severity !== 'info'}<h4>{diagnosticTitle(w.code)}</h4>
        <p>{w.message}</p>{/if}
      <ResultSection compact title={w.severity === 'info' ? diagnosticTitle(w.code) : 'Dettagli'}>
        <p>
          <code>{w.code}</code>
          {#if w.chamber}
            · {w.chamber}{/if}
        </p>
        {#if w.severity === 'info'}<p>{w.message}</p>{/if}
        {#if w.lawReference}<p>Riferimento: {w.lawReference}</p>{/if}
        {#if w.todoReference}<p>{w.todoReference}</p>{/if}
      </ResultSection>
    </Notice>
  {:else}<p>Nessun messaggio generale registrato.</p>{/each}
  {#if general.total > pageSize}<nav aria-label="Pagine messaggi generali">
      <button
        disabled={general.offset === 0}
        onclick={() => {
          generalOffset = general.offset - pageSize;
        }}
      >
        Generali precedenti
      </button>
      <span>{general.offset + 1}–{Math.min(general.offset + pageSize, general.total)} di {general.total}</span>
      <button
        disabled={general.offset + pageSize >= general.total}
        onclick={() => {
          generalOffset = general.offset + pageSize;
        }}
      >
        Generali successivi
      </button>
    </nav>{/if}
{/snippet}

<ResultSection title="Informazioni e avvisi generali">
  <div class="entries">
    {#if result.storage?.state === 'interrupted'}<Notice as="div" severity="error">
        <strong>Calcolo interrotto: {processed}/{requested} simulazioni conservate.</strong>
        Le simulazioni non elaborate restano nel denominatore richiesto e non sono considerate prive di messaggi.
      </Notice>{/if}
    {#if resultId}
      <svelte:boundary pending={loadingGeneral} failed={failedGeneral}>
        {@const general = await loadPage(client, resultId, owner, generalOffset)}
        {#if $effect.pending()}{@render loadingGeneral()}{/if}
        {@render messagePage(general)}
      </svelte:boundary>
    {:else}
      {@render messagePage(localPage)}
    {/if}
  </div>
</ResultSection>

<style>
  .entries { display: grid; gap: var(--space-2); margin-block: var(--space-2); }
  h4 { margin-block: var(--space-2); }
  .scope { margin: 0; font-size: var(--text-small); font-weight: 600; }
  nav { display: flex; flex-wrap: wrap; align-items: center; gap: var(--space-2); margin-block: var(--space-3); }
</style>
