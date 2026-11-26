<script lang="ts">
  import Notice from '$lib/ui/Notice.svelte';
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { ScrutinyWarning, SimulationResult } from '../core/types';
  import { diagnosticScope, diagnosticTitle, diagnosticSeverityLabel } from '../core/diagnostic-messages';
  import ResultSection from './ResultSection.svelte';
  let { client, result, owner }: { client: ResultServiceClient; result: SimulationResult; owner: string } =
    $props();
  type Context = { rows: ScrutinyWarning[]; total: number; exact: boolean };
  let general = $state.raw<Context>({ rows: [], total: 0, exact: false });
  let generalOffset = $state(0),
    issue = $state('');
  const requested = $derived(result.benchmark.simulations);
  const processed = $derived(result.storage?.completedSimulations ?? 0);
  const describe = (error: unknown) => (error instanceof Error ? error.message : String(error));
  $effect(() => {
    const resultId = result.storage?.id,
      offset = generalOffset;
    if (!resultId) {
      const rows = result.warnings.filter((w) => diagnosticScope(w) !== 'simulation');
      general = { rows: rows.slice(offset, offset + 50), total: rows.length, exact: false };
      return;
    }
    let alive = true;
    void client
      .call<Context>('messages-general', { resultId, owner, offset })
      .then((value) => {
        if (alive) general = value;
      })
      .catch((e) => {
        if (alive) issue = describe(e);
      });
    return () => {
      alive = false;
    };
  });
</script>

<ResultSection title="Informazioni e avvisi generali">
  <div class="entries">
  {#if result.storage?.state === 'interrupted'}<Notice as="div" severity="error">
      <strong>Calcolo interrotto: {processed}/{requested} simulazioni conservate.</strong>
       Le simulazioni non elaborate restano nel denominatore richiesto e non sono considerate prive di messaggi.
    </Notice>{/if}
  {#each general.rows as w, i (generalOffset + i)}
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
  </div>
  {#if general.total > 50}<nav aria-label="Pagine messaggi generali">
      <button
        disabled={generalOffset === 0}
        onclick={() => {
          generalOffset -= 50;
        }}
      >
        Generali precedenti
      </button>
      <span>{generalOffset + 1}–{Math.min(generalOffset + 50, general.total)} di {general.total}</span>
      <button
        disabled={generalOffset + 50 >= general.total}
        onclick={() => {
          generalOffset += 50;
        }}
      >
        Generali successivi
      </button>
    </nav>{/if}
  {#if issue}<Notice as="p" severity="error" role="status">{issue}</Notice>{/if}
</ResultSection>

<style>
  .entries { display: grid; gap: var(--space-2); margin-block: var(--space-2); }
  h4 { margin-block: var(--space-2); }
  .scope { margin: 0; font-size: var(--text-small); font-weight: 600; }
  nav { display: flex; flex-wrap: wrap; align-items: center; gap: var(--space-2); margin-block: var(--space-3); }
</style>
