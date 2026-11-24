<script lang="ts">
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { SimulationResult } from '../core/types';
  import {
    diagnosticSeverityLabel,
    type DiagnosticFilter,
    type MessageSummary,
  } from '../core/diagnostic-messages';
  import ResultSection from './ResultSection.svelte';
  import AffectedSimulations from './AffectedSimulations.svelte';
  let {
    client,
    result,
    owner,
    onInspect,
  }: {
    client: ResultServiceClient;
    result: SimulationResult;
    owner: string;
    onInspect: (simulation: number, chamber?: string) => void;
  } = $props();
  let summary = $state.raw<MessageSummary>();
  let summaryOffset = $state(0);
  let severity = $state(''),
    chamber = $state(''),
    text = $state('');
  let filter = $state.raw<DiagnosticFilter>({});
  let selected = $state.raw<{ code: string; severity: string }>();
  let issue = $state('');
  const requested = $derived(result.benchmark.simulations);
  const processed = $derived(result.storage?.completedSimulations ?? 0);
  const pct = (n: number, denominator: number) =>
    denominator
      ? `${((100 * n) / denominator).toLocaleString('it-IT', { maximumFractionDigits: 2 })}%`
      : 'Non disponibile';
  const describe = (error: unknown) => (error instanceof Error ? error.message : String(error));
  $effect(() => {
    const resultId = result.storage?.id,
      applied = filter,
      offset = summaryOffset;
    summary = undefined;
    if (!resultId) return;
    let alive = true;
    void client
      .call<MessageSummary>('messages-summary', { resultId, owner, filter: applied, offset })
      .then((value) => {
        if (alive) summary = value;
      })
      .catch((e) => {
        if (alive) issue = describe(e);
      });
    return () => {
      alive = false;
    };
  });
  function applyFilter(event: SubmitEvent) {
    event.preventDefault();
    filter = { severity, chamber, text };
    summaryOffset = 0;
    selected = undefined;
    issue = '';
  }
  function select(code: string, severity: string) {
    selected = { code, severity };
  }
</script>

<ResultSection title="Messaggi delle simulazioni">
  <form onsubmit={applyFilter}>
    <label>
      Gravità dei messaggi
      <select bind:value={severity}>
        <option value="">Tutte</option>
        <option value="error">Errori</option>
        <option value="warning">Avvisi</option>
        <option value="info">Informazioni</option>
      </select>
    </label>
    <label>
      Ramo dei messaggi
      <select bind:value={chamber} disabled={!summary || summary.contextAvailable === false}>
        <option value="">Tutti</option>
        <option value="camera">Camera</option>
        <option value="senato">Senato</option>
      </select>
    </label>
    <label>
      Cerca nei messaggi
      <input type="search" bind:value={text} />
    </label>
    <button>Filtra messaggi</button>
  </form>
  {#if summary}
    {#if !summary.exact}<p>
        Frequenze e ID esatti non ricostruibili da questo archivio. Sono mostrate solo le occorrenze
        registrate; nessuna simulazione viene rieseguita.
      </p>{/if}
    {#if summary.contextAvailable === false}<p>
        Contesto strutturato non disponibile: ramo, seggio e sorteggio non sono ricostruibili dal vecchio
        riepilogo. Il filtro del ramo è disattivato.
      </p>{/if}
    <p class="totals">
      {#each ['error', 'warning', 'info'] as s (s)}<span>
          {diagnosticSeverityLabel(s)}: {summary.exact
            ? `${summary.totals[s as keyof typeof summary.totals].simulations}/${requested}`
            : 'frequenza non disponibile'} · {summary.totals[s as keyof typeof summary.totals].occurrences} occorrenze
        </span>{/each}
    </p>
    <div class="scroll">
      <table>
        <caption>Riepilogo dei messaggi</caption>
        <thead>
          <tr>
            <th>Gravità</th>
            <th>Messaggio</th>
            <th>Simulazioni interessate</th>
            <th>Occorrenze</th>
          </tr>
        </thead>
        <tbody>
          {#each summary.groups as group (group.severity + group.code)}<tr>
              <td>{diagnosticSeverityLabel(group.severity)}</td>
              <td>
                {group.title}
                <br />
                <code>{group.code}</code>
                {#if !summary.exact}<br />
                  <button
                    onclick={() => select(group.code, group.severity)}
                    aria-label={`Testi per ${group.code} (${group.severity})`}
                  >
                    Testi originali
                  </button>{/if}
              </td>
              <td>
                {#if summary.exact}<button
                    onclick={() => select(group.code, group.severity)}
                    aria-label={`Simulazioni per ${group.code} (${group.severity})`}
                  >
                    {group.simulations}/{requested} · {pct(group.simulations, requested)}
                  </button>
                  {#if processed < requested}<small>
                      {pct(group.simulations, processed)} delle conservate
                    </small>{/if}{:else}Non disponibile{/if}
              </td>
              <td>{group.occurrences}</td>
            </tr>{:else}<tr><td colspan="4">Nessun messaggio corrisponde ai filtri.</td></tr>{/each}
        </tbody>
      </table>
    </div>
    {#if summary.groupCount > 50}<nav aria-label="Pagine riepilogo">
        <button
          disabled={summaryOffset === 0}
          onclick={() => {
            summaryOffset -= 50;
          }}
        >
          Riepilogo precedente
        </button>
        <span>
          {summaryOffset + 1}–{Math.min(summaryOffset + 50, summary.groupCount)} di {summary.groupCount}
        </span>
        <button
          disabled={summaryOffset + 50 >= summary.groupCount}
          onclick={() => {
            summaryOffset += 50;
          }}
        >
          Riepilogo successivo
        </button>
      </nav>{/if}
  {:else if result.storage}<p role="status">Caricamento riepilogo messaggi…</p>{:else}<p>
      Riepilogo delle simulazioni non disponibile: esecuzione non avviata.
    </p>{/if}
  {#if selected && summary}
    {#key selected}
      <AffectedSimulations
        {client}
        {result}
        {owner}
        {filter}
        {selected}
        exact={summary.exact}
        {onInspect}
        onClose={() => {
          selected = undefined;
        }}
      />
    {/key}
  {/if}
  {#if issue}<p role="status" class="error">{issue}</p>{/if}
</ResultSection>
