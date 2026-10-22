<script lang="ts">
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { ScrutinyWarning, SimulationResult } from '../core/types';
  import { diagnosticScope, diagnosticTitle, diagnosticSeverityLabel, type DiagnosticFilter, type MessageSummary } from '../core/diagnostic-messages';

  let { client, result, owner, onInspect }: { client: ResultServiceClient; result: SimulationResult; owner: string;
    onInspect: (simulation: number, chamber?: string) => void } = $props();
  type Context = { rows: ScrutinyWarning[]; total: number; exact: boolean };
  let general = $state.raw<Context>({ rows: [], total: 0, exact: false });
  let summary = $state.raw<MessageSummary>();
  let ids = $state.raw<{ ids: number[]; total: number }>();
  let context = $state.raw<Context>();
  let generalOffset = $state(0), summaryOffset = $state(0), idOffset = $state(0), contextOffset = $state(0);
  let severity = $state(''), chamber = $state(''), text = $state(''), reason = $state('');
  let idSearch = $state<number | undefined>();
  let filter = $state.raw<DiagnosticFilter>({});
  let detailFilter = $state.raw<DiagnosticFilter>({});
  let selected = $state.raw<{ code: string; severity: string }>();
  let inspecting = $state<number>();
  let issue = $state(''), exporting = $state(false);
  const selectedFilter = $derived({ ...filter, ...selected, ...detailFilter });
  const requested = $derived(result.benchmark.simulations);
  const processed = $derived(result.storage?.completedSimulations ?? 0);
  const pct = (n: number, denominator: number) => denominator ? `${(100 * n / denominator).toLocaleString('it-IT', { maximumFractionDigits: 2 })}%` : 'Non disponibile';
  const describe = (error: unknown) => error instanceof Error ? error.message : String(error);

  $effect(() => {
    const resultId = result.storage?.id, offset = generalOffset;
    if (!resultId) { const rows = result.warnings.filter(w => diagnosticScope(w) !== 'simulation'); general = { rows: rows.slice(offset, offset + 50), total: rows.length, exact: false }; return; }
    let alive = true;
    void client.call<Context>('messages-general', { resultId, owner, offset }).then(value => { if (alive) general = value; }).catch(e => { if (alive) issue = describe(e); });
    return () => { alive = false; };
  });
  $effect(() => {
    const resultId = result.storage?.id, applied = filter, offset = summaryOffset;
    summary = undefined;
    if (!resultId) return;
    let alive = true;
    void client.call<MessageSummary>('messages-summary', { resultId, owner, filter: applied, offset }).then(value => { if (alive) summary = value; }).catch(e => { if (alive) issue = describe(e); });
    return () => { alive = false; };
  });
  $effect(() => {
    const resultId = result.storage?.id, applied = selectedFilter, offset = idOffset, selection = selected;
    ids = undefined;
    if (!resultId || !selection || !summary?.exact) return;
    let alive = true;
    void client.call<{ ids: number[]; total: number }>('messages-ids', { resultId, owner, filter: applied, offset }).then(value => { if (alive) ids = value; }).catch(e => { if (alive) issue = describe(e); });
    return () => { alive = false; };
  });
  $effect(() => {
    const resultId = result.storage?.id, applied = selectedFilter, simulation = inspecting, offset = contextOffset;
    context = undefined;
    if (!resultId || !selected || !summary || summary.exact && simulation === undefined) return;
    let alive = true;
    void client.call<Context>('messages-context', { resultId, owner, filter: { ...applied, simulation }, offset }).then(value => { if (alive) context = value; }).catch(e => { if (alive) issue = describe(e); });
    return () => { alive = false; };
  });
  function applyFilter(event: SubmitEvent) {
    event.preventDefault(); filter = { severity, chamber, text }; summaryOffset = 0; selected = undefined; inspecting = undefined; issue = '';
  }
  function applyDetails(event: SubmitEvent) {
    event.preventDefault(); detailFilter = { reason, simulation: idSearch }; idOffset = 0; inspecting = undefined; issue = '';
  }
  function select(code: string, severity: string) {
    selected = { code, severity }; detailFilter = {}; reason = ''; idSearch = undefined; idOffset = 0; inspecting = undefined;
  }
  async function exportIds() {
    if (!result.storage) return;
    exporting = true; issue = '';
    try {
      const file = await client.call<File>('messages-export', { resultId: result.storage.id, owner, filter: selectedFilter });
      const url = URL.createObjectURL(file), link = document.createElement('a'); link.href = url; link.download = 'r26-simulazioni-interessate.csv'; link.click();
      setTimeout(() => URL.revokeObjectURL(url), 60000);
    } catch (e) { issue = describe(e); } finally { exporting = false; }
  }
</script>

<div class="messages" aria-label="Avvisi riforma">
  <section aria-label="Informazioni e avvisi generali">
    <h3>Informazioni e avvisi generali</h3>
    {#if result.storage?.state === 'interrupted'}<p class="execution"><strong>Calcolo interrotto: {processed}/{requested} simulazioni conservate.</strong> Le simulazioni non elaborate restano nel denominatore richiesto e non sono considerate prive di messaggi.</p>{/if}
    {#each general.rows as w, i (generalOffset + i)}
      <article class={{ error: w.severity === 'error', warning: w.severity !== 'info' && w.severity !== 'error' }}>
        <p class="scope">{diagnosticSeverityLabel(w.severity)} · {diagnosticScope(w) === 'global' ? 'Tutte le simulazioni' : diagnosticScope(w) === 'execution' ? 'Esecuzione' : 'Ambito non documentato nell’archivio'}</p>
        {#if w.severity !== 'info'}<h4>{diagnosticTitle(w.code)}</h4><p>{w.message}</p>{/if}
        <details><summary>{w.severity === 'info' ? diagnosticTitle(w.code) : 'Dettagli'}</summary>
          <p><code>{w.code}</code>{#if w.chamber} · {w.chamber}{/if}</p>
          {#if w.severity === 'info'}<p>{w.message}</p>{/if}
          {#if w.lawReference}<p>Riferimento: {w.lawReference}</p>{/if}
          {#if w.todoReference}<p>{w.todoReference}</p>{/if}
        </details>
      </article>
    {:else}<p>Nessun messaggio generale registrato.</p>{/each}
    {#if general.total > 50}<nav aria-label="Pagine messaggi generali"><button disabled={generalOffset === 0} onclick={() => { generalOffset -= 50; }}>Generali precedenti</button><span>{generalOffset + 1}–{Math.min(generalOffset + 50, general.total)} di {general.total}</span><button disabled={generalOffset + 50 >= general.total} onclick={() => { generalOffset += 50; }}>Generali successivi</button></nav>{/if}
  </section>

  <section aria-label="Messaggi delle simulazioni">
    <h3>Messaggi delle simulazioni</h3>
    <p>Simulazioni interessate su {requested} richieste; ogni simulazione conta una sola volta per codice e gravità. Le occorrenze contano anche le ripetizioni.</p>
    <form onsubmit={applyFilter}>
      <label>Gravità dei messaggi<select bind:value={severity}><option value="">Tutte</option><option value="error">Errori</option><option value="warning">Avvisi</option><option value="info">Informazioni</option></select></label>
      <label>Ramo dei messaggi<select bind:value={chamber} disabled={!summary || summary.contextAvailable === false}><option value="">Tutti</option><option value="camera">Camera</option><option value="senato">Senato</option></select></label>
      <label>Cerca nei messaggi<input type="search" bind:value={text} /></label><button>Filtra messaggi</button>
    </form>
    {#if summary}
      {#if !summary.exact}<p>Frequenze e ID esatti non ricostruibili da questo archivio. Sono mostrate solo le occorrenze registrate; nessuna simulazione viene rieseguita.</p>{/if}
      {#if summary.contextAvailable === false}<p>Contesto strutturato non disponibile: ramo, seggio e sorteggio non sono ricostruibili dal vecchio riepilogo. Il filtro del ramo è disattivato.</p>{/if}
      <p class="totals">{#each ['error', 'warning', 'info'] as s (s)}<span>{diagnosticSeverityLabel(s)}: {summary.exact ? `${summary.totals[s as keyof typeof summary.totals].simulations}/${requested}` : 'frequenza non disponibile'} · {summary.totals[s as keyof typeof summary.totals].occurrences} occorrenze</span>{/each}</p>
      <div class="scroll"><table><caption>Riepilogo dei messaggi</caption><thead><tr><th>Gravità</th><th>Messaggio</th><th>Simulazioni interessate</th><th>Occorrenze</th></tr></thead><tbody>
        {#each summary.groups as group (group.severity + group.code)}<tr><td>{diagnosticSeverityLabel(group.severity)}</td><td>{group.title}<br /><code>{group.code}</code>{#if !summary.exact}<br /><button onclick={() => select(group.code, group.severity)} aria-label={`Testi per ${group.code} (${group.severity})`}>Testi originali</button>{/if}</td><td>{#if summary.exact}<button onclick={() => select(group.code, group.severity)} aria-label={`Simulazioni per ${group.code} (${group.severity})`}>{group.simulations}/{requested} · {pct(group.simulations, requested)}</button>{#if processed < requested}<small>{pct(group.simulations, processed)} delle conservate</small>{/if}{:else}Non disponibile{/if}</td><td>{group.occurrences}</td></tr>{:else}<tr><td colspan="4">Nessun messaggio corrisponde ai filtri.</td></tr>{/each}
      </tbody></table></div>
      {#if summary.groupCount > 50}<nav aria-label="Pagine riepilogo"><button disabled={summaryOffset === 0} onclick={() => { summaryOffset -= 50; }}>Riepilogo precedente</button><span>{summaryOffset + 1}–{Math.min(summaryOffset + 50, summary.groupCount)} di {summary.groupCount}</span><button disabled={summaryOffset + 50 >= summary.groupCount} onclick={() => { summaryOffset += 50; }}>Riepilogo successivo</button></nav>{/if}
    {:else if result.storage}<p role="status">Caricamento riepilogo messaggi…</p>{:else}<p>Riepilogo delle simulazioni non disponibile: esecuzione non avviata.</p>{/if}
  </section>

  {#if selected}
    <section aria-label={summary?.exact ? 'Simulazioni interessate' : 'Dettagli messaggio'}>
      <h3>{summary?.exact ? 'Simulazioni interessate' : 'Dettagli messaggio'}</h3><p>{diagnosticTitle(selected.code)} · <code>{selected.code}</code> · {diagnosticSeverityLabel(selected.severity)}</p>
      {#if summary?.exact}
      <form onsubmit={applyDetails}><label>Cerca ID simulazione<input type="number" min="1" max={processed} step="1" bind:value={idSearch} /></label><label>Filtra motivo originale<input type="search" bind:value={reason} /></label><button>Filtra simulazioni</button></form>
      <p>ID unici in ordine crescente, massimo 50 per pagina. Il filtro del ramo del riepilogo resta applicato.</p>
      <button onclick={exportIds} disabled={exporting || !ids}>{exporting ? 'Preparazione CSV…' : 'Scarica tutti gli ID filtrati CSV'}</button>
      {#if ids}<p>{ids.total} simulazioni interessate</p><ul class="ids">{#each ids.ids as id (id)}<li><strong>{id}</strong> <button onclick={() => { inspecting = id; contextOffset = 0; }}>Messaggi di {id}</button> <button onclick={() => onInspect(id, filter.chamber)}>Risultato di {id}</button></li>{:else}<li>Nessuna simulazione corrisponde ai filtri.</li>{/each}</ul>
        {#if ids.total > 50}<nav aria-label="Pagine ID"><button disabled={idOffset === 0} onclick={() => { idOffset -= 50; }}>ID precedenti</button><span>{idOffset + 1}–{Math.min(idOffset + 50, ids.total)} di {ids.total}</span><button disabled={idOffset + 50 >= ids.total} onclick={() => { idOffset += 50; }}>ID successivi</button></nav>{/if}
      {:else}<p role="status">Caricamento ID…</p>{/if}
      {/if}
      {#if inspecting !== undefined || summary?.exact === false}<h4>{inspecting !== undefined ? `Messaggi originali della simulazione ${inspecting}` : 'Testi originali salvati (ID esatti non disponibili)'}</h4>
        {#if context}{#each context.rows as w, i (contextOffset + i)}<article><p>{w.message}</p><p>{w.occurrences ?? 1} occorrenze · <code>{w.code}</code>{#if w.chamber} · {w.chamber}{/if}</p>
          {#if w.reason}<p>Motivo: {w.reason}</p>{/if}{#if w.lawReference}<p>Riferimento: {w.lawReference}</p>{/if}{#if w.interpretationId}<p>Interpretazione: {w.interpretationId}</p>{/if}{#if w.seatId}<p>Seggio: {w.seatId}</p>{/if}{#if w.drawContextId}<p>Sorteggio: {w.drawContextId}</p>{/if}{#if w.todoReference}<p>{w.todoReference}</p>{/if}
        </article>{/each}
          {#if context.total > 50}<nav aria-label="Pagine contesti"><button disabled={contextOffset === 0} onclick={() => { contextOffset -= 50; }}>Contesti precedenti</button><span>{contextOffset + 1}–{Math.min(contextOffset + 50, context.total)} di {context.total}</span><button disabled={contextOffset + 50 >= context.total} onclick={() => { contextOffset += 50; }}>Contesti successivi</button></nav>{/if}
        {:else}<p role="status">Caricamento messaggi originali…</p>{/if}
      {/if}
    </section>
  {/if}
  {#if issue}<p role="status" class="error">{issue}</p>{/if}
</div>

<style>
  .messages { margin: 1rem 0; min-width: 0; }
  section { border-top: 1px solid #ccd8df; padding: 1rem 0; }
  h3 { margin-top: 0; } h4 { margin: .4rem 0; }
  article { border-left: 4px solid #6795ae; background: #f4f8fb; padding: .6rem 1rem; margin: .6rem 0; overflow-wrap: anywhere; }
  article.warning { border-color: #a96700; background: #fffaef; }
  article.error, .execution { border-color: #a92d22; background: #fff1ef; }
  .scope { font-size: .85rem; margin: 0; font-weight: 600; }
  .execution { padding: .8rem; } .error { color: #8b2119; }
  form, nav, .totals { display: flex; flex-wrap: wrap; gap: .7rem; align-items: end; }
  nav { align-items: center; margin: .8rem 0; }
  label { display: grid; gap: .3rem; max-width: 100%; }
  input, select, button { font: inherit; padding: .4rem; min-width: 0; max-width: 100%; }
  button, summary { cursor: pointer; } small { display: block; }
  .scroll { overflow: auto; } table { width: 100%; min-width: 40rem; border-collapse: collapse; }
  caption { text-align: left; font-weight: 600; padding: .7rem 0; }
  th, td { text-align: left; padding: .5rem; border-bottom: 1px solid #d7e0e5; overflow-wrap: anywhere; }
  .ids { padding-left: 1.3rem; } .ids li { margin: .4rem 0; }
</style>
