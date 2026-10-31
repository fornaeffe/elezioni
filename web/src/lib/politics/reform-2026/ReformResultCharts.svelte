<script lang="ts">
  import ElectionProbabilityTable from './ElectionProbabilityTable.svelte';
  import { probabilityLabel } from './election-probability';
  import type { storedBonusProbabilities } from './stored-probabilities';
  import { onMount } from 'svelte';
  import { SvelteMap } from 'svelte/reactivity';
  import type { ResultServiceClient } from '../../core/result-service-client';
  import type { Distribution, SeriesSummary, StatisticsOverview } from '../../results/statistics';
  import type { BoxPlotResultChart, BoxPlotItem } from '../../results/chart-types';
  import { boxPlotItem, chartTicks, displayNumber } from '../../results/chart-builders';
  import { summaryTable } from '../../results/statistics';
  import PairedBars from '../../results/PairedBars.svelte';
  import ResultCharts from '../../results/ResultCharts.svelte';
  import StreamedPlot from '../../results/StreamedPlot.svelte';
  import ReformTerritorialSpinogram from './ReformTerritorialSpinogram.svelte';
  import ReformSeriesChart from './ReformSeriesChart.svelte';
  import type { ReformSeatEndowments } from './stored-statistics';
  let { client, resultId, owner }: { client: ResultServiceClient; resultId: string; owner: string } = $props();
  let overview = $state.raw<StatisticsOverview | null>(null), boxes = $state.raw<BoxPlotResultChart[]>([]);
  let endowments = $state.raw<ReformSeatEndowments | null>(null);
  let showProbability = $state(false), bonusChamber = $state('camera');
  let bonus = $state.raw<Awaited<ReturnType<typeof storedBonusProbabilities>>>([]);
  let meanKind = $state('list'), boxKind = $state('list');
  let error = $state('');
  let scatter = $state(false), nationalSpines = $state(false), localSpine = $state(false), showSummary = $state(false), summaryPage = $state(0);
  let meanChamber = $state<'camera' | 'senato'>('camera'), boxChamber = $state<'camera' | 'senato'>('camera');
  const meanLists = $derived(overview?.series.filter(s => s.chamber === meanChamber && s.kind === meanKind) ?? []);
  const selectedBoxes = $derived(boxes.filter(chart => chart.id.endsWith(boxKind)).map(chart => ({ ...chart, panels: chart.panels.filter(panel => panel.label === boxChamber) })));
  const bonusRows = $derived(bonus.filter(s => s.chamber === bonusChamber));
  const table = $derived(overview ? summaryTable(overview) : null);
  const headings: Record<string, string> = { Chamber: 'Ramo', Name: 'Lista', Requested: 'Richieste', 'Vote observations': 'Percentuali note', 'Election observations': 'Esiti noti',
    'Mean vote share %': 'Percentuale media %', 'Mean elected': 'Eletti medi', 'Confirmed elected minimum / requested': 'Eletti confermati minimi / richieste',
    'Ordinary minimum / requested': 'Ordinari minimi / richieste', 'Bonus minimum / requested': 'Premio minimo / richieste', 'Special minimum / requested': 'Uninominali speciali minimi / richieste', 'TAA minimum / requested': 'Proporzionali TAA minimi / richieste' };
  const boxBySeries = new SvelteMap<string, { distribution: Distribution; max: number }>();
  onMount(() => {
    let alive = true;
    async function load() {
      try {
        const o = await client.call<StatisticsOverview>('statistics', { resultId, owner });
        if (!alive) return; overview = o;
        endowments = await client.call<ReformSeatEndowments>('statistics-endowments', { resultId, owner });
        if (!alive) return;
        bonus = await client.call('statistics-bonus', { resultId, owner });
        if (!alive) return;
        const distributions: { summary: SeriesSummary; distribution: Distribution }[] = [];
        for (const summary of o.series.filter(s => (s.kind === 'list' || s.kind === 'subject') && s.votesKnown)) {
          const distribution = await client.call<Distribution>('statistics-box', { resultId, owner, series: summary.id });
          if (!alive) return; distributions.push({ summary, distribution });
        }
        const charts: BoxPlotResultChart[] = [];
        for (const kind of ['list', 'subject']) {
          const chart: BoxPlotResultChart = { kind: 'boxplot', id: `reform-vote-boxplot-${kind}`, title: kind === 'list' ? 'Percentuali simulate per lista' : 'Percentuali simulate per coalizione / lista singola', xLabel: 'Percentuale sui voti validi · distribuzione delle osservazioni disponibili', panels: [] };
          for (const chamber of ['camera', 'senato'] as const) {
            const entries = distributions.filter(e => e.summary.chamber === chamber && e.summary.kind === kind);
            if (!entries.length) continue;
            const max = Math.max(1, Math.ceil(Math.max(...entries.map(e => e.distribution.max ?? 0))));
            const items: BoxPlotItem[] = [];
            for (const e of entries) { const item = boxPlotItem(e.summary, e.distribution, o.requested, max); if (item) { items.push(item); boxBySeries.set(e.summary.id, { distribution: e.distribution, max }); } }
            chart.panels.push({ label: chamber, ticks: chartTicks(0, max, '%'), items });
          }
          if (chart.panels.length) charts.push(chart);
        }
        boxes = charts;
      } catch (e) { if (alive) error = String(e); }
    }
    void load(); return () => { alive = false; };
  });
  const toggle = (event: Event) => (event.currentTarget as HTMLDetailsElement).open;
</script>

<div class="reform-statistics" aria-label="Statistiche riforma">
  {#if error}<p role="alert">{error}</p>{/if}
  {#if overview}
    {@const o = overview}
    <h3>Eletti medi e percentuali sui voti validi</h3>
    <p>{o.saved}/{o.requested} simulazioni conservate. Eletti per affiliazione personale finale; coalizioni per titolarità del seggio. Sono inclusi ordinari, premio, uninominali speciali e proporzionali TAA; Estero escluso.</p>
    <p>≥ indica una media minima di eletti confermati sulle simulazioni richieste. Le medie complete richiedono tutti gli esiti noti; dati mancanti e preferenze irrisolte non valgono zero. Identità e recuperi restano condizionati ai dati/modelli delle preferenze.</p>
    {#if o.legacy}<p role="note">Archivio precedente: alcune affiliazioni non sono ricostruibili; i grafici locali richiedono una nuova esecuzione. L’apertura dell’archivio non esegue simulazioni.</p>{/if}
    <label>Raggruppamento delle medie<select bind:value={meanKind}><option value="list">Liste</option><option value="subject">Coalizioni / liste singole</option></select></label>
    <label>Ramo delle medie<select bind:value={meanChamber}><option value="camera">Camera</option><option value="senato">Senato</option></select></label>
    {#if meanLists.length}
      <h4>{meanChamber === 'camera' ? 'Camera' : 'Senato'}</h4><PairedBars entityLabel={meanKind === 'list' ? 'Lista' : 'Coalizione / lista singola'} rows={meanLists.map(s => ({ id: s.id, label: s.name, color: s.color,
        elected: s.electedKnown === o.requested ? s.electedSum / o.requested : null, minimum: s.confirmed / o.requested,
        share: s.votesKnown === o.requested ? s.voteSum / o.requested : null }))} />
    {:else}<p role="status">Nessuna lista disponibile per questo ramo.</p>{/if}
    {#if endowments && (endowments.colleges.length || endowments.bonusSlates.length)}
      <details>
        <summary>Dotazioni dei seggi per collegio e liste del premio</summary>
        {#if endowments.colleges.length}
          <h4>Seggi per collegio</h4>
          <div class="table-scroll"><table><caption>Dotazione di ogni collegio, con e senza premio</caption>
            <thead><tr><th>Ramo</th><th>Circoscrizione</th><th>Collegio</th><th>Tipo</th><th>Senza premio</th><th>Con premio</th></tr></thead>
            <tbody>{#each endowments.colleges as row (`${row.chamber}:${row.college}`)}
              <tr><td>{row.chamber === 'camera' ? 'Camera' : 'Senato'}</td><td>{row.territoryName} ({row.territory})</td>
                <td>{row.collegeName} ({row.college})</td><td>{row.kind}</td><td>{row.seatsWithoutBonus}</td><td>{row.seatsWithBonus}</td></tr>
            {/each}</tbody>
          </table></div>
        {/if}
        {#if endowments.bonusSlates.length}
          <h4>Seggi delle liste del premio</h4>
          <div class="table-scroll"><table><caption>Dotazione della lista del premio per circoscrizione</caption>
            <thead><tr><th>Ramo</th><th>Circoscrizione</th><th>Seggi della lista del premio</th></tr></thead>
            <tbody>{#each endowments.bonusSlates as row (`${row.chamber}:${row.territory}`)}
              <tr><td>{row.chamber === 'camera' ? 'Camera' : 'Senato'}</td><td>{row.territoryName} ({row.territory})</td><td>{row.seats}</td></tr>
            {/each}</tbody>
          </table></div>
        {/if}
      </details>
    {/if}
    <details ontoggle={e => { showSummary = toggle(e); }}><summary>Tabella riepilogativa degli eletti e delle percentuali</summary>
      {#if showSummary && table}
        <p>Richieste: tutte le simulazioni previste, incluse quelle fallite o interrotte. Percentuali note ed esiti noti: numero di osservazioni disponibili per la lista e il ramo. Percentuale media ed eletti medi sono disponibili solo con tutte le osservazioni note.</p>
        <p>Le colonne «minimi / richieste» dividono gli eletti confermati per tutte le simulazioni richieste: sono medie minime, non percentuali né probabilità. Ordinari, premio, uninominali speciali e proporzionali TAA suddividono quel minimo per categoria; possono includere recuperi nella sede finale. Estero escluso.</p>
        <div class="table-scroll"><table><caption>Eletti e percentuali medi per lista</caption><thead><tr>{#each table.columns.filter(c => c !== 'List') as column (column)}<th>{headings[column] ?? column}</th>{/each}</tr></thead>
          <tbody>{#each table.rows.slice(summaryPage * 50, summaryPage * 50 + 50) as row (String(row.Chamber) + String(row.List))}<tr>{#each table.columns.filter(c => c !== 'List') as column (column)}<td>{typeof row[column] === 'number' ? displayNumber(row[column] as number) : row[column] ?? 'Non disponibile'}</td>{/each}</tr>{/each}</tbody>
        </table></div>
        {#if table.rows.length > 50}<button disabled={summaryPage === 0} onclick={() => summaryPage--}>Riepilogo precedente</button><button disabled={(summaryPage + 1) * 50 >= table.rows.length} onclick={() => summaryPage++}>Riepilogo successivo</button>{/if}
      {/if}
    </details>
    {#if boxes.length}
      <label>Raggruppamento dei boxplot<select bind:value={boxKind}><option value="list">Liste</option><option value="subject">Coalizioni / liste singole</option></select></label>
      <label>Ramo dei boxplot<select bind:value={boxChamber}><option value="camera">Camera</option><option value="senato">Senato</option></select></label>
      {#key boxChamber + boxKind}
        <ResultCharts charts={selectedBoxes}>
          {#snippet outliers(item)}
            {@const b = boxBySeries.get(item.streamId!)!}
            <StreamedPlot {client} {resultId} {owner} series={item.streamId!} label={`Valori anomali ${item.label}`} color={item.color} variant="outliers" maxX={b.max} whiskerLow={b.distribution.whiskerLow!} whiskerHigh={b.distribution.whiskerHigh!} />
          {/snippet}
        </ResultCharts>
        {#if !selectedBoxes[0]?.panels.length}<p role="status">Nessuna percentuale disponibile per questo ramo.</p>{/if}
      {/key}
    {/if}
    <details role="region" aria-label="Vincitori del premio"><summary>Vincitori del premio</summary>
      <label>Ramo del premio<select bind:value={bonusChamber}><option value="camera">Camera</option><option value="senato">Senato</option></select></label>
      <p>Percentuale delle simulazioni richieste in cui il premio congiunto è assegnato al soggetto nel ramo selezionato, indipendentemente dai seggi poi coperti. ≥ indica un minimo con decisioni mancanti; il premio non assegnato resta nel denominatore.</p>
      <div class="bonus-bars">{#each bonusRows as row (row.id)}<div class="bonus-row"><span>{row.name}</span><span class="bonus-track"><span style:width={`${100 * row.wins / o.requested}%`} style:background={row.color}></span></span><strong>{probabilityLabel(row, o.requested)}</strong></div>{:else}<p>Nessun soggetto disponibile per questo ramo.</p>{/each}</div>
    </details>
    <details role="region" aria-label="Probabilità di elezione" ontoggle={e => { showProbability = toggle(e); }}><summary>Probabilità di elezione</summary>
      {#if showProbability}<ElectionProbabilityTable {client} {resultId} {owner} series={o.series} />{/if}
    </details>
    <details role="region" aria-label="Eletti e percentuali" ontoggle={e => { scatter = toggle(e); }}><summary>Eletti e percentuali</summary>
      {#if scatter}<ReformSeriesChart {client} {resultId} {owner} series={o.series} variant="scatter" />{/if}
    </details>
    <details role="region" aria-label="Spinogrammi degli eletti" ontoggle={e => { nationalSpines = toggle(e); }}><summary>Spinogrammi degli eletti</summary>
      {#if nationalSpines}<ReformSeriesChart {client} {resultId} {owner} series={o.series} variant="spine" />{/if}
    </details>
    <details ontoggle={e => { localSpine = toggle(e); }}><summary>Eletti per collegio o circoscrizione</summary>
      <ReformTerritorialSpinogram {client} {resultId} {owner} overview={o} active={localSpine} />
    </details>
  {:else if !error}<p role="status">Preparazione delle statistiche…</p>{/if}
</div>

<style>
  .reform-statistics { margin: 1.5rem 0; }
  p { font-size: .85rem; line-height: 1.5; color: #4d5963; }
  details { border-bottom: 1px solid #e5e9ed; padding: 1rem 0; }
  summary { cursor: pointer; color: #4d5963; font-size: .9rem; font-weight: 750; }
  label { display: grid; gap: .5rem; margin: 1rem 0; }
  select, button { font: inherit; padding: .5rem; }
  select { box-sizing: border-box; width: 100%; min-width: 0; }
  .bonus-bars { display: grid; gap: .6rem; margin: 1rem 0; font-size: .85rem; }
  .bonus-row { display: grid; grid-template-columns: minmax(5rem, 1fr) 2fr 5rem; gap: .5rem; align-items: center; }
  .bonus-track { background: #e8edf1; height: .75rem; border-radius: 1rem; overflow: hidden; }
  .bonus-track > span { display: block; height: 100%; }
  .table-scroll { overflow: auto; }
  table { border-collapse: collapse; font-size: .8rem; }
  th, td { padding: .5rem; border-bottom: 1px solid #e5e9ed; text-align: left; }
</style>
