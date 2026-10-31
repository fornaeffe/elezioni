<script lang="ts">
  import type { ResultServiceClient } from '../../core/result-service-client';
  import type { BonusFilter, CollegeMeasure, SeatScope, StatisticsOverview } from '../../results/statistics';
  import ReformSeriesPanel from './ReformSeriesPanel.svelte';

  let { client, resultId, owner, overview, active }: {
    client: ResultServiceClient; resultId: string; owner: string; overview: StatisticsOverview; active: boolean;
  } = $props();
  let chamber = $state('camera'), level = $state('college'), grouping = $state('list');
  let preferredCollege = $state(''), preferredConstituency = $state(''), preferredEntity = $state('');
  let measure = $state<CollegeMeasure>('elected'), bonusFilter = $state<BonusFilter>('all');
  let seatScope = $state<SeatScope>('ordinary-and-bonus');
  const constituency = $derived(level === 'constituency');
  const kind = $derived(constituency ? `constituency-${grouping}` : 'college');
  const series = $derived((constituency ? overview.constituencies ?? [] : overview.colleges).filter(s => s.chamber === chamber && s.kind === kind));
  const preferred = $derived(constituency ? preferredConstituency : preferredCollege);
  const geographyId = $derived((constituency ? series.find(s => s.territory === preferred)?.territory : series.find(s => s.college === preferred)?.college)
    ?? (constituency ? series[0]?.territory : series[0]?.college));
  const entities = $derived(series.filter(s => (constituency ? s.territory : s.college) === geographyId));
  const selected = $derived(entities.find(s => s.owner === preferredEntity) ?? entities[0]);
  const options = $derived([...new Map(series.map(s => {
    const id = (constituency ? s.territory : s.college)!;
    return [id, { id, label: s.name.split(' · ').slice(1).join(' · ') || id }];
  })).values()]);
  const entityOptions = $derived(entities.map(s => ({ id: s.owner,
    label: overview.series.find(n => n.chamber === chamber && n.kind === (constituency ? grouping : 'list') && n.owner === s.owner)?.name ?? s.name.split(' · ')[0] })));
  const effectiveMeasure = $derived(constituency && measure === 'last-position' ? 'elected' : measure);
  const effectiveScope = $derived(constituency && effectiveMeasure === 'elected' ? seatScope : 'ordinary-and-bonus');
  const outcomeLabel = $derived(effectiveMeasure === 'last-position' ? 'ultima posizione eletta' : effectiveMeasure === 'entitled'
    ? constituency ? 'seggi ordinari spettanti' : 'seggi spettanti' : constituency ? seatScope === 'ordinary' ? 'eletti ordinari' : 'eletti ordinari + premio' : 'eletti');
</script>

<div class="territorial-spinogram">
  <label>Ramo {constituency ? 'della circoscrizione' : 'del collegio'}<select bind:value={chamber}><option value="camera">Camera</option><option value="senato">Senato</option></select></label>
  <label>Livello territoriale<select value={level} onchange={e => {
    preferredEntity = selected?.owner ?? preferredEntity;
    if (e.currentTarget.value === 'constituency') {
      preferredConstituency = selected?.territory ?? preferredConstituency;
      if (measure === 'last-position') measure = 'elected';
    }
    level = e.currentTarget.value;
  }}><option value="college">Collegio</option><option value="constituency">Circoscrizione</option></select></label>
  {#if constituency}
    <p>La vista comprende le circoscrizioni ordinarie. Valle d’Aosta e Trentino-Alto Adige mantengono le rispettive viste speciali; il collegio proporzionale TAA resta selezionabile nella vista per collegio.</p>
    <label>Raggruppamento della circoscrizione<select bind:value={grouping}><option value="list">Liste</option><option value="subject">Coalizioni / liste singole</option></select></label>
  {/if}
  {#if series.length}
    <label>{constituency ? 'Circoscrizione' : 'Collegio'} del grafico<select value={geographyId ?? ''} onchange={e => {
      preferredEntity = selected?.owner ?? preferredEntity;
      if (constituency) preferredConstituency = e.currentTarget.value; else preferredCollege = e.currentTarget.value;
    }}>{#each options as item (item.id)}<option value={item.id}>{item.label}</option>{/each}</select></label>
    <label>{constituency ? grouping === 'subject' ? 'Coalizione / lista singola della circoscrizione' : 'Lista della circoscrizione' : 'Lista del collegio'}<select value={selected?.owner ?? ''} onchange={e => { preferredEntity = e.currentTarget.value; }}>
      {#each entityOptions as item (item.id)}<option value={item.id}>{item.label}</option>{/each}
    </select></label>
    <label>Misura {constituency ? 'della circoscrizione' : 'del collegio'}<select value={effectiveMeasure} onchange={e => { measure = e.currentTarget.value as CollegeMeasure; }}>
      <option value="elected">Eletti nella sede effettiva</option>
      <option value="entitled">{constituency ? 'Seggi ordinari spettanti nella sede originaria' : 'Seggi spettanti nella sede originaria'}</option>
      <option value="last-position" disabled={constituency}>Ultima posizione eletta nella lista</option>
    </select></label>
    {#if constituency}<p>L’ultima posizione eletta è disponibile solo per un singolo collegio: le liste dei candidati hanno ordinamenti distinti.</p>{/if}
    {#if effectiveMeasure === 'last-position'}<p>Posizione dell’ultimo candidato eletto da questa lista in questo collegio: 0 = nessuno, 1 = capolista, 2 = prima preferenza, …, 7 = sesta preferenza. Le elezioni altrove non contano; i recuperi contano nella sede finale. Le posizioni saltate restano nell’ordine, anche senza un rapporto di causa tra le elezioni. Un esito incompleto può lasciare ignota l’ultima posizione.</p>{/if}
    {#if constituency}
      <label>Seggi inclusi<select bind:value={seatScope} disabled={effectiveMeasure !== 'elected'}><option value="ordinary-and-bonus">Ordinari + premio</option><option value="ordinary">Solo ordinari</option></select></label>
      <p>La categoria segue il seggio originario: un seggio del premio resta del premio anche se recuperato da una lista ordinaria. Gli eletti contano nella circoscrizione finale, compresi quelli della lista del premio senza collegio. Le liste seguono l’affiliazione personale finale; le coalizioni la titolarità del seggio. I seggi spettanti comprendono solo gli ordinari nella sede originaria.</p>
    {:else}<p>Gli eletti sono contati nella sede finale, inclusi i recuperi del premio. Il premio senza collegio compare nella vista per circoscrizione e nei grafici nazionali. I seggi spettanti seguono la sede originaria.</p>{/if}
    <label>Simulazioni {constituency ? 'della circoscrizione' : 'del collegio'}<select bind:value={bonusFilter}><option value="all">Tutte le simulazioni</option><option value="awarded">Solo con premio attribuito</option><option value="not-awarded">Solo senza premio attribuito</option></select></label>
    <p>Il filtro usa l’attribuzione del premio congiunto, indipendentemente dalla lista selezionata e dai seggi inclusi. Le decisioni sul premio mancanti o non calcolabili sono escluse dai due sottoinsiemi.</p>
    {#if selected && active}
      <p class="axis-sources">Asse Y: {outcomeLabel} · {selected.chamber} · {selected.name}<br />Asse X: percentuale sui voti validi · {selected.chamber} · {selected.name}</p>
      {#key JSON.stringify([selected.id, effectiveMeasure, bonusFilter, effectiveScope])}
        <ReformSeriesPanel {client} {resultId} {owner} descriptor={selected} variant="spine" measure={effectiveMeasure} {bonusFilter} seatScope={effectiveScope} collapsible={false} eager />
      {/key}
    {/if}
  {:else}<p role="status">Dati {constituency ? 'per circoscrizione' : 'locali'} non disponibili per questo ramo nell’archivio. È necessaria una nuova esecuzione per produrli.</p>{/if}
</div>

<style>
  label { display: grid; gap: .5rem; margin: 1rem 0; }
  select { box-sizing: border-box; width: 100%; min-width: 0; font: inherit; padding: .5rem; }
  p { font-size: .85rem; line-height: 1.5; color: #4d5963; }
</style>
