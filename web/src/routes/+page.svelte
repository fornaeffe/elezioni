<script lang="ts">
  import { Plus, Play, Trash2 } from '@lucide/svelte';
  import SimulationWorker from '$lib/workers/simulation.worker?worker';
  import type {
    ResultTable,
    Scenario,
    ScenarioCoalition,
    ScenarioList,
    SimulationRequest,
    SimulationWorkerMessage
  } from '$lib/core/types';

  const dataVersion = 'v1';

  let simulations = $state(100);
  let seed = $state('politiche-2027');
  let running = $state(false);
  let phase = $state('idle');
  let elapsedMs = $state(0);
  let tables = $state<ResultTable[]>([]);
  let warnings = $state<string[]>([]);

  let coalitions = $state<ScenarioCoalition[]>([
    { id: 'centrosinistra', name: 'Centrosinistra', color: '#d94848' },
    { id: 'centrodestra', name: 'Centrodestra', color: '#3267b1' },
    { id: 'm5s', name: 'Movimento 5 Stelle', color: '#d8b400' }
  ]);

  let lists = $state<ScenarioList[]>([
    {
      id: 'pd',
      name: 'Partito Democratico',
      coalition: 'Centrosinistra',
      color: '#d94848',
      startingShare: 23
    },
    {
      id: 'fdi',
      name: "Fratelli d'Italia",
      coalition: 'Centrodestra',
      color: '#3267b1',
      startingShare: 28
    },
    {
      id: 'm5s',
      name: 'Movimento 5 Stelle',
      coalition: 'Movimento 5 Stelle',
      color: '#d8b400',
      startingShare: 12
    }
  ]);

  const scenario = $derived.by<Scenario>(() => ({
    id: 'politiche-2027',
    name: 'Politiche 2027',
    electionDate: '2027-03-01',
    lists: lists.map((list) => ({ ...list })),
    coalitions: coalitions.map((coalition) => ({ ...coalition }))
  }));
  const runButtonLabel = $derived(running ? phase : 'Esegui');
  const elapsedLabel = $derived(`${elapsedMs.toFixed(0)} ms`);

  function addList(): void {
    lists = [
      ...lists,
      {
        id: crypto.randomUUID(),
        name: 'Nuova lista',
        coalition: coalitions[0]?.name ?? null,
        color: '#6f7f8f',
        startingShare: 1
      }
    ];
  }

  function removeList(id: string): void {
    lists = lists.filter((row) => row.id !== id);
  }

  function runSimulation(): void {
    const worker = new SimulationWorker();
    const request: SimulationRequest = {
      kind: 'politiche',
      scenario,
      electionDate: '2027-03-01',
      simulations,
      seed,
      dataVersion
    };

    running = true;
    phase = 'validate';
    elapsedMs = 0;
    tables = [];
    warnings = [];

    worker.onmessage = (event: MessageEvent<SimulationWorkerMessage>) => {
      const message = event.data;
      if (message.type === 'progress') {
        phase = message.phase;
        elapsedMs = message.elapsedMs;
        return;
      }

      tables = message.tables;
      warnings = message.warnings.map((warning) => `${warning.code}: ${warning.message}`);
      elapsedMs = message.benchmark.elapsedMs;
      running = false;
      phase = message.status;
      worker.terminate();
    };

    worker.onerror = (error) => {
      warnings = [`WORKER_ERROR: ${error.message}`];
      elapsedMs = performance.now();
      running = false;
      phase = 'error';
      worker.terminate();
    };

    try {
      worker.postMessage(request);
    } catch (error) {
      warnings = [`WORKER_POST_ERROR: ${error instanceof Error ? error.message : String(error)}`];
      elapsedMs = 0;
      running = false;
      phase = 'error';
      worker.terminate();
    }
  }
</script>

<svelte:head>
  <title>Elezioni</title>
</svelte:head>

<main class="workspace">
  <section class="toolbar" aria-label="Simulazione">
    <div>
      <h1>Politiche 2027</h1>
      <p>Snapshot {dataVersion}</p>
    </div>
    <label>
      Simulazioni
      <input type="number" min="1" max="10000" step="1" bind:value={simulations} />
    </label>
    <label>
      Seed
      <input type="text" bind:value={seed} />
    </label>
    <button class="primary" type="button" onclick={runSimulation} disabled={running}>
      <Play size={18} aria-hidden="true" />
      <span>{runButtonLabel}</span>
    </button>
  </section>

  <section class="grid">
    <div class="panel scenario-panel">
      <div class="panel-heading">
        <h2>Scenario</h2>
        <button type="button" class="icon-button" onclick={addList} title="Aggiungi lista">
          <Plus size={18} aria-hidden="true" />
        </button>
      </div>

      <div class="list-editor">
        {#each lists as list (list.id)}
          <div class="list-row">
            <input class="color" type="color" bind:value={list.color} aria-label="Colore lista" />
            <input type="text" bind:value={list.name} aria-label="Nome lista" />
            <select bind:value={list.coalition} aria-label="Coalizione">
              {#each coalitions as coalition}
                <option value={coalition.name}>{coalition.name}</option>
              {/each}
            </select>
            <input
              class="share"
              type="number"
              min="0"
              max="100"
              step="0.1"
              bind:value={list.startingShare}
              aria-label="Quota iniziale"
            />
            <button type="button" class="icon-button danger" onclick={() => removeList(list.id)} title="Rimuovi lista">
              <Trash2 size={18} aria-hidden="true" />
            </button>
          </div>
        {/each}
      </div>
    </div>

    <div class="panel result-panel">
      <div class="panel-heading">
        <h2>Risultati</h2>
        <span>{elapsedLabel}</span>
      </div>

      {#if warnings.length > 0}
        <div class="warnings">
          {#each warnings as warning}
            <p>{warning}</p>
          {/each}
        </div>
      {/if}

      {#each tables as table}
        <table>
          <caption>{table.name}</caption>
          <thead>
            <tr>
              {#each table.columns as column}
                <th>{column}</th>
              {/each}
            </tr>
          </thead>
          <tbody>
            {#each table.rows as row}
              <tr>
                {#each table.columns as column}
                  <td>{row[column]}</td>
                {/each}
              </tr>
            {/each}
          </tbody>
        </table>
      {/each}
    </div>
  </section>
</main>

<style>
  :global(body) {
    margin: 0;
    background: #f4f6f7;
    color: #182026;
    font-family:
      Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  }

  button,
  input,
  select {
    font: inherit;
  }

  .workspace {
    min-height: 100vh;
    padding: 24px;
  }

  .toolbar {
    display: grid;
    grid-template-columns: minmax(220px, 1fr) 160px 220px auto;
    gap: 16px;
    align-items: end;
    margin: 0 auto 20px;
    max-width: 1180px;
  }

  h1,
  h2,
  p {
    margin: 0;
  }

  h1 {
    font-size: 28px;
    font-weight: 750;
  }

  h2 {
    font-size: 16px;
    font-weight: 700;
  }

  .toolbar p,
  .panel-heading span {
    color: #697681;
    font-size: 13px;
  }

  label {
    display: grid;
    gap: 6px;
    color: #4d5963;
    font-size: 13px;
    font-weight: 650;
  }

  input,
  select {
    min-height: 38px;
    border: 1px solid #c9d0d6;
    border-radius: 6px;
    background: #ffffff;
    color: #182026;
    padding: 0 10px;
  }

  .grid {
    display: grid;
    grid-template-columns: minmax(0, 1.1fr) minmax(320px, 0.9fr);
    gap: 20px;
    max-width: 1180px;
    margin: 0 auto;
  }

  .panel {
    border: 1px solid #d8dee3;
    border-radius: 8px;
    background: #ffffff;
    min-width: 0;
  }

  .panel-heading {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    min-height: 52px;
    border-bottom: 1px solid #e5e9ed;
    padding: 0 16px;
  }

  .list-editor {
    display: grid;
    gap: 10px;
    padding: 16px;
  }

  .list-row {
    display: grid;
    grid-template-columns: 40px minmax(130px, 1fr) minmax(130px, 0.8fr) 86px 40px;
    gap: 8px;
    align-items: center;
  }

  .color {
    width: 40px;
    padding: 3px;
  }

  .share {
    text-align: right;
  }

  button {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    gap: 8px;
    min-height: 38px;
    border: 1px solid #bdc7d0;
    border-radius: 6px;
    background: #ffffff;
    color: #182026;
    cursor: pointer;
  }

  button:disabled {
    cursor: wait;
    opacity: 0.7;
  }

  .primary {
    min-width: 116px;
    border-color: #2f6f57;
    background: #2f6f57;
    color: #ffffff;
    font-weight: 700;
  }

  .icon-button {
    width: 38px;
    padding: 0;
  }

  .danger {
    color: #9d2d2d;
  }

  .result-panel {
    overflow: hidden;
  }

  .warnings {
    margin: 16px;
    border-left: 4px solid #c4822e;
    background: #fff7ec;
    padding: 10px 12px;
    color: #6f4315;
    font-size: 13px;
  }

  table {
    width: calc(100% - 32px);
    margin: 16px;
    border-collapse: collapse;
    font-size: 13px;
  }

  caption {
    margin-bottom: 8px;
    text-align: left;
    color: #4d5963;
    font-weight: 700;
  }

  th,
  td {
    border-bottom: 1px solid #e5e9ed;
    padding: 8px 6px;
    text-align: left;
  }

  th {
    color: #5c6872;
    font-weight: 700;
  }

  @media (max-width: 860px) {
    .workspace {
      padding: 16px;
    }

    .toolbar,
    .grid {
      grid-template-columns: 1fr;
    }

    .list-row {
      grid-template-columns: 40px minmax(0, 1fr) 78px 40px;
    }

    .list-row select {
      grid-column: 2 / 4;
    }
  }
</style>
