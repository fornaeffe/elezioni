<script lang="ts">
  import type { StoredResult } from '../core/result-store';
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { ResultInspection } from './result-inspection';
  import DiagnosticMessages from '../results/DiagnosticMessages.svelte';
  import PoliticsResultStatistics from './PoliticsResultStatistics.svelte';
  import PoliticsResultTables from './PoliticsResultTables.svelte';
  import ReformSingleSimulationSeats from './reform-2026/ReformSingleSimulationSeats.svelte';

  let { record, client }: { record: StoredResult; client: ResultServiceClient } = $props();
  let inspection = $state.raw<ResultInspection>();
  function inspect(simulation: number, chamber?: string) {
    inspection = { simulation, chamber, revision: (inspection?.revision ?? 0) + 1 };
  }
</script>

<div class="session-results">
  <DiagnosticMessages {client} result={record.result} owner={record.owner} onInspect={inspect} />
  <PoliticsResultStatistics
    {client}
    resultId={record.id}
    owner={record.owner}
    scrutinyId={record.politics!.run.execution.scrutiny.id}
  />
  {#if record.politics!.run.execution.scrutiny.id === 'politiche-reform-2026-v1'}
    <ReformSingleSimulationSeats {record} {client} {inspection} />
  {/if}
  <PoliticsResultTables {record} {client} {inspection} />
</div>

<style>
  .session-results {
    min-width: 0;
  }
</style>
