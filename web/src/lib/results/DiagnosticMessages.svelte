<script lang="ts">
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { SimulationResult } from '../core/types';
  import GeneralMessages from './GeneralMessages.svelte';
  import SimulationMessages from './SimulationMessages.svelte';

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
</script>

<div class="messages" aria-label="Avvisi riforma">
  <GeneralMessages {client} {result} {owner} />
  <SimulationMessages {client} {result} {owner} {onInspect} />
</div>

<style>
  .messages { min-inline-size: 0; margin-block: var(--space-4); }
</style>
