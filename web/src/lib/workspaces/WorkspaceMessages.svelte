<script lang="ts">
  import Notice from '$lib/ui/Notice.svelte';
  import type { SimulationResult } from '../core/types';
  let { messages = [], result }: { messages?: { code?: string; message: string; severity?: 'info' | 'warning' | 'error' }[]; result?: SimulationResult | null } = $props();
  // Action feedback (including reset/import) must not hide the interpretation
  // warnings attached to the executed result still displayed underneath.
  const combined = $derived([...messages, ...(result?.warnings ?? []).filter(w => !messages.some(m => m.code === w.code && m.message === w.message && (m.severity ?? 'warning') === (w.severity ?? 'warning')))]);
  const groups = $derived([
    { label: 'Note simulazione', severity: 'info', values: combined.filter(m => m.severity === 'info') },
    { label: 'Avvisi simulazione', severity: 'warning', values: combined.filter(m => m.severity !== 'info' && m.severity !== 'error') },
    { label: 'Errori simulazione', severity: 'error', values: combined.filter(m => m.severity === 'error') },
  ] as const);
</script>
{#if combined.length}
  <div class="messages">
    {#each groups as group (group.severity)}
      {#if group.values.length}
        <Notice as="section" severity={group.severity} aria-label={group.label} role={group.severity === 'error' ? 'alert' : undefined}>
          {#each group.values as message, i (i)}<p>{#if message.code}<strong>{message.code}</strong>: {/if}{message.message}</p>{/each}
        </Notice>
      {/if}
    {/each}
  </div>
{/if}
<style>
  .messages { display: grid; gap: var(--space-3); }
  p { margin-block: var(--space-1); }
</style>
