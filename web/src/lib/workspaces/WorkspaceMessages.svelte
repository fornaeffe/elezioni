<script lang="ts">
  import type { SimulationResult } from '../core/types';
  let { messages = [], result }: { messages?: { code?: string; message: string; severity?: 'info' | 'warning' | 'error' }[]; result?: SimulationResult | null } = $props();
  // Action feedback (including reset/import) must not hide the interpretation
  // warnings attached to the executed result still displayed underneath.
  const combined = $derived([...messages, ...(result?.warnings ?? []).filter(w => !messages.some(m => m.code === w.code && m.message === w.message && (m.severity ?? 'warning') === (w.severity ?? 'warning')))]);
  const groups = $derived([
    { label: 'Note simulazione', severity: 'info', values: combined.filter(m => m.severity === 'info') },
    { label: 'Avvisi simulazione', severity: 'warning', values: combined.filter(m => m.severity !== 'info' && m.severity !== 'error') },
    { label: 'Errori simulazione', severity: 'error', values: combined.filter(m => m.severity === 'error') },
  ]);
</script>
{#each groups as group (group.severity)}
  {#if group.values.length}<section class={group.severity} aria-label={group.label} role={group.severity === 'error' ? 'alert' : undefined}>
    {#each group.values as message, i (i)}<p>{#if message.code}<strong>{message.code}</strong>: {/if}{message.message}</p>{/each}
  </section>{/if}
{/each}
<style>
  section { padding: .75rem 1rem; border-radius: .4rem; margin: .75rem 0; overflow-wrap: anywhere; }
  .info { background: #edf6fb; border: 1px solid #c6dfef; }
  .warning { background: #fff8e5; border: 1px solid #e7d69a; }
  .error { background: #fff0ed; border: 1px solid #e8bbb2; }
  p { margin: .35rem 0; }
</style>
