<script lang="ts">
  import { CircleCheck, LoaderCircle, CircleAlert } from '@lucide/svelte';
  import type { AppStatus } from './app-status.svelte';
  let { status }: { status: AppStatus } = $props();
  let open = $state(false);
  let entries = $derived(Object.entries(status.entries));
  let kind = $derived(entries.some(([, e]) => e.state === 'error') ? 'error' : entries.some(([, e]) => e.state === 'working') ? 'working' : 'ready');
  let selected = $derived(entries.find(([, e]) => e.state === kind)?.[1]);
  let label = $derived(selected?.label ?? 'Pronto');
</script>
<aside data-state={kind} aria-label="Stato dell'applicazione">
  <button class="toggle" aria-expanded={open} aria-label={`Stato: ${label}`} onclick={() => open = !open}>
    {#if kind === 'error'}<CircleAlert size={20} aria-hidden="true" />{:else if kind === 'working'}<span class="working-icon" aria-hidden="true"><LoaderCircle size={20} /></span>{:else}<CircleCheck size={20} aria-hidden="true" />{/if}
    <span class="toggle-label">{label}</span>
  </button>
  <div class="visually-hidden" role="status" aria-live="polite">{kind === 'error' ? 'Errore' : kind === 'working' ? 'Operazione in corso' : 'Pronto'}</div>
    <div class="content" hidden={!open}>
      {#if entries.length === 0}<p>Pronto</p>{/if}
      {#each entries as [id, entry] (id)}
        <div class="entry"><strong>{entry.label}</strong>{#if entry.detail}<p>{entry.detail}</p>{/if}
          {#if entry.cancel}<button onclick={entry.cancel}>Annulla</button>{/if}
          {#if entry.retry}<button onclick={entry.retry}>Riprova</button>{/if}
          {#if entry.state === 'error'}<button onclick={() => status.clear(id)}>Chiudi avviso</button>{/if}
        </div>
      {/each}
    </div>
</aside>
<style>
  aside {
    position: fixed;
    top: max(var(--space-2), env(safe-area-inset-top));
    right: max(var(--space-2), env(safe-area-inset-right));
    z-index: 100;
    max-inline-size: min(24rem, calc(100vw - 1rem));
    border: 1px solid;
    border-radius: var(--radius-panel);
    box-shadow: var(--shadow-overlay);
    line-height: 1.4;
  }
  [data-state=ready] { background: var(--color-success-surface); border-color: var(--color-success-border); }
  [data-state=working] { background: var(--color-warning-surface); border-color: var(--color-warning-border); }
  [data-state=error] { background: var(--color-error-surface); border-color: var(--color-error-border); }
  .toggle {
    display: flex;
    align-items: center;
    gap: var(--space-2);
    width: 100%;
    text-align: start;
    border: 0;
    background: transparent;
    font-weight: 600;
    padding: var(--space-2);
  }
  .toggle-label, .entry { overflow-wrap: anywhere; }
  .content { padding: 0 var(--space-3) var(--space-3); max-block-size: min(60vh, 30rem); overflow: auto; }
  .entry { padding-block: var(--space-2); }
  p { margin-block: var(--space-1); white-space: pre-wrap; }
  .content button { margin: var(--space-1) var(--space-1) var(--space-1) 0; }
  .working-icon { display: inline-flex; animation: status-spin 1s linear infinite; }
  @keyframes status-spin { to { transform: rotate(360deg); } }
  @media (prefers-reduced-motion: reduce) { .working-icon { animation: none; } }
  @media (max-width: 650px) { .toggle-label { display: none; } .toggle { width: auto; margin-left: auto; } }
</style>

