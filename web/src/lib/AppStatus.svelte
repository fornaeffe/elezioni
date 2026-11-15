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
<aside class={{ ready: kind === 'ready', working: kind === 'working', error: kind === 'error' }} aria-label="Stato dell'applicazione">
  <button class="toggle" aria-expanded={open} aria-label={`Stato: ${label}`} onclick={() => open = !open}>
    {#if kind === 'error'}<CircleAlert size={20} aria-hidden="true" />{:else if kind === 'working'}<span class="working-icon" aria-hidden="true"><LoaderCircle size={20} /></span>{:else}<CircleCheck size={20} aria-hidden="true" />{/if}
    <span class="toggle-label">{label}</span>
  </button>
  <div class="announcement" role="status" aria-live="polite">{kind === 'error' ? 'Errore' : kind === 'working' ? 'Operazione in corso' : 'Pronto'}</div>
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
  aside { position: fixed; top: max(.5rem, env(safe-area-inset-top)); right: max(.5rem, env(safe-area-inset-right)); z-index: 100; border: 1px solid; border-radius: .5rem; box-shadow: 0 2px 8px #17304240; color: #172b23; max-width: min(24rem, calc(100vw - 1rem)); font: 1rem/1.4 system-ui, sans-serif; }
  .ready { background: #e4f5e9; border-color: #388451; } .working { background: #fff3c9; border-color: #a37b00; } .error { background: #ffe5e3; border-color: #b33128; }
  .toggle { display: flex; align-items: center; gap: .5rem; width: 100%; text-align: left; border: 0; background: transparent; color: inherit; font: inherit; font-weight: 600; padding: .6rem; cursor: pointer; }
  .toggle-label { overflow-wrap: anywhere; } .content { padding: 0 .75rem .75rem; max-height: min(60vh, 30rem); overflow: auto; } .entry { padding: .4rem 0; overflow-wrap: anywhere; } p { margin: .25rem 0; white-space: pre-wrap; } .content button { font: inherit; margin: .2rem .3rem .2rem 0; } button:focus-visible { outline: 3px solid #176694; outline-offset: 2px; }
  .announcement { position: absolute; width: 1px; height: 1px; overflow: hidden; clip-path: inset(50%); }
  .working-icon { display: inline-flex; animation: status-spin 1s linear infinite; }
  @keyframes status-spin { to { transform: rotate(360deg); } }
  @media (prefers-reduced-motion: reduce) { .working-icon { animation: none; } }
  @media (max-width: 650px) { .toggle-label { display: none; } .toggle { width: auto; margin-left: auto; } }
</style>

