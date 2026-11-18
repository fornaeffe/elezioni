<script lang="ts">
  import type { Snippet } from 'svelte';
  import { provideStatus } from '$lib/app-status.svelte';
  import AppStatus from '$lib/AppStatus.svelte';
  import { page } from '$app/state';
  import { resolve } from '$app/paths';
  import { House } from '@lucide/svelte';
  const status = provideStatus();
  let { children }: { children: Snippet } = $props();
</script>
<AppStatus {status} />
<div class="app-shell">
  <header class="site-header">
    {#if page.route.id !== '/'}
      <a class="home-button" href={resolve('/')} aria-label="Home" title="Home">
        <House size={19} aria-hidden="true" />
      </a>
    {/if}
  </header>
  <div class="site-main">{@render children()}</div>
  <footer class="site-footer">
    <a href="https://github.com/fornaeffe/elezioni" target="_blank" rel="noreferrer">GitHub repository</a>
  </footer>
</div>
<style>
  :global(body) { margin: 0; min-height: 100vh; background: #f4f7f8; color: #15232d; font-family: system-ui, sans-serif; }
  :global(*), :global(*::before), :global(*::after) { box-sizing: border-box; }
  .app-shell { min-height: 100vh; display: flex; flex-direction: column; }
  .site-header { min-height: 3.25rem; padding: 0.45rem max(1rem, calc((100vw - 1200px) / 2)); display: flex; align-items: center; }
  .home-button { display: inline-flex; align-items: center; justify-content: center; width: 2.25rem; height: 2.25rem; border-radius: 6px; color: inherit; }
  .home-button:hover { background: #e5ecef; }
  .home-button:focus-visible, .site-footer a:focus-visible { outline: 2px solid #315f75; outline-offset: 2px; }
  .site-main { flex: 1; }
  .site-footer { min-height: 2.75rem; display: flex; align-items: center; justify-content: center; padding: 0.5rem 1rem; border-top: 1px solid #d7e0e5; font-size: 0.85rem; }
  .site-footer a { color: #405b6a; }
</style>
