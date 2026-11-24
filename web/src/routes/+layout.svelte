<script lang="ts">
  import type { Snippet } from 'svelte';
  import { provideStatus } from '$lib/app-status.svelte';
  import AppStatus from '$lib/AppStatus.svelte';
  import { page } from '$app/state';
  import { resolve } from '$app/paths';
  import { House } from '@lucide/svelte';
  const status = provideStatus();
  let { children }: { children: Snippet } = $props();
  const isLanding = $derived(page.route.id === '/');
</script>
<AppStatus {status} />
<div class="app-shell">
  {#if !isLanding}
    <header class="site-header page-container">
      <a class="home-button" href={resolve('/')} aria-label="Home" title="Home">
        <House size={19} aria-hidden="true" />
      </a>
    </header>
  {/if}
  <main class={['site-main', 'page-container', { landing: isLanding }]} aria-label={isLanding ? 'Selezione simulazione' : undefined}>
    {@render children()}
  </main>
  <footer class="site-footer">
    <div class="page-container">
      <a href="https://github.com/fornaeffe/elezioni" target="_blank" rel="noreferrer">GitHub repository</a>
    </div>
  </footer>
</div>
<style>
  :global(body) { margin: 0; background: #f4f7f8; color: #15232d; font-family: system-ui, sans-serif; }
  :global(*), :global(*::before), :global(*::after) { box-sizing: border-box; }

  .app-shell {
    --page-content-width: 90rem;
    --page-gutter: clamp(0.75rem, 2vw, 1.5rem);
    min-block-size: 100vh;
    min-block-size: 100dvh;
    display: flex;
    flex-direction: column;
  }

  .page-container {
    inline-size: min(var(--page-content-width), calc(100% - 2 * var(--page-gutter)));
    margin-inline: auto;
  }

  .site-header { min-height: 3.25rem; padding-block: 0.45rem; display: flex; align-items: center; }
  .home-button { display: inline-flex; align-items: center; justify-content: center; width: 2.25rem; height: 2.25rem; border-radius: 6px; color: inherit; }
  .home-button:hover { background: #e5ecef; }
  .home-button:focus-visible, .site-footer a:focus-visible { outline: 2px solid #315f75; outline-offset: 2px; }
  .site-main { flex: 1 0 auto; padding-block: 1rem 3rem; }
  .site-main.landing {
    display: grid;
    grid-template-columns: minmax(0, 1fr);
    align-content: center;
    padding-block: 2rem;
  }
  .site-footer { min-height: 2.75rem; display: flex; align-items: center; padding-block: 0.5rem; border-top: 1px solid #d7e0e5; font-size: 0.85rem; text-align: center; }
  .site-footer a { color: #405b6a; }
</style>
