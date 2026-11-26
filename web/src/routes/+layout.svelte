<script lang="ts">
  import type { Snippet } from 'svelte';
  import { provideStatus } from '$lib/app-status.svelte';
  import AppStatus from '$lib/AppStatus.svelte';
  import { page } from '$app/state';
  import { resolve } from '$app/paths';
  import { House } from '@lucide/svelte';
  import '$lib/ui/token.css';
  import '$lib/ui/base.css';
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
  .home-button { display: inline-flex; align-items: center; justify-content: center; width: 2.25rem; height: 2.25rem; border-radius: var(--radius-control); color: inherit; }
  .home-button:hover { background: var(--color-action-hover); }
  .site-main { flex: 1 0 auto; padding-block: 1rem 3rem; }
  .site-main.landing {
    display: grid;
    grid-template-columns: minmax(0, 1fr);
    align-content: center;
    padding-block: 2rem;
  }
  .site-footer { min-height: 2.75rem; display: flex; align-items: center; padding-block: var(--space-2); border-top: 1px solid var(--color-border); font-size: var(--text-small); text-align: center; }
  .site-footer a { color: var(--color-text-muted); }
</style>
