<script lang="ts">
  import { electionWorkspaces } from '$lib/workspaces/registry';
  import { resolve } from '$app/paths';
  import { ArrowRight } from '@lucide/svelte';
</script>

<svelte:head>
  <title>Simulatore elezioni</title>
</svelte:head>

<div class="selector">
  <section class="intro">
    <p class="eyebrow">Simulatore elezioni</p>
    <h1>Scegli il flusso di simulazione</h1>
  </section>

  <nav class="workflow-grid" aria-label="Tipi di elezione">
    {#each electionWorkspaces as item (item.id)}
      <a class="workflow" href={resolve(item.route)}>
        <span><strong>{item.name}</strong>
          <small>{item.kind === 'politiche' ? 'Camera e Senato, scenario nazionale e risultati per ramo.' : 'Consiglio regionale, coalizioni, liste provinciali e premio di maggioranza.'}</small>
        </span><ArrowRight size={20} aria-hidden="true" />
      </a>
    {/each}
  </nav>
</div>

<style>

  .selector {
    display: grid;
    gap: 2rem;
  }

  .intro {
    display: grid;
    gap: 0.45rem;
  }

  .eyebrow {
    margin: 0;
    font-size: 0.8rem;
    text-transform: uppercase;
    color: var(--color-text-muted);
    font-weight: 700;
  }

  h1 {
    margin: 0;
    font-size: clamp(2rem, 5vw, 3.25rem);
    line-height: 1.05;
    letter-spacing: 0;
  }

  .workflow-grid {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 1rem;
  }

  .workflow {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 1rem;
    min-height: 9rem;
    padding: 1.25rem;
    border: 1px solid var(--color-border);
    border-radius: 8px;
    background: var(--color-surface);
    color: inherit;
    text-decoration: none;
    box-shadow: 0 10px 30px rgba(25, 46, 61, 0.08);
  }

  .workflow:hover {
    border-color: var(--color-control-border);
  }

  .workflow span {
    display: grid;
    gap: 0.5rem;
  }

  .workflow strong {
    font-size: 1.25rem;
  }

  .workflow small {
    color: var(--color-text-muted);
    line-height: 1.45;
  }

  @media (max-width: 760px) {
    .workflow-grid {
      grid-template-columns: 1fr;
    }
  }
</style>
