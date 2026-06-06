<script lang="ts">
  import { ArrowRight } from '@lucide/svelte';
</script>

<svelte:head>
  <title>Simulatore elezioni</title>
</svelte:head>

<main class="selector" aria-label="Selezione simulazione">
  <section class="intro">
    <p class="eyebrow">Simulatore elezioni</p>
    <h1>Scegli il flusso di simulazione</h1>
  </section>

  <nav class="workflow-grid" aria-label="Tipi di elezione">
    <a class="workflow" href="/politics">
      <span>
        <strong>Politiche</strong>
        <small>Camera e Senato, scenario nazionale e risultati per ramo.</small>
      </span>
      <ArrowRight size={20} aria-hidden="true" />
    </a>

    <a class="workflow" href="/emilia-romagna">
      <span>
        <strong>Regionali Emilia-Romagna</strong>
        <small>Consiglio regionale, coalizioni, liste provinciali e premio di maggioranza.</small>
      </span>
      <ArrowRight size={20} aria-hidden="true" />
    </a>
  </nav>
</main>

<style>
  :global(body) {
    margin: 0;
    min-height: 100vh;
    font-family:
      Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    color: #15232d;
    background: #f4f7f8;
  }

  .selector {
    min-height: 100vh;
    display: grid;
    align-content: center;
    gap: 2rem;
    width: min(960px, calc(100% - 2rem));
    margin: 0 auto;
    padding: 2rem 0;
  }

  .intro {
    display: grid;
    gap: 0.45rem;
  }

  .eyebrow {
    margin: 0;
    font-size: 0.8rem;
    text-transform: uppercase;
    color: #5e6f7b;
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
    border: 1px solid #d7e0e5;
    border-radius: 8px;
    background: #ffffff;
    color: inherit;
    text-decoration: none;
    box-shadow: 0 10px 30px rgba(25, 46, 61, 0.08);
  }

  .workflow:hover {
    border-color: #8aa7b8;
  }

  .workflow span {
    display: grid;
    gap: 0.5rem;
  }

  .workflow strong {
    font-size: 1.25rem;
  }

  .workflow small {
    color: #5e6f7b;
    line-height: 1.45;
  }

  @media (max-width: 760px) {
    .workflow-grid {
      grid-template-columns: 1fr;
    }
  }
</style>
