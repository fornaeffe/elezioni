<script lang="ts">
  import type { RegionalErResultChart } from './result-charts';

  interface Props {
    charts: RegionalErResultChart[];
  }

  let { charts }: Props = $props();
</script>

{#if charts.length > 0}
  <div class="charts">
    {#each charts as chart (chart.id)}
      <section class="chart" aria-label={chart.title}>
        <h3>{chart.title}</h3>
        <div class="bars">
          {#each chart.items as item (item.label)}
            {@const width = Math.max(2, Math.min(100, item.value))}
            <div class="bar-row">
              <span class="label">{item.label}</span>
              <div class="track">
                <div class="fill" style={`width: ${width}%; background: ${item.color};`}></div>
              </div>
              <span class="value">{item.value.toFixed(1)} {chart.unit}</span>
            </div>
          {/each}
        </div>
      </section>
    {/each}
  </div>
{/if}

<style>
  .charts {
    display: grid;
    gap: 1rem;
  }

  .chart {
    display: grid;
    gap: 0.75rem;
  }

  .chart h3 {
    margin: 0;
    font-size: 1rem;
  }

  .bars {
    display: grid;
    gap: 0.55rem;
  }

  .bar-row {
    display: grid;
    grid-template-columns: minmax(8rem, 1fr) minmax(8rem, 2fr) minmax(5rem, auto);
    gap: 0.75rem;
    align-items: center;
  }

  .label,
  .value {
    font-size: 0.85rem;
  }

  .value {
    text-align: right;
    font-variant-numeric: tabular-nums;
  }

  .track {
    height: 0.7rem;
    overflow: hidden;
    background: #e6edf1;
    border-radius: 999px;
  }

  .fill {
    height: 100%;
    border-radius: inherit;
  }

  @media (max-width: 720px) {
    .bar-row {
      grid-template-columns: 1fr;
      gap: 0.3rem;
    }

    .value {
      text-align: left;
    }
  }
</style>
