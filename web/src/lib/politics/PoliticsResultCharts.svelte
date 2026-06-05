<script lang="ts">
  import { X } from '@lucide/svelte';
  import type {
    PoliticsPlurinominalChartOption,
    PoliticsResultChart,
    PoliticsSpinePanel,
    PoliticsSpineResultChart
  } from './result-charts';

  interface Props {
    charts: PoliticsResultChart[];
    plurinominalOptions: PoliticsPlurinominalChartOption[];
    selectedPlurinominalOption: PoliticsPlurinominalChartOption | null;
    plurinominalChart: PoliticsSpineResultChart | null;
    onSelectPlurinominalOption: (id: string) => void;
  }

  let {
    charts,
    plurinominalOptions,
    selectedPlurinominalOption,
    plurinominalChart,
    onSelectPlurinominalOption
  }: Props = $props();

  interface EnlargedSpinogram {
    title: string;
    xLabel: string;
    panel: PoliticsSpinePanel;
  }

  const scatterLeft = 14;
  const scatterTop = 7;
  const scatterWidth = 80;
  const scatterHeight = 49;
  const scatterBottom = scatterTop + scatterHeight;

  let enlargedSpinogram = $state<EnlargedSpinogram | null>(null);

  function bounded(value: number): number {
    return Math.max(0, Math.min(100, value));
  }

  function pct(value: number): string {
    return `${bounded(value)}%`;
  }

  function scatterX(value: number): number {
    return scatterLeft + (bounded(value) / 100) * scatterWidth;
  }

  function scatterY(value: number): number {
    return scatterBottom - (bounded(value) / 100) * scatterHeight;
  }

  function detailOpen(chart: PoliticsResultChart): boolean {
    return chart.kind === 'bar' || chart.kind === 'boxplot';
  }

  function openSpinogram(title: string, xLabel: string, panel: PoliticsSpinePanel): void {
    enlargedSpinogram = { title, xLabel, panel };
  }

  function closeSpinogram(): void {
    enlargedSpinogram = null;
  }

  function handleKeydown(event: KeyboardEvent): void {
    if (event.key === 'Escape') closeSpinogram();
  }
</script>

<svelte:window onkeydown={handleKeydown} />

<div class="result-charts">
  {#each charts as chart}
    <details
      class:wide-chart={chart.kind === 'scatter' || chart.kind === 'spine'}
      open={detailOpen(chart)}
      role="region"
      aria-label={chart.title}
    >
      <summary>{chart.title}</summary>

      {#if chart.kind === 'bar'}
        <div class="bar-chart">
          {#each chart.groups as group}
            <section class="chart-group" aria-label={group.label}>
              <h4>{group.label}</h4>
              <div class="bar-list">
                {#each group.bars as bar}
                  <div class="bar-row">
                    <span class="bar-label" title={bar.label}>{bar.label}</span>
                    <span class="bar-track" aria-hidden="true">
                      <span class="bar-fill" style={`width: ${pct(bar.widthPercent)}; background-color: ${bar.color};`}
                      ></span>
                    </span>
                    <span class="bar-value">{bar.displayValue}</span>
                  </div>
                {/each}
              </div>
            </section>
          {/each}
        </div>
      {:else if chart.kind === 'boxplot'}
        <div class="boxplot-chart">
          {#each chart.panels as panel}
            <section class="boxplot-panel" aria-label={panel.label}>
              <h4>{panel.label}</h4>
              <div class="boxplot-axis" aria-hidden="true">
                {#each panel.ticks as tick}
                  <span class="axis-tick" style={`left: ${pct(tick.positionPercent)};`}>
                    <span></span>
                    <em>{tick.label}</em>
                  </span>
                {/each}
              </div>
              <div class="boxplot-list">
                {#each panel.items as item}
                  <div class="boxplot-row">
                    <span class="boxplot-label" title={item.label}>{item.label}</span>
                    <span class="boxplot-area">
                      <span
                        class="boxplot-whisker"
                        style={`left: ${pct(item.whiskerLowPercent)}; width: ${pct(item.whiskerHighPercent - item.whiskerLowPercent)};`}
                      ></span>
                      <span
                        class="boxplot-box"
                        style={`left: ${pct(item.q1Percent)}; width: ${pct(item.q3Percent - item.q1Percent)}; border-color: ${item.color}; background-color: ${item.color}22;`}
                      ></span>
                      <span class="boxplot-median" style={`left: ${pct(item.medianPercent)}; background-color: ${item.color};`}
                      ></span>
                      <span class="boxplot-mean" style={`left: ${pct(item.averagePercent)}; border-color: ${item.color};`}
                      ></span>
                      {#each item.outlierPercents as outlier}
                        <span class="boxplot-outlier" style={`left: ${pct(outlier)}; border-color: ${item.color};`}
                        ></span>
                      {/each}
                    </span>
                  </div>
                {/each}
              </div>
              <p class="axis-label">{chart.xLabel}</p>
            </section>
          {/each}
        </div>
      {:else if chart.kind === 'scatter'}
        <div class="small-multiple-grid">
          {#each chart.panels as panel}
            <section class="small-panel" aria-label={panel.label}>
              <h4>{panel.label}</h4>
              <svg viewBox="0 0 100 68" role="img" aria-label={panel.label}>
                <line x1={scatterLeft} y1={scatterBottom} x2={scatterLeft + scatterWidth} y2={scatterBottom} class="axis" />
                <line x1={scatterLeft} y1={scatterTop} x2={scatterLeft} y2={scatterBottom} class="axis" />
                {#each panel.xTicks as tick}
                  <line
                    x1={scatterX(tick.positionPercent)}
                    y1={scatterBottom}
                    x2={scatterX(tick.positionPercent)}
                    y2={scatterBottom + 2}
                    class="axis"
                  />
                  <text x={scatterX(tick.positionPercent)} y="64" text-anchor="middle">{tick.label}</text>
                {/each}
                {#each panel.yTicks as tick}
                  <line
                    x1={scatterLeft - 2}
                    y1={scatterY(tick.positionPercent)}
                    x2={scatterLeft}
                    y2={scatterY(tick.positionPercent)}
                    class="axis"
                  />
                  <text x="11" y={scatterY(tick.positionPercent) + 1.6} text-anchor="end">{tick.label}</text>
                {/each}
                {#each panel.points as point}
                  <circle
                    cx={scatterX(point.xPercent)}
                    cy={scatterY(point.yPercent)}
                    r="1.6"
                    fill={panel.color}
                    opacity="0.14"
                  />
                {/each}
              </svg>
            </section>
          {/each}
        </div>
      {:else if chart.kind === 'spine'}
        <div class="small-multiple-grid">
          {#each chart.panels as panel}
            <section class="small-panel" aria-label={panel.label}>
              <h4>{panel.label}</h4>
              <button
                type="button"
                class="spine-panel-button"
                onclick={() => openSpinogram(chart.title, chart.xLabel, panel)}
                aria-label={panel.label}
                title={panel.label}
              >
                <div class="spine-plot" role="img" aria-label={panel.label}>
                  {#each panel.bins as bin}
                    <span
                      class="spine-bin"
                      title={bin.label}
                      style={`left: ${pct(bin.xPercent)}; width: ${pct(bin.widthPercent)};`}
                    >
                      {#each bin.cells as cell}
                        <span
                          class="spine-cell"
                          style={`top: ${pct(cell.yPercent)}; height: ${pct(cell.heightPercent)}; background-color: ${cell.color}; color: ${cell.textColor};`}
                        >
                          {#if cell.label}
                            <span>{cell.label}</span>
                          {/if}
                        </span>
                      {/each}
                    </span>
                  {/each}
                </div>
              </button>
              <div class="spine-axis" aria-hidden="true">
                {#each panel.ticks as tick}
                  <span class="spine-axis-tick" style={`left: ${pct(tick.positionPercent)};`}>
                    <span></span>
                    <em>{tick.label}</em>
                  </span>
                {/each}
              </div>
              <p class="axis-label">{chart.xLabel}</p>
            </section>
          {/each}
        </div>
      {/if}
    </details>
  {/each}

  {#if plurinominalOptions.length > 0 && plurinominalChart}
    <details class="wide-chart" open role="region" aria-label={plurinominalChart.title}>
      <summary>{plurinominalChart.title}</summary>
      <div class="pluri-control">
        <label>
          Collegio
          <select
            value={selectedPlurinominalOption?.id ?? ''}
            onchange={(event) => onSelectPlurinominalOption((event.currentTarget as HTMLSelectElement).value)}
          >
            {#each plurinominalOptions as option}
              <option value={option.id}>{option.label}</option>
            {/each}
          </select>
        </label>
      </div>
      <div class="small-multiple-grid single">
        {#each plurinominalChart.panels as panel}
          <section class="small-panel" aria-label={panel.label}>
            <h4>{panel.label}</h4>
            <button
              type="button"
              class="spine-panel-button"
              onclick={() => openSpinogram(plurinominalChart.title, plurinominalChart.xLabel, panel)}
              aria-label={panel.label}
              title={panel.label}
            >
              <div class="spine-plot large" role="img" aria-label={panel.label}>
                {#each panel.bins as bin}
                  <span
                    class="spine-bin"
                    title={bin.label}
                    style={`left: ${pct(bin.xPercent)}; width: ${pct(bin.widthPercent)};`}
                  >
                    {#each bin.cells as cell}
                      <span
                        class="spine-cell"
                        style={`top: ${pct(cell.yPercent)}; height: ${pct(cell.heightPercent)}; background-color: ${cell.color}; color: ${cell.textColor};`}
                      >
                        {#if cell.label}
                          <span>{cell.label}</span>
                        {/if}
                      </span>
                    {/each}
                  </span>
                {/each}
              </div>
            </button>
            <div class="spine-axis" aria-hidden="true">
              {#each panel.ticks as tick}
                <span class="spine-axis-tick" style={`left: ${pct(tick.positionPercent)};`}>
                  <span></span>
                  <em>{tick.label}</em>
                </span>
              {/each}
            </div>
          </section>
        {/each}
      </div>
    </details>
  {/if}
</div>

{#if enlargedSpinogram}
  <div class="overlay" role="presentation">
    <button type="button" class="overlay-backdrop" onclick={closeSpinogram} aria-label="Chiudi"></button>
    <div class="overlay-panel" role="dialog" aria-modal="true" aria-label={enlargedSpinogram.panel.label}>
      <div class="overlay-heading">
        <div>
          <h3>{enlargedSpinogram.title}</h3>
          <p>{enlargedSpinogram.panel.label}</p>
        </div>
        <button type="button" class="close-button" onclick={closeSpinogram} aria-label="Chiudi" title="Chiudi">
          <X size={20} aria-hidden="true" />
        </button>
      </div>
      <div class="spine-plot enlarged" role="img" aria-label={enlargedSpinogram.panel.label}>
        {#each enlargedSpinogram.panel.bins as bin}
          <span
            class="spine-bin"
            title={bin.label}
            style={`left: ${pct(bin.xPercent)}; width: ${pct(bin.widthPercent)};`}
          >
            {#each bin.cells as cell}
              <span
                class="spine-cell enlarged-cell"
                style={`top: ${pct(cell.yPercent)}; height: ${pct(cell.heightPercent)}; background-color: ${cell.color}; color: ${cell.textColor};`}
              >
                {#if cell.label}
                  <span>{cell.label}</span>
                {/if}
              </span>
            {/each}
          </span>
        {/each}
      </div>
      <div class="spine-axis enlarged-axis" aria-hidden="true">
        {#each enlargedSpinogram.panel.ticks as tick}
          <span class="spine-axis-tick" style={`left: ${pct(tick.positionPercent)};`}>
            <span></span>
            <em>{tick.label}</em>
          </span>
        {/each}
      </div>
      <p class="axis-label">{enlargedSpinogram.xLabel}</p>
    </div>
  </div>
{/if}

<style>
  .result-charts {
    display: grid;
    gap: 16px;
    padding: 16px 16px 0;
  }

  details {
    min-width: 0;
    border-bottom: 1px solid #e5e9ed;
    padding-bottom: 14px;
  }

  summary {
    cursor: pointer;
    color: #4d5963;
    font-size: 14px;
    font-weight: 750;
  }

  .bar-chart,
  .boxplot-chart {
    display: grid;
    gap: 14px;
    margin-top: 12px;
  }

  .chart-group,
  .boxplot-panel,
  .small-panel {
    display: grid;
    gap: 8px;
    min-width: 0;
  }

  h4 {
    margin: 0;
    color: #697681;
    font-size: 12px;
    font-weight: 750;
    text-transform: uppercase;
  }

  .bar-list {
    display: grid;
    gap: 7px;
  }

  .bar-row {
    display: grid;
    grid-template-columns: minmax(120px, 0.32fr) minmax(120px, 1fr) 64px;
    gap: 8px;
    align-items: center;
    min-height: 22px;
  }

  .bar-label,
  .boxplot-label {
    overflow: hidden;
    color: #34414a;
    font-size: 12px;
    font-weight: 650;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .bar-track {
    display: block;
    overflow: hidden;
    height: 12px;
    border-radius: 999px;
    background: #e8edf1;
  }

  .bar-fill {
    display: block;
    height: 100%;
    min-width: 2px;
  }

  .bar-value {
    color: #4d5963;
    font-size: 12px;
    font-variant-numeric: tabular-nums;
    font-weight: 700;
    text-align: right;
  }

  .boxplot-axis {
    position: relative;
    height: 24px;
    margin-left: min(180px, 34%);
    border-bottom: 1px solid #dbe2e7;
  }

  .axis-tick {
    position: absolute;
    top: 0;
    display: grid;
    justify-items: center;
    transform: translateX(-50%);
  }

  .axis-tick span {
    width: 1px;
    height: 7px;
    background: #c8d0d7;
  }

  .axis-tick em {
    color: #697681;
    font-size: 11px;
    font-style: normal;
    font-variant-numeric: tabular-nums;
  }

  .boxplot-list {
    display: grid;
    gap: 6px;
  }

  .boxplot-row {
    display: grid;
    grid-template-columns: minmax(100px, 180px) minmax(140px, 1fr);
    gap: 10px;
    align-items: center;
    min-height: 24px;
  }

  .boxplot-area {
    position: relative;
    display: block;
    height: 22px;
  }

  .boxplot-whisker,
  .boxplot-box,
  .boxplot-median,
  .boxplot-mean,
  .boxplot-outlier {
    position: absolute;
    top: 50%;
    transform: translateY(-50%);
  }

  .boxplot-whisker {
    height: 1px;
    background: #72808a;
  }

  .boxplot-box {
    height: 14px;
    border: 1px solid;
    border-radius: 3px;
  }

  .boxplot-median {
    width: 2px;
    height: 18px;
  }

  .boxplot-mean,
  .boxplot-outlier {
    width: 7px;
    height: 7px;
    border: 1px solid;
    border-radius: 50%;
    background: #ffffff;
  }

  .axis-label {
    margin: 2px 0 0;
    color: #697681;
    font-size: 11px;
    text-align: right;
  }

  .small-multiple-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(230px, 1fr));
    gap: 14px;
    margin-top: 12px;
  }

  .small-multiple-grid.single {
    grid-template-columns: 1fr;
  }

  .small-panel {
    border: 1px solid #e1e6ea;
    border-radius: 6px;
    padding: 10px;
  }

  svg {
    width: 100%;
    min-height: 160px;
  }

  text {
    fill: #697681;
    font-size: 4px;
    font-variant-numeric: tabular-nums;
  }

  .axis {
    stroke: #aeb8c0;
    stroke-width: 0.35;
  }

  .spine-plot {
    position: relative;
    overflow: hidden;
    height: 150px;
    border: 1px solid #cfd7de;
    background: #f7f9fa;
  }

  .spine-plot.large {
    height: 240px;
  }

  .spine-plot.enlarged {
    height: min(68vh, 620px);
  }

  .spine-panel-button {
    display: block;
    width: 100%;
    border: 0;
    background: transparent;
    padding: 0;
    cursor: zoom-in;
    font: inherit;
  }

  .spine-panel-button:focus-visible {
    outline: 2px solid #2f6f57;
    outline-offset: 3px;
  }

  .spine-bin,
  .spine-cell {
    position: absolute;
    display: block;
  }

  .spine-bin {
    top: 0;
    bottom: 0;
    border-right: 1px solid rgba(255, 255, 255, 0.75);
  }

  .spine-cell {
    right: 0;
    left: 0;
    display: grid;
    place-items: center;
    min-height: 8px;
    font-size: 11px;
    font-weight: 750;
  }

  .enlarged-cell {
    font-size: 16px;
  }

  .spine-axis {
    position: relative;
    height: 28px;
    border-top: 1px solid #cfd7de;
  }

  .spine-axis.enlarged-axis {
    height: 34px;
  }

  .spine-axis-tick {
    position: absolute;
    top: 0;
    display: grid;
    justify-items: center;
    transform: translateX(-50%);
  }

  .spine-axis-tick span {
    width: 1px;
    height: 8px;
    background: #aeb8c0;
  }

  .spine-axis-tick em {
    color: #697681;
    font-size: 10px;
    font-style: normal;
    font-variant-numeric: tabular-nums;
    white-space: nowrap;
  }

  .pluri-control {
    max-width: 520px;
    margin-top: 12px;
  }

  label {
    display: grid;
    gap: 6px;
    color: #4d5963;
    font-size: 13px;
    font-weight: 650;
  }

  select {
    min-height: 38px;
    border: 1px solid #c9d0d6;
    border-radius: 6px;
    background: #ffffff;
    color: #182026;
    padding: 0 10px;
    font: inherit;
  }

  .overlay {
    position: fixed;
    inset: 0;
    z-index: 20;
    display: grid;
    place-items: center;
    background: rgba(24, 32, 38, 0.58);
    padding: 28px;
  }

  .overlay-backdrop {
    position: absolute;
    inset: 0;
    border: 0;
    background: transparent;
    padding: 0;
  }

  .overlay-panel {
    position: relative;
    z-index: 1;
    display: grid;
    gap: 14px;
    width: min(1180px, 100%);
    max-height: calc(100vh - 56px);
    overflow: auto;
    border: 1px solid #d8dee3;
    border-radius: 8px;
    background: #ffffff;
    padding: 16px;
    box-shadow: 0 24px 70px rgba(24, 32, 38, 0.28);
  }

  .overlay-heading {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
  }

  .overlay-heading h3,
  .overlay-heading p {
    margin: 0;
  }

  .overlay-heading h3 {
    color: #182026;
    font-size: 17px;
    font-weight: 750;
  }

  .overlay-heading p {
    margin-top: 3px;
    color: #697681;
    font-size: 13px;
    font-weight: 650;
  }

  .close-button {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 38px;
    min-width: 38px;
    height: 38px;
    border: 1px solid #bdc7d0;
    border-radius: 6px;
    background: #ffffff;
    color: #182026;
    cursor: pointer;
  }

  @media (min-width: 1120px) {
    .result-charts {
      grid-template-columns: repeat(2, minmax(0, 1fr));
    }

    .wide-chart {
      grid-column: 1 / -1;
    }
  }

  @media (max-width: 720px) {
    .bar-row {
      grid-template-columns: minmax(0, 1fr) 64px;
      gap: 4px 8px;
    }

    .bar-label {
      grid-column: 1 / -1;
    }

    .boxplot-axis {
      margin-left: 0;
    }

    .boxplot-row {
      grid-template-columns: 1fr;
      gap: 2px;
    }

    .overlay {
      padding: 12px;
    }

    .overlay-panel {
      max-height: calc(100vh - 24px);
    }
  }
</style>
