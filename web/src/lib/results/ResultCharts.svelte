<script lang="ts">
  import { tick, type Snippet } from 'svelte';
  import { X } from '@lucide/svelte';
  import ResultSection from './ResultSection.svelte';
  import type {
    CollegeChartOption,
    ResultChart,
    SpinePanel,
    SpineResultChart,
    BoxPlotItem
  } from './chart-types';

  interface Props {
    charts: ResultChart[];
    plurinominalOptions?: CollegeChartOption[];
    selectedPlurinominalOption?: CollegeChartOption | null;
    plurinominalChart?: SpineResultChart | null;
    onSelectPlurinominalOption?: (id: string) => void;
    outliers?: Snippet<[BoxPlotItem]>;
    initialOpen?: boolean;
    collapsible?: boolean;
  }

  let {
    charts,
    plurinominalOptions = [],
    selectedPlurinominalOption = null,
    plurinominalChart = null,
    onSelectPlurinominalOption = () => {},
    outliers,
    initialOpen,
    collapsible = true
  }: Props = $props();

  interface EnlargedSpinogram {
    title: string;
    xLabel: string;
    panel: SpinePanel;
  }

  const scatterLeft = 14;
  const scatterTop = 7;
  const scatterWidth = 80;
  const scatterHeight = 49;
  const scatterBottom = scatterTop + scatterHeight;

  let enlargedSpinogram = $state<EnlargedSpinogram | null>(null);
  let spinogramDialog: HTMLDialogElement;
  let spinogramTrigger: HTMLButtonElement;
  let pointerStartedOnBackdrop = false;

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

  function detailOpen(chart: ResultChart): boolean {
    return initialOpen ?? (chart.kind === 'bar' || chart.kind === 'boxplot');
  }

  async function openSpinogram(trigger: HTMLButtonElement, title: string, xLabel: string, panel: SpinePanel) {
    spinogramTrigger = trigger;
    enlargedSpinogram = { title, xLabel, panel };
    await tick();
    if (spinogramDialog?.isConnected && !spinogramDialog.open) spinogramDialog.showModal();
  }

  function closeSpinogram(): void {
    enlargedSpinogram = null;
    spinogramTrigger.focus({ preventScroll: true });
  }
</script>

<div class="result-charts">
  {#each charts as chart, index (index)}
    {#snippet chartContent()}
      {#if chart.kind === 'bar'}
        <div class="bar-chart">
          {#each chart.groups as group, index (index)}
            <section class="chart-group" aria-label={group.label}>
              <h4>{group.label}</h4>
              <div class="bar-list">
                {#each group.bars as bar, index (index)}
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
          {#each chart.panels as panel, index (index)}
            <section class="boxplot-panel" aria-label={panel.label}>
              <h4>{panel.label}</h4>
              <div class="boxplot-axis" aria-hidden="true">
                {#each panel.ticks as tick, index (index)}
                  <span class="axis-tick" style={`left: ${pct(tick.positionPercent)};`}>
                    <span></span>
                    <em>{tick.label}</em>
                  </span>
                {/each}
              </div>
              <div class="boxplot-list">
                {#each panel.items as item, index (index)}
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
                      {#if item.streamId && outliers}{@render outliers(item)}{/if}
                      {#each item.outlierPercents as outlier, index (index)}
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
          {#each chart.panels as panel, index (index)}
            <section class="small-panel" aria-label={panel.label}>
              <h4>{panel.label}</h4>
              <svg viewBox="0 0 100 68" role="img" aria-label={panel.label}>
                <line x1={scatterLeft} y1={scatterBottom} x2={scatterLeft + scatterWidth} y2={scatterBottom} class="axis" />
                <line x1={scatterLeft} y1={scatterTop} x2={scatterLeft} y2={scatterBottom} class="axis" />
                {#each panel.xTicks as tick, index (index)}
                  <line
                    x1={scatterX(tick.positionPercent)}
                    y1={scatterBottom}
                    x2={scatterX(tick.positionPercent)}
                    y2={scatterBottom + 2}
                    class="axis"
                  />
                  <text x={scatterX(tick.positionPercent)} y="64" text-anchor="middle">{tick.label}</text>
                {/each}
                {#each panel.yTicks as tick, index (index)}
                  <line
                    x1={scatterLeft - 2}
                    y1={scatterY(tick.positionPercent)}
                    x2={scatterLeft}
                    y2={scatterY(tick.positionPercent)}
                    class="axis"
                  />
                  <text x="11" y={scatterY(tick.positionPercent) + 1.6} text-anchor="end">{tick.label}</text>
                {/each}
                {#each panel.points as point, index (index)}
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
          {#each chart.panels as panel, index (index)}
            <section class="small-panel" aria-label={panel.label}>
              <h4>{panel.label}</h4>
              <button
                type="button"
                class="spine-panel-button"
                onclick={event => openSpinogram(event.currentTarget, chart.title, chart.xLabel, panel)}
                aria-label={panel.label}
                title={panel.label}
              >
                <div class="spine-plot" role="img" aria-label={panel.label}>
                  {#each panel.bins as bin, index (index)}
                    <span
                      class="spine-bin"
                      title={bin.label}
                      style={`left: ${pct(bin.xPercent)}; width: ${pct(bin.widthPercent)};`}
                    >
                      {#each bin.cells as cell, index (index)}
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
                {#each panel.ticks as tick, index (index)}
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
    {/snippet}
    <div class={{ 'wide-chart': chart.kind === 'scatter' || chart.kind === 'spine' }}>
      {#if collapsible}
        <ResultSection title={chart.title} open={detailOpen(chart)}>
          {@render chartContent()}
        </ResultSection>
      {:else}
        <section aria-label={chart.title}>
          {@render chartContent()}
        </section>
      {/if}
    </div>
  {/each}

  {#if plurinominalOptions.length > 0 && plurinominalChart}
    <div class="wide-chart">
      <ResultSection title={plurinominalChart.title} open>
        <div class="pluri-control">
          <label>
            Collegio
            <select
              value={selectedPlurinominalOption?.id ?? ''}
              onchange={(event) => onSelectPlurinominalOption((event.currentTarget as HTMLSelectElement).value)}
            >
              {#each plurinominalOptions as option, index (index)}
                <option value={option.id}>{option.label}</option>
              {/each}
            </select>
          </label>
        </div>
        <div class="small-multiple-grid single">
          {#each plurinominalChart.panels as panel, index (index)}
            <section class="small-panel" aria-label={panel.label}>
              <h4>{panel.label}</h4>
              <button
                type="button"
                class="spine-panel-button"
                onclick={event => openSpinogram(event.currentTarget, plurinominalChart.title, plurinominalChart.xLabel, panel)}
                aria-label={panel.label}
                title={panel.label}
              >
                <div class="spine-plot large" role="img" aria-label={panel.label}>
                  {#each panel.bins as bin, index (index)}
                    <span
                      class="spine-bin"
                      title={bin.label}
                      style={`left: ${pct(bin.xPercent)}; width: ${pct(bin.widthPercent)};`}
                    >
                      {#each bin.cells as cell, index (index)}
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
                {#each panel.ticks as tick, index (index)}
                  <span class="spine-axis-tick" style={`left: ${pct(tick.positionPercent)};`}>
                    <span></span>
                    <em>{tick.label}</em>
                  </span>
                {/each}
              </div>
            </section>
          {/each}
        </div>
      </ResultSection>
    </div>
  {/if}
</div>

<dialog
  bind:this={spinogramDialog}
  closedby="any"
  aria-label={enlargedSpinogram?.panel.label}
  onclose={closeSpinogram}
  onpointerdown={event => {
    pointerStartedOnBackdrop = event.target === event.currentTarget;
  }}
  onpointerup={event => {
    // Older browsers support modal dialogs but ignore the closedby attribute.
    if (!('closedBy' in HTMLDialogElement.prototype) && pointerStartedOnBackdrop &&
        event.target === event.currentTarget) spinogramDialog.close();
  }}
>
  {#if enlargedSpinogram}
    <div class="dialog-content">
      <div class="overlay-heading">
        <div>
          <h3>{enlargedSpinogram.title}</h3>
          <p>{enlargedSpinogram.panel.label}</p>
        </div>
        <form method="dialog">
          <button class="close-button" aria-label="Chiudi" title="Chiudi">
            <X size={20} aria-hidden="true" />
          </button>
        </form>
      </div>
      <div class="spine-plot enlarged" role="img" aria-label={enlargedSpinogram.panel.label}>
        {#each enlargedSpinogram.panel.bins as bin, index (index)}
          <span
            class="spine-bin"
            title={bin.label}
            style={`left: ${pct(bin.xPercent)}; width: ${pct(bin.widthPercent)};`}
          >
            {#each bin.cells as cell, index (index)}
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
        {#each enlargedSpinogram.panel.ticks as tick, index (index)}
          <span class="spine-axis-tick" style={`left: ${pct(tick.positionPercent)};`}>
            <span></span>
            <em>{tick.label}</em>
          </span>
        {/each}
      </div>
      <p class="axis-label">{enlargedSpinogram.xLabel}</p>
    </div>
  {/if}
</dialog>

<style>
  .result-charts {
    display: grid;
    gap: 16px;
    padding: 16px 16px 0;
  }

  .result-charts > div { min-inline-size: 0; }

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
    color: var(--color-text-muted);
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
    color: var(--color-text);
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
    background: var(--color-track);
  }

  .bar-fill {
    display: block;
    height: 100%;
    min-width: 2px;
  }

  .bar-value {
    color: var(--color-text-muted);
    font-size: 12px;
    font-variant-numeric: tabular-nums;
    font-weight: 700;
    text-align: right;
  }

  .boxplot-axis {
    position: relative;
    height: 24px;
    margin-left: min(180px, 34%);
    border-bottom: 1px solid var(--color-border);
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
    background: var(--color-chart-axis);
  }

  .axis-tick em {
    color: var(--color-text-muted);
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
    background: var(--color-chart-axis);
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
    background: var(--color-surface);
  }

  .axis-label {
    margin: 2px 0 0;
    color: var(--color-text-muted);
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
    border: 1px solid var(--color-border);
    border-radius: 6px;
    padding: 10px;
  }

  svg {
    width: 100%;
    min-height: 160px;
  }

  text {
    fill: var(--color-text-muted);
    font-size: 4px;
    font-variant-numeric: tabular-nums;
  }

  .axis {
    stroke: var(--color-chart-axis);
    stroke-width: 0.35;
  }

  .spine-plot {
    position: relative;
    overflow: hidden;
    height: 150px;
    border: 1px solid var(--color-border);
    background: var(--color-surface-muted);
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
    border-top: 1px solid var(--color-border);
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
    background: var(--color-chart-axis);
  }

  .spine-axis-tick em {
    color: var(--color-text-muted);
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
    color: var(--color-text-muted);
    font-size: 13px;
    font-weight: 650;
  }

  select {
    min-height: 38px;
    color: var(--color-text);
  }

  dialog {
    width: min(1180px, calc(100% - 56px));
    max-width: none;
    max-height: calc(100dvh - 56px);
    overscroll-behavior: contain;
    border: 1px solid var(--color-border);
    border-radius: var(--radius-panel);
    background: var(--color-surface);
    color: var(--color-text);
    padding: 0;
    box-shadow: var(--shadow-overlay);
  }

  dialog::backdrop { background: var(--color-backdrop); }

  .dialog-content {
    display: grid;
    gap: var(--space-3);
    padding: var(--space-4);
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
    color: var(--color-text);
    font-size: 17px;
    font-weight: 750;
  }

  .overlay-heading p {
    margin-top: 3px;
    color: var(--color-text-muted);
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

    dialog {
      width: calc(100% - 24px);
      max-height: calc(100dvh - 24px);
    }
  }
</style>
