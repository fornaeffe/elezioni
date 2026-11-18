<script lang="ts">
  import { onMount } from 'svelte';
  import type { ResultServiceClient } from '../core/result-service-client';
  import type { StatisticalSample } from './statistics';
  import { displayNumber } from './chart-builders';
  let { client, resultId, owner, series, label, color, variant = 'scatter', maxX = 100, maxY = 1, whiskerLow = 0, whiskerHigh = 100, measure = 'elected' }: {
    client: ResultServiceClient; resultId: string; owner: string; series: string; label: string; color: string;
    variant?: 'scatter' | 'outliers'; maxX?: number; maxY?: number; whiskerLow?: number; whiskerHigh?: number; measure?: 'elected' | 'entitled';
  } = $props();
  let canvas: HTMLCanvasElement;
  let count = $state(0), error = $state(''), finished = $state(false);
  onMount(() => {
    let alive = true, started = false;
    async function draw() {
      if (started) return; started = true;
      const context = canvas.getContext('2d'); if (!context) { error = 'Grafico canvas non disponibile'; return; }
      const outliers = variant === 'outliers', width = canvas.width, height = canvas.height;
      const left = outliers ? 0 : 64, top = outliers ? 0 : 16, plotWidth = width - left - (outliers ? 0 : 16), plotHeight = height - (outliers ? 0 : 66);
      context.clearRect(0, 0, width, height);
      if (!outliers) {
        context.strokeStyle = '#c8d0d7'; context.fillStyle = '#697681'; context.font = '12px system-ui';
        context.beginPath(); context.moveTo(left, top); context.lineTo(left, top + plotHeight); context.lineTo(left + plotWidth, top + plotHeight); context.stroke();
        for (let i = 0; i <= 4; i++) {
          context.textAlign = 'center'; context.fillText(`${displayNumber(maxX * i / 4)}%`, left + plotWidth * i / 4, top + plotHeight + 22);
          context.textAlign = 'right'; context.fillText(displayNumber(maxY * i / 4), left - 8, top + plotHeight * (1 - i / 4) + 4);
        }
        context.textAlign = 'center'; context.fillText('Percentuale sui voti validi', left + plotWidth / 2, height - 5);
        context.save(); context.translate(14, top + plotHeight / 2); context.rotate(-Math.PI / 2); context.fillText(measure === 'entitled' ? 'Seggi spettanti' : 'Eletti', 0, 0); context.restore();
      }
      let after = 0;
      try {
        for (;;) {
          const page = await client.call<StatisticalSample[]>('statistics-points', { resultId, owner, series, offset: after });
          if (!alive) return;
          if (!page.length) break;
          for (const p of page) {
            const y = measure === 'entitled' ? p.entitled : p.y;
            if (p.x === null || (outliers ? p.x >= whiskerLow && p.x <= whiskerHigh : y === null)) continue;
            const x = left + p.x / maxX * plotWidth;
            context.beginPath(); context.arc(x, outliers ? height / 2 : top + plotHeight * (1 - y! / maxY), outliers ? 2 : 3, 0, Math.PI * 2);
            context.strokeStyle = color; context.fillStyle = color;
            context.globalAlpha = outliers ? .6 : .14;
            if (outliers) context.stroke(); else context.fill(); count++;
          }
          after = page.at(-1)!.simulation;
        }
        if (alive) finished = true;
      } catch (e) { if (alive) error = String(e); }
    }
    const observer = new IntersectionObserver(entries => { if (entries.some(e => e.isIntersecting)) { observer.disconnect(); void draw(); } }, { rootMargin: '200px' });
    observer.observe(canvas);
    return () => { alive = false; observer.disconnect(); };
  });
</script>

<div class={['streamed-plot', { outliers: variant === 'outliers' }]}>
  <canvas bind:this={canvas} width={variant === 'outliers' ? 1000 : 800} height={variant === 'outliers' ? 22 : 360} aria-label={label} data-points={count} data-complete={finished}>{label}</canvas>
  {#if variant === 'scatter'}<p aria-live="polite">{finished ? `${count} osservazioni disegnate` : 'Caricamento del grafico…'}</p>{/if}
  {#if error}<p role="alert">{error}</p>{/if}
</div>

<style>
  .streamed-plot { width: 100%; min-width: 0; }
  canvas { display: block; width: 100%; height: auto; }
  p { margin: .25rem 0; font-size: .8rem; color: #697681; }
  .outliers { position: absolute; inset: 0; pointer-events: none; }
  .outliers canvas { height: 22px; }
</style>
