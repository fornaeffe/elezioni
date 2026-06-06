<script lang="ts">
  import { Play } from '@lucide/svelte';
  import * as tokens from '$lib/design/tokens';

  interface Props {
    running: boolean;
    simulations: number;
    seed: string;
    phase: string;
    elapsedMs: number;
    canRun: boolean;
    onSimulationsChange: (value: number) => void;
    onSeedChange: (value: string) => void;
    onRun: () => void;
  }

  let {
    running = false,
    simulations = 10,
    seed = '',
    phase = 'idle',
    elapsedMs = 0,
    canRun = true,
    onSimulationsChange,
    onSeedChange,
    onRun
  }: Props = $props();

  const runButtonLabel = $derived(running ? phase : 'Esegui');
  const elapsedLabel = $derived(`${elapsedMs.toFixed(0)} ms`);
</script>

<div class="control-bar">
  <label>
    Simulazioni
    <input
      type="number"
      min="1"
      max="1000"
      value={simulations}
      oninput={(e) => onSimulationsChange(Number(e.currentTarget.value))}
      disabled={running}
    />
  </label>

  <label>
    Seed
    <input
      type="text"
      value={seed}
      oninput={(e) => onSeedChange(e.currentTarget.value)}
      disabled={running}
    />
  </label>

  <button class="primary" type="button" disabled={!canRun} onclick={onRun}>
    <Play size={16} aria-hidden="true" />
    {runButtonLabel}
  </button>

  <span class="elapsed" data-testid="elapsed-ms">{elapsedLabel}</span>
</div>

<style>
  .control-bar {
    display: flex;
    gap: {tokens.spacing.lg};
    align-items: flex-end;
    flex-wrap: wrap;
  }

  label {
    display: grid;
    gap: {tokens.spacing.xs};
    font-size: {tokens.typography.label.fontSize};
    color: {tokens.colors.textSecondary};
  }

  input {
    font: inherit;
    min-height: {tokens.sizing.inputHeight};
    border: 1px solid {tokens.colors.border};
    border-radius: {tokens.borderRadius.medium};
    padding: {tokens.spacing.sm};
    background: {tokens.colors.background};
    color: {tokens.colors.textPrimary};
  }

  input[type='number'] {
    width: {tokens.sizing.inputWidthNumber};
  }

  input:disabled {
    opacity: 0.65;
  }

  button {
    font: inherit;
    min-height: {tokens.sizing.buttonHeight};
    border: 1px solid {tokens.colors.border};
    border-radius: {tokens.borderRadius.medium};
    padding: {tokens.spacing.sm} {tokens.spacing.md};
    display: inline-flex;
    align-items: center;
    gap: {tokens.spacing.sm};
    color: {tokens.colors.textPrimary};
    background: {tokens.colors.background};
    cursor: pointer;
    transition: {tokens.transitions.normal};
  }

  button:hover:not(:disabled) {
    background: {tokens.colors.surface};
  }

  button:disabled {
    opacity: 0.55;
    cursor: not-allowed;
  }

  button.primary {
    background: {tokens.colors.primary};
    border-color: {tokens.colors.primary};
    color: {tokens.colors.textInverse};
  }

  button.primary:hover:not(:disabled) {
    background: {tokens.colors.primaryHover};
    border-color: {tokens.colors.primaryHover};
  }

  .elapsed {
    color: {tokens.colors.textMuted};
    font-variant-numeric: tabular-nums;
    font-size: {tokens.typography.bodySmall.fontSize};
  }
</style>
