<script lang="ts">
  import { Plus, Trash2 } from '@lucide/svelte';
  import * as tokens from '$lib/design/tokens';
  import type { ScenarioCoalition, ScenarioList } from '$lib/core/types';

  interface Props {
    coalitions: ScenarioCoalition[];
    lists: ScenarioList[];
    onAddCoalition: () => void;
    onUpdateCoalitionName: (id: string, name: string) => void;
    onUpdateCoalitionColor?: (id: string, color: string) => void;
    onRemoveCoalition: (id: string) => void;
    onAddList: () => void;
    onUpdateListName: (id: string, name: string) => void;
    onUpdateListCoalition: (id: string, coalition: string) => void;
    onUpdateListColor: (id: string, color: string) => void;
    onUpdateListShare: (id: string, share: number, override: boolean) => void;
    onRemoveList: (id: string) => void;
    minCoalitions?: number;
    minLists?: number;
  }

  let {
    coalitions = [],
    lists = [],
    onAddCoalition,
    onUpdateCoalitionName,
    onUpdateCoalitionColor,
    onRemoveCoalition,
    onAddList,
    onUpdateListName,
    onUpdateListCoalition,
    onUpdateListColor,
    onUpdateListShare,
    onRemoveList,
    minCoalitions = 1,
    minLists = 1
  }: Props = $props();
</script>

<div class="list-editor">
  <div class="section">
    <div class="section-heading">
      <h3>Coalizioni</h3>
      <button type="button" onclick={onAddCoalition}>
        <Plus size={16} aria-hidden="true" />
        Aggiungi
      </button>
    </div>

    <div class="rows">
      {#each coalitions as coalition (coalition.id)}
        <div class="coalition-row">
          <input
            aria-label="Nome coalizione"
            type="text"
            value={coalition.name}
            oninput={(event) => onUpdateCoalitionName(coalition.id, (event.currentTarget as HTMLInputElement).value)}
          />
          <input
            aria-label="Colore coalizione"
            type="color"
            value={coalition.color}
            oninput={(event) => onUpdateCoalitionColor?.(coalition.id, (event.currentTarget as HTMLInputElement).value)}
          />
          <button
            type="button"
            aria-label={`Rimuovi coalizione ${coalition.name}`}
            disabled={coalitions.length <= minCoalitions}
            onclick={() => onRemoveCoalition(coalition.id)}
          >
            <Trash2 size={16} aria-hidden="true" />
          </button>
        </div>
      {/each}
    </div>
  </div>

  <div class="section">
    <div class="section-heading">
      <h3>Liste</h3>
      <button type="button" onclick={onAddList}>
        <Plus size={16} aria-hidden="true" />
        Aggiungi
      </button>
    </div>

    <div class="rows">
      {#each lists as list (list.id)}
        <div class="list-row">
          <input
            aria-label="Nome lista"
            type="text"
            value={list.name}
            oninput={(event) => onUpdateListName(list.id, (event.currentTarget as HTMLInputElement).value)}
          />
          <select
            value={list.coalition}
            onchange={(event) => onUpdateListCoalition(list.id, (event.currentTarget as HTMLSelectElement).value)}
            aria-label={`Coalizione ${list.name}`}
          >
            {#each coalitions as coalition}
              <option value={coalition.name}>{coalition.name}</option>
            {/each}
          </select>
          <input
            aria-label={`Colore ${list.name}`}
            type="color"
            value={list.color}
            oninput={(event) => onUpdateListColor(list.id, (event.currentTarget as HTMLInputElement).value)}
          />
          <input
            aria-label="Quota iniziale"
            type="number"
            min="0"
            max="100"
            step="0.1"
            value={list.startingShare}
            oninput={(event) => onUpdateListShare(list.id, Number(event.currentTarget.value), true)}
          />
          <label class="checkbox small">
            <input
              type="checkbox"
              checked={list.shareOverride}
              onchange={(event) => onUpdateListShare(list.id, list.startingShare, (event.currentTarget as HTMLInputElement).checked)}
            />
            Usa
          </label>
          <button
            type="button"
            aria-label={`Rimuovi lista ${list.name}`}
            disabled={lists.length <= minLists}
            onclick={() => onRemoveList(list.id)}
          >
            <Trash2 size={16} aria-hidden="true" />
          </button>
        </div>
      {/each}
    </div>
  </div>
</div>

<style>
  .list-editor {
    display: grid;
    gap: {tokens.spacing.xl};
  }

  .section {
    display: grid;
    gap: {tokens.spacing.md};
  }

  .section-heading {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: {tokens.spacing.md};
  }

  h3 {
    margin: 0;
    font-size: {tokens.typography.h3.fontSize};
    font-weight: {tokens.typography.h3.fontWeight};
  }

  .rows {
    display: grid;
    gap: {tokens.spacing.md};
  }

  .coalition-row,
  .list-row {
    display: flex;
    gap: {tokens.spacing.md};
    align-items: center;
    flex-wrap: wrap;
  }

  .list-row {
    display: grid;
    grid-template-columns: minmax(13rem, 1.2fr) minmax(9rem, 0.8fr) auto auto auto auto;
    align-items: center;
  }

  .coalition-row {
    display: grid;
    grid-template-columns: minmax(13rem, 1fr) auto auto;
    align-items: center;
    justify-content: start;
  }

  input,
  select {
    font: inherit;
    min-height: {tokens.sizing.inputHeight};
    border: 1px solid {tokens.colors.border};
    border-radius: {tokens.borderRadius.medium};
    padding: {tokens.spacing.sm};
    background: {tokens.colors.background};
    color: {tokens.colors.textPrimary};
  }

  input[type='color'] {
    width: {tokens.sizing.colorPickerWidth};
    padding: {tokens.spacing.xs};
  }

  input[type='number'] {
    width: {tokens.sizing.inputWidthNumber};
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

  .checkbox {
    display: flex;
    align-items: center;
    gap: {tokens.spacing.sm};
    font-size: {tokens.typography.label.fontSize};
  }

  .checkbox.small {
    font-size: {tokens.typography.labelSmall.fontSize};
  }

  @media (max-width: {tokens.breakpoints.mobile}) {
    .list-row,
    .coalition-row {
      grid-template-columns: 1fr;
    }
  }
</style>
