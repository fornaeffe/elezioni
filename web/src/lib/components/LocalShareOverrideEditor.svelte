<script lang="ts">
  import { Plus, Trash2 } from '@lucide/svelte';
  import * as tokens from '$lib/design/tokens';
  import SearchableLocationPicker from './SearchableLocationPicker.svelte';
  import type { ScenarioList, ScenarioLocalShareOverride, ScenarioLocalShareOverrideScope } from '$lib/core/types';

  interface LocationOption {
    scope: ScenarioLocalShareOverrideScope;
    code: string;
    label: string;
    sublabel: string;
    searchText: string;
  }

  interface OverrideGroup {
    key: string;
    scope: ScenarioLocalShareOverrideScope;
    locationCode: string;
    locationLabel: string;
    locationSubLabel: string;
    overrides: ScenarioLocalShareOverride[];
  }

  interface Props {
    overrides: ScenarioLocalShareOverride[];
    lists: ScenarioList[];
    locationOptions: LocationOption[];
    overrideGroups: OverrideGroup[];
    selectedScope: ScenarioLocalShareOverrideScope;
    searchQuery: string;
    selectedLocationCode: string;
    selectedList: string;
    overrideShare: number;
    scopeOptions: ScenarioLocalShareOverrideScope[];
    scopeLabel?: (scope: string) => string;
    onScopeChange: (scope: ScenarioLocalShareOverrideScope) => void;
    onSearchChange: (query: string) => void;
    onSelectLocation: (option: LocationOption) => void;
    onListChange: (list: string) => void;
    onShareChange: (value: number) => void;
    onAddOverride: () => void;
    onUpdateOverride: (id: string, patch: Partial<ScenarioLocalShareOverride>) => void;
    onRemoveOverride: (id: string) => void;
    onRemoveLocation: (scope: ScenarioLocalShareOverrideScope, code: string) => void;
  }

  let {
    overrides = [],
    lists = [],
    locationOptions = [],
    overrideGroups = [],
    selectedScope = 'municipality',
    searchQuery = '',
    selectedLocationCode = '',
    selectedList = '',
    overrideShare = 0,
    scopeOptions = [],
    scopeLabel,
    onScopeChange,
    onSearchChange,
    onSelectLocation,
    onListChange,
    onShareChange,
    onAddOverride,
    onUpdateOverride,
    onRemoveOverride,
    onRemoveLocation
  }: Props = $props();
</script>

<div class="override-editor">
  <SearchableLocationPicker
    {locationOptions}
    selectedScope={selectedScope}
    searchQuery={searchQuery}
    selectedCode={selectedLocationCode}
    scopeOptions={scopeOptions}
    scopeLabel={scopeLabel}
    onScopeChange={onScopeChange}
    onSearchChange={onSearchChange}
    onSelectOption={onSelectLocation}
  />

  <label>
    Lista quota locale
    <select value={selectedList} onchange={(e) => onListChange(e.currentTarget.value)}>
      {#each lists as list}
        <option value={list.name}>{list.name}</option>
      {/each}
    </select>
  </label>

  <label>
    Quota locale da aggiungere
    <input
      type="number"
      min="0"
      max="100"
      step="0.1"
      value={overrideShare}
      oninput={(e) => onShareChange(Number(e.currentTarget.value))}
    />
  </label>

  <button
    type="button"
    disabled={!selectedLocationCode || lists.length === 0}
    onclick={onAddOverride}
  >
    <Plus size={16} aria-hidden="true" />
    Aggiungi quota locale
  </button>

  {#if overrideGroups.length > 0}
    <div class="override-groups">
      {#each overrideGroups as group (group.key)}
        <div class="override-group">
          <details open>
            <summary>
              <span>{group.locationLabel}</span>
              <small>{group.locationSubLabel}</small>
              <span class="count">{group.overrides.length}</span>
            </summary>

            <div class="override-rows">
              {#each group.overrides as override (override.id)}
                <div class="override-row">
                  <span>{override.list}</span>
                  <input
                    type="number"
                    min="0"
                    max="100"
                    step="0.1"
                    value={override.startingShare}
                    oninput={(e) => onUpdateOverride(override.id, { startingShare: Number(e.currentTarget.value) })}
                  />
                  <button
                    type="button"
                    aria-label={`Rimuovi quota per ${override.list} in ${group.locationLabel}`}
                    onclick={() => onRemoveOverride(override.id)}
                  >
                    <Trash2 size={16} aria-hidden="true" />
                  </button>
                </div>
              {/each}
            </div>

            <button
              type="button"
              class="location-action"
              onclick={() => onRemoveLocation(group.scope, group.locationCode)}
            >
              Rimuovi tutte le quote per {group.locationLabel}
            </button>
          </details>
        </div>
      {/each}
    </div>
  {/if}
</div>

<style>
  .override-editor {
    display: grid;
    gap: {tokens.spacing.md};
  }

  label {
    display: grid;
    gap: {tokens.spacing.xs};
    font-size: {tokens.typography.label.fontSize};
    color: {tokens.colors.textSecondary};
  }

  select,
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

  .override-groups {
    display: grid;
    gap: {tokens.spacing.md};
    margin-top: {tokens.spacing.md};
  }

  .override-group {
    border: 1px solid {tokens.colors.border};
    border-radius: {tokens.borderRadius.medium};
    padding: {tokens.spacing.md};
  }

  details summary {
    cursor: pointer;
    font-weight: 700;
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: {tokens.spacing.md};
    margin-bottom: {tokens.spacing.md};
  }

  summary small {
    font-size: {tokens.typography.labelSmall.fontSize};
    color: {tokens.colors.textMuted};
    font-weight: 400;
  }

  summary .count {
    font-size: {tokens.typography.labelSmall.fontSize};
    color: {tokens.colors.textMuted};
    margin-left: auto;
  }

  .override-rows {
    display: grid;
    gap: {tokens.spacing.sm};
    margin-bottom: {tokens.spacing.md};
  }

  .override-row {
    display: grid;
    grid-template-columns: minmax(12rem, 1fr) {tokens.sizing.inputWidthNumber} auto;
    align-items: center;
    gap: {tokens.spacing.md};
  }

  .location-action {
    font-size: {tokens.typography.labelSmall.fontSize};
    border: 0;
    padding: {tokens.spacing.xs} {tokens.spacing.md};
    background: transparent;
    color: {tokens.colors.primary};
    text-align: left;
  }

  .location-action:hover {
    text-decoration: underline;
  }

  @media (max-width: {tokens.breakpoints.mobile}) {
    .override-row {
      grid-template-columns: 1fr;
    }
  }
</style>
