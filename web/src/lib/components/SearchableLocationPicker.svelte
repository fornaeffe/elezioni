<script lang="ts">
  import { ChevronDown } from '@lucide/svelte';
  import * as tokens from '$lib/design/tokens';
  import { searchByTokens, type Searchable } from './search-helpers';

  interface LocationOption extends Searchable {
    scope: string;
    code: string;
    label: string;
    sublabel: string;
  }

  interface Props {
    options: LocationOption[];
    selectedScope: string;
    searchQuery: string;
    selectedCode: string;
    scopeOptions: string[];
    scopeLabel?: (scope: string) => string;
    onScopeChange: (scope: string) => void;
    onSearchChange: (query: string) => void;
    onSelectOption: (option: LocationOption) => void;
  }

  let { 
    options = [], 
    selectedScope = 'municipality',
    searchQuery = '',
    selectedCode = '',
    scopeOptions = [],
    scopeLabel = (s) => s,
    onScopeChange,
    onSearchChange,
    onSelectOption
  }: Props = $props();

  const searchResults = $derived(
    searchByTokens(searchQuery, filteredOptions, 10)
  );

  const filteredOptions = $derived(
    options.filter((opt) => opt.scope === selectedScope)
  );

  function handleSelectOption(option: LocationOption): void {
    onSelectOption(option);
    searchQuery = '';
  }
</script>

<div class="location-picker">
  <label>
    Ambito
    <select {selectedScope} onchange={(e) => onScopeChange(e.currentTarget.value)}>
      {#each scopeOptions as scope}
        <option value={scope}>{scopeLabel(scope)}</option>
      {/each}
    </select>
  </label>

  <label>
    Cerca localita
    <input
      type="text"
      value={searchQuery}
      placeholder="Es. Bologna, 37006..."
      oninput={(e) => onSearchChange(e.currentTarget.value)}
    />
  </label>

  {#if searchResults.length > 0}
    <div class="search-results" role="listbox">
      {#each searchResults as option (option.code)}
        <button
          type="button"
          class="result-item"
          class:active={selectedCode === option.code}
          onclick={() => handleSelectOption(option)}
        >
          <span>{option.label}</span>
          <span>{option.sublabel}</span>
        </button>
      {/each}
    </div>
  {/if}
</div>

<style>
  .location-picker {
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

  .search-results {
    display: grid;
    min-width: min(30rem, 100%);
    border: 1px solid {tokens.colors.borderLight};
    border-radius: {tokens.borderRadius.medium};
    overflow: hidden;
    z-index: {tokens.zIndex.dropdown};
  }

  .result-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    border: 0;
    border-bottom: 1px solid {tokens.colors.borderLighter};
    border-radius: 0;
    padding: {tokens.spacing.md};
    background: {tokens.colors.background};
    color: {tokens.colors.textPrimary};
    cursor: pointer;
    text-align: left;
    font-size: {tokens.typography.bodySmall.fontSize};
    transition: {tokens.transitions.normal};
  }

  .result-item:last-child {
    border-bottom: 0;
  }

  .result-item:hover,
  .result-item.active {
    background: {tokens.colors.surface};
  }

  .result-item span:last-child {
    color: {tokens.colors.textMuted};
    font-size: {tokens.typography.labelSmall.fontSize};
  }
</style>
