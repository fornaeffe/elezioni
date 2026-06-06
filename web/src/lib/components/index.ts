/**
 * Shared UI components for election simulations.
 * These components are used across Politics, Regional-ER, and other election types.
 */

export { default as SimulationControlBar } from './SimulationControlBar.svelte';
export { default as ScenarioListEditor } from './ScenarioListEditor.svelte';
export { default as SearchableLocationPicker } from './SearchableLocationPicker.svelte';
export { default as LocalShareOverrideEditor } from './LocalShareOverrideEditor.svelte';
export { default as ResultsPanel } from './ResultsPanel.svelte';

export { normalizeSearchText, searchByTokens, matchesExactly, type Searchable, type DisplayItem } from './search-helpers';
