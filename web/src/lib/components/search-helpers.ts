/**
 * Shared search and text normalization helpers for UI components.
 */

/**
 * Normalize text for case-insensitive, accent-insensitive search.
 * Converts to lowercase, normalizes Unicode (NFKD), removes accents/diacritics.
 */
export function normalizeSearchText(value: string): string {
  return value
    .trim()
    .toLocaleLowerCase('it-IT')
    .normalize('NFKD')
    .replace(/[\u0300-\u036f]/g, '');
}

/**
 * Search an array of items by multiple tokens (all must match).
 * Each item must have a searchText property with normalized text.
 */
export interface Searchable {
  searchText: string;
}

export function searchByTokens<T extends Searchable>(
  query: string,
  items: T[],
  maxResults = 10
): T[] {
  const normalized = normalizeSearchText(query);
  if (normalized.length < 2) return [];

  const tokens = normalized.split(/\s+/).filter(Boolean);
  return items
    .filter((item) => tokens.every((token) => item.searchText.includes(token)))
    .slice(0, maxResults);
}

/**
 * Extract a primary text and sublabel from a searchable item for display.
 * Useful for location pickers and similar dropdowns.
 */
export interface DisplayItem {
  label: string;
  sublabel?: string;
}

/**
 * Check if a text string matches a normalized query exactly (ignoring accents/case).
 */
export function matchesExactly(text: string, query: string): boolean {
  return normalizeSearchText(text) === normalizeSearchText(query);
}
