import type { Scenario, ScenarioList } from './types';

export const OTHER_LIST_ID = '__other__';
export const OTHER_LIST_NAME = 'Altre liste';
export const OTHER_LIST_COLOR = '#9ca3af';
export type ListKind = 'list' | 'other';
export const isOtherList = (list: { kind?: ListKind }) => list.kind === 'other';

/** A vote category, never a candidate-bearing electoral subject. */
export function createOtherList(): ScenarioList {
  return { id: OTHER_LIST_ID, kind: 'other', name: OTHER_LIST_NAME, color: OTHER_LIST_COLOR,
    coalition: null, startingShare: 0, shareOverride: false };
}

export function validateOtherScenarioLists(s: Scenario): string[] {
  const errors: string[] = [];
  if (s.lists.filter(isOtherList).length > 1) errors.push('È consentita una sola voce Altre liste.');
  for (const list of s.lists) {
    if (list.kind !== undefined && list.kind !== 'list' && list.kind !== 'other') errors.push('Tipo lista non valido.');
    if (!isOtherList(list)) {
      if (list.id === OTHER_LIST_ID) errors.push('Identità Altre liste riservata.');
      continue;
    }
    if (list.id !== OTHER_LIST_ID || list.name !== OTHER_LIST_NAME || list.color !== OTHER_LIST_COLOR || list.coalition !== null)
      errors.push('Altre liste richiede identità, nome e colore fissi e nessuna coalizione.');
    if (s.candidateTemplates.some(t => t.list === list.name)) errors.push('Altre liste non può avere candidati.');
  }
  return errors;
}

/** Disable removes only this category and its owned references. */
export function setOtherListEnabled(s: Scenario, enabled: boolean): Scenario {
  if (enabled) return s.lists.some(isOtherList) ? s : { ...s, lists: [...s.lists, createOtherList()] };
  const names = new Set(s.lists.filter(isOtherList).map(l => l.name));
  return { ...s, lists: s.lists.filter(l => !isOtherList(l)),
    listCorrespondences: s.listCorrespondences.filter(c => !names.has(c.futureList)),
    localShareOverrides: s.localShareOverrides.filter(o => !names.has(o.list)),
    candidateTemplates: s.candidateTemplates.filter(t => !t.list || !names.has(t.list)) };
}
