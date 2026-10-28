import type { ChamberInput, PoliticalList } from './input';
import { OTHER_LIST_ID, OTHER_LIST_NAME } from '../../core/other-list';

export function otherListErrors(lists: readonly PoliticalList[]): string[] {
  const errors: string[] = [];
  if (lists.filter(l => l.kind === 'other').length > 1) errors.push('Only one Other list is allowed per chamber');
  for (const l of lists) {
    if (l.kind === 'other' && (l.id !== OTHER_LIST_ID || l.name !== OTHER_LIST_NAME || l.coalitionId !== null || l.protectedMinority !== null))
      errors.push('Other requires its fixed identity/name, no coalition and no minority exception');
    if (l.kind !== 'other' && l.id === OTHER_LIST_ID) errors.push('Reserved Other list identity');
  }
  return errors;
}

/** Candidate-free voting presence is distinct from a filed slate. */
export function otherListsInCollege(c: ChamberInput, collegeId: string): string[] {
  return (c.otherListCoverage ?? []).filter(r => r.collegeId === collegeId).map(r => r.listId);
}
