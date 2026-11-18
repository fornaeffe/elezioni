import type { ChamberInput, Subject } from './input';

/** I12-v1 (provisional): national admission does not create a filed list in
 * another territory. C31(4-bis)/S11(3-bis) omit absent candidatures from ballots;
 * C84(1)/S17-bis(1) proclaim filed candidates. Neither expressly resolves the
 * absent-list cap-remainder case. Coverage is independent of votes: a filed
 * zero-vote list remains eligible. Special-only links do not establish ordinary
 * coverage. See the specification's I12 source review. */
export function territorialEligibility(chamber: ChamberInput) {
  const territoryOf = new Map(chamber.colleges.map(c => [c.id, c.territoryId]));
  const filed = new Map<string, Set<string>>();
  for (const slate of chamber.ordinarySlates) {
    const territory = territoryOf.get(slate.collegeId);
    if (!territory) throw new RangeError('Unknown filed-slate college');
    const lists = filed.get(territory) ?? new Set<string>();
    lists.add(slate.listId); filed.set(territory, lists);
  }
  const list = (territoryId: string, listId: string) => filed.get(territoryId)?.has(listId) ?? false;
  return {
    list,
    subject(territoryId: string, subject: Subject): boolean {
      return subject.kind === 'list' ? list(territoryId, subject.id) :
        (chamber.coalitions.find(c => c.id === subject.id)?.listIds.some(id => list(territoryId, id)) ?? false);
    }
  };
}
