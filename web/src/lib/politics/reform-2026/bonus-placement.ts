import type { Candidate } from './input';
import type { ReformCandidateTemplate } from './generation-types';
import { bonusKey } from './bonus-reference-support';

export interface OrdinaryPlan {
  collegeId: string;
  territoryId: string;
  listId: string;
  startsFemale?: boolean;
  people: (Candidate | undefined)[];
}
export interface BonusPerson { template: ReformCandidateTemplate; listId: string }
export interface BonusPlacement {
  /** One witness prefix per ordinary slate; extra ordinary uses need no prefix. */
  prefixes: number[];
  startsFemale: boolean[];
  assignments: Map<string, Candidate>;
  traversalCost: number;
}

/** Enumerate legal witness prefixes, preferring the supplied college traversal.
 * Keep one construction for each reachable female count. Supplied ordinary
 * people without an explicit bonus declaration block the prefix, never get
 * promoted automatically. Names may repeat in ordinary slates, not bonus slots.
 * C19(4)/S9(5): one qualifying local prefix suffices; C19(2): five ordinary uses. */
export function bonusPlacementOptions(plans: readonly OrdinaryPlan[],
  required: readonly BonusPerson[], declarations: ReadonlyMap<string, BonusPerson>, ordinaryUses: ReadonlyMap<string, number>,
  maximumUses: number, count: number): Map<number, BonusPlacement> {
  const result = new Map<number, BonusPlacement>();
  const requiredIds = new Set(required.map(p => p.template.candidate.id));
  if (requiredIds.size > count) return result;
  const limits = plans.map(p => {
    const blocked = p.people.findIndex(person => person && !declarations.has(person.id));
    return blocked < 0 ? 7 : blocked;
  });
  const vectors: number[][] = [];
  function enumerate(prefixes: number[], selected: Set<string>, blanks: number) {
    if (selected.size + blanks > count) return;
    const index = prefixes.length;
    if (index === plans.length) {
      if (selected.size + blanks === count) vectors.push(prefixes);
      return;
    }
    const ids = new Set(selected);
    for (let length = 0, empty = blanks; length <= limits[index]; length++) {
      enumerate([...prefixes, length], new Set(ids), empty);
      const person = plans[index].people[length];
      if (person && requiredIds.has(person.id)) ids.add(person.id);
      if (!person) empty++;
    }
  }
  enumerate([], new Set(), 0);
  // Lexicographically prefer taking the earliest traversal slot. With no
  // constraints this is precisely heads first, then second positions, etc.
  vectors.sort((a, b) => {
    for (let level = 0; level < 7; level++) for (let i = 0; i < plans.length; i++) {
      const difference = Number(b[i] > level) - Number(a[i] > level);
      if (difference) return difference;
    }
    return 0;
  });
  for (const [traversalCost, prefixes] of vectors.entries()) {
    const selected = new Set(plans.flatMap((p, i) => p.people.slice(0, prefixes[i])
      .flatMap(person => person && requiredIds.has(person.id) ? [person.id] : [])));
    const pending = required.filter(p => !selected.has(p.template.candidate.id));
    const blanks: { plan: number; position: number }[] = [];
    for (let level = 0; level < 7; level++) for (const [i, p] of plans.entries()) {
      if (prefixes[i] > level && !p.people[level]) blanks.push({ plan: i, position: level });
    }
    if (pending.length > blanks.length) continue;
    const starts = plans.map(p => p.startsFemale), assignments = new Map<string, Candidate>();
    function placeNamed(index: number, occupied: Set<number>): void {
      if (index < pending.length) {
        const person = pending[index].template.candidate;
        if ((ordinaryUses.get(person.id) ?? 0) >= maximumUses) return;
        for (const [i, slot] of blanks.entries()) {
          const p = plans[slot.plan], start = (person.gender === 'female') !== (slot.position % 2 === 1);
          if (occupied.has(i) || p.people.some(c => c?.id === person.id) ||
            starts[slot.plan] !== undefined && starts[slot.plan] !== start) continue;
          const previous = starts[slot.plan]; starts[slot.plan] = start;
          assignments.set(bonusKey(p.collegeId, p.listId, slot.position), person);
          placeNamed(index + 1, new Set([...occupied, i]));
          assignments.delete(bonusKey(p.collegeId, p.listId, slot.position)); starts[slot.plan] = previous;
        }
        return;
      }
      const free = plans.flatMap((p, i) => starts[i] === undefined && prefixes[i] > 0 ? [i] : []);
      function orient(index: number): void {
        if (index < free.length) {
          // Stable neutral preference; both orientations are available to the
          // national gender planner, with no need to reroll a simulation.
          starts[free[index]] = true; orient(index + 1);
          starts[free[index]] = false; orient(index + 1);
          starts[free[index]] = undefined; return;
        }
        let female = required.filter(p => p.template.candidate.gender === 'female').length;
        for (const slot of blanks) {
          const p = plans[slot.plan];
          if (!assignments.has(bonusKey(p.collegeId, p.listId, slot.position)) &&
            starts[slot.plan] !== (slot.position % 2 === 1)) female++;
        }
        if (!result.has(female)) result.set(female, { prefixes: [...prefixes], traversalCost,
          startsFemale: starts.map((v, i) => v ?? plans[i].startsFemale ?? true), assignments: new Map(assignments) });
      }
      orient(0);
    }
    placeNamed(0, new Set());
    if (result.size === count + 1) break;
  }
  return result;
}
