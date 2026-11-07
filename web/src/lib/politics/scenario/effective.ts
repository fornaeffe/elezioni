import { chambers, REFORM, type Chamber, type ExplicitBonusLink, type LawId, type Placement,
  type PoliticsScenario, type Subject, type Location } from './schema';

export interface GeographyRelation { readonly parent: Location; readonly child: Location }
const sameLocation = (a: Location, b: Location) => a.law === b.law && a.geographyVersion === b.geographyVersion &&
  a.chamber === b.chamber && a.level === b.level && a.code === b.code;
function related(a: Location, b: Location, relations: readonly GeographyRelation[]): boolean {
  const reaches = (parent: Location, child: Location, seen = new Set<string>()): boolean => {
    if (sameLocation(parent, child)) return true;
    const key = JSON.stringify([parent.law, parent.geographyVersion, parent.chamber, parent.level, parent.code]);
    if (seen.has(key)) return false;
    seen.add(key);
    return relations.some(r => sameLocation(r.parent, parent) && reaches(r.child, child, seen));
  };
  return reaches(a, b) || reaches(b, a);
}

export function renewed(s: PoliticsScenario, chamber: Chamber): boolean { return s.renewal === 'joint' || s.renewal === chamber; }
export function sameSubject(a: Subject, b: Subject): boolean { return a.kind === b.kind && a.id === b.id; }
export function subjectExists(s: PoliticsScenario, subject: Subject): boolean {
  return (subject.kind === 'coalition' ? s.subjects.coalitions : s.subjects.lists).some(x => x.id === subject.id);
}
/** Declaration only: renewal and member-list participation never rewrite coalition scope. */
export function declaredChambers(s: PoliticsScenario, subject: Subject, law: LawId): readonly Chamber[] {
  if (subject.kind === 'coalition') return s.subjects.coalitions.find(c => c.id === subject.id)?.chambers ?? [];
  const list = s.subjects.lists.find(l => l.id === subject.id);
  if (!list || list.coalitionId !== null || list.kind === 'other') return [];
  const filings = s.lawSettings[law].filings.find(f => f.listId === list.id)?.locations ?? [];
  return chambers.filter(ch => filings.some(l => l.chamber === ch));
}
export function effectiveFilings(s: PoliticsScenario, law: LawId, listId: string, includeInactiveChambers = false) {
  const list = s.subjects.lists.find(l => l.id === listId);
  if (!list) return [];
  const scope = list.coalitionId === null ? chambers :
    s.subjects.coalitions.find(c => c.id === list.coalitionId)?.chambers ?? [];
  return (s.lawSettings[law].filings.find(f => f.listId === listId)?.locations ?? [])
    .filter(l => scope.includes(l.chamber) && (includeInactiveChambers || renewed(s, l.chamber)));
}
export function effectiveMembers(s: PoliticsScenario, listId: string, ch: Chamber): readonly string[] {
  const membership = s.memberships.find(m => m.listId === listId);
  if (!membership) return [];
  const historical = membership[ch].filter(id => id !== membership.newComponentId);
  return historical.length ? historical : [membership.newComponentId];
}
export function effectivePlacements(s: PoliticsScenario, law: LawId, relations: readonly GeographyRelation[] = []): readonly Placement[] {
  return s.lawSettings[law].placements.filter(p => {
    if (!renewed(s, p.location.chamber)) return false;
    if (p.owner.kind === 'list') return effectiveFilings(s, law, p.owner.id).some(l => related(l, p.location, relations));
    return declaredChambers(s, p.owner, law).includes(p.location.chamber) && s.subjects.lists.some(l =>
      l.coalitionId === p.owner.id && effectiveFilings(s, law, l.id).some(f => related(f, p.location, relations)));
  });
}
export interface ResolvedBonusLink extends ExplicitBonusLink {
  readonly status: 'automatic' | 'explicit-active' | 'explicit-inactive';
  readonly reasons: readonly string[];
}
function ineligible(s: PoliticsScenario, subject: Subject, ch: Chamber): string | null {
  if (!subjectExists(s, subject)) return 'unknown-subject';
  const scope = declaredChambers(s, subject, REFORM);
  if (scope.length === 2) return 'automatic-counterpart';
  if (scope.length !== 1 || scope[0] !== ch) return 'endpoint-chamber-or-affiliation';
  return null;
}
/** The sole D11 resolver: consumers use status/reasons, never persist them. */
export function resolveBonusLinks(s: PoliticsScenario): readonly ResolvedBonusLink[] {
  const result: ResolvedBonusLink[] = [];
  for (const [kind, rows] of [['coalition', s.subjects.coalitions], ['list', s.subjects.lists]] as const) {
    for (const row of rows) {
      const subject = { kind, id: row.id };
      if (declaredChambers(s, subject, REFORM).length === 2)
        result.push({ camera: subject, senato: subject, status: 'automatic', reasons: [] });
    }
  }
  for (const pair of s.lawSettings[REFORM].bonusLinks) {
    const reasons = chambers.flatMap(ch => {
      const reason = ineligible(s, pair[ch], ch);
      return reason ? [`${ch}:${reason}`] : [];
    });
    result.push({ ...pair, status: reasons.length ? 'explicit-inactive' : 'explicit-active', reasons });
  }
  return result;
}
export function eligibleBonusEndpoint(s: PoliticsScenario, subject: Subject, ch: Chamber): boolean {
  return ineligible(s, subject, ch) === null;
}
/** Single renewal saves declarations but never emits a joint scrutiny relationship. */
export function materializeBonusLinks(s: PoliticsScenario): readonly ExplicitBonusLink[] {
  if (s.selection.scrutiny.id !== REFORM || s.renewal !== 'joint') return [];
  return resolveBonusLinks(s).filter(l => l.status !== 'explicit-inactive').map(({ camera, senato }) => ({ camera, senato }));
}
