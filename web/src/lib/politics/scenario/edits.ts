import { isCalendarDate } from '../../core/json-shape';
import { chambers, laws, REFORM, type Candidate, type Chamber, type ExplicitBonusLink, type GeneratorId,
  type LawId, type Location, type Placement, type PoliticsScenario, type Subject } from './schema';
import { eligibleBonusEndpoint, sameSubject, isListFilingLocation } from './effective';
import { parsePoliticsScenario, type DomainContext } from './validation';
import { withReferenceDefault } from './references';

type Mutable<T> = T extends object ? { -readonly [K in keyof T]: Mutable<T[K]> } : T;
/** Every operation is atomic and detached; a failed conflict leaves the caller's draft untouched. */
function edit(s: PoliticsScenario, apply: (next: Mutable<PoliticsScenario>) => void, context?: DomainContext): PoliticsScenario {
  const next = structuredClone(s) as Mutable<PoliticsScenario>;
  apply(next);
  return parsePoliticsScenario(next, context);
}
export function selectLaw(s: PoliticsScenario, law: LawId): PoliticsScenario {
  return edit(s, n => { n.selection.scrutiny.id = law; });
}
export function selectGenerator(s: PoliticsScenario, generator: GeneratorId, version: '2.0.0'): PoliticsScenario {
  return edit(s, n => { n.selection.generatorsByLaw[n.selection.scrutiny.id] = { id: generator, version }; });
}
export function setRenewal(s: PoliticsScenario, renewal: PoliticsScenario['renewal']): PoliticsScenario {
  return edit(s, n => { n.renewal = renewal; });
}
export function updateList(s: PoliticsScenario, list: PoliticsScenario['subjects']['lists'][number]): PoliticsScenario {
  return edit(s, n => {
    const index = n.subjects.lists.findIndex(l => l.id === list.id);
    if (index < 0) throw new Error('UNKNOWN_LIST');
    n.subjects.lists[index] = structuredClone(list);
  });
}
export function updateCoalition(s: PoliticsScenario, coalition: PoliticsScenario['subjects']['coalitions'][number]): PoliticsScenario {
  return edit(s, n => {
    const index = n.subjects.coalitions.findIndex(c => c.id === coalition.id);
    if (index < 0) throw new Error('UNKNOWN_COALITION');
    n.subjects.coalitions[index] = structuredClone(coalition) as Mutable<typeof coalition>;
  });
}
export function upsertPlacement<L extends LawId>(s: PoliticsScenario, law: L,
  placement: PoliticsScenario['lawSettings'][L]['placements'][number], context?: DomainContext): PoliticsScenario {
  const block = s.lawSettings[law];
  return parsePoliticsScenario({ ...s, lawSettings: { ...s.lawSettings, [law]: { ...block,
    placements: [...(block.placements as readonly Placement[]).filter(p => p.id !== placement.id), placement] } } }, context);
}
export function setMembers(s: PoliticsScenario, listId: string, chamber: Chamber, members: readonly string[], context?: DomainContext): PoliticsScenario {
  return edit(s, n => {
    const m = n.memberships.find(m => m.listId === listId);
    if (!m) throw new Error('UNKNOWN_LIST');
    if (m.locked && chamber === 'senato') throw new Error('MEMBERSHIP_LOCKED: edit Camera or unlock first');
    m[chamber] = [...members];
    if (m.locked) m.senato = [...members];
  }, context);
}
export function setMembershipLock(s: PoliticsScenario, listId: string, locked: boolean, context?: DomainContext): PoliticsScenario {
  return edit(s, n => {
    const m = n.memberships.find(m => m.listId === listId);
    if (!m) throw new Error('UNKNOWN_LIST');
    if (locked) m.senato = [...m.camera];
    m.locked = locked;
  }, context);
}
export function setCoalitionScope(s: PoliticsScenario, id: string, scope: readonly Chamber[]): PoliticsScenario {
  return edit(s, n => {
    const c = n.subjects.coalitions.find(c => c.id === id);
    if (!c) throw new Error('UNKNOWN_COALITION');
    c.chambers = [...scope];
  });
}
export function setListCoalition(s: PoliticsScenario, id: string, coalitionId: string | null): PoliticsScenario {
  return edit(s, n => {
    const l = n.subjects.lists.find(l => l.id === id);
    if (!l) throw new Error('UNKNOWN_LIST');
    l.coalitionId = coalitionId;
  });
}
export function setFilings(s: PoliticsScenario, law: LawId, listId: string, locations: readonly Location[], context?: DomainContext): PoliticsScenario {
  return edit(s, n => {
    const f = n.lawSettings[law].filings.find(f => f.listId === listId);
    if (!f) throw new Error('UNKNOWN_LIST');
    f.locations = structuredClone(locations) as Mutable<Location[]>;
  }, context);
}
export function createCoalition(s: PoliticsScenario, coalition: Omit<PoliticsScenario['subjects']['coalitions'][number], 'chambers'>): PoliticsScenario {
  return edit(s, n => { n.subjects.coalitions.push({ ...coalition, chambers: [...chambers] }); });
}
export function createList(s: PoliticsScenario, list: PoliticsScenario['subjects']['lists'][number],
  component: PoliticsScenario['newComponents'][number], context: DomainContext): PoliticsScenario {
  return edit(s, n => {
    n.subjects.lists.push(structuredClone(list));
    n.newComponents.push(structuredClone(component));
    n.memberships.push({ listId: list.id, newComponentId: component.id, camera: [], senato: [], locked: true });
    for (const law of laws) n.lawSettings[law].filings.push({ listId: list.id, locations: context.locations.filter(l =>
      l.law === law && l.geographyVersion === n.data.lawAssets[law].geography.version &&
      isListFilingLocation(l, context.geographyRelations)).map(l => ({ ...l })) });
  }, context);
}
export function createCandidate(s: PoliticsScenario, input: { readonly id: string; readonly name: string;
  readonly gender: Candidate['gender']; readonly birthDate?: string | null; readonly availability: Candidate['availability'] }): PoliticsScenario {
  const supplied = input.birthDate != null && input.birthDate.trim() !== '';
  const birthDate = supplied ? input.birthDate! : `${Number(s.electionDate.slice(0, 4)) - 50}-01-01`;
  if (!isCalendarDate(birthDate)) throw new Error('INVALID_BIRTH_DATE');
  return edit(s, n => { n.candidates.push({ id: input.id, name: input.name, gender: input.gender, birthDate,
    birthDateOrigin: supplied ? 'declared' : 'assumed', availability: structuredClone(input.availability) }); });
}
export function updateCandidate(s: PoliticsScenario, candidate: Candidate): PoliticsScenario {
  return edit(s, n => {
    const index = n.candidates.findIndex(c => c.id === candidate.id);
    if (index < 0) throw new Error('UNKNOWN_CANDIDATE');
    n.candidates[index] = structuredClone(candidate);
  });
}
export function deleteCandidate(s: PoliticsScenario, candidateId: string): PoliticsScenario {
  return edit(s, n => {
    n.candidates = n.candidates.filter(c => c.id !== candidateId);
    for (const law of laws) {
      const block = n.lawSettings[law];
      for (let i = block.placements.length - 1; i >= 0; i--) if (block.placements[i].candidateId === candidateId) block.placements.splice(i, 1);
      block.externalCandidacies = block.externalCandidacies.filter(c => c.candidateId !== candidateId);
    }
    // Supplied evidence cannot keep dangling identities. Remove affected declarations, never invent zero counts.
    const reform = n.lawSettings[REFORM];
    reform.suppliedVotes = reform.suppliedVotes.filter(r => !('candidateTotals' in r.votes) ||
      ![...r.votes.candidateTotals, ...('candidateOnlyVotes' in r.votes ? r.votes.candidateOnlyVotes : [])].some(c => c.candidateId === candidateId));
    reform.suppliedPreferences = reform.suppliedPreferences.filter(r => r.preferences.kind !== 'counts' ||
      !r.preferences.counts.some(c => c.candidateId === candidateId) &&
      !(r.preferences.validity.kind === 'combinations' && r.preferences.validity.ballots.some(b => b.candidateIds.includes(candidateId))));
  });
}
export function removePlacement(s: PoliticsScenario, law: LawId, placementId: string): PoliticsScenario {
  return edit(s, n => {
    const pins = n.lawSettings[law].placements;
    for (let i = pins.length - 1; i >= 0; i--) if (pins[i].id === placementId) pins.splice(i, 1);
  });
}
export function replaceBonusLink(s: PoliticsScenario, pair: ExplicitBonusLink): { scenario: PoliticsScenario; replaced: readonly ExplicitBonusLink[] } {
  if (!eligibleBonusEndpoint(s, pair.camera, 'camera') || !eligibleBonusEndpoint(s, pair.senato, 'senato'))
    throw new Error('INELIGIBLE_LINK_ENDPOINT');
  const replaced = s.lawSettings[REFORM].bonusLinks.filter(p => sameSubject(p.camera, pair.camera) || sameSubject(p.senato, pair.senato));
  const scenario = edit(s, n => { n.lawSettings[REFORM].bonusLinks = [
    ...n.lawSettings[REFORM].bonusLinks.filter(p =>
      !sameSubject(p.camera, pair.camera) && !sameSubject(p.senato, pair.senato)), structuredClone(pair)]; });
  return { scenario, replaced: structuredClone(replaced) };
}
export function removeBonusLink(s: PoliticsScenario, pair: ExplicitBonusLink): PoliticsScenario {
  return edit(s, n => { n.lawSettings[REFORM].bonusLinks = n.lawSettings[REFORM].bonusLinks.filter(p =>
    !(sameSubject(p.camera, pair.camera) && sameSubject(p.senato, pair.senato))); });
}
export function deleteSubject(s: PoliticsScenario, subject: Subject): PoliticsScenario {
  return edit(s, n => {
    if (subject.kind === 'coalition') {
      n.subjects.coalitions = n.subjects.coalitions.filter(c => c.id !== subject.id);
      for (const l of n.subjects.lists) if (l.coalitionId === subject.id) l.coalitionId = null;
    } else {
      n.subjects.lists = n.subjects.lists.filter(l => l.id !== subject.id);
      n.memberships = n.memberships.filter(m => m.listId !== subject.id);
      n.references = n.references.filter(r => r.subject.kind !== 'list' || r.subject.id !== subject.id);
      n.modelSettings.M02.uncertaintyMultipliers = n.modelSettings.M02.uncertaintyMultipliers.filter(r => r.listId !== subject.id);
    }
    for (const law of laws) {
      const block = n.lawSettings[law];
      if (subject.kind === 'list') block.filings = block.filings.filter(f => f.listId !== subject.id);
      for (let i = block.placements.length - 1; i >= 0; i--) {
        const p = block.placements[i];
        if (sameSubject(p.owner, subject) || subject.kind === 'list' && p.affiliationListId === subject.id) block.placements.splice(i, 1);
      }
    }
    const reform = n.lawSettings[REFORM];
    reform.bonusLinks = reform.bonusLinks.filter(p => !sameSubject(p.camera, subject) && !sameSubject(p.senato, subject));
    reform.suppliedPreferences = reform.suppliedPreferences.filter(r => subject.kind !== 'list' || r.listId !== subject.id);
    reform.suppliedVotes = reform.suppliedVotes.filter(r => {
      const votes = r.votes, lists = votes.kind === 'components' ? votes.directListVotes : votes.listVotes;
      return !(subject.kind === 'list' && lists.some(l => l.listId === subject.id)) &&
        !('bonusOnlyVotes' in votes && votes.bonusOnlyVotes.some(b => sameSubject(b.subject, subject)));
    });
  });
}
/** Explicit edit with a reviewable list of initialized references; loading/switching does not repair imports. */
export function initializeReferenceUncertainty(s: PoliticsScenario): { scenario: PoliticsScenario; initializedIds: readonly string[] } {
  const initializedIds: string[] = [];
  const scenario = edit(s, n => {
    for (const rows of [n.references, n.nonValidReferences]) for (let i = 0; i < rows.length; i++) {
      if (rows[i].appliesTo.includes('M02') && rows[i].uncertainty == null) {
        initializedIds.push(rows[i].id); rows[i] = withReferenceDefault(rows[i]);
      }
    }
  });
  return { scenario, initializedIds };
}
