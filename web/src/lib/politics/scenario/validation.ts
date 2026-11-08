import { scenarioShape, chambers, laws, REFORM, ROSATELLUM, type Asset, type LawId,
  type Location, type Placement, type PoliticsScenario } from './schema';
import { activeGenerator, requirePair, scrutinies } from './registry';
import { effectiveFilings, effectiveMembers, effectivePlacements, renewed, resolveBonusLinks, subjectExists, isListFilingLocation, type GeographyRelation } from './effective';
import { assetIdentity, declaredAssets, resolveDependencies } from './dependencies';
import type { ValidationIssue } from '../../core/json-shape';
import { OTHER_LIST_COLOR, OTHER_LIST_ID, OTHER_LIST_NAME } from '../../core/other-list';

export interface DomainIssue { readonly code: string; readonly path: string; readonly message: string }
/** Catalogues contain identities/availability, not copies of historical vote tables. */
export interface DomainContext {
  readonly componentIds: readonly string[];
  readonly sources: readonly { readonly id: string; readonly questionId: string; readonly choiceIds: readonly string[];
    readonly availableDate: string }[];
  readonly pollIds: readonly string[];
  readonly referenceGeography: readonly { readonly kind: 'region' | 'province' | 'municipality'; readonly code: string }[];
  readonly locations: readonly Location[];
  readonly geographyRelations: readonly GeographyRelation[];
  readonly availableAssets: readonly Asset[];
}
export function locationKey(l: Location): string {
  return JSON.stringify([l.law, l.geographyVersion, l.chamber, l.level, l.code]);
}
export function validateScenario(value: unknown, context?: DomainContext): readonly DomainIssue[] {
  if (value && typeof value === 'object' && !Array.isArray(value)) {
    const header = value as Record<string, unknown>;
    if (header.kind !== 'politics-scenario') return [{ code: 'UNSUPPORTED_SCENARIO_FORMAT', path: '$.kind',
      message: 'Only the common politics-scenario format is supported; old scenarios and result/archive envelopes cannot be loaded as scenarios.' }];
    if (header.schemaVersion !== 1) return [{ code: 'UNSUPPORTED_SCENARIO_VERSION', path: '$.schemaVersion', message: 'Expected politics-scenario schemaVersion 1; no automatic conversion is available.' }];
  }
  const shapeIssues: ValidationIssue[] = [];
  if (!scenarioShape.check(value, '$', shapeIssues)) return shapeIssues;
  const s = value as PoliticsScenario, errors: DomainIssue[] = [];
  const error = (code: string, path: string, message: string) => errors.push({ code, path, message });
  const unique = <T>(rows: readonly T[], key: (r: T) => string, path: string) => {
    const seen = new Set<string>();
    for (const row of rows) { const id = key(row); if (seen.has(id)) error('DUPLICATE_ID', path, `Duplicate ${id}`); seen.add(id); }
  };
  const has = (ok: boolean, path: string, message: string) => { if (!ok) error('UNKNOWN_REFERENCE', path, message); };
  const listIds = new Set(s.subjects.lists.map(l => l.id));
  const coalitionIds = new Set(s.subjects.coalitions.map(c => c.id));
  const candidateIds = new Set(s.candidates.map(c => c.id));
  const newIds = new Set(s.newComponents.map(c => c.id));
  const componentExists = (id: string) => newIds.has(id) || s.history.pooling.some(p => p.id === id) || !!context?.componentIds.includes(id) || !context;
  const ownedComponents = new Map(s.memberships.map(m => [m.newComponentId, m.listId]));
  const refSubjectExists = (subject: { kind: 'list' | 'component'; id: string }) => subject.kind === 'list' ? listIds.has(subject.id) : componentExists(subject.id);
  const geoVersion = (law: LawId) => s.data.lawAssets[law].geography.version;
  const checkLocation = (location: Location, law: LawId, path: string) => {
    if (location.law !== law || location.geographyVersion !== geoVersion(law)) error('INVALID_GEOGRAPHY', path, 'Law/geography namespace mismatch');
    if (context) has(context.locations.some(l => locationKey(l) === locationKey(location)), path, 'Unknown pinned law/chamber/location');
  };
  unique(s.subjects.lists, l => l.id, '$.subjects.lists'); unique(s.subjects.coalitions, c => c.id, '$.subjects.coalitions');
  unique(s.candidates, c => c.id, '$.candidates'); unique(s.newComponents, c => c.id, '$.newComponents');
  unique(s.subjects.components, c => c.id, '$.subjects.components');
  for (const c of s.subjects.components) {
    has(componentExists(c.id), '$.subjects.components', 'Unknown historical/pooled presentation component');
    if (newIds.has(c.id)) error('DUPLICATE_PRESENTATION', '$.subjects.components', 'New-component presentation belongs in newComponents');
  }
  unique(s.memberships, m => m.listId, '$.memberships'); unique(s.memberships, m => m.newComponentId, '$.memberships.newComponentId');
  if (context) for (const id of newIds) if (context.componentIds.includes(id)) error('DUPLICATE_ID', '$.newComponents', `Historical ID cannot be redefined: ${id}`);
  for (const c of s.subjects.coalitions) unique(c.chambers, ch => ch, '$.subjects.coalitions.chambers');
  for (const l of s.subjects.lists) {
    has(l.coalitionId === null || coalitionIds.has(l.coalitionId), '$.subjects.lists.coalitionId', `Unknown coalition for ${l.id}`);
    has(s.memberships.some(m => m.listId === l.id), '$.memberships', `Missing membership declaration for ${l.id}`);
    if (l.minority && l.minority.namespace !== s.data.referenceGeography.version) error('INVALID_GEOGRAPHY', '$.subjects.lists.minority', 'Minority region namespace mismatch');
    if (l.kind === 'other' && (l.id !== OTHER_LIST_ID || l.name !== OTHER_LIST_NAME || l.color !== OTHER_LIST_COLOR || l.coalitionId !== null || l.minority !== null))
      error('INVALID_OTHER', '$.subjects.lists', 'Altre liste requires its reserved identity, grey color and autonomous non-minority declaration');
    if (l.id === OTHER_LIST_ID && l.kind !== 'other') error('INVALID_OTHER', '$.subjects.lists', 'Reserved residual ID');
  }
  for (const m of s.memberships) {
    has(listIds.has(m.listId), '$.memberships', `Unknown list ${m.listId}`);
    has(newIds.has(m.newComponentId), '$.memberships.newComponentId', `Missing owned fallback ${m.newComponentId}`);
    for (const ch of chambers) {
      unique(m[ch], id => id, `$.memberships.${ch}`);
      for (const id of m[ch]) {
        has(componentExists(id), `$.memberships.${ch}`, `Unknown component ${id}`);
        if (newIds.has(id) && ownedComponents.get(id) !== m.listId) error('INVALID_MEMBERSHIP', '$.memberships', 'A list cannot use another list’s fallback component');
      }
    }
    if (m.locked && (m.camera.length !== m.senato.length || m.camera.some(id => !m.senato.includes(id)))) error('LOCK_MISMATCH', '$.memberships', `Locked chamber sets differ for ${m.listId}`);
  }
  for (const ch of chambers) {
    const owners = new Map<string, string>();
    for (const m of s.memberships) for (const id of effectiveMembers(s, m.listId, ch)) {
      if (owners.has(id) && owners.get(id) !== m.listId) error('MEMBERSHIP_CONFLICT', `$.memberships.${ch}`, `${id} belongs to conflicting lists`);
      owners.set(id, m.listId);
    }
  }
  for (const law of laws) {
    const block = s.lawSettings[law], path = `$.lawSettings.${law}`;
    try { requirePair(law, s.selection.generatorsByLaw[law].id); } catch (e) { error('UNSUPPORTED_PAIR', '$.selection', String(e)); }
    unique(block.filings, f => f.listId, `${path}.filings`);
    for (const id of listIds) has(block.filings.some(f => f.listId === id), `${path}.filings`, `Missing explicit filings for ${id}`);
    for (const f of block.filings) {
      has(listIds.has(f.listId), `${path}.filings`, `Unknown list ${f.listId}`);
      unique(f.locations, locationKey, `${path}.filings.locations`);
      for (const l of f.locations) {
        checkLocation(l, law, path);
        if (!(scrutinies[law].filingLevels as readonly string[]).includes(l.level)) error('INVALID_GEOGRAPHY', path, `Invalid filing level ${l.level}`);
        if (law === ROSATELLUM && context && !isListFilingLocation(l, context.geographyRelations))
          error('INVALID_GEOGRAPHY', path, 'Uninominal filings are reserved for statutory ballots without a proportional college');
      }
    }
    unique<Placement>(block.placements, p => p.id, `${path}.placements`);
    unique<Placement>(block.placements, p => JSON.stringify([locationKey(p.location), p.owner.kind, p.owner.id, p.role, p.position]), `${path}.placements.slots`);
    for (const p of block.placements) {
      has(candidateIds.has(p.candidateId), path, `Unknown candidate ${p.candidateId}`);
      has(subjectExists(s, p.owner), path, `Unknown placement owner ${p.owner.id}`);
      has(p.affiliationListId === null || listIds.has(p.affiliationListId), path, 'Unknown affiliation');
      checkLocation(p.location, law, path);
      const expected = p.role === 'ordinary' || p.role === 'special' ? 'college' : p.role === 'bonus' ? 'territory' : p.role;
      if (p.location.level !== expected) error('INVALID_GEOGRAPHY', path, 'Placement role/location mismatch');
      if (s.subjects.lists.some(l => l.kind === 'other' && (p.affiliationListId === l.id || p.owner.kind === 'list' && p.owner.id === l.id)))
        error('INVALID_OTHER', path, 'Residual lists cannot have candidates');
    }
    unique(block.externalCandidacies, c => JSON.stringify([c.candidateId, c.chamber, c.kind]), path);
    for (const c of block.externalCandidacies) has(candidateIds.has(c.candidateId), path, 'Unknown external candidate');
  }
  const pairs = s.lawSettings[REFORM].bonusLinks;
  for (const ch of chambers) {
    unique(pairs, p => JSON.stringify([p[ch].kind, p[ch].id]), '$.lawSettings.bonusLinks');
    for (const p of pairs) has(subjectExists(s, p[ch]), '$.lawSettings.bonusLinks', `Unknown ${ch} endpoint`);
  }
  // Eligibility is deliberately not an import invariant: retained pairs must roundtrip.
  resolveBonusLinks(s);
  const reform = s.lawSettings[REFORM];
  unique(reform.suppliedVotes, p => locationKey(p.location), '$.lawSettings.suppliedVotes');
  unique(reform.suppliedPreferences, p => JSON.stringify([locationKey(p.location), p.listId]), '$.lawSettings.suppliedPreferences');
  for (const row of reform.suppliedVotes) {
    checkLocation(row.location, REFORM, '$.lawSettings.suppliedVotes');
    const votes = row.votes;
    const lists = votes.kind === 'components' ? votes.directListVotes : votes.listVotes;
    unique(lists, l => l.listId, '$.lawSettings.suppliedVotes');
    for (const l of lists) has(listIds.has(l.listId), '$.lawSettings.suppliedVotes', 'Unknown supplied-vote list');
    if ('bonusOnlyVotes' in votes) for (const b of votes.bonusOnlyVotes) has(subjectExists(s, b.subject), '$.lawSettings.suppliedVotes', 'Unknown bonus subject');
    if ('candidateTotals' in votes) {
      unique(votes.candidateTotals, c => c.candidateId, '$.lawSettings.suppliedVotes.candidateTotals');
      if ('candidateOnlyVotes' in votes) unique(votes.candidateOnlyVotes, c => c.candidateId, '$.lawSettings.suppliedVotes.candidateOnlyVotes');
    }
  }
  for (const row of reform.suppliedPreferences) {
    checkLocation(row.location, REFORM, '$.lawSettings.suppliedPreferences');
    has(listIds.has(row.listId), '$.lawSettings.suppliedPreferences', 'Unknown preference list');
    if (s.subjects.lists.some(l => l.id === row.listId && l.kind === 'other')) error('INVALID_OTHER', '$.lawSettings.suppliedPreferences', 'Residual lists cannot have preferences');
    if (row.preferences.kind === 'counts') {
      unique(row.preferences.counts, c => c.candidateId, '$.lawSettings.suppliedPreferences.counts');
      if (row.preferences.validity.kind === 'combinations') for (const b of row.preferences.validity.ballots)
        unique(b.candidateIds, id => id, '$.lawSettings.suppliedPreferences.ballots');
    }
  }
  // Supplied legal evidence may address generated candidate keys. Resolve these against the assembled
  // ballot in CP-03's legal validators; never require runtime-generated biographies in the scenario.
  const referenceIds = [...s.references, ...s.nonValidReferences];
  unique(referenceIds, r => r.id, '$.references');
  for (const rows of [s.references, s.nonValidReferences]) {
    const declarations = new Set<string>();
    for (const r of rows) {
      unique(r.appliesTo, id => id, '$.references.appliesTo');
      for (const model of r.appliesTo) {
        const scopeKey = r.scope.kind === 'national' ? ['national'] : [r.scope.kind, r.scope.namespace, r.scope.code];
        const key = JSON.stringify(['subject' in r ? [r.subject.kind, r.subject.id] : null, scopeKey, r.channel, model]);
        if (declarations.has(key)) error('REFERENCE_CONFLICT', '$.references', 'Duplicate reference declaration'); declarations.add(key);
      }
      if ('subject' in r) has(refSubjectExists(r.subject), '$.references.subject', 'Unknown reference subject');
      if (r.scope.kind !== 'national') {
        if (r.scope.namespace !== s.data.referenceGeography.version) error('INVALID_GEOGRAPHY', '$.references.scope', 'Reference namespace mismatch');
        const scope = r.scope;
        if (context) has(context.referenceGeography.some(g => g.kind === scope.kind && g.code === scope.code), '$.references.scope', 'Unknown reference geography');
      }
      if (r.uncertainty && ((r.uncertainty.origin === 'default') !== (r.uncertainty.policyVersion !== null)))
        error('INVALID_UNCERTAINTY', '$.references.uncertainty', 'Default SD requires its policy version; explicit SD has no default policy');
      if (r.uncertainty && (r.mean === 0 || r.mean === 1) && r.uncertainty.sdPercentagePoints !== 0)
        error('INVALID_UNCERTAINTY', '$.references.uncertainty', 'Boundary means require zero SD');
    }
  }
  unique(s.history.electionIds, id => id, '$.history.electionIds'); unique(s.history.polls.selected, id => id, '$.history.polls.selected');
  unique(s.history.polls.added, p => p.id, '$.history.polls.added');
  for (const p of s.history.polls.added) {
    if (context?.pollIds.includes(p.id)) error('DUPLICATE_ID', '$.history.polls.added', 'Bundled polls cannot be overwritten');
    for (const option of p.normalized) for (const id of option.members) has(componentExists(id), '$.history.polls.added', 'Unknown poll component');
  }
  if (context) {
    for (const id of [...s.history.electionIds, ...[s.history.pairedCamera, s.history.pairedSenato].filter((x): x is string => x !== null)])
      has(context.sources.some(x => x.id === id), '$.history', `Unknown source ${id}`);
    for (const id of [...s.history.polls.selected, ...Object.keys(s.history.polls.excluded)])
      has(context.pollIds.includes(id) || s.history.polls.added.some(p => p.id === id), '$.history.polls', `Unknown poll ${id}`);
  }
  const sourceKey = (r: { sourceId: string; questionId: string; choiceId: string }) => JSON.stringify([r.sourceId, r.questionId, r.choiceId]);
  unique(s.history.sourceMemberships, sourceKey, '$.history.sourceMemberships');
  const sourceOwners = new Map<string, string>();
  for (const r of s.history.sourceMemberships) {
    unique(r.members, id => id, '$.history.sourceMemberships');
    if (context) has(context.sources.some(x => x.id === r.sourceId && x.questionId === r.questionId && x.choiceIds.includes(r.choiceId)), '$.history.sourceMemberships', 'Unknown source/question/choice');
    for (const id of r.members) {
      has(componentExists(id), '$.history.sourceMemberships', `Unknown member ${id}`);
      const key = JSON.stringify([r.sourceId, r.questionId, id]);
      if (sourceOwners.has(key) && sourceOwners.get(key) !== r.choiceId) error('MEMBERSHIP_CONFLICT', '$.history.sourceMemberships', 'Conflicting choices in one source/question');
      sourceOwners.set(key, r.choiceId);
    }
  }
  unique(s.history.pooling, p => p.id, '$.history.pooling');
  unique(s.history.pooling.flatMap(p => p.members), id => id, '$.history.pooling.members');
  for (const p of s.history.pooling) {
    if (newIds.has(p.id) || context?.componentIds.includes(p.id)) error('DUPLICATE_ID', '$.history.pooling', 'Pooling output must be a separate identity');
    for (const id of p.members) if (context) has(context.componentIds.includes(id), '$.history.pooling', 'Pool only whole original historical components');
  }
  const compositions = s.modelSettings.M01.compositions;
  unique(compositions, sourceKey, '$.modelSettings.M01.compositions');
  for (const c of compositions) {
    for (const id of Object.keys(c.fractions)) has(componentExists(id), '$.modelSettings.M01.compositions', 'Unknown composition component');
    if (context) has(context.sources.some(x => x.id === c.sourceId && x.questionId === c.questionId && x.choiceIds.includes(c.choiceId)), '$.modelSettings.M01.compositions', 'Unknown composition source choice');
  }
  for (const p of s.modelSettings.M01.supplementalPolls) for (const g of p.groups) for (const id of g.members)
    has(componentExists(id), '$.modelSettings.M01.supplementalPolls', 'Unknown partial-poll component');
  unique(s.modelSettings.M01.supplementalPolls, p => p.id, '$.modelSettings.M01.supplementalPolls');
  unique(s.modelSettings.M02.uncertaintyMultipliers, r => JSON.stringify([r.listId, r.chamber]), '$.modelSettings.M02.uncertaintyMultipliers');
  for (const r of s.modelSettings.M02.uncertaintyMultipliers) has(listIds.has(r.listId), '$.modelSettings.M02.uncertaintyMultipliers', 'Unknown list');
  const assetPins = new Map<string, string>();
  for (const a of declaredAssets(s)) {
    const identity = assetIdentity(a);
    if (assetPins.has(a.id) && assetPins.get(a.id) !== identity) error('CONFLICTING_DEPENDENCY', '$.data', `Conflicting exact pin for ${a.id}`);
    assetPins.set(a.id, identity);
  }
  return errors;
}
export class ScenarioValidationError extends Error {
  constructor(readonly issues: readonly DomainIssue[]) {
    super(issues.map(i => `${i.code} ${i.path}: ${i.message}`).join('\n')); this.name = 'ScenarioValidationError';
  }
}
export function parsePoliticsScenario(value: unknown, context?: DomainContext): PoliticsScenario {
  const issues = validateScenario(value, context);
  if (issues.length) throw new ScenarioValidationError(issues);
  // Detach the authored snapshot from caller-owned objects. Never normalize/default during import.
  return structuredClone(value) as PoliticsScenario;
}
/** Domain preflight only. CP-03 must additionally run the selected adapter's existing legal validators. */
export function validateActiveScenario(s: PoliticsScenario, context: DomainContext): readonly DomainIssue[] {
  const issues = [...validateScenario(s, context)];
  if (issues.length) return issues;
  const error = (code: string, path: string, message: string) => issues.push({ code, path, message });
  const law = s.selection.scrutiny.id, model = activeGenerator(s).id;
  try { resolveDependencies(s, context.availableAssets); } catch (e) { error('EXACT_DEPENDENCY_UNAVAILABLE', '$.data', String(e)); }
  if (!s.data.lawAssets[law].models[model].length) error('MISSING_MODEL_ASSETS', '$.data', 'Active pair needs exact model assets');
  if (s.referenceDate > s.electionDate) error('INVALID_DATE', '$.referenceDate', 'Reference cutoff is after election');
  for (const ch of chambers.filter(ch => renewed(s, ch))) {
    if (!s.subjects.lists.some(l => effectiveFilings(s, law, l.id).some(f => f.chamber === ch)))
      error('EMPTY_PARTICIPATION', '$.lawSettings', `No effective ${ch} lists`);
  }
  if (law === ROSATELLUM) {
    const shares = s.lawSettings[law].candidateGeneration.plurinominalCandidacyCountShares;
    if (shares.length !== 5 || Math.abs(shares.reduce((a, b) => a + b, 0) - 1) > 1e-12)
      error('INVALID_CANDIDATE_POLICY', '$.lawSettings', 'Rosatellum requires five normalized candidature probabilities');
  } else {
    for (const l of s.subjects.lists) if (l.minority && l.minority.legalReference === null && effectiveFilings(s, law, l.id).length)
      error('INCOMPLETE_MINORITY', '$.subjects.lists.minority', `Reform minority declaration needs a legal reference for ${l.id}`);
  }
  if (model === 'M01') {
    if (s.modelSettings.M01.parameterDate > s.electionDate) error('INVALID_DATE', '$.modelSettings.M01.parameterDate', 'Parameter date is after election');
    for (const c of s.modelSettings.M01.compositions) if (Math.abs(Object.values(c.fractions).reduce((a, b) => a + b, 0) - 1) > 1e-12)
      error('INVALID_COMPOSITION', '$.modelSettings.M01', 'Composition fractions must sum to one');
  } else {
    if (!s.history.electionIds.length || s.history.pairedCamera === null || s.history.pairedSenato === null)
      error('INCOMPLETE_HISTORY', '$.history', 'M02 requires selected elections and a Camera/Senato pair');
    for (const id of s.history.electionIds) if (context.sources.some(x => x.id === id && x.availableDate > s.referenceDate))
      error('SOURCE_UNAVAILABLE', '$.history', `Source ${id} is unavailable at the cutoff`);
    for (const r of [...s.references, ...s.nonValidReferences]) if (r.appliesTo.includes(model) && r.uncertainty == null)
      error('MISSING_REFERENCE_SD', '$.references', `Resolve and persist M02 uncertainty for ${r.id}`);
  }
  const placements = effectivePlacements(s, law, context.geographyRelations);
  for (const p of placements) {
    const c = s.candidates.find(c => c.id === p.candidateId)!;
    if (c.availability.kind === 'unavailable') error('INVALID_CANDIDATURE', '$.candidates', `Unavailable candidate ${c.id}`);
    const age = Number(s.electionDate.slice(0, 4)) - Number(c.birthDate.slice(0, 4)) - (s.electionDate.slice(5) < c.birthDate.slice(5) ? 1 : 0);
    if (age < (p.location.chamber === 'senato' ? 40 : 25)) error('INVALID_CANDIDATURE', '$.candidates', `Candidate ${c.id} is too young`);
    if (placements.some(other => other.candidateId === c.id && other.location.chamber !== p.location.chamber) ||
      s.lawSettings[law].externalCandidacies.some(e => e.candidateId === c.id && (e.kind === 'foreign' || e.chamber !== p.location.chamber)))
      error('INVALID_CANDIDATURE', '$.lawSettings', `Incompatible candidacies for ${c.id}`);
  }
  if (law === REFORM && s.renewal === 'joint') for (const link of resolveBonusLinks(s).filter(l => l.status !== 'explicit-inactive')) {
    for (const ch of chambers) {
      const endpoint = link[ch];
      const lists = s.subjects.lists.filter(l => endpoint.kind === 'list' ? l.id === endpoint.id : l.coalitionId === endpoint.id);
      if (!lists.some(l => effectiveFilings(s, law, l.id).some(f => f.chamber === ch)))
        error('EMPTY_LINK_COUNTERPART', '$.lawSettings.bonusLinks', `Declared ${ch} counterpart ${endpoint.id} has no participating lists`);
    }
  }
  return issues;
}
