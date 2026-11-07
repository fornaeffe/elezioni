import { describe, expect, it } from 'vitest';
import { fixture, location } from './scenario.fixture';
import { activeGenerator, canonicalJson, capabilities, createCandidate, createCoalition, createList, decodeScenario, deleteCandidate, deleteSubject,
  dependencyFingerprint, effectiveFilings, effectiveMembers, effectivePlacements, encodeScenario, initializeReferenceUncertainty,
  materializeBonusLinks, parsePoliticsScenario, parseScenarioJson, REFORM, removePlacement, replaceBonusLink, requiredAssets,
  resolveBonusLinks, resolveDependencies, ROSATELLUM, selectGenerator, selectLaw, serializeScenario, setCoalitionScope,
  setFilings, setListCoalition, setMembershipLock, setMembers, validateActiveScenario, validateScenario, verifyAssetBytes,
  type ExplicitBonusLink, type PoliticsScenario } from './index';
import { defaultReferenceSd, resolveReferenceFamily, withReferenceDefault } from './references';

type Mutable<T> = T extends object ? { -readonly [K in keyof T]: Mutable<T[K]> } : T;
const mutable = (s: PoliticsScenario) => structuredClone(s) as Mutable<PoliticsScenario>;
const statuses = (s: PoliticsScenario) => resolveBonusLinks(s).map(l => l.status);
describe('common politics contract', () => {
  it('accepts each of the three target pairs with independent registries and exact active assets', () => {
    const { scenario: s, context } = fixture();
    for (const next of [s, selectGenerator(s, 'M01', '2.0.0'), selectLaw(s, ROSATELLUM)]) {
      expect(validateScenario(next, context)).toEqual([]);
      expect(validateActiveScenario(next, context)).toEqual([]);
      expect(capabilities(next).pair.law).toBe(next.selection.scrutiny.id);
      expect(requiredAssets(next).some(a => a.id.endsWith(`-${activeGenerator(next).id.toLowerCase()}`))).toBe(true);
    }
  });
  it('honors saved per-law selections without resetting inactive blocks or data', () => {
    const { scenario: s } = fixture();
    const selected = selectGenerator(s, 'M01', '2.0.0');
    const round = selectLaw(selectLaw(selected, ROSATELLUM), REFORM);
    expect(round).toEqual(selected);
    expect(activeGenerator(round).id).toBe('M01');
    expect(activeGenerator(s).id).toBe('M02');
    expect(round.modelSettings).toEqual(s.modelSettings);
  });
  it.each([
    ['missing selection', (s: Mutable<PoliticsScenario>) => { delete (s.selection.generatorsByLaw as Partial<typeof s.selection.generatorsByLaw>)[ROSATELLUM]; }],
    ['unsupported pair', (s: Mutable<PoliticsScenario>) => { s.selection.generatorsByLaw[ROSATELLUM].id = 'M02'; }],
    ['old model version', (s: Mutable<PoliticsScenario>) => { (s.selection.generatorsByLaw[REFORM] as { version: string }).version = '1.0.0'; }],
    ['extension version', (s: Mutable<PoliticsScenario>) => { (s.modelSettings.M01 as { schemaVersion: number }).schemaVersion = 2; }],
    ['law block version', (s: Mutable<PoliticsScenario>) => { (s.lawSettings[ROSATELLUM] as { schemaVersion: number }).schemaVersion = 2; }],
    ['scrutiny version', (s: Mutable<PoliticsScenario>) => { (s.selection.scrutiny as { version: string }).version = '2.0.0'; }],
    ['mean percentage', (s: Mutable<PoliticsScenario>) => { s.references[0].mean = 40; }],
    ['negative SD', (s: Mutable<PoliticsScenario>) => { s.references[0].uncertainty!.sdPercentagePoints = -1; }],
    ['unknown list', (s: Mutable<PoliticsScenario>) => { s.references[0].subject.id = 'absent'; }],
    ['duplicate ID', (s: Mutable<PoliticsScenario>) => { s.subjects.lists.push(s.subjects.lists[0]); }],
    ['invalid calendar', (s: Mutable<PoliticsScenario>) => { s.electionDate = '2027-02-29'; }],
    ['missing auto policy', (s: Mutable<PoliticsScenario>) => { delete (s.execution as Partial<typeof s.execution>).workers; }],
    ['missing inactive block', (s: Mutable<PoliticsScenario>) => { delete (s.lawSettings as Partial<typeof s.lawSettings>)[ROSATELLUM]; }],
    ['locked mismatch', (s: Mutable<PoliticsScenario>) => { s.memberships[0].senato = []; }],
    ['historical redefinition', (s: Mutable<PoliticsScenario>) => { s.newComponents[0].id = 'ca'; }],
    ['unknown historical component', (s: Mutable<PoliticsScenario>) => { s.memberships[0].camera = s.memberships[0].senato = ['unknown']; }]
  ])('rejects %s instead of repairing it', (_name, change) => {
    const { scenario, context } = fixture(), s = mutable(scenario); change(s);
    expect(() => parsePoliticsScenario(s, context)).toThrow();
  });
  it('rejects unknown fields, runtime state, old scenarios and result archives', () => {
    const { scenario } = fixture();
    for (const value of [{ ...scenario, fittedState: [] }, { ...scenario, kind: 'politics-reform-scenario' },
      { ...scenario, schemaVersion: 9 }, { schemaVersion: 1, kind: 'politics-result', scenario }])
      expect(() => parsePoliticsScenario(value)).toThrow();
    expect(() => parseScenarioJson('{bad json')).toThrow('INVALID_SCENARIO_JSON');
    expect(() => decodeScenario(new Uint8Array([0xff]))).toThrow('INVALID_SCENARIO_ENCODING');
  });
  it('preserves null, absence, empty collections and explicit zeros in JSON and gzip, including inactive data', () => {
    const { scenario, context } = fixture(), s = mutable(scenario);
    s.references[0].mean = 0; s.references[0].uncertainty!.sdPercentagePoints = 0;
    s.references.push({ ...s.references[0], id: 'absent-sd', channel: 'camera', uncertainty: undefined });
    s.references.push({ ...s.references[0], id: 'null-sd', channel: 'senato', uncertainty: null });
    const imported = parseScenarioJson(serializeScenario(s), context);
    expect(Object.hasOwn(imported.references[1], 'uncertainty')).toBe(false);
    expect(imported.references[2].uncertainty).toBeNull();
    expect(imported.references[0].mean).toBe(0);
    expect(imported.modelSettings.M01.compositions[0].fractions.cb).toBe(0);
    for (const compressed of [false, true]) expect(decodeScenario(encodeScenario(imported, compressed), context)).toEqual(imported);
    expect(encodeScenario(imported, true)).toEqual(encodeScenario(imported, true));
  });
  it('distinguishes incomplete inactive semantics from full-document structure and active errors', () => {
    const { scenario, context } = fixture(), s = mutable(scenario);
    s.lawSettings[ROSATELLUM].candidateGeneration.plurinominalCandidacyCountShares = [];
    expect(validateScenario(s, context)).toEqual([]);
    expect(validateActiveScenario(s, context)).toEqual([]);
    expect(validateActiveScenario(selectLaw(s, ROSATELLUM), context).map(i => i.code)).toContain('INVALID_CANDIDATE_POLICY');
    expect(decodeScenario(encodeScenario(s, true))).toEqual(s);
  });
  it('validates law/chamber geography even for inactive declarations and rejects equal-looking foreign codes', () => {
    const { scenario, context } = fixture(), s = mutable(scenario);
    s.lawSettings[ROSATELLUM].filings[0].locations[0] = location(REFORM, 'camera');
    expect(validateScenario(s, context).map(i => i.code)).toContain('INVALID_GEOGRAPHY');
    s.lawSettings[ROSATELLUM].filings[0].locations[0] = { ...location(ROSATELLUM, 'camera'), chamber: 'senato', code: 'missing' };
    expect(validateScenario(s, context).map(i => i.code)).toContain('UNKNOWN_REFERENCE');
  });
  it('resolves exact dependencies and bytes without latest/fallback substitutions or requiring inactive assets', async () => {
    const { scenario: s, context } = fixture();
    const required = requiredAssets(s);
    expect(resolveDependencies(s, required)).toEqual(required);
    expect(() => resolveDependencies(s, required.map(a => a.id === 'history' ? { ...a, version: 'latest' } : a))).toThrow('EXACT_DEPENDENCY_UNAVAILABLE');
    expect(() => resolveDependencies(s, [...required, required[0]])).toThrow('EXACT_DEPENDENCY_UNAVAILABLE');
    const edited = { ...s, name: 'Renamed' };
    expect(await dependencyFingerprint(edited)).toBe(await dependencyFingerprint(s));
    expect(await dependencyFingerprint(selectLaw(s, ROSATELLUM))).not.toBe(await dependencyFingerprint(s));
    const bytes = new TextEncoder().encode('abc');
    await expect(verifyAssetBytes({ ...required[0], bytes: 3, sha256: 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad' }, bytes)).resolves.toBeUndefined();
    await expect(verifyAssetBytes(required[0], bytes)).rejects.toThrow('CHECKSUM_MISMATCH');
    expect(validateActiveScenario(s, { ...context, availableAssets: required.slice(1) }).map(i => i.code)).toContain('EXACT_DEPENDENCY_UNAVAILABLE');
  });
});

describe('atomic identity and membership edits', () => {
  it('copies Camera to Senato while locked, preserves independent inactive sets and replaces on relock', () => {
    const { scenario: s, context } = fixture();
    const changed = setMembers(s, 'a', 'camera', ['cc'], context);
    expect(changed.memberships[0].senato).toEqual(['cc']);
    expect(s.memberships[0].camera).toEqual(['ca']);
    expect(() => setMembers(s, 'a', 'senato', [])).toThrow('MEMBERSHIP_LOCKED');
    const unlocked = setMembers(setMembershipLock(changed, 'a', false), 'a', 'senato', []);
    expect(unlocked.memberships[0].camera).toEqual(['cc']);
    expect(effectiveMembers(unlocked, 'a', 'senato')).toEqual(['na']);
    const locked = setMembershipLock(unlocked, 'a', true, context);
    expect(locked.memberships[0].senato).toEqual(['cc']);
    expect(locked.newComponents).toEqual(s.newComponents);
    expect(setMembers(locked, 'a', 'camera', []).memberships[0].senato).toEqual([]);
  });
  it('scopes conflicts by chamber and rejects relock without any partial edits', () => {
    const { scenario, context } = fixture();
    let s = setMembershipLock(setMembershipLock(scenario, 'a', false), 'b', false);
    s = setMembers(s, 'a', 'senato', []);
    s = setMembers(s, 'b', 'senato', ['ca']);
    expect(validateScenario(s, context)).toEqual([]);
    const before = canonicalJson(s);
    expect(() => setMembershipLock(s, 'a', true, context)).toThrow('MEMBERSHIP_CONFLICT');
    expect(canonicalJson(s)).toBe(before);
    expect(() => setMembers(scenario, 'a', 'camera', ['cb'])).toThrow('MEMBERSHIP_CONFLICT');
    expect(() => setMembers(scenario, 'a', 'camera', ['ca', 'ca'])).toThrow('DUPLICATE_ID');
  });
  it('new lists initialize all eligible filings under every law and remain effectively restricted by coalition scope', () => {
    const { scenario, context } = fixture();
    const s = createList(scenario, { ...scenario.subjects.lists[0], id: 'c', name: 'Nuova C' },
      { ...scenario.newComponents[0], id: 'nc' }, context);
    for (const law of [ROSATELLUM, REFORM] as const) {
      expect(s.lawSettings[law].filings.find(f => f.listId === 'c')!.locations.some(l => l.chamber === 'senato')).toBe(true);
      expect(effectiveFilings(s, law, 'c').every(l => l.chamber === 'camera')).toBe(true);
    }
    expect(s.memberships.at(-1)).toMatchObject({ locked: true, camera: [], senato: [], newComponentId: 'nc' });
    const empty = setFilings(s, REFORM, 'c', []);
    expect(selectLaw(selectLaw(empty, ROSATELLUM), REFORM).lawSettings[REFORM].filings.at(-1)!.locations).toEqual([]);
  });
  it('candidate creation requires explicit gender and never merges names or recalculates assumed dates', () => {
    const { scenario } = fixture();
    const input = { id: 'one', name: 'Omonimo', gender: 'female' as const, birthDate: '', availability: { kind: 'available' as const } };
    const s = createCandidate(createCandidate(scenario, input), { ...input, id: 'two' });
    expect(s.candidates.map(c => c.id)).toEqual(['one', 'two']);
    expect(s.candidates[0]).toMatchObject({ birthDate: '1977-01-01', birthDateOrigin: 'assumed' });
    expect(selectLaw({ ...s, electionDate: '2030-01-01' }, ROSATELLUM).candidates[0].birthDate).toBe('1977-01-01');
    expect(() => createCandidate(scenario, { ...input, birthDate: '2020-02-30' })).toThrow('INVALID_BIRTH_DATE');
    expect(() => createCandidate(scenario, { ...input, gender: undefined! })).toThrow();
  });
  it('identity deletion cascades across inactive laws while individual removal preserves slot numbers and identities', () => {
    const { scenario } = fixture();
    const s = mutable(createCandidate(scenario, { id: 'one', name: 'Person', gender: 'male', availability: { kind: 'available' } }));
    const pin = { id: 'pin', candidateId: 'one', location: location(REFORM, 'camera'), owner: { kind: 'list' as const, id: 'a' }, position: 2, affiliationListId: 'a' };
    s.lawSettings[REFORM].placements.push({ ...pin, role: 'ordinary' });
    s.lawSettings[ROSATELLUM].placements.push({ ...pin, id: 'old-pin', location: location(ROSATELLUM, 'camera'), role: 'plurinominal', minority: false });
    s.lawSettings[ROSATELLUM].externalCandidacies.push({ candidateId: 'one', chamber: 'senato', kind: 'domestic' });
    expect(effectivePlacements(s, REFORM)).toHaveLength(1);
    const removed = removePlacement(s, REFORM, 'pin');
    expect(removed.candidates).toHaveLength(1);
    expect(removed.lawSettings[ROSATELLUM].placements[0].position).toBe(2);
    const deleted = deleteCandidate(s, 'one');
    expect(deleted.candidates).toEqual([]);
    for (const law of [REFORM, ROSATELLUM] as const) {
      expect(deleted.lawSettings[law].placements).toEqual([]);
      expect(deleted.lawSettings[law].externalCandidacies).toEqual([]);
      expect(deleted.lawSettings[law].filings).toEqual(s.lawSettings[law].filings);
    }
  });
});

describe('D11 reciprocal declarations', () => {
  it('retains and reactivates original endpoints across scope, affiliation and filing edits', () => {
    const { scenario: s } = fixture();
    expect(statuses(s)).toEqual(['explicit-active']);
    const both = setCoalitionScope(s, 'A', ['camera', 'senato']);
    expect(statuses(both)).toEqual(['automatic', 'explicit-inactive']);
    expect(materializeBonusLinks(both)).toEqual([{ camera: { kind: 'coalition', id: 'A' }, senato: { kind: 'coalition', id: 'A' } }]);
    expect(statuses(setCoalitionScope(both, 'A', ['camera']))).toEqual(['explicit-active']);
    const reversed = setCoalitionScope(s, 'A', ['senato']);
    expect(statuses(reversed)).toEqual(['explicit-inactive']);
    expect(reversed.lawSettings[REFORM].bonusLinks).toEqual(s.lawSettings[REFORM].bonusLinks);
    expect(materializeBonusLinks(reversed)).toEqual([]);
    expect(statuses(setListCoalition(s, 'b', 'A'))).toEqual(['explicit-inactive']);
    expect(statuses(setListCoalition(setListCoalition(s, 'b', 'A'), 'b', null))).toEqual(['explicit-active']);
    expect(statuses(setFilings(s, REFORM, 'a', []))).toEqual(['explicit-active']);
    const listBoth = setFilings(s, REFORM, 'b', [location(REFORM, 'camera'), location(REFORM, 'senato')]);
    expect(statuses(listBoth)).toEqual(['automatic', 'explicit-inactive']);
    expect(statuses(setFilings(listBoth, REFORM, 'b', [location(REFORM, 'senato')]))).toEqual(['explicit-active']);
  });
  it('preserves empty automatic counterparts and reports participation separately; single renewal emits no links', () => {
    const { scenario, context } = fixture();
    const s = setCoalitionScope(scenario, 'A', ['camera', 'senato']);
    expect(resolveBonusLinks(s)[0].status).toBe('automatic');
    expect(validateActiveScenario(s, context).map(i => i.code)).toContain('EMPTY_LINK_COUNTERPART');
    expect(materializeBonusLinks({ ...s, renewal: 'camera' })).toEqual([]);
    expect(decodeScenario(encodeScenario({ ...s, renewal: 'camera' }, true))).toEqual({ ...s, renewal: 'camera' });
  });
  it('atomically replaces every pair using either endpoint, including retained pairs', () => {
    const { scenario } = fixture();
    let s = createCoalition(scenario, { id: 'C', name: 'C', color: '#123456' });
    s = setCoalitionScope(s, 'C', ['camera']);
    s = createCoalition(s, { id: 'D', name: 'D', color: '#654321' });
    s = setCoalitionScope(s, 'D', ['senato']);
    const pair: ExplicitBonusLink = { camera: { kind: 'coalition', id: 'C' }, senato: { kind: 'coalition', id: 'D' } };
    s = replaceBonusLink(s, pair).scenario;
    const next = replaceBonusLink(s, { camera: { kind: 'coalition', id: 'A' }, senato: pair.senato });
    expect(next.replaced).toHaveLength(2);
    expect(next.scenario.lawSettings[REFORM].bonusLinks).toHaveLength(1);
    expect(() => replaceBonusLink(setCoalitionScope(s, 'A', ['camera', 'senato']), scenario.lawSettings[REFORM].bonusLinks[0])).toThrow('INELIGIBLE_LINK_ENDPOINT');
  });
  it('rejects malformed, duplicated, unknown or overlapping saved endpoints even while inactive', () => {
    const { scenario } = fixture();
    const both = mutable(setCoalitionScope(scenario, 'A', ['camera', 'senato']));
    both.lawSettings[REFORM].bonusLinks.push(both.lawSettings[REFORM].bonusLinks[0]);
    expect(() => parsePoliticsScenario(both)).toThrow('DUPLICATE_ID');
    // Property order cannot evade endpoint identity checks.
    both.lawSettings[REFORM].bonusLinks[1] = { camera: { id: 'A', kind: 'coalition' }, senato: { id: 'b', kind: 'list' } };
    expect(() => parsePoliticsScenario(both)).toThrow('DUPLICATE_ID');
    both.lawSettings[REFORM].bonusLinks.pop();
    both.lawSettings[REFORM].bonusLinks[0].senato.id = 'unknown';
    expect(() => parsePoliticsScenario(both)).toThrow('UNKNOWN_REFERENCE');
    const serialized = serializeScenario(setCoalitionScope(scenario, 'A', ['camera', 'senato']));
    expect(serialized).not.toContain('explicit-inactive');
    expect(serialized).not.toContain('automatic-counterpart');
    expect(() => parseScenarioJson(serialized.replace('"bonusLinks":[{', '"bonusLinks":[{"status":"automatic",'))).toThrow();
  });
  it('deletes subject pairs and does not recreate them from equal names', () => {
    const { scenario } = fixture();
    const deleted = deleteSubject(scenario, { kind: 'coalition', id: 'A' });
    expect(deleted.lawSettings[REFORM].bonusLinks).toEqual([]);
    expect(deleted.subjects.lists[0].coalitionId).toBeNull();
    const recreated = createCoalition(deleted, { id: 'new-A', name: 'Coalizione A', color: '#2563EB' });
    expect(recreated.lawSettings[REFORM].bonusLinks).toEqual([]);
    expect(deleteSubject(scenario, { kind: 'list', id: 'b' }).lawSettings[REFORM].bonusLinks).toEqual([]);
  });
});

describe('reference conventions and retained model meaning', () => {
  it('uses explicit units, capped scope defaults, boundary zero and preserves explicit SD', () => {
    const { scenario } = fixture();
    const row = scenario.references[0];
    for (const [kind, sd] of [['national', 1], ['region', 2], ['province', 3], ['municipality', 5]] as const) {
      const scope = kind === 'national' ? { kind } : { kind, namespace: 'geo', code: '01' };
      expect(defaultReferenceSd(0.1, scope)).toBe(sd);
      expect(defaultReferenceSd(0.01, scope)).toBe(0.5);
      expect(defaultReferenceSd(0, scope)).toBe(0);
      expect(defaultReferenceSd(1, scope)).toBe(0);
    }
    expect(withReferenceDefault(row)).toEqual(row);
    expect(withReferenceDefault({ ...row, mean: 0.01, uncertainty: null }).uncertainty).toMatchObject({ sdPercentagePoints: 0.5, origin: 'default' });
  });
  it('retains missing M01 SDs but reports M02 gaps and makes initialization an explicit reviewable edit', () => {
    const { scenario, context } = fixture(), s = mutable(scenario);
    delete s.references[0].uncertainty;
    expect(validateActiveScenario(selectGenerator(s, 'M01', '2.0.0'), context)).toEqual([]);
    expect(validateActiveScenario(s, context).map(i => i.code)).toContain('MISSING_REFERENCE_SD');
    const resolved = initializeReferenceUncertainty(s);
    expect(resolved.initializedIds).toEqual(['r-a']);
    expect(s.references[0].uncertainty).toBeUndefined();
    expect(resolved.scenario.references[0].uncertainty).toMatchObject({ origin: 'default', policyVersion: 'reference-defaults-v1' });
  });
  it('validates complementary reference families through the existing M02 feasibility engine', () => {
    const { scenario } = fixture(), row = scenario.references[0];
    const a = { ...row, mean: 0.01, uncertainty: null };
    const b = { ...row, id: 'r-b', mean: 0.99, uncertainty: { sdPercentagePoints: 0.4, origin: 'explicit' as const, policyVersion: null } };
    const resolved = resolveReferenceFamily([{ reference: a, members: ['ca'] }, { reference: b, members: ['cb'] }], false);
    expect(resolved.references.map(r => r.uncertainty!.sdPercentagePoints)).toEqual([0.4, 0.4]);
    expect(a.uncertainty).toBeNull();
    expect(() => resolveReferenceFamily([{ reference: { ...a, uncertainty: { ...b.uncertainty, sdPercentagePoints: 0.1 } }, members: ['ca'] },
      { reference: b, members: ['cb'] }], false)).toThrow('REFERENCE_MOMENTS_UNSUPPORTED');
    const fixed = resolveReferenceFamily([{ reference: a, members: ['ca'] },
      { reference: { ...b, uncertainty: { ...b.uncertainty, sdPercentagePoints: 0 } }, members: ['cb'] }], false);
    expect(fixed.references.map(r => r.uncertainty!.sdPercentagePoints)).toEqual([0, 0]);
  });
});

describe('retained advanced declarations', () => {
  it('roundtrips source/poll metadata, pooling, M01 compositions/partial observations and component presentation', () => {
    const { scenario, context } = fixture(), s = mutable(scenario);
    s.history.pooling = [{ id: 'pool', members: ['cc'], provenance: 'Whole original components, not fractional counts' }];
    s.subjects.components = [{ id: 'pool', name: 'Componente raggruppato', support: 'fitted' }];
    s.history.polls.added = [{ id: 'poll', surveyId: 'survey', context: 'camera', date: '2026-01-01',
      fieldworkStart: '2025-12-29', fieldworkEnd: '2025-12-31', availableDate: '2026-01-02', citation: 'Test citation',
      sampleSize: 1000, questionSampleSize: null, decidedFraction: 0.6, effectiveSample: 600, designEffect: 1,
      effectiveSamplePolicy: 'test-policy', errorGroup: 'survey', roundingWidth: 0.01,
      original: [{ label: 'A', percentage: 40, kind: 'list' }], normalized: [
        { label: 'A', members: ['ca'], share: 0.4, other: false }, { label: 'Other', members: [], share: 0.6, other: true }],
      otherDefinition: 'offered-complement', status: 'supported', reasons: [], method: 'Test method' }];
    s.history.polls.selected = ['poll']; s.history.polls.excluded = { poll: 'Explicit exclusion reason' };
    s.modelSettings.M01.supplementalPolls = [{ id: 'partial', date: '2026-01-01', available: '2026-01-02', end: '2025-12-31',
      context: 'camera', groups: [{ label: 'A', members: ['ca'], share: 0.4 }], sample: 500, source: 'Partial poll citation', enabled: true }];
    for (const selected of [s, selectGenerator(s, 'M01', '2.0.0'), selectLaw(s, ROSATELLUM)])
      expect(decodeScenario(encodeScenario(selected, true), context)).toEqual(selected);
    const conflicting = mutable(s);
    conflicting.history.sourceMemberships.push({ ...s.history.sourceMemberships[0], choiceId: 'b' });
    expect(validateScenario(conflicting, context).map(i => i.code)).toContain('MEMBERSHIP_CONFLICT');
  });
  it('retains every supplied vote/preference representation, zero counts, validity evidence and runtime candidate keys', () => {
    const { scenario, context } = fixture();
    const provenance = { kind: 'supplied' as const, reference: 'Reviewed ballot totals' };
    const ordinary = [
      { kind: 'attributed' as const, listVotes: [{ listId: 'a', votes: 0 }], provenance },
      { kind: 'components' as const, directListVotes: [{ listId: 'a', votes: 0 }], bonusOnlyVotes: [{ subject: { kind: 'coalition' as const, id: 'A' }, votes: 10 }], provenance }
    ];
    const special = [
      { kind: 'attributed' as const, listVotes: [{ listId: 'a', votes: 0 }], candidateTotals: [{ candidateId: 'generated-key', votes: 0 }], provenance },
      { kind: 'components' as const, directListVotes: [{ listId: 'a', votes: 0 }], candidateOnlyVotes: [{ candidateId: 'generated-key', votes: 0 }],
        candidateTotals: [{ candidateId: 'generated-key', votes: 0 }], provenance }
    ];
    for (const votes of [...ordinary, ...special]) {
      const s = mutable(scenario);
      s.lawSettings[REFORM].suppliedVotes = [{ location: location(REFORM, 'camera'), votes }];
      const inactive = selectLaw(s, ROSATELLUM);
      expect(decodeScenario(encodeScenario(inactive, true), context)).toEqual(inactive);
    }
    for (const preferences of [
      { kind: 'missing' as const, reason: 'Not observed, not zero' },
      { kind: 'counts' as const, counts: [{ candidateId: 'generated-key', count: 0 }], provenance,
        validity: { kind: 'adjudicated' as const, reference: 'Adjudicated totals' } },
      { kind: 'counts' as const, counts: [{ candidateId: 'generated-key', count: 0 }], provenance,
        validity: { kind: 'combinations' as const, ballots: [{ candidateIds: [] as string[], count: 20 }] } }
    ]) {
      const s = mutable(scenario);
      s.lawSettings[REFORM].suppliedPreferences = [{ location: location(REFORM, 'camera'), listId: 'a', preferences }];
      expect(decodeScenario(encodeScenario(s), context)).toEqual(s);
    }
  });
  it('limits effective placements to related filings and retains pins when a branch becomes inactive', () => {
    const { scenario, context } = fixture();
    const s = mutable(createCandidate(scenario, { id: 'one', name: 'Person', gender: 'male', availability: { kind: 'available' } }));
    s.lawSettings[REFORM].placements = [{ id: 'pin', candidateId: 'one', role: 'bonus', location: location(REFORM, 'camera', 'territory'),
      owner: { kind: 'coalition', id: 'A' }, position: 1, affiliationListId: 'a' }];
    expect(effectivePlacements(s, REFORM, context.geographyRelations)).toHaveLength(1);
    const unfiled = setFilings(s, REFORM, 'a', []);
    expect(effectivePlacements(unfiled, REFORM, context.geographyRelations)).toEqual([]);
    expect(unfiled.lawSettings[REFORM].placements).toEqual(s.lawSettings[REFORM].placements);
    expect(effectivePlacements({ ...s, renewal: 'senato' }, REFORM, context.geographyRelations)).toEqual([]);
  });
  it('validates active biography/availability without treating inactive-law candidacies as simultaneous', () => {
    const { scenario, context } = fixture();
    const s = mutable(createCandidate(scenario, { id: 'one', name: 'Person', gender: 'male', birthDate: '2000-01-01', availability: { kind: 'available' } }));
    const common = { id: 'pin', candidateId: 'one', position: 1, affiliationListId: 'a', owner: { kind: 'list' as const, id: 'a' } };
    s.lawSettings[REFORM].placements.push({ ...common, role: 'ordinary', location: location(REFORM, 'camera') });
    s.lawSettings[ROSATELLUM].externalCandidacies.push({ candidateId: 'one', chamber: 'senato', kind: 'domestic' });
    expect(validateActiveScenario(s, context)).toEqual([]);
    s.candidates[0].availability = { kind: 'unavailable', reason: 'Declared unavailable' };
    expect(validateScenario(s, context)).toEqual([]);
    expect(validateActiveScenario(s, context).map(i => i.code)).toContain('INVALID_CANDIDATURE');
  });
});
