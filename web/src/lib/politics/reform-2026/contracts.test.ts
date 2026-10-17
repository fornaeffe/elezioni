import { describe, expect, it } from 'vitest';
import { validateReformScrutinyInput, type ValidationIssue } from './index';
import { jointFixture, singleFixture, supplied, type Fixture } from './contracts.fixture';

function camera(f: Fixture) {
  if (f.renewal.kind !== 'joint') throw new Error('Expected joint fixture');
  return f.renewal.camera;
}
function issues(value: unknown): readonly ValidationIssue[] {
  const result = validateReformScrutinyInput(value);
  expect(result.valid).toBe(false);
  return result.issues;
}
function valid(value: unknown) {
  const result = validateReformScrutinyInput(value);
  expect(result.issues).toEqual([]);
  expect(result.valid).toBe(true);
  if (!result.valid) throw new Error('Invalid fixture');
  return result.input;
}

describe('R26-05 versioned input boundary', () => {
  it('accepts a JSON-safe joint renewal with complete domestic totals and explicit links', () => {
    const input = jointFixture();
    expect(JSON.parse(JSON.stringify(valid(input)))).toEqual(input);
  });
  it.each(['camera', 'senato'] as const)('accepts a single %s renewal without bonus slates', side => {
    valid(singleFixture(side));
  });
  it('isolates the validated payload from later caller mutations', () => {
    const f = jointFixture();
    const result = valid(f);
    f.candidates[0].name = 'Changed';
    expect(result.candidates[0].name).not.toBe('Changed');
  });
  it.each([0, 2, '1'])('rejects envelope version %s rather than silently migrating it', schemaVersion => {
    expect(issues({ ...jointFixture(), schemaVersion })[0].code).toBe('INVALID_SHAPE');
  });
  it('rejects a legacy scrutiny input without changing its contract', () => {
    expect(issues({ liste_uni: [], candidati_pluri: [] }).some(x => x.path === '$.schemaVersion')).toBe(true);
  });
  it.each([-1, 0.5, NaN, Infinity, Number.MAX_SAFE_INTEGER + 1])('rejects an invalid vote count %s', votes => {
    const f = jointFixture();
    camera(f).ordinaryVotes[0].votes = { kind: 'attributed', listVotes: [{ listId: 'A', votes }], provenance: supplied };
    expect(issues(f).some(x => x.code === 'INVALID_SHAPE')).toBe(true);
  });
  it('rejects duplicate identities even when display names differ', () => {
    const f = jointFixture();
    f.candidates.push({ ...f.candidates[0], name: 'Another name' });
    expect(issues(f).some(x => x.code === 'DUPLICATE_ID')).toBe(true);
  });
  it('reports unknown geographical and political references', () => {
    const f = jointFixture();
    camera(f).colleges[0].territoryId = 'missing';
    camera(f).ordinarySlates[0].listId = 'unknown';
    const errors = issues(f);
    expect(errors.some(x => x.code === 'UNKNOWN_REFERENCE' && x.message.includes('missing'))).toBe(true);
    expect(errors.some(x => x.code === 'UNKNOWN_REFERENCE' && x.message.includes('unknown'))).toBe(true);
  });
  it('requires explicit reciprocal coalition and cross-chamber links', () => {
    const f = jointFixture();
    camera(f).lists[0].coalitionId = 'C';
    camera(f).coalitions = [{ id: 'C', listIds: [] }];
    f.bonusLinks[0].senatoDeclares = false;
    expect(issues(f).filter(x => x.code === 'INVALID_LINK').length).toBeGreaterThanOrEqual(2);
  });
  it('accepts a single coalition bonus slate with separate candidate affiliations', () => {
    const f = jointFixture();
    const c = camera(f);
    c.lists[0].coalitionId = 'C';
    c.coalitions = [{ id: 'C', listIds: ['A'] }];
    c.bonusSlates[0].subject = { kind: 'coalition', id: 'C' };
    f.bonusLinks[0].camera = { kind: 'coalition', id: 'C' };
    valid(f);
  });
  it('does not require a cross-chamber link inferred from equal list labels', () => {
    const f = jointFixture(); f.bonusLinks = [];
    valid(f);
  });
  it('rejects mismatched chamber tags in a joint envelope', () => {
    const f = jointFixture(); camera(f).chamber = 'senato';
    expect(issues(f).some(x => x.code === 'INCOMPATIBLE_RENEWAL')).toBe(true);
  });
});

describe('filed candidatures, coverage and later availability', () => {
  it('retains seven filed slots when a candidate becomes unavailable', () => {
    const f = jointFixture();
    f.candidates[0].availability = { kind: 'unavailable', reason: 'Initial proclamation cannot fill this candidacy' };
    valid(f);
    camera(f).ordinarySlates[0].candidateIds.pop();
    expect(issues(f).some(x => x.code === 'INVALID_CANDIDATURE')).toBe(true);
  });
  it('rejects duplicate ordinary slots and a broken alternating order', () => {
    const f = jointFixture();
    const s = camera(f).ordinarySlates[0]; s.candidateIds[1] = s.candidateIds[0];
    const errors = issues(f);
    expect(errors.some(x => x.code === 'DUPLICATE_ID')).toBe(true);
    expect(errors.some(x => x.message.includes('alternate'))).toBe(true);
  });
  it.each([['camera', '2002-05-01'], ['senato', '1987-05-01']] as const)('uses exact election-day candidate age for %s', (side, birthDate) => {
    const f = singleFixture(side);
    f.candidates[0].birthDate = birthDate;
    valid(f);
    f.candidates[0].birthDate = birthDate.slice(0, 8) + '02';
    expect(issues(f).some(x => x.lawReference === 'K56/K58')).toBe(true);
  });
  it('rejects invalid dates and the former Senate age-25 electorate', () => {
    const f = jointFixture(); f.candidates[0].birthDate = '1980-02-30';
    expect(issues(f).some(x => x.code === 'INVALID_SHAPE')).toBe(true);
    const g = jointFixture();
    const malformed = { ...camera(g).ordinaryVotes[0], electorate: { minimumAge: 25, electors: 200 } };
    expect(issues({ ...g, renewal: { ...g.renewal, camera: { ...camera(g), ordinaryVotes: [malformed] } } }).some(x => x.code === 'INVALID_SHAPE')).toBe(true);
  });
  it('rejects the same identity in both chambers and declared foreign ordinary candidatures', () => {
    const f = jointFixture();
    if (f.renewal.kind !== 'joint') throw new Error('Expected joint');
    f.renewal.senato.ordinarySlates[0].candidateIds[0] = camera(f).ordinarySlates[0].candidateIds[0];
    expect(issues(f).some(x => x.message.includes('both chambers'))).toBe(true);
    const g = jointFixture(); g.candidates[0].externalCandidacies = [{ chamber: 'camera', kind: 'foreign' }];
    expect(issues(g).some(x => x.lawReference?.includes('C19(5)'))).toBe(true);
  });
  it('checks declared other-chamber candidatures in a single renewal', () => {
    const f = singleFixture('camera');
    f.candidates[0].externalCandidacies = [{ chamber: 'senato', kind: 'foreign' }];
    expect(issues(f).some(x => x.lawReference === 'C19(6)')).toBe(true);
  });
  it('requires the qualifying local bonus prefix, not just any ordinary appearance', () => {
    const f = jointFixture(); const c = camera(f);
    const fresh = { ...f.candidates[0], id: 'new-head' }; f.candidates.push(fresh);
    c.ordinarySlates[0].candidateIds[0] = fresh.id;
    c.bonusSlates[0].candidates[0].candidateId = fresh.id;
    valid(f);
    c.bonusSlates[0].candidates[0].candidateId = c.ordinarySlates[1].candidateIds[0];
    expect(issues(f).some(x => x.message.includes('placement/prefix'))).toBe(true);
  });
  it('rejects a special candidate simultaneously placed in a bonus slate', () => {
    const f = jointFixture(); const c = camera(f);
    c.specialCandidatures[0].candidateId = c.bonusSlates[0].candidates[0].candidateId;
    expect(issues(f).some(x => x.message.includes('special/bonus'))).toBe(true);
  });
  it('enforces at most five ordinary colleges independently of later exhaustion', () => {
    const f = singleFixture('camera');
    if (f.renewal.kind !== 'single') throw new Error('Expected single');
    const c = f.renewal.chamber;
    const ids = [...c.ordinarySlates[0].candidateIds];
    for (let i = 1; i < 5; i++) c.ordinarySlates[i].candidateIds = [...ids];
    valid(f);
    c.ordinarySlates[5].candidateIds = [...ids];
    expect(issues(f).some(x => x.message.includes('multiple candidacies'))).toBe(true);
  });
  it('checks the national special uninominal gender limit', () => {
    const f = jointFixture();
    // Special contests have their own national subject limit.
    if (f.renewal.kind !== 'joint') throw new Error('Expected joint');
    for (const candidate of f.renewal.senato.specialCandidatures) {
      f.candidates.find(p => p.id === candidate.candidateId)!.gender = 'male';
    }
    expect(issues(f).some(x => x.message.includes('60%'))).toBe(true);
  });
  it('accepts 42/70 bonus candidates of one gender and rejects 43/70', () => {
    const f = jointFixture(); const c = camera(f);
    // Ten original slates: keep seven complete prefixes and three five-name
    // prefixes (37 men, 27 women), then six new heads (five men, one woman).
    c.bonusSlates[0].candidates = c.ordinarySlates.slice(0, 10).flatMap((s, i) =>
      s.candidateIds.slice(0, i < 7 ? 7 : 5).map(candidateId => ({ candidateId, affiliationListId: 'A' })));
    c.colleges[0].seatsWithoutBonus -= 6; c.colleges[0].seatsWithBonus -= 6;
    const addedPeople: typeof f.candidates = [];
    for (let i = 0; i < 6; i++) {
      const collegeId = `extra-${i}`;
      const people = c.ordinarySlates[0].candidateIds.map((id, j) => ({ ...f.candidates.find(p => p.id === id)!, id: `${collegeId}-${j}` }));
      if (i === 0) people.forEach(p => p.gender = p.gender === 'male' ? 'female' : 'male');
      addedPeople.push(...people); f.candidates.push(...people);
      c.colleges.push({ id: collegeId, territoryId: 'O', kind: 'ordinary', seatsWithoutBonus: 1, seatsWithBonus: 1 });
      c.ordinarySlates.push({ collegeId, listId: 'A', candidateIds: people.map(p => p.id), preferences: { kind: 'missing', reason: 'Test' } });
      c.ordinaryVotes.push({ collegeId, electorate: { minimumAge: 18, electors: 200 }, votes: { kind: 'attributed', listVotes: [{ listId: 'A', votes: 100 }], provenance: supplied } });
      c.bonusSlates[0].candidates.push({ candidateId: people[0].id, affiliationListId: 'A' });
    }
    valid(f);
    addedPeople.slice(0, 7).forEach(p => p.gender = p.gender === 'male' ? 'female' : 'male');
    expect(issues(f).some(x => x.message.includes('60%'))).toBe(true);
  });
  it('allows Senate presentation in just one ordinary region without Camera coverage', () => {
    const f = singleFixture('senato');
    if (f.renewal.kind !== 'single') throw new Error('Expected single');
    const c = f.renewal.chamber;
    c.lists.push({ id: 'special', name: 'Special only', coalitionId: null, protectedMinority: null });
    for (const s of c.specialCandidatures) { s.linkedListIds = ['special']; s.affiliationListId = 'special'; }
    for (const v of c.specialVotes) if (v.votes.kind === 'attributed') v.votes.listVotes[0].listId = 'special';
    valid(f);
  });
  it('applies Camera half-constituency coverage, and does not impose it on special-only lists', () => {
    const f = singleFixture('camera');
    if (f.renewal.kind !== 'single') throw new Error('Expected single');
    const c = f.renewal.chamber;
    c.lists.push({ id: 'special', name: 'Special only', coalitionId: null, protectedMinority: null });
    c.ordinarySlates.find(s => s.collegeId === 'TP')!.listId = 'special';
    const votes = c.ordinaryVotes.find(v => v.collegeId === 'TP')!.votes;
    if (votes.kind === 'attributed') votes.listVotes[0].listId = 'special';
    for (const s of c.specialCandidatures) { s.linkedListIds = ['special']; s.affiliationListId = 'special'; }
    for (const v of c.specialVotes) if (v.votes.kind === 'attributed') v.votes.listVotes[0].listId = 'special';
    expect(issues(f).some(x => x.message.includes('half-constituency'))).toBe(true);
    c.lists[0].protectedMinority = { regionId: 'O', legalReference: 'test exception' };
    c.regions[0].autonomous = true;
    // Make the ordinary minority list exclusive to O; the special list remains
    // outside the nationwide requirement, retaining its TAA ordinary slate.
    valid(f);
  });
  it('counts declared special-contest coverage alongside ordinary Camera slates', () => {
    const f = singleFixture('camera');
    if (f.renewal.kind !== 'single') throw new Error('Expected single');
    const c = f.renewal.chamber;
    c.lists.push({ id: 'TAA', name: 'TAA proportional only', coalitionId: null, protectedMinority: null });
    c.ordinarySlates.find(s => s.collegeId === 'TP')!.listId = 'TAA';
    const votes = c.ordinaryVotes.find(v => v.collegeId === 'TP')!.votes;
    if (votes.kind === 'attributed') votes.listVotes[0].listId = 'TAA';
    valid(f);
  });
  it('requires two-thirds local college coverage in both chambers', () => {
    const f = singleFixture('senato');
    if (f.renewal.kind !== 'single') throw new Error('Expected single');
    f.renewal.chamber.ordinarySlates.splice(0, 2);
    expect(issues(f).some(x => x.message.includes('two-thirds'))).toBe(true);
  });
  it('rejects bonus slates in a single renewal and missing slates in a joint renewal', () => {
    const f = singleFixture('camera');
    if (f.renewal.kind !== 'single') throw new Error('Expected single');
    f.renewal.chamber.bonusSlates = camera(jointFixture()).bonusSlates;
    expect(issues(f).some(x => x.code === 'INCOMPATIBLE_RENEWAL')).toBe(true);
    const g = jointFixture(); camera(g).bonusSlates = [];
    expect(issues(g).some(x => x.message.includes('Missing required'))).toBe(true);
  });
});

describe('votes and preference validity', () => {
  it('accepts raw direct/bonus-only components without performing attribution', () => {
    const f = jointFixture();
    camera(f).ordinaryVotes[0].votes = { kind: 'components', directListVotes: [{ listId: 'A', votes: 100 }], bonusOnlyVotes: [{ subject: { kind: 'list', id: 'A' }, votes: 7 }], provenance: supplied };
    expect(valid(f)).toEqual(f);
  });
  it('rejects already attributed totals carrying raw components (double attribution)', () => {
    const f = jointFixture();
    const row = camera(f).ordinaryVotes[0];
    Object.assign(row.votes, { bonusOnlyVotes: [{ subject: { kind: 'list', id: 'A' }, votes: 7 }] });
    expect(issues(f).some(x => x.code === 'INVALID_SHAPE')).toBe(true);
  });
  it('accepts zero-vote elections for explicit non-computable stages downstream', () => {
    const f = jointFixture();
    if (f.renewal.kind !== 'joint') throw new Error('Expected joint');
    for (const c of [f.renewal.camera, f.renewal.senato]) {
      for (const row of c.ordinaryVotes) if (row.votes.kind === 'attributed') row.votes.listVotes.forEach(x => x.votes = 0);
      for (const row of c.specialVotes) if (row.votes.kind === 'attributed') {
        row.votes.listVotes.forEach(x => x.votes = 0); row.votes.candidateTotals.forEach(x => x.votes = 0);
      }
    }
    valid(f);
  });
  it('requires complete college/list counts instead of treating missing as zero', () => {
    const f = jointFixture(); const votes = camera(f).ordinaryVotes[0].votes;
    if (votes.kind === 'attributed') votes.listVotes = [];
    expect(issues(f).some(x => x.message.includes('Missing vote count'))).toBe(true);
    camera(f).ordinaryVotes.pop();
    expect(issues(f).some(x => x.message.includes('Missing votes for college'))).toBe(true);
  });
  it('keeps candidate-only special ballots separate and counts each valid ballot once', () => {
    const f = jointFixture(); const row = camera(f).specialVotes[0];
    const candidateId = camera(f).specialCandidatures[0].candidateId;
    row.electorate.electors = 110;
    row.votes = { kind: 'components', directListVotes: [{ listId: 'A', votes: 100 }],
      candidateOnlyVotes: [{ candidateId, votes: 10 }], candidateTotals: [{ candidateId, votes: 110 }], provenance: supplied };
    valid(f);
    row.votes.candidateTotals[0].votes = 100;
    expect(issues(f).some(x => x.message.includes('Candidate totals disagree'))).toBe(true);
  });
  it('checks electorate ceilings and safe aggregate arithmetic', () => {
    const f = jointFixture(); camera(f).ordinaryVotes[0].electorate.electors = 99;
    expect(issues(f).some(x => x.message.includes('electorate'))).toBe(true);
    const g = jointFixture(); camera(g).ordinaryVotes[0].votes = { kind: 'components', directListVotes: [{ listId: 'A', votes: Number.MAX_SAFE_INTEGER }], bonusOnlyVotes: [{ subject: { kind: 'list', id: 'A' }, votes: 1 }], provenance: supplied };
    expect(issues(g).some(x => x.message.includes('safe integer'))).toBe(true);
  });
  function preferences(f: Fixture) {
    const slate = camera(f).ordinarySlates[0];
    const [a, b, c] = slate.candidateIds.slice(1);
    slate.preferences = {
      kind: 'counts', provenance: { kind: 'generated', modelId: 'synthetic-preferences-v1', parameterVersion: 'test', profileId: 'central', sourceVersions: ['test'], seed: 'p', assumptions: ['constructed'] },
      counts: slate.candidateIds.slice(1).map(candidateId => ({ candidateId, count: candidateId === a ? 40 : candidateId === b || candidateId === c ? 20 : 0 })),
      validity: { kind: 'combinations', ballots: [{ candidateIds: [a, b, c], count: 20 }, { candidateIds: [a], count: 20 }, { candidateIds: [], count: 60 }] }
    };
    return slate.preferences;
  }
  it('accepts realizable synthetic preference margins and an explicit zero profile', () => {
    const f = jointFixture(); const prefs = preferences(f); valid(f);
    prefs.counts.forEach(x => x.count = 0);
    prefs.validity = { kind: 'combinations', ballots: [{ candidateIds: [], count: 100 }] };
    valid(f);
  });
  it('keeps missing preferences distinct from explicit zero counts', () => {
    const f = jointFixture(); valid(f);
    const prefs = preferences(f); prefs.counts = [];
    expect(issues(f).some(x => x.message.includes('unknown is not zero'))).toBe(true);
  });
  it('accepts adjudicated supplied counts with provenance and applies aggregate bounds', () => {
    const f = jointFixture(); const prefs = preferences(f);
    prefs.validity = { kind: 'adjudicated', reference: 'official valid preference counts' };
    prefs.provenance = supplied; valid(f);
    prefs.counts[0].count = 101;
    expect(issues(f).some(x => x.message.includes('ballot budget'))).toBe(true);
  });
  it('rejects capolista preferences and unrealizable certificates despite plausible totals', () => {
    const f = jointFixture(); const prefs = preferences(f);
    prefs.counts[0].candidateId = camera(f).ordinarySlates[0].candidateIds[0];
    expect(issues(f).some(x => x.message.includes('non-capolista'))).toBe(true);
    const g = jointFixture(); const p = preferences(g);
    if (p.validity.kind !== 'combinations') throw new Error('Expected combinations');
    p.validity.ballots[0].candidateIds = [camera(g).ordinarySlates[0].candidateIds[1], camera(g).ordinarySlates[0].candidateIds[3]];
    expect(issues(g).some(x => x.message.includes('both genders'))).toBe(true);
    expect(issues(g).some(x => x.message.includes('margins disagree'))).toBe(true);
  });
  it('does not use bonus-only ballots as raw direct-list preference capacity', () => {
    const f = jointFixture(); preferences(f);
    camera(f).ordinaryVotes[0].votes = { kind: 'components', directListVotes: [{ listId: 'A', votes: 30 }], bonusOnlyVotes: [{ subject: { kind: 'list', id: 'A' }, votes: 70 }], provenance: supplied };
    expect(issues(f).some(x => x.code === 'INVALID_PREFERENCES')).toBe(true);
  });
  it('requires the approved combination certificate for generated preferences', () => {
    const f = jointFixture(); const prefs = preferences(f);
    prefs.validity = { kind: 'adjudicated', reference: 'synthetic marginal bounds alone' };
    expect(issues(f).some(x => x.message.includes('realizable combination certificate'))).toBe(true);
  });
  it('rejects repeated candidates or more than three marks in a combination', () => {
    const f = jointFixture(); const prefs = preferences(f);
    if (prefs.validity.kind !== 'combinations') throw new Error('Expected combinations');
    const ids = camera(f).ordinarySlates[0].candidateIds;
    prefs.validity.ballots[0].candidateIds = [ids[1], ids[2], ids[3], ids[1]];
    const errors = issues(f);
    expect(errors.some(x => x.code === 'DUPLICATE_ID')).toBe(true);
    expect(errors.some(x => x.message.includes('Invalid candidate subset'))).toBe(true);
  });
});

describe('seat table and model provenance', () => {
  it('rejects domestic totals that include the separate foreign seats', () => {
    const f = jointFixture(); camera(f).colleges[0].seatsWithoutBonus += 8;
    expect(issues(f).some(x => x.code === 'INVALID_ENDOWMENT')).toBe(true);
  });
  it('requires consistent census, geography and method versions for I08 tables', () => {
    const f = jointFixture();
    camera(f).seatTable = { kind: 'provisional-census', reference: 'legal census', version: 'P0-test', censusDate: '2021-12-31', geographyVersion: f.geographyVersion, method: 'I08-v1' };
    valid(f);
    const c = camera(f);
    if (c.seatTable.kind !== 'provisional-census') throw new Error('Expected census table');
    c.seatTable.geographyVersion = 'wrong';
    expect(issues(f).some(x => x.code === 'INVALID_METADATA')).toBe(true);
  });
  it('reserves common anchors, explicit chamber precedence, scopes and mean-mode covariance metadata', () => {
    const f = jointFixture();
    f.chamberModel = { kind: 'generated', modelId: 'paired-chamber-votes-v1', parameterVersion: 'test-1', covarianceProfileVersion: 'test-profile', sourceVersions: ['2022-review'], seed: 'paired', assumptions: ['unfitted fixture'], mode: 'mean', references: [
      { scope: { kind: 'national' }, listIds: { camera: 'A', senato: 'A' }, common: { share: .4, referenceDate: '2026-10-17' }, camera: { share: .39, referenceDate: '2026-10-17', origin: 'model-derived' }, senato: { share: .41, referenceDate: '2026-10-17', origin: 'model-derived' } },
      { scope: { kind: 'municipality', code: '001001' }, listIds: { camera: 'A', senato: 'A' }, common: { share: .5, referenceDate: '2026-10-17' }, camera: { share: .51, referenceDate: '2026-10-17', origin: 'supplied' }, senato: { share: .49, referenceDate: '2026-10-17', origin: 'model-derived' } }
    ] };
    valid(f);
    f.chamberModel.references[0].listIds.camera = 'missing';
    expect(issues(f).some(x => x.code === 'UNKNOWN_REFERENCE')).toBe(true);
  });
  it.each(['common-only', 'one-explicit', 'both-explicit', 'without-common'] as const)('accepts resolved metadata for %s', variant => {
    const f = jointFixture();
    f.chamberModel = { kind: 'generated', modelId: 'paired-chamber-votes-v1', parameterVersion: 'test', covarianceProfileVersion: 'test', sourceVersions: ['constructed'], seed: 'p', assumptions: ['unfitted fixture'], mode: 'mean', references: [{
      scope: { kind: 'national' }, listIds: { camera: 'A', senato: 'A' },
      common: variant === 'without-common' ? null : { share: .4, referenceDate: '2026-10-17' },
      camera: { share: .39, referenceDate: '2026-10-17', origin: variant === 'common-only' ? 'model-derived' : 'supplied' },
      senato: { share: .41, referenceDate: '2026-10-17', origin: variant === 'both-explicit' ? 'supplied' : 'model-derived' }
    }] };
    valid(f);
    f.chamberModel.references.push(structuredClone(f.chamberModel.references[0]));
    expect(issues(f).some(x => x.message.includes('Duplicate resolved reference'))).toBe(true);
  });
});
