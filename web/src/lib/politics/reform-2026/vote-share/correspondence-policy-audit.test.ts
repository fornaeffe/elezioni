/** Quantitative policy audit. These characterize current limitations, not desired fixes. */
import { describe, expect, it } from 'vitest';
import { generationFixture } from '../generation.fixture';
import { preparePairedVoteReferences } from '../paired-vote-generation';
import { aggregateReformCollegeVotes } from '../generation';
import { correspondenceRow } from './correspondences';
import { latestLegacyPoints, type LegacyPollObservation } from './legacy-polls';
import { removeAbsent } from './interventions';
import { projectMargins, ProjectionCapacityError } from './projection';

function fixture() {
  const { scenario: s, snapshot: d } = generationFixture();
  const ids = s.lists.camera.map(l => l.id);
  s.projectionPolicy = 'preserve-historical-turnout-v1';
  s.renewal = 'camera';
  s.shares = [];
  s.abstention = [];
  return { s, d, ids };
}

describe('Post-FV-08 correspondence-policy audit', () => {
  it('quantifies incomplete destination redistribution and preserves original archive policy', () => {
    const { s, d, ids } = fixture();
    // A=135k, B=165k, non-valid=200k. Only half of A survives.
    s.correspondences = [{ chamber: 'camera', pastListId: 'A', futureListId: ids[0], factor: 0.5 },
      { chamber: 'camera', pastListId: 'B', futureListId: ids[1], factor: 1 }];
    const u = preparePairedVoteReferences(s, d).units[0].camera;
    expect(u.abstention).toBeCloseTo(0.4);
    expect(u.shares.get(ids[0])).toBeCloseTo(9 / 31);
    expect(u.shares.get(ids[1])).toBeCloseTo(22 / 31);
    expect(300000 * u.shares.get(ids[1])! - 165000).toBeCloseTo(47903.225806);
    delete s.projectionPolicy;
    const old = preparePairedVoteReferences(s, d).units[0].camera;
    expect(old.abstention).toBeCloseTo(0.4);
    expect(old.shares.get(ids[0])).toBeCloseTo(0.45); // old per-source row renormalization
    s.correspondences = s.correspondences.filter(c => c.pastListId === 'A');
    expect(preparePairedVoteReferences(s, d).units[0].camera.abstention).toBeCloseTo(0.73);
  });

  it('distinguishes selected full-simplex conditioning from legacy turnout preservation', () => {
    expect([...removeAbsent([0.4, 0.27, 0.33], [true, false, true])])
      .toEqual([0.4 / 0.73, 0, 0.33 / 0.73]);
    const { s, d, ids } = fixture();
    s.coverage.filter(c => c.chamber === 'camera').forEach(c => { c.listIds = [ids[1]]; });
    const u = preparePairedVoteReferences(s, d).units[0].camera;
    expect(u.abstention).toBeCloseTo(0.4);
    expect(u.shares.get(ids[1])).toBe(1);
  });

  it('exposes regional and equal fallbacks and the discontinuity at zero retained support', () => {
    const { s, d, ids } = fixture();
    s.correspondences = [];
    const equal = preparePairedVoteReferences(s, d);
    expect(equal.units[0].camera.shares.get(ids[0])).toBe(0.5);
    expect(equal.trace.warnings.some(w => w.code === 'LEGACY_UNMAPPED_POLITICAL_PROXY')).toBe(true);
    // One additional same-region donor municipality; target has only unmapped B.
    const donor = structuredClone(d.municipalities[0]);
    donor.code = 'audit-donor';
    d.municipalities.push(donor);
    d.memberships.push(...d.memberships.filter(m => m.municipalityCode === d.municipalities[0].code)
      .map(m => ({ ...m, municipalityCode: donor.code })));
    d.municipalities[0].camera.votes = [{ listId: 'B', votes: 300000 }];
    s.correspondences = [{ chamber: 'camera', pastListId: 'A', futureListId: ids[0], factor: 1 }];
    const regional = preparePairedVoteReferences(s, d);
    expect(regional.units[0].camera.shares.get(ids[0])).toBe(1);
    expect(regional.trace.warnings.some(w => w.message.includes('regionale'))).toBe(true);
    // A tiny B -> second-list mapping suppresses the regional proxy completely.
    s.correspondences.push({ chamber: 'camera', pastListId: 'B', futureListId: ids[1], factor: 1e-9 });
    const tiny = preparePairedVoteReferences(s, d);
    expect(tiny.units[0].camera.shares.get(ids[1])).toBe(1);
    expect(tiny.units[0].camera.abstention).toBeCloseTo(0.4);
  });

  it('quantifies poll group renormalization and duplicated national targets on disjoint tickets', () => {
    const poll: LegacyPollObservation = { id: 'audit', date: '2026-10-01', end: '2026-10-01',
      available: '2026-10-02', context: 'camera', enabled: true, sample: 1000, source: 'constructed',
      groups: [{ label: 'AB', members: ['a', 'b'], share: 0.2 }] };
    const onlyA = latestLegacyPoints([poll], '2026-10-03', [{ id: 'a', componentIds: ['a'] }],
      { 'poll:audit:AB': { fractions: { a: 60, b: 40 }, provenance: 'audit' } }, id => id);
    expect(onlyA.points.get('a')!.share).toBeCloseTo(0.2); // not 0.12
    poll.groups = [{ label: 'A', members: ['a'], share: 0.2 }];
    const tickets = [{ id: 'north', componentIds: ['a'] }, { id: 'south', componentIds: ['a'] }];
    expect(correspondenceRow(['a'], undefined, tickets).map(r => r.factor)).toEqual([1, 1]);
    const points = latestLegacyPoints([poll], '2026-10-03', tickets, {}, id => id).points;
    expect(points.get('north')!.share).toBeCloseTo(0.1);
    expect(points.get('south')!.share).toBeCloseTo(0.1);
    // On 80/20 equal-turnout geography, a local 20% gives national 16%/4%.
    expect(0.1 / 0.8).toBeCloseTo(0.125);
    expect(0.1 / 0.2).toBeCloseTo(0.5);
  });

  it('leaves a new list at zero until a reference supplies support, and rejects no filed choices', () => {
    const { s, d, ids } = fixture();
    s.correspondences = s.correspondences.filter(c => c.futureListId === ids[0]);
    const unsupported = preparePairedVoteReferences(s, d).units[0].camera;
    expect(unsupported.shares.get(ids[1])).toBe(0);
    expect(unsupported.shares.get(ids[0])).toBe(1);
    s.shares = [{ scope: { kind: 'national' }, listId: ids[1], common: null, camera: 0.2, senato: null }];
    expect(preparePairedVoteReferences(s, d).units[0].camera.shares.get(ids[1])).toBeCloseTo(0.2);
    s.shares = [];
    s.coverage.filter(c => c.chamber === 'camera').forEach(c => { c.listIds = []; });
    expect(() => preparePairedVoteReferences(s, d)).toThrow(/No filed political choice/);
  });

  it('accepts municipal-union targets that cannot fit physical submunicipal filing', () => {
    const { s, d, ids } = fixture();
    const municipality = structuredClone(d.municipalities[0]);
    municipality.camera = { electors: 100, votes: [{ listId: 'A', votes: 60 }, { listId: 'B', votes: 20 }] };
    d.municipalities = [municipality];
    const colleges = d.seats.geography.colleges.filter(c => c.chamber === 'camera' && c.kind === 'ordinary').slice(0, 2);
    d.memberships = colleges.map((c, i) => ({ chamber: 'camera', municipalityCode: municipality.code,
      collegeId: c.id, population: i ? 80 : 20 }));
    s.coverage = colleges.map((c, i) => ({ chamber: 'camera', collegeId: c.id, listIds: i ? [ids[1]] : ids }));
    s.pollReferences = { version: 'audit', archiveVersion: 'audit', policyVersion: 'poll-reference-v1',
      date: '2026-10-01', sourceQuestionIds: [], assumptions: ['constructed'],
      rows: ids.map(id => ({ chamber: 'camera', listId: id, share: 0.5,
        sigma: 0, donorHistoricalListIds: [] })) };
    const p = preparePairedVoteReferences(s, d).units[0].camera;
    expect(p.shares.get(ids[0])).toBeCloseTo(0.5); // 40 votes, but only 20 electors are eligible
    expect(() => aggregateReformCollegeVotes(s, d, [{ chamber: 'camera', municipalityCode: municipality.code,
      electors: 100, abstention: 20, votes: ids.map(listId => ({ listId, votes: 40 })) }], 0))
      .toThrow(/filing\/capacity/);
    expect(() => projectMargins([0.2, 0.4, 0.4], 2, i => ({ weight: i ? 80 : 20,
      scores: i ? [0, -Infinity, 0] : [0, 0, 0] }))).toThrow(ProjectionCapacityError);
  });

  it('reports generic legacy nonconvergence for jointly impossible overlapping targets', () => {
    const { s, d, ids } = fixture();
    const third = 'audit-free';
    s.lists.camera.push({ ...s.lists.camera[0], id: third, name: 'Audit free list' });
    const first = structuredClone(d.municipalities[0]);
    const second = structuredClone(first);
    first.camera = { electors: 200, votes: [{ listId: 'A', votes: 80 }, { listId: 'B', votes: 80 }] };
    second.code = 'audit-second';
    second.camera = { electors: 800, votes: [{ listId: 'A', votes: 320 }, { listId: 'B', votes: 320 }] };
    d.municipalities = [first, second];
    const colleges = d.seats.geography.colleges.filter(c => c.chamber === 'camera' && c.kind === 'ordinary').slice(0, 2);
    d.memberships = colleges.map((c, i) => ({ chamber: 'camera', municipalityCode: i ? second.code : first.code,
      collegeId: c.id, population: i ? 800 : 200 }));
    s.coverage = colleges.map((c, i) => ({ chamber: 'camera', collegeId: c.id, listIds: i ? [third] : [...ids, third] }));
    s.pollReferences = { version: 'audit', archiveVersion: 'audit', policyVersion: 'poll-reference-v1',
      date: '2026-10-01', sourceQuestionIds: [], assumptions: ['constructed'],
      rows: [...ids, third].map((listId, i) => ({ chamber: 'camera', listId, share: i < 2 ? 0.15 : 0.7,
        sigma: 0, donorHistoricalListIds: [] })) };
    expect(() => preparePairedVoteReferences(s, d)).toThrow(/did not converge/);
  });

  it('exposes greedy allocation failure even when joint filing is feasible', () => {
    const { s, d, ids } = fixture();
    const code = d.municipalities[0].code;
    const colleges = d.seats.geography.colleges.filter(c => c.chamber === 'camera' && c.kind === 'ordinary').slice(0, 2);
    d.memberships = colleges.map(c => ({ chamber: 'camera', municipalityCode: code, collegeId: c.id, population: 50 }));
    s.coverage = colleges.map((c, i) => ({ chamber: 'camera', collegeId: c.id, listIds: i ? [ids[0]] : ids }));
    const row = { chamber: 'camera' as const, municipalityCode: code, electors: 100, abstention: 0,
      votes: [{ listId: ids[0], votes: 50 }, { listId: ids[1], votes: 50 }] };
    expect(() => aggregateReformCollegeVotes(s, d, [row], 0)).toThrow(/filing\/capacity/);
    row.votes.reverse(); // restricted list first allows the feasible 50/50 allocation
    const allocated = aggregateReformCollegeVotes(s, d, [row], 0);
    expect(allocated.get(colleges[0].id)!.votes.get(ids[1])).toBe(50);
    expect(allocated.get(colleges[1].id)!.votes.get(ids[0])).toBe(50);
  });
});
