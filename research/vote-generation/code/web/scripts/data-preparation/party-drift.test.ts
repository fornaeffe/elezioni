import { describe, expect, it } from 'vitest';
import { basis, mv, tr, dot, zeros } from './drift-math';
import { PARTY_DRIFT_SCHEMA, otherMembership, partyFitFingerprint, validatePartyContract, type PartyResearchContract } from './party-drift-contract';
import { aggregatePoliticalShares, groupedCountDerivatives, groupedCountLogLikelihood, groupedProbabilities, pollContrastFactor, referenceGroupShift } from './party-drift-likelihood';
import { calibrateReferenceOffset, evaluateAffine, makeReferenceWalk, priorProbabilityDraw } from './party-drift-prior';
import { fitReducedParty, reducedDerivatives, reducedChainDiagnostics } from './party-drift-fit';
import { createNormalSampler, createSeededRng } from '../../src/lib/core/rng';
import { groupedReferenceQuadrature } from './party-drift-quadrature';
import { componentResolution } from './party-drift-identifiability';
import { calibrateSoftReference, logisticNormalMoments, softReferenceLogDensity, eliminateHardReference, referenceValue } from './party-drift-reference';
import { membershipFor, buildMembershipTable, type MembershipPolicy } from './party-drift-memberships';
import { coarsenGroups, makeAdmissiblePanel } from './party-drift-panel';
import { reviewedEarlyShares } from './party-drift-panel-data';

function contract(): PartyResearchContract {
  return { schema: PARTY_DRIFT_SCHEMA, referenceDate: '2010-01-01', cutoff: '2010-01-01',
    components: ['a', 'b', 'c'].map(id => ({ id, label: id, supportStart: '2008-01-01', origin: 'historical' })),
    ballots: [{ id: 'ballot', date: '2009-01-01', kind: 'camera', geography: 'national', available: ['a', 'b', 'c'], groups: [{ id: 'joint', members: ['a', 'b'] }, { id: 'c', members: ['c'] }] }],
    counts: [{ sourceId: 'source/row', ballotId: 'ballot', municipalityId: 'city', electors: 100, nonValid: 30, counts: [{ groupId: 'joint', count: 60 }, { groupId: 'c', count: 10 }] }],
    sourceVersions: ['pin'], referenceInformation: ['a','b'].map(id => ({ id: 'ref-'+id, date: '2010-01-01', availableOn: '2010-01-01', scope: 'national', members: [id], meanValidFraction: .2, sdPercentagePoints: 1 })),
    priorPolicy: { version: 'reference-test' }, fittingPolicy: { version: 'test' } };
}
describe('party-level research contract and original grouped likelihood', () => {
  it('replaces a coarse early view with the reviewed named answers from the same survey', () => {
    const original={id:'survey',shares:[.2,.16,.25,.39]},refined={id:'survey',shares:[.2,.16,.25,.15,.07,.17]};
    const projected=reviewedEarlyShares(original,refined);
    expect(projected.groups).toEqual([['pd'],['m5s'],['lega'],['fdi'],['fi-pdl']]);
    expect(projected.shares).toEqual([.2,.16,.25,.15,.07]);expect(projected.other).toBe(.17);
    expect(reviewedEarlyShares(original).groups).toHaveLength(3);
    expect(()=>reviewedEarlyShares(original,{...refined,shares:[.21,.15,.25,.15,.07,.17]})).toThrow(/Inconsistent/);
  });
  it('pools unsupported future distinctions without treating future memberships as evidence', () => {
    const panel=makeAdmissiblePanel(['a','b','new','minor'],[{id:'past',groups:[['a','b'],['minor']]}],new Set(['a','b','minor']),['a','b'],[['a'],['b'],['new']]);
    expect(panel.audit?.resolved).toBe(true);
    expect(panel.mapping.a).toBe(panel.mapping.b);
    expect(panel.mapping.new).toBe('research-other');
    expect(panel.mapping.minor).toBe('research-other');
  });
  it('retains a joint-list contrast resolved collectively by cutoff observations', () => {
    const panel=makeAdmissiblePanel(['a','b','c'],[{id:'joint',groups:[['a','b'],['c']]},{id:'separate',groups:[['a'],['b'],['c']]}],new Set(['a','b','c']),['a']);
    expect(panel.mapping.a).not.toBe(panel.mapping.b);
    expect(panel.mapping.c).toBe('research-other');
  });
  it('rejects a future-only residual when the whole historical catalog leaves it unobserved', () => {
    expect(()=>makeAdmissiblePanel(['a','b','new'],[{id:'past',groups:[['a'],['b']]}],
      new Set(['a','b']),['a','b'],[['a'],['b'],['new']])).toThrow(/Unsupported research residual/);
  });
  it('combines overlapping research outcomes once and retains original source indices', () => {
    const result=coarsenGroups([['a'],['minor'],['unnamed']], [40,10,50], id=>id==='a'?id:'research-other');
    expect(result).toEqual([{members:['a'],value:40,originalIndices:[0]},{members:['research-other'],value:60,originalIndices:[1,2]}]);
    const crossing=coarsenGroups([['a','minor'],['b'],['unnamed']],[20,30,50],id=>id==='minor'||id==='unnamed'?'residual':id);
    expect(crossing.reduce((s,g)=>s+g.value,0)).toBe(100);
    expect(crossing.find(g=>g.members.includes('residual'))).toEqual({members:['a','residual'],value:70,originalIndices:[0,2]});
  });
  it('preserves one source count and rejects overlapping, fractional or duplicated observations', () => {
    expect(() => validatePartyContract(contract())).not.toThrow();
    const overlap = contract(); overlap.ballots[0].groups[1].members.push('a');
    expect(() => validatePartyContract(overlap)).toThrow(/membership/);
    const pseudo = contract(); pseudo.counts[0].counts[0].count = 59.5;
    expect(() => validatePartyContract(pseudo)).toThrow(/integers/);
    const duplicate = contract(); duplicate.counts.push(structuredClone(duplicate.counts[0]));
    expect(() => validatePartyContract(duplicate)).toThrow(/source observation/);
    const otherSource = contract(); otherSource.counts.push({ ...structuredClone(otherSource.counts[0]), sourceId: 'duplicate-cell' });
    expect(() => validatePartyContract(otherSource)).toThrow(/municipality\/ballot/);
  });
  it('rejects pre-birth observations and leakage', () => {
    const c = contract(); c.components[0].supportStart = '2009-06-01';
    expect(() => validatePartyContract(c)).toThrow(/pre-birth/);
    const future = contract(); future.ballots[0].date = '2011-01-01';
    expect(() => validatePartyContract(future)).toThrow(/cutoff/);
    const newParty = contract(); newParty.components[0].origin = 'new';
    expect(() => validatePartyContract(newParty)).toThrow(/reference constraints/);
  });
  it('invalidates reference-date and semantic membership edits while retaining presentation renames', () => {
    const first = contract(), rename = contract(); rename.components[0].label = 'renamed';
    expect(partyFitFingerprint(first)).toBe(partyFitFingerprint(rename));
    const date = contract(); date.referenceDate = '2009-12-31';
    expect(partyFitFingerprint(first)).not.toBe(partyFitFingerprint(date));
    const membership = contract(); membership.ballots[0].groups[0].members = ['a']; membership.ballots[0].groups[1].members.push('b');
    expect(partyFitFingerprint(first)).not.toBe(partyFitFingerprint(membership));
  });
  it('constructs Other as the political complement and detects unresolved support', () => {
    expect(otherMembership(['a', 'b', 'c'], [['a', 'b']], .1)).toEqual(['c']);
    expect(() => otherMembership(['a'], [['a']], .01)).toThrow(/empty political complement/);
    expect(() => otherMembership(['a'], [['a', 'b']], 0)).toThrow(/pre-birth/);
  });
  it('requires independent distinguishing information and fingerprints its numerical units', () => {
    const unresolved = contract(); unresolved.referenceInformation = [];
    expect(() => validatePartyContract(unresolved)).toThrow(/Unresolved political/);
    expect(() => validatePartyContract(unresolved, { allowUnresolvedNegativeControl: true })).not.toThrow();
    unresolved.referenceInformation = [{ id: 'total', date: '2010-01-01', availableOn: '2010-01-01', scope: 'national', members: ['a','b'], meanValidFraction: .4, sdPercentagePoints: 5 }];
    expect(() => validatePartyContract(unresolved)).toThrow(/Unresolved political/);
    const shifted = contract(); shifted.referenceInformation![0].meanValidFraction = .25;
    expect(partyFitFingerprint(contract())).not.toEqual(partyFitFingerprint(shifted));
    shifted.referenceInformation![0].availableOn = '2011-01-01';
    expect(() => validatePartyContract(shifted)).toThrow(/unavailable at cutoff/);
    expect(componentResolution(['a','b','c'], [{ id:'overlap',groups:[['a','b'],['b','c'],['a','c']] }]).resolved).toBe(true);
  });
  it('retains original grouped derivatives, including negative observed curvature', () => {
    const scores = [1.5, -.3, .2, .1], observation = { groups: [[0], [1, 2], [3]], counts: [30, 40, 30], electors: 100 };
    const der = groupedCountDerivatives(scores, observation), step = 1e-5;
    for (let r = 0; r < 4; r++) {
      const plus = [...scores], minus = [...scores]; plus[r] += step; minus[r] -= step;
      const p = groupedCountDerivatives(plus, observation), m = groupedCountDerivatives(minus, observation);
      expect(der.gradient[r]).toBeCloseTo((p.logLikelihood - m.logLikelihood) / (2 * step), 6);
      for (let s = 0; s < 4; s++) expect(der.precision[s * 4 + r]).toBeCloseTo(-(p.gradient[s] - m.gradient[s]) / (2 * step), 6);
    }
    const split = groupedCountDerivatives([1.5, 0, 0, 0], { ...observation, counts: [15, 80, 5] });
    const v = [0, 1, -1, 0];
    expect(dot(v, mv(split.precision, v, 4))).toBeLessThan(0);
  });
  it('keeps observed zeros finite and grouping invariant for non-valid', () => {
    const scores = [.3, -.2, .1, 0], separate = groupedProbabilities(scores, [[0], [1], [2], [3]]), grouped = groupedProbabilities(scores, [[0], [1, 2], [3]]);
    expect(grouped[0]).toBeCloseTo(separate[0], 14);
    expect(grouped[1]).toBeCloseTo(separate[1] + separate[2], 14);
    const d = groupedCountDerivatives(scores, { groups: [[0], [1, 2], [3]], counts: [30, 0, 70], electors: 100 }, true);
    expect(Number.isFinite(d.logLikelihood)).toBe(true);
    expect(Array.from(d.precision).every(Number.isFinite)).toBe(true);
    expect(Number.isFinite(groupedCountLogLikelihood([1000, -1000, -900, 0], { groups: [[0], [1, 2], [3]], counts: [30, 0, 70], electors: 100 }))).toBe(true);
  });
  it('redistributes unavailable support proportionally including non-valid', () => {
    const scores = [.3, .2, .5].map(Math.log), p = groupedProbabilities(scores, [[0], [2]]);
    expect(p[0]).toBeCloseTo(.375, 14); expect(p[1]).toBeCloseTo(.625, 14);
  });
  it('uses expected valid-vote weights before shared poll tilting', () => {
    const cells = [{ scores: [.8, .18, .02].map(Math.log), electors: 100, groups: [[0], [1], [2]] },
      { scores: [.2, .08, .72].map(Math.log), electors: 100, groups: [[0], [1], [2]] }];
    expect(aggregatePoliticalShares(cells)[0]).toBeCloseTo(.26, 12);
    const tilted = aggregatePoliticalShares(cells, [0, Math.log(2), 0]);
    expect(tilted[0]).toBeCloseTo(.2 * (.36 / .38) + .8 * (.16 / .88), 12);
    const shifted = referenceGroupShift(cells, 0, .4);
    expect(aggregatePoliticalShares(shifted.cells)[0]).toBeCloseTo(.4, 12);
    expect(() => referenceGroupShift(cells, 0, 0)).toThrow(/interior/);
  });
  it('treats sample size as measurement uncertainty and rejects unimplemented printed-zero factors', () => {
    const a = pollContrastFactor([.4, .6], [.42, .58], 200), b = pollContrastFactor([.4, .6], [.42, .58], 400);
    expect(a.covariance[0]).toBeGreaterThan(b.covariance[0]);
    expect(() => pollContrastFactor([.4, .6], [0, 1], 200)).toThrow(/printed zero/);
  });
});
describe('restricted reference information and minimal memberships', () => {
  it('matches entered share-scale mean and uncertainty without pretending percentage points are logit SD', () => {
    const factor = calibrateSoftReference(.2, 15), checked = logisticNormalMoments(factor.location, factor.scale);
    expect(checked.mean).toBeCloseTo(.2, 10); expect(checked.variance).toBeCloseTo(.15 ** 2, 10);
    expect(factor.scale).not.toBeCloseTo(.15, 2);
    expect(Number.isFinite(softReferenceLogDensity(.2, factor))).toBe(true);
    expect(() => calibrateSoftReference(.2, 45)).toThrow(/Infeasible/);
  });
  it('conditions an exact aggregate constraint in the density and keeps local probabilities free', () => {
    const cities = ['a','b'], walk = makeReferenceWalk({ j:4,cities,dates:['2010-01-01'],referenceDate:'2010-01-01',offset:1.7,nationalSd:1.5,localSd:.5,nationalInnovationSd:.35,localInnovationSd:.25 });
    const reference = cities.map(c => ({ state:walk.state(c,'2010-01-01'),groups:[[0],[1],[2],[3]],electors:100 }));
    const original = { dimension:walk.dimension,factors:reference.map(c=>({state:c.state,observation:{groups:[[0],[1,2],[3]],counts:[30,40,30],electors:100}})) };
    const h=basis(4), direction=zeros(walk.dimension,1); for(let k=0;k<3;k++)direction[k]=h[3+k]/1.5;
    const constrained=eliminateHardReference(original,reference,1,.2,direction);
    const normal=createNormalSampler(createSeededRng('hard-constraint-density'));
    for(let i=0;i<5;i++){
      const z=Float64Array.from({length:constrained.problem.dimension},()=>normal(0,.5)), decoded=constrained.decode(z);
      expect(referenceValue(reference,decoded.x,1)).toBeCloseTo(.2,10);
      expect(Number.isFinite(constrained.problem.extraLogLikelihood!(z))).toBe(true);
      const local=reference.map(c=>aggregatePoliticalShares([{...c,scores:evaluateAffine(c.state,decoded.x)}])[0]);
      expect(Math.abs(local[0]-local[1])).toBeGreaterThan(1e-6);
      expect(decoded.derivative).toBeGreaterThan(0);
    }
  });
  it('preserves atomic source identities and applies dated joint overrides without factor allocations', () => {
    const policy:MembershipPolicy={schemaVersion:1,version:'test',status:'proposal',policy:'test',aliases:[{id:'a',labels:['A'],sources:['source']}],joints:[{label:'AB',members:['a','b'],sources:['source']}],eventOverrides:[{event:'new',label:'A',members:['a','b'],sources:['source']}],reviewWarnings:[]};
    expect(membershipFor(policy,'old','A').members).toEqual(['a']); expect(membershipFor(policy,'new','A').members).toEqual(['a','b']);
    expect(membershipFor(policy,'old','AB').members).toEqual(['a','b']);
    expect(membershipFor(policy,'old','unmapped').members[0]).toMatch(/^atomic-/);
    expect(membershipFor(policy,'old','unmapped').members).not.toEqual(membershipFor(policy,'old','different').members);
  });
});
describe('proper neutral reference priors and backward construction', () => {
  it('calibrates expected participation and exchangeable political shares on independent draws', () => {
    const context = { components: 3, cells: [{ electors: 100, available: [1, 2, 3] }, { electors: 200, available: [1, 2, 3], kind: 'european' as const }], nationalSd: 1.5, localSd: .5, kindSd: .5 };
    const calibrated = calibrateReferenceOffset(context, 16384), verification = priorProbabilityDraw(context, calibrated.offset, 'independent-prior-verification', 32768);
    expect(calibrated.offset).toBeGreaterThan(Math.log(3));
    expect(verification.expectedNonValid).toBeCloseTo(.5, 2);
    for (const p of verification.electorShares.slice(1)) expect(Math.abs(p - 1 / 6)).toBeLessThan(.005);
    const deterministic = calibrateReferenceOffset({ ...context, nationalSd: 0, localSd: 0, kindSd: 0 }, 16);
    expect(deterministic.offset).toBeCloseTo(Math.log(3), 12);
  });
  const settings = { j: 3, cities: ['city'], referenceDate: '2010-01-01', offset: 1, nationalSd: 1.5, localSd: .5, nationalInnovationSd: .35, localInnovationSd: .25 };
  const covariance = (a: ReturnType<typeof makeReferenceWalk>, d1: string, d2: string) => {
    const x = a.state('city', d1), y = a.state('city', d2), n = a.dimension;
    // Political score difference (1 minus 2) is gauge invariant.
    return Array.from({ length: n }, (_, k) => (x.design[n + k] - x.design[2 * n + k]) * (y.design[n + k] - y.design[2 * n + k])).reduce((s, v) => s + v, 0);
  };
  it('integrates empty historical windows without changing reference or common-date priors', () => {
    const a = makeReferenceWalk({ ...settings, dates: ['2009-01-01', '2010-01-01'] });
    const b = makeReferenceWalk({ ...settings, dates: ['2000-01-01', '2005-01-01', '2009-01-01', '2010-01-01'] });
    expect(covariance(a, '2010-01-01', '2010-01-01')).toBeCloseTo(covariance(b, '2010-01-01', '2010-01-01'), 13);
    expect(covariance(a, '2009-01-01', '2009-01-01')).toBeCloseTo(covariance(b, '2009-01-01', '2009-01-01'), 13);
    expect(covariance(a, '2009-01-01', '2009-01-01')).toBeGreaterThan(covariance(a, '2010-01-01', '2010-01-01'));
  });
  it('shares overlapping backward intervals and separates future innovations at the reference', () => {
    const a = makeReferenceWalk({ ...settings, dates: ['2008-01-01', '2009-01-01', '2010-01-01', '2011-01-01'] });
    const v = covariance(a, '2010-01-01', '2010-01-01');
    expect(covariance(a, '2008-01-01', '2009-01-01')).toBeCloseTo(covariance(a, '2009-01-01', '2009-01-01'), 13);
    expect(covariance(a, '2008-01-01', '2011-01-01')).toBeCloseTo(v, 13);
  });
  it('bounds genuine births without resetting retrospectively split identities', () => {
    const walk = makeReferenceWalk({ ...settings, dates: ['2008-01-01', '2010-01-01'], supportStarts: ['2000-01-01', '2000-01-01', '2009-01-01'] });
    expect(walk.blocks[0].active).toEqual([0, 1]); expect(walk.blocks[1].active).toEqual([0, 1, 2]);
    expect(() => makeReferenceWalk({ ...settings, dates: ['2010-01-01'], supportStarts: ['2000-01-01', '2011-01-01', '2000-01-01'] })).toThrow(/Future birth/);
  });
  it('uses orthonormal contrast coordinates and exact transformed grouped derivatives', () => {
    const h = basis(3), ht = tr(h, 3, 2), normal = createNormalSampler(createSeededRng('orthonormal'));
    const z = Float64Array.from({ length: 2 }, () => normal(0, 1)), back = mv(ht, mv(h, z, 3), 2);
    expect(back[0]).toBeCloseTo(z[0], 14); expect(back[1]).toBeCloseTo(z[1], 14);
    const walk = makeReferenceWalk({ ...settings, dates: ['2009-01-01'] }), state = walk.state('city', '2009-01-01'), x = zeros(walk.dimension, 1);
    const problem = { dimension: walk.dimension, factors: [{ state, observation: { groups: [[0], [1, 2]], counts: [30, 70], electors: 100 } }] };
    const der = reducedDerivatives(problem, x), step = 1e-5;
    for (let k = 0; k < x.length; k++) {
      const plus = x.slice(), minus = x.slice(); plus[k] += step; minus[k] -= step;
      const p = reducedDerivatives(problem, plus), m = reducedDerivatives(problem, minus);
      expect(der.gradient[k]).toBeCloseTo((p.logPosterior - m.logPosterior) / (2 * step), 5);
      for (let r = 0; r < x.length; r++) expect(der.precision[r * x.length + k]).toBeCloseTo(-(p.gradient[r] - m.gradient[r]) / (2 * step), 5);
    }
    const fit = fitReducedParty(problem); expect(fit.converged).toBe(true);
    expect(Number.isFinite(groupedProbabilities(evaluateAffine(state, fit.mode), [[0], [1, 2]])[0])).toBe(true);
  });
  it('detects separated sampler chains and does not manufacture precision for shared splits', () => {
    const normal = createNormalSampler(createSeededRng('chain-diagnostics'));
    const chains = Array.from({ length: 4 }, () => Array.from({ length: 1000 }, () => normal(0, 1)));
    const good = reducedChainDiagnostics(chains), bad = reducedChainDiagnostics(chains.map((c, i) => c.map(x => x + i * 5)));
    expect(good.rhat).toBeLessThan(1.02); expect(good.ess).toBeGreaterThan(1500);
    expect(bad.rhat).toBeGreaterThan(2); expect(bad.ess).toBeLessThan(10);
  });
  it('checks the original reference density by independent aggregate-logit quadrature', () => {
    const result = groupedReferenceQuadrature({ counts: [30, 40, 30], offset: 1.5, scoreVariance: 2.5, step: .08, splitStep: .12 });
    expect(result.means.reduce((s, x, i) => s + (i < 4 ? x : 0), 0)).toBeCloseTo(1, 11);
    expect(result.means[1]).toBeCloseTo(result.means[2], 10);
    expect(result.means[4]).toBeCloseTo(.5, 10);
    expect(result.means[0]).toBeGreaterThan(.29); expect(result.means[0]).toBeLessThan(.32);
    expect(result.boundaryWeightFraction).toBeLessThan(1e-8);
  });
});
