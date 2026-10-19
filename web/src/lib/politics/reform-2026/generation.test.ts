import { describe, expect, it } from 'vitest';
import { createSeededRng } from '../../core/rng';
import { generationFixture } from './generation.fixture';
import { apportion, binomial, generationRng, mean, multinomial, variance } from './generation-math';
import { candidatePreferenceStrength, generateSyntheticPreferences, preferenceCombinationDistribution, validPreferenceSubsets } from './preference-generation';
import { generateReformSimulation, prepareReformGeneration } from './generation';
import { deriveCommonReferences, pairedStandardShock, preparePairedVoteReferences, generatePairedMunicipalVotes } from './paired-vote-generation';
import { parseReformScenario, serializeReformScenario, validateReformScenario, validateReformSnapshot } from './generation-scenario';
import { validateReformScrutinyInput } from './validation';
import { runPoliticsReformScrutiny } from './scrutiny';
import type { Preferences } from './input';
import { coalitionScrutinyFixture } from './scrutiny.fixture';
import { createSeededDrawContext } from './ties';
import { distributeBonusOnlyVotes } from './attribution';
import { parseScenario, serializeScenario, defaultPoliticsScenario } from '../../scenario/politics';
import { convertLegacyScenarioToReform } from './generation-scenario';

describe('R26-19 isolated streams and aggregate samplers', () => {
  it('conserves integer totals, including reconstruction ties and degenerate categories', () => {
    expect(apportion(5, [1, 1, 1], createSeededRng('x')).reduce((s, v) => s + v)).toBe(5);
    expect(apportion(0, [0, 0], createSeededRng('x'))).toEqual([0, 0]);
    expect(() => apportion(1, [0, 0], createSeededRng('x'))).toThrow();
    expect(multinomial(1000000, [0, 1, 0], createSeededRng('x'))).toEqual([0, 1000000, 0]);
    expect(multinomial(0, [1, 0, 0], createSeededRng('x'))).toEqual([0, 0, 0]);
  });
  it.each([[100, 0.4], [10000, 0.001], [10000, 0.8]])('matches binomial moments for n=%s p=%s', (n, p) => {
    const rng = createSeededRng('binomial-test'), xs = Array.from({ length: 10000 }, () => binomial(n, p, rng));
    expect(Math.abs(mean(xs) - n * p)).toBeLessThan(4 * Math.sqrt(n * p * (1 - p) / xs.length));
    expect(variance(xs) / (n * p * (1 - p))).toBeGreaterThan(0.9);
    expect(variance(xs) / (n * p * (1 - p))).toBeLessThan(1.1);
  });
  it('addresses streams by simulation and component, independently of chunking', () => {
    const first = generationRng('seed', 12, 'preferences')();
    for (let i = 0; i < 20; i++) generationRng('seed', 12, 'candidates')();
    expect(generationRng('seed', 12, 'preferences')()).toBe(first);
    expect(generationRng('seed', 13, 'preferences')()).not.toBe(first);
  });
});

describe('synthetic valid preference combinations', () => {
  const eligible = Array.from({ length: 6 }, (_, i) => ({ id: String(i), gender: i % 2 ? 'female' as const : 'male' as const }));
  it('changes territorial persistence without changing the fitted marginal concentration budget', () => {
    for (const persistence of [0.2, 0.7, 0.95]) {
      const profile = { id: 'test', intensityMultiplier: 1, concentrationMultiplier: 1, persistence, tripleWeight: 1 };
      const draws = Array.from({ length: 1000 }, (_, i) => candidatePreferenceStrength(`r26-19-validation-${i}`, 1, 'camera', 'P1', 'A', 'person', { intensity: 0.5, strengthSigma: 1, localSigma: 0.3 }, profile));
      expect(Math.abs(variance(draws) / 1.09 - 1)).toBeLessThan(0.15);
    }
  });
  it('enumerates all and only the 34 valid combinations, excluding a capolista', () => {
    const ss = validPreferenceSubsets(eligible);
    expect(ss).toHaveLength(34);
    expect(new Set(ss.map(s => s.join(','))).size).toBe(34);
    for (const s of ss) {
      expect(s.length).toBeLessThanOrEqual(3);
      if (s.length > 1) expect(new Set(s.map(id => eligible.find(c => c.id === id)!.gender)).size).toBe(2);
    }
  });
  it('satisfies maximum-entropy intensity and symmetric marginal constraints', () => {
    const ds = preferenceCombinationDistribution(eligible, new Map(eligible.map(c => [c.id, 0])), 0.55);
    expect(ds.reduce((s, d) => s + d.probability, 0)).toBeCloseTo(1, 12);
    expect(ds.reduce((s, d) => s + d.probability * d.candidateIds.length, 0)).toBeCloseTo(0.55, 12);
    for (const c of eligible) expect(ds.filter(d => d.candidateIds.includes(c.id)).reduce((s, d) => s + d.probability, 0)).toBeCloseTo(0.55 / 6, 12);
    expect(preferenceCombinationDistribution(eligible, new Map(eligible.map(c => [c.id, 0])), 0).filter(d => d.probability > 0)).toEqual([{ candidateIds: [], probability: 1 }]);
  });
  it('constructs exact integer ballots and honors supplied/missing scope without draws', () => {
    const options = { seed: 'pref', simulationId: 1, chamber: 'camera', collegeId: 'P1', listId: 'A', eligible, ballotBudget: 1000000,
      parameters: { intensity: 0.55, strengthSigma: 1, localSigma: 0.2 }, profile: { id: 'central-v1', intensityMultiplier: 1, concentrationMultiplier: 1, persistence: 0.7, tripleWeight: 1 },
      provenance: { kind: 'supplied' as const, reference: 'test generated parameters' } };
    const p = generateSyntheticPreferences(options);
    expect(p).toEqual(generateSyntheticPreferences(options));
    if (p.kind !== 'counts' || p.validity.kind !== 'combinations') throw new Error();
    expect(p.validity.ballots.reduce((s, b) => s + b.count, 0)).toBe(options.ballotBudget);
    for (const c of p.counts) expect(c.count).toBe(p.validity.ballots.filter(b => b.candidateIds.includes(c.candidateId)).reduce((s, b) => s + b.count, 0));
    const missing: Preferences = { kind: 'missing', reason: 'not supplied' };
    expect(generateSyntheticPreferences({ ...options, supplied: missing })).toEqual(missing);
    expect(generateSyntheticPreferences({ ...options, ballotBudget: 0 }).kind).toBe('counts');
    const noTriples = generateSyntheticPreferences({ ...options, profile: { ...options.profile, tripleWeight: 0 } });
    if (noTriples.kind !== 'counts' || noTriples.validity.kind !== 'combinations') throw new Error();
    expect(noTriples.validity.ballots.every(b => b.candidateIds.length <= 2)).toBe(true);
  });
});

describe('paired references and uncertainty', () => {
  it('preserves the common log-odds midpoint and distinct chamber references', () => {
    const p = deriveCommonReferences(0.4, 0.3, 0.5, 1);
    expect(p.camera).toBeLessThan(0.4); expect(p.senato).toBeGreaterThan(0.4);
    expect(deriveCommonReferences(0, 0.3, 0.5, 1)).toEqual({ camera: 0, senato: 0 });
  });
  it('preserves marginal budgets and specified correlations over the predeclared 1000 seeds', () => {
    const { snapshot } = generationFixture();
    for (const profile of snapshot.models.chambers.profiles) {
      const xs = Array.from({ length: 1000 }, (_, i) => pairedStandardShock(`r26-19-validation-${i}`, 1, profile, 'national', 'A'));
      const c = xs.map(x => x.camera), s = xs.map(x => x.senato);
      for (const v of [c, s]) { expect(Math.abs(mean(v))).toBeLessThan(4 / Math.sqrt(1000)); expect(Math.abs(variance(v) - 1)).toBeLessThan(0.15); }
      const covariance = mean(xs.map(x => (x.camera - mean(c)) * (x.senato - mean(s))));
      const r = covariance / Math.sqrt(variance(c) * variance(s));
      expect(r).toBeCloseTo(profile.sharedVariance + (1 - profile.sharedVariance) * profile.residualCorrelation, 1);
    }
  });
  it('applies broad-to-specific scope, chamber precedence, partial shares and fixed abstention', () => {
    const { snapshot, scenario } = generationFixture();
    scenario.shares = [
      { scope: { kind: 'national' }, listId: 'A', common: 0.4, camera: 0.5, senato: null },
      { scope: { kind: 'region', code: '01' }, listId: 'A', common: 0.3, camera: null, senato: null },
      { scope: { kind: 'municipality', code: '990001' }, listId: 'A', common: 0.6, camera: 0.7, senato: 0.65 }
    ];
    const prepared = preparePairedVoteReferences(scenario, snapshot), local = prepared.units.find(u => u.municipality.code === '990001')!;
    expect(local.camera.shares.get('A')).toBeCloseTo(0.7, 8); expect(local.senato.shares.get('A')).toBeCloseTo(0.65, 8);
    expect(local.camera.abstention).toBeCloseTo(0.4); expect(local.senato.abstention).toBeCloseTo(0.4);
    expect(local.camera.dates.get('A')).toBe(scenario.projectionDate);
    expect(prepared.trace.references.find(r => r.scope.kind === 'national' && r.listIds.camera === 'A')?.camera?.origin).toBe('supplied');
    expect(prepared.trace.references.find(r => r.scope.kind === 'national' && r.listIds.camera === 'A')?.senato?.origin).toBe('model-derived');
    const first = generatePairedMunicipalVotes(scenario, snapshot, prepared.units, 1), second = generatePairedMunicipalVotes(scenario, snapshot, prepared.units, 2);
    expect(first).not.toEqual(second);
    for (const r of first) expect(r.abstention + r.votes.reduce((s, v) => s + v.votes, 0)).toBe(r.electors);
  });
  it('keeps an unanchored other chamber baseline and normalizes fully supplied valid shares', () => {
    const { snapshot, scenario } = generationFixture();
    scenario.shares = ['A', 'B'].map(listId => ({ scope: { kind: 'national' }, listId, common: null, camera: 0.2, senato: null }));
    const p = preparePairedVoteReferences(scenario, snapshot);
    expect(p.units[0].camera.shares.get('A')).toBeCloseTo(0.5, 8);
    expect(p.units[0].senato.shares.get('A')).toBeCloseTo(0.46, 8);
    expect(p.trace.warnings[0].code).toBe('NORMALIZED_SHARES');
  });
  it('preserves normalized marginal distributions across dependence profiles', () => {
    const { snapshot, scenario } = generationFixture();
    scenario.shares = ['A', 'B'].map(listId => ({ scope: { kind: 'national' }, listId, common: listId === 'A' ? 0.45 : 0.55, camera: null, senato: null }));
    scenario.electionDate = '2026-10-20';
    const prepared = preparePairedVoteReferences(scenario, snapshot);
    const summaries = snapshot.models.chambers.profiles.map(profile => {
      const camera: number[] = [], senato: number[] = [];
      for (let i = 0; i < 1000; i++) {
        const rows = generatePairedMunicipalVotes({ ...scenario, seed: `r26-19-validation-${i}`, chamberProfileId: profile.id }, snapshot, prepared.units, 1);
        for (const ch of ['camera', 'senato'] as const) {
          const rs = rows.filter(r => r.chamber === ch), valid = rs.reduce((n, r) => n + r.electors - r.abstention, 0);
          (ch === 'camera' ? camera : senato).push(rs.reduce((n, r) => n + r.votes.find(v => v.listId === 'A')!.votes, 0) / valid);
        }
      }
      return { camera: { mean: mean(camera), variance: variance(camera) }, senato: { mean: mean(senato), variance: variance(senato) } };
    });
    for (const ch of ['camera', 'senato'] as const) for (const stats of summaries) {
      expect(Math.abs(stats[ch].mean - summaries[0][ch].mean)).toBeLessThan(0.002);
      expect(Math.abs(stats[ch].variance / summaries[0][ch].variance - 1)).toBeLessThan(0.15);
    }
  });
});

describe('versioned generator and complete legal handoff', () => {
  it('round trips strict scenarios, rejects missing/unknown versions and preserves snapshot ownership', () => {
    const { snapshot, scenario } = generationFixture();
    expect(validateReformSnapshot(snapshot)).toEqual([]);
    expect(parseReformScenario(serializeReformScenario(scenario), snapshot)).toEqual(scenario);
    expect(() => parseReformScenario(JSON.stringify({ ...scenario, schemaVersion: 9 }), snapshot)).toThrow();
    expect(validateReformScenario({ ...scenario, algorithmId: 'politiche-r-parity-v1' }, snapshot).length).toBeGreaterThan(0);
    expect(validateReformScenario({ ...scenario, modelVersion: 'other' }, snapshot)).toContain('Scenario data/model version mismatch; explicit conversion is required');
    expect(validateReformSnapshot({ ...snapshot, schemaVersion: 2 }).length).toBeGreaterThan(0);
    const bad = structuredClone(snapshot); bad.memberships.pop(); expect(validateReformSnapshot(bad).length).toBeGreaterThan(0);
  });
  it('preserves legacy scenario parsing and requires explicit mapped model conversion', () => {
    const { snapshot, scenario } = generationFixture();
    expect(parseScenario(serializeScenario(defaultPoliticsScenario))).toEqual(defaultPoliticsScenario);
    expect(() => parseScenario(serializeReformScenario(scenario))).toThrow('convertirlo esplicitamente');
    expect(() => parseScenario(JSON.stringify({ schema_version: 99, scenario: defaultPoliticsScenario }))).toThrow('Versione');
    expect(() => parseScenario(JSON.stringify({ schema_version: 7, algorithmId: 'politiche-reform-2026-v1', scenario: defaultPoliticsScenario }))).toThrow('convertirlo esplicitamente');
    expect(() => parseReformScenario(serializeScenario(defaultPoliticsScenario))).toThrow();
    const legacy = structuredClone(defaultPoliticsScenario);
    legacy.lists = ['A', 'B'].map(name => ({ id: name, name, coalition: null, color: '#123456', startingShare: 50, shareOverride: true }));
    legacy.listCorrespondences = []; legacy.localShareOverrides = []; legacy.candidateTemplates = [];
    const original = serializeScenario(legacy);
    const converted = convertLegacyScenarioToReform(legacy, snapshot, { sourceAlgorithmId: 'politiche-r-parity-v1', projectionDate: '2026-10-19', seed: 'convert', listMap: { A: 'A', B: 'B' } });
    expect(converted.shares.map(r => r.common)).toEqual([0.5, 0.5]);
    expect(converted.conversion?.from).toContain('politiche-r-parity-v1');
    expect(serializeScenario(legacy)).toBe(original);
    expect(() => convertLegacyScenarioToReform(legacy, snapshot, { sourceAlgorithmId: 'politiche-r-parity-v1', projectionDate: '2026-10-19', seed: 'convert', listMap: {} })).toThrow('Missing explicit');
  });
  it('generates valid seven-slot/bonus/special candidacies, paired identities and deterministic complete scrutiny', () => {
    const { snapshot, scenario } = generationFixture();
    const prepared = prepareReformGeneration(scenario, snapshot);
    const generated = generateReformSimulation(prepared, 1);
    expect(validateReformScrutinyInput(generated.input).valid).toBe(true);
    expect(generated).toEqual(generateReformSimulation(prepared, 1));
    const result = runPoliticsReformScrutiny(generated.input);
    expect(result.simulationId).toBe(1);
    expect(result.chambers.map(c => c.simulationId)).toEqual([1, 1]);
    expect(result.stages.filter(s => s.status === 'non-computable')).toEqual([]);
    expect(result.seats.filter(s => s.entitlement.chamber === 'camera')).toHaveLength(392);
    expect(result.seats.filter(s => s.entitlement.chamber === 'senato')).toHaveLength(196);
    expect(new Set(result.seats.filter(s => s.status === 'filled').map(s => s.status === 'filled' && s.candidateId)).size).toBe(result.seats.filter(s => s.status === 'filled').length);
  });
  it('isolates preferences from vote/candidate/legal streams and keeps single renewals single', () => {
    const { snapshot, scenario } = generationFixture();
    const a = generateReformSimulation(prepareReformGeneration(scenario, snapshot), 1).input;
    scenario.preferenceProfileId = 'dispersed-v1';
    const b = generateReformSimulation(prepareReformGeneration(scenario, snapshot), 1).input;
    expect(a.candidates).toEqual(b.candidates);
    if (a.renewal.kind !== 'joint' || b.renewal.kind !== 'joint') throw new Error();
    for (const ch of ['camera', 'senato'] as const) { expect(a.renewal[ch].ordinaryVotes).toEqual(b.renewal[ch].ordinaryVotes); expect(a.renewal[ch].specialVotes).toEqual(b.renewal[ch].specialVotes); }
    expect(a.randomness).toEqual(b.randomness);
    scenario.renewal = 'senato'; scenario.bonusLinks = [];
    const single = generateReformSimulation(prepareReformGeneration(scenario, snapshot), 1).input;
    expect(single.renewal.kind).toBe('single');
    if (single.renewal.kind !== 'single') throw new Error();
    expect(single.renewal.chamber.chamber).toBe('senato'); expect(single.renewal.chamber.bonusSlates).toEqual([]);
    expect(single.candidates.every(c => c.id.includes(':senato:'))).toBe(true);
    expect(runPoliticsReformScrutiny(single).bonus.status).toBe('not-awarded');
  });
  it('uses direct ballots as the preference budget and preserves explicitly missing supplied counts', () => {
    const { snapshot, scenario } = generationFixture();
    const original = generateReformSimulation(prepareReformGeneration(scenario, snapshot), 1).input;
    if (original.renewal.kind !== 'joint') throw new Error();
    const row = original.renewal.camera.ordinaryVotes[0];
    if (row.votes.kind !== 'attributed') throw new Error();
    const direct = row.votes.listVotes.map(r => ({ ...r, votes: Math.floor(r.votes / 2) }));
    scenario.suppliedVotes = [{ chamber: 'camera', collegeId: row.collegeId, votes: { kind: 'components', directListVotes: direct,
      bonusOnlyVotes: row.votes.listVotes.map((r, i) => ({ subject: { kind: 'list', id: r.listId }, votes: r.votes - direct[i].votes })), provenance: { kind: 'supplied', reference: 'explicit composition' } } }];
    const generated = generateReformSimulation(prepareReformGeneration(scenario, snapshot), 1).input;
    if (generated.renewal.kind !== 'joint') throw new Error();
    for (const slate of generated.renewal.camera.ordinarySlates.filter(s => s.collegeId === row.collegeId)) {
      if (slate.preferences.kind !== 'counts' || slate.preferences.validity.kind !== 'combinations') throw new Error();
      expect(slate.preferences.validity.ballots.reduce((n, b) => n + b.count, 0)).toBe(direct.find(r => r.listId === slate.listId)!.votes);
    }
    scenario.suppliedPreferences.push({ chamber: 'camera', collegeId: row.collegeId, listId: direct[0].listId, preferences: { kind: 'missing', reason: 'explicitly unavailable' } });
    const missing = generateReformSimulation(prepareReformGeneration(scenario, snapshot), 1).input;
    if (missing.renewal.kind !== 'joint') throw new Error();
    expect(missing.renewal.camera.ordinarySlates.find(s => s.collegeId === row.collegeId && s.listId === direct[0].listId)?.preferences).toEqual({ kind: 'missing', reason: 'explicitly unavailable' });
  });
  it('preserves candidate identities across compatible multiple candidacies and applies pinned names and independent special affiliation', () => {
    const { snapshot, scenario } = generationFixture();
    const generated = generateReformSimulation(prepareReformGeneration(scenario, snapshot), 1).input;
    if (generated.renewal.kind !== 'joint') throw new Error();
    const c = generated.renewal.camera, slate = c.ordinarySlates[0], special = c.specialCandidatures[0];
    const uses = new Map<string, number>();
    for (const slate of c.ordinarySlates) for (const id of slate.candidateIds) uses.set(id, (uses.get(id) ?? 0) + 1);
    expect([...uses.values()].some(n => n > 1)).toBe(true);
    expect(Math.max(...uses.values())).toBeLessThanOrEqual(5);
    scenario.candidateTemplates = [
      { chamber: 'camera', role: 'ordinary', locationId: slate.collegeId, ownerId: slate.listId, position: 1, affiliationListId: slate.listId,
        candidate: { ...generated.candidates.find(p => p.id === slate.candidateIds[0])!, name: 'Supplied ordinary name' } },
      { chamber: 'camera', role: 'special', locationId: special.collegeId, ownerId: special.linkedListIds[0], position: 1, affiliationListId: null,
        candidate: { ...generated.candidates.find(p => p.id === special.candidateId)!, name: 'Supplied independent special name' } }
    ];
    const pinned = generateReformSimulation(prepareReformGeneration(scenario, snapshot), 1).input;
    expect(pinned.candidates.find(p => p.id === slate.candidateIds[0])?.name).toBe('Supplied ordinary name');
    if (pinned.renewal.kind !== 'joint') throw new Error();
    expect(pinned.renewal.camera.specialCandidatures.find(p => p.candidateId === special.candidateId)?.affiliationListId).toBeNull();
  });
  it('preserves exact downstream scrutiny across attributed and explicit representations, including an attribution tie', () => {
    const components = coalitionScrutinyFixture();
    if (components.renewal.kind !== 'joint') throw new Error();
    const chamber = components.renewal.camera, row = chamber.ordinaryVotes.find(r => r.collegeId === 'P00')!;
    row.votes = { kind: 'components', directListVotes: [{ listId: 'A', votes: 1001 }, { listId: 'B', votes: 38500 }, { listId: 'C', votes: 1001 }],
      bonusOnlyVotes: [{ subject: { kind: 'coalition', id: 'G' }, votes: 1 }], provenance: { kind: 'supplied', reference: 'worked tied attribution' } };
    const context = createSeededDrawContext(components.randomness.seed, JSON.stringify([components.randomness.streamVersion, components.algorithmId, components.simulationId, 'attribution']));
    const attributed = distributeBonusOnlyVotes({ chamber: 'camera', simulationId: components.simulationId, collegeId: row.collegeId, coalitions: chamber.coalitions, votes: row.votes }, context);
    if (attributed.status !== 'complete') throw new Error();
    expect(attributed.draws).toHaveLength(1);
    const final = structuredClone(components);
    if (final.renewal.kind !== 'joint') throw new Error();
    final.renewal.camera.ordinaryVotes.find(r => r.collegeId === 'P00')!.votes = { kind: 'attributed', listVotes: attributed.listVotes as { listId: string; votes: number }[], provenance: row.votes.provenance };
    const a = runPoliticsReformScrutiny(components), b = runPoliticsReformScrutiny(final);
    expect(a.seats).toEqual(b.seats); expect(a.bonus).toEqual(b.bonus);
    expect(a.draws.filter(d => d.norm !== 'N04')).toEqual(b.draws.filter(d => d.norm !== 'N04'));
    for (const ch of ['camera', 'senato'] as const) {
      const ac = a.chambers.find(c => c.chamber === ch)!, bc = b.chambers.find(c => c.chamber === ch)!;
      expect(ac.admission).toEqual(bc.admission); expect(ac.figures).toEqual(bc.figures); expect(ac.recovery).toEqual(bc.recovery);
    }
  });
});
