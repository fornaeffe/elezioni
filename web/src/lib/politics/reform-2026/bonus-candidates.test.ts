import { describe, expect, it } from 'vitest';
import { generationFixture } from './generation.fixture';
import { generateReformSimulation, prepareReformGeneration } from './generation';
import { generateReformCandidatures } from './reform-candidate-generation';
import { generateLegacyReformCandidatures } from './legacy-candidate-generation';
import { parseReformScenario, serializeReformScenario, validateReformScenario } from './generation-scenario';
import { previewReformScenario } from './editor-preview';
import { bonusKey, bonusReferenceSupport } from './bonus-reference-support';
import { preparePairedVoteReferences } from './paired-vote-generation';
import type { Candidate } from './input';

const person = (id: string, gender: Candidate['gender'] = 'female'): Candidate => ({ id, name: id, gender,
  birthDate: '1970-01-01', availability: { kind: 'available' }, externalCandidacies: [] });
function fixture(coalition = false) {
  const f = generationFixture();
  if (coalition) {
    for (const ch of ['camera', 'senato'] as const) f.scenario.lists[ch] = f.scenario.lists[ch].map(l => ({ ...l, coalitionId: 'G' }));
    f.scenario.presentation!.coalitions = [{ id: 'G', name: 'G', color: '#123456' }];
    f.scenario.bonusLinks = [{ camera: { kind: 'coalition', id: 'G' }, senato: { kind: 'coalition', id: 'G' }, cameraDeclares: true, senatoDeclares: true }];
  }
  return f;
}
function candidates(f: ReturnType<typeof fixture>, id = 1) {
  const p = prepareReformGeneration(f.scenario, f.snapshot);
  return generateReformCandidatures(p.scenario, p.snapshot, id, p.bonusPlan);
}
const large = 'camera:territory:03';

describe('reference proportional bonus candidatures', () => {
  it('conserves fractional municipal reference totals across filing-restricted census splits', () => {
    const f = fixture(), units = preparePairedVoteReferences(f.scenario, f.snapshot).units.slice(0, 1), u = units[0];
    f.scenario.renewal = 'camera';
    u.camera.shares = new Map([['A', .4], ['B', .6]]); u.camera.abstention = 0;
    const a = f.snapshot.seats.geography.colleges.find(c => c.chamber === 'camera' && c.kind === 'ordinary')!;
    const b = f.snapshot.seats.geography.colleges.find(c => c.chamber === 'camera' && c.kind === 'ordinary' && c.territoryId !== a.territoryId)!;
    f.snapshot.memberships = [a, b].map(c => ({ chamber: 'camera', municipalityCode: u.municipality.code, collegeId: c.id, population: 1 }));
    f.scenario.coverage.find(c => c.collegeId === a.id)!.listIds = ['A', 'B'];
    f.scenario.coverage.find(c => c.collegeId === b.id)!.listIds = ['B'];
    const support = bonusReferenceSupport(f.scenario, f.snapshot, units), electors = u.municipality.camera.electors;
    expect(support.get(bonusKey('camera', a.territoryId, 'A'))).toBeCloseTo(electors * .4, 8);
    expect(support.get(bonusKey('camera', a.territoryId, 'B'))).toBeCloseTo(electors * .1, 8);
    expect(support.get(bonusKey('camera', b.territoryId, 'B'))).toBeCloseTo(electors * .5, 8);
    expect(support.get(bonusKey('camera', b.territoryId, 'A')) ?? 0).toBe(0);
    expect([...support.values()].reduce((a, b) => a + b, 0)).toBeCloseTo(electors, 8);
  });
  it('freezes turnout-weighted local Hamilton allocations before vote draws and preserves replay', () => {
    const f = fixture(true), prepared = prepareReformGeneration(f.scenario, f.snapshot);
    const b = prepared.bonusPlan!.slates.find(b => b.chamber === 'camera' && b.territoryId === large)!;
    expect(b.allocations.map(a => [a.listId, a.slots])).toEqual([['A', 2], ['B', 3]]);
    expect(b.allocations[0].share).toBeCloseTo(.45, 12); expect(b.allocations[1].share).toBeCloseTo(.55, 12);
    const before = JSON.stringify(prepared.bonusPlan);
    const a = generateReformSimulation(prepared, 1), c = generateReformSimulation(prepared, 2);
    expect(a.input.renewal).not.toEqual(c.input.renewal);
    expect(a.trace.bonusSelection).toEqual(c.trace.bonusSelection);
    expect(JSON.stringify(prepared.bonusPlan)).toBe(before);
    expect(generateReformSimulation(prepared, 1)).toEqual(a);
    const changed = structuredClone(f.scenario); changed.electionDate = '2030-01-01'; changed.chamberProfileId = 'weaker-v1';
    expect(prepareReformGeneration(changed, f.snapshot).bonusPlan).toEqual(prepared.bonusPlan);
  });
  it('uses specific local reference support and keeps missing territorial support unchanged', () => {
    const f = fixture(true);
    const territory = f.snapshot.seats.chambers[0].territories.find(t => t.id === large)!;
    f.scenario.shares = [{ scope: { kind: 'region', code: territory.regionId }, listId: 'A', common: null, camera: .8, senato: null }];
    const p = prepareReformGeneration(f.scenario, f.snapshot);
    expect(p.bonusPlan!.slates.find(b => b.chamber === 'camera' && b.territoryId === large)!.allocations.map(a => a.slots)).toEqual([4, 1]);
    const zero = fixture(true);
    zero.snapshot.municipalities.forEach(m => { m.camera.votes.find(v => v.listId === 'A')!.votes = 0; });
    const z = prepareReformGeneration(zero.scenario, zero.snapshot);
    expect(z.bonusPlan!.slates.filter(b => b.chamber === 'camera').every(b => b.allocations[0].expectedVotes === 0 && b.allocations[0].slots === 0)).toBe(true);
  });
  it('shuffles colleges reproducibly and traverses heads before later ordinary positions', () => {
    const f = fixture(); f.scenario.candidateGeneration.reuseProbability = 0;
    const prepared = prepareReformGeneration(f.scenario, f.snapshot), orders = new Set<string>();
    for (let id = 1; id <= 12; id++) {
      const c = generateReformCandidatures(f.scenario, f.snapshot, id, prepared.bonusPlan).chambers.get('camera')!;
      const b = c.bonusSlates.find(b => b.territoryId === large && b.subject.id === 'A')!;
      const placements = b.candidates.map(p => {
        const ordinary = c.ordinarySlates.find(o => o.candidateIds.includes(p.candidateId))!;
        return [ordinary.collegeId, ordinary.candidateIds.indexOf(p.candidateId)];
      });
      expect(placements.map(p => p[1])).toEqual([0, 0, 1, 1, 2]);
      expect(placements[0][0]).toBe(placements[2][0]); expect(placements[1][0]).toBe(placements[3][0]);
      orders.add(String(placements[0][0]));
    }
    expect(orders.size).toBe(2);
    const c = generateReformCandidatures(f.scenario, f.snapshot, 1, prepared.bonusPlan).chambers.get('camera')!;
    for (const b of c.bonusSlates) {
      const local = c.ordinarySlates.filter(o => o.listId === b.subject.id && f.snapshot.seats.chambers[0].colleges.find(p => p.id === o.collegeId)?.territoryId === b.territoryId);
      expect(b.candidates.map((p, i) => c.ordinarySlates.find(o => o.candidateIds.includes(p.candidateId))!.candidateIds.indexOf(p.candidateId)))
        .toEqual(b.candidates.map((_, i) => Math.floor(i / local.length)));
    }
  });
  it('allows bonus people up to five ordinary uses while retaining unique bonus identities', () => {
    const f = fixture(); f.scenario.candidateGeneration.reuseProbability = 1;
    const c = candidates(f).chambers.get('camera')!;
    const bonus = c.bonusSlates.flatMap(b => b.candidates.map(p => p.candidateId));
    const uses = new Map<string, number>();
    c.ordinarySlates.forEach(o => o.candidateIds.forEach(id => uses.set(id, (uses.get(id) ?? 0) + 1)));
    expect(new Set(bonus).size).toBe(bonus.length);
    expect(bonus.some(id => uses.get(id) === 5)).toBe(true);
    expect(Math.max(...uses.values())).toBe(5);
    expect(() => generateReformSimulation(prepareReformGeneration(f.scenario, f.snapshot), 1)).not.toThrow();
  });
  it('keeps archives without a policy on the exact original generator and rejects unknown policies', () => {
    const f = fixture(); delete f.scenario.candidateGeneration.bonusPolicy;
    const text = serializeReformScenario(f.scenario);
    expect(serializeReformScenario(parseReformScenario(text))).toBe(text);
    const p = prepareReformGeneration(f.scenario, f.snapshot); expect(p.bonusPlan).toBeUndefined();
    expect(generateReformCandidatures(f.scenario, f.snapshot, 4)).toEqual(generateLegacyReformCandidatures(f.scenario, f.snapshot, 4));
    expect(validateReformScenario({ ...f.scenario, candidateGeneration: { ...f.scenario.candidateGeneration, bonusPolicy: 'unknown' } }).length).toBeGreaterThan(0);
  });
});

describe('supplied people take precedence over generated bonus selections', () => {
  it('keeps supplied ordinary positions and excludes undeclared people and later prefix positions', () => {
    const f = fixture(), colleges = f.snapshot.seats.chambers[0].colleges.filter(c => c.territoryId === large);
    f.scenario.candidateTemplates.push({ chamber: 'camera', role: 'ordinary', locationId: colleges[0].id, ownerId: 'A', position: 1, affiliationListId: 'A', candidate: person('ordinary-only') });
    const c = candidates(f).chambers.get('camera')!;
    expect(c.ordinarySlates.find(o => o.collegeId === colleges[0].id && o.listId === 'A')!.candidateIds[0]).toBe('ordinary-only');
    expect(c.bonusSlates.every(b => b.candidates.every(p => p.candidateId !== 'ordinary-only'))).toBe(true);
    const other = c.ordinarySlates.find(o => o.collegeId === colleges[1].id && o.listId === 'A')!;
    expect(c.bonusSlates.find(b => b.territoryId === large && b.subject.id === 'A')!.candidates.map(p => p.candidateId)).toEqual(other.candidateIds.slice(0, 5));
  });
  it('keeps supplied bonus positions and adjusts only the remaining allocation when pins exceed Hamilton', () => {
    const f = fixture(true);
    for (let i = 1; i <= 3; i++) f.scenario.candidateTemplates.push({ chamber: 'camera', role: 'bonus', locationId: large, ownerId: 'G', position: i,
      affiliationListId: 'A', candidate: person(`bonus-${i}`, i % 2 ? 'female' : 'male') });
    const prepared = prepareReformGeneration(f.scenario, f.snapshot);
    const b = prepared.bonusPlan!.slates.find(b => b.chamber === 'camera' && b.territoryId === large)!;
    expect(b.allocations.map(a => [a.referenceSlots, a.slots])).toEqual([[2, 3], [3, 2]]);
    const c = generateReformCandidatures(f.scenario, f.snapshot, 7, prepared.bonusPlan).chambers.get('camera')!;
    expect(c.bonusSlates.find(b => b.territoryId === large)!.candidates.slice(0, 3).map(p => p.candidateId)).toEqual(['bonus-1', 'bonus-2', 'bonus-3']);
    expect(prepared.references.trace.warnings.some(w => w.code === 'BONUS_SELECTION_CONSTRAINTS')).toBe(true);
  });
  it('preserves declared shared ordinary identities, including a supplied bonus person already in five colleges', () => {
    const f = fixture(); f.scenario.candidateGeneration.reuseProbability = 1;
    const local = f.snapshot.seats.chambers[0].colleges.filter(c => c.territoryId === large);
    const ordinary = [local[0], ...f.snapshot.seats.chambers[0].colleges.filter(c => c.kind === 'ordinary' && c.territoryId !== large).slice(0, 4)];
    ordinary.forEach(c => f.scenario.candidateTemplates.push({ chamber: 'camera', role: 'ordinary', locationId: c.id, ownerId: 'A', position: 1, affiliationListId: 'A', candidate: person('shared') }));
    f.scenario.candidateTemplates.push({ chamber: 'camera', role: 'bonus', locationId: large, ownerId: 'A', position: 5, affiliationListId: null, candidate: person('shared') });
    const p = prepareReformGeneration(f.scenario, f.snapshot);
    for (const id of [1, 2, 7]) {
      const c = generateReformCandidatures(f.scenario, f.snapshot, id, p.bonusPlan).chambers.get('camera')!;
      expect(c.ordinarySlates.filter(o => o.candidateIds.includes('shared'))).toHaveLength(5);
      expect(c.bonusSlates.find(b => b.territoryId === large && b.subject.id === 'A')!.candidates[4].candidateId).toBe('shared');
    }
    const preview = previewReformScenario(f.scenario, f.snapshot, { chamber: 'camera', role: 'bonus', locationId: large, ownerId: 'A' });
    expect(preview.placements[4].ordinaryLocation).toBe(local[0].id);
  });
  it('reallocates capacity blocked by ordinary-only heads and rejects an impossible standalone slate', () => {
    const f = fixture(true), territory = 'camera:territory:06';
    const college = f.snapshot.seats.chambers[0].colleges.find(c => c.territoryId === territory)!;
    f.scenario.candidateTemplates.push({ chamber: 'camera', role: 'ordinary', locationId: college.id, ownerId: 'A', position: 1, affiliationListId: 'A', candidate: person('blocked') });
    const p = prepareReformGeneration(f.scenario, f.snapshot);
    expect(p.bonusPlan!.slates.find(b => b.chamber === 'camera' && b.territoryId === territory)!.allocations.map(a => a.slots)).toEqual([0, 2]);
    const standalone = fixture(); standalone.scenario.candidateTemplates = f.scenario.candidateTemplates;
    expect(() => prepareReformGeneration(standalone.scenario, standalone.snapshot)).toThrow('prefix capacity');
  });
  it('rejects duplicate bonus declarations and incompatible supplied genders', () => {
    const f = fixture();
    for (let i = 1; i <= 2; i++) f.scenario.candidateTemplates.push({ chamber: 'camera', role: 'bonus', locationId: large, ownerId: 'A', position: i, affiliationListId: 'A', candidate: person('duplicate') });
    expect(() => prepareReformGeneration(f.scenario, f.snapshot)).toThrow('multiple bonus slots');
    f.scenario.candidateTemplates = [];
    const college = f.snapshot.seats.chambers[0].colleges[0];
    for (let i = 1; i <= 2; i++) f.scenario.candidateTemplates.push({ chamber: 'camera', role: 'ordinary', locationId: college.id, ownerId: 'A', position: i, affiliationListId: 'A', candidate: person(`same-gender-${i}`) });
    expect(() => prepareReformGeneration(f.scenario, f.snapshot)).toThrow('do not alternate');
  });
  it('counts a supplied person shared by two local heads only once on the bonus slate', () => {
    const f = fixture();
    const local = f.snapshot.seats.chambers[0].colleges.filter(c => c.territoryId === large);
    local.forEach(c => f.scenario.candidateTemplates.push({ chamber: 'camera', role: 'ordinary', locationId: c.id, ownerId: 'A', position: 1, affiliationListId: 'A', candidate: person('two-heads') }));
    f.scenario.candidateTemplates.push({ chamber: 'camera', role: 'bonus', locationId: large, ownerId: 'A', position: 2, affiliationListId: 'A', candidate: person('two-heads') });
    const prepared = prepareReformGeneration(f.scenario, f.snapshot);
    for (const id of [1, 2, 3]) {
      const c = generateReformCandidatures(f.scenario, f.snapshot, id, prepared.bonusPlan).chambers.get('camera')!;
      const bonus = c.bonusSlates.find(b => b.territoryId === large && b.subject.id === 'A')!;
      expect(bonus.candidates).toHaveLength(5);
      expect(bonus.candidates.filter(c => c.candidateId === 'two-heads')).toHaveLength(1);
      expect(new Set(bonus.candidates.map(c => c.candidateId)).size).toBe(5);
    }
    expect(() => generateReformSimulation(prepared, 1)).not.toThrow();
  });
  it('retains an unavailable supplied bonus candidate in its filed position', () => {
    const f = fixture();
    const unavailable = { ...person('unavailable'), availability: { kind: 'unavailable' as const, reason: 'Supplied condition' } };
    f.scenario.candidateTemplates.push({ chamber: 'camera', role: 'bonus', locationId: large, ownerId: 'A', position: 4, affiliationListId: 'A', candidate: unavailable });
    const c = candidates(f);
    expect(c.chambers.get('camera')!.bonusSlates.find(b => b.territoryId === large && b.subject.id === 'A')!.candidates[3].candidateId).toBe(unavailable.id);
    expect(c.candidates.find(c => c.id === unavailable.id)!.availability).toEqual(unavailable.availability);
  });
  it('checks national gender budgets per coalition and rejects an ordinary-only blocking head', () => {
    const f = fixture(true), p = prepareReformGeneration(f.scenario, f.snapshot);
    for (const ch of ['camera', 'senato'] as const) {
      const allocations = p.bonusPlan!.slates.filter(b => b.chamber === ch).flatMap(b => b.allocations);
      const total = allocations.reduce((n, a) => n + a.slots, 0), female = allocations.reduce((n, a) => n + a.female, 0);
      expect(female).toBeLessThanOrEqual(Math.round(total * .6));
      expect(total - female).toBeLessThanOrEqual(Math.round(total * .6));
    }
    // The ordinary-only head cannot be promoted to make the distinct supplied
    // bonus person eligible later in the same single-college constituency.
    const c = candidates(f).chambers.get('camera')!;
    const t = 'camera:territory:09', college = c.ordinarySlates.find(o => o.collegeId.startsWith('camera:') && f.snapshot.seats.chambers[0].colleges.find(p => p.id === o.collegeId)?.territoryId === t)!;
    f.scenario.candidateTemplates.push({ chamber: 'camera', role: 'ordinary', locationId: college.collegeId, ownerId: 'A', position: 1, affiliationListId: 'A', candidate: person('male-head', 'male') });
    f.scenario.candidateTemplates.push({ chamber: 'camera', role: 'bonus', locationId: t, ownerId: 'G', position: 1, affiliationListId: 'A', candidate: person('female-bonus') });
    expect(() => prepareReformGeneration(f.scenario, f.snapshot)).toThrow('prefix capacity');
  });
});
