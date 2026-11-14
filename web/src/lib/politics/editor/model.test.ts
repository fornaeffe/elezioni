import { describe, expect, it } from 'vitest';
import { fixture, location } from '../scenario/scenario.fixture';
import { createDefaultCommonPoliticsScenario } from '../scenario/defaults';
import { parsePoliticsScenario } from '../scenario/validation';
import { selectGenerator, selectLaw, createCandidate, setMembers, setMembershipLock, setFilings } from '../scenario/edits';
import { encodeScenario, decodeScenario } from '../scenario/serialization';
import { REFORM, ROSATELLUM, type Placement } from '../scenario/schema';
import { referenceRows, setReference, removeReferences, candidateReason, editScenario } from './model';

describe('common editor form semantics', () => {
  it('preserves every inactive input and per-law selection across edits and both encodings', () => {
    const original = createDefaultCommonPoliticsScenario();
    let s = selectGenerator(original, 'M01', '2.0.0');
    s = editScenario(s, n => { n.execution.fittingSeed = 'visible-fit'; n.execution.simulationSeed = 'visible-sim'; });
    const expected = s;
    s = selectLaw(s, ROSATELLUM);
    for (const gzip of [false, true]) {
      const loaded = decodeScenario(encodeScenario(s, gzip));
      expect(loaded.selection.generatorsByLaw[REFORM].id).toBe('M01');
      expect(selectLaw(loaded, REFORM)).toEqual(expected);
      expect(loaded.modelSettings).toEqual(original.modelSettings);
    }
  });
  it('distinguishes a blank elector reference from explicit zero and defaults its SD', () => {
    const { scenario } = fixture();
    const next = setReference(scenario, null, { kind: 'national' }, 'common', '0');
    expect(next.nonValidReferences[0]).toMatchObject({ mean: 0, denominator: 'electors', uncertainty: { sdPercentagePoints: 0, origin: 'default' } });
    expect(setReference(next, null, { kind: 'national' }, 'common', '').nonValidReferences).toEqual([]);
    const one = setReference(scenario, null, { kind: 'municipality', namespace: scenario.data.referenceGeography.version, code: '001001' }, 'senato', '1');
    expect(one.nonValidReferences[0].uncertainty?.sdPercentagePoints).toBe(0.5);
  });
  it('retains explicit SDs, recalculates default SDs on mean edits and rejects boundary conflicts atomically', () => {
    const { scenario } = fixture();
    const target = { kind: 'list', id: 'a' } as const, scope = { kind: 'national' } as const;
    const edited = setReference(scenario, target, scope, 'common', '1');
    expect(edited.references[0].uncertainty?.sdPercentagePoints).toBe(1);
    const defaults = setReference(edited, target, scope, 'common', '1', '');
    expect(defaults.references[0].uncertainty).toMatchObject({ sdPercentagePoints: 0.5, origin: 'default' });
    expect(setReference(defaults, target, scope, 'common', '10').references[0].uncertainty?.sdPercentagePoints).toBe(1);
    expect(() => setReference(edited, target, scope, 'common', '0')).toThrow('INVALID_UNCERTAINTY');
    expect(edited.references[0].mean).toBe(0.01);
  });
  it('honors model-specific reference declarations during an edit', () => {
    const { scenario } = fixture();
    const s = editScenario(scenario, n => {
      n.references[0].appliesTo = ['M02'];
      n.references.push({ ...n.references[0], id: 'm01-ref', appliesTo: ['M01'], mean: 0.25 });
    });
    const updated = setReference(s, { kind: 'list', id: 'a' }, { kind: 'national' }, 'common', '35');
    expect(updated.references.find(r => r.id === 'm01-ref')?.mean).toBe(0.25);
    expect(updated.references.find(r => r.id === 'r-a')?.mean).toBe(0.35);
    expect(referenceRows(updated, { kind: 'list', id: 'a' })).toHaveLength(2);
  });
  it('removes a selected subtree while retaining other subjects and scopes', () => {
    let { scenario } = fixture();
    const region = { kind: 'region', namespace: scenario.data.referenceGeography.version, code: '01' } as const;
    scenario = setReference(scenario, { kind: 'list', id: 'a' }, region, 'camera', '10');
    scenario = setReference(scenario, { kind: 'list', id: 'b' }, region, 'camera', '20');
    const next = removeReferences(scenario, { kind: 'list', id: 'a' }, [region]);
    expect(next.references).toHaveLength(2);
    expect(next.references.some(r => r.subject.id === 'b')).toBe(true);
  });
  it('addresses imported local scopes independently of JSON property order', () => {
    const s = createDefaultCommonPoliticsScenario();
    const loaded = decodeScenario(encodeScenario(s));
    const row = loaded.nonValidReferences[0];
    if (row.scope.kind === 'national') throw new Error('Expected the authored regional reference');
    const scope = { kind: row.scope.kind, namespace: row.scope.namespace, code: row.scope.code };
    const next = setReference(loaded, null, scope, row.channel, '20');
    expect(next.nonValidReferences).toHaveLength(loaded.nonValidReferences.length);
    expect(next.nonValidReferences.find(r => r.id === row.id)?.mean).toBe(0.2);
  });
  it('retains pins on unfiling and rejects a conflicting re-lock without changing either chamber', () => {
    const { scenario, context } = fixture();
    let s = setMembershipLock(scenario, 'a', false, context);
    s = setMembers(s, 'a', 'camera', ['cc'], context);
    s = setMembershipLock(s, 'b', false, context);
    s = setMembers(s, 'b', 'senato', ['cc'], context);
    expect(() => setMembershipLock(s, 'a', true, context)).toThrow('MEMBERSHIP_CONFLICT');
    expect(s.memberships.find(m => m.listId === 'a')).toMatchObject({ locked: false, camera: ['cc'], senato: ['ca'] });
    const unfiled = setFilings(s, REFORM, 'a', [], context);
    expect(unfiled.memberships).toEqual(s.memberships);
    expect(unfiled.lawSettings[ROSATELLUM]).toEqual(s.lawSettings[ROSATELLUM]);
  });
  it('explains disabled candidate options and allows editing invalid retained declarations', () => {
    let { scenario, context } = fixture();
    scenario = createCandidate(scenario, { id: 'c', name: 'Candidate', gender: 'female', availability: { kind: 'available' }, birthDate: '2000-01-01' });
    const pin: Placement = { id: 'p', candidateId: 'c', location: location(REFORM, 'senato'), owner: { kind: 'list', id: 'b' }, role: 'ordinary', position: 1, affiliationListId: 'b' };
    expect(candidateReason(scenario, scenario.candidates[0], pin, context)).toMatch(/Età/);
    const unavailable = { ...scenario.candidates[0], availability: { kind: 'unavailable', reason: 'Rinuncia' } } as const;
    expect(candidateReason(scenario, unavailable, pin, context)).toBe('Non disponibile: Rinuncia');
    const retained = editScenario(scenario, n => { n.lawSettings[REFORM].placements.push(pin); });
    expect(parsePoliticsScenario(retained).lawSettings[REFORM].placements).toHaveLength(1);
  });
});
