import { describe, expect, it } from 'vitest';
import { generationFixture } from './generation.fixture';
import { deriveCommonReferences, preparePairedVoteReferences, type ProjectedMunicipality } from './paired-vote-generation';
import { setReformReference } from './scenario-editor';
import { validateReformScenario, validateReformSnapshot } from './generation-scenario';
import type { Chamber } from './input';

/** A/B compete in 19 municipalities; C/D share a disconnected special ballot.
 * Equal valid-vote totals make that ballot's national mass exactly 5%. */
function disconnectedFixture() {
  const { snapshot, scenario } = generationFixture();
  const special = snapshot.municipalities.find(m => m.regionId === '02')!;
  snapshot.historicalLists.push(...['C', 'D'].map(id => ({ id, name: id, kind: 'list' as const })));
  for (const ch of ['camera', 'senato'] as const) {
    special[ch].votes = [{ listId: 'C', votes: 90000 }, { listId: 'D', votes: 210000 }];
    scenario.lists[ch].push(...['C', 'D'].map(id => ({ id, name: id, coalitionId: null, protectedMinority: null })));
    scenario.correspondences.push(...['C', 'D'].map(id => ({ chamber: ch, pastListId: id, futureListId: id, factor: 1 })));
    const colleges = new Set(snapshot.memberships.filter(m => m.chamber === ch && m.municipalityCode === special.code).map(m => m.collegeId));
    for (const row of scenario.coverage) if (row.chamber === ch && colleges.has(row.collegeId)) row.listIds = ['C', 'D'];
  }
  scenario.presentation!.lists.push(...['C', 'D'].map(id => ({ id, color: '#123456' })));
  return { snapshot, scenario, special };
}

function aggregate(units: readonly ProjectedMunicipality[], ch: Chamber, id: string) {
  const valid = (u: ProjectedMunicipality) => u.municipality[ch].electors * (1 - u[ch].abstention);
  return units.reduce((n, u) => n + valid(u) * u[ch].shares.get(id)!, 0) / units.reduce((n, u) => n + valid(u), 0);
}

describe('partial share references with disconnected geographic support', () => {
  it.each(['common', 'camera', 'senato'] as const)('reconciles a 26.7%% %s reference while preserving special ballots and turnout', side => {
    const { snapshot, scenario, special } = disconnectedFixture();
    expect(validateReformSnapshot(snapshot)).toEqual([]);
    expect(validateReformScenario(scenario, snapshot)).toEqual([]);
    const baseline = preparePairedVoteReferences(scenario, snapshot);
    const anchors = deriveCommonReferences(0.267, aggregate(baseline.units, 'camera', 'A'), aggregate(baseline.units, 'senato', 'A'), 1);
    const edited = setReformReference(scenario, { kind: 'national' }, 'A', side, 26.7);
    const prepared = preparePairedVoteReferences(edited, snapshot);
    for (const ch of ['camera', 'senato'] as const) {
      const expected = side === 'common' ? anchors[ch] : side === ch ? 0.267 : aggregate(baseline.units, ch, 'A');
      expect(aggregate(prepared.units, ch, 'A')).toBeCloseTo(expected, 8);
      expect(aggregate(prepared.units, ch, 'C')).toBeCloseTo(0.015, 12);
      expect(aggregate(prepared.units, ch, 'D')).toBeCloseTo(0.035, 12);
      prepared.units.forEach((u, i) => {
        expect(u[ch].abstention).toBe(baseline.units[i][ch].abstention);
        expect([...u[ch].shares.values()].reduce((a, b) => a + b, 0)).toBeCloseTo(1, 12);
        for (const id of u.municipality.code === special.code ? ['A', 'B'] : ['C', 'D']) expect(u[ch].shares.get(id)).toBe(0);
      });
      const local = prepared.units.find(u => u.municipality.code === special.code)!;
      expect(local[ch].shares.get('C')! / local[ch].shares.get('D')!).toBeCloseTo(3 / 7, 12);
    }
  });

  it('allows references above the baseline as well as below it', () => {
    const { snapshot, scenario } = disconnectedFixture();
    const prepared = preparePairedVoteReferences(setReformReference(scenario, { kind: 'national' }, 'A', 'camera', 60), snapshot);
    expect(aggregate(prepared.units, 'camera', 'A')).toBeCloseTo(0.6, 8);
    expect(aggregate(prepared.units, 'camera', 'C')).toBeCloseTo(0.015, 12);
  });

  it('keeps explicit zeros and leaves unspecified lists free to fill each local ballot', () => {
    const { snapshot, scenario } = disconnectedFixture();
    const prepared = preparePairedVoteReferences(setReformReference(scenario, { kind: 'national' }, 'A', 'camera', 0), snapshot);
    expect(prepared.units.every(u => u.camera.shares.get('A') === 0)).toBe(true);
    expect(aggregate(prepared.units, 'camera', 'B')).toBeCloseTo(0.95, 12);
    expect(aggregate(prepared.units, 'camera', 'C')).toBeCloseTo(0.015, 12);
  });

  it('rejects a reference exceeding the valid votes in its filed geography with a capacity explanation', () => {
    const { snapshot, scenario } = disconnectedFixture();
    const edited = setReformReference(scenario, { kind: 'national' }, 'C', 'camera', 6);
    expect(() => preparePairedVoteReferences(edited, snapshot)).toThrow(/camera\/C requests 6\.000000%, but available valid votes allow at most 5\.000000%/);
  });

  it('rejects zeroing every list on a disconnected ballot', () => {
    const { snapshot, scenario } = disconnectedFixture();
    scenario.shares = ['C', 'D'].map(listId => ({ scope: { kind: 'national' }, listId, common: null, camera: 0, senato: null }));
    expect(() => preparePairedVoteReferences(scenario, snapshot)).toThrow('Override removes all locally filed support');
  });

  it('honors a reference exactly at its filed geographic capacity', () => {
    const { snapshot, scenario, special } = disconnectedFixture();
    const prepared = preparePairedVoteReferences(setReformReference(scenario, { kind: 'national' }, 'C', 'camera', 5), snapshot);
    expect(aggregate(prepared.units, 'camera', 'C')).toBeCloseTo(0.05, 12);
    const local = prepared.units.find(u => u.municipality.code === special.code)!;
    expect(local.camera.shares.get('C')).toBe(1);
    expect(local.camera.shares.get('D')).toBe(0);
  });

  it('honors a 100% partial reference without asymptotically retaining unspecified mass', () => {
    const { snapshot, scenario } = generationFixture();
    const prepared = preparePairedVoteReferences(setReformReference(scenario, { kind: 'national' }, 'A', 'camera', 100), snapshot);
    expect(prepared.units.every(u => u.camera.shares.get('A') === 1 && u.camera.shares.get('B') === 0)).toBe(true);
  });
});
