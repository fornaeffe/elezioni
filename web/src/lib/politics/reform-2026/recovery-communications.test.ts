import { describe, expect, it } from 'vitest';
import { runPoliticsReformScrutiny } from './scrutiny';
import { completeScrutinyFixture, coalitionScrutinyFixture } from './scrutiny.fixture';
import { assembleRecoveryEligibility, assembleRecoveryRemainders } from './recovery-communications';

describe('recovery communication of existing allocation quotients', () => {
  it.each([false, true])('carries single-college Senate regional list values/states, coalition=%s', coalition => {
    const input = coalition ? coalitionScrutinyFixture() : completeScrutinyFixture(60);
    if (input.renewal.kind !== 'joint') throw new Error();
    const result = runPoliticsReformScrutiny(input), source = result.chambers[1];
    const c = structuredClone(input.renewal.senato);
    // This adapter communication has a sole college; the quotas supplied below
    // are real completed N19/N21 records. No college allocation is rerun.
    c.colleges = c.colleges.filter(p => p.kind !== 'ordinary' || p.id === 'P00');
    const rows = assembleRecoveryRemainders(c, { regional: source.regional, lists: source.senateTerritorial!.lists,
      colleges: { ...source.senateTerritorial!.colleges, matrices: [] } });
    const parents = rows.filter(r => r.collegeId === null && r.subject.kind === 'list');
    expect(parents).toHaveLength(3);
    for (const parent of parents) {
      const child = rows.find(r => r.collegeId === 'P00' && r.subject.id === parent.subject.id)!;
      expect(child).toMatchObject({ sourceId: parent.id, value: parent.value, state: parent.state });
      expect(child.id).not.toBe(parent.id);
    }
    expect(new Set(rows.map(r => r.id)).size).toBe(rows.length);
  });
  it('communicates known zero TAA list targets and rejects a missing territorial target', () => {
    const input = completeScrutinyFixture(60);
    if (input.renewal.kind !== 'joint') throw new Error();
    const c = input.renewal.camera, source = runPoliticsReformScrutiny(input).chambers[0];
    const stages = { ...source.cameraTerritorial!, taa: source.taa };
    const rows = assembleRecoveryEligibility(c, source.admission!, stages);
    expect(rows).toContainEqual({ territoryId: 'T', listId: 'C', ordinarySeats: 0 });
    expect(source.recovery!.remainders.some(r => r.collegeId === 'TP' && r.subject.kind === 'list' && r.subject.id === 'C')).toBe(false);
    expect(source.recovery!.excludedLists).toContainEqual({ territoryId: 'T', listId: 'C', reason: 'zero-territorial-entitlement' });
    expect(rows.find(r => r.territoryId === 'O' && r.listId === 'A')!.ordinarySeats).toBeGreaterThan(0);
    const missing = structuredClone(stages);
    if (missing.lists.status !== 'complete') throw new Error();
    missing.lists = { ...missing.lists, targets: missing.lists.targets.filter(t => t.listId !== 'A') };
    expect(() => assembleRecoveryEligibility(c, source.admission!, missing)).toThrow(/Missing final territorial list target/);
  });
});
