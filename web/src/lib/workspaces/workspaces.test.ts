import { describe, expect, it, vi } from 'vitest';
import { workspaceFor, validateWorkspaceRequest, workspaces } from './registry';
import { scenarioWorkspace, fileWorkspace } from './import-routing';
import { createDefaultPoliticsScenario, cloneScenario, parseScenario, serializeScenario, validateScenario, renameScenarioList, removeScenarioList } from '../scenario/politics';
import { politicsCandidateTickets } from '../scenario/politics-tickets';
import { generatedSnapshotPoliticsScenario } from '../scenario/politics-defaults.generated';
import { createDefaultRegionalErScenario } from '../scenario/regional-er';
import { isFiled } from './filing';
import { validateSnapshot, loadWorkspaceSnapshot } from './data';
import { snapshotDescriptors } from './snapshots.generated';
import { readResultPage } from './result-access';
import type { SimulationResult } from '../core/types';
import future from '../../../../data/fast-share-future-scenario.json';

describe('supported workspace contracts', () => {
  it('registers only supported combinations and preserves the default law', () => {
    expect(workspaces).toHaveLength(3);
    expect(workspaceFor('politiche').id).toBe('politics-legacy');
    expect(workspaceFor('politiche', 'politiche-reform-2026-v1').models.map(m => m.id)).toEqual(['M02', 'M01']);
    expect(() => workspaceFor('comunali')).toThrow('Unsupported');
    expect(() => workspaceFor('regionali-er', 'politiche-r-parity-v1')).toThrow('Unsupported');
  });
  it('rejects mismatched scenarios and invalid counts at dispatch', () => {
    const s = createDefaultPoliticsScenario();
    const request = { kind: 'politiche' as const, scenario: s, electionDate: s.electionDate, simulations: 1, seed: 'test', dataVersion: 'v1' };
    expect(validateWorkspaceRequest(request).id).toBe('politics-legacy');
    expect(() => validateWorkspaceRequest({ ...request, simulations: 1001 })).toThrow('massimo 1000');
    expect(() => validateWorkspaceRequest({ ...request, simulations: NaN })).toThrow();
    expect(() => validateWorkspaceRequest({ ...request, scenario: createDefaultRegionalErScenario() })).toThrow('mismatch');
    expect(() => validateWorkspaceRequest({ ...request, scrutinyAlgorithmId: 'politiche-reform-2026-v1' })).toThrow('mismatch');
  });
  it('routes standalone and archived scenarios by metadata', async () => {
    const s = createDefaultPoliticsScenario();
    expect(scenarioWorkspace(s).id).toBe('politics-legacy');
    expect(scenarioWorkspace(createDefaultRegionalErScenario()).id).toBe('regional-er');
    for (const payload of [s, { scenario: s, result: {} }])
      expect((await fileWorkspace(new File([JSON.stringify(payload)], 'arbitrary-name.json'))).id).toBe('politics-legacy');
    expect(() => scenarioWorkspace({ defaultSource: { electionKind: 'unknown' } })).toThrow();
  });
});

describe('shared politics default compilation', () => {
  it('keeps autonomous political membership separate from candidate tickets across edits', () => {
    const s = createDefaultPoliticsScenario(), list = s.lists.find(l => l.id === 'futuro-nazionale')!;
    s.candidateTemplates.push({ id: 'autonomous-candidate', ramo: 'camera', kind: 'uninominal',
      candidateName: 'Candidate', birthDate: null, coalition: list.name, uninominalCode: '20001' });
    expect(validateScenario(s)).toEqual([]);
    expect(politicsCandidateTickets(s).some(t => t.name === list.name && t.independent)).toBe(true);
    const renamed = renameScenarioList(s, list.id, 'Autonomous test');
    expect(renamed.candidateTemplates[0].coalition).toBe('Autonomous test');
    expect(validateScenario(renamed)).toEqual([]);
    expect(removeScenarioList(renamed, list.id).candidateTemplates).toEqual([]);
    expect(validateScenario(renameScenarioList(s, list.id, 'Centrodestra')).some(e => e.includes('coincide'))).toBe(true);
  });
  it('matches the authored choice set, coalitions and geography', () => {
    const s = createDefaultPoliticsScenario();
    expect(validateScenario(s)).toEqual([]);
    expect(s.lists.map(l => [l.id, l.name])).toEqual(future.lists.map(l => [l.id, l.name]));
    expect(s.coalitions.map(c => [c.id, c.name])).toEqual(future.coalitions.map(c => [c.id, c.name]));
    expect(s.lists.find(l => l.id === 'futuro-nazionale')!.coalition).toBeNull();
    expect(s.lists.filter(l => l.minorityRegion).map(l => l.id)).toEqual(['uv', 'svp-patt', 'team-k', 'freiheitlichen', 'suedtiroler-freiheit']);
    expect(s.lists.find(l => l.id === 'fdi')!.filing!.camera).toHaveLength(27);
    expect(s.lists.find(l => l.id === 'futuro-nazionale')!.filing!.camera).toHaveLength(28);
    expect(s.lists.find(l => l.id === 'ora')!.filing!.camera).toHaveLength(14);
    expect(s.lists.find(l => l.id === 'ora')!.filing!.camera).toContain('1902');
    expect(s.lists.find(l => l.id === 'ora')!.filing!.camera).not.toContain('304');
    for (const id of ['cdx-vda', 'csx-vda', 'uv']) expect(s.lists.find(l => l.id === id)!.filing).toEqual({ camera: ['201'], senato: ['2'] });
    expect(s.lists.find(l => l.id === 'm5s')!.coalition).toBe('Centrosinistra');
    expect(s.defaultSource.notes?.length).toBeGreaterThan(5);
    expect(s.localShareOverrides.some(r => r.locationCode === '021')).toBe(true);
  });
  it('roundtrips the new fields, copies owned arrays and preserves old semantics', () => {
    const s = createDefaultPoliticsScenario();
    expect(JSON.parse(serializeScenario(s)).schema_version).toBe(9);
    expect(parseScenario(serializeScenario(s))).toEqual(s);
    const copy = cloneScenario(s); copy.lists[0].filing!.camera.length = 0;
    expect(s.lists[0].filing!.camera.length).toBe(27);
    const old = parseScenario(JSON.stringify({ schema_version: 7, scenario: generatedSnapshotPoliticsScenario }));
    expect(old.lists.every(l => l.filing === undefined)).toBe(true);
    expect(isFiled(old.lists[0], 'camera', '201')).toBe(true);
    expect(old.lists.map(l => l.name)).toEqual(generatedSnapshotPoliticsScenario.lists.map(l => l.name));
    s.lists[0].minorityRegion = '04';
    expect(validateScenario(s).some(e => e.includes('minoranza'))).toBe(true);
  });
});

describe('data and result access boundaries', () => {
  it('rejects corrupt or missing production data without a debug fallback', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response('wrong data')));
    await expect(loadWorkspaceSnapshot('politics', '/elezioni')).rejects.toThrow('checksum');
    expect(fetch).toHaveBeenCalledWith('/elezioni/' + snapshotDescriptors.politics!.path);
    vi.stubGlobal('fetch', vi.fn(async () => new Response('', { status: 404 })));
    await expect(loadWorkspaceSnapshot('regional-er', '')).rejects.toThrow('Missing production');
    vi.unstubAllGlobals();
    expect(() => validateSnapshot('politics', { metadata: { schema_version: 99 } }, 4)).toThrow('schema');
  });
  it('uses one table-page contract for memory and local storage', async () => {
    const table = { name: 'Test', columns: ['Value'], rows: [{ Value: 1 }, { Value: 2 }] };
    const result: SimulationResult = { type: 'result', status: 'completed', warnings: [], tables: [table],
      benchmark: { startedAt: '2026-11-02T00:00:00Z', elapsedMs: 0, simulations: 1, dataVersion: 'test' } };
    expect((await readResultPage(result, { table: 0, offset: 1, limit: 1 })).rows).toEqual([{ Value: 2 }]);
    const stored = vi.fn(async () => table);
    const disk = { tables: [], storage: { id: 'saved', tables: [table] } } as unknown as SimulationResult;
    expect(await readResultPage(disk, { table: 0, offset: 0 }, stored)).toEqual(table);
    expect(stored).toHaveBeenCalledWith({ table: 0, offset: 0, limit: 50, resultId: 'saved' });
    await expect(readResultPage(result, { table: 0, offset: -1 })).rejects.toThrow();
  });
});
