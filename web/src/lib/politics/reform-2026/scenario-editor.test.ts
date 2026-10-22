import { describe, expect, it } from 'vitest';
import { generationFixture } from './generation.fixture';
import { parseReformScenario, serializeReformScenario, validateReformScenario } from './generation-scenario';
import { upgradeReformDraft } from './presentation-metadata';
import { addReformCoalition, addReformList, copyReformPoliticalConfiguration, removeReformList, removedDependencies, saveReformCandidate, setReformCorrespondence, setReformCoverage, setReformReference, updateReformList } from './scenario-editor';
import { createReformEditorCatalog } from './editor-catalog';
import { previewReformScenario } from './editor-preview';
import { generateReformSimulation, prepareReformGeneration } from './generation';
import { createSimulationResultExport, parseSimulationResultExport } from '../../core/result-export';
import type { SimulationResult } from '../../core/types';

describe('reform editor versioning and presentation', () => {
  it('preserves v1 scenario and result archives verbatim; upgrades only editable drafts without changing generation', () => {
    const { scenario, snapshot } = generationFixture();
    const old = { ...scenario, schemaVersion: 1 as const }; delete old.presentation;
    const encoded = JSON.stringify(old);
    expect(serializeReformScenario(parseReformScenario(encoded))).toBe(encoded);
    const upgraded = upgradeReformDraft(old);
    expect(upgraded.schemaVersion).toBe(2);
    expect(old.presentation).toBeUndefined();
    upgraded.presentation!.name = 'Prova'; upgraded.presentation!.lists[0].color = '#123456';
    upgraded.presentation!.coalitions.push({ id: 'display-only', name: 'Coalizione futura', color: '#abcdef' });
    expect(parseReformScenario(serializeReformScenario(upgraded))).toEqual(upgraded);
    expect(generateReformSimulation(prepareReformGeneration(upgraded, snapshot), 1))
      .toEqual(generateReformSimulation(prepareReformGeneration(old, snapshot), 1));
    const result: SimulationResult = { type: 'result', status: 'completed', tables: [], warnings: [], benchmark: {
      startedAt: '2026-10-22T00:00:00Z', elapsedMs: 1, simulations: 1, dataVersion: old.snapshotVersion, scrutinyAlgorithmId: old.algorithmId, seed: old.seed } };
    const exportedAt = '2026-10-22T00:00:00Z';
    const payload = createSimulationResultExport({ scenario: old, result, exportedAt });
    expect(parseSimulationResultExport(JSON.stringify(payload)).scenario).toEqual(old);
    expect(parseSimulationResultExport(JSON.stringify(createSimulationResultExport({ scenario: upgraded, result, exportedAt }))).scenario).toEqual(upgraded);
    expect(validateReformScenario({ ...upgraded, schemaVersion: 3 }).length).toBeGreaterThan(0);
    expect(validateReformScenario({ ...upgraded, presentation: { ...upgraded.presentation, lists: [] } })).toContain('Presentation lists must match scenario list identities');
  });
  it('generates an editor projection tied to the snapshot with no municipal vote matrices', () => {
    const { snapshot } = generationFixture(), catalog = createReformEditorCatalog(snapshot);
    expect(catalog.snapshotVersion).toBe(snapshot.version);
    expect(catalog.municipalities).toHaveLength(snapshot.municipalities.length);
    expect(catalog.municipalities[0]).not.toHaveProperty('camera');
    expect(catalog.colleges.map(c => c.id)).toEqual(snapshot.seats.geography.colleges.map(c => c.id));
  });
});

describe('common editing and explicit chamber overrides', () => {
  it('updates common references without overwriting advanced overrides and distinguishes clearing from zero', () => {
    const { scenario } = generationFixture(); const scope = { kind: 'national' as const };
    let n = setReformReference(scenario, scope, 'A', 'camera', 23);
    n = setReformReference(n, scope, 'A', 'common', 40);
    expect(n.shares[0]).toMatchObject({ common: .4, camera: .23, senato: null });
    n = setReformReference(n, scope, 'A', 'common', 0);
    expect(n.shares[0].common).toBe(0);
    n = setReformReference(n, scope, 'A', 'common', undefined);
    expect(n.shares[0]).toMatchObject({ common: null, camera: .23 });
    expect(setReformReference(n, scope, 'A', 'camera', undefined).shares).toHaveLength(0);
    expect(setReformReference(n, { kind: 'region', code: '01' }, null, 'common', 0).abstention[0].common).toBe(0);
    expect(() => setReformReference(n, scope, 'A', 'common', 101)).toThrow();
    expect(scenario.shares).toHaveLength(0);
  });
  it('adds common identities, preserves them on rename and changes a chamber only on an explicit operation', () => {
    const { scenario } = generationFixture();
    const n = addReformList(scenario, 'common'), id = n.lists.camera.at(-1)!.id;
    expect(n.lists.senato.at(-1)!.id).toBe(id);
    expect(n.coverage.some(c => c.listIds.includes(id))).toBe(false);
    const renamed = updateReformList(n, 'common', id, { name: 'Nome nuovo' });
    expect(renamed.lists.camera.at(-1)).toMatchObject({ id, name: 'Nome nuovo' });
    const cameraOnly = updateReformList(renamed, 'camera', id, { name: 'Solo Camera' });
    expect(cameraOnly.lists.senato.at(-1)!.name).toBe('Nome nuovo');
    const copied = copyReformPoliticalConfiguration(cameraOnly, 'camera', 'senato');
    expect(copied.lists.camera).toEqual(copied.lists.senato);
  });
  it('applies common split factors to both chambers and keeps the historical normalization semantics', () => {
    const { scenario, snapshot } = generationFixture();
    const n = setReformCorrespondence(scenario, 'common', 'A', 'B', 3);
    for (const ch of ['camera', 'senato'] as const) expect(n.correspondences.filter(c => c.chamber === ch && c.pastListId === 'A').map(c => c.factor)).toEqual([1, 3]);
    expect(validateReformScenario(n, snapshot)).toEqual([]);
  });
});

describe('dependent edits and candidate planning', () => {
  it('previews cascading list removal without mutating the original and rejects structural edits to supplied data', () => {
    const { scenario, snapshot } = generationFixture();
    const withShares = setReformReference(scenario, { kind: 'national' }, 'A', 'common', 30);
    const n = removeReformList(withShares, 'common', 'A');
    expect(removedDependencies(withShares, n)).toEqual(expect.arrayContaining(['2 corrispondenze', '1 riferimenti', '1 collegamenti premio']));
    expect(n.coverage.every(c => !c.listIds.includes('A'))).toBe(true);
    expect(validateReformScenario(n, snapshot)).toEqual([]);
    expect(withShares.lists.camera.some(l => l.id === 'A')).toBe(true);
    const supplied = { ...scenario, suppliedPreferences: [{ chamber: 'camera' as const, collegeId: scenario.coverage[0].collegeId, listId: 'A', preferences: { kind: 'missing' as const, reason: 'manual' } }] };
    expect(() => removeReformList(supplied, 'common', 'A')).toThrow('JSON');
    expect(() => setReformCoverage(supplied, 'camera', [], 'A', false)).toThrow('JSON');
  });
  it('shows real bonus-to-ordinary prefixes and applies compatible templates to both placements', () => {
    const { scenario, snapshot } = generationFixture();
    const territory = snapshot.seats.chambers[0].territories.find(t => t.bonusSeats > 0)!;
    const selection = { chamber: 'camera' as const, role: 'bonus' as const, locationId: territory.id, ownerId: 'A' };
    const preview = previewReformScenario(scenario, snapshot, selection), slot = preview.placements[0];
    expect(preview.placements).toHaveLength(territory.bonusSeats);
    expect(slot.ordinaryLocation).not.toBeNull();
    const candidate = { id: 'named-person', name: 'Persona test', birthDate: '1975-01-01', gender: slot.gender as 'female' | 'male', availability: { kind: 'available' as const }, externalCandidacies: [] };
    const n = saveReformCandidate(scenario, { ...selection, position: 1, affiliationListId: 'A', candidate });
    expect(previewReformScenario(n, snapshot, selection).placements[0]).toMatchObject({ candidateId: candidate.id, ordinaryLocation: slot.ordinaryLocation, ordinaryPosition: slot.ordinaryPosition });
    const second = saveReformCandidate(n, { chamber: 'camera', role: 'ordinary', locationId: slot.ordinaryLocation!, ownerId: 'A', position: slot.ordinaryPosition!, affiliationListId: 'A', candidate: { ...candidate, name: 'Nome aggiornato' } });
    expect(second.candidateTemplates.every(t => t.candidate.name === 'Nome aggiornato')).toBe(true);
    expect(scenario.candidateTemplates).toHaveLength(0);
  });
  it('cleans obsolete bonus subjects after a coalition assignment, while retaining editable coalition metadata', () => {
    const { scenario } = generationFixture(); const n = addReformCoalition(scenario);
    const changed = updateReformList(n, 'common', 'A', { coalitionId: n.presentation!.coalitions.at(-1)!.id });
    expect(changed.bonusLinks.some(l => l.camera.id === 'A')).toBe(false);
    expect(changed.presentation!.coalitions).toHaveLength(1);
  });
});
