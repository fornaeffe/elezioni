import { describe, it, expect } from 'vitest';
import { commonPoliticsFixture } from '../../../../tests/fixtures/common-politics-input';
import { preparePoliticsExecution } from './prepare';
import { REFORM, ROSATELLUM } from '../scenario/schema';
import { preparePoliticsCompute, computePoliticsRun } from '../../workers/politics-compute';
import { prepareScenarioGeneration } from '../reform-2026/vote-share/unified-generation';
import { compileLegacy } from '../reform-2026/vote-share/legacy-adapter';
import { requiredAssets } from '../scenario/dependencies';
import { runPoliticsSimulation } from '../simulation-scrutiny';
import { createList } from '../scenario/edits';
import { loadExecutionData } from './assets';

describe('CP-03 common execution', () => {
  it.each(['M01', 'M02'] as const)('executes reform/%s from the shared scenario and preserves retained votes', async model => {
    const f = await commonPoliticsFixture(REFORM, model);
    const prepared = await preparePoliticsExecution(f.scenario, f.readers);
    const old = structuredClone(f.original.accepted.scenario);
    old.generation = { model, modelVersion: '2.0.0', compositions: {}, supplementalPolls: [] };
    old.models.forecast = f.scenario.modelSettings.M02.forecast;
    old.models.forecastScale = f.scenario.modelSettings.M02.forecastScale;
    old.models.profileRegionCorrelation = f.scenario.modelSettings.M02.profileRegionCorrelation;
    old.models.forecastReference = f.scenario.modelSettings.M02.forecastReference;
    const legacy = model === 'M01' ? await compileLegacy(old, f.original.source, f.original.read, f.original.accepted.assets, f.original.accepted.context) : undefined;
    const retained = prepareScenarioGeneration(old, f.original.accepted.assets, f.original.accepted.context, legacy).run(7);
    const generated = prepared.generate(7);
    expect(generated.request.algorithmId).toBe(REFORM);
    if (generated.request.algorithmId !== REFORM) throw new Error('Expected reform');
    // Keep statistical totals separate from candidate RNG/identity and legal metadata.
    const ballots = (input: typeof retained.input) => (input.renewal.kind === 'joint' ? [input.renewal.camera, input.renewal.senato] : [input.renewal.chamber])
      .map(c => [c.chamber, c.ordinaryVotes, c.specialVotes]).map(r => JSON.stringify(r, (k, v) => k === 'provenance' ? undefined : v));
    expect(ballots(generated.request.input)).toEqual(ballots(retained.input));
    expect(prepared.generate(7)).toEqual(generated);
    const retainedLegal = runPoliticsSimulation({ algorithmId: REFORM, simulationId: 7, input: retained.input });
    const nextLegal = runPoliticsSimulation(generated.request);
    const legalOutcome = (value: unknown) => JSON.parse(JSON.stringify(value, (key, v) => {
      if (key === 'chamberModel' || key === 'provenance') return undefined;
      if (key === 'reference' && typeof v === 'string' && v.startsWith('{"kind":"generated"')) {
        const { sourceVersions, assumptions, parameterVersion, ...identity } = JSON.parse(v);
        return identity;
      }
      return v;
    }));
    expect(legalOutcome(nextLegal)).toEqual(legalOutcome(retainedLegal));
    const batch = computePoliticsRun(prepared, 7);
    expect(batch.batch.warnings.filter(w => w.severity === 'error').map(w => w.reason))
      .toEqual(model === 'M02' ? expect.arrayContaining(['initial-overallocation']) : []);
  }, 60000);
  it('runs Rosatellum/M01 with conditional electorates, VdA, both chambers and deterministic candidature', async () => {
    const f = await commonPoliticsFixture(ROSATELLUM);
    const prepared = await preparePoliticsExecution(f.scenario, f.readers);
    const generated = prepared.generate(1);
    expect(generated.request.algorithmId).toBe(ROSATELLUM);
    if (generated.request.algorithmId !== ROSATELLUM) throw new Error('Expected Rosatellum');
    expect(generated.request.chambers.map(c => c.context.ramo)).toEqual(['camera', 'senato']);
    for (const ch of ['camera', 'senato'] as const) {
      const input = generated.request.chambers.find(c => c.context.ramo === ch)!.input;
      expect(input.candidati_uni.some(c => c.COLLEGIOPLURINOMINALE === null)).toBe(true);
      expect(input.candidati_pluri.every(c => c.COLLEGIOPLURINOMINALE !== null)).toBe(true);
      const expected = f.original.accepted.assets.electorates.reduce((n, e) => n + e[ch], 0);
      expect([...generated.votes.counts].filter(([id]) => id.startsWith(`ros:${ch}:`)).reduce((n, [, c]) => n + c.electors, 0)).toBe(expected);
    }
    expect(prepared.generate(1)).toEqual(generated);
    expect(computePoliticsRun(prepared, 1).ready.counts).toMatchObject({ complete: 1, failed: 0 });
  }, 60000);
  it('checks exact assets and the same supported pairs at the compute boundary', async () => {
    const f = await commonPoliticsFixture();
    const prepared = await preparePoliticsExecution(f.scenario, f.readers);
    const packet = { implementationId: prepared.implementationId, scenario: f.scenario, keys: prepared.keys,
      assets: requiredAssets(f.scenario).map(a => ({ id: a.id, bytes: f.bytes.get(a.id)! })),
      sourceChunks: [...f.original.files].map(([path, bytes]) => ({ path, bytes })) };
    expect((await preparePoliticsCompute(packet, prepared.implementationId)).generate(7)).toEqual(prepared.generate(7));
    await expect(preparePoliticsCompute(packet, 'different-build')).rejects.toThrow('POLITICS_IMPLEMENTATION_MISMATCH');
    const invalid = structuredClone(packet);
    invalid.keys.assemblyKey = 'stale';
    await expect(preparePoliticsCompute(invalid, prepared.implementationId)).rejects.toThrow('FROZEN_POLITICS_BINDING_MISMATCH');
    const broken = structuredClone(packet);
    broken.assets[0].bytes[0] ^= 1;
    await expect(preparePoliticsCompute(broken, prepared.implementationId)).rejects.toThrow('DEPENDENCY_CHECKSUM_MISMATCH');
    f.scenario.selection.scrutiny.id = ROSATELLUM;
    (f.scenario.selection.generatorsByLaw[ROSATELLUM] as { id: string }).id = 'M02';
    await expect(preparePoliticsExecution(f.scenario, f.readers)).rejects.toThrow('UNSUPPORTED_PAIR');
  }, 60000);
  it('rejects unsupported asset schemas and 25+ targets without changing the draft', async () => {
    const f = await commonPoliticsFixture();
    f.scenario.data.electorate.schemaVersion = 2;
    await expect(preparePoliticsExecution(f.scenario, f.readers)).rejects.toThrow('UNSUPPORTED_EXECUTION_ASSET_SCHEMA');
    f.scenario.data.electorate = await f.put('electorate', 'invalid-25-plus', { schemaVersion: 1, kind: 'politics-electorate',
      minimumAge: 25, targets: f.original.accepted.assets.electorates });
    const before = structuredClone(f.scenario);
    await expect(preparePoliticsExecution(f.scenario, f.readers)).rejects.toThrow('ELECTORATE_CONTRACT');
    expect(f.scenario).toEqual(before);
    const parameterPin = f.scenario.data.lawAssets[REFORM].models.M01[0];
    const parameters = JSON.parse(new TextDecoder().decode(f.bytes.get(parameterPin.id)!));
    parameters.models.schemaVersion = 2;
    f.scenario.data.lawAssets[REFORM].models.M01[0] = await f.put(parameterPin.id, parameterPin.version, parameters);
    await expect(preparePoliticsExecution(f.scenario, f.readers)).rejects.toThrow('MODEL_ASSET_CONTRACT');
  });
  it('creates every eligible law filing, including uni-only VdA and Senate TAA, without ordinary uni duplicates', async () => {
    const f = await commonPoliticsFixture(), data = await loadExecutionData(f.scenario, f.readers);
    const created = createList(f.scenario, { id: 'created', name: 'Created', color: '#123456', kind: 'list', coalitionId: null, minority: null },
      { ...f.scenario.newComponents[0], id: 'created-component' }, data.domain);
    const filings = created.lawSettings[ROSATELLUM].filings.find(f => f.listId === 'created')!.locations;
    const uni = filings.filter(f => f.level === 'uninominal');
    expect(uni.filter(l => l.chamber === 'camera')).toHaveLength(1);
    expect(uni.filter(l => l.chamber === 'senato')).toHaveLength(7);
    const ordinary = data.domain.locations.find(l => l.law === ROSATELLUM && l.level === 'uninominal' && !uni.some(u => u.chamber === l.chamber && u.code === l.code))!;
    f.scenario.lawSettings[ROSATELLUM].filings[0].locations.push(ordinary);
    await expect(preparePoliticsExecution(f.scenario, f.readers)).rejects.toThrow('INVALID_GEOGRAPHY');
  });
  it.each([[ROSATELLUM, 'M01'], [REFORM, 'M01'], [REFORM, 'M02']] as const)('supports either single renewal for %s/%s', async (law, model) => {
    const f = await commonPoliticsFixture(law, model);
    for (const ch of ['camera', 'senato'] as const) {
      f.scenario.renewal = ch;
      const prepared = await preparePoliticsExecution(f.scenario, f.readers);
      const generated = prepared.generate(7).request;
      expect(prepared.scenario.lawSettings).toEqual(f.scenario.lawSettings);
      if (generated.algorithmId === REFORM) {
        expect(generated.input.renewal.kind).toBe('single');
        if (generated.input.renewal.kind === 'single') expect(generated.input.renewal.chamber.chamber).toBe(ch);
        expect(generated.input.bonusLinks).toEqual([]);
      } else expect(generated.chambers.map(c => c.context.ramo)).toEqual([ch]);
    }
  }, 60000);
  it('assembles only eligible explicit/automatic links and validates an empty automatic counterpart separately', async () => {
    const f = await commonPoliticsFixture();
    const [a, b] = f.scenario.subjects.lists;
    f.scenario.subjects.coalitions = [{ id: 'C', name: 'C', color: '#123456', chambers: ['camera'] }];
    a.coalitionId = 'C'; b.coalitionId = null;
    f.scenario.lawSettings[REFORM].filings.find(r => r.listId === b.id)!.locations =
      f.scenario.lawSettings[REFORM].filings.find(r => r.listId === b.id)!.locations.filter(l => l.chamber === 'senato');
    const explicit = { camera: { kind: 'coalition' as const, id: 'C' }, senato: { kind: 'list' as const, id: b.id } };
    f.scenario.lawSettings[REFORM].bonusLinks = [explicit];
    const links = async () => {
      const prepared = await preparePoliticsExecution(f.scenario, f.readers), request = prepared.generate(7).request;
      if (request.algorithmId !== REFORM) throw new Error('Expected reform');
      return request.input.bonusLinks;
    };
    expect(await links()).toEqual([{ ...explicit, cameraDeclares: true, senatoDeclares: true }]);
    f.scenario.subjects.coalitions[0].chambers.push('senato');
    expect(await links()).toEqual([{ camera: explicit.camera, senato: explicit.camera, cameraDeclares: true, senatoDeclares: true }]);
    expect(f.scenario.lawSettings[REFORM].bonusLinks).toEqual([explicit]);
    f.scenario.subjects.coalitions[0].chambers = ['camera'];
    expect(await links()).toEqual([{ ...explicit, cameraDeclares: true, senatoDeclares: true }]);
    f.scenario.subjects.coalitions[0].chambers.push('senato');
    f.scenario.lawSettings[REFORM].filings.find(r => r.listId === a.id)!.locations =
      f.scenario.lawSettings[REFORM].filings.find(r => r.listId === a.id)!.locations.filter(l => l.chamber === 'camera');
    await expect(links()).rejects.toThrow('EMPTY_LINK_COUNTERPART');
    f.scenario.renewal = 'camera';
    expect(await links()).toEqual([]);
  }, 60000);
  it.each([ROSATELLUM, REFORM] as const)('keeps declared candidate identity and rejects incompatible active pins under %s', async law => {
    const f = await commonPoliticsFixture(law);
    const list = f.scenario.subjects.lists[0];
    const location = f.scenario.lawSettings[law].filings.find(r => r.listId === list.id)!.locations.find(l =>
      l.chamber === 'camera' && (law === REFORM ? f.original.accepted.context.seats.geography.colleges.some(c => c.id === l.code && c.kind === 'ordinary') : l.level === 'plurinominal'))!;
    f.scenario.candidates.push({ id: 'UNI_1_1', name: 'Declared person', birthDate: '1970-01-01', birthDateOrigin: 'declared', gender: 'female', availability: { kind: 'available' } });
    if (law === ROSATELLUM) f.scenario.lawSettings[ROSATELLUM].placements.push({ id: 'pin', candidateId: 'UNI_1_1',
      location, owner: { kind: 'list', id: list.id }, position: 1, affiliationListId: list.id, role: 'plurinominal', minority: false });
    else f.scenario.lawSettings[REFORM].placements.push({ id: 'pin', candidateId: 'UNI_1_1',
      location, owner: { kind: 'list', id: list.id }, position: 1, affiliationListId: list.id, role: 'ordinary' });
    const prepared = await preparePoliticsExecution(f.scenario, f.readers);
    const request = prepared.generate(7).request;
    if (request.algorithmId === REFORM) expect(request.input.candidates.find(c => c.id === 'UNI_1_1')).toMatchObject({ name: 'Declared person', birthDate: '1970-01-01', gender: 'female' });
    else {
      expect(request.chambers[0].input.candidati_pluri.filter(c => c.CANDIDATO === 'UNI_1_1')).toEqual([
        expect.objectContaining({ LISTA: list.id, NUMERO: 1, COLLEGIOPLURINOMINALE: location.code })]);
      expect(request.chambers.flatMap(c => c.input.candidati_uni).every(c => c.CANDIDATO !== 'UNI_1_1')).toBe(true);
    }
    f.scenario.candidates[0].availability = { kind: 'unavailable', reason: 'Test exclusion' };
    await expect(preparePoliticsExecution(f.scenario, f.readers)).rejects.toThrow('INVALID_CANDIDATURE');
    expect(prepared.generate(7).request).toEqual(request);
  }, 60000);
  it.each(['M01', 'M02'] as const)('excludes presentation, inactive-law and inactive-model edits from %s preparation/generation identities', async model => {
    const f = await commonPoliticsFixture(REFORM, model);
    const initial = await preparePoliticsExecution(f.scenario, f.readers);
    f.scenario.name = 'Renamed'; f.scenario.subjects.lists[0].name = 'Renamed list'; f.scenario.subjects.lists[0].color = '#ABCDEF';
    f.scenario.lawSettings[ROSATELLUM].candidateGeneration.uninominalToPlurinominalShare = 0.8;
    if (model === 'M01') f.scenario.modelSettings.M02.forecastScale = 9;
    else f.scenario.modelSettings.M01.supplementalPolls.push({ id: 'inactive', date: '2026-01-01', end: '2026-01-01', available: '2026-01-02', context: 'camera', sample: 1000, source: 'Inactive model', enabled: true, groups: [{ label: 'a', members: ['a'], share: 0.5 }] });
    const next = await preparePoliticsExecution(f.scenario, f.readers);
    expect(next.keys.preparationKey).toBe(initial.keys.preparationKey);
    expect(next.keys.generationKey).toBe(initial.keys.generationKey);
    expect(next.generateVotes(7)).toEqual(initial.generateVotes(7));
    f.scenario.lawSettings[REFORM].candidateGeneration.reuseProbability = 0.9;
    const candidates = await preparePoliticsExecution(f.scenario, f.readers);
    expect(candidates.keys.preparationKey).toBe(initial.keys.preparationKey);
    expect(candidates.keys.generationKey).toBe(initial.keys.generationKey);
    expect(candidates.keys.assemblyKey).not.toBe(next.keys.assemblyKey);
  }, 60000);
  it('preserves the restricted M01 capacity blocker while M02 generates only filed support', async () => {
    const f = await commonPoliticsFixture();
    const college = f.original.accepted.context.seats.geography.colleges.find(c => c.chamber === 'camera' && c.kind === 'ordinary')!;
    const ids = new Set(f.original.accepted.context.seats.geography.colleges.filter(c => c.chamber === 'camera' && c.territoryId === college.territoryId).map(c => c.id));
    const list = f.scenario.subjects.lists[1].id;
    f.scenario.lawSettings[REFORM].filings.find(r => r.listId === list)!.locations =
      f.scenario.lawSettings[REFORM].filings.find(r => r.listId === list)!.locations.filter(l => !ids.has(l.code));
    await expect(preparePoliticsExecution(f.scenario, f.readers)).rejects.toThrow('Reference votes conflict with filing/capacity camera/990001/A');
    f.scenario.selection.generatorsByLaw[REFORM].id = 'M02';
    const prepared = await preparePoliticsExecution(f.scenario, f.readers);
    for (const [id, counts] of prepared.generateVotes(7).counts) if (ids.has(id)) expect(counts.votes.get(list) ?? 0).toBe(0);
  }, 60000);
  it.each([[ROSATELLUM, 'M01'], [REFORM, 'M01'], [REFORM, 'M02']] as const)('preserves residual/new-list generation under %s/%s', async (law, model) => {
    const f = await commonPoliticsFixture(law, model), original = f.scenario.memberships[1];
    const residual = original.camera.pop()!; original.senato = [...original.camera];
    for (const [id, name, kind, color, members] of [
      ['__other__', 'Altre liste', 'other', '#9ca3af', [residual]],
      ['new-list', 'New list', 'list', '#123456', []],
    ] as const) {
      f.scenario.subjects.lists.push({ id, name, kind, color, coalitionId: null, minority: null });
      f.scenario.newComponents.push({ ...structuredClone(f.scenario.newComponents[0]), id: `fallback-${id}`, name });
      f.scenario.memberships.push({ listId: id, camera: [...members], senato: [...members], locked: true, newComponentId: `fallback-${id}` });
      for (const l of [ROSATELLUM, REFORM] as const) f.scenario.lawSettings[l].filings.push({ listId: id,
        locations: structuredClone(f.scenario.lawSettings[l].filings[0].locations) });
    }
    f.scenario.references.push({ id: 'new', scope: { kind: 'national' }, channel: 'common', subject: { kind: 'list', id: 'new-list' },
      mean: 0.1, denominator: 'valid-votes', appliesTo: ['M01', 'M02'], uncertainty: { sdPercentagePoints: 1, origin: 'explicit', policyVersion: null } });
    const prepared = await preparePoliticsExecution(f.scenario, f.readers), generated = prepared.generate(7);
    expect([...generated.votes.counts.values()].reduce((n, c) => n + (c.votes.get('new-list') ?? 0), 0)).toBeGreaterThan(0);
    if (generated.request.algorithmId === REFORM) {
      const input = generated.request.input, active = input.renewal.kind === 'joint' ? [input.renewal.camera, input.renewal.senato] : [input.renewal.chamber];
      expect(active.flatMap(c => c.ordinarySlates).some(s => s.listId === '__other__')).toBe(false);
    } else expect(generated.request.chambers.flatMap(c => c.input.candidati_pluri).some(c => c.LISTA === '__other__')).toBe(false);
  }, 60000);
});
