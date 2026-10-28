import { readFileSync } from 'node:fs';
import { describe, expect, it } from 'vitest';
import { createOtherList, OTHER_LIST_ID, OTHER_LIST_NAME, setOtherListEnabled, validateOtherScenarioLists } from './other-list';
import { createDefaultPoliticsScenario, parseScenario, serializeScenario } from '../scenario/politics';
import { createDefaultRegionalErScenario, parseRegionalErScenario, serializeRegionalErScenario } from '../scenario/regional-er';
import { runEarlyPoliticsScrutiny, runPoliticsScrutinyTrace } from '../politics/scrutiny';
import { buildPoliticsDirectScrutinySnapshot } from '../politics/pipeline';
import type { PoliticsPipelineFixture, Ramo } from '../politics/types';
import { runRegionalErScrutiny } from '../regional-er/scrutiny';
import { generationFixture } from '../politics/reform-2026/generation.fixture';
import { generateReformSimulation, prepareReformGeneration } from '../politics/reform-2026/generation';
import { setReformOtherListEnabled, setReformReference, setReformCoverage, reformSubjects } from '../politics/reform-2026/scenario-editor';
import { parseReformScenario, serializeReformScenario, validateReformScenario } from '../politics/reform-2026/generation-scenario';
import { validateReformScrutinyInput } from '../politics/reform-2026/validation';
import { runPoliticsReformScrutiny } from '../politics/reform-2026/scrutiny';
import { createSeededDrawContext } from '../politics/reform-2026/ties';
import { reformRunSeries } from '../politics/reform-2026/result-series';
import { attributeSpecialVotes } from '../politics/reform-2026/special-attribution';
import { createSimulationResultExport, parseSimulationResultExport } from './result-export';
import type { SimulationResult } from './types';

describe('unique Other category and persistence', () => {
  it.each([
    [createDefaultPoliticsScenario, serializeScenario, parseScenario],
    [createDefaultRegionalErScenario, serializeRegionalErScenario, parseRegionalErScenario]
  ])('round trips a unique fixed category with its own references', (defaults, serialize, parse) => {
    let s = setOtherListEnabled(defaults(), true);
    s = setOtherListEnabled(s, true);
    expect(s.lists.filter(l => l.kind === 'other')).toEqual([createOtherList()]);
    s.lists.at(-1)!.startingShare = 42; s.lists.at(-1)!.shareOverride = true;
    s.localShareOverrides.push({ id: 'other-local', scope: 'region', locationCode: '08', list: OTHER_LIST_NAME, startingShare: 50 });
    const restored = parse(serialize(s));
    expect(restored).toEqual(s);
    const disabled = setOtherListEnabled(restored, false);
    expect(disabled.lists.some(l => l.kind === 'other')).toBe(false);
    expect(disabled.localShareOverrides).not.toContainEqual(s.localShareOverrides.at(-1));
    expect(disabled.lists).toEqual(defaults().lists);
  });
  it('rejects duplicate categories, changed identity/color and supplied candidates', () => {
    const s = setOtherListEnabled(createDefaultPoliticsScenario(), true);
    s.lists.push({ ...createOtherList(), id: 'duplicate' });
    expect(validateOtherScenarioLists(s).length).toBeGreaterThan(0);
    s.lists.pop(); s.lists.at(-1)!.color = '#ffffff';
    expect(() => parseScenario(serializeScenario(s))).toThrow();
  });
  it('preserves Other in a complete result archive', () => {
    const scenario = setOtherListEnabled(createDefaultPoliticsScenario(), true);
    const result: SimulationResult = { type: 'result', status: 'completed', warnings: [],
      benchmark: { startedAt: '2026-10-28T00:00:00Z', elapsedMs: 1, simulations: 1, dataVersion: 'constructed' },
      tables: [{ name: 'Other', columns: ['List', 'Seats'], rows: [{ List: OTHER_LIST_NAME, Seats: 0 }] }] };
    const exported = createSimulationResultExport({ scenario, result, exportedAt: result.benchmark.startedAt });
    expect(parseSimulationResultExport(JSON.stringify(exported)).scenario).toEqual(scenario);
  });
});

describe('legacy politics Other', () => {
  const fixture: PoliticsPipelineFixture = JSON.parse(readFileSync(new URL('../../../../test/fixtures/politiche/pipeline.json', import.meta.url), 'utf8'));
  it.each(['camera', 'senato'] as const)('retains candidate-free votes and excludes every admission route in %s', (ramo: Ramo) => {
    const r = structuredClone(fixture.expected.rami[ramo]);
    const input = r.simulations[0].input;
    const colleges = [...new Map(input.liste_uni.map(l => [String(l.COLLEGIOUNINOMINALE), l])).values()];
    for (const l of colleges) input.liste_uni.push({ ...l, LISTA: OTHER_LIST_NAME, CANDIDATO: null, MINORANZA: false, CAND_MINORANZA: false, VOTI_LISTA: 10000 });
    const early = runEarlyPoliticsScrutiny(input);
    expect(early.liste_uni_cifre.filter(l => l.LISTA === OTHER_LIST_NAME).map(l => l.CIFRA)).toEqual(colleges.map(() => 10000));
    expect(early.candidati_uni_elezione.some(c => c.CANDIDATO === null)).toBe(false);
    const context = { ramo, liste_naz: [...r.liste_naz, { LISTA: OTHER_LIST_NAME, COALIZIONE: null, MINORANZA: false, kind: 'other' as const }], totali_pluri: r.totali_pluri, totale_seggi: r.totali_pluri.reduce((n, t) => n + t.SEGGI, 0) + colleges.length };
    // At this overwhelming share all genuine lists may fall below admission.
    expect(() => runPoliticsScrutinyTrace(input, context)).toThrow(/No eligible/);
    input.liste_uni.filter(l => l.LISTA === OTHER_LIST_NAME).forEach(l => { l.VOTI_LISTA = 100; });
    const trace = runPoliticsScrutinyTrace(input, context);
    const other = trace.liste_naz_soglie.find(l => l.LISTA === OTHER_LIST_NAME)!;
    expect(other.CIFRA).toBe(colleges.length * 100);
    expect([other.SOGLIA1M, other.SOGLIA3M, other.SOGLIA_SOLA]).toEqual([false, false, false]);
  });
  it('the generation adapter retains Other without inventing template candidates', () => {
    const s = structuredClone(fixture.input);
    s.liste.push({ ...s.liste[0], LISTA: OTHER_LIST_NAME, COALIZIONE: null, kind: 'other' });
    s.comuni_liste.push(...s.comuni_liste.filter(l => l.LISTA === s.liste[0].LISTA).map(l => ({ ...l, LISTA: OTHER_LIST_NAME })));
    const result = buildPoliticsDirectScrutinySnapshot(s, { seed: 'other-adapter' });
    for (const ch of ['camera', 'senato'] as const) {
      const input = result.rami[ch].simulations[0].input;
      expect(input.liste_uni.filter(l => l.LISTA === OTHER_LIST_NAME).length).toBeGreaterThan(0);
      expect(input.liste_uni.filter(l => l.LISTA === OTHER_LIST_NAME).every(l => l.CANDIDATO === null)).toBe(true);
      expect(input.candidati_pluri.some(c => c.LISTA === OTHER_LIST_NAME)).toBe(false);
    }
  });
});

describe('regional Other', () => {
  const metadata = ['A', 'B', OTHER_LIST_NAME].map((LISTA, i) => ({ LISTA, COALIZIONE: i === 2 ? null : LISTA, DATA: '2026-01-01', LOGIT_P: 0, SIGMA_GLOBAL: 0, PERCENTUALE: 0, ...(i === 2 ? { kind: 'other' as const } : {}) }));
  const population = [{ CODICE_COMUNE: '1', COMUNE: 'Test', CODICE_PROVINCIA: '1', PROVINCIA: 'Test', CODICE_REGIONE: '08', REGIONE: 'Emilia-Romagna', POPOLAZIONE: 10000 }];
  it.each([1, 30, 80])('never awards seats or a presidential place at %s%%', share => {
    const counts = [(100 - share) * 60, (100 - share) * 40, share * 100];
    const input = { comuni_liste: metadata.map((l, i) => ({ CODICE_COMUNE: '1', COMUNE: 'Test', CODICE_PROVINCIA: '1', PROVINCIA: 'Test', LISTA: l.LISTA, VOTI_LISTA_SIM: counts[i] })) };
    const out = runRegionalErScrutiny(input, { liste: metadata, pop_legale: population }, { seed: 'other-regional' });
    expect(out.liste.find(l => l.LISTA === OTHER_LIST_NAME)).toMatchObject({ ELETTI: 0, PERCENTUALE: share / 100 });
    expect(out.prov_lista.find(l => l.LISTA === OTHER_LIST_NAME)).toMatchObject({ ELETTI: 0, PERCENTUALE: share / 100 });
    expect(out.coalizioni.map(c => c.COALIZIONE).sort()).toEqual(['A', 'B']);
    expect(out.coalizioni.find(c => c.COALIZIONE === 'A')).toMatchObject({ PRESIDENTE: true });
    expect(out.liste.reduce((n, l) => n + l.ELETTI, 0)).toBe(48);
  });
  it('does not merge Other into a standalone contender with null coalition metadata', () => {
    const liste = metadata.map(l => l.LISTA === 'A' ? { ...l, COALIZIONE: null } : l);
    const comuni_liste = liste.map((l, i) => ({ CODICE_COMUNE: '1', COMUNE: 'Test', CODICE_PROVINCIA: '1', PROVINCIA: 'Test', LISTA: l.LISTA, VOTI_LISTA_SIM: [1200, 800, 8000][i] }));
    const out = runRegionalErScrutiny({ comuni_liste }, { liste, pop_legale: population }, { seed: 'other-null-coalition' });
    expect(out.coalizioni.reduce((n, c) => n + c.VOTI_LISTA_ITER, 0)).toBe(2000);
    expect(out.coalizioni.reduce((n, c) => n + c.PERCENTUALE, 0)).toBeCloseTo(.2);
    expect(out.liste.find(l => l.LISTA === OTHER_LIST_NAME)!.ELETTI).toBe(0);
  });
  it('fails explicitly when only Other has votes', () => {
    const comuni_liste = metadata.map((l, i) => ({ CODICE_COMUNE: '1', COMUNE: 'Test', CODICE_PROVINCIA: '1', PROVINCIA: 'Test', LISTA: l.LISTA, VOTI_LISTA_SIM: i === 2 ? 10000 : 0 }));
    expect(() => runRegionalErScrutiny({ comuni_liste }, { liste: metadata, pop_legale: population })).toThrow(/No eligible/);
  });
});

describe('reform Other through generation and scrutiny', () => {
  it.each([undefined, 'reference-proportional-v1'].flatMap(policy => [1, 55, 100].map(share => [policy as undefined | 'reference-proportional-v1', share] as const)))('is candidate-free under policy %s at %s%%, with special ballots counted once', (policy, share) => {
    const { snapshot, scenario } = generationFixture();
    let s = setReformOtherListEnabled(scenario, true);
    s.candidateGeneration.bonusPolicy = policy;
    s = setReformReference(s, { kind: 'national' }, OTHER_LIST_ID, 'common', share);
    expect(validateReformScenario(s, snapshot)).toEqual([]);
    expect(parseReformScenario(serializeReformScenario(s))).toEqual(s);
    expect(reformSubjects(s, 'camera').some(x => x.id === OTHER_LIST_ID)).toBe(false);
    const prepared = prepareReformGeneration(s, snapshot);
    const { input } = generateReformSimulation(prepared, 1);
    expect(generateReformSimulation(prepared, 1).input).toEqual(input);
    expect(validateReformScrutinyInput(input).valid).toBe(true);
    const chambers = input.renewal.kind === 'joint' ? [input.renewal.camera, input.renewal.senato] : [input.renewal.chamber];
    for (const c of chambers) {
      expect(c.ordinarySlates.some(x => x.listId === OTHER_LIST_ID)).toBe(false);
      expect(c.bonusSlates.some(x => x.subject.id === OTHER_LIST_ID || x.candidates.some(p => p.affiliationListId === OTHER_LIST_ID))).toBe(false);
      expect(c.specialCandidatures.some(x => x.linkedListIds.includes(OTHER_LIST_ID))).toBe(false);
      for (const row of c.specialVotes) {
        const a = attributeSpecialVotes({ chamber: c, collegeId: row.collegeId, simulationId: 1 }, createSeededDrawContext(input.randomness.seed, input.randomness.streamVersion));
        expect(a.status).toBe('complete');
        if (a.status === 'complete') expect(a.validVotes).toBe(a.listVotes.reduce((n, l) => n + l.votes, 0));
      }
    }
    const result = runPoliticsReformScrutiny(input);
    for (const c of result.chambers) {
      expect(c.admission?.status).toBe('complete');
      if (c.admission?.status === 'complete') expect(c.admission.lists.find(l => l.listId === OTHER_LIST_ID)).toMatchObject({ admitted: false, reasons: ['other-list-excluded'] });
      const source = chambers.find(s => s.chamber === c.chamber)!;
      const directTotal = [...source.ordinaryVotes.filter(r => source.colleges.find(c => c.id === r.collegeId)?.kind === 'ordinary'), ...source.specialVotes].reduce((n, row) => n + (row.votes.kind === 'attributed' ? row.votes.listVotes : row.votes.directListVotes).reduce((n, l) => n + l.votes, 0), 0);
      expect(c.figures?.national.validVotes).toBe(directTotal);
    }
    expect(result.seats.some(s => s.entitlement.subject.id === OTHER_LIST_ID)).toBe(false);
    const rows = reformRunSeries(input, result, s, snapshot).filter(r => r.Owner === `list:${OTHER_LIST_ID}`);
    expect(rows.some(r => r.Kind === 'college')).toBe(true);
    expect(rows.every(r => r.Elected === 0 && (r.Kind !== 'college' || r.Entitled === 0))).toBe(true);
  });
  it('rejects candidate/coalition links and supports geographic absence', () => {
    const { snapshot, scenario } = generationFixture();
    let s = setReformOtherListEnabled(scenario, true);
    const college = snapshot.seats.geography.colleges.find(c => c.chamber === 'camera' && c.kind === 'ordinary')!;
    s = setReformCoverage(s, 'camera', [college.id], OTHER_LIST_ID, false);
    expect(s.coverage.find(c => c.chamber === 'camera' && c.collegeId === college.id)?.listIds).not.toContain(OTHER_LIST_ID);
    s.lists.camera = s.lists.camera.map(l => l.id === OTHER_LIST_ID ? { ...l, coalitionId: 'bad' } : l);
    expect(validateReformScenario(s).length).toBeGreaterThan(0);
  });
  it('uses historical correspondences without a supplied reference share', () => {
    const { snapshot, scenario } = generationFixture();
    const s = setReformOtherListEnabled(scenario, true);
    s.correspondences = s.correspondences.map(r => r.pastListId === 'A' ? { ...r, futureListId: OTHER_LIST_ID } : r);
    expect(s.shares).toEqual([]);
    const input = generateReformSimulation(prepareReformGeneration(s, snapshot), 1).input;
    const out = runPoliticsReformScrutiny(input);
    for (const c of out.chambers) expect(c.figures!.national.lists.find(l => l.listId === OTHER_LIST_ID)!.votes).toBeGreaterThan(0);
  });
  it('rejects every candidate and bonus association in direct reform inputs', () => {
    const { snapshot, scenario } = generationFixture();
    const s = setReformOtherListEnabled(scenario, true);
    const valid = generateReformSimulation(prepareReformGeneration(s, snapshot), 1).input;
    for (const association of ['ordinary', 'special', 'bonus-affiliation', 'bonus-subject', 'bonus-link'] as const) {
      const input = structuredClone(valid);
      if (input.renewal.kind !== 'joint') throw new Error();
      const c = input.renewal.camera;
      const changed = association === 'ordinary' ? { ...c, ordinarySlates: c.ordinarySlates.map((slate, i) => i === 0 ? { ...slate, listId: OTHER_LIST_ID } : slate) }
        : association === 'special' ? { ...c, specialCandidatures: c.specialCandidatures.map((x, i) => i === 0 ? { ...x, linkedListIds: [...x.linkedListIds, OTHER_LIST_ID] } : x) }
        : association === 'bonus-affiliation' ? { ...c, bonusSlates: c.bonusSlates.map((x, i) => i === 0 ? { ...x, candidates: x.candidates.map((p, j) => j === 0 ? { ...p, affiliationListId: OTHER_LIST_ID } : p) } : x) }
        : association === 'bonus-subject' ? { ...c, bonusSlates: c.bonusSlates.map((x, i) => i === 0 ? { ...x, subject: { kind: 'list' as const, id: OTHER_LIST_ID } } : x) } : c;
      const mutated = { ...input, renewal: { ...input.renewal, camera: changed }, bonusLinks: association === 'bonus-link' ? [{ camera: { kind: 'list' as const, id: OTHER_LIST_ID }, senato: { kind: 'list' as const, id: OTHER_LIST_ID }, cameraDeclares: true, senatoDeclares: true }] : input.bonusLinks };
      expect(validateReformScrutinyInput(mutated).valid, association).toBe(false);
    }
  });
});
