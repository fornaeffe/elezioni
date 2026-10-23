import { describe, expect, it } from 'vitest';
import { fileURLToPath } from 'node:url';
import { prepareSelectedPolls } from './polls';
import { createPollInformedScenario } from './poll-references';
import { estimatePollReference } from '../../src/lib/polls/forecast';
import { generationFixture } from '../../src/lib/politics/reform-2026/generation.fixture';
import { preparePairedVoteReferences, generatePairedMunicipalVotes } from '../../src/lib/politics/reform-2026/paired-vote-generation';
import { parseReformScenario, serializeReformScenario } from '../../src/lib/politics/reform-2026/generation-scenario';
import { upgradeReformDraft } from '../../src/lib/politics/reform-2026/presentation-metadata';
const root = fileURLToPath(new URL('../../../', import.meta.url));
const { bundle } = await prepareSelectedPolls(root);
const context = { lists: [{ id: 'pd', name: 'PD', baselineShare: .5 }, { id: 'fdi', name: "Fratelli d'Italia", baselineShare: .5 }], historicalLists: [], correspondences: [] };
function fixture() {
  const { scenario, snapshot } = generationFixture();
  for (const ch of ['camera', 'senato'] as const) { scenario.lists[ch] = scenario.lists[ch].map((l, i) => ({ ...l, name: i === 0 ? 'PD' : "Fratelli d'Italia" })); }
  snapshot.historicalLists[0].name = 'PD'; snapshot.historicalLists[1].name = "Fratelli d'Italia";
  return { scenario, snapshot };
}
describe('poll-informed references', () => {
  it('selects dated generic questions, caps repeated houses and retains estimated sample provenance', () => {
    const r = estimatePollReference(bundle.questions, bundle.entityCatalog, context, '2026-10-23');
    expect(r.evidence.map(e => e.questionId).sort()).toEqual(['pcm-14346-q1', 'pcm-14358-q4', 'pcm-14362-q1', 'pcm-14364-q1']);
    expect(r.evidence.every(e => e.sampleEstimated && e.effectiveSample <= 600)).toBe(true);
    expect(r.rows.reduce((n, p) => n + p.share, 0)).toBeCloseTo(1);
    expect(r.rows.every(p => p.sigma > 0)).toBe(true);
    const past = estimatePollReference(bundle.questions, bundle.entityCatalog, context, '2026-10-10');
    expect(past.evidence.map(e => e.questionId).sort()).toEqual(['pcm-14342-q1', 'pcm-14358-q4']);
    expect(estimatePollReference(bundle.questions, bundle.entityCatalog, context, '2027-05-01').applied).toBe(false);
  });
  it('selects exactly one explicit conditional alternative per house', () => {
    const r = estimatePollReference(bundle.questions, bundle.entityCatalog, context, '2026-10-23', 'centrosinistra-conte');
    expect(r.evidence.filter(e => e.pollster === 'SWG S.p.A.').map(e => e.questionId)).toEqual(['pcm-14363-q2']);
  });
  it('round-trips versioned polling without changing legacy scenarios or special units', () => {
    const { scenario, snapshot } = fixture(), original = structuredClone(scenario);
    const result = createPollInformedScenario(scenario, snapshot, bundle, '2026-10-23');
    expect(scenario).toEqual(original);
    expect(result.scenario.schemaVersion).toBe(3);
    expect(parseReformScenario(serializeReformScenario(result.scenario), snapshot)).toEqual(result.scenario);
    expect(upgradeReformDraft(result.scenario).schemaVersion).toBe(3);
    const old = preparePairedVoteReferences(scenario, snapshot), next = preparePairedVoteReferences(result.scenario, snapshot);
    expect(next.units.map(u => u.camera.abstention)).toEqual(old.units.map(u => u.camera.abstention));
    const special = next.units.find(u => u.municipality.regionId === '02')!;
    expect(special.camera).toEqual(old.units.find(u => u.municipality.code === special.municipality.code)!.camera);
    expect(result).toEqual(createPollInformedScenario(scenario, snapshot, bundle, '2026-10-23'));
  });
  it('honours national/chamber and local user references after polling', () => {
    const { scenario, snapshot } = fixture();
    scenario.shares = [{ scope: { kind: 'national' }, listId: 'A', common: null, camera: .7, senato: null },
      { scope: { kind: 'region', code: '03' }, listId: 'A', common: null, camera: .8, senato: null }];
    const { scenario: s } = createPollInformedScenario(scenario, snapshot, bundle, '2026-10-23');
    const { units } = preparePairedVoteReferences(s, snapshot);
    const local = units.find(u => u.municipality.regionId === '03')!;
    expect(local.camera.shares.get('A')).toBeCloseTo(.8);
    expect(local.camera.measurement?.has('A')).toBe(false);
    expect(local.senato.measurement?.has('A')).toBe(true);
  });
  it('retains measurement uncertainty at zero horizon and replays by simulation ID', () => {
    const { scenario, snapshot } = fixture();
    scenario.electionDate = '2026-10-23'; snapshot.models.chambers.sigmaPerSqrtDay = { national: 0, region: 0, local: 0 };
    const { scenario: s } = createPollInformedScenario(scenario, snapshot, bundle, '2026-10-23');
    const p = preparePairedVoteReferences(s, snapshot);
    const a = generatePairedMunicipalVotes(s, snapshot, p.units, 1), b = generatePairedMunicipalVotes(s, snapshot, p.units, 2);
    expect(a).toEqual(generatePairedMunicipalVotes(s, snapshot, p.units, 1)); expect(a).not.toEqual(b);
    for (const row of a) expect(row.votes.reduce((n, v) => n + v.votes, row.abstention)).toBe(row.electors);
  });
  it('falls back without polling and rejects invalid provenance', () => {
    const { scenario, snapshot } = fixture();
    expect(createPollInformedScenario(scenario, snapshot, { ...bundle, questions: [] }).scenario).toEqual(scenario);
    const { scenario: s } = createPollInformedScenario(scenario, snapshot, bundle);
    s.pollReferences!.date = '2026-10-01';
    expect(() => serializeReformScenario(s)).toThrow();
  });
  it('fills a new party territorial gap while preserving historical turnout', () => {
    const { scenario, snapshot } = fixture();
    for (const ch of ['camera', 'senato'] as const) scenario.lists[ch].push({ id: 'fn', name: 'Futuro Nazionale', coalitionId: null, protectedMinority: null });
    for (const c of scenario.coverage) if (snapshot.seats.geography.colleges.find(g => g.id === c.collegeId)?.kind === 'ordinary') c.listIds.push('fn');
    delete scenario.presentation; scenario.schemaVersion = 1;
    const { scenario: s } = createPollInformedScenario(scenario, snapshot, bundle, '2026-10-23');
    const p = preparePairedVoteReferences(s, snapshot);
    const u = p.units.find(u => u.municipality.regionId === '03')!;
    expect(u.camera.shares.get('fn')).toBeGreaterThan(0);
    expect(u.camera.abstention).toBeCloseTo(.4);
    expect(p.trace.warnings.some(w => w.code === 'POLL_TERRITORIAL_PROXY')).toBe(true);
  });
  it('keeps measurement errors shared between chambers over repeated draws', () => {
    const { scenario, snapshot } = fixture();
    scenario.electionDate = '2026-10-23'; snapshot.models.chambers.sigmaPerSqrtDay = { national: 0, region: 0, local: 0 };
    const { scenario: s } = createPollInformedScenario(scenario, snapshot, bundle, '2026-10-23');
    const p = preparePairedVoteReferences(s, snapshot);
    const draws = Array.from({ length: 100 }, (_, i) => generatePairedMunicipalVotes(s, snapshot, p.units, i + 1));
    const values = (ch: string) => draws.map(d => {
      const rs = d.filter(r => r.chamber === ch && r.municipalityCode === '990003');
      return rs.reduce((n, r) => n + r.votes.find(v => v.listId === 'A')!.votes, 0);
    });
    const c = values('camera'), t = values('senato');
    const mean = (xs: number[]) => xs.reduce((n, x) => n + x, 0) / xs.length;
    const a = mean(c), b = mean(t), covariance = c.reduce((n, x, i) => n + (x - a) * (t[i] - b), 0);
    const vc = c.reduce((n, x) => n + (x - a) ** 2, 0), vt = t.reduce((n, x) => n + (x - b) ** 2, 0);
    expect(vc).toBeGreaterThan(1000); expect(covariance / Math.sqrt(vc * vt)).toBeGreaterThan(.99);
  });
});
