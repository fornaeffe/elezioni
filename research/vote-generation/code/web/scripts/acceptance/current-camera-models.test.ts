import { describe, it, expect } from 'vitest';
import { reducedShareInput } from '../../tests/fixtures/fast-share-input';
import { generationFixture } from '../../src/lib/politics/reform-2026/generation.fixture';
import { design, restoreOriginalStrata, prepareCameraM01, prepareCameraM02, cameraM02Reference, generateCameraDraw, type ResearchInput } from './current-camera-models';
import { forecastRandom } from '../../src/lib/politics/reform-2026/vote-share/forecast-random';
import type { ReplaySource } from './fast-share-replay';
import type { ReleasedScenario } from '../../src/lib/politics/reform-2026/vote-share/release';
import type { PhysicalCell } from '../../src/lib/politics/reform-2026/vote-share/forecast-types';

const input = reducedShareInput(), rename = (id: string) => id === 'd' ? 'research-other' : id;
const source: ReplaySource = {
  components: input.national.components.map(rename), cities: ['n1', 'n2', 's1'], cutoff: input.national.referenceDate,
  targetDate: '2016-05-01', targetKind: 'camera',
  nationalHistory: input.national.elections.map(({ context, ...e }) => ({ ...e, kind: context, groups: e.groups.map(g => g.map(rename)) })),
  elections: input.profiles.elections.map(({ context, ...e }) => ({ ...e, kind: context, rows: e.rows.map(r => ({ ...r, groups: r.groups.map(g => g.map(rename)) })) })),
  polls: input.national.polls.map(({ context, availableDate, fieldworkEnd, errorGroup, ...p }) => ({ ...p,
    kind: context, available: availableDate, end: fieldworkEnd, group: errorGroup, groups: p.groups.map(g => g.map(rename)) })),
  truth: ['n1', 'n2', 's1'].map(id => ({ id, region: id === 's1' ? 'south' : 'north', electors: 1000, nonValid: 200,
    groups: [['a'], ['b'], ['c'], ['research-other']], counts: [200, 200, 200, 200] })),
  nationalTruth: { id: 'held-out', date: '2016-05-01', kind: 'camera', electors: 3000,
    groups: [['a'], ['b'], ['c'], ['research-other']], counts: [600, 600, 600, 600, 600] },
};
const { snapshot: context, scenario: original } = generationFixture();
const seed = 'camera-adapter-tests', d = design(source), prepared = prepareCameraM02(source, d.targets, seed),
  cells: PhysicalCell[] = d.targets.flatMap(t => [400, 600].map((electors, i) => ({ id: t.id + ':' + i,
    municipality: t.id, region: t.region, province: '', collegeId: 'ordinary', chamber: 'camera' as const, electors })));
const research = {
  history: { events: [{ id: 'camera-2014', date: '2014-01-01', kind: 'camera', municipalities: d.targets.map(t => ({
    id: t.id, regionId: t.region, matchable: true, electors: 1000, validVotes: 800,
    lists: d.components.map(name => ({ name, votes: 200 })),
  })) }] },
  policy: { eventOverrides: [], joints: [], aliases: d.components.map(id => ({ labels: [id], id, sources: [] })) },
} as unknown as ResearchInput;
function legacy() {
  return prepareCameraM01(d, research, original as unknown as ReleasedScenario, context, cells, ['ordinary'], seed);
}
const profile = context.models.chambers.profiles.find(p => p.id === original.chamberProfileId)!,
  rho = profile.sharedVariance + (1 - profile.sharedVariance) * profile.residualCorrelation;

describe('current Camera-only comparison boundary', () => {
  it('retains different original choice sets without inventing offered zeros', () => {
    const raw = structuredClone(research), event = raw.history.events[0];
    event.municipalities = event.municipalities.slice(0, 2);
    event.municipalities[0].lists = [{ name: 'a', votes: 800 }];
    event.municipalities[1].lists = [{ name: 'b', votes: 800 }];
    const restored = restoreOriginalStrata({ components: ['a', 'b', 'research-other'], cutoff: '2016-01-01',
      nationalHistory: [{ id: 'camera-2014', date: '2014-01-01', kind: 'camera' as const, electors: 2000,
        groups: [['a'], ['b']], counts: [400, 800, 800] }] }, raw);
    expect(restored.nationalHistory[0].strata).toEqual([
      { groups: [['a']], counts: [200, 800], electors: 1000 },
      { groups: [['b']], counts: [200, 800], electors: 1000 },
    ]);
  });
  it('keeps held-out votes outside the preparation design', () => {
    const changed = structuredClone(source);
    changed.truth.forEach(t => { t.nonValid = 900; t.counts = [0, 0, 0, 100]; });
    changed.nationalTruth.counts.reverse();
    expect(design(changed)).toEqual(d);
    expect(Object.keys(d.targets[0]).sort()).toEqual(['electors', 'id', 'offered', 'region']);
  });
  it('calibrates unrestricted M02 references before any filing restriction', () => {
    const draw = 3, reference = cameraM02Reference(prepared, cells, seed, draw, rho),
      row = forecastRandom(seed, draw, 'posterior-political-row', 'camera', rho).integer(prepared.fit.political.chains * prepared.fit.political.drawsPerChain),
      row0 = forecastRandom(seed, draw, 'posterior-non-valid-row', 'camera', rho).integer(prepared.fit.participation.chains * prepared.fit.participation.drawsPerChain),
      a = prepared.fit.participation.values[row0 * 4], k = d.components.length;
    const actual = Array.from({ length: k + 1 }, (_, j) => reference.reduce((s, r, i) => s + cells[i].electors * r[j] / 3000, 0));
    expect(actual[0]).toBeCloseTo(a, 8);
    for (let j = 0; j < k; j++) expect(actual[j + 1]).toBeCloseTo((1 - a) * prepared.fit.political.values[row * 2 * k + j], 8);
  });
  for (const model of ['M01', 'M02'] as const) {
    it(model + ' conserves physical/municipal electorates and produces deterministic draws', () => {
      const l = legacy(), anchors = l.setPolls(source.polls);
      const first = generateCameraDraw(model, d, cells, seed, 2, context, prepared, l, anchors);
      expect(generateCameraDraw(model, d, cells, seed, 2, context, prepared, l, anchors)).toEqual(first);
      d.targets.forEach((t, i) => expect([...first.subarray(i * 5, (i + 1) * 5)].reduce((a, b) => a + b, 0)).toBe(t.electors));
    });
    it(model + ' enforces filing only after reference and future movement', () => {
      const restricted = structuredClone(d); restricted.targets.forEach(t => t.offered = ['a', 'b']);
      const l = legacy(), anchors = l.setPolls(source.polls);
      const draw = generateCameraDraw(model, restricted, cells, seed, 2, context, prepared, l, anchors);
      const ids = prepared.fit.componentIds;
      restricted.targets.forEach((t, i) => ids.forEach((id, j) => {
        if (!t.offered.includes(id)) expect(draw[i * 5 + j + 1]).toBe(0);
      }));
    });
  }
});
