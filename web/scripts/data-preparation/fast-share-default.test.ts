import { describe, it, expect } from 'vitest';
import { originalShareFixture } from '../../tests/fixtures/fast-share-source-input';
import {
  encodeSource,
  canonicalJson,
} from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import type { OriginalSource } from '../../src/lib/politics/reform-2026/vote-share/source-types';
import {
  defaultSharePlan,
  type DefaultSharePolicy,
} from './fast-share-default';
import policyData from '../../../data/fast-share-default-policy.json' with { type: 'json' };
async function fixture() {
  const base = originalShareFixture(),
    original: OriginalSource = {
      ...base,
      polls: [],
      units: [
        {
          id: 'A',
          name: 'Special town',
          region: '04',
          province: '001',
          targetCode: '001',
          match: 'same-place',
        },
        {
          id: 'B',
          name: 'Ordinary town',
          region: '01',
          province: '002',
          targetCode: '002',
          match: 'same-place',
        },
      ],
      choices: [
        {
          id: 'ca',
          eventId: 'camera-2022',
          label: 'MAIN A',
          members: ['a'],
          method: 'Observed fixture',
          citations: ['fixture-source'],
          opaque: true,
        },
        {
          id: 'cl',
          eventId: 'camera-2022',
          label: 'LOCAL PARTNERS',
          members: ['minor-a', 'minor-b'],
          method: 'Observed joint fixture',
          citations: ['fixture-source'],
          opaque: false,
        },
        {
          id: 'sa',
          eventId: 'senato-2022',
          label: 'MAIN A',
          members: ['a'],
          method: 'Observed fixture',
          citations: ['fixture-source'],
          opaque: true,
        },
        {
          id: 'sl',
          eventId: 'senato-2022',
          label: 'REGIONAL GROUP',
          members: ['special-local'],
          method: 'Opaque special fixture',
          citations: ['fixture-source'],
          opaque: true,
        },
      ],
      events: [
        {
          ...base.events[0],
          id: 'camera-2022',
          date: '2022-09-25',
          availableDate: '2022-09-26',
          context: 'camera',
          rows: [
            {
              unit: 'A',
              electors: 1000,
              voters: 900,
              choices: [
                { id: 'ca', count: 600, direct: 600 },
                { id: 'cl', count: 50, direct: 50 },
              ],
            },
            {
              unit: 'B',
              electors: 100,
              voters: 80,
              choices: [{ id: 'ca', count: 60, direct: 60 }],
            },
          ],
        },
        {
          ...base.events[0],
          id: 'senato-2022',
          date: '2022-09-25',
          availableDate: '2022-09-26',
          context: 'senato',
          role: 'paired-adaptation',
          rows: [
            {
              unit: 'A',
              electors: 1000,
              voters: 900,
              choices: [
                { id: 'sa', count: 620, direct: 620 },
                { id: 'sl', count: 40, direct: 40 },
              ],
            },
            {
              unit: 'B',
              electors: 100,
              voters: 80,
              choices: [{ id: 'sa', count: 60, direct: 60 }],
            },
          ],
        },
      ],
      geography: {
        version: 'fixture',
        censusDate: '2021-12-31',
        targets: [
          {
            code: '001',
            name: 'Special town',
            region: '04',
            province: '001',
            population: 1200,
          },
          {
            code: '002',
            name: 'Ordinary town',
            region: '01',
            province: '002',
            population: 200,
          },
        ],
        colleges: ['camera', 'senato'].flatMap((chamber) => [
          {
            id: chamber + '-ordinary',
            chamber: chamber as 'camera' | 'senato',
            kind: 'ordinary',
            territoryId: chamber + '-01',
          },
          {
            id: chamber + '-special',
            chamber: chamber as 'camera' | 'senato',
            kind: 'special-uninominal',
            territoryId: chamber + '-04',
          },
        ]),
        cells: ['camera', 'senato'].flatMap((chamber) => [
          {
            chamber: chamber as 'camera' | 'senato',
            collegeId: chamber + '-special',
            unitId: '001',
            municipality: '001',
            population: 1200,
          },
          {
            chamber: chamber as 'camera' | 'senato',
            collegeId: chamber + '-ordinary',
            unitId: '002',
            municipality: '002',
            population: 200,
          },
        ]),
      },
    };
  const list = (id: string, name: string) => ({
    id,
    name,
    coalitionId: null,
    protectedMinority: null,
  });
  const old = {
    lists: {
      camera: [
        list('list-main', 'MAIN A'),
        list('list-local', 'LOCAL PARTNERS'),
      ],
      senato: [
        list('list-main', 'MAIN A'),
        list('group-local', 'REGIONAL GROUP'),
      ],
    },
    coverage: ['camera', 'senato'].flatMap((ch) => [
      {
        chamber: ch as 'camera' | 'senato',
        collegeId: ch + '-ordinary',
        listIds: ['list-main'],
      },
      {
        chamber: ch as 'camera' | 'senato',
        collegeId: ch + '-special',
        listIds: ['list-main', ch === 'camera' ? 'list-local' : 'group-local'],
      },
    ]),
  };
  const policy = {
    ...policyData,
    futureScenario: undefined,
    history: ['camera-2022'],
    polls: [],
    excludedPolls: {},
    specialRegionNonValidAnchors: ['04'],
  } as DefaultSharePolicy;
  const encoded = await encodeSource(original);
  return { original, source: encoded.envelope, old, policy };
}
describe('FV-05 explicit complete-default policy', () => {
  it('keeps restricted joint/special groups whole and computes labelled regional reference shares', async () => {
    const f = await fixture(),
      before = canonicalJson(f.original),
      p = defaultSharePlan(
        f.source,
        f.old,
        f.policy,
        f.original.events[0].rows,
        f.original.events[1].rows,
      );
    expect(p.selection.components).toEqual(['a', 'historical-rest']);
    expect(p.listMembers.camera['list-local']).toEqual(['joint:list-local']);
    expect(
      p.components.filter((c) => c.kind === 'new').map((c) => c.id),
    ).toEqual(['joint:list-local', 'special-local']);
    expect(p.references.find((r) => r.channel === 'camera')!.mean).toBeCloseTo(
      50 / 650,
      14,
    );
    expect(p.references.find((r) => r.channel === 'senato')!.mean).toBeCloseTo(
      40 / 660,
      14,
    );
    expect(
      p.references.every(
        (r) => r.scope.kind === 'region' && r.scope.code === '04',
      ),
    ).toBe(true);
    expect(p.nonValidReferences.map((r) => r.mean)).toEqual([
      0.35, 0.33999999999999997,
    ]);
    expect(p.selection.componentMap!['minor-a']).toBe('historical-rest');
    expect(p.selection.sourceMemberships!['cl'].members).toEqual([
      'minor-a',
      'minor-b',
    ]);
    expect(canonicalJson(f.original)).toBe(before);
  });
  it('requires a reviewed full poll inventory and unambiguous ballot identity', async () => {
    const f = await fixture();
    f.policy.polls = ['unknown'];
    expect(() =>
      defaultSharePlan(
        f.source,
        f.old,
        f.policy,
        f.original.events[0].rows,
        f.original.events[1].rows,
      ),
    ).toThrow('inventory drift');
    f.policy.polls = [];
    f.source.payload.choices.push({
      ...f.source.payload.choices.find((c) => c.id === 'ca')!,
      id: 'duplicate',
    });
    expect(() =>
      defaultSharePlan(
        f.source,
        f.old,
        f.policy,
        f.original.events[0].rows,
        f.original.events[1].rows,
      ),
    ).toThrow('Ambiguous');
  });
  it('rejects an unanchored special domain instead of dropping its lists', async () => {
    const f = await fixture();
    f.policy.specialRegionNonValidAnchors = ['02'];
    expect(() =>
      defaultSharePlan(
        f.source,
        f.old,
        f.policy,
        f.original.events[0].rows,
        f.original.events[1].rows,
      ),
    ).toThrow('regional reference denominator');
  });
});
