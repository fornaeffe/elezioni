import type { OriginalSource, SourcePoll } from '../../src/lib/politics/reform-2026/vote-share/source-types';
import type { SourceSelection } from '../../src/lib/politics/reform-2026/vote-share/source-compiler';
export function originalShareFixture(): OriginalSource {
  const events = ['2010-01-01','2011-01-01','2014-01-01','2016-01-01'].map((date, e) => ({
    id: 'e' + e, date, availableDate: date.slice(0, 8) + '31', context: e % 2 ? 'european' as const : 'camera' as const,
    minimumAge: 18 as const, role: 'reference' as const, sourceId: 'official-' + e, sourceUrl: 'https://example.org/election-' + e, sourceSha256: 'a'.repeat(64), availabilityBasis: 'Explicit fixture availability',
    rows: [{ unit: 'A', electors: 1000, voters: 900, choices: [{ id: e + '-a', count: e === 0 ? 0 : 100, direct: e === 0 ? 0 : 90 }, { id: e + '-b', count: 300, direct: 290 }, { id: e + '-c', count: 400, direct: 400 }] },
      { unit: 'X', electors: 100, voters: 80, choices: [{ id: e + '-a', count: 10, direct: 10 }, { id: e + '-b', count: 20, direct: 20 }, { id: e + '-c', count: 30, direct: 30 }] }]
  }));
  const poll: SourcePoll = { id: 'q1', surveyId: 'survey1', context: 'camera', date: '2015-01-02', fieldworkStart: '2015-01-01', fieldworkEnd: '2015-01-03', availableDate: '2015-01-04', citation: 'https://example.org/fixture', sampleSize: 1000, questionSampleSize: null,
    decidedFraction: 0.5, effectiveSample: 250, designEffect: 2, effectiveSamplePolicy: 'fast-share-effective-sample-v1', errorGroup: '2015-H1-camera', roundingWidth: 0.001,
    original: [{ label: 'A', percentage: 50, kind: 'list' },{ label: 'B', percentage: 20, kind: 'list' },{ label: 'Other', percentage: 30, kind: 'residual' }],
    normalized: [{ label: 'A', members: ['a'], share: 0.5, other: false }, { label: 'B', members: ['b'], share: 0.2, other: false }, { label: 'Other', members: [], share: 0.3, other: true }], otherDefinition: 'offered-complement', status: 'supported', reasons: [], method: 'Fixture original reported support' };
  return { sourceVersions: ['fixture-source-v1'], methodVersions: ['fixture-v1'], assumptions: ['Constructed source boundary only; not additional election evidence'],
    correspondencePolicy: { id: 'fixture-correspondences', status: 'constructed-test-only', warnings: [] }, citations: [{ id: 'fixture-source', url: 'https://example.org/fixture' }],
    units: [{ id: 'A', name: 'Alpha', region: '01', province: '001', targetCode: '001', match: 'same-place' }, { id: 'X', name: 'Historical unmatched place', region: '01', province: null, targetCode: null, match: 'unmatched' }],
    choices: events.flatMap((e, i) => ['a','b','c'].map(id => ({ id: i + '-' + id, eventId: e.id, label: id.toUpperCase(), members: [id], method: 'Fixture original list', citations: ['fixture-source'], opaque: id === 'c' }))), events, polls: [poll],
    geography: { version: 'fixture-geography', censusDate: '2021-12-31', targets: [{ code: '001', name: 'Alpha', region: '01', province: '001', population: 1200 }, { code: '002', name: 'New target without history', region: '02', province: '002', population: 200 }],
      colleges: [{ id: 'c', chamber: 'camera', kind: 'ordinary', territoryId: 'C' }, { id: 's', chamber: 'senato', kind: 'special-uninominal', territoryId: 'S' }],
      cells: ['001','002'].flatMap((code, i) => [{ chamber: 'camera' as const, collegeId: 'c', unitId: code, municipality: code, population: i ? 200 : 1200 }, { chamber: 'senato' as const, collegeId: 's', unitId: code, municipality: code, population: i ? 200 : 1200 }]) }
  };
}
export function sourceSelection(): SourceSelection { return { referenceDate: '2017-01-01', electionIds: ['e0','e1','e2','e3'], pollIds: ['q1'], components: ['a','b','c'], targets: [{ code: '001', electors: 1000 }, { code: '002', electors: 100 }], targetContext: 'camera' }; }
