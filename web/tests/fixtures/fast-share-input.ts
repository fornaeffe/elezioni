import type { NationalSource } from '../../src/lib/politics/reform-2026/vote-share/types';
import type { ProfileSource } from '../../src/lib/politics/reform-2026/vote-share/profiles';

/** Deliberately constructed reduced observations, never production defaults. */
export function reducedShareInput(): { national: NationalSource; profiles: ProfileSource } {
  const components = ['a', 'b', 'c', 'd'];
  const elections: NationalSource['elections'] = [
    { id: 'c10', date: '2010-01-01', context: 'camera', electors: 1000000, groups: [['a'], ['b'], ['c'], ['d']], counts: [250000, 150000, 200000, 250000, 150000] },
    { id: 'e11', date: '2011-01-01', context: 'european', electors: 1200000, groups: [['a', 'b'], ['c'], ['d']], counts: [400000, 300000, 280000, 220000] },
    { id: 'c14', date: '2014-01-01', context: 'camera', electors: 1100000, groups: [['a'], ['b'], ['c'], ['d']], counts: [330000, 200000, 180000, 240000, 150000] },
    { id: 'e16', date: '2016-01-01', context: 'european', electors: 1300000, groups: [['a'], ['b'], ['c'], ['d']], counts: [450000, 220000, 240000, 250000, 140000] }
  ];
  const national: NationalSource = { components, referenceDate: '2016-02-01', elections, polls: [
    { id: 'p13', date: '2013-12-20', context: 'camera', groups: [['a'], ['b'], ['c', 'd']], shares: [0.28, 0.30, 0.42], sample: 700, errorGroup: '2013-H2-camera', availableDate: '2013-12-23', fieldworkEnd: '2013-12-21' },
    { id: 'p16a', date: '2016-01-10', context: 'camera', groups: [['a', 'b'], ['c'], ['d']], shares: [0.52, 0.30, 0.18], sample: 900, errorGroup: '2016-H1-camera', availableDate: '2016-01-12', fieldworkEnd: '2016-01-11' },
    { id: 'p16b', date: '2016-01-13', context: 'camera', groups: [['a'], ['c'], ['d']], shares: [0.40, 0.35, 0.25], sample: 600, errorGroup: '2016-H1-camera', availableDate: '2016-01-15', fieldworkEnd: '2016-01-14' },
    { id: 'p16e', date: '2016-01-16', context: 'european', groups: [['a'], ['b'], ['c'], ['d']], shares: [0.20, 0.30, 0.30, 0.20], sample: 800, errorGroup: '2016-H1-european', availableDate: '2016-01-18', fieldworkEnd: '2016-01-17' }
  ] };
  const counts = [
    [[10, 20, 30, 20], [0, 25, 25, 20], [60, 20, 40, 20]],
    [[35, 30, 15], [25, 25, 20], [85, 35, 20]],
    [[15, 25, 25, 15], [5, 25, 25, 15], [70, 20, 30, 20]],
    [[25, 25, 20, 10], [15, 15, 20, 20], [40, 20, 50, 30]]
  ];
  const history: ProfileSource['elections'] = elections.map((e, i) => ({ id: e.id, date: e.date, context: e.context,
    rows: ['n1', 'n2', 's1'].map((id, c) => ({ id, region: c < 2 ? 'north' : 'south',
      electors: (c < 2 ? 100 : 200) * 100, nonValid: [20, 30, 60][c] * 100,
      groups: e.groups.map(g => [...g]), counts: counts[i][c].map(n => n * 100) })) }));
  history[1].rows.push({ id: 'joint', region: 'north', electors: 15000, nonValid: 4500,
    groups: [['a', 'b'], ['c'], ['d']], counts: [6000, 3000, 1500] });
  history[1].rows.push({ id: 'cross', region: 'south', electors: 15000, nonValid: 4500,
    groups: [['a'], ['b', 'c'], ['d']], counts: [2000, 6000, 2500] });
  history[0].rows.push({ id: 'retired', region: 'south', electors: 16000, nonValid: 4000,
    groups: [['a'], ['b'], ['c'], ['d']], counts: [4000, 3000, 3000, 2000] });
  const profiles: ProfileSource = { components, referenceDate: national.referenceDate, targetContext: 'camera',
    nationalElections: elections, elections: history,
    targets: ['n1', 'n2', 's1', 'joint', 'cross', 'fresh'].map(id => ({ id,
      region: ['s1', 'cross'].includes(id) ? 'south' : 'north', electors: 20000 })) };
  return { national, profiles };
}
