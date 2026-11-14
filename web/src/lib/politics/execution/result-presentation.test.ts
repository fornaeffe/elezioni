import { expect, it } from 'vitest';
import { rosatellumSeries } from './result-presentation';
import { fixture } from '../scenario/scenario.fixture';

it('keeps Rosatellum list plurinominal seats distinct from support totals and resolves ticket labels', () => {
  const original = fixture().scenario, list = original.subjects.lists[0];
  const scenario = { ...original, subjects: { ...original.subjects,
    coalitions: [{ id: 'C', name: 'Coalizione visibile', color: '#abcdef', chambers: ['camera' as const] }] } };
  const table = rosatellumSeries(scenario, [{ ramo: 'camera', sim: 7,
    seatVoteByList: new Map([[list.id, { voteShare: 0, seats: 0 }]]),
    seatVoteByCoalition: new Map([['coalition:C', { voteShare: 60, seats: 3 }], [`list:${list.id}`, { voteShare: 0, seats: 1 }]]) }]);
  expect(table.rows).toEqual(expect.arrayContaining([
    expect.objectContaining({ Simulation: 7, Kind: 'list', Owner: `list:${list.id}`, Elected: 0, 'Vote share %': 0, Name: `${list.name} · seggi plurinominali` }),
    expect.objectContaining({ Kind: 'subject', Owner: 'coalition:C', Elected: 3, Name: 'Coalizione visibile', Color: '#abcdef' }),
    expect.objectContaining({ Kind: 'subject', Owner: `list:${list.id}`, Elected: 1, Name: list.name, Color: list.color }),
  ]));
});
