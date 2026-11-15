import { expect, it } from 'vitest';
import { rosatellumSeries, summarizeCommonRosatellum } from './result-presentation';
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

it('restores residual stable IDs and known zero seats without inventing missing observations', () => {
  const params: Parameters<typeof summarizeCommonRosatellum>[0] = {
    ramo: 'camera', sim: 1, elapsedMs: 0,
    input: { candidati_uni: [], candidati_pluri: [], liste_uni: ['ordinary', 'Altre liste'].map((LISTA, i) => ({
      CIRCOSCRIZIONE: '1', COLLEGIOPLURINOMINALE: '1', COLLEGIOUNINOMINALE: '1',
      CANDIDATO: i ? null : 'candidate', CAND_MINORANZA: false, MINORANZA: false, LISTA, VOTI_LISTA: i ? 0 : 100 })) },
    output: { candidati_uni: [], candidati_pluri: [], liste_pluri: [{ CIRCOSCRIZIONE: '1', COLLEGIOPLURINOMINALE: '1',
      LISTA: 'ordinary', ELETTI: 3, NUMERO_MAX: 3, SEGGI_PRE_SUBENTRI: 3 }] },
    listeNaz: [{ LISTA: 'ordinary', COALIZIONE: 'list:ordinary', MINORANZA: false },
      { LISTA: 'Altre liste', COALIZIONE: null, MINORANZA: false, kind: 'other' }]
  };
  const result = summarizeCommonRosatellum(params);
  expect(result.seatVoteByList.get('__other__')).toEqual({ voteShare: 0, seats: 0 });
  expect(result.seatVoteByCoalition.get('list:__other__')).toEqual({ voteShare: 0, seats: 0 });
  expect(result.seatVoteByList.get('ordinary')).toEqual({ voteShare: 100, seats: 3 });
  expect(params.input.liste_uni[1].LISTA).toBe('Altre liste');
  expect(summarizeCommonRosatellum({ ...params, input: { ...params.input, liste_uni: params.input.liste_uni.slice(0, 1) } })
    .seatVoteByList.has('__other__')).toBe(false);
});
