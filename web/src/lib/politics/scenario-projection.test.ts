import { describe, expect, test } from 'vitest';
import type { Scenario } from '$lib/core/types';
import type { PoliticsHistoricalMunicipalListVoteRow, PoliticsPipelineSource } from './types';
import { projectScenarioOntoPoliticsSource } from './scenario-projection';

const sourceModelCorrespondenceElection = 'politics-static source model';

function logit(probability: number): number {
  return Math.log(probability / (1 - probability));
}

function source(): PoliticsPipelineSource {
  return {
    data_elezione: '2027-03-01T00:00:00.000Z',
    simulazioni: 1,
    frazione_uni_in_pluri: 0.25,
    frazioni_pluricandidature: [0.5, 0.3, 0.2],
    default_data_nascita: '2000-01-01T00:00:00.000Z',
    liste: [
      { LISTA: 'Lista A', COALIZIONE: 'Coalizione A', DATA: '2022-09-25T00:00:00.000Z', LOGIT_P: logit(0.1), SIGMA_GLOBAL: 0.1, PERCENTUALE: 0.1 },
      { LISTA: 'Lista B', COALIZIONE: 'Coalizione B', DATA: '2022-09-25T00:00:00.000Z', LOGIT_P: logit(0.2), SIGMA_GLOBAL: 0.1, PERCENTUALE: 0.2 },
      { LISTA: 'Lista C', COALIZIONE: 'Coalizione C', DATA: '2022-09-25T00:00:00.000Z', LOGIT_P: logit(0.3), SIGMA_GLOBAL: 0.1, PERCENTUALE: 0.3 },
      { LISTA: 'astensione', COALIZIONE: null, DATA: '2022-09-25T00:00:00.000Z', LOGIT_P: logit(0.4), SIGMA_GLOBAL: 0.1, PERCENTUALE: 0.4 }
    ],
    comuni_liste: [
      { CODICE_COMUNE: 1, LISTA: 'Lista A', DATA: '2022-09-25T00:00:00.000Z', DELTA: 0, SIGMA_DELTA: 0.1 },
      { CODICE_COMUNE: 1, LISTA: 'Lista B', DATA: '2022-09-25T00:00:00.000Z', DELTA: 0, SIGMA_DELTA: 0.1 },
      { CODICE_COMUNE: 1, LISTA: 'Lista C', DATA: '2022-09-25T00:00:00.000Z', DELTA: 0, SIGMA_DELTA: 0.1 },
      { CODICE_COMUNE: 1, LISTA: 'astensione', DATA: '2022-09-25T00:00:00.000Z', DELTA: 0, SIGMA_DELTA: 0.1 }
    ],
    base_dati: [{ CODICE_COMUNE: 1, CODITA_20N: 1, ELETTORI: 1000, CU20_COD: 10, SU20_COD: 20 }],
    camera: {
      uni: [{ CIRC_COD: 1, PLURI_COD: 11, UNI_COD: 10 }],
      pluri: [{ CIRC_COD: 1, PLURI_COD: 11, SEGGI_PLURI: 2 }],
      candidati_uni: [
        { COALIZIONE: 'Coalizione A', UNI_COD: 10, LISTA_MINORANZA: null, CANDIDATO_ID: null, DATA_NASCITA: null },
        { COALIZIONE: 'Coalizione B', UNI_COD: 10, LISTA_MINORANZA: null, CANDIDATO_ID: null, DATA_NASCITA: null },
        { COALIZIONE: 'Coalizione C', UNI_COD: 10, LISTA_MINORANZA: null, CANDIDATO_ID: null, DATA_NASCITA: null }
      ],
      candidati_pluri: [
        { CIRC_COD: 1, LISTA: 'Lista A', PLURI_COD: 11, NUMERO_CANDIDATO: 1, MINORANZA: false, CANDIDATO_ID: null, DATA_NASCITA: null },
        { CIRC_COD: 1, LISTA: 'Lista B', PLURI_COD: 11, NUMERO_CANDIDATO: 1, MINORANZA: false, CANDIDATO_ID: null, DATA_NASCITA: null },
        { CIRC_COD: 1, LISTA: 'Lista C', PLURI_COD: 11, NUMERO_CANDIDATO: 1, MINORANZA: false, CANDIDATO_ID: null, DATA_NASCITA: null }
      ]
    },
    senato: {
      uni: [{ CIRC_COD: 1, PLURI_COD: 21, UNI_COD: 20 }],
      pluri: [{ CIRC_COD: 1, PLURI_COD: 21, SEGGI_PLURI: 1 }],
      candidati_uni: [
        { COALIZIONE: 'Coalizione A', UNI_COD: 20, LISTA_MINORANZA: null, CANDIDATO_ID: null, DATA_NASCITA: null },
        { COALIZIONE: 'Coalizione B', UNI_COD: 20, LISTA_MINORANZA: null, CANDIDATO_ID: null, DATA_NASCITA: null },
        { COALIZIONE: 'Coalizione C', UNI_COD: 20, LISTA_MINORANZA: null, CANDIDATO_ID: null, DATA_NASCITA: null }
      ],
      candidati_pluri: [
        { CIRC_COD: 1, LISTA: 'Lista A', PLURI_COD: 21, NUMERO_CANDIDATO: 1, MINORANZA: false, CANDIDATO_ID: null, DATA_NASCITA: null },
        { CIRC_COD: 1, LISTA: 'Lista B', PLURI_COD: 21, NUMERO_CANDIDATO: 1, MINORANZA: false, CANDIDATO_ID: null, DATA_NASCITA: null },
        { CIRC_COD: 1, LISTA: 'Lista C', PLURI_COD: 21, NUMERO_CANDIDATO: 1, MINORANZA: false, CANDIDATO_ID: null, DATA_NASCITA: null }
      ]
    }
  };
}

function scenario(lists: Scenario['lists']): Scenario {
  return {
    id: 'scenario',
    name: 'Scenario',
    electionDate: '2027-04-01',
    defaultSource: {
      kind: 'manual',
      electionKind: 'politiche',
      territory: 'Italia',
      dataVersion: 'test'
    },
    globalShareMode: 'mean',
    abstentionShare: 40,
    abstentionOverride: false,
    coalitions: [
      { id: 'a', name: 'Coalizione A', color: '#000000' },
      { id: 'b', name: 'Coalizione B', color: '#111111' },
      { id: 'c', name: 'Coalizione C', color: '#222222' },
      { id: 'new', name: 'Coalizione Nuova', color: '#333333' }
    ],
    lists,
    listCorrespondences: []
  };
}

function historicalVotes(): PoliticsHistoricalMunicipalListVoteRow[] {
  return [
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: 1, LISTA: 'Historical A', VOTI: 40 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: 1, LISTA: 'Historical B', VOTI: 20 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: 1, LISTA: 'Historical C', VOTI: 20 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: 1, LISTA: 'Historical X', VOTI: 10 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: 1, LISTA: 'astensione', VOTI: 10 }
  ];
}

describe('politics scenario projection', () => {
  test('keeps source percentages when no list has a share override', () => {
    const projection = projectScenarioOntoPoliticsSource(
      source(),
      scenario([
        { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false },
        { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
        { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
      ]),
      { simulations: 3 }
    );

    expect(projection.source.simulazioni).toBe(3);
    expect(projection.source.data_elezione).toBe('2027-04-01T00:00:00.000Z');
    expect(projection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => Number(row.PERCENTUALE.toFixed(3)))).toEqual([
      0.1, 0.2, 0.3
    ]);
    expect(projection.warnings).toEqual([]);
  });

  test('removes missing source lists and rescales the remaining default model', () => {
    const projection = projectScenarioOntoPoliticsSource(
      source(),
      scenario([
        { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false },
        { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
      ]),
      { simulations: 1 }
    );

    const rows = projection.source.liste.filter((row) => row.LISTA !== 'astensione');
    expect(rows.map((row) => row.LISTA)).toEqual(['Lista A', 'Lista C']);
    expect(rows.map((row) => Number(row.PERCENTUALE.toFixed(3)))).toEqual([0.15, 0.45]);
    expect(projection.source.comuni_liste.map((row) => row.LISTA)).toEqual(['Lista A', 'Lista C', 'astensione']);
    expect(projection.source.camera.candidati_pluri.map((row) => row.LISTA)).toEqual(['Lista A', 'Lista C']);
    expect(projection.rows.find((row) => row.list === 'Lista B')?.status).toBe('removed');
  });

  test('applies partial share overrides and recalculates unspecified lists proportionally', () => {
    const projection = projectScenarioOntoPoliticsSource(
      source(),
      scenario([
        { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 50, shareOverride: true },
        { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
        { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
      ]),
      { simulations: 1 }
    );

    expect(projection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => Number(row.PERCENTUALE.toFixed(3)))).toEqual([
      0.3, 0.12, 0.18
    ]);
    expect(projection.rows.find((row) => row.list === 'Lista A')?.projectedShare).toBe(50);
  });

  test('uses abstention override as the elector-fraction anchor for valid-vote list shares', () => {
    const abstentionScenario = scenario([
      { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 50, shareOverride: true },
      { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
      { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
    ]);
    abstentionScenario.abstentionShare = 50;
    abstentionScenario.abstentionOverride = true;

    const projection = projectScenarioOntoPoliticsSource(source(), abstentionScenario, { simulations: 1 });

    expect(projection.source.liste.map((row) => [row.LISTA, Number(row.PERCENTUALE.toFixed(3))])).toEqual([
      ['Lista A', 0.25],
      ['Lista B', 0.1],
      ['Lista C', 0.15],
      ['astensione', 0.5]
    ]);
    expect(projection.source.liste.find((row) => row.LISTA === 'astensione')?.SIGMA_GLOBAL).toBe(0);
    expect(projection.rows.find((row) => row.list === 'Lista A')?.projectedShare).toBe(50);
  });

  test('uses declared correspondences to project renamed scenario lists', () => {
    const renamedScenario = scenario([
      { id: 'x', name: 'Lista X', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false },
      { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
      { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
    ]);
    renamedScenario.listCorrespondences = [
      {
        id: 'corr-a-x',
        futureList: 'Lista X',
        pastElection: sourceModelCorrespondenceElection,
        pastDate: '2022-09-25',
        pastList: 'Lista A',
        factor: 1,
        source: 'manual'
      }
    ];

    const projection = projectScenarioOntoPoliticsSource(source(), renamedScenario, { simulations: 1 });

    expect(projection.warnings).toEqual([]);
    expect(projection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => row.LISTA)).toEqual([
      'Lista X',
      'Lista B',
      'Lista C'
    ]);
    expect(projection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => Number(row.PERCENTUALE.toFixed(3)))).toEqual([
      0.1, 0.2, 0.3
    ]);
    expect(projection.source.comuni_liste.map((row) => row.LISTA)).toEqual(['Lista X', 'Lista B', 'Lista C', 'astensione']);
    expect(projection.source.camera.candidati_pluri.map((row) => row.LISTA)).toEqual(['Lista X', 'Lista B', 'Lista C']);
    expect(projection.rows.find((row) => row.list === 'Lista X')).toEqual(
      expect.objectContaining({
        sourceList: 'Lista A',
        matchMode: 'declared-correspondence',
        status: 'matched'
      })
    );
  });

  test('retargets rich historical parameters through source-model correspondences', () => {
    const renamedScenario = scenario([
      { id: 'x', name: 'Lista X', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false },
      { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
      { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
    ]);
    renamedScenario.listCorrespondences = [
      {
        id: 'source-a-x',
        futureList: 'Lista X',
        pastElection: sourceModelCorrespondenceElection,
        pastDate: '2022-09-25',
        pastList: 'Lista A',
        factor: 1,
        source: 'manual'
      },
      {
        id: 'history-a',
        futureList: 'Lista A',
        pastElection: 'election 2024',
        pastDate: '2024-01-01',
        pastList: 'Historical A',
        factor: 1,
        source: 'bundled'
      },
      {
        id: 'history-b',
        futureList: 'Lista B',
        pastElection: 'election 2024',
        pastDate: '2024-01-01',
        pastList: 'Historical B',
        factor: 1,
        source: 'bundled'
      },
      {
        id: 'history-c',
        futureList: 'Lista C',
        pastElection: 'election 2024',
        pastDate: '2024-01-01',
        pastList: 'Historical C',
        factor: 1,
        source: 'bundled'
      }
    ];

    const projection = projectScenarioOntoPoliticsSource(source(), renamedScenario, {
      historicalVotes: historicalVotes(),
      simulations: 1
    });

    expect(projection.warnings).toEqual([]);
    expect(projection.source.liste.map((row) => [row.LISTA, Number(row.PERCENTUALE.toFixed(3))])).toEqual([
      ['Lista B', 0.2],
      ['Lista C', 0.2],
      ['Lista X', 0.4],
      ['astensione', 0.2]
    ]);
    expect(projection.source.comuni_liste.map((row) => row.LISTA)).toEqual(['astensione', 'Lista B', 'Lista C', 'Lista X']);
    const renamedProjectionRow = projection.rows.find((row) => row.list === 'Lista X');
    expect(renamedProjectionRow).toEqual(
      expect.objectContaining({
        sourceList: 'Lista A',
        matchMode: 'declared-correspondence'
      })
    );
    expect(renamedProjectionRow?.projectedShare ?? Number.NaN).toBeCloseTo(50);
  });

  test('warns when declared correspondences cannot use the current snapshot', () => {
    const staleScenario = scenario([
      { id: 'x', name: 'Lista X', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false }
    ]);
    staleScenario.listCorrespondences = [
      {
        id: 'stale',
        futureList: 'Lista X',
        pastElection: sourceModelCorrespondenceElection,
        pastDate: '2022-09-25',
        pastList: 'Lista che non esiste',
        factor: 1,
        source: 'manual'
      }
    ];

    const projection = projectScenarioOntoPoliticsSource(source(), staleScenario, { simulations: 1 });

    expect(projection.warnings.map((warning) => warning.code)).toEqual(
      expect.arrayContaining([
        'POLITICS_SCENARIO_CORRESPONDENCES_UNUSED',
        'POLITICS_SCENARIO_LISTS_IGNORED',
        'POLITICS_SCENARIO_NO_MATCHING_LISTS'
      ])
    );
    expect(projection.rows.find((row) => row.list === 'Lista X')).toEqual(
      expect.objectContaining({
        sourceList: null,
        matchMode: 'none',
        status: 'unmatched'
      })
    );
  });

  test('keeps mean mode stochastic parameters and zeros global sigma in fixed mode', () => {
    const meanProjection = projectScenarioOntoPoliticsSource(
      source(),
      scenario([
        { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 50, shareOverride: true },
        { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
        { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
      ]),
      { simulations: 1 }
    );
    const fixedScenario = scenario([
      { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 50, shareOverride: true },
      { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
      { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
    ]);
    fixedScenario.globalShareMode = 'fixed';
    const fixedProjection = projectScenarioOntoPoliticsSource(source(), fixedScenario, { simulations: 1 });

    expect(meanProjection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => row.SIGMA_GLOBAL)).toEqual([
      0.1, 0.1, 0.1
    ]);
    expect(fixedProjection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => row.SIGMA_GLOBAL)).toEqual([
      0, 0, 0
    ]);
  });

  test('ignores unmatched new lists and creates placeholder candidates for new matched coalitions', () => {
    const projection = projectScenarioOntoPoliticsSource(
      source(),
      scenario([
        { id: 'a', name: 'Lista A', coalition: 'Coalizione Nuova', color: '#000000', startingShare: 10, shareOverride: false },
        { id: 'x', name: 'Lista X', coalition: 'Coalizione Nuova', color: '#333333', startingShare: 15, shareOverride: true }
      ]),
      { simulations: 1 }
    );

    expect(projection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => row.LISTA)).toEqual(['Lista A']);
    expect(projection.source.liste.find((row) => row.LISTA === 'Lista A')?.COALIZIONE).toBe('Coalizione Nuova');
    expect(projection.source.camera.candidati_uni).toEqual(
      expect.arrayContaining([
        { COALIZIONE: 'Coalizione Nuova', UNI_COD: 10, LISTA_MINORANZA: null, CANDIDATO_ID: null, DATA_NASCITA: null }
      ])
    );
    expect(projection.warnings.map((warning) => warning.code)).toEqual(
      expect.arrayContaining(['POLITICS_SCENARIO_LISTS_IGNORED', 'POLITICS_SCENARIO_SYNTHETIC_COALITIONS'])
    );
    expect(projection.rows.find((row) => row.list === 'Lista X')?.status).toBe('unmatched');
  });
});
