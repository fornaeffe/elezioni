import { describe, expect, test } from 'vitest';
import type { Scenario } from '$lib/core/types';
import type { PoliticsPipelineSource } from './types';
import { projectScenarioOntoPoliticsSource } from './scenario-projection';

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
    coalitions: [
      { id: 'a', name: 'Coalizione A', color: '#000000' },
      { id: 'b', name: 'Coalizione B', color: '#111111' },
      { id: 'c', name: 'Coalizione C', color: '#222222' },
      { id: 'new', name: 'Coalizione Nuova', color: '#333333' }
    ],
    lists
  };
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
    expect(projection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => row.PERCENTUALE)).toEqual([
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
