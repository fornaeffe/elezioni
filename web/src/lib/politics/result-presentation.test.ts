import { describe, expect, test } from 'vitest';
import type { ListaNazRow, PoliticsScrutinyInput, PoliticsScrutinyOutput } from './types';
import {
  buildPoliticsResultTables,
  summarizePoliticsGeneratedRuns,
  summarizePoliticsScrutinyRun
} from './result-presentation';

const listeNaz: ListaNazRow[] = [
  { LISTA: 'Lista A', COALIZIONE: 'Coalizione A', MINORANZA: false },
  { LISTA: 'Lista B', COALIZIONE: null, MINORANZA: false }
];

function inputForSimulation(sim: number): PoliticsScrutinyInput {
  const listaAVotes = sim === 1 ? 100 : 40;
  const listaBVotes = sim === 1 ? 50 : 60;

  return {
    liste_uni: [
      {
        CIRCOSCRIZIONE: 1,
        COLLEGIOPLURINOMINALE: 10,
        COLLEGIOUNINOMINALE: 100,
        CANDIDATO: 'Candidate A',
        CAND_MINORANZA: false,
        LISTA: 'Lista A',
        MINORANZA: false,
        VOTI_LISTA: listaAVotes
      },
      {
        CIRCOSCRIZIONE: 1,
        COLLEGIOPLURINOMINALE: 10,
        COLLEGIOUNINOMINALE: 100,
        CANDIDATO: 'Candidate B',
        CAND_MINORANZA: false,
        LISTA: 'Lista B',
        MINORANZA: false,
        VOTI_LISTA: listaBVotes
      }
    ],
    candidati_uni: [
      {
        CIRCOSCRIZIONE: 1,
        COLLEGIOPLURINOMINALE: 10,
        COLLEGIOUNINOMINALE: 100,
        CANDIDATO: 'Candidate A',
        DATA_NASCITA: '1980-01-01T00:00:00.000Z',
        VOTI_CANDIDATO: sim === 1 ? 120 : 50
      },
      {
        CIRCOSCRIZIONE: 1,
        COLLEGIOPLURINOMINALE: 10,
        COLLEGIOUNINOMINALE: 100,
        CANDIDATO: 'Candidate B',
        DATA_NASCITA: '1970-01-01T00:00:00.000Z',
        VOTI_CANDIDATO: sim === 1 ? 80 : 70
      }
    ],
    candidati_pluri: [
      {
        CIRCOSCRIZIONE: 1,
        COLLEGIOPLURINOMINALE: 10,
        LISTA: 'Lista A',
        NUMERO: 1,
        CANDIDATO: 'Pluri A'
      },
      {
        CIRCOSCRIZIONE: 1,
        COLLEGIOPLURINOMINALE: 10,
        LISTA: 'Lista B',
        NUMERO: 1,
        CANDIDATO: 'Pluri B'
      }
    ]
  };
}

function outputForSimulation(sim: number): PoliticsScrutinyOutput {
  return {
    liste_pluri: [
      {
        CIRCOSCRIZIONE: 1,
        COLLEGIOPLURINOMINALE: 10,
        LISTA: 'Lista A',
        ELETTI: sim === 1 ? 2 : 0,
        NUMERO_MAX: sim === 1 ? 2 : 0,
        SEGGI_PRE_SUBENTRI: sim === 1 ? 2 : 0
      },
      {
        CIRCOSCRIZIONE: 1,
        COLLEGIOPLURINOMINALE: 10,
        LISTA: 'Lista B',
        ELETTI: sim === 1 ? 0 : 1,
        NUMERO_MAX: sim === 1 ? 0 : 1,
        SEGGI_PRE_SUBENTRI: sim === 1 ? 0 : 1
      }
    ],
    candidati_uni: [
      {
        CIRCOSCRIZIONE: 1,
        COLLEGIOPLURINOMINALE: 10,
        COLLEGIOUNINOMINALE: 100,
        CANDIDATO: 'Candidate A',
        ELETTO: sim === 1
      },
      {
        CIRCOSCRIZIONE: 1,
        COLLEGIOPLURINOMINALE: 10,
        COLLEGIOUNINOMINALE: 100,
        CANDIDATO: 'Candidate B',
        ELETTO: sim === 2
      }
    ],
    candidati_pluri: [
      {
        CIRCOSCRIZIONE: 1,
        COLLEGIOPLURINOMINALE: 10,
        LISTA: 'Lista A',
        NUMERO: 1,
        CANDIDATO: 'Pluri A',
        ELETTO: sim === 1,
        ELETTO_QUI_O_ALTROVE: sim === 1
      },
      {
        CIRCOSCRIZIONE: 1,
        COLLEGIOPLURINOMINALE: 10,
        LISTA: 'Lista B',
        NUMERO: 1,
        CANDIDATO: 'Pluri B',
        ELETTO: sim === 2,
        ELETTO_QUI_O_ALTROVE: sim === 2
      }
    ]
  };
}

describe('politics result presentation', () => {
  const runs = [1, 2].map((sim) =>
    summarizePoliticsScrutinyRun({
      ramo: 'camera',
      sim,
      elapsedMs: sim * 10,
      input: inputForSimulation(sim),
      output: outputForSimulation(sim),
      listeNaz
    })
  );

  test('builds user-facing summary tables from scrutiny runs', () => {
    const tables = buildPoliticsResultTables(runs);

    expect(tables.map((table) => table.name)).toEqual([
      'Election overview',
      'Average plurinominal seats by list',
      'Vote share by list',
      'Uninominal winners by support'
    ]);
    expect(tables[0].rows).toEqual([
      {
        Ramo: 'camera',
        Simulazioni: 2,
        'Media seggi pluri': 1.5,
        'Media eletti uni': 1,
        'Media candidati pluri eletti': 1,
        'Tempo medio ms': 15
      }
    ]);
    expect(tables[1].rows.find((row) => row.Lista === 'Lista A')).toEqual({
      Ramo: 'camera',
      Lista: 'Lista A',
      Media: 1,
      Min: 0,
      Max: 2,
      'Probabilità >=1': 50
    });
    expect(tables[2].rows.find((row) => row.Lista === 'Lista A')).toEqual({
      Ramo: 'camera',
      Lista: 'Lista A',
      'Media %': 53.33,
      'P05 %': 41.33,
      'P50 %': 53.33,
      'P95 %': 65.33
    });
    expect(tables[3].rows).toEqual([
      {
        Ramo: 'camera',
        Soggetto: 'Coalizione A',
        Media: 0.5,
        Min: 0,
        Max: 1,
        'Probabilità >=1': 50
      },
      {
        Ramo: 'camera',
        Soggetto: 'Lista B',
        Media: 0.5,
        Min: 0,
        Max: 1,
        'Probabilità >=1': 50
      }
    ]);
  });

  test('keeps generated run details as a diagnostic table', () => {
    expect(summarizePoliticsGeneratedRuns(runs).rows).toEqual([
      {
        Ramo: 'camera',
        Sim: 1,
        'Seggi pluri': 2,
        'Eletti uni': 1,
        'Candidati pluri eletti': 1,
        'Tempo ms': 10
      },
      {
        Ramo: 'camera',
        Sim: 2,
        'Seggi pluri': 1,
        'Eletti uni': 1,
        'Candidati pluri eletti': 1,
        'Tempo ms': 20
      }
    ]);
  });
});
