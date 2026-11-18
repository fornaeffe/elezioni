import { describe, expect, test } from 'vitest';
import type { Scenario } from '$lib/core/types';
import type { PoliticsHistoricalMunicipalListVoteRow, PoliticsMunicipalityCatalogRow, PoliticsPipelineSource } from './types';
import { projectScenarioOntoPoliticsSource } from './scenario-projection';

function logit(probability: number): number {
  return Math.log(probability / (1 - probability));
}

function source(): PoliticsPipelineSource {
  return {
    data_elezione: '2027-03-01T00:00:00.000Z',
    simulazioni: 1,
    frazione_uni_in_pluri: 0.25,
    frazioni_pluricandidature: [0.5, 0.3, 0.2, 0, 0],
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
    listCorrespondences: [],
    localShareOverrides: [],
    candidateTemplates: [],
    candidateGeneration: {
      uninominalToPlurinominalShare: 0,
      plurinominalCandidacyCountShares: [1, 0, 0, 0, 0]
    }
  };
}

function rawLocalFractions(
  projection: ReturnType<typeof projectScenarioOntoPoliticsSource>,
  municipalityCode: number
): Record<string, number> {
  const globalByList = new Map(projection.source.liste.map((row) => [row.LISTA, row]));
  const rows = projection.source.comuni_liste.filter((row) => row.CODICE_COMUNE === municipalityCode);
  const raw = rows.map((row) => {
    const global = globalByList.get(row.LISTA);
    if (!global) throw new Error(`Missing global row for ${row.LISTA}`);
    return {
      list: row.LISTA,
      value: 1 / (1 + Math.exp(-(global.LOGIT_P + row.DELTA)))
    };
  });
  const total = raw.reduce((sum, row) => sum + row.value, 0);

  return Object.fromEntries(raw.map((row) => [row.list, row.value / total]));
}

function localFractions(projection: ReturnType<typeof projectScenarioOntoPoliticsSource>, municipalityCode: number): Record<string, number> {
  const fractions = rawLocalFractions(projection, municipalityCode);
  return Object.fromEntries(Object.entries(fractions).map(([list, value]) => [list, Number(value.toFixed(3))]));
}

function localValidShares(projection: ReturnType<typeof projectScenarioOntoPoliticsSource>, municipalityCode: number): Record<string, number> {
  const fractions = rawLocalFractions(projection, municipalityCode);
  const politicalTotal = 1 - (fractions.astensione ?? 0);
  return Object.fromEntries(
    Object.entries(fractions)
      .filter(([list]) => list !== 'astensione')
      .map(([list, fraction]) => [list, fraction / politicalTotal])
  );
}

function localRowsFor(
  municipalityCode: number,
  validShares: Record<'Lista A' | 'Lista B' | 'Lista C', number>,
  abstentionFraction = 0.4
): PoliticsPipelineSource['comuni_liste'] {
  return [
    {
      CODICE_COMUNE: municipalityCode,
      LISTA: 'Lista A',
      DATA: '2022-09-25T00:00:00.000Z',
      DELTA: logit((1 - abstentionFraction) * validShares['Lista A']) - logit(0.1),
      SIGMA_DELTA: 0.1
    },
    {
      CODICE_COMUNE: municipalityCode,
      LISTA: 'Lista B',
      DATA: '2022-09-25T00:00:00.000Z',
      DELTA: logit((1 - abstentionFraction) * validShares['Lista B']) - logit(0.2),
      SIGMA_DELTA: 0.1
    },
    {
      CODICE_COMUNE: municipalityCode,
      LISTA: 'Lista C',
      DATA: '2022-09-25T00:00:00.000Z',
      DELTA: logit((1 - abstentionFraction) * validShares['Lista C']) - logit(0.3),
      SIGMA_DELTA: 0.1
    },
    {
      CODICE_COMUNE: municipalityCode,
      LISTA: 'astensione',
      DATA: '2022-09-25T00:00:00.000Z',
      DELTA: 0,
      SIGMA_DELTA: 0.1
    }
  ];
}

function aggregateSource(): PoliticsPipelineSource {
  return {
    ...source(),
    comuni_liste: [
      ...localRowsFor(1, { 'Lista A': 0.1, 'Lista B': 0.3, 'Lista C': 0.6 }),
      ...localRowsFor(2, { 'Lista A': 0.3, 'Lista B': 0.4, 'Lista C': 0.3 }),
      ...localRowsFor(3, { 'Lista A': 0.6, 'Lista B': 0.2, 'Lista C': 0.2 })
    ],
    base_dati: [
      { CODICE_COMUNE: 1, CODITA_20N: 1, ELETTORI: 1000, CU20_COD: 10, SU20_COD: 20 },
      { CODICE_COMUNE: 2, CODITA_20N: 2, ELETTORI: 2000, CU20_COD: 10, SU20_COD: 20 },
      { CODICE_COMUNE: 3, CODITA_20N: 3, ELETTORI: 3000, CU20_COD: 11, SU20_COD: 21 }
    ]
  };
}

function municipalities(): PoliticsMunicipalityCatalogRow[] {
  return [
    { CODICE_COMUNE: 1, COMUNE: 'Comune 1', CODICE_PROVINCIA: 10, PROVINCIA: 'Provincia 10', CODICE_REGIONE: 100, REGIONE: 'Regione 100' },
    { CODICE_COMUNE: 2, COMUNE: 'Comune 2', CODICE_PROVINCIA: 10, PROVINCIA: 'Provincia 10', CODICE_REGIONE: 100, REGIONE: 'Regione 100' },
    { CODICE_COMUNE: 3, COMUNE: 'Comune 3', CODICE_PROVINCIA: 11, PROVINCIA: 'Provincia 11', CODICE_REGIONE: 100, REGIONE: 'Regione 100' }
  ];
}

function aggregateValidShare(
  projection: ReturnType<typeof projectScenarioOntoPoliticsSource>,
  municipalityCodes: readonly number[],
  list: string
): number {
  const baseByCode = new Map(projection.source.base_dati.map((row) => [Number(row.CODICE_COMUNE), row]));
  let numerator = 0;
  let denominator = 0;

  for (const code of municipalityCodes) {
    const fractions = rawLocalFractions(projection, code);
    const politicalTotal = 1 - (fractions.astensione ?? 0);
    const electors = Number(baseByCode.get(code)?.ELETTORI ?? 0);
    numerator += electors * (fractions[list] ?? 0);
    denominator += electors * politicalTotal;
  }

  return numerator / denominator;
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

  test('overrides candidate-generation settings from the scenario', () => {
    const candidateScenario = scenario([
      { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false },
      { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
      { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
    ]);
    candidateScenario.candidateGeneration = {
      uninominalToPlurinominalShare: 0.4,
      plurinominalCandidacyCountShares: [0.5, 0.5, 0, 0, 0]
    };

    const projection = projectScenarioOntoPoliticsSource(source(), candidateScenario, { simulations: 1 });

    expect(projection.source.frazione_uni_in_pluri).toBe(0.4);
    expect(projection.source.frazioni_pluricandidature).toEqual([2 / 3, 1 / 3, 0, 0, 0]);
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
    expect(projection.source.comuni_liste.map((row) => row.LISTA)).toEqual(['astensione', 'Lista A', 'Lista C']);
    expect(projection.source.camera.candidati_pluri.map((row) => row.LISTA)).toEqual(['Lista A', 'Lista C']);
    expect(projection.rows.map((row) => row.list)).toEqual(['Lista A', 'Lista C']);
  });

  test('applies partial share overrides and recalculates unspecified lists proportionally', () => {
    const projection = projectScenarioOntoPoliticsSource(
      source(),
      scenario([
        { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 50, shareOverride: true },
        { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
        { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
      ]),
      { currentDate: '2026-06-04', simulations: 1 }
    );

    expect(projection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => Number(row.PERCENTUALE.toFixed(3)))).toEqual([
      0.3, 0.12, 0.18
    ]);
    expect(projection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => [row.LISTA, row.DATA])).toEqual([
      ['Lista A', '2026-06-04T00:00:00.000Z'],
      ['Lista B', '2022-09-25T00:00:00.000Z'],
      ['Lista C', '2022-09-25T00:00:00.000Z']
    ]);
    expect(projection.rows.find((row) => row.list === 'Lista A')?.projectedShare).toBe(50);
  });

  test('normalizes all active valid-vote overrides when their total is not 100', () => {
    const projection = projectScenarioOntoPoliticsSource(
      source(),
      scenario([
        { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 20, shareOverride: true },
        { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: true },
        { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 20, shareOverride: true }
      ]),
      { currentDate: '2026-06-04', simulations: 1 }
    );

    expect(projection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => Number(row.PERCENTUALE.toFixed(3)))).toEqual([
      0.2, 0.2, 0.2
    ]);
    expect(projection.rows.filter((row) => row.status === 'active').map((row) => Number((row.projectedShare ?? 0).toFixed(2)))).toEqual([
      33.33, 33.33, 33.33
    ]);
    expect(projection.warnings).toEqual([
      expect.objectContaining({
        code: 'POLITICS_SCENARIO_OVERRIDES_RENORMALIZED',
        message:
          'All active lists have explicit valid-vote share overrides totaling 60%; they were normalized to 100% across active lists before conversion to elector fractions.'
      })
    ]);
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

  test('applies local valid-vote share overrides by recomputing municipal deltas', () => {
    const localScenario = scenario([
      { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false },
      { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
      { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
    ]);
    localScenario.localShareOverrides = [
      { id: 'local-a', scope: 'municipality', locationCode: '1', list: 'Lista A', startingShare: 50 }
    ];

    const projection = projectScenarioOntoPoliticsSource(source(), localScenario, {
      currentDate: '2026-06-04',
      simulations: 1
    });

    expect(localFractions(projection, 1)).toEqual({
      'Lista A': 0.3,
      'Lista B': 0.12,
      'Lista C': 0.18,
      astensione: 0.4
    });
    expect(projection.source.comuni_liste.map((row) => [row.LISTA, row.DATA])).toEqual([
      ['astensione', '2026-06-04T00:00:00.000Z'],
      ['Lista A', '2026-06-04T00:00:00.000Z'],
      ['Lista B', '2026-06-04T00:00:00.000Z'],
      ['Lista C', '2026-06-04T00:00:00.000Z']
    ]);
    expect(projection.warnings).toEqual([]);
  });

  test('normalizes all local list overrides when their valid-vote total is not 100', () => {
    const localScenario = scenario([
      { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false },
      { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
      { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
    ]);
    localScenario.localShareOverrides = [
      { id: 'local-a', scope: 'municipality', locationCode: '1', list: 'Lista A', startingShare: 20 },
      { id: 'local-b', scope: 'municipality', locationCode: '1', list: 'Lista B', startingShare: 20 },
      { id: 'local-c', scope: 'municipality', locationCode: '1', list: 'Lista C', startingShare: 20 }
    ];

    const projection = projectScenarioOntoPoliticsSource(source(), localScenario, {
      currentDate: '2026-06-04',
      simulations: 1
    });

    expect(localFractions(projection, 1)).toEqual({
      'Lista A': 0.2,
      'Lista B': 0.2,
      'Lista C': 0.2,
      astensione: 0.4
    });
    expect(projection.warnings).toEqual([
      expect.objectContaining({
        code: 'POLITICS_SCENARIO_LOCAL_OVERRIDES_RENORMALIZED'
      })
    ]);
  });

  test('applies province valid-vote share overrides while preserving geographic distribution', () => {
    const localScenario = scenario([
      { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false },
      { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
      { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
    ]);
    localScenario.localShareOverrides = [
      { id: 'province-a', scope: 'province', locationCode: '10', list: 'Lista A', startingShare: 40 }
    ];

    const projection = projectScenarioOntoPoliticsSource(aggregateSource(), localScenario, {
      currentDate: '2026-06-04',
      municipalities: municipalities(),
      simulations: 1
    });
    const first = localValidShares(projection, 1);
    const second = localValidShares(projection, 2);

    expect(Number(aggregateValidShare(projection, [1, 2], 'Lista A').toFixed(3))).toBe(0.4);
    expect(first['Lista A']).toBeLessThan(second['Lista A']);
    expect(projection.warnings).toEqual([]);
  });

  test('applies multi-list region overrides and keeps municipal political shares normalized', () => {
    const localScenario = scenario([
      { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false },
      { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
      { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
    ]);
    localScenario.localShareOverrides = [
      { id: 'region-a', scope: 'region', locationCode: '100', list: 'Lista A', startingShare: 35 },
      { id: 'region-b', scope: 'region', locationCode: '100', list: 'Lista B', startingShare: 35 }
    ];

    const projection = projectScenarioOntoPoliticsSource(aggregateSource(), localScenario, {
      currentDate: '2026-06-04',
      municipalities: municipalities(),
      simulations: 1
    });

    expect(Number(aggregateValidShare(projection, [1, 2, 3], 'Lista A').toFixed(3))).toBe(0.35);
    expect(Number(aggregateValidShare(projection, [1, 2, 3], 'Lista B').toFixed(3))).toBe(0.35);
    for (const code of [1, 2, 3]) {
      const shares = localValidShares(projection, code);
      expect(Number(Object.values(shares).reduce((sum, value) => sum + value, 0).toFixed(3))).toBe(1);
    }
    expect(projection.warnings).toEqual([]);
  });

  test('applies province and municipality overrides after broader region overrides', () => {
    const localScenario = scenario([
      { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false },
      { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
      { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
    ]);
    localScenario.localShareOverrides = [
      { id: 'region-a', scope: 'region', locationCode: '100', list: 'Lista A', startingShare: 30 },
      { id: 'province-a', scope: 'province', locationCode: '10', list: 'Lista A', startingShare: 45 },
      { id: 'municipality-a', scope: 'municipality', locationCode: '2', list: 'Lista A', startingShare: 60 }
    ];

    const projection = projectScenarioOntoPoliticsSource(aggregateSource(), localScenario, {
      currentDate: '2026-06-04',
      municipalities: municipalities(),
      simulations: 1
    });

    expect(localValidShares(projection, 2)['Lista A']).toBeCloseTo(0.6, 3);
    expect(Number(aggregateValidShare(projection, [1, 2], 'Lista A').toFixed(3))).not.toBe(0.45);
    expect(projection.warnings.map((warning) => warning.code)).toEqual([
      'POLITICS_SCENARIO_LOCAL_OVERRIDES_OVERLAP',
      'POLITICS_SCENARIO_LOCAL_OVERRIDES_OVERLAP'
    ]);
  });

  test('uses historical correspondences to project renamed scenario lists', () => {
    const renamedScenario = scenario([
      { id: 'x', name: 'Lista X', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false },
      { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
      { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
    ]);
    renamedScenario.listCorrespondences = [
      {
        id: 'history-a-x',
        futureList: 'Lista X',
        pastElection: 'election 2024',
        pastDate: '2024-01-01',
        pastList: 'Historical A',
        factor: 1,
        source: 'manual'
      },
      {
        id: 'history-b',
        futureList: 'Lista B',
        pastElection: 'election 2024',
        pastDate: '2024-01-01',
        pastList: 'Historical B',
        factor: 1,
        source: 'manual'
      },
      {
        id: 'history-c',
        futureList: 'Lista C',
        pastElection: 'election 2024',
        pastDate: '2024-01-01',
        pastList: 'Historical C',
        factor: 1,
        source: 'manual'
      }
    ];

    const projection = projectScenarioOntoPoliticsSource(source(), renamedScenario, {
      historicalVotes: historicalVotes(),
      simulations: 1
    });

    expect(projection.warnings).toEqual([]);
    expect(projection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => row.LISTA)).toEqual([
      'Lista X',
      'Lista B',
      'Lista C'
    ]);
    expect(projection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => Number(row.PERCENTUALE.toFixed(3)))).toEqual([
      0.4, 0.2, 0.2
    ]);
    expect(projection.source.comuni_liste.map((row) => row.LISTA)).toEqual(['astensione', 'Lista B', 'Lista C', 'Lista X']);
    expect(projection.source.camera.candidati_pluri.map((row) => row.LISTA)).toEqual(['Lista X', 'Lista B', 'Lista C']);
    expect(projection.rows.find((row) => row.list === 'Lista X')).toEqual(
      expect.objectContaining({
        parameterSource: 'historical',
        status: 'active'
      })
    );
  });

  test('applies scenario candidate templates to generated new-list slots', () => {
    const templateScenario = scenario([
      { id: 'x', name: 'Lista X', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false },
      { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
      { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
    ]);
    templateScenario.candidateTemplates = [
      {
        id: 'uni-template',
        ramo: 'camera',
        kind: 'uninominal',
        coalition: 'Coalizione A',
        uninominalCode: '10',
        candidateName: 'Candidate Uni',
        birthDate: '1977-01-02'
      },
      {
        id: 'pluri-template',
        ramo: 'camera',
        kind: 'plurinominal',
        list: 'Lista X',
        plurinominalCode: '11',
        candidateNumber: 1,
        minority: false,
        candidateName: 'Candidate Pluri',
        birthDate: '1988-03-04'
      }
    ];

    const projection = projectScenarioOntoPoliticsSource(source(), templateScenario, { simulations: 1 });

    expect(projection.source.camera.candidati_uni.find((row) => row.COALIZIONE === 'Coalizione A' && row.UNI_COD === 10)).toEqual(
      expect.objectContaining({
        CANDIDATO_ID: 'Candidate Uni',
        DATA_NASCITA: '1977-01-02T00:00:00.000Z'
      })
    );
    expect(projection.source.camera.candidati_pluri.find((row) => row.LISTA === 'Lista X' && row.PLURI_COD === 11)).toEqual(
      expect.objectContaining({
        CANDIDATO_ID: 'Candidate Pluri',
        DATA_NASCITA: '1988-03-04T00:00:00.000Z'
      })
    );
    expect(projection.source.senato.candidati_uni.some((row) => row.CANDIDATO_ID === 'Candidate Uni')).toBe(false);
    expect(projection.warnings).toEqual([]);
  });

  test('warns when scenario candidate templates do not match projected slots', () => {
    const templateScenario = scenario([
      { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false }
    ]);
    templateScenario.candidateTemplates = [
      {
        id: 'missing-template',
        ramo: 'camera',
        kind: 'plurinominal',
        list: 'Lista A',
        plurinominalCode: 'missing',
        candidateNumber: 1,
        minority: false,
        candidateName: 'Missing Candidate',
        birthDate: null
      }
    ];

    const projection = projectScenarioOntoPoliticsSource(source(), templateScenario, { simulations: 1 });

    expect(projection.warnings).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          code: 'POLITICS_SCENARIO_CANDIDATE_TEMPLATES_UNUSED'
        })
      ])
    );
  });

  test('splits one historical list across two future lists and sends unmatched history to abstention', () => {
    const splitScenario = scenario([
      { id: 'one', name: 'Lista Uno', coalition: 'Coalizione A', color: '#000000', startingShare: 10, shareOverride: false },
      { id: 'two', name: 'Lista Due', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false }
    ]);
    splitScenario.listCorrespondences = [
      {
        id: 'history-a-one',
        futureList: 'Lista Uno',
        pastElection: 'election 2024',
        pastDate: '2024-01-01',
        pastList: 'Historical A',
        factor: 2,
        source: 'manual'
      },
      {
        id: 'history-a-two',
        futureList: 'Lista Due',
        pastElection: 'election 2024',
        pastDate: '2024-01-01',
        pastList: 'Historical A',
        factor: 1,
        source: 'manual'
      }
    ];

    const projection = projectScenarioOntoPoliticsSource(source(), splitScenario, {
      historicalVotes: historicalVotes(),
      simulations: 1
    });

    expect(projection.warnings).toEqual([]);
    expect(projection.source.liste.map((row) => [row.LISTA, Number(row.PERCENTUALE.toFixed(3))])).toEqual([
      ['Lista Uno', 0.267],
      ['Lista Due', 0.133],
      ['astensione', 0.6]
    ]);
    expect(projection.source.comuni_liste.map((row) => row.LISTA)).toEqual(['astensione', 'Lista Due', 'Lista Uno']);
  });

  test('keeps stochastic parameters for active political lists', () => {
    const projection = projectScenarioOntoPoliticsSource(
      source(),
      scenario([
        { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#000000', startingShare: 50, shareOverride: true },
        { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: '#111111', startingShare: 20, shareOverride: false },
        { id: 'c', name: 'Lista C', coalition: 'Coalizione C', color: '#222222', startingShare: 30, shareOverride: false }
      ]),
      { simulations: 1 }
    );

    expect(projection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => row.SIGMA_GLOBAL)).toEqual([
      0.1, 0.1, 0.1
    ]);
  });

  test('keeps new lists active and creates legal candidate slots for new coalitions', () => {
    const projection = projectScenarioOntoPoliticsSource(
      source(),
      scenario([
        { id: 'a', name: 'Lista A', coalition: 'Coalizione Nuova', color: '#000000', startingShare: 10, shareOverride: false },
        { id: 'x', name: 'Lista X', coalition: 'Coalizione Nuova', color: '#333333', startingShare: 15, shareOverride: true }
      ]),
      { simulations: 1 }
    );

    expect(projection.source.liste.filter((row) => row.LISTA !== 'astensione').map((row) => row.LISTA)).toEqual(['Lista A', 'Lista X']);
    expect(projection.source.liste.find((row) => row.LISTA === 'Lista A')?.COALIZIONE).toBe('Coalizione Nuova');
    expect(projection.source.liste.find((row) => row.LISTA === 'Lista X')?.PERCENTUALE).toBeCloseTo(0.09);
    expect(projection.source.camera.candidati_uni).toEqual(
      expect.arrayContaining([
        { COALIZIONE: 'Coalizione Nuova', UNI_COD: 10, LISTA_MINORANZA: null, CANDIDATO_ID: null, DATA_NASCITA: null }
      ])
    );
    expect(projection.source.camera.candidati_pluri.map((row) => row.LISTA)).toEqual(['Lista A', 'Lista X']);
    expect(projection.warnings).toEqual([]);
    expect(projection.rows.find((row) => row.list === 'Lista X')).toEqual(
      expect.objectContaining({
        parameterSource: 'synthetic',
        status: 'active'
      })
    );
  });
});
