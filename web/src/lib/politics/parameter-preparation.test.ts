import { fileURLToPath } from 'node:url';
import { describe, expect, test } from 'vitest';
import { createDefaultPoliticsScenario } from '$lib/scenario/politics';
import { describeWithGeneratedFixtures, loadGeneratedJsonFixture } from '$lib/test/generated-fixtures';
import {
  buildPoliticsParametersFromHistoricalVotes,
  projectHistoricalMunicipalVotes,
  type ProjectedHistoricalMunicipalVoteRow
} from './parameter-preparation';
import type {
  AdministrativeCode,
  PoliticsHistoricalMunicipalListVoteRow,
  PoliticsMunicipalListParameterRow,
  PoliticsPipelineListRow,
  PoliticsScenarioListElectionSnapshotRow,
  PoliticsStaticSnapshot
} from './types';
import type { Scenario } from '$lib/core/types';

const productionStaticSnapshotPath = fileURLToPath(
  new URL('../../../../web/static/data/v1/politics-static.json', import.meta.url)
);

function scenario(): Scenario {
  const lists = [
    { id: 'future-1', name: 'Future One', coalition: 'Coalition A', color: '#111111', startingShare: 0, shareOverride: false },
    { id: 'future-2', name: 'Future Two', coalition: 'Coalition A', color: '#222222', startingShare: 0, shareOverride: false },
    { id: 'future-3', name: 'Future Three', coalition: 'Coalition B', color: '#333333', startingShare: 0, shareOverride: false }
  ];

  return {
    id: 'rich-correspondence-test',
    name: 'Rich correspondence test',
    electionDate: '2027-03-01',
    defaultSource: {
      kind: 'manual',
      electionKind: 'politiche',
      territory: 'Italia',
      dataVersion: 'test'
    },
    globalShareMode: 'mean',
    coalitions: [
      { id: 'coalition-a', name: 'Coalition A', color: '#111111' },
      { id: 'coalition-b', name: 'Coalition B', color: '#222222' }
    ],
    lists,
    listCorrespondences: ['election 2020', 'election 2024'].flatMap((pastElection) => [
      {
        id: `${pastElection}-a-one`,
        futureList: 'Future One',
        pastElection,
        pastDate: pastElection.endsWith('2020') ? '2020-01-01' : '2024-01-01',
        pastList: 'Original A',
        factor: 2,
        source: 'manual' as const
      },
      {
        id: `${pastElection}-a-two`,
        futureList: 'Future Two',
        pastElection,
        pastDate: pastElection.endsWith('2020') ? '2020-01-01' : '2024-01-01',
        pastList: 'Original A',
        factor: 1,
        source: 'manual' as const
      },
      {
        id: `${pastElection}-b-one`,
        futureList: 'Future One',
        pastElection,
        pastDate: pastElection.endsWith('2020') ? '2020-01-01' : '2024-01-01',
        pastList: 'Original B',
        factor: 1,
        source: 'manual' as const
      },
      {
        id: `${pastElection}-c-three`,
        futureList: 'Future Three',
        pastElection,
        pastDate: pastElection.endsWith('2020') ? '2020-01-01' : '2024-01-01',
        pastList: 'Original C',
        factor: 1,
        source: 'manual' as const
      }
    ])
  };
}

function historicalVotes(): PoliticsHistoricalMunicipalListVoteRow[] {
  return [
    { DATA: '2020-01-01T00:00:00.000Z', ELEZIONE: 'election 2020', CODICE_COMUNE: '001', LISTA: 'Original A', VOTI: 60 },
    { DATA: '2020-01-01T00:00:00.000Z', ELEZIONE: 'election 2020', CODICE_COMUNE: '001', LISTA: 'Original B', VOTI: 30 },
    { DATA: '2020-01-01T00:00:00.000Z', ELEZIONE: 'election 2020', CODICE_COMUNE: '001', LISTA: 'Original C', VOTI: 10 },
    { DATA: '2020-01-01T00:00:00.000Z', ELEZIONE: 'election 2020', CODICE_COMUNE: '001', LISTA: 'Original X', VOTI: 20 },
    { DATA: '2020-01-01T00:00:00.000Z', ELEZIONE: 'election 2020', CODICE_COMUNE: '001', LISTA: 'astensione', VOTI: 100 },
    { DATA: '2020-01-01T00:00:00.000Z', ELEZIONE: 'election 2020', CODICE_COMUNE: '002', LISTA: 'Original A', VOTI: 20 },
    { DATA: '2020-01-01T00:00:00.000Z', ELEZIONE: 'election 2020', CODICE_COMUNE: '002', LISTA: 'Original B', VOTI: 40 },
    { DATA: '2020-01-01T00:00:00.000Z', ELEZIONE: 'election 2020', CODICE_COMUNE: '002', LISTA: 'Original C', VOTI: 40 },
    { DATA: '2020-01-01T00:00:00.000Z', ELEZIONE: 'election 2020', CODICE_COMUNE: '002', LISTA: 'astensione', VOTI: 100 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: '001', LISTA: 'Original A', VOTI: 120 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: '001', LISTA: 'Original B', VOTI: 60 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: '001', LISTA: 'Original C', VOTI: 20 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: '001', LISTA: 'Original X', VOTI: 30 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: '001', LISTA: 'astensione', VOTI: 100 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: '002', LISTA: 'Original A', VOTI: 30 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: '002', LISTA: 'Original B', VOTI: 30 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: '002', LISTA: 'Original C', VOTI: 40 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: '002', LISTA: 'Original X', VOTI: 20 },
    { DATA: '2024-01-01T00:00:00.000Z', ELEZIONE: 'election 2024', CODICE_COMUNE: '002', LISTA: 'astensione', VOTI: 100 }
  ];
}

function projectedRow(
  rows: readonly ProjectedHistoricalMunicipalVoteRow[],
  commune: AdministrativeCode,
  election: string,
  list: string
): ProjectedHistoricalMunicipalVoteRow {
  const row = rows.find((candidate) => candidate.CODICE_COMUNE === commune && candidate.ELEZIONE === election && candidate.LISTA === list);
  if (!row) throw new Error(`Missing projected row ${commune} ${election} ${list}`);
  return row;
}

function byList<T extends { LISTA: string }>(rows: readonly T[]): Map<string, T> {
  return new Map(rows.map((row) => [row.LISTA, row]));
}

function byKey<T>(rows: readonly T[], key: (row: T) => string): Map<string, T> {
  return new Map(rows.map((row) => [key(row), row]));
}

function expectClose(actual: number, expected: number, tolerance = 1e-9): void {
  expect(Math.abs(actual - expected)).toBeLessThanOrEqual(tolerance);
}

function expectPipelineListRowsClose(actual: readonly PoliticsPipelineListRow[], expected: readonly PoliticsPipelineListRow[]): void {
  const actualByList = byList(actual);

  expect([...actualByList.keys()]).toEqual(expected.map((row) => row.LISTA));
  for (const expectedRow of expected) {
    const actualRow = actualByList.get(expectedRow.LISTA);
    expect(actualRow).toBeDefined();
    expect(actualRow?.COALIZIONE).toBe(expectedRow.COALIZIONE);
    expect(actualRow?.DATA).toBe(expectedRow.DATA);
    expectClose(actualRow?.PERCENTUALE ?? Number.NaN, expectedRow.PERCENTUALE, 1e-10);
    expectClose(actualRow?.LOGIT_P ?? Number.NaN, expectedRow.LOGIT_P, 1e-9);
    expectClose(actualRow?.SIGMA_GLOBAL ?? Number.NaN, expectedRow.SIGMA_GLOBAL, 1e-10);
  }
}

function expectListElectionRowsClose(
  actual: readonly PoliticsScenarioListElectionSnapshotRow[],
  expected: readonly PoliticsScenarioListElectionSnapshotRow[]
): void {
  const actualByKey = byKey(actual, (row) => [row.DATA, row.ELEZIONE, row.LISTA].join('|'));

  expect(actualByKey.size).toBe(expected.length);
  for (const expectedRow of expected) {
    const actualRow = actualByKey.get([expectedRow.DATA, expectedRow.ELEZIONE, expectedRow.LISTA].join('|'));
    expect(actualRow).toBeDefined();
    expectClose(actualRow?.VOTI ?? Number.NaN, expectedRow.VOTI, 1e-5);
    expectClose(actualRow?.PERCENTUALE ?? Number.NaN, expectedRow.PERCENTUALE, 1e-11);
    expectClose(actualRow?.LOGIT_P ?? Number.NaN, expectedRow.LOGIT_P, 1e-9);
  }
}

function expectMunicipalRowsClose(
  actual: readonly PoliticsMunicipalListParameterRow[],
  expected: readonly PoliticsMunicipalListParameterRow[]
): void {
  const actualByKey = byKey(actual, (row) => [String(row.CODICE_COMUNE), row.LISTA].join('|'));

  expect(actualByKey.size).toBe(expected.length);
  for (const expectedRow of expected) {
    const actualRow = actualByKey.get([String(expectedRow.CODICE_COMUNE), expectedRow.LISTA].join('|'));
    expect(actualRow).toBeDefined();
    expect(actualRow?.DATA).toBe(expectedRow.DATA);
    expectClose(actualRow?.DELTA ?? Number.NaN, expectedRow.DELTA, 1e-9);
    expectClose(actualRow?.SIGMA_DELTA ?? Number.NaN, expectedRow.SIGMA_DELTA, 1e-10);
  }
}

describe('politics historical correspondence parameter preparation', () => {
  test('splits, merges, normalizes factors, and sends unmapped votes to abstention', () => {
    const projected = projectHistoricalMunicipalVotes(historicalVotes(), scenario());

    expectClose(projectedRow(projected, '001', 'election 2020', 'Future One').VOTI, 70);
    expectClose(projectedRow(projected, '001', 'election 2020', 'Future Two').VOTI, 20);
    expectClose(projectedRow(projected, '001', 'election 2020', 'Future Three').VOTI, 10);
    expectClose(projectedRow(projected, '001', 'election 2020', 'astensione').VOTI, 120);
    expectClose(projectedRow(projected, '001', 'election 2024', 'Future One').VOTI, 140);
    expectClose(projectedRow(projected, '001', 'election 2024', 'Future Two').VOTI, 40);
    expectClose(projectedRow(projected, '001', 'election 2024', 'Future Three').VOTI, 20);
    expectClose(projectedRow(projected, '001', 'election 2024', 'astensione').VOTI, 130);
    expectClose(projectedRow(projected, '001', 'election 2024', 'Future One').ELETTORI, 330);
    expectClose(projectedRow(projected, '002', 'election 2024', 'astensione').VOTI, 120);
  });

  test('builds starting list, election, and local-delta parameters from projected history', () => {
    const result = buildPoliticsParametersFromHistoricalVotes(historicalVotes(), scenario());
    const listRows = byList(result.liste);
    const listElectionRows = byKey(result.liste_elezioni, (row) => `${row.ELEZIONE}|${row.LISTA}`);
    const municipalRows = byKey(result.comuni_liste, (row) => `${String(row.CODICE_COMUNE)}|${row.LISTA}`);

    expect(result.warnings).toEqual([]);
    expect(result.liste.map((row) => row.LISTA)).toEqual(['Future One', 'Future Two', 'Future Three', 'astensione']);
    expectClose(listRows.get('Future One')?.PERCENTUALE ?? Number.NaN, 190 / 550);
    expectClose(listRows.get('Future Two')?.PERCENTUALE ?? Number.NaN, 50 / 550);
    expectClose(listRows.get('Future Three')?.PERCENTUALE ?? Number.NaN, 60 / 550);
    expectClose(listRows.get('astensione')?.PERCENTUALE ?? Number.NaN, 250 / 550);
    expectClose(listElectionRows.get('election 2024|Future One')?.VOTI ?? Number.NaN, 190);
    expect(municipalRows.get('001|Future One')?.DATA).toBe('2024-01-01T00:00:00.000Z');
    expect(Number.isFinite(municipalRows.get('001|Future One')?.DELTA)).toBe(true);
  });
});

describeWithGeneratedFixtures('politics parameter preparation production parity', [productionStaticSnapshotPath], () => {
  const snapshot = loadGeneratedJsonFixture<PoliticsStaticSnapshot>(productionStaticSnapshotPath);

  test('rebuilds the R-exported default politics parameters from raw historical votes and bundled correspondences', { timeout: 30_000 }, () => {
    const result = buildPoliticsParametersFromHistoricalVotes(snapshot.data.comuni_liste_elezioni ?? [], createDefaultPoliticsScenario(), {
      percentualiPartenza: 'europee'
    });

    expectPipelineListRowsClose(result.liste, snapshot.default_scenario.liste);
    expectListElectionRowsClose(result.liste_elezioni, snapshot.default_scenario.liste_elezioni ?? []);
    expectMunicipalRowsClose(result.comuni_liste, snapshot.default_scenario.comuni_liste);
  });
});
