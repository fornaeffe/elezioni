import { describe, expect, test } from 'vitest';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { runEarlyPoliticsScrutiny, runInitialPoliticsScrutiny, runPoliticsScrutinyTrace } from './scrutiny';
import type {
  CandidatoUniResultRow,
  PoliticsEarlyTrace,
  PoliticsGoldenFixture,
  PoliticsGoldenSimulation,
  PoliticsScrutinyTrace,
  Ramo
} from './types';

const fixturePath = fileURLToPath(
  new URL('../../../../test/fixtures/politiche/debug_scrutinio.json', import.meta.url)
);

function loadFixture(): PoliticsGoldenFixture {
  return JSON.parse(readFileSync(fixturePath, 'utf8')) as PoliticsGoldenFixture;
}

function uninominalKey(row: CandidatoUniResultRow): string {
  return [row.CIRCOSCRIZIONE, row.COLLEGIOPLURINOMINALE, row.COLLEGIOUNINOMINALE, row.CANDIDATO].join('|');
}

function normalizeUninominal(rows: CandidatoUniResultRow[]): CandidatoUniResultRow[] {
  return rows
    .map((row) => ({
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      COLLEGIOPLURINOMINALE: row.COLLEGIOPLURINOMINALE,
      COLLEGIOUNINOMINALE: row.COLLEGIOUNINOMINALE,
      CANDIDATO: row.CANDIDATO,
      ELETTO: row.ELETTO
    }))
    .sort((left, right) => uninominalKey(left).localeCompare(uninominalKey(right)));
}

function normalizeValue(value: unknown): unknown {
  if (typeof value !== 'number') return value;

  if (Object.is(value, -0)) return 0;
  return Math.round(value * 1_000_000_000) / 1_000_000_000;
}

function normalizeRows<T extends object>(rows: T[], keyColumns: readonly string[]): Record<string, unknown>[] {
  return rows
    .map((row) =>
      Object.fromEntries(Object.entries(row).map(([key, value]) => [key, normalizeValue(value)]))
    )
    .sort((left, right) => {
      const leftKey = keyColumns.map((column) => String(left[column])).join('|');
      const rightKey = keyColumns.map((column) => String(right[column])).join('|');
      return leftKey.localeCompare(rightKey);
    });
}

function expectRowsToMatch<T extends object>(
  actual: T[],
  expected: T[],
  keyColumns: readonly string[]
): void {
  expect(normalizeRows(actual, keyColumns)).toEqual(normalizeRows(expected, keyColumns));
}

function expectEarlyTraceToMatch(actual: PoliticsEarlyTrace, expected: PoliticsEarlyTrace): void {
  expectRowsToMatch(actual.candidati_uni_elezione, expected.candidati_uni_elezione, [
    'CIRCOSCRIZIONE',
    'COLLEGIOPLURINOMINALE',
    'COLLEGIOUNINOMINALE',
    'CANDIDATO'
  ]);
  expectRowsToMatch(actual.candidati_uni_attribuzione, expected.candidati_uni_attribuzione, [
    'CIRCOSCRIZIONE',
    'COLLEGIOPLURINOMINALE',
    'COLLEGIOUNINOMINALE',
    'CANDIDATO'
  ]);
  expectRowsToMatch(actual.liste_uni_cifre, expected.liste_uni_cifre, [
    'CIRCOSCRIZIONE',
    'COLLEGIOPLURINOMINALE',
    'COLLEGIOUNINOMINALE',
    'CANDIDATO',
    'LISTA'
  ]);
  expectRowsToMatch(actual.liste_pluri_cifre, expected.liste_pluri_cifre, [
    'CIRCOSCRIZIONE',
    'COLLEGIOPLURINOMINALE',
    'LISTA'
  ]);
  expectRowsToMatch(actual.liste_circ_cifre, expected.liste_circ_cifre, ['CIRCOSCRIZIONE', 'LISTA']);
  expectRowsToMatch(actual.candidati_uni_graduatoria, expected.candidati_uni_graduatoria, [
    'CIRCOSCRIZIONE',
    'COLLEGIOPLURINOMINALE',
    'COLLEGIOUNINOMINALE',
    'CANDIDATO'
  ]);
  expectRowsToMatch(actual.totali_circ, expected.totali_circ, ['CIRCOSCRIZIONE']);
}

function expectThresholdTraceToMatch(actual: PoliticsScrutinyTrace, expected: PoliticsScrutinyTrace): void {
  expect(normalizeValue(actual.totale_naz)).toBe(normalizeValue(expected.totale_naz));
  expectRowsToMatch(actual.liste_naz_soglie, expected.liste_naz_soglie, ['LISTA']);
  expectRowsToMatch(actual.liste_circ_soglie, expected.liste_circ_soglie, ['CIRCOSCRIZIONE', 'LISTA']);
  expectRowsToMatch(actual.coal_naz_soglie, expected.coal_naz_soglie, ['COALIZIONE']);
  expectRowsToMatch(actual.coal_circ_cifre, expected.coal_circ_cifre, ['CIRCOSCRIZIONE', 'COALIZIONE']);
}

function expectCameraRipartoTraceToMatch(actual: PoliticsScrutinyTrace, expected: PoliticsScrutinyTrace): void {
  expect(normalizeValue(actual.camera_riparto.seggi_proporzionale)).toBe(
    normalizeValue(expected.camera_riparto.seggi_proporzionale)
  );
  expect(normalizeValue(actual.camera_riparto.totale_naz_riparto)).toBe(
    normalizeValue(expected.camera_riparto.totale_naz_riparto)
  );
  expect(normalizeValue(actual.camera_riparto.quoziente_elettorale_naz)).toBe(
    normalizeValue(expected.camera_riparto.quoziente_elettorale_naz)
  );
  expect(normalizeValue(actual.camera_riparto.ancora_da_attribuire)).toBe(
    normalizeValue(expected.camera_riparto.ancora_da_attribuire)
  );
  expectRowsToMatch(actual.camera_riparto.riparto_naz, expected.camera_riparto.riparto_naz, [
    'SOGGETTO_RIPARTO'
  ]);
  expectRowsToMatch(actual.camera_riparto.ammesse_naz, expected.camera_riparto.ammesse_naz, [
    'SOGGETTO_RIPARTO',
    'LISTA'
  ]);
  expectRowsToMatch(actual.camera_riparto.liste_naz_riparto, expected.camera_riparto.liste_naz_riparto, [
    'LISTA'
  ]);
}

function expectCircRipartoTraceToMatch(actual: PoliticsScrutinyTrace, expected: PoliticsScrutinyTrace): void {
  expectRowsToMatch(actual.circ_riparto.totali_circ, expected.circ_riparto.totali_circ, ['CIRCOSCRIZIONE']);
  expectRowsToMatch(actual.circ_riparto.liste_circ, expected.circ_riparto.liste_circ, [
    'CIRCOSCRIZIONE',
    'LISTA'
  ]);
  expectRowsToMatch(actual.circ_riparto.riparto_circ, expected.circ_riparto.riparto_circ, [
    'CIRCOSCRIZIONE',
    'SOGGETTO_RIPARTO'
  ]);
  expectRowsToMatch(actual.circ_riparto.riparto_naz, expected.circ_riparto.riparto_naz, ['SOGGETTO_RIPARTO']);
}

function expectInternalCircRipartoTraceToMatch(actual: PoliticsScrutinyTrace, expected: PoliticsScrutinyTrace): void {
  expectRowsToMatch(actual.internal_circ_riparto.liste_circ, expected.internal_circ_riparto.liste_circ, [
    'CIRCOSCRIZIONE',
    'LISTA'
  ]);
  expectRowsToMatch(actual.internal_circ_riparto.riparto_circ, expected.internal_circ_riparto.riparto_circ, [
    'CIRCOSCRIZIONE',
    'SOGGETTO_RIPARTO'
  ]);
  expectRowsToMatch(actual.internal_circ_riparto.ammesse_circ, expected.internal_circ_riparto.ammesse_circ, [
    'CIRCOSCRIZIONE',
    'SOGGETTO_RIPARTO',
    'LISTA'
  ]);
  expectRowsToMatch(actual.internal_circ_riparto.ammesse_naz, expected.internal_circ_riparto.ammesse_naz, ['LISTA']);
}

describe('politics golden fixture', () => {
  const fixture = loadFixture();

  test('has the expected direct scrutiny fixture shape', () => {
    expect(fixture.metadata.schema_version).toBe(6);
    expect(fixture.rami.camera.simulations).toHaveLength(10);
    expect(fixture.rami.senato.simulations).toHaveLength(10);
    expect(fixture.rami.camera.simulations[3].warnings).toHaveLength(1);
    expect(fixture.rami.senato.simulations[3].warnings).toHaveLength(1);
    expect(fixture.rami.camera.simulations[0].trace.liste_uni_cifre.length).toBeGreaterThan(0);
    expect(fixture.rami.camera.simulations[0].trace.liste_naz_soglie.length).toBeGreaterThan(0);
    expect(fixture.rami.camera.simulations[0].trace.camera_riparto.riparto_naz.length).toBeGreaterThan(0);
    expect(fixture.rami.senato.simulations[0].trace.camera_riparto.riparto_naz).toHaveLength(0);
    expect(fixture.rami.camera.simulations[0].trace.circ_riparto.riparto_circ.length).toBeGreaterThan(0);
    expect(fixture.rami.senato.simulations[0].trace.circ_riparto.riparto_circ.length).toBeGreaterThan(0);
    expect(fixture.rami.camera.simulations[0].trace.internal_circ_riparto.ammesse_circ.length).toBeGreaterThan(0);
    expect(fixture.rami.senato.simulations[0].trace.internal_circ_riparto.ammesse_circ.length).toBeGreaterThan(0);
  });

  for (const ramo of ['camera', 'senato'] as Ramo[]) {
    for (const simulation of fixture.rami[ramo].simulations as PoliticsGoldenSimulation[]) {
      test(`${ramo} sim ${simulation.sim}: elects uninominal candidates like R`, () => {
        const actual = runInitialPoliticsScrutiny(simulation.input).candidati_uni;
        expect(normalizeUninominal(actual)).toEqual(normalizeUninominal(simulation.expected.candidati_uni));
      });

      test(`${ramo} sim ${simulation.sim}: matches early scrutiny trace like R`, () => {
        expectEarlyTraceToMatch(runEarlyPoliticsScrutiny(simulation.input), simulation.trace);
      });

      test(`${ramo} sim ${simulation.sim}: matches national threshold trace like R`, () => {
        const actual = runPoliticsScrutinyTrace(simulation.input, {
          ramo,
          liste_naz: fixture.rami[ramo].liste_naz,
          totali_pluri: fixture.rami[ramo].totali_pluri,
          totale_seggi: fixture.rami[ramo].totale_seggi
        });
        expectEarlyTraceToMatch(actual, simulation.trace);
        expectThresholdTraceToMatch(actual, simulation.trace);
        expectCameraRipartoTraceToMatch(actual, simulation.trace);
        expectCircRipartoTraceToMatch(actual, simulation.trace);
        expectInternalCircRipartoTraceToMatch(actual, simulation.trace);
      });
    }
  }
});
