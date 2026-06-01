import { describe, expect, test } from 'vitest';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { runInitialPoliticsScrutiny } from './scrutiny';
import type { CandidatoUniResultRow, PoliticsGoldenFixture, PoliticsGoldenSimulation, Ramo } from './types';

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

describe('politics golden fixture', () => {
  const fixture = loadFixture();

  test('has the expected direct scrutiny fixture shape', () => {
    expect(fixture.metadata.schema_version).toBe(1);
    expect(fixture.rami.camera.simulations).toHaveLength(10);
    expect(fixture.rami.senato.simulations).toHaveLength(10);
    expect(fixture.rami.camera.simulations[3].warnings).toHaveLength(1);
    expect(fixture.rami.senato.simulations[3].warnings).toHaveLength(1);
  });

  for (const ramo of ['camera', 'senato'] as Ramo[]) {
    for (const simulation of fixture.rami[ramo].simulations as PoliticsGoldenSimulation[]) {
      test(`${ramo} sim ${simulation.sim}: elects uninominal candidates like R`, () => {
        const actual = runInitialPoliticsScrutiny(simulation.input).candidati_uni;
        expect(normalizeUninominal(actual)).toEqual(normalizeUninominal(simulation.expected.candidati_uni));
      });
    }
  }
});
