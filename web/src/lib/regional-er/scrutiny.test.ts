import { fileURLToPath } from 'node:url';
import { expect, test } from 'vitest';
import { describeWithGeneratedFixtures, loadGeneratedJsonFixture } from '$lib/test/generated-fixtures';
import { runRegionalErScrutiny } from './scrutiny';
import type { RegionalErGoldenFixture, RegionalErScrutinyOutput } from './types';

const goldenFixturePath = fileURLToPath(
  new URL('../../../../test/fixtures/regionali-er/debug_scrutiny.json', import.meta.url)
);

type RegionalErOutputTableName = keyof RegionalErScrutinyOutput;
type RegionalErOutputRow = RegionalErScrutinyOutput[RegionalErOutputTableName][number];

const tableSortKeys: Record<RegionalErOutputTableName, string[]> = {
  coalizioni: ['COALIZIONE'],
  liste: ['LISTA'],
  prov_lista: ['PROVINCIA', 'LISTA']
};

function rowSortKey(row: RegionalErOutputRow, keys: readonly string[]): string {
  return keys.map((key) => String((row as unknown as Record<string, unknown>)[key] ?? '')).join('\u001f');
}

function sortedRows<T extends RegionalErOutputRow>(rows: readonly T[], keys: readonly string[]): T[] {
  return [...rows].sort((left, right) => rowSortKey(left, keys).localeCompare(rowSortKey(right, keys), 'it'));
}

function expectRowsToMatch(actualRows: readonly RegionalErOutputRow[], expectedRows: readonly RegionalErOutputRow[], keys: readonly string[]): void {
  const actual = sortedRows(actualRows, keys);
  const expected = sortedRows(expectedRows, keys);

  expect(actual).toHaveLength(expected.length);
  for (let index = 0; index < expected.length; index += 1) {
    const actualRow = actual[index] as unknown as Record<string, unknown>;
    const expectedRow = expected[index] as unknown as Record<string, unknown>;

    expect(Object.keys(actualRow).sort()).toEqual(Object.keys(expectedRow).sort());
    for (const column of Object.keys(expectedRow)) {
      const actualValue = actualRow[column];
      const expectedValue = expectedRow[column];
      if (typeof expectedValue === 'number') {
        expect(actualValue, column).toBeTypeOf('number');
        expect(actualValue as number, column).toBeCloseTo(expectedValue, 8);
      } else {
        expect(actualValue, column).toBe(expectedValue);
      }
    }
  }
}

describeWithGeneratedFixtures('regional Emilia-Romagna scrutiny golden parity', [goldenFixturePath], () => {
  const fixture = loadGeneratedJsonFixture<RegionalErGoldenFixture>(goldenFixturePath);

  for (const simulation of fixture.simulations) {
    test(`matches R scrutiny output for simulation ${simulation.sim}`, () => {
      const actual = runRegionalErScrutiny(simulation.input, fixture.context, {
        seed: `regional-er-golden-${simulation.sim}`
      });

      for (const tableName of Object.keys(tableSortKeys) as RegionalErOutputTableName[]) {
        expectRowsToMatch(actual[tableName], simulation.expected[tableName], tableSortKeys[tableName]);
      }
    });
  }
});
