import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, test } from 'vitest';
import { generateVotes, type VoteGenerationListRow, type VoteGenerationLocalRow, type VoteGenerationOutputRow } from './vote-generation';

interface VoteGenerationFixture {
  metadata: {
    schema_version: number;
    random_seed: number;
  };
  input: {
    data_elezione: string;
    simulazioni: number;
    colonna_localita: string;
    liste: VoteGenerationListRow[];
    comuni_liste: VoteGenerationLocalRow[];
  };
  normal_draws: Array<{
    phase: 'global' | 'local';
    LISTA: string;
    SIM: number;
    LOCALITA: number | null;
    mean: number;
    sd: number;
    value: number;
  }>;
  expected: VoteGenerationOutputRow[];
}

const fixturePath = fileURLToPath(new URL('../../../../test/fixtures/core/vote_generation.json', import.meta.url));

function loadFixture(): VoteGenerationFixture {
  return JSON.parse(readFileSync(fixturePath, 'utf8')) as VoteGenerationFixture;
}

function expectRowsToMatch(actual: VoteGenerationOutputRow[], expected: VoteGenerationOutputRow[]): void {
  expect(actual).toHaveLength(expected.length);

  for (let index = 0; index < expected.length; index += 1) {
    const actualRow = actual[index];
    const expectedRow = expected[index];

    expect(actualRow.SIM).toBe(expectedRow.SIM);
    expect(actualRow.CODICE_COMUNE).toBe(expectedRow.CODICE_COMUNE);
    expect(actualRow.AREA).toBe(expectedRow.AREA);
    expect(actualRow.LISTA).toBe(expectedRow.LISTA);
    expect(actualRow.DATA).toBe(expectedRow.DATA);
    expect(actualRow.DELTA).toBe(expectedRow.DELTA);
    expect(actualRow.SIGMA_DELTA).toBe(expectedRow.SIGMA_DELTA);
    expect(actualRow.ELETTORI).toBe(expectedRow.ELETTORI);
    expect(actualRow.PERCENTUALE_SIM).toBeCloseTo(expectedRow.PERCENTUALE_SIM, 12);
    expect(actualRow.VOTI_LISTA_SIM).toBe(expectedRow.VOTI_LISTA_SIM);
  }
}

describe('generic vote generation', () => {
  const fixture = loadFixture();

  test('has the expected R trace fixture shape', () => {
    expect(fixture.metadata.schema_version).toBe(1);
    expect(fixture.input.liste).toHaveLength(3);
    expect(fixture.input.comuni_liste).toHaveLength(6);
    expect(fixture.normal_draws).toHaveLength(27);
    expect(fixture.expected).toHaveLength(18);
  });

  test('matches R vote-generation math and row order when R normal draws are injected', () => {
    let drawIndex = 0;
    const actual = generateVotes(fixture.input.comuni_liste, fixture.input.liste, {
      electionDate: fixture.input.data_elezione,
      simulations: fixture.input.simulazioni,
      localityColumn: fixture.input.colonna_localita,
      normal: (mean, standardDeviation) => {
        const draw = fixture.normal_draws[drawIndex];
        drawIndex += 1;

        expect(mean).toBeCloseTo(draw.mean, 12);
        expect(standardDeviation).toBeCloseTo(draw.sd, 12);
        return draw.value;
      }
    });

    expect(drawIndex).toBe(fixture.normal_draws.length);
    expectRowsToMatch(actual, fixture.expected);
  });

  test('is reproducible with the TypeScript seeded normal sampler', () => {
    const first = generateVotes(fixture.input.comuni_liste, fixture.input.liste, {
      electionDate: fixture.input.data_elezione,
      simulations: fixture.input.simulazioni,
      localityColumn: fixture.input.colonna_localita,
      seed: 'vote-generation-test'
    });
    const second = generateVotes(fixture.input.comuni_liste, fixture.input.liste, {
      electionDate: fixture.input.data_elezione,
      simulations: fixture.input.simulazioni,
      localityColumn: fixture.input.colonna_localita,
      seed: 'vote-generation-test'
    });

    expect(second).toEqual(first);
  });
});
