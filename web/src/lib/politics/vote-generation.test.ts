import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, test } from 'vitest';
import { generatePoliticsVotes } from './vote-generation';
import type { PoliticsGeneratedVoteTables, PoliticsVoteGenerationFixture, Ramo } from './types';

const fixturePath = fileURLToPath(
  new URL('../../../../test/fixtures/politiche/vote_generation.json', import.meta.url)
);

function loadFixture(): PoliticsVoteGenerationFixture {
  return JSON.parse(readFileSync(fixturePath, 'utf8')) as PoliticsVoteGenerationFixture;
}

function expectGeneratedTablesToMatch(actual: PoliticsGeneratedVoteTables, expected: PoliticsGeneratedVoteTables): void {
  for (const ramo of ['camera', 'senato'] as Ramo[]) {
    expect(actual[ramo].uni_liste_sim).toEqual(expected[ramo].uni_liste_sim);
    expect(actual[ramo].candidati_uni_sim).toEqual(expected[ramo].candidati_uni_sim);
  }
}

describe('politics vote generation orchestration', () => {
  const fixture = loadFixture();

  test('has the expected politics vote-generation fixture shape', () => {
    expect(fixture.metadata.schema_version).toBe(1);
    expect(fixture.input.liste).toHaveLength(4);
    expect(fixture.input.comuni_liste).toHaveLength(12);
    expect(fixture.input.base_dati).toHaveLength(3);
    expect(fixture.normal_draws).toHaveLength(48);
    expect(fixture.expected.camera.uni_liste_sim.length).toBeGreaterThan(0);
    expect(fixture.expected.senato.uni_liste_sim.length).toBeGreaterThan(0);
  });

  test('matches R politics vote generation when R normal draws are injected', () => {
    let drawIndex = 0;
    const actual = generatePoliticsVotes(fixture.input, {
      normal: (mean, standardDeviation) => {
        const draw = fixture.normal_draws[drawIndex];
        drawIndex += 1;

        expect(mean).toBeCloseTo(draw.mean, 12);
        expect(standardDeviation).toBeCloseTo(draw.sd, 12);
        return draw.value;
      }
    });

    expect(drawIndex).toBe(fixture.normal_draws.length);
    expectGeneratedTablesToMatch(actual, fixture.expected);
  });

  test('is reproducible with the TypeScript seeded normal sampler', () => {
    const first = generatePoliticsVotes(fixture.input, { seed: 'politics-vote-generation-test' });
    const second = generatePoliticsVotes(fixture.input, { seed: 'politics-vote-generation-test' });

    expect(second).toEqual(first);
  });
});
