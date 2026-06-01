import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, test } from 'vitest';
import { preparePoliticsVoteTables } from './vote-preparation';
import type { PoliticsVotePreparationFixture, Ramo } from './types';

const fixturePath = fileURLToPath(
  new URL('../../../../test/fixtures/politiche/vote_preparation.json', import.meta.url)
);

function loadFixture(): PoliticsVotePreparationFixture {
  return JSON.parse(readFileSync(fixturePath, 'utf8')) as PoliticsVotePreparationFixture;
}

describe('politics vote preparation', () => {
  const fixture = loadFixture();

  test('has the expected vote-preparation fixture shape', () => {
    expect(fixture.metadata.schema_version).toBe(1);
    expect(fixture.rami.camera.source.uni_liste_sim.length).toBeGreaterThan(
      fixture.rami.camera.expected.uni_liste_sim.length
    );
    expect(fixture.rami.senato.source.uni_liste_sim.length).toBeGreaterThan(
      fixture.rami.senato.expected.uni_liste_sim.length
    );
    expect(fixture.rami.camera.source.uni_liste_sim.some((row) => row.LISTA === 'astensione')).toBe(true);
    expect(fixture.rami.senato.source.uni_liste_sim.some((row) => row.LISTA === '__lista_non_valida__')).toBe(true);
  });

  for (const ramo of ['camera', 'senato'] as Ramo[]) {
    test(`${ramo}: prepares generated vote tables like R`, () => {
      expect(preparePoliticsVoteTables(fixture.rami[ramo].source)).toEqual(fixture.rami[ramo].expected);
    });
  }
});
