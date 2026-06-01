import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, test } from 'vitest';
import { adaptGeneratedPoliticsFixture } from './adapter';
import type { AdaptedPoliticsSimulation } from './adapter';
import type { PoliticsGeneratedAdapterFixture, PoliticsGoldenFixture, Ramo } from './types';

const generatedFixturePath = fileURLToPath(
  new URL('../../../../test/fixtures/politiche/generated_adapter.json', import.meta.url)
);
const goldenFixturePath = fileURLToPath(
  new URL('../../../../test/fixtures/politiche/debug_scrutinio.json', import.meta.url)
);

function loadGeneratedFixture(): PoliticsGeneratedAdapterFixture {
  return JSON.parse(readFileSync(generatedFixturePath, 'utf8')) as PoliticsGeneratedAdapterFixture;
}

function loadGoldenFixture(): PoliticsGoldenFixture {
  return JSON.parse(readFileSync(goldenFixturePath, 'utf8')) as PoliticsGoldenFixture;
}

function bySimulation(rows: AdaptedPoliticsSimulation[]): Map<number, AdaptedPoliticsSimulation> {
  return new Map(rows.map((row) => [row.sim, row]));
}

describe('politics generated-table adapter', () => {
  const generated = loadGeneratedFixture();
  const golden = loadGoldenFixture();
  const adapted = adaptGeneratedPoliticsFixture(generated);

  test('has the expected generated source fixture shape', () => {
    expect(generated.metadata.schema_version).toBe(1);
    expect(generated.rami.camera.uni_liste_sim.length).toBeGreaterThan(0);
    expect(generated.rami.camera.candidati_uni_sim.length).toBeGreaterThan(0);
    expect(generated.rami.camera.candidati_pluri_sim.length).toBeGreaterThan(0);
    expect(generated.rami.senato.uni_liste_sim.length).toBeGreaterThan(0);
    expect(generated.rami.senato.candidati_uni_sim.length).toBeGreaterThan(0);
    expect(generated.rami.senato.candidati_pluri_sim.length).toBeGreaterThan(0);
  });

  for (const ramo of ['camera', 'senato'] as Ramo[]) {
    test(`${ramo}: builds the same direct scrutiny context as R`, () => {
      expect(adapted[ramo].context).toEqual({
        ramo,
        liste_naz: golden.rami[ramo].liste_naz,
        totali_pluri: golden.rami[ramo].totali_pluri,
        totale_seggi: golden.rami[ramo].totale_seggi
      });
    });

    test(`${ramo}: builds the same per-simulation direct scrutiny inputs as R`, () => {
      const adaptedBySim = bySimulation(adapted[ramo].simulations);

      expect([...adaptedBySim.keys()]).toEqual(golden.rami[ramo].simulations.map((simulation) => simulation.sim));

      for (const simulation of golden.rami[ramo].simulations) {
        expect(adaptedBySim.get(simulation.sim)?.input).toEqual(simulation.input);
      }
    });
  }
});
