import { fileURLToPath } from 'node:url';
import { describe, expect, test } from 'vitest';
import { hasGeneratedFixture, loadGeneratedJsonFixture } from '$lib/test/generated-fixtures';
import {
  defaultPoliticsScrutinyAlgorithmId,
  politicsScrutinyAlgorithms,
  resolvePoliticsScrutinyAlgorithm
} from './scrutiny-algorithms';
import { runPoliticsScrutiny } from './scrutiny';
import type { PoliticsGoldenFixture } from './types';

const fixturePath = fileURLToPath(
  new URL('../../../../test/fixtures/politiche/debug_scrutinio.json', import.meta.url)
);

function loadFixture(): PoliticsGoldenFixture {
  return loadGeneratedJsonFixture<PoliticsGoldenFixture>(fixturePath);
}

describe('politics scrutiny algorithm registry', () => {
  test('registers the R-parity algorithm as the default implementation', () => {
    const resolution = resolvePoliticsScrutinyAlgorithm();

    expect(resolution.algorithm.id).toBe(defaultPoliticsScrutinyAlgorithmId);
    expect(resolution.fallback).toBe(false);
    expect(politicsScrutinyAlgorithms.map((algorithm) => algorithm.id)).toEqual([defaultPoliticsScrutinyAlgorithmId]);
  });

  test('falls back to the default algorithm for an unknown requested id', () => {
    const resolution = resolvePoliticsScrutinyAlgorithm('unknown-law-review-draft');

    expect(resolution.requestedId).toBe('unknown-law-review-draft');
    expect(resolution.algorithm.id).toBe(defaultPoliticsScrutinyAlgorithmId);
    expect(resolution.fallback).toBe(true);
  });

  const fixtureTest = hasGeneratedFixture(fixturePath) ? test : test.skip;

  fixtureTest('runs the same implementation as the direct R-parity scrutiny function', () => {
    const fixture = loadFixture();
    const ramoFixture = fixture.rami.camera;
    const simulation = ramoFixture.simulations[0];
    const context = {
      ramo: 'camera' as const,
      liste_naz: ramoFixture.liste_naz,
      totali_pluri: ramoFixture.totali_pluri,
      totale_seggi: ramoFixture.totale_seggi
    };
    const resolution = resolvePoliticsScrutinyAlgorithm(defaultPoliticsScrutinyAlgorithmId);

    expect(resolution.algorithm.run(simulation.input, context)).toEqual(runPoliticsScrutiny(simulation.input, context));
  });
});
