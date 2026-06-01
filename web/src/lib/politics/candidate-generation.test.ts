import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, test } from 'vitest';
import { generatePoliticsCandidates, type CandidateSampleValue } from './candidate-generation';
import type { PoliticsCandidateGenerationFixture, PoliticsCandidateSampleDraw } from './types';

const fixturePath = fileURLToPath(
  new URL('../../../../test/fixtures/politiche/candidate_generation.json', import.meta.url)
);

function loadFixture(): PoliticsCandidateGenerationFixture {
  return JSON.parse(readFileSync(fixturePath, 'utf8')) as PoliticsCandidateGenerationFixture;
}

function arrayFromDrawValue(value: PoliticsCandidateSampleDraw['values']): CandidateSampleValue[] {
  return Array.isArray(value) ? value : [value];
}

describe('politics candidate generation', () => {
  const fixture = loadFixture();

  test('has the expected candidate-generation fixture shape', () => {
    expect(fixture.metadata.schema_version).toBe(1);
    expect(fixture.input.simulazioni).toBe(2);
    expect(fixture.input.liste).toHaveLength(4);
    expect(fixture.sample_draws).toHaveLength(56);
    expect(fixture.expected.camera.candidati_uni_sim).toHaveLength(12);
    expect(fixture.expected.camera.candidati_pluri_sim).toHaveLength(26);
    expect(fixture.expected.senato.candidati_uni_sim).toHaveLength(12);
    expect(fixture.expected.senato.candidati_pluri_sim).toHaveLength(26);
  });

  test('matches R candidate generation when R sample draws are replayed', () => {
    let drawIndex = 0;
    const actual = generatePoliticsCandidates(fixture.input, {
      sample: (values, size, replace, context) => {
        const draw = fixture.sample_draws[drawIndex];
        drawIndex += 1;

        expect(values).toEqual(arrayFromDrawValue(draw.values));
        expect(size).toBe(draw.size);
        expect(replace).toBe(draw.replace);
        expect(context).toEqual(draw.context);

        return arrayFromDrawValue(draw.result) as typeof values[number][];
      }
    });

    expect(drawIndex).toBe(fixture.sample_draws.length);
    expect(actual).toEqual(fixture.expected);
  });

  test('is reproducible with the TypeScript seeded sampler', () => {
    const first = generatePoliticsCandidates(fixture.input, { seed: 'politics-candidate-generation-test' });
    const second = generatePoliticsCandidates(fixture.input, { seed: 'politics-candidate-generation-test' });

    expect(second).toEqual(first);
  });
});
