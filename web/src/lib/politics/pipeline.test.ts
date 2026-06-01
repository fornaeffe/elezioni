import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, test } from 'vitest';
import { buildPoliticsDirectScrutinySnapshot } from './pipeline';
import type { CandidateSampleValue } from './candidate-generation';
import type { PoliticsCandidateSampleDraw, PoliticsPipelineFixture } from './types';

const fixturePath = fileURLToPath(new URL('../../../../test/fixtures/politiche/pipeline.json', import.meta.url));

function loadFixture(): PoliticsPipelineFixture {
  return JSON.parse(readFileSync(fixturePath, 'utf8')) as PoliticsPipelineFixture;
}

function arrayFromDrawValue(value: PoliticsCandidateSampleDraw['values']): CandidateSampleValue[] {
  return Array.isArray(value) ? value : [value];
}

describe('politics composed generation pipeline', () => {
  const fixture = loadFixture();

  test('has the expected composed pipeline fixture shape', () => {
    expect(fixture.metadata.schema_version).toBe(1);
    expect(fixture.input.simulazioni).toBe(2);
    expect(fixture.sample_draws).toHaveLength(48);
    expect(fixture.normal_draws).toHaveLength(32);
    expect(fixture.expected.rami.camera.simulations).toHaveLength(2);
    expect(fixture.expected.rami.senato.simulations).toHaveLength(2);
  });

  test('matches R direct-scrutiny inputs when candidate and vote draws are replayed', () => {
    let sampleIndex = 0;
    let normalIndex = 0;

    const actual = buildPoliticsDirectScrutinySnapshot(fixture.input, {
      candidateSample: (values, size, replace, context) => {
        const draw = fixture.sample_draws[sampleIndex];
        sampleIndex += 1;

        expect(values).toEqual(arrayFromDrawValue(draw.values));
        expect(size).toBe(draw.size);
        expect(replace).toBe(draw.replace);
        expect(context).toEqual(draw.context);

        return arrayFromDrawValue(draw.result) as typeof values[number][];
      },
      voteNormal: (mean, standardDeviation) => {
        const draw = fixture.normal_draws[normalIndex];
        normalIndex += 1;

        expect(mean).toBeCloseTo(draw.mean, 12);
        expect(standardDeviation).toBeCloseTo(draw.sd, 12);
        return draw.value;
      }
    });

    expect(sampleIndex).toBe(fixture.sample_draws.length);
    expect(normalIndex).toBe(fixture.normal_draws.length);
    expect(actual).toEqual(fixture.expected);
  });

  test('is reproducible with one TypeScript seed across candidate and vote generation', () => {
    const first = buildPoliticsDirectScrutinySnapshot(fixture.input, { seed: 'politics-pipeline-test' });
    const second = buildPoliticsDirectScrutinySnapshot(fixture.input, { seed: 'politics-pipeline-test' });

    expect(second).toEqual(first);
  });
});
