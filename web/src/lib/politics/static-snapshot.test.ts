import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, test } from 'vitest';
import { buildPoliticsPipelineSourceFromSnapshot } from './static-snapshot';
import type { PoliticsPipelineSource, PoliticsStaticSnapshot } from './types';

interface PipelineSourceFixture {
  metadata: {
    schema_version: number;
    source: string;
    purpose: string;
  };
  source: PoliticsPipelineSource;
}

const pipelineSourcePath = fileURLToPath(
  new URL('../../../../web/static/data/v1/politics-pipeline-source-debug.json', import.meta.url)
);
const staticSnapshotPath = fileURLToPath(
  new URL('../../../../web/static/data/v1/politics-static-debug.json', import.meta.url)
);

function loadJson<T>(path: string): T {
  return JSON.parse(readFileSync(path, 'utf8')) as T;
}

describe('politics static snapshot bridge', () => {
  const pipelineSource = loadJson<PipelineSourceFixture>(pipelineSourcePath).source;
  const staticSnapshot = loadJson<PoliticsStaticSnapshot>(staticSnapshotPath);

  test('splits reusable data from the default scenario', () => {
    expect(staticSnapshot.metadata.schema_version).toBe(1);
    expect(staticSnapshot.data.base_dati).toHaveLength(pipelineSource.base_dati.length);
    expect(staticSnapshot.data.camera.uni).toHaveLength(pipelineSource.camera.uni.length);
    expect(staticSnapshot.default_scenario.liste).toHaveLength(pipelineSource.liste.length);
  });

  test('reconstructs the existing pipeline source exactly', () => {
    const actual = buildPoliticsPipelineSourceFromSnapshot(staticSnapshot, {
      simulations: pipelineSource.simulazioni
    });

    expect(actual).toEqual(pipelineSource);
  });
});
