import { fileURLToPath } from 'node:url';
import { describe, expect, test } from 'vitest';
import { describeWithGeneratedFixtures, loadGeneratedJsonFixture } from '$lib/test/generated-fixtures';
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
  new URL('../../../../test/fixtures/politiche/pipeline_source_debug.json', import.meta.url)
);
const staticSnapshotPath = fileURLToPath(
  new URL('../../../../test/fixtures/politiche/static-debug.json', import.meta.url)
);
const productionStaticSnapshotPath = fileURLToPath(
  new URL('../../../../web/static/data/v1/politics-static.json', import.meta.url)
);

function loadJson<T>(path: string): T {
  return loadGeneratedJsonFixture<T>(path);
}

describe('politics static snapshot bridge', () => {
  describeWithGeneratedFixtures('debug static snapshot bridge', [pipelineSourcePath, staticSnapshotPath], () => {
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

  describeWithGeneratedFixtures('production static snapshot bridge', [productionStaticSnapshotPath], () => {
    const productionStaticSnapshot = loadJson<PoliticsStaticSnapshot>(productionStaticSnapshotPath);

    test('loads the production snapshot prepared directly from pinned originals by Node', () => {
      expect(productionStaticSnapshot.metadata.source).toBe('Pinned Node/TypeScript workspace preparation');
      expect(productionStaticSnapshot.metadata.cache_path).toBeUndefined();
      expect(productionStaticSnapshot.metadata.preparation?.geographyVersion).toMatch(/^workspace-geography-v1-/);
      expect(productionStaticSnapshot.metadata.schema_version).toBe(4);
      expect(productionStaticSnapshot.data.base_dati.length).toBeGreaterThan(8000);
      expect(productionStaticSnapshot.data.municipalities?.length).toBeGreaterThan(7800);
      expect(productionStaticSnapshot.data.comuni_liste_elezioni?.length).toBeGreaterThan(400000);
      expect(productionStaticSnapshot.default_scenario.liste).toHaveLength(10);
      expect(productionStaticSnapshot.default_scenario.comuni_liste.length).toBeGreaterThan(70000);
      expect(productionStaticSnapshot.default_scenario.coalizioni).toBeDefined();
      expect(productionStaticSnapshot.default_scenario.corrispondenza_liste).toBeDefined();
      expect(productionStaticSnapshot.data.camera.uni[0]).toEqual(
        expect.objectContaining({
          CIRC_DEN: expect.any(String),
          UNI_DEN: expect.any(String)
        })
      );
      expect(productionStaticSnapshot.data.camera.pluri[0]).toEqual(
        expect.objectContaining({
          CIRC_DEN: expect.any(String),
          MAX_CANDIDATI: expect.any(Number)
        })
      );
      const municipalityCodes = new Set(productionStaticSnapshot.data.municipalities?.map((row) => String(row.CODICE_COMUNE)));
      expect(municipalityCodes.size).toBe(productionStaticSnapshot.data.municipalities?.length);
      expect(productionStaticSnapshot.data.base_dati.every((row) => municipalityCodes.has(String(row.CODICE_COMUNE)))).toBe(
        true
      );

      const actual = buildPoliticsPipelineSourceFromSnapshot(productionStaticSnapshot, {
        simulations: 2
      });

      expect(actual.simulazioni).toBe(2);
      expect(actual.camera.candidati_uni.length).toBeGreaterThan(0);
      expect(actual.senato.candidati_pluri.length).toBeGreaterThan(0);
    });
  });
});
