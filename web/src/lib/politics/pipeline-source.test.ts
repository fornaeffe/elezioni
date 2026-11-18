import { fileURLToPath } from 'node:url';
import { expect, test } from 'vitest';
import { describeWithGeneratedFixtures, loadGeneratedJsonFixture } from '$lib/test/generated-fixtures';
import { buildPoliticsDirectScrutinySnapshot } from './pipeline';
import { runPoliticsScrutiny } from './scrutiny';
import type { PoliticsPipelineSource, Ramo } from './types';

interface PipelineSourceFixture {
  metadata: {
    schema_version: number;
    source: string;
    purpose: string;
  };
  source: PoliticsPipelineSource;
}

const fixturePath = fileURLToPath(
  new URL('../../../../test/fixtures/politiche/pipeline_source_debug.json', import.meta.url)
);

function loadFixture(): PipelineSourceFixture {
  return loadGeneratedJsonFixture<PipelineSourceFixture>(fixturePath);
}

describeWithGeneratedFixtures('politics real-source generation pipeline', [fixturePath], () => {
  const fixture = loadFixture();

  test('has the expected compact real-source fixture shape', () => {
    expect(fixture.metadata.schema_version).toBe(1);
    expect(fixture.source.liste).toHaveLength(10);
    expect(fixture.source.comuni_liste.length).toBeGreaterThan(70_000);
    expect(fixture.source.base_dati.length).toBeGreaterThan(8_000);
    expect(fixture.source.camera.candidati_uni).toHaveLength(588);
    expect(fixture.source.senato.candidati_uni).toHaveLength(296);
  });

  test('builds and scrutinizes one generated politics simulation from the real debug source', { timeout: 30_000 }, () => {
    const snapshot = buildPoliticsDirectScrutinySnapshot(
      {
        ...fixture.source,
        simulazioni: 1
      },
      { seed: 'real-source-smoke' }
    );

    expect(snapshot.rami.camera.simulations).toHaveLength(1);
    expect(snapshot.rami.senato.simulations).toHaveLength(1);
    expect(snapshot.rami.camera.simulations[0].input.liste_uni.length).toBeGreaterThan(1_000);
    expect(snapshot.rami.senato.simulations[0].input.liste_uni.length).toBeGreaterThan(500);

    for (const ramo of ['camera', 'senato'] as Ramo[]) {
      const ramoSnapshot = snapshot.rami[ramo];
      const output = runPoliticsScrutiny(ramoSnapshot.simulations[0].input, {
        ramo,
        liste_naz: ramoSnapshot.liste_naz,
        totali_pluri: ramoSnapshot.totali_pluri,
        totale_seggi: ramoSnapshot.totale_seggi
      });

      expect(output.liste_pluri.length).toBeGreaterThan(0);
      expect(output.candidati_uni.filter((row) => row.ELETTO)).toHaveLength(ramo === 'camera' ? 147 : 74);
      expect(output.candidati_pluri.filter((row) => row.ELETTO).length).toBeGreaterThan(0);
    }
  });
});
