import { fileURLToPath } from 'node:url';
import { expect, test } from 'vitest';
import { describeWithGeneratedFixtures, loadGeneratedJsonFixture } from '$lib/test/generated-fixtures';
import { createDefaultPoliticsScenario, validateScenario } from '$lib/scenario/politics';
import { buildPoliticsDirectScrutinySnapshot } from './pipeline';
import { projectScenarioOntoPoliticsSource } from './scenario-projection';
import { defaultPoliticsScrutinyAlgorithmId, resolvePoliticsScrutinyAlgorithm } from './scrutiny-algorithms';
import { buildPoliticsPipelineSourceFromSnapshot } from './static-snapshot';
import type { PoliticsStaticSnapshot, Ramo } from './types';

const productionStaticSnapshotPath = fileURLToPath(
  new URL('../../../../web/static/data/v1/politics-static.json', import.meta.url)
);

function loadProductionStaticSnapshot(): PoliticsStaticSnapshot {
  return loadGeneratedJsonFixture<PoliticsStaticSnapshot>(productionStaticSnapshotPath);
}

function politicalListSharePercent(snapshot: PoliticsStaticSnapshot, listName: string): number {
  const politicalLists = snapshot.default_scenario.liste.filter((row) => row.LISTA !== 'astensione');
  const totalPoliticalShare = politicalLists.reduce((sum, row) => sum + row.PERCENTUALE, 0);
  const list = politicalLists.find((row) => row.LISTA === listName);

  if (!list) {
    throw new Error(`Missing production snapshot list ${listName}`);
  }

  return (list.PERCENTUALE / totalPoliticalShare) * 100;
}

describeWithGeneratedFixtures('politics browser vertical slice', [productionStaticSnapshotPath], () => {
  const snapshot = loadProductionStaticSnapshot();

  test('keeps the UI default scenario aligned with the production static snapshot', () => {
    const scenario = createDefaultPoliticsScenario();
    const snapshotListNames = snapshot.default_scenario.liste
      .filter((row) => row.LISTA !== 'astensione')
      .map((row) => row.LISTA);
    const snapshotCoalitionNames = new Set(snapshot.default_scenario.coalizioni?.map((row) => row.COALIZIONE) ?? []);

    expect(validateScenario(scenario)).toEqual([]);
    expect(scenario.lists.map((row) => row.name)).toEqual(snapshotListNames);
    expect(new Set(scenario.coalitions.map((row) => row.name))).toEqual(snapshotCoalitionNames);
    expect(scenario.listCorrespondences).toHaveLength(snapshot.default_scenario.corrispondenza_liste?.length ?? 0);
    expect(scenario.listCorrespondences.every((row) => row.source === 'bundled')).toBe(true);

    for (const list of scenario.lists) {
      const snapshotList = snapshot.default_scenario.liste.find((row) => row.LISTA === list.name);
      expect(snapshotList?.COALIZIONE).toBe(list.coalition);
      expect(Math.abs(list.startingShare - politicalListSharePercent(snapshot, list.name))).toBeLessThanOrEqual(0.05);
    }
  });

  test('projects, generates, and scrutinizes one production politics browser simulation', { timeout: 30_000 }, () => {
    const scenario = createDefaultPoliticsScenario();
    const source = buildPoliticsPipelineSourceFromSnapshot(snapshot, { simulations: 1 });
    const projection = projectScenarioOntoPoliticsSource(source, scenario, {
      electionDate: scenario.electionDate,
      simulations: 1
    });
    const scrutinyAlgorithm = resolvePoliticsScrutinyAlgorithm(defaultPoliticsScrutinyAlgorithmId).algorithm;

    expect(projection.warnings).toEqual([]);
    expect(projection.rows.every((row) => row.status === 'matched')).toBe(true);

    const directSnapshot = buildPoliticsDirectScrutinySnapshot(projection.source, {
      seed: 'production-vertical-slice'
    });

    for (const ramo of ['camera', 'senato'] as Ramo[]) {
      const ramoSnapshot = directSnapshot.rami[ramo];
      const output = scrutinyAlgorithm.run(ramoSnapshot.simulations[0].input, {
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
