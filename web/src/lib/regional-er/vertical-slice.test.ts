import { fileURLToPath } from 'node:url';
import { expect, test } from 'vitest';
import { createDefaultRegionalErScenario, validateRegionalErScenario } from '$lib/scenario/regional-er';
import { describeWithGeneratedFixtures, loadGeneratedJsonFixture } from '$lib/test/generated-fixtures';
import { buildRegionalErDirectScrutinySnapshot } from './pipeline';
import { projectScenarioOntoRegionalErSource } from './scenario-projection';
import { runRegionalErScrutiny } from './scrutiny';
import { buildRegionalErPipelineSourceFromSnapshot } from './static-snapshot';
import type { RegionalErStaticSnapshot } from './types';

const productionStaticSnapshotPath = fileURLToPath(
  new URL('../../../../web/static/data/v1/regional-er-static.json', import.meta.url)
);

function regionalListSharePercent(snapshot: RegionalErStaticSnapshot, listName: string): number {
  const politicalLists = snapshot.default_scenario.liste.filter((row) => row.LISTA !== 'astensione');
  const totalPoliticalShare = politicalLists.reduce((sum, row) => sum + row.PERCENTUALE, 0);
  const list = politicalLists.find((row) => row.LISTA === listName);

  if (!list) {
    throw new Error(`Missing production regional snapshot list ${listName}`);
  }

  return (list.PERCENTUALE / totalPoliticalShare) * 100;
}

describeWithGeneratedFixtures('regional Emilia-Romagna browser vertical slice', [productionStaticSnapshotPath], () => {
  const snapshot = loadGeneratedJsonFixture<RegionalErStaticSnapshot>(productionStaticSnapshotPath);

  test('keeps the UI default scenario aligned with the production regional static snapshot', () => {
    const scenario = createDefaultRegionalErScenario();
    const snapshotListNames = snapshot.default_scenario.liste
      .filter((row) => row.LISTA !== 'astensione')
      .map((row) => row.LISTA);
    const snapshotCoalitionNames = new Set(snapshot.default_scenario.coalizioni?.map((row) => row.COALIZIONE) ?? []);

    expect(validateRegionalErScenario(scenario)).toEqual([]);
    expect(scenario.defaultSource.electionKind).toBe('regionali-er');
    expect(scenario.lists.map((row) => row.name)).toEqual(snapshotListNames);
    expect(new Set(scenario.coalitions.map((row) => row.name))).toEqual(snapshotCoalitionNames);
    expect(scenario.candidateTemplates).toEqual([]);

    for (const list of scenario.lists) {
      const snapshotList = snapshot.default_scenario.liste.find((row) => row.LISTA === list.name);
      expect(snapshotList?.COALIZIONE).toBe(list.coalition);
      expect(Math.abs(list.startingShare - regionalListSharePercent(snapshot, list.name))).toBeLessThanOrEqual(0.05);
    }
  });

  test('projects, generates, and scrutinizes one production regional browser simulation', { timeout: 30_000 }, () => {
    const scenario = createDefaultRegionalErScenario();
    const source = buildRegionalErPipelineSourceFromSnapshot(snapshot, { simulations: 1 });
    const projection = projectScenarioOntoRegionalErSource(source, scenario, {
      electionDate: scenario.electionDate,
      historicalVotes: snapshot.data.comuni_liste_elezioni,
      municipalities: snapshot.data.municipalities,
      simulations: 1
    });

    expect(projection.warnings).toEqual([]);
    expect(projection.rows.every((row) => row.status === 'active')).toBe(true);

    const directSnapshot = buildRegionalErDirectScrutinySnapshot(projection.source, {
      seed: 'regional-er-production-vertical-slice'
    });
    const output = runRegionalErScrutiny(directSnapshot.simulations[0].input, {
      liste: projection.source.liste,
      pop_legale: projection.source.pop_legale
    });

    expect(output.coalizioni.filter((row) => row.PRESIDENTE)).toHaveLength(1);
    expect(output.coalizioni.filter((row) => row.MIGLIOR_PERDENTE)).toHaveLength(1);
    expect(output.liste.reduce((sum, row) => sum + row.ELETTI, 0)).toBe(48);
    expect(output.prov_lista.filter((row) => row.ELETTI > 0).length).toBeGreaterThan(0);
  });
});
