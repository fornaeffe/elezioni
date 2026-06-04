import { describe, expect, test } from 'vitest';
import {
  createDefaultPoliticsScenario,
  createScenarioListCorrespondence,
  normalizeScenario,
  parseScenario,
  serializeScenario,
  validateScenario
} from './politics';

describe('politics web-native scenario model', () => {
  test('default scenario is valid', () => {
    const scenario = createDefaultPoliticsScenario();

    expect(scenario.defaultSource).toEqual({
      kind: 'bundled',
      electionKind: 'politiche',
      territory: 'Italia',
      dataVersion: 'v1',
      snapshotId: 'politics-static.json'
    });
    expect(scenario.globalShareMode).toBe('mean');
    expect(scenario.abstentionShare).toBeGreaterThan(0);
    expect(scenario.abstentionOverride).toBe(false);
    expect(scenario.localShareOverrides).toEqual([]);
    expect(scenario.listCorrespondences.length).toBeGreaterThan(0);
    expect(scenario.listCorrespondences.every((correspondence) => correspondence.source === 'bundled')).toBe(true);
    expect(scenario.listCorrespondences).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          futureList: 'Partito Democratico',
          pastElection: 'europee 2024',
          pastDate: '2024-06-08',
          pastList: 'PARTITO DEMOCRATICO',
          factor: 1,
          source: 'bundled'
        }),
        expect.objectContaining({
          futureList: 'astensione',
          pastElection: 'europee 2024',
          pastDate: '2024-06-08',
          pastList: 'astensione',
          factor: 1,
          source: 'bundled'
        })
      ])
    );
    expect(scenario.lists.every((list) => !list.shareOverride)).toBe(true);
    expect(validateScenario(scenario)).toEqual([]);
  });

  test('round-trips through the versioned JSON format', () => {
    const scenario = createDefaultPoliticsScenario();
    const parsed = parseScenario(serializeScenario(scenario));

    expect(parsed).toEqual(scenario);
  });

  test('detects duplicate list names and unknown coalitions', () => {
    const scenario = createDefaultPoliticsScenario();
    scenario.lists[1].name = scenario.lists[0].name;
    scenario.lists[1].coalition = 'missing';

    expect(validateScenario(scenario)).toEqual(
      expect.arrayContaining([
        'I nomi delle liste devono essere unici.',
        'Coalizione mancante o sconosciuta per +Europa.'
      ])
    );
  });

  test('normalizes partial untrusted input before validation', () => {
    const scenario = normalizeScenario({
      lists: [{ name: 'Lista A', coalition: 'Coalizione A', startingShare: '40', shareOverride: true }],
      coalitions: [{ name: 'Coalizione A' }]
    });

    expect(scenario.id).toBe('politiche-2027');
    expect(scenario.defaultSource.kind).toBe('bundled');
    expect(scenario.globalShareMode).toBe('mean');
    expect(scenario.abstentionShare).toBe(createDefaultPoliticsScenario().abstentionShare);
    expect(scenario.abstentionOverride).toBe(false);
    expect(scenario.listCorrespondences).toEqual([]);
    expect(scenario.localShareOverrides).toEqual([]);
    expect(scenario.lists[0].id).toBe('list-lista-a');
    expect(scenario.lists[0].startingShare).toBe(40);
    expect(scenario.lists[0].shareOverride).toBe(true);
    expect(validateScenario(scenario)).toEqual([]);
  });

  test('keeps old scenario JSON compatible by defaulting missing override flags to false', () => {
    const scenario = parseScenario(
      JSON.stringify({
        schema_version: 1,
        scenario: {
          name: 'Old scenario',
          electionDate: '2027-03-01',
          coalitions: [{ name: 'Coalizione A' }],
          lists: [{ name: 'Lista A', coalition: 'Coalizione A', startingShare: 40 }]
        }
      })
    );

    expect(scenario.lists[0].shareOverride).toBe(false);
    expect(scenario.defaultSource.kind).toBe('bundled');
    expect(scenario.globalShareMode).toBe('mean');
    expect(scenario.abstentionShare).toBe(createDefaultPoliticsScenario().abstentionShare);
    expect(scenario.abstentionOverride).toBe(false);
    expect(scenario.listCorrespondences).toEqual([]);
    expect(scenario.localShareOverrides).toEqual([]);
  });

  test('round-trips v5 default metadata, normalizes retired share mode, abstention, correspondences, and local overrides', () => {
    const scenario = parseScenario(
      JSON.stringify({
        schema_version: 5,
        scenario: {
          name: 'Scenario con corrispondenze',
          electionDate: '2027-03-01',
          defaultSource: {
            kind: 'last-election',
            electionKind: 'politiche',
            territory: 'Italia',
            dataVersion: 'v1'
          },
          globalShareMode: 'fixed',
          abstentionShare: 47.5,
          abstentionOverride: true,
          coalitions: [{ name: 'Coalizione A' }],
          lists: [{ name: 'Lista A', coalition: 'Coalizione A', startingShare: 40 }],
          listCorrespondences: [
            {
              futureList: 'Lista A',
              pastElection: 'camera 2022',
              pastDate: '2022-09-25',
              pastList: 'LISTA A PASSATA',
              factor: 0.5,
              source: 'manual'
            },
            {
              futureList: 'astensione',
              pastElection: 'camera 2022',
              pastDate: '2022-09-25',
              pastList: 'LISTA B PASSATA',
              factor: 1,
              source: 'manual'
            }
          ],
          localShareOverrides: [
            {
              locationCode: '1',
              list: 'Lista A',
              startingShare: 55
            }
          ]
        }
      })
    );

    expect(scenario.defaultSource.kind).toBe('last-election');
    expect(scenario.globalShareMode).toBe('mean');
    expect(scenario.abstentionShare).toBe(47.5);
    expect(scenario.abstentionOverride).toBe(true);
    expect(scenario.listCorrespondences).toHaveLength(2);
    expect(scenario.localShareOverrides).toEqual([
      {
        id: 'local-share-1-lista-a',
        scope: 'municipality',
        locationCode: '1',
        list: 'Lista A',
        startingShare: 55
      }
    ]);
    expect(scenario.listCorrespondences[0]).toEqual(
      expect.objectContaining({
        id: 'correspondence-camera-2022-lista-a-passata-lista-a',
        futureList: 'Lista A',
        pastElection: 'camera 2022',
        pastDate: '2022-09-25',
        pastList: 'LISTA A PASSATA',
        factor: 0.5,
        source: 'manual'
      })
    );
    expect(parseScenario(serializeScenario(scenario))).toEqual(scenario);
  });

  test('validates only explicitly used global share overrides as a total', () => {
    const scenario = createDefaultPoliticsScenario();
    scenario.lists[0].startingShare = 60;
    scenario.lists[1].startingShare = 60;
    scenario.lists[0].shareOverride = true;

    expect(validateScenario(scenario)).toEqual([]);

    scenario.lists[1].shareOverride = true;
    expect(validateScenario(scenario)).toEqual(expect.arrayContaining(['La somma delle quote usate non puo superare 100.']));
  });

  test('validates abstention share as an elector percentage', () => {
    const scenario = createDefaultPoliticsScenario();
    scenario.abstentionShare = 100;

    expect(validateScenario(scenario)).toEqual(expect.arrayContaining(['Astensione non valida.']));
  });

  test('ignores bundled correspondences that no longer target active scenario lists', () => {
    const scenario = createDefaultPoliticsScenario();
    scenario.lists = scenario.lists.filter((list) => list.name !== '+Europa');

    expect(validateScenario(scenario)).toEqual([]);
  });

  test('creates valid manual list correspondences for compact editing', () => {
    const scenario = createDefaultPoliticsScenario();
    scenario.lists[0].name = '+Europa Test';
    scenario.listCorrespondences = [];

    const correspondence = createScenarioListCorrespondence(scenario);
    scenario.listCorrespondences = [correspondence];

    expect(correspondence).toEqual(
      expect.objectContaining({
        futureList: '+Europa Test',
        pastElection: 'politics-static source model',
        pastDate: scenario.electionDate,
        pastList: '+Europa',
        factor: 1,
        source: 'manual'
      })
    );
    expect(validateScenario(scenario)).toEqual([]);
  });

  test('validates malformed list correspondences', () => {
    const scenario = createDefaultPoliticsScenario();
    scenario.listCorrespondences = [
      {
        id: 'bad',
        futureList: 'Lista inesistente',
        pastElection: '',
        pastDate: 'not-a-date',
        pastList: '',
        factor: 0,
        source: 'manual'
      },
      {
        id: 'bad-duplicate',
        futureList: 'Lista inesistente',
        pastElection: '',
        pastDate: 'not-a-date',
        pastList: '',
        factor: 1,
        source: 'manual'
      }
    ];

    expect(validateScenario(scenario)).toEqual(
      expect.arrayContaining([
        'Corrispondenza verso lista sconosciuta: Lista inesistente.',
        'Ogni corrispondenza lista deve indicare una elezione precedente.',
        'Ogni corrispondenza lista deve indicare una lista precedente.',
        'Data non valida per corrispondenza Lista inesistente.',
        'Fattore non valido per corrispondenza Lista inesistente.',
        'Corrispondenza duplicata per lista precedente -> Lista inesistente.'
      ])
    );
  });

  test('validates malformed local share overrides', () => {
    const scenario = createDefaultPoliticsScenario();
    scenario.localShareOverrides = [
      {
        id: 'bad-local',
        scope: 'municipality',
        locationCode: '',
        list: 'Lista inesistente',
        startingShare: -1
      },
      {
        id: 'bad-local-duplicate',
        scope: 'municipality',
        locationCode: '',
        list: 'Lista inesistente',
        startingShare: 1
      }
    ];

    expect(validateScenario(scenario)).toEqual(
      expect.arrayContaining([
        'Ogni quota locale deve indicare un comune.',
        'Quota locale verso lista sconosciuta: Lista inesistente.',
        'Quota locale non valida per Lista inesistente.',
        'Quota locale duplicata per comune / Lista inesistente.'
      ])
    );
  });

  test('allows all local list shares to be renormalized but rejects impossible partial totals', () => {
    const scenario = createDefaultPoliticsScenario();
    scenario.lists = [
      { id: 'a', name: 'Lista A', coalition: scenario.coalitions[0].name, color: '#000000', startingShare: 50, shareOverride: false },
      { id: 'b', name: 'Lista B', coalition: scenario.coalitions[0].name, color: '#111111', startingShare: 50, shareOverride: false },
      { id: 'c', name: 'Lista C', coalition: scenario.coalitions[0].name, color: '#222222', startingShare: 50, shareOverride: false }
    ];
    scenario.listCorrespondences = [];
    scenario.localShareOverrides = [
      { id: 'a-local', scope: 'municipality', locationCode: '1', list: 'Lista A', startingShare: 70 },
      { id: 'b-local', scope: 'municipality', locationCode: '1', list: 'Lista B', startingShare: 70 },
      { id: 'c-local', scope: 'municipality', locationCode: '1', list: 'Lista C', startingShare: 70 }
    ];

    expect(validateScenario(scenario)).toEqual([]);

    scenario.localShareOverrides = [scenario.localShareOverrides[0], scenario.localShareOverrides[1]];
    expect(validateScenario(scenario)).toEqual(
      expect.arrayContaining(['La somma delle quote locali usate per 1 non puo superare 100.'])
    );
  });
});
