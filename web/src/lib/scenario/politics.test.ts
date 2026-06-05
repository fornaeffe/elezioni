import { describe, expect, test } from 'vitest';
import {
  buildScenarioHistoricalCorrespondenceGroups,
  createDefaultPoliticsScenario,
  defaultScenarioCandidateGeneration,
  normalizeScenario,
  parseScenario,
  plurinominalCandidacyCountSharesToFractions,
  pluricandidatureFractionsToPlurinominalCandidacyCountShares,
  removeScenarioList,
  removeScenarioHistoricalCorrespondence,
  renameScenarioList,
  resetScenarioHistoricalCorrespondenceSource,
  serializeScenario,
  splitScenarioHistoricalCorrespondence,
  updateScenarioHistoricalCorrespondence,
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
    expect(scenario.candidateTemplates).toEqual([]);
    expect(scenario.candidateGeneration).toEqual(defaultScenarioCandidateGeneration);
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
    expect(scenario.candidateTemplates).toEqual([]);
    expect(scenario.candidateGeneration).toEqual(defaultScenarioCandidateGeneration);
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
    expect(scenario.candidateTemplates).toEqual([]);
    expect(scenario.candidateGeneration).toEqual(defaultScenarioCandidateGeneration);
  });

  test('round-trips v6 default metadata, normalizes retired share mode, abstention, correspondences, local overrides, and candidate templates', () => {
    const scenario = parseScenario(
      JSON.stringify({
        schema_version: 6,
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
          ],
          candidateTemplates: [
            {
              ramo: 'camera',
              kind: 'uninominal',
              coalition: 'Coalizione A',
              uninominalCode: '10',
              candidateName: 'Candidato Uni',
              birthDate: '1980-01-02'
            },
            {
              ramo: 'senato',
              kind: 'plurinominal',
              list: 'Lista A',
              plurinominalCode: '20',
              candidateNumber: '2',
              minority: true,
              candidateName: 'Candidato Pluri',
              birthDate: null
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
    expect(scenario.candidateTemplates).toEqual([
      {
        id: 'candidate-template-camera-coalizione-a-10-candidato-uni',
        ramo: 'camera',
        kind: 'uninominal',
        candidateName: 'Candidato Uni',
        birthDate: '1980-01-02',
        coalition: 'Coalizione A',
        uninominalCode: '10',
        list: null,
        plurinominalCode: null,
        candidateNumber: null,
        minority: false
      },
      {
        id: 'candidate-template-senato-lista-a-20-2-true-candidato-pluri',
        ramo: 'senato',
        kind: 'plurinominal',
        candidateName: 'Candidato Pluri',
        birthDate: null,
        coalition: null,
        uninominalCode: null,
        list: 'Lista A',
        plurinominalCode: '20',
        candidateNumber: 2,
        minority: true
      }
    ]);
    expect(scenario.candidateGeneration).toEqual(defaultScenarioCandidateGeneration);
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

  test('round-trips v7 candidate generation settings', () => {
    const scenario = parseScenario(
      JSON.stringify({
        schema_version: 7,
        scenario: {
          name: 'Scenario candidati',
          electionDate: '2027-03-01',
          coalitions: [{ name: 'Coalizione A' }],
          lists: [{ name: 'Lista A', coalition: 'Coalizione A', startingShare: 40 }],
          candidateGeneration: {
            uninominalToPlurinominalShare: 0.25,
            plurinominalCandidacyCountShares: [0.5, 0.3, 0.1, 0.1, 0]
          }
        }
      })
    );

    expect(scenario.candidateGeneration).toEqual({
      uninominalToPlurinominalShare: 0.25,
      plurinominalCandidacyCountShares: [0.5, 0.3, 0.1, 0.1, 0]
    });
    expect(parseScenario(serializeScenario(scenario))).toEqual(scenario);
  });

  test('converts candidacy-count shares to internal pluricandidature fractions and back', () => {
    const internal = plurinominalCandidacyCountSharesToFractions([0.5, 0.5, 0, 0, 0]);
    expect(internal).toEqual([2 / 3, 1 / 3, 0, 0, 0]);

    const friendly = pluricandidatureFractionsToPlurinominalCandidacyCountShares(internal);
    expect(friendly).toEqual([0.5, 0.5, 0, 0, 0]);
  });

  test('validates malformed candidate generation settings', () => {
    const scenario = createDefaultPoliticsScenario();
    scenario.candidateGeneration = {
      uninominalToPlurinominalShare: 1.2,
      plurinominalCandidacyCountShares: [1, -0.1, 0, 0, 0]
    };

    expect(validateScenario(scenario)).toEqual(
      expect.arrayContaining([
        'Quota uninominali in plurinominale non valida.',
        'Distribuzione pluricandidature non valida.',
        'La somma della distribuzione pluricandidature deve essere 1.'
      ])
    );

    scenario.candidateGeneration = {
      uninominalToPlurinominalShare: 0,
      plurinominalCandidacyCountShares: [0.5, 0.5, 0.5, 0, 0]
    };
    expect(validateScenario(scenario)).toEqual(
      expect.arrayContaining(['La somma della distribuzione pluricandidature deve essere 1.'])
    );

    scenario.candidateGeneration = {
      uninominalToPlurinominalShare: 0,
      plurinominalCandidacyCountShares: [1, 0, 0] as never
    };
    expect(validateScenario(scenario)).toEqual(
      expect.arrayContaining(['La distribuzione delle pluricandidature deve avere cinque valori.'])
    );
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

  test('groups historical correspondences for preset matrix editing', () => {
    const scenario = createDefaultPoliticsScenario();
    const groups = buildScenarioHistoricalCorrespondenceGroups(scenario);
    const latest = groups.find((group) => group.pastElection === 'europee 2024');

    expect(groups.map((group) => group.pastElection)).toEqual(
      expect.arrayContaining(['camera 2018', 'camera 2022', 'europee 2019', 'europee 2024'])
    );
    expect(latest?.sources).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          pastList: 'PARTITO DEMOCRATICO',
          customized: false,
          correspondences: expect.arrayContaining([
            expect.objectContaining({
              futureList: 'Partito Democratico',
              factor: 1
            })
          ])
        })
      ])
    );
  });

  test('edits, splits, removes, and resets historical correspondences', () => {
    let scenario = createDefaultPoliticsScenario();
    const target = scenario.listCorrespondences.find(
      (row) => row.pastElection === 'europee 2024' && row.pastList === 'PARTITO DEMOCRATICO'
    );

    expect(target).toBeDefined();
    scenario = updateScenarioHistoricalCorrespondence(scenario, target?.id ?? '', {
      futureList: 'Movimento 5 Stelle',
      factor: 2
    });
    expect(scenario.listCorrespondences.find((row) => row.id === target?.id)).toEqual(
      expect.objectContaining({
        futureList: 'Movimento 5 Stelle',
        factor: 2,
        source: 'manual'
      })
    );

    scenario = splitScenarioHistoricalCorrespondence(scenario, target?.id ?? '');
    const splitRows = scenario.listCorrespondences.filter(
      (row) => row.pastElection === 'europee 2024' && row.pastList === 'PARTITO DEMOCRATICO'
    );
    expect(splitRows).toHaveLength(2);
    expect(splitRows.every((row) => row.source === 'manual')).toBe(true);

    scenario = removeScenarioHistoricalCorrespondence(scenario, splitRows[1].id);
    expect(
      scenario.listCorrespondences.filter(
        (row) => row.pastElection === 'europee 2024' && row.pastList === 'PARTITO DEMOCRATICO'
      )
    ).toHaveLength(1);

    scenario = resetScenarioHistoricalCorrespondenceSource(scenario, 'europee 2024', 'PARTITO DEMOCRATICO');
    expect(
      scenario.listCorrespondences.filter(
        (row) => row.pastElection === 'europee 2024' && row.pastList === 'PARTITO DEMOCRATICO'
      )
    ).toEqual([
      expect.objectContaining({
        futureList: 'Partito Democratico',
        factor: 1,
        source: 'bundled'
      })
    ]);
    expect(validateScenario(scenario)).toEqual([]);
  });

  test('cascades list rename and removal through scenario references', () => {
    let scenario = createDefaultPoliticsScenario();
    const list = scenario.lists.find((row) => row.name === 'Partito Democratico');

    expect(list).toBeDefined();
    scenario.localShareOverrides = [
      { id: 'local-pd', scope: 'municipality', locationCode: '1', list: 'Partito Democratico', startingShare: 40 }
    ];
    scenario.candidateTemplates = [
      {
        id: 'candidate-pd',
        ramo: 'camera',
        kind: 'plurinominal',
        list: 'Partito Democratico',
        plurinominalCode: '10',
        candidateNumber: 1,
        minority: false,
        candidateName: 'Candidato',
        birthDate: null
      }
    ];

    scenario = renameScenarioList(scenario, list?.id ?? '', 'Democratici');
    expect(scenario.listCorrespondences.some((row) => row.futureList === 'Democratici')).toBe(true);
    expect(scenario.localShareOverrides[0].list).toBe('Democratici');
    expect(scenario.candidateTemplates[0].list).toBe('Democratici');

    scenario = removeScenarioList(scenario, list?.id ?? '');
    expect(scenario.lists.some((row) => row.name === 'Democratici')).toBe(false);
    expect(scenario.listCorrespondences.some((row) => row.futureList === 'Democratici')).toBe(false);
    expect(scenario.listCorrespondences.some((row) => row.futureList === 'astensione' && row.source === 'manual')).toBe(true);
    expect(scenario.localShareOverrides).toEqual([]);
    expect(scenario.candidateTemplates).toEqual([]);
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

  test('validates malformed candidate templates', () => {
    const scenario = createDefaultPoliticsScenario();
    scenario.candidateTemplates = [
      {
        id: 'bad-uni',
        ramo: 'camera',
        kind: 'uninominal',
        candidateName: '',
        birthDate: 'not-a-date',
        coalition: 'Coalizione inesistente',
        uninominalCode: '',
        list: null,
        plurinominalCode: null,
        candidateNumber: null,
        minority: false
      },
      {
        id: 'bad-pluri',
        ramo: 'senato',
        kind: 'plurinominal',
        candidateName: 'Candidato',
        birthDate: null,
        coalition: null,
        uninominalCode: null,
        list: 'Lista inesistente',
        plurinominalCode: '',
        candidateNumber: 0,
        minority: false
      },
      {
        id: 'bad-pluri-duplicate',
        ramo: 'senato',
        kind: 'plurinominal',
        candidateName: 'Duplicato',
        birthDate: null,
        coalition: null,
        uninominalCode: null,
        list: 'Lista inesistente',
        plurinominalCode: '',
        candidateNumber: 0,
        minority: false
      }
    ];

    expect(validateScenario(scenario)).toEqual(
      expect.arrayContaining([
        'Ogni candidato definito nello scenario deve avere un nome.',
        'Data di nascita non valida per candidato senza nome.',
        'Coalizione candidato uninominale sconosciuta: Coalizione inesistente.',
        'Collegio uninominale mancante per candidato senza nome.',
        'Lista candidato plurinominale sconosciuta: Lista inesistente.',
        'Collegio plurinominale mancante per Candidato.',
        'Numero candidato plurinominale non valido per Candidato.',
        'Candidato duplicato per lo stesso slot: Duplicato.'
      ])
    );
  });
});
