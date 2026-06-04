import type {
  ElectionKind,
  Scenario,
  ScenarioCoalition,
  ScenarioDefaultSource,
  ScenarioDefaultSourceKind,
  ScenarioGlobalShareMode,
  ScenarioList,
  ScenarioListCorrespondence,
  ScenarioListCorrespondenceSource
} from '$lib/core/types';
import { generatedDefaultPoliticsScenario } from './politics-defaults.generated';

export const politicsScenarioStorageKey = 'elezioni:web:politics-scenario:v1';
export const politicsScenarioSchemaVersion = 4;

export const defaultPoliticsScenario: Scenario = generatedDefaultPoliticsScenario;
export const defaultPoliticsSourceModelListNames = defaultPoliticsScenario.lists.map((list) => list.name);
const manualCorrespondencePastElection = 'politics-static source model';

interface SerializedScenario {
  schema_version: number;
  scenario: Scenario;
}

function stableId(prefix: string, value: string, fallback: number): string {
  const normalized = value
    .trim()
    .toLowerCase()
    .normalize('NFKD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-|-$/g, '');
  return normalized ? `${prefix}-${normalized}` : `${prefix}-${fallback}`;
}

function uniqueName(base: string, existingNames: readonly string[]): string {
  const used = new Set(existingNames.map((name) => name.trim().toLocaleLowerCase('it-IT')));
  if (!used.has(base.toLocaleLowerCase('it-IT'))) return base;

  let suffix = 2;
  while (used.has(`${base} ${suffix}`.toLocaleLowerCase('it-IT'))) {
    suffix += 1;
  }
  return `${base} ${suffix}`;
}

function cleanColor(value: unknown, fallback: string): string {
  return typeof value === 'string' && /^#[0-9a-f]{6}$/i.test(value) ? value : fallback;
}

function cleanString(value: unknown): string {
  return typeof value === 'string' ? value : '';
}

function cleanShare(value: unknown): number {
  const numeric = Number(value);
  return Number.isFinite(numeric) ? numeric : 0;
}

function cleanPositiveFactor(value: unknown): number {
  const numeric = Number(value);
  return Number.isFinite(numeric) ? numeric : 1;
}

function cleanDefaultSourceKind(value: unknown): ScenarioDefaultSourceKind {
  return value === 'last-election' || value === 'manual' ? value : 'bundled';
}

function cleanElectionKind(value: unknown): ElectionKind {
  return value === 'regionali-er' || value === 'comunali' ? value : 'politiche';
}

function cleanGlobalShareMode(value: unknown): ScenarioGlobalShareMode {
  return 'mean';
}

function cleanCorrespondenceSource(value: unknown): ScenarioListCorrespondenceSource {
  return value === 'homonymous' || value === 'manual' ? value : 'bundled';
}

function normalizeDefaultSource(value: unknown): ScenarioDefaultSource {
  const source =
    value !== null && typeof value === 'object' ? (value as Partial<ScenarioDefaultSource>) : defaultPoliticsScenario.defaultSource;

  return {
    kind: cleanDefaultSourceKind(source.kind),
    electionKind: cleanElectionKind(source.electionKind),
    territory: cleanString(source.territory) || defaultPoliticsScenario.defaultSource.territory,
    dataVersion: cleanString(source.dataVersion) || defaultPoliticsScenario.defaultSource.dataVersion,
    snapshotId: cleanString(source.snapshotId) || defaultPoliticsScenario.defaultSource.snapshotId
  };
}

export function cloneScenario(scenario: Scenario): Scenario {
  return {
    id: scenario.id,
    name: scenario.name,
    electionDate: scenario.electionDate,
    defaultSource: { ...scenario.defaultSource },
    globalShareMode: scenario.globalShareMode,
    abstentionShare: scenario.abstentionShare,
    abstentionOverride: scenario.abstentionOverride,
    coalitions: scenario.coalitions.map((coalition) => ({ ...coalition })),
    lists: scenario.lists.map((list) => ({ ...list })),
    listCorrespondences: scenario.listCorrespondences.map((correspondence) => ({ ...correspondence }))
  };
}

export function createDefaultPoliticsScenario(): Scenario {
  return cloneScenario(defaultPoliticsScenario);
}

export function createScenarioList(
  coalitions: readonly ScenarioCoalition[],
  existingLists: readonly ScenarioList[] = []
): ScenarioList {
  const id = crypto.randomUUID();
  return {
    id,
    name: uniqueName('Nuova lista', existingLists.map((list) => list.name)),
    coalition: coalitions[0]?.name ?? null,
    color: '#6f7f8f',
    startingShare: 0,
    shareOverride: false
  };
}

export function createScenarioCoalition(existingCoalitions: readonly ScenarioCoalition[] = []): ScenarioCoalition {
  return {
    id: crypto.randomUUID(),
    name: uniqueName('Nuova coalizione', existingCoalitions.map((coalition) => coalition.name)),
    color: '#6f7f8f'
  };
}

export function createScenarioListCorrespondence(scenario: Scenario): ScenarioListCorrespondence {
  const futureList = scenario.lists[0]?.name ?? '';
  const usedSourceKeys = new Set(
    scenario.listCorrespondences
      .filter((correspondence) => correspondence.source === 'manual' && correspondence.futureList === futureList)
      .map((correspondence) => correspondence.pastList.trim().toLocaleLowerCase('it-IT'))
  );
  const pastList =
    defaultPoliticsSourceModelListNames.find((name) => !usedSourceKeys.has(name.trim().toLocaleLowerCase('it-IT'))) ??
    defaultPoliticsSourceModelListNames[0] ??
    '';

  return {
    id: crypto.randomUUID(),
    futureList,
    pastElection: manualCorrespondencePastElection,
    pastDate: scenario.electionDate,
    pastList,
    factor: 1,
    source: 'manual'
  };
}

export function normalizeScenario(value: unknown): Scenario {
  const input = value !== null && typeof value === 'object' ? (value as Partial<Scenario>) : {};
  const coalitions = Array.isArray(input.coalitions) ? input.coalitions : [];
  const lists = Array.isArray(input.lists) ? input.lists : [];
  const listCorrespondences = Array.isArray(input.listCorrespondences) ? input.listCorrespondences : [];

  return {
    id: cleanString(input.id) || defaultPoliticsScenario.id,
    name: cleanString(input.name) || defaultPoliticsScenario.name,
    electionDate: cleanString(input.electionDate) || defaultPoliticsScenario.electionDate,
    defaultSource: normalizeDefaultSource(input.defaultSource),
    globalShareMode: cleanGlobalShareMode(input.globalShareMode),
    abstentionShare: cleanShare(input.abstentionShare ?? defaultPoliticsScenario.abstentionShare),
    abstentionOverride: input.abstentionOverride === true,
    coalitions: coalitions.map((coalition, index) => {
      const source = coalition as Partial<ScenarioCoalition>;
      const name = cleanString(source.name);
      return {
        id: cleanString(source.id) || stableId('coalition', name, index + 1),
        name,
        color: cleanColor(source.color, '#6f7f8f')
      };
    }),
    lists: lists.map((list, index) => {
      const source = list as Partial<ScenarioList>;
      const name = cleanString(source.name);
      return {
        id: cleanString(source.id) || stableId('list', name, index + 1),
        name,
        coalition: source.coalition === null ? null : cleanString(source.coalition),
        color: cleanColor(source.color, '#6f7f8f'),
        startingShare: cleanShare(source.startingShare),
        shareOverride: source.shareOverride === true
      };
    }),
    listCorrespondences: listCorrespondences.map((correspondence, index) => {
      const source = correspondence as Partial<ScenarioListCorrespondence>;
      const futureList = cleanString(source.futureList);
      const pastElection = cleanString(source.pastElection);
      const pastList = cleanString(source.pastList);
      return {
        id:
          cleanString(source.id) ||
          stableId('correspondence', `${pastElection}-${pastList}-${futureList}`, index + 1),
        futureList,
        pastElection,
        pastDate: cleanString(source.pastDate),
        pastList,
        factor: cleanPositiveFactor(source.factor),
        source: cleanCorrespondenceSource(source.source)
      };
    })
  };
}

export function validateScenario(scenario: Scenario): string[] {
  const messages: string[] = [];
  const coalitionNames = scenario.coalitions.map((coalition) => coalition.name.trim()).filter(Boolean);
  const coalitionNameSet = new Set(coalitionNames);
  const listNames = scenario.lists.map((list) => list.name.trim()).filter(Boolean);
  const listNameSet = new Set(listNames.map((name) => name.toLocaleLowerCase('it-IT')));
  const correspondenceDestinationSet = new Set([...listNameSet, 'astensione']);
  const overrideLists = scenario.lists.filter((list) => list.shareOverride);
  const totalOverrideShare = overrideLists.reduce((sum, list) => sum + Math.max(Number(list.startingShare) || 0, 0), 0);
  const correspondenceKeys = new Set<string>();

  if (!scenario.name.trim()) messages.push('Lo scenario deve avere un nome.');
  if (Number.isNaN(Date.parse(scenario.electionDate))) messages.push('La data elezione non e valida.');
  if (!scenario.defaultSource.territory.trim()) messages.push('La fonte dei default deve indicare un territorio.');
  if (!scenario.defaultSource.dataVersion.trim()) messages.push('La fonte dei default deve indicare una versione dati.');
  if (scenario.coalitions.length === 0) messages.push('Serve almeno una coalizione.');
  if (coalitionNames.length !== scenario.coalitions.length) messages.push('Ogni coalizione deve avere un nome.');
  if (coalitionNameSet.size !== coalitionNames.length) messages.push('I nomi delle coalizioni devono essere unici.');
  if (scenario.lists.length === 0) messages.push('Serve almeno una lista.');
  if (listNames.length !== scenario.lists.length) messages.push('Ogni lista deve avere un nome.');
  if (listNameSet.size !== listNames.length) messages.push('I nomi delle liste devono essere unici.');
  if (!Number.isFinite(scenario.abstentionShare) || scenario.abstentionShare < 0 || scenario.abstentionShare >= 100) {
    messages.push('Astensione non valida.');
  }
  if (overrideLists.length > 0 && totalOverrideShare <= 0) {
    messages.push('La somma delle quote usate deve essere maggiore di zero.');
  }
  if (totalOverrideShare > 100.01) messages.push('La somma delle quote usate non puo superare 100.');

  for (const list of scenario.lists) {
    const share = Number(list.startingShare);
    if (!Number.isFinite(share) || share < 0 || share > 100) {
      messages.push(`Quota non valida per ${list.name || 'lista senza nome'}.`);
    }
    if (!list.coalition || !coalitionNameSet.has(list.coalition)) {
      messages.push(`Coalizione mancante o sconosciuta per ${list.name || 'lista senza nome'}.`);
    }
  }

  for (const correspondence of scenario.listCorrespondences) {
    if (correspondence.source !== 'manual') continue;

    const futureListKey = correspondence.futureList.trim().toLocaleLowerCase('it-IT');
    const pastList = correspondence.pastList.trim();
    const pastElection = correspondence.pastElection.trim();
    const key = [
      correspondence.pastDate,
      pastElection.toLocaleLowerCase('it-IT'),
      pastList.toLocaleLowerCase('it-IT'),
      futureListKey
    ].join('|');

    if (!correspondenceDestinationSet.has(futureListKey)) {
      messages.push(`Corrispondenza verso lista sconosciuta: ${correspondence.futureList || 'lista senza nome'}.`);
    }
    if (!pastElection) messages.push('Ogni corrispondenza lista deve indicare una elezione precedente.');
    if (!pastList) messages.push('Ogni corrispondenza lista deve indicare una lista precedente.');
    if (Number.isNaN(Date.parse(correspondence.pastDate))) {
      messages.push(`Data non valida per corrispondenza ${pastList || correspondence.futureList || 'lista'}.`);
    }
    if (!Number.isFinite(correspondence.factor) || correspondence.factor <= 0) {
      messages.push(`Fattore non valido per corrispondenza ${pastList || correspondence.futureList || 'lista'}.`);
    }
    if (correspondenceKeys.has(key)) {
      messages.push(`Corrispondenza duplicata per ${pastList || 'lista precedente'} -> ${correspondence.futureList || 'lista'}.`);
    }
    correspondenceKeys.add(key);
  }

  return messages;
}

export function serializeScenario(scenario: Scenario): string {
  const payload: SerializedScenario = {
    schema_version: politicsScenarioSchemaVersion,
    scenario: cloneScenario(scenario)
  };
  return `${JSON.stringify(payload, null, 2)}\n`;
}

export function parseScenario(text: string): Scenario {
  const parsed = JSON.parse(text) as unknown;
  const scenarioInput =
    parsed !== null && typeof parsed === 'object' && 'scenario' in parsed
      ? (parsed as Partial<SerializedScenario>).scenario
      : parsed;
  const scenario = normalizeScenario(scenarioInput);
  const errors = validateScenario(scenario);

  if (errors.length > 0) {
    throw new Error(errors.join('\n'));
  }

  return scenario;
}
