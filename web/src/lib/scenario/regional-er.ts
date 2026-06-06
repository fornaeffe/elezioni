import type {
  ElectionKind,
  Scenario,
  ScenarioCoalition,
  ScenarioDefaultSource,
  ScenarioDefaultSourceKind,
  ScenarioGlobalShareMode,
  ScenarioList,
  ScenarioListCorrespondence,
  ScenarioListCorrespondenceSource,
  ScenarioLocalShareOverride,
  ScenarioLocalShareOverrideScope,
  ScenarioPlurinominalCandidacyCountShares
} from '$lib/core/types';
import { generatedDefaultRegionalErScenario } from './regional-er-defaults.generated';

export const regionalErScenarioStorageKey = 'elezioni:web:regional-er-scenario:v1';
export const regionalErScenarioSchemaVersion = 1;
export const regionalErAbstentionListName = 'astensione';
export const defaultRegionalErScenario: Scenario = generatedDefaultRegionalErScenario;

const vectorLength = 5;
const shareSumTolerance = 1e-9;

interface SerializedScenario {
  schema_version: number;
  scenario: Scenario;
}

export interface ScenarioLocalShareOverrideGroup {
  key: string;
  scope: ScenarioLocalShareOverrideScope;
  locationCode: string;
  overrides: ScenarioLocalShareOverride[];
  totalShare: number;
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
  while (used.has(`${base} ${suffix}`.toLocaleLowerCase('it-IT'))) suffix += 1;
  return `${base} ${suffix}`;
}

function listKey(name: string): string {
  return name.trim().toLocaleLowerCase('it-IT');
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

function cleanDefaultSourceKind(value: unknown): ScenarioDefaultSourceKind {
  return value === 'last-election' || value === 'manual' ? value : 'bundled';
}

function cleanElectionKind(value: unknown): ElectionKind {
  return value === 'politiche' || value === 'comunali' ? value : 'regionali-er';
}

function cleanGlobalShareMode(_value: unknown): ScenarioGlobalShareMode {
  return 'mean';
}

function cleanCorrespondenceSource(value: unknown): ScenarioListCorrespondenceSource {
  return value === 'manual' ? 'manual' : 'bundled';
}

function cleanLocalShareOverrideScope(value: unknown): ScenarioLocalShareOverrideScope {
  if (value === 'province' || value === 'region') return value;
  return 'municipality';
}

function normalizeDefaultSource(value: unknown): ScenarioDefaultSource {
  const source =
    value !== null && typeof value === 'object' ? (value as Partial<ScenarioDefaultSource>) : defaultRegionalErScenario.defaultSource;

  return {
    kind: cleanDefaultSourceKind(source.kind),
    electionKind: cleanElectionKind(source.electionKind),
    territory: cleanString(source.territory) || defaultRegionalErScenario.defaultSource.territory,
    dataVersion: cleanString(source.dataVersion) || defaultRegionalErScenario.defaultSource.dataVersion,
    snapshotId: cleanString(source.snapshotId) || defaultRegionalErScenario.defaultSource.snapshotId
  };
}

function normalizeCandidacyShares(values: unknown): ScenarioPlurinominalCandidacyCountShares {
  if (!Array.isArray(values) || values.length !== vectorLength) return [1, 0, 0, 0, 0];
  const numeric = values.map(Number);
  if (numeric.some((value) => !Number.isFinite(value) || value < 0)) return [1, 0, 0, 0, 0];
  const total = numeric.reduce((sum, value) => sum + value, 0);
  if (total <= 0) return [1, 0, 0, 0, 0];
  return numeric.map((value) => value / total) as ScenarioPlurinominalCandidacyCountShares;
}

function localShareOverrideKey(scope: ScenarioLocalShareOverrideScope, locationCode: string, list: string): string {
  return `${scope}\u001f${locationCode.trim()}\u001f${listKey(list)}`;
}

function localShareOverrideLocationKey(scope: ScenarioLocalShareOverrideScope, locationCode: string): string {
  return `${scope}\u001f${locationCode.trim()}`;
}

function localShareOverrideScopeLabel(scope: ScenarioLocalShareOverrideScope): string {
  if (scope === 'region') return 'regione';
  if (scope === 'province') return 'provincia';
  return 'comune';
}

export function cloneRegionalErScenario(scenario: Scenario): Scenario {
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
    listCorrespondences: scenario.listCorrespondences.map((correspondence) => ({ ...correspondence })),
    localShareOverrides: scenario.localShareOverrides.map((override) => ({ ...override })),
    candidateTemplates: [],
    candidateGeneration: {
      uninominalToPlurinominalShare: 0,
      plurinominalCandidacyCountShares: [1, 0, 0, 0, 0]
    }
  };
}

export function createDefaultRegionalErScenario(): Scenario {
  return cloneRegionalErScenario(defaultRegionalErScenario);
}

export function createRegionalErScenarioList(
  coalitions: readonly ScenarioCoalition[],
  existingLists: readonly ScenarioList[] = []
): ScenarioList {
  return {
    id: crypto.randomUUID(),
    name: uniqueName('Nuova lista', existingLists.map((list) => list.name)),
    coalition: coalitions[0]?.name ?? null,
    color: '#6f7f8f',
    startingShare: 0,
    shareOverride: false
  };
}

export function createRegionalErScenarioCoalition(existingCoalitions: readonly ScenarioCoalition[] = []): ScenarioCoalition {
  return {
    id: crypto.randomUUID(),
    name: uniqueName('Nuova coalizione', existingCoalitions.map((coalition) => coalition.name)),
    color: '#6f7f8f'
  };
}

export function renameRegionalErScenarioCoalition(scenario: Scenario, id: string, name: string): Scenario {
  const previous = scenario.coalitions.find((row) => row.id === id);
  if (!previous) return scenario;

  return {
    ...scenario,
    coalitions: scenario.coalitions.map((row) => (row.id === id ? { ...row, name } : row)),
    lists: scenario.lists.map((row) => (row.coalition === previous.name ? { ...row, coalition: name } : row))
  };
}

export function removeRegionalErScenarioCoalition(scenario: Scenario, id: string): Scenario {
  const removed = scenario.coalitions.find((row) => row.id === id);
  if (!removed) return scenario;

  const coalitions = scenario.coalitions.filter((row) => row.id !== id);
  const fallback = coalitions[0]?.name ?? null;

  return {
    ...scenario,
    coalitions,
    lists: scenario.lists.map((row) => (row.coalition === removed.name ? { ...row, coalition: fallback } : row))
  };
}

export function renameRegionalErScenarioList(scenario: Scenario, id: string, name: string): Scenario {
  const previous = scenario.lists.find((row) => row.id === id);
  if (!previous) return scenario;

  return {
    ...scenario,
    lists: scenario.lists.map((row) => (row.id === id ? { ...row, name } : row)),
    listCorrespondences: scenario.listCorrespondences.map((row) =>
      row.futureList === previous.name ? { ...row, futureList: name, source: 'manual' } : row
    ),
    localShareOverrides: scenario.localShareOverrides.map((row) => (row.list === previous.name ? { ...row, list: name } : row))
  };
}

export function removeRegionalErScenarioList(scenario: Scenario, id: string): Scenario {
  const removed = scenario.lists.find((row) => row.id === id);
  if (!removed) return scenario;

  return {
    ...scenario,
    lists: scenario.lists.filter((row) => row.id !== id),
    listCorrespondences: scenario.listCorrespondences.map((row) =>
      row.futureList === removed.name ? { ...row, futureList: regionalErAbstentionListName, source: 'manual' } : row
    ),
    localShareOverrides: scenario.localShareOverrides.filter((row) => row.list !== removed.name)
  };
}

export function updateRegionalErScenarioHistoricalCorrespondence(
  scenario: Scenario,
  id: string,
  patch: Partial<ScenarioListCorrespondence>
): Scenario {
  return {
    ...scenario,
    listCorrespondences: scenario.listCorrespondences.map((row) =>
      row.id === id ? { ...row, ...patch, source: 'manual' } : row
    )
  };
}

export function buildRegionalErScenarioLocalShareOverrideGroups(
  scenario: Scenario
): ScenarioLocalShareOverrideGroup[] {
  const groups = new Map<string, ScenarioLocalShareOverride[]>();

  for (const override of scenario.localShareOverrides) {
    const key = localShareOverrideLocationKey(override.scope, override.locationCode);
    const rows = groups.get(key) ?? [];
    rows.push({ ...override });
    groups.set(key, rows);
  }

  return [...groups.entries()]
    .map(([groupKey, overrides]) => {
      const [scope, locationCode] = groupKey.split('\u001f') as [ScenarioLocalShareOverrideScope, string];
      return {
        key: groupKey,
        scope,
        locationCode,
        overrides: overrides.sort((left, right) => listKey(left.list).localeCompare(listKey(right.list), 'it')),
        totalShare: overrides.reduce((sum, override) => sum + Math.max(Number(override.startingShare) || 0, 0), 0)
      };
    })
    .sort((left, right) =>
      [left.scope, left.locationCode].join('\u001f').localeCompare([right.scope, right.locationCode].join('\u001f'), 'it')
    );
}

export function upsertRegionalErScenarioLocalShareOverride(
  scenario: Scenario,
  input: Pick<ScenarioLocalShareOverride, 'locationCode' | 'list' | 'startingShare'> &
    Partial<Pick<ScenarioLocalShareOverride, 'scope'>>
): Scenario {
  const scope = input.scope ?? 'municipality';
  const locationCode = input.locationCode.trim();
  const list = input.list.trim();
  const startingShare = Number(input.startingShare);
  const key = localShareOverrideKey(scope, locationCode, list);
  let matched = false;

  const localShareOverrides = scenario.localShareOverrides.map((override) => {
    if (localShareOverrideKey(override.scope, override.locationCode, override.list) !== key) return override;
    matched = true;
    return { ...override, scope, locationCode, list, startingShare };
  });

  if (!matched) {
    localShareOverrides.push({
      id: crypto.randomUUID(),
      scope,
      locationCode,
      list,
      startingShare
    });
  }

  return { ...scenario, localShareOverrides };
}

export function updateRegionalErScenarioLocalShareOverride(
  scenario: Scenario,
  id: string,
  patch: Partial<ScenarioLocalShareOverride>
): Scenario {
  return {
    ...scenario,
    localShareOverrides: scenario.localShareOverrides.map((override) =>
      override.id === id
        ? {
            ...override,
            ...patch,
            scope: patch.scope === undefined ? override.scope : cleanLocalShareOverrideScope(patch.scope),
            locationCode: patch.locationCode === undefined ? override.locationCode : cleanString(patch.locationCode).trim(),
            list: patch.list === undefined ? override.list : cleanString(patch.list).trim(),
            startingShare: patch.startingShare === undefined ? override.startingShare : Number(patch.startingShare)
          }
        : override
    )
  };
}

export function removeRegionalErScenarioLocalShareOverride(scenario: Scenario, id: string): Scenario {
  return {
    ...scenario,
    localShareOverrides: scenario.localShareOverrides.filter((override) => override.id !== id)
  };
}

export function normalizeRegionalErScenario(value: unknown): Scenario {
  const input = value !== null && typeof value === 'object' ? (value as Partial<Scenario>) : {};
  const coalitions = Array.isArray(input.coalitions) ? input.coalitions : [];
  const lists = Array.isArray(input.lists) ? input.lists : [];
  const listCorrespondences = Array.isArray(input.listCorrespondences) ? input.listCorrespondences : [];
  const localShareOverrides = Array.isArray(input.localShareOverrides) ? input.localShareOverrides : [];
  const candidateGeneration =
    input.candidateGeneration !== null && typeof input.candidateGeneration === 'object'
      ? input.candidateGeneration
      : defaultRegionalErScenario.candidateGeneration;

  return {
    id: cleanString(input.id) || defaultRegionalErScenario.id,
    name: cleanString(input.name) || defaultRegionalErScenario.name,
    electionDate: cleanString(input.electionDate) || defaultRegionalErScenario.electionDate,
    defaultSource: normalizeDefaultSource(input.defaultSource),
    globalShareMode: cleanGlobalShareMode(input.globalShareMode),
    abstentionShare: cleanShare(input.abstentionShare ?? defaultRegionalErScenario.abstentionShare),
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
        id: cleanString(source.id) || stableId('correspondence', `${pastElection}-${pastList}-${futureList}`, index + 1),
        futureList,
        pastElection,
        pastDate: cleanString(source.pastDate),
        pastList,
        factor: Number.isFinite(Number(source.factor)) && Number(source.factor) > 0 ? Number(source.factor) : 1,
        source: cleanCorrespondenceSource(source.source)
      };
    }),
    localShareOverrides: localShareOverrides.map((override, index) => {
      const source = override as Partial<ScenarioLocalShareOverride>;
      const scope = cleanLocalShareOverrideScope(source.scope);
      const locationCode = cleanString(source.locationCode);
      const list = cleanString(source.list);
      return {
        id: cleanString(source.id) || stableId('local-share', `${scope}-${locationCode}-${list}`, index + 1),
        scope,
        locationCode,
        list,
        startingShare: cleanShare(source.startingShare)
      };
    }),
    candidateTemplates: [],
    candidateGeneration: {
      uninominalToPlurinominalShare: 0,
      plurinominalCandidacyCountShares: normalizeCandidacyShares(
        (candidateGeneration as Partial<Scenario['candidateGeneration']>).plurinominalCandidacyCountShares
      )
    }
  };
}

export function validateRegionalErScenario(scenario: Scenario): string[] {
  const messages: string[] = [];
  const coalitionNames = scenario.coalitions.map((coalition) => coalition.name.trim()).filter(Boolean);
  const coalitionNameSet = new Set(coalitionNames);
  const listNames = scenario.lists.map((list) => list.name.trim()).filter(Boolean);
  const listNameSet = new Set(listNames.map((name) => name.toLocaleLowerCase('it-IT')));
  const correspondenceDestinationSet = new Set([...listNameSet, regionalErAbstentionListName]);
  const overrideLists = scenario.lists.filter((list) => list.shareOverride);
  const totalOverrideShare = overrideLists.reduce((sum, list) => sum + Math.max(Number(list.startingShare) || 0, 0), 0);
  const localOverrideKeys = new Set<string>();
  const localOverridesByLocation = new Map<string, ScenarioLocalShareOverride[]>();
  const candidacyShares = scenario.candidateGeneration.plurinominalCandidacyCountShares;
  const candidacyShareTotal = candidacyShares.reduce((sum, share) => sum + share, 0);

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
  if (overrideLists.length > 0 && totalOverrideShare <= 0) messages.push('La somma delle quote usate deve essere maggiore di zero.');
  if (totalOverrideShare > 100.01) messages.push('La somma delle quote usate non puo superare 100.');
  if (candidacyShares.length !== vectorLength || Math.abs(candidacyShareTotal - 1) > shareSumTolerance) {
    messages.push('La distribuzione candidature regionale non e valida.');
  }

  for (const list of scenario.lists) {
    const share = Number(list.startingShare);
    if (!Number.isFinite(share) || share < 0 || share > 100) messages.push(`Quota non valida per ${list.name || 'lista senza nome'}.`);
    if (!list.coalition || !coalitionNameSet.has(list.coalition)) {
      messages.push(`Coalizione mancante o sconosciuta per ${list.name || 'lista senza nome'}.`);
    }
  }

  for (const correspondence of scenario.listCorrespondences) {
    if (correspondence.source !== 'manual') continue;
    const futureListKey = correspondence.futureList.trim().toLocaleLowerCase('it-IT');
    if (!correspondenceDestinationSet.has(futureListKey)) {
      messages.push(`Corrispondenza verso lista sconosciuta: ${correspondence.futureList || 'lista senza nome'}.`);
    }
    if (!correspondence.pastElection.trim()) messages.push('Ogni corrispondenza lista deve indicare una elezione precedente.');
    if (!correspondence.pastList.trim()) messages.push('Ogni corrispondenza lista deve indicare una lista precedente.');
    if (Number.isNaN(Date.parse(correspondence.pastDate))) {
      messages.push(`Data non valida per corrispondenza ${correspondence.pastList || correspondence.futureList || 'lista'}.`);
    }
    if (!Number.isFinite(correspondence.factor) || correspondence.factor <= 0) {
      messages.push(`Fattore non valido per corrispondenza ${correspondence.pastList || correspondence.futureList || 'lista'}.`);
    }
  }

  for (const override of scenario.localShareOverrides) {
    const locationCode = override.locationCode.trim();
    const list = override.list.trim();
    const listKeyValue = list.toLocaleLowerCase('it-IT');
    const key = localShareOverrideKey(override.scope, locationCode, list);
    const share = Number(override.startingShare);
    const locationKey = localShareOverrideLocationKey(override.scope, locationCode);
    const grouped = localOverridesByLocation.get(locationKey) ?? [];
    grouped.push(override);
    localOverridesByLocation.set(locationKey, grouped);

    if (!['municipality', 'province', 'region'].includes(override.scope)) messages.push('Ambito quota locale non valido.');
    if (!locationCode) messages.push('Ogni quota locale deve indicare una localita.');
    if (!listNameSet.has(listKeyValue)) messages.push(`Quota locale verso lista sconosciuta: ${list || 'lista senza nome'}.`);
    if (!Number.isFinite(share) || share < 0 || share > 100) messages.push(`Quota locale non valida per ${list || 'lista senza nome'}.`);
    if (localOverrideKeys.has(key)) {
      messages.push(`Quota locale duplicata per ${localShareOverrideScopeLabel(override.scope)} ${locationCode || 'senza codice'} / ${list || 'lista'}.`);
    }
    localOverrideKeys.add(key);
  }

  for (const [locationKey, overrides] of localOverridesByLocation) {
    const [scope, locationCode] = locationKey.split('\u001f') as [ScenarioLocalShareOverrideScope, string];
    const total = overrides.reduce((sum, override) => sum + Math.max(Number(override.startingShare) || 0, 0), 0);
    const locationLabel = `${localShareOverrideScopeLabel(scope)} ${locationCode || 'senza codice'}`;
    if (total <= 0) messages.push(`La somma delle quote locali usate per ${locationLabel} deve essere maggiore di zero.`);
    if (total > 100.01 && overrides.length < scenario.lists.length) {
      messages.push(`La somma delle quote locali usate per ${locationLabel} non puo superare 100.`);
    }
  }

  return messages;
}

export function serializeRegionalErScenario(scenario: Scenario): string {
  const payload: SerializedScenario = {
    schema_version: regionalErScenarioSchemaVersion,
    scenario: cloneRegionalErScenario(scenario)
  };
  return `${JSON.stringify(payload, null, 2)}\n`;
}

export function parseRegionalErScenario(text: string): Scenario {
  const parsed = JSON.parse(text) as unknown;
  const scenarioInput =
    parsed !== null && typeof parsed === 'object' && 'scenario' in parsed
      ? (parsed as Partial<SerializedScenario>).scenario
      : parsed;
  const scenario = normalizeRegionalErScenario(scenarioInput);
  const errors = validateRegionalErScenario(scenario);
  if (errors.length > 0) throw new Error(errors.join('\n'));
  return scenario;
}
