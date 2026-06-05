import type {
  ElectionKind,
  Scenario,
  ScenarioCandidateTemplate,
  ScenarioCandidateTemplateKind,
  ScenarioCandidateTemplateRamo,
  ScenarioCoalition,
  ScenarioDefaultSource,
  ScenarioDefaultSourceKind,
  ScenarioGlobalShareMode,
  ScenarioList,
  ScenarioListCorrespondence,
  ScenarioListCorrespondenceSource,
  ScenarioLocalShareOverride,
  ScenarioLocalShareOverrideScope
} from '$lib/core/types';
import { generatedDefaultPoliticsScenario } from './politics-defaults.generated';

export const politicsScenarioStorageKey = 'elezioni:web:politics-scenario:v1';
export const politicsScenarioSchemaVersion = 6;

export const defaultPoliticsScenario: Scenario = generatedDefaultPoliticsScenario;
export const politicsAbstentionListName = 'astensione';

export interface ScenarioHistoricalCorrespondenceSource {
  key: string;
  pastElection: string;
  pastDate: string;
  pastList: string;
  correspondences: ScenarioListCorrespondence[];
  defaultCorrespondences: ScenarioListCorrespondence[];
  customized: boolean;
}

export interface ScenarioHistoricalCorrespondenceGroup {
  key: string;
  pastElection: string;
  pastDate: string;
  sources: ScenarioHistoricalCorrespondenceSource[];
}

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

function listKey(name: string): string {
  return name.trim().toLocaleLowerCase('it-IT');
}

function correspondenceSourceKey(pastElection: string, pastList: string): string {
  return `${pastElection.trim()}\u001f${listKey(pastList)}`;
}

function correspondenceRowKey(correspondence: ScenarioListCorrespondence): string {
  return [
    correspondence.pastElection.trim().toLocaleLowerCase('it-IT'),
    correspondence.pastList.trim().toLocaleLowerCase('it-IT'),
    correspondence.futureList.trim().toLocaleLowerCase('it-IT')
  ].join('|');
}

function correspondenceEditableKey(correspondence: ScenarioListCorrespondence): string {
  return [
    correspondence.pastElection.trim(),
    correspondence.pastList.trim(),
    correspondence.futureList.trim(),
    String(correspondence.factor)
  ].join('|');
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

function cleanNullableString(value: unknown): string | null {
  const cleaned = cleanString(value).trim();
  return cleaned || null;
}

function cleanNullableNumber(value: unknown): number | null {
  if (value === null || value === undefined || value === '') return null;
  const numeric = Number(value);
  return Number.isFinite(numeric) ? numeric : null;
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
  return value === 'manual' ? 'manual' : 'bundled';
}

function cleanLocalShareOverrideScope(value: unknown): ScenarioLocalShareOverrideScope {
  return 'municipality';
}

function cleanCandidateTemplateRamo(value: unknown): ScenarioCandidateTemplateRamo {
  return value === 'senato' ? 'senato' : 'camera';
}

function cleanCandidateTemplateKind(value: unknown): ScenarioCandidateTemplateKind {
  return value === 'uninominal' ? 'uninominal' : 'plurinominal';
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
    listCorrespondences: scenario.listCorrespondences.map((correspondence) => ({ ...correspondence })),
    localShareOverrides: scenario.localShareOverrides.map((override) => ({ ...override })),
    candidateTemplates: scenario.candidateTemplates.map((template) => ({ ...template }))
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

function cloneCorrespondence(correspondence: ScenarioListCorrespondence): ScenarioListCorrespondence {
  return {
    ...correspondence
  };
}

function defaultDestinationForSource(
  scenario: Scenario,
  pastElection: string,
  pastList: string
): string {
  const usedDestinations = new Set(
    scenario.listCorrespondences
      .filter((row) => correspondenceSourceKey(row.pastElection, row.pastList) === correspondenceSourceKey(pastElection, pastList))
      .map((row) => listKey(row.futureList))
  );
  const candidate = scenario.lists.find((list) => !usedDestinations.has(listKey(list.name)));
  return candidate?.name ?? politicsAbstentionListName;
}

export function buildScenarioHistoricalCorrespondenceGroups(
  scenario: Scenario
): ScenarioHistoricalCorrespondenceGroup[] {
  const defaultRowsBySource = new Map<string, ScenarioListCorrespondence[]>();
  const currentRowsBySource = new Map<string, ScenarioListCorrespondence[]>();
  const sourceOrder = new Map<string, number>();

  const rememberSource = (row: ScenarioListCorrespondence, index: number): void => {
    const key = correspondenceSourceKey(row.pastElection, row.pastList);
    if (!sourceOrder.has(key)) sourceOrder.set(key, index);
  };

  defaultPoliticsScenario.listCorrespondences.forEach((row, index) => {
    const key = correspondenceSourceKey(row.pastElection, row.pastList);
    const rows = defaultRowsBySource.get(key) ?? [];
    rows.push(cloneCorrespondence(row));
    defaultRowsBySource.set(key, rows);
    rememberSource(row, index);
  });

  scenario.listCorrespondences.forEach((row, index) => {
    const key = correspondenceSourceKey(row.pastElection, row.pastList);
    const rows = currentRowsBySource.get(key) ?? [];
    rows.push(cloneCorrespondence(row));
    currentRowsBySource.set(key, rows);
    rememberSource(row, defaultPoliticsScenario.listCorrespondences.length + index);
  });

  const sources = [...new Set([...defaultRowsBySource.keys(), ...currentRowsBySource.keys()])]
    .map((key) => {
      const current = currentRowsBySource.get(key) ?? [];
      const defaults = defaultRowsBySource.get(key) ?? [];
      const reference = current[0] ?? defaults[0];
      const defaultSignature = defaults.map(correspondenceEditableKey).sort().join('\u001e');
      const currentSignature = current.map(correspondenceEditableKey).sort().join('\u001e');

      return {
        key,
        pastElection: reference?.pastElection ?? '',
        pastDate: reference?.pastDate ?? '',
        pastList: reference?.pastList ?? '',
        correspondences: current.sort((left, right) => listKey(left.futureList).localeCompare(listKey(right.futureList), 'it')),
        defaultCorrespondences: defaults,
        customized: currentSignature !== defaultSignature
      } satisfies ScenarioHistoricalCorrespondenceSource;
    })
    .sort((left, right) => {
      const leftOrder = sourceOrder.get(left.key) ?? Number.MAX_SAFE_INTEGER;
      const rightOrder = sourceOrder.get(right.key) ?? Number.MAX_SAFE_INTEGER;
      if (leftOrder !== rightOrder) return leftOrder - rightOrder;
      return [left.pastElection, left.pastList].join('|').localeCompare([right.pastElection, right.pastList].join('|'), 'it');
    });

  const groups = new Map<string, ScenarioHistoricalCorrespondenceGroup>();
  for (const source of sources) {
    const groupKey = source.pastElection.trim();
    const group = groups.get(groupKey) ?? {
      key: groupKey,
      pastElection: source.pastElection,
      pastDate: source.pastDate,
      sources: []
    };
    group.sources.push(source);
    groups.set(groupKey, group);
  }

  return [...groups.values()];
}

export function addScenarioHistoricalCorrespondence(
  scenario: Scenario,
  source: Pick<ScenarioHistoricalCorrespondenceSource, 'pastElection' | 'pastDate' | 'pastList'>
): Scenario {
  return {
    ...scenario,
    listCorrespondences: [
      ...scenario.listCorrespondences,
      {
        id: crypto.randomUUID(),
        futureList: defaultDestinationForSource(scenario, source.pastElection, source.pastList),
        pastElection: source.pastElection,
        pastDate: source.pastDate,
        pastList: source.pastList,
        factor: 1,
        source: 'manual'
      }
    ]
  };
}

export function splitScenarioHistoricalCorrespondence(scenario: Scenario, id: string): Scenario {
  const sourceRow = scenario.listCorrespondences.find((row) => row.id === id);
  if (!sourceRow) return scenario;

  const nextDestination = defaultDestinationForSource(scenario, sourceRow.pastElection, sourceRow.pastList);
  const listCorrespondences = scenario.listCorrespondences.map((row) =>
    row.id === id ? { ...row, source: 'manual' as const } : row
  );

  return {
    ...scenario,
    listCorrespondences: [
      ...listCorrespondences,
      {
        ...sourceRow,
        id: crypto.randomUUID(),
        futureList: nextDestination,
        factor: 1,
        source: 'manual'
      }
    ]
  };
}

export function updateScenarioHistoricalCorrespondence(
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

export function removeScenarioHistoricalCorrespondence(scenario: Scenario, id: string): Scenario {
  return {
    ...scenario,
    listCorrespondences: scenario.listCorrespondences.filter((row) => row.id !== id)
  };
}

export function resetScenarioHistoricalCorrespondenceSource(
  scenario: Scenario,
  pastElection: string,
  pastList: string
): Scenario {
  const sourceKey = correspondenceSourceKey(pastElection, pastList);
  const defaults = defaultPoliticsScenario.listCorrespondences
    .filter((row) => correspondenceSourceKey(row.pastElection, row.pastList) === sourceKey)
    .map(cloneCorrespondence);

  return {
    ...scenario,
    listCorrespondences: [
      ...scenario.listCorrespondences.filter((row) => correspondenceSourceKey(row.pastElection, row.pastList) !== sourceKey),
      ...defaults
    ]
  };
}

export function renameScenarioList(scenario: Scenario, id: string, name: string): Scenario {
  const previous = scenario.lists.find((row) => row.id === id);
  if (!previous) return scenario;

  return {
    ...scenario,
    lists: scenario.lists.map((row) => (row.id === id ? { ...row, name } : row)),
    listCorrespondences: scenario.listCorrespondences.map((row) =>
      row.futureList === previous.name ? { ...row, futureList: name, source: 'manual' } : row
    ),
    localShareOverrides: scenario.localShareOverrides.map((row) =>
      row.list === previous.name ? { ...row, list: name } : row
    ),
    candidateTemplates: scenario.candidateTemplates.map((row) =>
      row.kind === 'plurinominal' && row.list === previous.name ? { ...row, list: name } : row
    )
  };
}

export function removeScenarioList(scenario: Scenario, id: string): Scenario {
  const removed = scenario.lists.find((row) => row.id === id);
  if (!removed) return scenario;

  return {
    ...scenario,
    lists: scenario.lists.filter((row) => row.id !== id),
    listCorrespondences: scenario.listCorrespondences.map((row) =>
      row.futureList === removed.name ? { ...row, futureList: politicsAbstentionListName, source: 'manual' } : row
    ),
    localShareOverrides: scenario.localShareOverrides.filter((row) => row.list !== removed.name),
    candidateTemplates: scenario.candidateTemplates.filter(
      (row) => row.kind !== 'plurinominal' || row.list !== removed.name
    )
  };
}

export function normalizeScenario(value: unknown): Scenario {
  const input = value !== null && typeof value === 'object' ? (value as Partial<Scenario>) : {};
  const coalitions = Array.isArray(input.coalitions) ? input.coalitions : [];
  const lists = Array.isArray(input.lists) ? input.lists : [];
  const listCorrespondences = Array.isArray(input.listCorrespondences) ? input.listCorrespondences : [];
  const localShareOverrides = Array.isArray(input.localShareOverrides) ? input.localShareOverrides : [];
  const candidateTemplates = Array.isArray(input.candidateTemplates) ? input.candidateTemplates : [];

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
    }),
    localShareOverrides: localShareOverrides.map((override, index) => {
      const source = override as Partial<ScenarioLocalShareOverride>;
      const locationCode = cleanString(source.locationCode);
      const list = cleanString(source.list);
      return {
        id: cleanString(source.id) || stableId('local-share', `${locationCode}-${list}`, index + 1),
        scope: cleanLocalShareOverrideScope(source.scope),
        locationCode,
        list,
        startingShare: cleanShare(source.startingShare)
      };
    }),
    candidateTemplates: candidateTemplates.map((template, index) => {
      const source = template as Partial<ScenarioCandidateTemplate>;
      const kind = cleanCandidateTemplateKind(source.kind);
      const candidateName = cleanString(source.candidateName);
      const slot =
        kind === 'uninominal'
          ? `${source.ramo}-${source.coalition}-${source.uninominalCode}`
          : `${source.ramo}-${source.list}-${source.plurinominalCode}-${source.candidateNumber}-${source.minority}`;
      return {
        id: cleanString(source.id) || stableId('candidate-template', `${slot}-${candidateName}`, index + 1),
        ramo: cleanCandidateTemplateRamo(source.ramo),
        kind,
        candidateName,
        birthDate: cleanNullableString(source.birthDate),
        coalition: cleanNullableString(source.coalition),
        uninominalCode: cleanNullableString(source.uninominalCode),
        list: cleanNullableString(source.list),
        plurinominalCode: cleanNullableString(source.plurinominalCode),
        candidateNumber: cleanNullableNumber(source.candidateNumber),
        minority: source.minority === true
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
  const localOverrideKeys = new Set<string>();
  const localOverridesByLocation = new Map<string, ScenarioLocalShareOverride[]>();
  const candidateTemplateKeys = new Set<string>();

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
    const key = correspondenceRowKey(correspondence);

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

  for (const override of scenario.localShareOverrides) {
    const locationCode = override.locationCode.trim();
    const list = override.list.trim();
    const listKeyValue = list.toLocaleLowerCase('it-IT');
    const key = [override.scope, locationCode, listKeyValue].join('|');
    const share = Number(override.startingShare);
    const grouped = localOverridesByLocation.get(locationCode) ?? [];

    grouped.push(override);
    localOverridesByLocation.set(locationCode, grouped);

    if (override.scope !== 'municipality') messages.push('Le quote locali supportano solo il livello comunale.');
    if (!locationCode) messages.push('Ogni quota locale deve indicare un comune.');
    if (!listNameSet.has(listKeyValue)) messages.push(`Quota locale verso lista sconosciuta: ${list || 'lista senza nome'}.`);
    if (!Number.isFinite(share) || share < 0 || share > 100) {
      messages.push(`Quota locale non valida per ${list || 'lista senza nome'}.`);
    }
    if (localOverrideKeys.has(key)) {
      messages.push(`Quota locale duplicata per ${locationCode || 'comune'} / ${list || 'lista'}.`);
    }
    localOverrideKeys.add(key);
  }

  for (const [locationCode, overrides] of localOverridesByLocation) {
    const total = overrides.reduce((sum, override) => sum + Math.max(Number(override.startingShare) || 0, 0), 0);
    if (total <= 0) messages.push(`La somma delle quote locali usate per ${locationCode || 'comune'} deve essere maggiore di zero.`);
    if (total > 100.01 && overrides.length < scenario.lists.length) {
      messages.push(`La somma delle quote locali usate per ${locationCode || 'comune'} non puo superare 100.`);
    }
  }

  for (const template of scenario.candidateTemplates) {
    const candidateName = template.candidateName.trim();
    const birthDate = template.birthDate?.trim() ?? '';
    const key =
      template.kind === 'uninominal'
        ? [template.ramo, template.kind, template.coalition?.trim().toLocaleLowerCase('it-IT') ?? '', template.uninominalCode?.trim() ?? ''].join('|')
        : [
            template.ramo,
            template.kind,
            template.list?.trim().toLocaleLowerCase('it-IT') ?? '',
            template.plurinominalCode?.trim() ?? '',
            String(template.candidateNumber ?? ''),
            String(template.minority === true)
          ].join('|');

    if (!candidateName) messages.push('Ogni candidato definito nello scenario deve avere un nome.');
    if (birthDate && Number.isNaN(Date.parse(birthDate))) {
      messages.push(`Data di nascita non valida per ${candidateName || 'candidato senza nome'}.`);
    }

    if (template.kind === 'uninominal') {
      const coalition = template.coalition?.trim() ?? '';
      if (!coalitionNameSet.has(coalition)) {
        messages.push(`Coalizione candidato uninominale sconosciuta: ${coalition || 'coalizione mancante'}.`);
      }
      if (!template.uninominalCode?.trim()) {
        messages.push(`Collegio uninominale mancante per ${candidateName || 'candidato senza nome'}.`);
      }
    } else {
      const list = template.list?.trim() ?? '';
      if (!listNameSet.has(list.toLocaleLowerCase('it-IT'))) {
        messages.push(`Lista candidato plurinominale sconosciuta: ${list || 'lista mancante'}.`);
      }
      if (!template.plurinominalCode?.trim()) {
        messages.push(`Collegio plurinominale mancante per ${candidateName || 'candidato senza nome'}.`);
      }
      if (!Number.isInteger(template.candidateNumber) || (template.candidateNumber ?? 0) < 1) {
        messages.push(`Numero candidato plurinominale non valido per ${candidateName || 'candidato senza nome'}.`);
      }
    }

    if (candidateTemplateKeys.has(key)) {
      messages.push(`Candidato duplicato per lo stesso slot: ${candidateName || 'candidato senza nome'}.`);
    }
    candidateTemplateKeys.add(key);
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
