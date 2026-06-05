import type {
  ElectionKind,
  Scenario,
  ScenarioCandidateGeneration,
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
  ScenarioLocalShareOverrideScope,
  ScenarioPlurinominalCandidacyCountShares
} from '$lib/core/types';
import { generatedDefaultPoliticsScenario } from './politics-defaults.generated';

export const politicsScenarioStorageKey = 'elezioni:web:politics-scenario:v1';
export const politicsScenarioSchemaVersion = 7;

export const defaultPoliticsScenario: Scenario = generatedDefaultPoliticsScenario;
export const politicsAbstentionListName = 'astensione';
const singleCandidacyCandidateGeneration: ScenarioCandidateGeneration = {
  uninominalToPlurinominalShare: 0,
  plurinominalCandidacyCountShares: [1, 0, 0, 0, 0]
};
export const defaultScenarioCandidateGeneration: ScenarioCandidateGeneration = {
  uninominalToPlurinominalShare:
    defaultPoliticsScenario.candidateGeneration?.uninominalToPlurinominalShare ??
    singleCandidacyCandidateGeneration.uninominalToPlurinominalShare,
  plurinominalCandidacyCountShares: [
    ...(defaultPoliticsScenario.candidateGeneration?.plurinominalCandidacyCountShares ??
      singleCandidacyCandidateGeneration.plurinominalCandidacyCountShares)
  ] as ScenarioPlurinominalCandidacyCountShares
};

const vectorLength = 5;
const shareSumTolerance = 1e-9;

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

export interface ScenarioLocalShareOverrideGroup {
  key: string;
  scope: ScenarioLocalShareOverrideScope;
  locationCode: string;
  overrides: ScenarioLocalShareOverride[];
  totalShare: number;
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

function localShareOverrideKey(scope: ScenarioLocalShareOverrideScope, locationCode: string, list: string): string {
  return `${scope}\u001f${locationCode.trim()}\u001f${listKey(list)}`;
}

function candidateTemplateSlotKey(template: ScenarioCandidateTemplate): string {
  if (template.kind === 'uninominal') {
    return [
      template.ramo,
      template.kind,
      listKey(template.coalition ?? ''),
      template.uninominalCode?.trim() ?? ''
    ].join('|');
  }

  return [
    template.ramo,
    template.kind,
    listKey(template.list ?? ''),
    template.plurinominalCode?.trim() ?? '',
    String(template.candidateNumber ?? ''),
    String(template.minority === true)
  ].join('|');
}

function localShareOverrideLocationKey(scope: ScenarioLocalShareOverrideScope, locationCode: string): string {
  return `${scope}\u001f${locationCode.trim()}`;
}

function localShareOverrideScopeLabel(scope: ScenarioLocalShareOverrideScope): string {
  if (scope === 'region') return 'regione';
  if (scope === 'province') return 'provincia';
  return 'comune';
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

function cleanNumber(value: unknown): number {
  return Number(value);
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
  if (value === 'province' || value === 'region') return value;
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

function normalizeCandidateGeneration(value: unknown): ScenarioCandidateGeneration {
  if (value === undefined || value === null || typeof value !== 'object' || Array.isArray(value)) {
    return {
      uninominalToPlurinominalShare: defaultScenarioCandidateGeneration.uninominalToPlurinominalShare,
      plurinominalCandidacyCountShares: [...defaultScenarioCandidateGeneration.plurinominalCandidacyCountShares]
    };
  }

  const source = value as Partial<ScenarioCandidateGeneration>;
  const shares = Array.isArray(source.plurinominalCandidacyCountShares)
    ? source.plurinominalCandidacyCountShares.map(cleanNumber)
    : [...defaultScenarioCandidateGeneration.plurinominalCandidacyCountShares];

  return {
    uninominalToPlurinominalShare:
      source.uninominalToPlurinominalShare === undefined
        ? defaultScenarioCandidateGeneration.uninominalToPlurinominalShare
        : cleanNumber(source.uninominalToPlurinominalShare),
    plurinominalCandidacyCountShares: shares as ScenarioPlurinominalCandidacyCountShares
  };
}

function normalizeShareVector(
  shares: readonly number[],
  fallback: ScenarioPlurinominalCandidacyCountShares = defaultScenarioCandidateGeneration.plurinominalCandidacyCountShares
): ScenarioPlurinominalCandidacyCountShares {
  if (
    shares.length !== vectorLength ||
    shares.some((share) => !Number.isFinite(share) || share < 0) ||
    shares.reduce((sum, share) => sum + share, 0) <= 0
  ) {
    return [...fallback];
  }

  const total = shares.reduce((sum, share) => sum + share, 0);
  return shares.map((share) => share / total) as ScenarioPlurinominalCandidacyCountShares;
}

export function plurinominalCandidacyCountSharesToFractions(
  shares: readonly number[]
): ScenarioPlurinominalCandidacyCountShares {
  const normalizedShares = normalizeShareVector(shares);
  const totalSlots = normalizedShares.reduce((sum, share, index) => sum + share * (index + 1), 0);

  return normalizedShares.map((_, index) => {
    const tail = normalizedShares.slice(index).reduce((sum, share) => sum + share, 0);
    return tail / totalSlots;
  }) as ScenarioPlurinominalCandidacyCountShares;
}

export function pluricandidatureFractionsToPlurinominalCandidacyCountShares(
  fractions: readonly number[]
): ScenarioPlurinominalCandidacyCountShares {
  if (
    fractions.length !== vectorLength ||
    fractions.some((fraction) => !Number.isFinite(fraction) || fraction < 0) ||
    fractions[0] <= 0
  ) {
    return [...defaultScenarioCandidateGeneration.plurinominalCandidacyCountShares];
  }

  const shares = fractions.map((fraction, index) => {
    const next = fractions[index + 1] ?? 0;
    return (fraction - next) / fractions[0];
  });

  return normalizeShareVector(shares);
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
    candidateTemplates: scenario.candidateTemplates.map((template) => ({ ...template })),
    candidateGeneration: {
      uninominalToPlurinominalShare: scenario.candidateGeneration.uninominalToPlurinominalShare,
      plurinominalCandidacyCountShares: [...scenario.candidateGeneration.plurinominalCandidacyCountShares]
    }
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

export function renameScenarioCoalition(scenario: Scenario, id: string, name: string): Scenario {
  const previous = scenario.coalitions.find((row) => row.id === id);
  if (!previous) return scenario;

  return {
    ...scenario,
    coalitions: scenario.coalitions.map((row) => (row.id === id ? { ...row, name } : row)),
    lists: scenario.lists.map((row) => (row.coalition === previous.name ? { ...row, coalition: name } : row)),
    candidateTemplates: scenario.candidateTemplates.map((row) =>
      row.kind === 'uninominal' && row.coalition === previous.name ? { ...row, coalition: name } : row
    )
  };
}

export function removeScenarioCoalition(scenario: Scenario, id: string): Scenario {
  const removed = scenario.coalitions.find((row) => row.id === id);
  if (!removed) return scenario;

  const coalitions = scenario.coalitions.filter((row) => row.id !== id);
  const fallback = coalitions[0]?.name ?? null;

  return {
    ...scenario,
    coalitions,
    lists: scenario.lists.map((row) => (row.coalition === removed.name ? { ...row, coalition: fallback } : row)),
    candidateTemplates: scenario.candidateTemplates.filter(
      (row) => row.kind !== 'uninominal' || row.coalition !== removed.name
    )
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

export function buildScenarioLocalShareOverrideGroups(scenario: Scenario): ScenarioLocalShareOverrideGroup[] {
  const groups = new Map<string, ScenarioLocalShareOverride[]>();

  for (const override of scenario.localShareOverrides) {
    const locationCode = override.locationCode.trim();
    const key = localShareOverrideLocationKey(override.scope, locationCode);
    const grouped = groups.get(key) ?? [];
    grouped.push({ ...override, locationCode });
    groups.set(key, grouped);
  }

  return [...groups.entries()]
    .map(([key, overrides]) => {
      const reference = overrides[0];
      const scope = reference?.scope ?? 'municipality';
      const locationCode = reference?.locationCode.trim() ?? '';
      return {
        key,
        scope,
        locationCode,
        overrides: overrides
          .map((override) => ({ ...override }))
          .sort((left, right) => listKey(left.list).localeCompare(listKey(right.list), 'it')),
        totalShare: overrides.reduce((sum, override) => sum + Math.max(Number(override.startingShare) || 0, 0), 0)
      };
    })
    .sort((left, right) =>
      [left.scope, left.locationCode].join('\u001f').localeCompare([right.scope, right.locationCode].join('\u001f'), 'it')
    );
}

export function upsertScenarioLocalShareOverride(
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
    return {
      ...override,
      scope,
      locationCode,
      list,
      startingShare
    };
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

  return {
    ...scenario,
    localShareOverrides
  };
}

export function updateScenarioLocalShareOverride(
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
            locationCode:
              patch.locationCode === undefined ? override.locationCode : cleanString(patch.locationCode).trim(),
            list: patch.list === undefined ? override.list : cleanString(patch.list).trim(),
            startingShare:
              patch.startingShare === undefined ? override.startingShare : Number(patch.startingShare)
          }
        : override
    )
  };
}

export function removeScenarioLocalShareOverride(scenario: Scenario, id: string): Scenario {
  return {
    ...scenario,
    localShareOverrides: scenario.localShareOverrides.filter((override) => override.id !== id)
  };
}

export function removeScenarioLocalShareOverridesForLocation(
  scenario: Scenario,
  locationCode: string,
  scope: ScenarioLocalShareOverrideScope = 'municipality'
): Scenario {
  const normalizedLocationCode = locationCode.trim();

  return {
    ...scenario,
    localShareOverrides: scenario.localShareOverrides.filter(
      (override) => override.scope !== scope || override.locationCode.trim() !== normalizedLocationCode
    )
  };
}

function cleanCandidateTemplate(
  scenario: Scenario,
  template: Partial<ScenarioCandidateTemplate>,
  fallback?: ScenarioCandidateTemplate
): ScenarioCandidateTemplate {
  const kind = cleanCandidateTemplateKind(template.kind ?? fallback?.kind);
  const ramo = cleanCandidateTemplateRamo(template.ramo ?? fallback?.ramo);
  const candidateName = cleanString(template.candidateName ?? fallback?.candidateName);
  const birthDate = cleanNullableString(template.birthDate ?? fallback?.birthDate);

  if (kind === 'uninominal') {
    return {
      id: cleanString(template.id ?? fallback?.id) || crypto.randomUUID(),
      ramo,
      kind,
      candidateName,
      birthDate,
      coalition:
        cleanNullableString(template.coalition ?? fallback?.coalition) ?? scenario.coalitions[0]?.name ?? null,
      uninominalCode: cleanNullableString(template.uninominalCode ?? fallback?.uninominalCode),
      list: null,
      plurinominalCode: null,
      candidateNumber: null,
      minority: false
    };
  }

  return {
    id: cleanString(template.id ?? fallback?.id) || crypto.randomUUID(),
    ramo,
    kind,
    candidateName,
    birthDate,
    coalition: null,
    uninominalCode: null,
    list: cleanNullableString(template.list ?? fallback?.list) ?? scenario.lists[0]?.name ?? null,
    plurinominalCode: cleanNullableString(template.plurinominalCode ?? fallback?.plurinominalCode),
    candidateNumber: cleanNullableNumber(template.candidateNumber ?? fallback?.candidateNumber),
    minority: template.minority ?? fallback?.minority ?? false
  };
}

export function createScenarioCandidateTemplate(
  scenario: Scenario,
  input: Partial<ScenarioCandidateTemplate> = {}
): ScenarioCandidateTemplate {
  return cleanCandidateTemplate(scenario, input);
}

export function upsertScenarioCandidateTemplate(
  scenario: Scenario,
  input: Partial<ScenarioCandidateTemplate>
): Scenario {
  const template = createScenarioCandidateTemplate(scenario, input);
  const slotKey = candidateTemplateSlotKey(template);
  let matched = false;
  const candidateTemplates = scenario.candidateTemplates.map((row) => {
    if (candidateTemplateSlotKey(row) !== slotKey) return row;
    matched = true;
    return { ...template, id: row.id };
  });

  if (!matched) candidateTemplates.push(template);

  return {
    ...scenario,
    candidateTemplates
  };
}

export function updateScenarioCandidateTemplate(
  scenario: Scenario,
  id: string,
  patch: Partial<ScenarioCandidateTemplate>
): Scenario {
  return {
    ...scenario,
    candidateTemplates: scenario.candidateTemplates.map((template) =>
      template.id === id ? cleanCandidateTemplate(scenario, { ...template, ...patch, id: template.id }, template) : template
    )
  };
}

export function removeScenarioCandidateTemplate(scenario: Scenario, id: string): Scenario {
  return {
    ...scenario,
    candidateTemplates: scenario.candidateTemplates.filter((template) => template.id !== id)
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
    }),
    candidateGeneration: normalizeCandidateGeneration(input.candidateGeneration)
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
  const candidateGeneration = scenario.candidateGeneration;
  const candidacyShares = candidateGeneration.plurinominalCandidacyCountShares;
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
  if (overrideLists.length > 0 && totalOverrideShare <= 0) {
    messages.push('La somma delle quote usate deve essere maggiore di zero.');
  }
  if (totalOverrideShare > 100.01) messages.push('La somma delle quote usate non puo superare 100.');
  if (
    !Number.isFinite(candidateGeneration.uninominalToPlurinominalShare) ||
    candidateGeneration.uninominalToPlurinominalShare < 0 ||
    candidateGeneration.uninominalToPlurinominalShare > 1
  ) {
    messages.push('Quota uninominali in plurinominale non valida.');
  }
  if (candidacyShares.length !== vectorLength) {
    messages.push('La distribuzione delle pluricandidature deve avere cinque valori.');
  } else {
    if (candidacyShares.some((share) => !Number.isFinite(share) || share < 0)) {
      messages.push('Distribuzione pluricandidature non valida.');
    }
    if (Math.abs(candidacyShareTotal - 1) > shareSumTolerance) {
      messages.push('La somma della distribuzione pluricandidature deve essere 1.');
    }
  }

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
    const locationKey = localShareOverrideLocationKey(override.scope, locationCode);
    const locationLabel = `${localShareOverrideScopeLabel(override.scope)} ${locationCode || 'senza codice'}`;
    const grouped = localOverridesByLocation.get(locationKey) ?? [];

    grouped.push(override);
    localOverridesByLocation.set(locationKey, grouped);

    if (!['municipality', 'province', 'region'].includes(override.scope)) {
      messages.push('Ambito quota locale non valido.');
    }
    if (!locationCode) messages.push('Ogni quota locale deve indicare una localita.');
    if (!listNameSet.has(listKeyValue)) messages.push(`Quota locale verso lista sconosciuta: ${list || 'lista senza nome'}.`);
    if (!Number.isFinite(share) || share < 0 || share > 100) {
      messages.push(`Quota locale non valida per ${list || 'lista senza nome'}.`);
    }
    if (localOverrideKeys.has(key)) {
      messages.push(`Quota locale duplicata per ${locationLabel} / ${list || 'lista'}.`);
    }
    localOverrideKeys.add(key);
  }

  for (const [locationKey, overrides] of localOverridesByLocation) {
    const [scope, locationCode] = locationKey.split('\u001f') as [ScenarioLocalShareOverrideScope, string];
    const locationLabel = `${localShareOverrideScopeLabel(scope)} ${locationCode || 'senza codice'}`;
    const total = overrides.reduce((sum, override) => sum + Math.max(Number(override.startingShare) || 0, 0), 0);
    if (total <= 0) messages.push(`La somma delle quote locali usate per ${locationLabel} deve essere maggiore di zero.`);
    if (total > 100.01 && overrides.length < scenario.lists.length) {
      messages.push(`La somma delle quote locali usate per ${locationLabel} non puo superare 100.`);
    }
  }

  for (const template of scenario.candidateTemplates) {
    const candidateName = template.candidateName.trim();
    const birthDate = template.birthDate?.trim() ?? '';
    const key = candidateTemplateSlotKey(template);

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
