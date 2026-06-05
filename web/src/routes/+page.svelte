<script lang="ts">
  import { ChevronDown, ChevronUp, Download, Plus, Play, RotateCcw, Trash2, Upload } from '@lucide/svelte';
  import { browser } from '$app/environment';
  import { onMount, tick } from 'svelte';
  import SimulationWorker from '$lib/workers/simulation.worker?worker';
  import PoliticsResultCharts from '$lib/politics/PoliticsResultCharts.svelte';
  import type {
    ResultTable,
    Scenario,
    ScenarioCandidateTemplate,
    ScenarioCandidateTemplateKind,
    ScenarioCandidateTemplateRamo,
    ScenarioListCorrespondence,
    ScenarioLocalShareOverride,
    ScenarioLocalShareOverrideScope,
    ScrutinyWarning,
    SimulationRequest,
    SimulationResult,
    SimulationWorkerMessage
  } from '$lib/core/types';
  import type { PoliticsMunicipalityCatalogRow } from '$lib/politics/types';
  import {
    createSimulationResultExport,
    parseSimulationResultExport,
    resultTablesToCsv,
    type SimulationResultExport
  } from '$lib/core/result-export';
  import {
    buildPoliticsPlurinominalChart,
    buildPoliticsPlurinominalChartOptions,
    buildPoliticsResultCharts
  } from '$lib/politics/result-charts';
  import { politicsResultPlotTableNames } from '$lib/politics/result-presentation';
  import {
    addScenarioHistoricalCorrespondence,
    buildScenarioHistoricalCorrespondenceGroups,
    buildScenarioLocalShareOverrideGroups,
    cloneScenario,
    createDefaultPoliticsScenario,
    createScenarioCoalition,
    removeScenarioCandidateTemplate,
    removeScenarioCoalition as removeScenarioCoalitionFromScenario,
    createScenarioList,
    politicsAbstentionListName,
    parseScenario,
    politicsScenarioStorageKey,
    removeScenarioHistoricalCorrespondence,
    removeScenarioList as removeScenarioListFromScenario,
    removeScenarioLocalShareOverride,
    removeScenarioLocalShareOverridesForLocation,
    renameScenarioList,
    resetScenarioHistoricalCorrespondenceSource,
    serializeScenario,
    renameScenarioCoalition,
    updateScenarioCandidateTemplate,
    updateScenarioHistoricalCorrespondence,
    updateScenarioLocalShareOverride,
    upsertScenarioCandidateTemplate,
    upsertScenarioLocalShareOverride,
    validateScenario
  } from '$lib/scenario/politics';
  import {
    candidateSlotCode,
    candidateSlotLabel,
    candidateSlotSearchText,
    candidateSlotSublabel,
    type PoliticsCandidateSlot
  } from '$lib/scenario/politics-candidate-slots';
  import { generatedPoliticsCandidateSlots } from '$lib/scenario/politics-candidate-slots.generated';
  import { generatedPoliticsMunicipalities } from '$lib/scenario/politics-municipalities.generated';

  type UiMessageSeverity = 'info' | 'warning' | 'error';

  interface UiMessage {
    code: string;
    message: string;
    severity: UiMessageSeverity;
  }

  interface LocalOverrideLocationOption {
    scope: ScenarioLocalShareOverrideScope;
    code: string;
    label: string;
    sublabel: string;
    municipalityCount: number;
    searchText: string;
  }

  interface CandidateTemplateGroup {
    key: string;
    label: string;
    templates: ScenarioCandidateTemplate[];
  }

  const dataVersion = 'v1';
  const diagnosticTableNames = new Set(['Generated pipeline runs']);
  const internalTableNames = new Set<string>(politicsResultPlotTableNames);
  const candidacyCountOptions = [1, 2, 3, 4, 5] as const;
  const candidateTemplateRamoOptions: ScenarioCandidateTemplateRamo[] = ['camera', 'senato'];
  const candidateTemplateKindOptions: ScenarioCandidateTemplateKind[] = ['uninominal', 'plurinominal'];

  let simulations = $state(10);
  let seed = $state('politiche-2027');
  let running = $state(false);
  let busyAction = $state('');
  let phase = $state('idle');
  let elapsedMs = $state(0);
  let tables = $state<ResultTable[]>([]);
  let messages = $state<UiMessage[]>([]);
  let lastResult = $state<SimulationResult | null>(null);
  let showDiagnostics = $state(false);
  let showAdvancedScenario = $state(false);
  let selectedPlurinominalOptionId = $state('');
  let selectedLocalOverrideScope = $state<ScenarioLocalShareOverrideScope>('municipality');
  let localOverrideSearch = $state('');
  let selectedLocalOverrideLocationCode = $state('');
  let selectedLocalOverrideList = $state('');
  let localOverrideShare = $state(0);
  let candidateTemplateRamo = $state<ScenarioCandidateTemplateRamo>('camera');
  let candidateTemplateKind = $state<ScenarioCandidateTemplateKind>('uninominal');
  let candidateTemplateCoalition = $state('');
  let candidateTemplateList = $state('');
  let candidateTemplateSlotSearch = $state('');
  let selectedCandidateTemplateSlotCode = $state('');
  let candidateTemplateNumber = $state(1);
  let candidateTemplateMinority = $state(false);
  let candidateTemplateName = $state('');
  let candidateTemplateBirthDate = $state('');
  let scenarioDraft = $state<Scenario>(createDefaultPoliticsScenario());
  let scenarioStorageReady = $state(false);
  let compressionSupported = $state(false);
  let fileInput: HTMLInputElement | undefined;
  let resultFileInput: HTMLInputElement | undefined;

  const scenario = $derived(cloneScenario(scenarioDraft));
  const validationMessages = $derived(validateScenario(scenario));
  const isBusy = $derived(running || busyAction !== '');
  const canRun = $derived(!isBusy && validationMessages.length === 0);
  const runButtonLabel = $derived(running ? phase : 'Esegui');
  const elapsedLabel = $derived(`${elapsedMs.toFixed(0)} ms`);
  const hasResult = $derived(lastResult !== null);
  const infoMessages = $derived(messages.filter((message) => message.severity === 'info'));
  const warningMessages = $derived(messages.filter((message) => message.severity !== 'info'));
  const primaryTables = $derived(
    tables
      .filter((table) => !diagnosticTableNames.has(table.name) && !internalTableNames.has(table.name))
      .sort((left, right) => resultTablePriority(left.name) - resultTablePriority(right.name))
  );
  const resultCharts = $derived(buildPoliticsResultCharts(tables, scenario));
  const plurinominalOptions = $derived(buildPoliticsPlurinominalChartOptions(tables, scenario));
  const selectedPlurinominalOption = $derived(
    plurinominalOptions.find((option) => option.id === selectedPlurinominalOptionId) ?? plurinominalOptions[0] ?? null
  );
  const plurinominalChart = $derived(buildPoliticsPlurinominalChart(tables, scenario, selectedPlurinominalOption));
  const diagnosticTables = $derived(tables.filter((table) => diagnosticTableNames.has(table.name)));
  const diagnosticsToggleLabel = $derived(showDiagnostics ? 'Nascondi dettagli' : 'Mostra dettagli');
  const historicalCorrespondenceGroups = $derived(buildScenarioHistoricalCorrespondenceGroups(scenarioDraft));
  const localOverrideGroups = $derived(buildScenarioLocalShareOverrideGroups(scenarioDraft));
  const localOverrideAllLocationOptions = $derived(buildLocalOverrideLocationOptions(generatedPoliticsMunicipalities));
  const localOverrideLocationOptions = $derived(
    localOverrideAllLocationOptions.filter((option) => option.scope === selectedLocalOverrideScope)
  );
  const localOverrideLocationByKey = $derived(
    new Map(localOverrideAllLocationOptions.map((option) => [localOverrideLocationOptionKey(option.scope, option.code), option]))
  );
  const localOverrideSearchResults = $derived(
    searchLocalOverrideLocations(localOverrideSearch, localOverrideLocationOptions)
  );
  const selectedLocalOverrideLocation = $derived(
    localOverrideLocationByKey.get(localOverrideLocationOptionKey(selectedLocalOverrideScope, selectedLocalOverrideLocationCode)) ??
      null
  );
  const bundledListCorrespondenceCount = $derived(
    scenarioDraft.listCorrespondences.filter((correspondence) => correspondence.source === 'bundled').length
  );
  const manualListCorrespondenceCount = $derived(
    scenarioDraft.listCorrespondences.filter((correspondence) => correspondence.source === 'manual').length
  );
  const correspondenceDestinations = $derived([...scenarioDraft.lists.map((list) => list.name), politicsAbstentionListName]);
  const localOverrideListSelection = $derived(selectedLocalOverrideList || scenarioDraft.lists[0]?.name || '');
  const localOverrideSummary = $derived(
    `${scenarioDraft.localShareOverrides.length} quote / ${localOverrideGroups.length} aree`
  );
  const candidateTemplateSlotOptions = $derived(
    candidateTemplateSlots(candidateTemplateRamo, candidateTemplateKind)
  );
  const selectedCandidateTemplateSlot = $derived(
    findCandidateTemplateSlot(candidateTemplateRamo, candidateTemplateKind, selectedCandidateTemplateSlotCode)
  );
  const candidateTemplateSearchResults = $derived(
    searchCandidateTemplateSlots(candidateTemplateSlotSearch, candidateTemplateSlotOptions)
  );
  const candidateTemplateCoalitionSelection = $derived(
    candidateTemplateCoalition || scenarioDraft.coalitions[0]?.name || ''
  );
  const candidateTemplateListSelection = $derived(candidateTemplateList || scenarioDraft.lists[0]?.name || '');
  const candidateTemplateNumberOptions = $derived(
    plurinominalCandidateNumbers(selectedCandidateTemplateSlot)
  );
  const candidateTemplateCanAdd = $derived(
    Boolean(
      resolveCandidateTemplateSlotCode() &&
        candidateTemplateName.trim() &&
        (candidateTemplateKind === 'uninominal' ? candidateTemplateCoalitionSelection : candidateTemplateListSelection)
    )
  );
  const candidateTemplateGroups = $derived(buildCandidateTemplateGroups(scenarioDraft.candidateTemplates));
  const candidateTemplateSummary = $derived(`${scenarioDraft.candidateTemplates.length} candidati`);
  const editablePlurinominalCandidacyShareTotal = $derived(
    scenarioDraft.candidateGeneration.plurinominalCandidacyCountShares
      .slice(1)
      .reduce((sum, share) => sum + Number(share), 0)
  );
  const calculatedSingleCandidacyShare = $derived(
    Number.isFinite(editablePlurinominalCandidacyShareTotal)
      ? Math.max(0, 1 - editablePlurinominalCandidacyShareTotal)
      : Number.NaN
  );
  const plurinominalCandidacyShareTotal = $derived(
    calculatedSingleCandidacyShare + editablePlurinominalCandidacyShareTotal
  );
  const plurinominalCandidacyShareTotalLabel = $derived(
    Number.isFinite(plurinominalCandidacyShareTotal) ? plurinominalCandidacyShareTotal.toFixed(3) : 'non valido'
  );

  onMount(() => {
    if (!browser) return;

    compressionSupported = 'CompressionStream' in globalThis && 'DecompressionStream' in globalThis;

    const stored = localStorage.getItem(politicsScenarioStorageKey);
    if (stored) {
      try {
        scenarioDraft = parseScenario(stored);
      } catch (error) {
        messages = [
          uiMessage('SCENARIO_STORAGE_ERROR', error instanceof Error ? error.message : String(error), 'warning')
        ];
      }
    }

    scenarioStorageReady = true;
  });

  $effect(() => {
    if (!browser || !scenarioStorageReady) return;
    localStorage.setItem(politicsScenarioStorageKey, serializeScenario(scenario));
  });

  function addList(): void {
    scenarioDraft.lists = [...scenarioDraft.lists, createScenarioList(scenarioDraft.coalitions, scenarioDraft.lists)];
  }

  function removeList(id: string): void {
    scenarioDraft = removeScenarioListFromScenario(scenarioDraft, id);
  }

  function addCoalition(): void {
    scenarioDraft.coalitions = [...scenarioDraft.coalitions, createScenarioCoalition(scenarioDraft.coalitions)];
  }

  function updateCoalitionName(id: string, name: string): void {
    scenarioDraft = renameScenarioCoalition(scenarioDraft, id, name);
  }

  function removeCoalition(id: string): void {
    scenarioDraft = removeScenarioCoalitionFromScenario(scenarioDraft, id);
  }

  function updateListCorrespondence(id: string, patch: Partial<ScenarioListCorrespondence>): void {
    scenarioDraft = updateScenarioHistoricalCorrespondence(scenarioDraft, id, patch);
  }

  function removeListCorrespondence(id: string): void {
    scenarioDraft = removeScenarioHistoricalCorrespondence(scenarioDraft, id);
  }

  function addListCorrespondenceForSource(pastElection: string, pastDate: string, pastList: string): void {
    scenarioDraft = addScenarioHistoricalCorrespondence(scenarioDraft, { pastElection, pastDate, pastList });
  }

  function resetListCorrespondenceSource(pastElection: string, pastList: string): void {
    scenarioDraft = resetScenarioHistoricalCorrespondenceSource(scenarioDraft, pastElection, pastList);
  }

  function catalogCode(value: string | number): string {
    return String(value);
  }

  function normalizeSearchText(value: string): string {
    return value
      .trim()
      .toLocaleLowerCase('it-IT')
      .normalize('NFKD')
      .replace(/[\u0300-\u036f]/g, '');
  }

  function candidateTemplateRamoLabel(ramo: ScenarioCandidateTemplateRamo): string {
    return ramo === 'senato' ? 'Senato' : 'Camera';
  }

  function candidateTemplateKindLabel(kind: ScenarioCandidateTemplateKind): string {
    return kind === 'uninominal' ? 'Uninominale' : 'Plurinominale';
  }

  function candidateTemplateSlots(
    ramo: ScenarioCandidateTemplateRamo,
    kind: ScenarioCandidateTemplateKind
  ): PoliticsCandidateSlot[] {
    const rows = kind === 'uninominal' ? generatedPoliticsCandidateSlots.uninominal : generatedPoliticsCandidateSlots.plurinominal;
    return rows.filter((slot) => slot.ramo === ramo);
  }

  function findCandidateTemplateSlot(
    ramo: ScenarioCandidateTemplateRamo,
    kind: ScenarioCandidateTemplateKind,
    code: string | null | undefined
  ): PoliticsCandidateSlot | null {
    const slotCode = String(code ?? '').trim();
    if (!slotCode) return null;
    return candidateTemplateSlots(ramo, kind).find((slot) => candidateSlotCode(slot) === slotCode) ?? null;
  }

  function searchCandidateTemplateSlots(query: string, options: PoliticsCandidateSlot[]): PoliticsCandidateSlot[] {
    const normalizedQuery = normalizeSearchText(query);
    if (normalizedQuery.length < 2) return [];

    const tokens = normalizedQuery.split(/\s+/).filter(Boolean);
    return options
      .filter((option) => tokens.every((token) => normalizeSearchText(candidateSlotSearchText(option)).includes(token)))
      .slice(0, 10);
  }

  function selectCandidateTemplateSlot(slot: PoliticsCandidateSlot): void {
    selectedCandidateTemplateSlotCode = candidateSlotCode(slot);
    candidateTemplateSlotSearch = candidateSlotLabel(slot);

    if ('maxCandidates' in slot) {
      candidateTemplateNumber = Math.min(Math.max(candidateTemplateNumber, 1), Math.max(slot.maxCandidates, 1));
    }
  }

  function resetCandidateTemplateSlotSelection(): void {
    candidateTemplateSlotSearch = '';
    selectedCandidateTemplateSlotCode = '';
    candidateTemplateNumber = 1;
  }

  function resolveCandidateTemplateSlotCode(): string {
    if (selectedCandidateTemplateSlotCode) return selectedCandidateTemplateSlotCode;

    const normalizedSearch = normalizeSearchText(candidateTemplateSlotSearch);
    const exactMatch = candidateTemplateSlotOptions.find(
      (option) =>
        normalizeSearchText(candidateSlotCode(option)) === normalizedSearch ||
        normalizeSearchText(candidateSlotLabel(option)) === normalizedSearch
    );

    return exactMatch ? candidateSlotCode(exactMatch) : '';
  }

  function plurinominalCandidateNumbers(slot: PoliticsCandidateSlot | null): number[] {
    const maxCandidates = slot && 'maxCandidates' in slot ? Math.max(1, Math.floor(slot.maxCandidates)) : 4;
    return Array.from({ length: maxCandidates }, (_, index) => index + 1);
  }

  function plurinominalCandidateNumbersForTemplate(template: ScenarioCandidateTemplate): number[] {
    const slot = findCandidateTemplateSlot(template.ramo, 'plurinominal', template.plurinominalCode);
    const options = plurinominalCandidateNumbers(slot);
    const current = Number(template.candidateNumber);

    if (Number.isInteger(current) && current > 0 && !options.includes(current)) {
      return [...options, current].sort((left, right) => left - right);
    }

    return options;
  }

  function candidateTemplateSlotDisplay(template: ScenarioCandidateTemplate): string {
    const slot = findCandidateTemplateSlot(
      template.ramo,
      template.kind,
      template.kind === 'uninominal' ? template.uninominalCode : template.plurinominalCode
    );
    const code = template.kind === 'uninominal' ? template.uninominalCode : template.plurinominalCode;
    return slot ? candidateSlotLabel(slot) : `${template.kind === 'uninominal' ? 'UNI' : 'PLURI'} ${code ?? ''}`.trim();
  }

  function candidateTemplateTargetLabel(template: ScenarioCandidateTemplate): string {
    if (template.kind === 'uninominal') {
      return `${template.coalition ?? 'Coalizione'} - ${candidateTemplateSlotDisplay(template)}`;
    }

    return `${template.list ?? 'Lista'} - ${candidateTemplateSlotDisplay(template)} #${template.candidateNumber ?? '?'}`;
  }

  function buildCandidateTemplateGroups(templates: readonly ScenarioCandidateTemplate[]): CandidateTemplateGroup[] {
    const groups = new Map<string, CandidateTemplateGroup>();

    for (const ramo of candidateTemplateRamoOptions) {
      for (const kind of candidateTemplateKindOptions) {
        const key = `${ramo}\u001f${kind}`;
        groups.set(key, {
          key,
          label: `${candidateTemplateRamoLabel(ramo)} ${candidateTemplateKindLabel(kind).toLowerCase()}`,
          templates: []
        });
      }
    }

    for (const template of templates) {
      const key = `${template.ramo}\u001f${template.kind}`;
      const group = groups.get(key);
      if (!group) continue;
      group.templates.push(template);
    }

    return [...groups.values()]
      .map((group) => ({
        ...group,
        templates: [...group.templates].sort((left, right) =>
          candidateTemplateTargetLabel(left).localeCompare(candidateTemplateTargetLabel(right), 'it', { numeric: true })
        )
      }))
      .filter((group) => group.templates.length > 0);
  }

  function localOverrideLocationOptionKey(scope: ScenarioLocalShareOverrideScope, code: string): string {
    return `${scope}\u001f${code}`;
  }

  function localOverrideScopeLabel(scope: ScenarioLocalShareOverrideScope): string {
    if (scope === 'region') return 'Regione';
    if (scope === 'province') return 'Provincia';
    return 'Comune';
  }

  function buildLocalOverrideLocationOptions(
    rows: readonly PoliticsMunicipalityCatalogRow[]
  ): LocalOverrideLocationOption[] {
    const municipalityOptions: LocalOverrideLocationOption[] = rows.map((row) => {
      const code = catalogCode(row.CODICE_COMUNE);
      const label = `${row.COMUNE} (${code})`;
      const sublabel = [row.PROVINCIA, row.REGIONE].filter(Boolean).join(' - ');

      return {
        scope: 'municipality',
        code,
        label,
        sublabel,
        municipalityCount: 1,
        searchText: normalizeSearchText([code, row.COMUNE, row.PROVINCIA, row.REGIONE].join(' '))
      };
    });
    const provinceGroups = new Map<string, PoliticsMunicipalityCatalogRow[]>();
    const regionGroups = new Map<string, PoliticsMunicipalityCatalogRow[]>();

    for (const row of rows) {
      const provinceCode = catalogCode(row.CODICE_PROVINCIA);
      const regionCode = catalogCode(row.CODICE_REGIONE);
      provinceGroups.set(provinceCode, [...(provinceGroups.get(provinceCode) ?? []), row]);
      regionGroups.set(regionCode, [...(regionGroups.get(regionCode) ?? []), row]);
    }

    const provinceOptions = [...provinceGroups.entries()].map(([code, municipalities]) => {
      const reference = municipalities[0];
      const label = `${reference?.PROVINCIA ?? code} (${code})`;
      const sublabel = `${reference?.REGIONE ?? ''} - ${municipalities.length} comuni`;
      return {
        scope: 'province' as const,
        code,
        label,
        sublabel,
        municipalityCount: municipalities.length,
        searchText: normalizeSearchText([code, reference?.PROVINCIA ?? '', reference?.REGIONE ?? ''].join(' '))
      };
    });
    const regionOptions = [...regionGroups.entries()].map(([code, municipalities]) => {
      const reference = municipalities[0];
      const label = `${reference?.REGIONE ?? code} (${code})`;
      const sublabel = `${municipalities.length} comuni`;
      return {
        scope: 'region' as const,
        code,
        label,
        sublabel,
        municipalityCount: municipalities.length,
        searchText: normalizeSearchText([code, reference?.REGIONE ?? ''].join(' '))
      };
    });

    return [...municipalityOptions, ...provinceOptions, ...regionOptions].sort((left, right) =>
      [left.scope, left.label, left.code].join('\u001f').localeCompare([right.scope, right.label, right.code].join('\u001f'), 'it')
    );
  }

  function searchLocalOverrideLocations(query: string, options: LocalOverrideLocationOption[]): LocalOverrideLocationOption[] {
    const normalizedQuery = normalizeSearchText(query);
    if (normalizedQuery.length < 2) return [];

    const tokens = normalizedQuery.split(/\s+/).filter(Boolean);
    return options.filter((option) => tokens.every((token) => option.searchText.includes(token))).slice(0, 10);
  }

  function localOverrideLocationDisplay(scope: ScenarioLocalShareOverrideScope, locationCode: string): string {
    return (
      localOverrideLocationByKey.get(localOverrideLocationOptionKey(scope, locationCode))?.label ??
      `${localOverrideScopeLabel(scope)} ${locationCode}`
    );
  }

  function localOverrideLocationSubLabel(scope: ScenarioLocalShareOverrideScope, locationCode: string): string {
    return localOverrideLocationByKey.get(localOverrideLocationOptionKey(scope, locationCode))?.sublabel ?? '';
  }

  function selectLocalOverrideLocation(option: LocalOverrideLocationOption): void {
    selectedLocalOverrideScope = option.scope;
    selectedLocalOverrideLocationCode = option.code;
    localOverrideSearch = option.label;
  }

  function resolveLocalOverrideLocationCode(): string {
    if (selectedLocalOverrideLocationCode) return selectedLocalOverrideLocationCode;

    const normalizedSearch = normalizeSearchText(localOverrideSearch);
    const exactMatch = localOverrideLocationOptions.find(
      (option) =>
        normalizeSearchText(option.code) === normalizedSearch ||
        normalizeSearchText(option.label) === normalizedSearch
    );

    return exactMatch?.code ?? '';
  }

  function upsertLocalOverride(
    scope: ScenarioLocalShareOverrideScope,
    locationCode: string,
    list: string,
    startingShare: string | number
  ): void {
    if (!locationCode || !list) return;

    scenarioDraft = upsertScenarioLocalShareOverride(scenarioDraft, {
      scope,
      locationCode,
      list,
      startingShare: Number(startingShare)
    });
  }

  function addSelectedLocalOverride(): void {
    const locationCode = resolveLocalOverrideLocationCode();
    const list = localOverrideListSelection;
    if (!locationCode || !list) return;

    selectedLocalOverrideLocationCode = locationCode;
    upsertLocalOverride(selectedLocalOverrideScope, locationCode, list, localOverrideShare);
  }

  function updateLocalOverride(id: string, patch: Partial<ScenarioLocalShareOverride>): void {
    scenarioDraft = updateScenarioLocalShareOverride(scenarioDraft, id, patch);
  }

  function removeLocalOverride(id: string): void {
    scenarioDraft = removeScenarioLocalShareOverride(scenarioDraft, id);
  }

  function removeLocalOverridesForLocation(scope: ScenarioLocalShareOverrideScope, locationCode: string): void {
    scenarioDraft = removeScenarioLocalShareOverridesForLocation(scenarioDraft, locationCode, scope);
  }

  function addLocalOverrideForLocation(scope: ScenarioLocalShareOverrideScope, locationCode: string): void {
    const existingLists = new Set(
      scenarioDraft.localShareOverrides
        .filter((override) => override.scope === scope && override.locationCode.trim() === locationCode.trim())
        .map((override) => normalizeSearchText(override.list))
    );
    const list = scenarioDraft.lists.find((candidate) => !existingLists.has(normalizeSearchText(candidate.name)));

    if (!list) return;
    upsertLocalOverride(scope, locationCode, list.name, 0);
  }

  function addCandidateTemplate(): void {
    const slotCode = resolveCandidateTemplateSlotCode();
    const candidateName = candidateTemplateName.trim();
    if (!slotCode || !candidateName) return;

    scenarioDraft = upsertScenarioCandidateTemplate(
      scenarioDraft,
      candidateTemplateKind === 'uninominal'
        ? {
            ramo: candidateTemplateRamo,
            kind: candidateTemplateKind,
            coalition: candidateTemplateCoalitionSelection,
            uninominalCode: slotCode,
            candidateName,
            birthDate: candidateTemplateBirthDate || null
          }
        : {
            ramo: candidateTemplateRamo,
            kind: candidateTemplateKind,
            list: candidateTemplateListSelection,
            plurinominalCode: slotCode,
            candidateNumber: candidateTemplateNumber,
            minority: candidateTemplateMinority,
            candidateName,
            birthDate: candidateTemplateBirthDate || null
          }
    );
    candidateTemplateName = '';
    candidateTemplateBirthDate = '';
  }

  function updateCandidateTemplate(id: string, patch: Partial<ScenarioCandidateTemplate>): void {
    scenarioDraft = updateScenarioCandidateTemplate(scenarioDraft, id, patch);
  }

  function removeCandidateTemplate(id: string): void {
    scenarioDraft = removeScenarioCandidateTemplate(scenarioDraft, id);
  }

  function updateUninominalToPlurinominalShare(value: string | number): void {
    scenarioDraft = {
      ...scenarioDraft,
      candidateGeneration: {
        ...scenarioDraft.candidateGeneration,
        uninominalToPlurinominalShare: Number(value)
      }
    };
  }

  function updatePlurinominalCandidacyCountShare(count: number, value: string | number): void {
    if (count === 1) return;

    const shares = [
      ...scenarioDraft.candidateGeneration.plurinominalCandidacyCountShares
    ] as Scenario['candidateGeneration']['plurinominalCandidacyCountShares'];
    const rawValue = Number(value);
    const otherEditableShareTotal = shares
      .slice(1)
      .reduce((sum, share, index) => sum + (index === count - 2 ? 0 : Number(share)), 0);
    const remaining = Math.max(0, 1 - otherEditableShareTotal);
    shares[count - 1] = Number.isFinite(rawValue) ? Math.min(Math.max(rawValue, 0), remaining) : rawValue;
    shares[0] = Math.max(0, 1 - shares.slice(1).reduce((sum, share) => sum + Number(share), 0));

    scenarioDraft = {
      ...scenarioDraft,
      candidateGeneration: {
        ...scenarioDraft.candidateGeneration,
        plurinominalCandidacyCountShares: shares
      }
    };
  }

  function resetScenario(): void {
    scenarioDraft = createDefaultPoliticsScenario();
    messages = [];
    clearDisplayedResults();
    showAdvancedScenario = false;
  }

  function clearDisplayedResults(): void {
    tables = [];
    lastResult = null;
    elapsedMs = 0;
    phase = 'idle';
    showDiagnostics = false;
    selectedPlurinominalOptionId = '';
  }

  function scenarioFilename(): string {
    const slug = scenario.name
      .trim()
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-|-$/g, '');
    return `${slug || 'scenario'}.json`;
  }

  async function withBusy<T>(label: string, action: () => T | Promise<T>): Promise<T> {
    busyAction = label;
    await tick();
    await nextFrame();

    try {
      return await action();
    } finally {
      busyAction = '';
    }
  }

  function nextFrame(): Promise<void> {
    if (!browser) return Promise.resolve();
    return new Promise((resolve) => requestAnimationFrame(() => resolve()));
  }

  async function downloadScenario(): Promise<void> {
    if (!browser) return;

    await withBusy('Preparo il file scenario...', () =>
      downloadText(serializeScenario(scenario), 'application/json', scenarioFilename())
    );
  }

  function downloadText(content: string, type: string, filename: string): void {
    downloadBlob(new Blob([content], { type }), filename);
  }

  function downloadBlob(blob: Blob, filename: string): void {
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = filename;
    link.click();
    URL.revokeObjectURL(url);
  }

  function resultFilename(extension: 'csv' | 'json.gz'): string {
    return `${scenarioFilename().replace(/\.json$/, '')}-risultati.${extension}`;
  }

  function currentResultExport() {
    if (!lastResult) return null;

    return createSimulationResultExport({
      result: lastResult,
      scenario,
      exportedAt: new Date().toISOString()
    });
  }

  async function gzipText(text: string): Promise<Blob> {
    if (!compressionSupported) throw new Error('Compressione gzip non supportata da questo browser.');

    const stream = new Blob([text], { type: 'application/json' })
      .stream()
      .pipeThrough(new CompressionStream('gzip'));
    return new Response(stream).blob();
  }

  async function readJsonFile(file: File): Promise<string> {
    if (!isGzipFile(file)) return file.text();
    if (!compressionSupported) throw new Error('Decompressione gzip non supportata da questo browser.');

    const stream = file.stream().pipeThrough(new DecompressionStream('gzip'));
    return new Response(stream).text();
  }

  function isGzipFile(file: File): boolean {
    const name = file.name.toLocaleLowerCase();
    return name.endsWith('.gz') || file.type === 'application/gzip' || file.type === 'application/x-gzip';
  }

  async function downloadResultsGzip(): Promise<void> {
    if (!browser || !lastResult) return;

    try {
      await withBusy('Comprimo il JSON dei risultati...', async () => {
        const payload = currentResultExport();
        if (!payload) return;
        const blob = await gzipText(JSON.stringify(payload));
        downloadBlob(blob, resultFilename('json.gz'));
      });
    } catch (error) {
      messages = [uiMessage('RESULT_EXPORT_ERROR', error instanceof Error ? error.message : String(error), 'error')];
    }
  }

  async function downloadResultsCsv(): Promise<void> {
    if (!browser || !lastResult) return;

    const result = lastResult;

    try {
      await withBusy('Preparo il CSV dei risultati...', () =>
        downloadText(resultTablesToCsv(result.tables), 'text/csv;charset=utf-8', resultFilename('csv'))
      );
    } catch (error) {
      messages = [uiMessage('RESULT_EXPORT_ERROR', error instanceof Error ? error.message : String(error), 'error')];
    }
  }

  function uiMessage(code: string, message: string, severity: UiMessageSeverity = 'warning'): UiMessage {
    return {
      code,
      message,
      severity
    };
  }

  function workerMessage(warning: ScrutinyWarning): UiMessage {
    return uiMessage(warning.code, warning.message, warning.severity ?? 'warning');
  }

  function isRecord(value: unknown): value is Record<string, unknown> {
    return value !== null && typeof value === 'object' && !Array.isArray(value);
  }

  function looksLikeResultJson(text: string): boolean {
    try {
      const parsed = JSON.parse(text) as unknown;
      return isRecord(parsed) && ('result' in parsed || parsed.type === 'result');
    } catch {
      return false;
    }
  }

  function applyResultExport(payload: SimulationResultExport): void {
    const importedScenario = parseScenario(JSON.stringify({ scenario: payload.scenario }));

    scenarioDraft = importedScenario;
    tables = payload.result.tables;
    lastResult = payload.result;
    elapsedMs = payload.result.benchmark.elapsedMs;
    phase = payload.result.status;
    showDiagnostics = false;
    showAdvancedScenario = false;
    selectedPlurinominalOptionId = '';
    messages = [
      uiMessage('RESULT_IMPORT', `Risultati importati dal JSON esportato il ${payload.exportedAt}.`, 'info'),
      ...payload.result.warnings.map(workerMessage)
    ];
  }

  function chooseScenarioFile(): void {
    fileInput?.click();
  }

  function chooseResultFile(): void {
    resultFileInput?.click();
  }

  async function loadScenarioFile(event: Event): Promise<void> {
    const input = event.currentTarget as HTMLInputElement;
    const file = input.files?.[0];
    if (!file) return;

    try {
      await withBusy('Carico il file...', async () => {
        const text = await readJsonFile(file);

        if (isGzipFile(file)) {
          try {
            applyResultExport(parseSimulationResultExport(text));
            return;
          } catch (error) {
            if (looksLikeResultJson(text)) throw error;
          }
        } else if (looksLikeResultJson(text)) {
          throw new Error('I risultati devono essere caricati come file .json.gz.');
        }

        scenarioDraft = parseScenario(text);
        messages = [];
        clearDisplayedResults();
        showAdvancedScenario = false;
      });
    } catch (error) {
      messages = [uiMessage('SCENARIO_LOAD_ERROR', error instanceof Error ? error.message : String(error), 'error')];
    } finally {
      input.value = '';
    }
  }

  async function loadResultFile(event: Event): Promise<void> {
    const input = event.currentTarget as HTMLInputElement;
    const file = input.files?.[0];
    if (!file) return;

    try {
      if (!isGzipFile(file)) throw new Error('I risultati devono essere caricati come file .json.gz.');

      await withBusy('Carico i risultati...', async () =>
        applyResultExport(parseSimulationResultExport(await readJsonFile(file)))
      );
    } catch (error) {
      messages = [uiMessage('RESULT_LOAD_ERROR', error instanceof Error ? error.message : String(error), 'error')];
    } finally {
      input.value = '';
    }
  }

  function runSimulation(): void {
    if (validationMessages.length > 0) {
      messages = validationMessages.map((message) => uiMessage('SCENARIO_VALIDATION', message, 'warning'));
      return;
    }

    const worker = new SimulationWorker();
    const scenarioSnapshot = cloneScenario(scenarioDraft);
    const request: SimulationRequest = {
      kind: 'politiche',
      scenario: scenarioSnapshot,
      electionDate: scenarioSnapshot.electionDate,
      simulations,
      seed,
      dataVersion
    };

    running = true;
    phase = 'validate';
    elapsedMs = 0;
    tables = [];
    messages = [];
    lastResult = null;
    showDiagnostics = false;
    selectedPlurinominalOptionId = '';

    worker.onmessage = (event: MessageEvent<SimulationWorkerMessage>) => {
      const message = event.data;
      if (message.type === 'progress') {
        phase = message.phase;
        elapsedMs = message.elapsedMs;
        return;
      }

      lastResult = message;
      tables = message.tables;
      messages = message.warnings.map(workerMessage);
      elapsedMs = message.benchmark.elapsedMs;
      running = false;
      phase = message.status;
      worker.terminate();
    };

    worker.onerror = (error) => {
      messages = [uiMessage('WORKER_ERROR', error.message, 'error')];
      elapsedMs = performance.now();
      running = false;
      phase = 'error';
      lastResult = null;
      worker.terminate();
    };

    try {
      worker.postMessage(request);
    } catch (error) {
      messages = [uiMessage('WORKER_POST_ERROR', error instanceof Error ? error.message : String(error), 'error')];
      elapsedMs = 0;
      running = false;
      phase = 'error';
      lastResult = null;
      worker.terminate();
    }
  }

  function resultTablePriority(name: string): number {
    if (name === 'Election overview') return 0;
    if (name === 'Average plurinominal seats by list') return 1;
    if (name === 'Vote share by list') return 2;
    if (name === 'Uninominal winners by support') return 3;
    if (name === 'Scenario projection') return 4;
    return 10;
  }

  function selectPlurinominalOption(id: string): void {
    selectedPlurinominalOptionId = id;
  }
</script>

<svelte:head>
  <title>Elezioni</title>
</svelte:head>

<main class="workspace">
  <section class="toolbar" aria-label="Simulazione">
    <div>
      <h1>{scenarioDraft.name || 'Scenario politiche'}</h1>
      <p>Snapshot {dataVersion}</p>
    </div>
    <label>
      Simulazioni
      <input type="number" min="1" max="1000" step="1" bind:value={simulations} />
    </label>
    <label>
      Seed
      <input type="text" bind:value={seed} />
    </label>
    <button class="primary" type="button" onclick={runSimulation} disabled={!canRun}>
      <Play size={18} aria-hidden="true" />
      <span>{runButtonLabel}</span>
    </button>
  </section>

  {#if busyAction}
    <div class="busy-status" role="status" aria-live="polite">{busyAction}</div>
  {/if}

  <section class="stack">
    <div class="panel scenario-panel">
      <div class="panel-heading">
        <h2>Scenario</h2>
        <div class="panel-actions">
          <button
            type="button"
            class="icon-button"
            onclick={resetScenario}
            disabled={isBusy}
            title="Ripristina scenario"
            aria-label="Ripristina scenario"
          >
            <RotateCcw size={18} aria-hidden="true" />
          </button>
          <button
            type="button"
            class="icon-button"
            onclick={chooseScenarioFile}
            disabled={isBusy}
            title="Carica scenario"
            aria-label="Carica scenario"
          >
            <Upload size={18} aria-hidden="true" />
          </button>
          <button
            type="button"
            class="icon-button"
            onclick={downloadScenario}
            disabled={isBusy}
            title="Scarica scenario"
            aria-label="Scarica scenario"
          >
            <Download size={18} aria-hidden="true" />
          </button>
        </div>
      </div>

      <input
        class="hidden-file"
        type="file"
        accept="application/json,application/gzip,.json,.json.gz"
        bind:this={fileInput}
        onchange={loadScenarioFile}
        data-testid="scenario-file-input"
      />

      <div class="scenario-meta">
        <label>
          Nome scenario
          <input type="text" bind:value={scenarioDraft.name} />
        </label>
        <label>
          Data elezione
          <input type="date" bind:value={scenarioDraft.electionDate} />
        </label>
      </div>

      {#if validationMessages.length > 0}
        <div class="validation" data-testid="scenario-validation">
          {#each validationMessages as message}
            <p>{message}</p>
          {/each}
        </div>
      {/if}

      

      <div class="section-heading">
        <h3>Coalizioni</h3>
        <button
          type="button"
          class="icon-button"
          onclick={addCoalition}
          title="Aggiungi coalizione"
          aria-label="Aggiungi coalizione"
        >
          <Plus size={18} aria-hidden="true" />
        </button>
      </div>

      <div class="coalition-editor">
        {#each scenarioDraft.coalitions as coalition (coalition.id)}
          <div class="coalition-row">
            <input class="color" type="color" bind:value={coalition.color} aria-label="Colore coalizione" />
            <input
              type="text"
              value={coalition.name}
              oninput={(event) => updateCoalitionName(coalition.id, (event.currentTarget as HTMLInputElement).value)}
              aria-label="Nome coalizione"
            />
            <button
              type="button"
              class="icon-button danger"
              onclick={() => removeCoalition(coalition.id)}
              disabled={scenarioDraft.coalitions.length <= 1}
              title="Rimuovi coalizione"
              aria-label="Rimuovi coalizione"
            >
              <Trash2 size={18} aria-hidden="true" />
            </button>
          </div>
        {/each}
      </div>

      <div class="section-heading">
        <h3>Liste</h3>
        <button
          type="button"
          class="icon-button"
          onclick={addList}
          title="Aggiungi lista"
          aria-label="Aggiungi lista"
        >
          <Plus size={18} aria-hidden="true" />
        </button>
      </div>

      <div class="list-editor">
        {#each scenarioDraft.lists as list (list.id)}
          <div class="list-row">
            <input class="color" type="color" bind:value={list.color} aria-label="Colore lista" />
            <input
              type="text"
              value={list.name}
              oninput={(event) =>
                (scenarioDraft = renameScenarioList(scenarioDraft, list.id, (event.currentTarget as HTMLInputElement).value))}
              aria-label="Nome lista"
            />
            <select bind:value={list.coalition} aria-label="Coalizione">
              {#each scenarioDraft.coalitions as coalition}
                <option value={coalition.name}>{coalition.name}</option>
              {/each}
            </select>
            <input
              class="share"
              type="number"
              min="0"
              max="100"
              step="0.1"
              bind:value={list.startingShare}
              oninput={() => (list.shareOverride = true)}
              aria-label="Quota iniziale"
            />
            <label class="override-toggle">
              <input
                type="checkbox"
                bind:checked={list.shareOverride}
                aria-label={`Usa quota ${list.name || 'lista'}`}
              />
              <span>Usa</span>
            </label>
            <button
              type="button"
              class="icon-button danger"
              onclick={() => removeList(list.id)}
              title="Rimuovi lista"
              aria-label="Rimuovi lista"
            >
              <Trash2 size={18} aria-hidden="true" />
            </button>
          </div>
        {/each}
      </div>

      <div class="advanced">
        <button
          class="advanced-toggle"
          type="button"
          onclick={() => (showAdvancedScenario = !showAdvancedScenario)}
          aria-expanded={showAdvancedScenario}
          aria-controls="scenario-advanced"
        >
          {#if showAdvancedScenario}
            <ChevronUp size={18} aria-hidden="true" />
          {:else}
            <ChevronDown size={18} aria-hidden="true" />
          {/if}
          <span>Impostazioni avanzate</span>
        </button>

        {#if showAdvancedScenario}
          <div id="scenario-advanced" class="advanced-content">
            <div class="setting-row">
              <label>
                Astensione elettori
                <input
                  type="number"
                  min="0"
                  max="99.9"
                  step="0.1"
                  bind:value={scenarioDraft.abstentionShare}
                  oninput={() => (scenarioDraft.abstentionOverride = true)}
                  aria-label="Astensione elettori"
                />
              </label>
              <label class="override-toggle">
                <input
                  type="checkbox"
                  bind:checked={scenarioDraft.abstentionOverride}
                  aria-label="Usa astensione"
                />
                <span>Usa</span>
              </label>
            </div>

            <div class="candidate-generation-block">
              <span class="setting-label">Generazione candidati</span>
              <div class="range-setting">
                <label>
                  Uninominali in plurinominale
                  <input
                    type="range"
                    min="0"
                    max="1"
                    step="0.01"
                    value={scenarioDraft.candidateGeneration.uninominalToPlurinominalShare}
                    oninput={(event) =>
                      updateUninominalToPlurinominalShare((event.currentTarget as HTMLInputElement).value)}
                    aria-label="Quota uninominali in plurinominale"
                  />
                </label>
                <input
                  class="compact-number"
                  type="number"
                  min="0"
                  max="1"
                  step="0.01"
                  value={scenarioDraft.candidateGeneration.uninominalToPlurinominalShare}
                  oninput={(event) =>
                    updateUninominalToPlurinominalShare((event.currentTarget as HTMLInputElement).value)}
                  aria-label="Quota uninominali in plurinominale"
                />
              </div>

              <div>
                <span class="setting-label">Pluricandidature</span>
                <span class="setting-meta">Somma {plurinominalCandidacyShareTotalLabel}</span>
              </div>
              <div class="candidacy-share-grid">
                {#each candidacyCountOptions as count}
                  <label>
                    {count} {count === 1 ? 'candidatura' : 'candidature'}
                    <input
                      type="number"
                      min="0"
                      max="1"
                      step="0.001"
                      value={count === 1
                        ? calculatedSingleCandidacyShare
                        : scenarioDraft.candidateGeneration.plurinominalCandidacyCountShares[count - 1]}
                      oninput={(event) =>
                        updatePlurinominalCandidacyCountShare(
                          count,
                          (event.currentTarget as HTMLInputElement).value
                        )}
                      disabled={count === 1}
                      aria-label={`Quota candidati con ${count} candidature`}
                    />
                  </label>
                {/each}
              </div>
            </div>

            <div class="candidate-template-block">
              <div class="candidate-template-heading">
                <div>
                  <span class="setting-label">Candidati</span>
                  <span class="setting-meta">{candidateTemplateSummary}</span>
                </div>
              </div>

              <div class="candidate-template-picker">
                <div class="candidate-template-meta">
                  <label>
                    Ramo
                    <select
                      value={candidateTemplateRamo}
                      onchange={(event) => {
                        candidateTemplateRamo = (event.currentTarget as HTMLSelectElement)
                          .value as ScenarioCandidateTemplateRamo;
                        resetCandidateTemplateSlotSelection();
                      }}
                      aria-label="Ramo candidato"
                    >
                      {#each candidateTemplateRamoOptions as ramo}
                        <option value={ramo}>{candidateTemplateRamoLabel(ramo)}</option>
                      {/each}
                    </select>
                  </label>
                  <label>
                    Tipo
                    <select
                      value={candidateTemplateKind}
                      onchange={(event) => {
                        candidateTemplateKind = (event.currentTarget as HTMLSelectElement)
                          .value as ScenarioCandidateTemplateKind;
                        resetCandidateTemplateSlotSelection();
                      }}
                      aria-label="Tipo candidato"
                    >
                      {#each candidateTemplateKindOptions as kind}
                        <option value={kind}>{candidateTemplateKindLabel(kind)}</option>
                      {/each}
                    </select>
                  </label>
                </div>

                <div class="candidate-template-target">
                  {#if candidateTemplateKind === 'uninominal'}
                    <label>
                      Coalizione
                      <select
                        value={candidateTemplateCoalitionSelection}
                        onchange={(event) => (candidateTemplateCoalition = (event.currentTarget as HTMLSelectElement).value)}
                        aria-label="Coalizione candidato"
                      >
                        {#each scenarioDraft.coalitions as coalition}
                          <option value={coalition.name}>{coalition.name}</option>
                        {/each}
                      </select>
                    </label>
                  {:else}
                    <label>
                      Lista
                      <select
                        value={candidateTemplateListSelection}
                        onchange={(event) => (candidateTemplateList = (event.currentTarget as HTMLSelectElement).value)}
                        aria-label="Lista candidato"
                      >
                        {#each scenarioDraft.lists as list}
                          <option value={list.name}>{list.name}</option>
                        {/each}
                      </select>
                    </label>
                  {/if}

                  <div class="candidate-slot-picker">
                    <label>
                      Collegio
                      <input
                        type="search"
                        value={candidateTemplateSlotSearch}
                        oninput={(event) => {
                          candidateTemplateSlotSearch = (event.currentTarget as HTMLInputElement).value;
                          selectedCandidateTemplateSlotCode = '';
                        }}
                        autocomplete="off"
                        aria-label="Cerca collegio candidato"
                      />
                    </label>
                    {#if selectedCandidateTemplateSlot}
                      <span class="selected-location">{candidateSlotSublabel(selectedCandidateTemplateSlot)}</span>
                    {/if}
                    {#if candidateTemplateSearchResults.length > 0 && !selectedCandidateTemplateSlotCode}
                      <div class="slot-results" role="listbox" aria-label="Collegi candidati trovati">
                        {#each candidateTemplateSearchResults as slot (`${candidateTemplateRamo}\u001f${candidateTemplateKind}\u001f${candidateSlotCode(slot)}`)}
                          <button type="button" onclick={() => selectCandidateTemplateSlot(slot)}>
                            <span>{candidateSlotLabel(slot)}</span>
                            <span>{candidateSlotSublabel(slot)}</span>
                          </button>
                        {/each}
                      </div>
                    {/if}
                  </div>

                  {#if candidateTemplateKind === 'plurinominal'}
                    <label>
                      Numero
                      <select
                        value={candidateTemplateNumber}
                        onchange={(event) => (candidateTemplateNumber = Number((event.currentTarget as HTMLSelectElement).value))}
                        aria-label="Numero candidato"
                      >
                        {#each candidateTemplateNumberOptions as candidateNumber}
                          <option value={candidateNumber}>{candidateNumber}</option>
                        {/each}
                      </select>
                    </label>
                    <label class="override-toggle">
                      <input
                        type="checkbox"
                        bind:checked={candidateTemplateMinority}
                        aria-label="Candidato minoranza"
                      />
                      <span>Min.</span>
                    </label>
                  {/if}
                </div>

                <div class="candidate-template-person">
                  <label>
                    Nome
                    <input
                      type="text"
                      value={candidateTemplateName}
                      oninput={(event) => (candidateTemplateName = (event.currentTarget as HTMLInputElement).value)}
                      aria-label="Nome candidato"
                    />
                  </label>
                  <label>
                    Nascita
                    <input
                      type="date"
                      value={candidateTemplateBirthDate}
                      oninput={(event) => (candidateTemplateBirthDate = (event.currentTarget as HTMLInputElement).value)}
                      aria-label="Data nascita candidato"
                    />
                  </label>
                </div>

                <button
                  type="button"
                  class="text-button"
                  onclick={addCandidateTemplate}
                  disabled={!candidateTemplateCanAdd}
                  aria-label="Aggiungi candidato"
                >
                  <Plus size={16} aria-hidden="true" />
                  <span>Aggiungi</span>
                </button>
              </div>

              {#if candidateTemplateGroups.length === 0}
                <p class="advanced-note">Nessun candidato</p>
              {:else}
                <div class="candidate-template-groups">
                  {#each candidateTemplateGroups as group (group.key)}
                    <details class="candidate-template-group" open>
                      <summary>
                        <span>{group.label}</span>
                        <span>{group.templates.length} candidati</span>
                      </summary>

                      <div class="candidate-template-rows">
                        {#each group.templates as template (template.id)}
                          <div class="candidate-template-row">
                            <div class="candidate-template-meta">
                              <label>
                                Ramo
                                <select
                                  value={template.ramo}
                                  onchange={(event) =>
                                    updateCandidateTemplate(template.id, {
                                      ramo: (event.currentTarget as HTMLSelectElement)
                                        .value as ScenarioCandidateTemplateRamo
                                    })}
                                  aria-label={`Ramo ${template.candidateName || 'candidato'}`}
                                >
                                  {#each candidateTemplateRamoOptions as ramo}
                                    <option value={ramo}>{candidateTemplateRamoLabel(ramo)}</option>
                                  {/each}
                                </select>
                              </label>
                              <label>
                                Tipo
                                <select
                                  value={template.kind}
                                  onchange={(event) =>
                                    updateCandidateTemplate(template.id, {
                                      kind: (event.currentTarget as HTMLSelectElement)
                                        .value as ScenarioCandidateTemplateKind
                                    })}
                                  aria-label={`Tipo ${template.candidateName || 'candidato'}`}
                                >
                                  {#each candidateTemplateKindOptions as kind}
                                    <option value={kind}>{candidateTemplateKindLabel(kind)}</option>
                                  {/each}
                                </select>
                              </label>
                            </div>

                            <div class="candidate-template-target">
                              {#if template.kind === 'uninominal'}
                                <label>
                                  Coalizione
                                  <select
                                    value={template.coalition ?? ''}
                                    onchange={(event) =>
                                      updateCandidateTemplate(template.id, {
                                        coalition: (event.currentTarget as HTMLSelectElement).value
                                      })}
                                    aria-label={`Coalizione ${template.candidateName || 'candidato'}`}
                                  >
                                    {#each scenarioDraft.coalitions as coalition}
                                      <option value={coalition.name}>{coalition.name}</option>
                                    {/each}
                                    {#if template.coalition && !scenarioDraft.coalitions.some((coalition) => coalition.name === template.coalition)}
                                      <option value={template.coalition}>{template.coalition}</option>
                                    {/if}
                                  </select>
                                </label>
                                <label>
                                  Collegio
                                  <select
                                    value={template.uninominalCode ?? ''}
                                    onchange={(event) =>
                                      updateCandidateTemplate(template.id, {
                                        uninominalCode: (event.currentTarget as HTMLSelectElement).value
                                      })}
                                    aria-label={`Collegio ${template.candidateName || 'candidato'}`}
                                  >
                                    {#each candidateTemplateSlots(template.ramo, 'uninominal') as slot (candidateSlotCode(slot))}
                                      <option value={candidateSlotCode(slot)}>{candidateSlotLabel(slot)}</option>
                                    {/each}
                                    {#if template.uninominalCode && !findCandidateTemplateSlot(template.ramo, 'uninominal', template.uninominalCode)}
                                      <option value={template.uninominalCode}>UNI {template.uninominalCode}</option>
                                    {/if}
                                  </select>
                                </label>
                              {:else}
                                <label>
                                  Lista
                                  <select
                                    value={template.list ?? ''}
                                    onchange={(event) =>
                                      updateCandidateTemplate(template.id, {
                                        list: (event.currentTarget as HTMLSelectElement).value
                                      })}
                                    aria-label={`Lista ${template.candidateName || 'candidato'}`}
                                  >
                                    {#each scenarioDraft.lists as list}
                                      <option value={list.name}>{list.name}</option>
                                    {/each}
                                    {#if template.list && !scenarioDraft.lists.some((list) => list.name === template.list)}
                                      <option value={template.list}>{template.list}</option>
                                    {/if}
                                  </select>
                                </label>
                                <label>
                                  Collegio
                                  <select
                                    value={template.plurinominalCode ?? ''}
                                    onchange={(event) =>
                                      updateCandidateTemplate(template.id, {
                                        plurinominalCode: (event.currentTarget as HTMLSelectElement).value
                                      })}
                                    aria-label={`Collegio ${template.candidateName || 'candidato'}`}
                                  >
                                    {#each candidateTemplateSlots(template.ramo, 'plurinominal') as slot (candidateSlotCode(slot))}
                                      <option value={candidateSlotCode(slot)}>{candidateSlotLabel(slot)}</option>
                                    {/each}
                                    {#if template.plurinominalCode && !findCandidateTemplateSlot(template.ramo, 'plurinominal', template.plurinominalCode)}
                                      <option value={template.plurinominalCode}>PLURI {template.plurinominalCode}</option>
                                    {/if}
                                  </select>
                                </label>
                                <label>
                                  Numero
                                  <select
                                    value={template.candidateNumber ?? 1}
                                    onchange={(event) =>
                                      updateCandidateTemplate(template.id, {
                                        candidateNumber: Number((event.currentTarget as HTMLSelectElement).value)
                                      })}
                                    aria-label={`Numero ${template.candidateName || 'candidato'}`}
                                  >
                                    {#each plurinominalCandidateNumbersForTemplate(template) as candidateNumber}
                                      <option value={candidateNumber}>{candidateNumber}</option>
                                    {/each}
                                  </select>
                                </label>
                                <label class="override-toggle">
                                  <input
                                    type="checkbox"
                                    checked={template.minority === true}
                                    onchange={(event) =>
                                      updateCandidateTemplate(template.id, {
                                        minority: (event.currentTarget as HTMLInputElement).checked
                                      })}
                                    aria-label={`Minoranza ${template.candidateName || 'candidato'}`}
                                  />
                                  <span>Min.</span>
                                </label>
                              {/if}
                            </div>

                            <div class="candidate-template-person">
                              <label>
                                Nome
                                <input
                                  type="text"
                                  value={template.candidateName}
                                  oninput={(event) =>
                                    updateCandidateTemplate(template.id, {
                                      candidateName: (event.currentTarget as HTMLInputElement).value
                                    })}
                                  aria-label={`Nome ${template.candidateName || 'candidato'}`}
                                />
                              </label>
                              <label>
                                Nascita
                                <input
                                  type="date"
                                  value={template.birthDate?.slice(0, 10) ?? ''}
                                  oninput={(event) =>
                                    updateCandidateTemplate(template.id, {
                                      birthDate: (event.currentTarget as HTMLInputElement).value || null
                                    })}
                                  aria-label={`Nascita ${template.candidateName || 'candidato'}`}
                                />
                              </label>
                            </div>

                            <button
                              type="button"
                              class="icon-button danger"
                              onclick={() => removeCandidateTemplate(template.id)}
                              title="Rimuovi candidato"
                              aria-label={`Rimuovi candidato ${template.candidateName || 'candidato'}`}
                            >
                              <Trash2 size={18} aria-hidden="true" />
                            </button>
                          </div>
                        {/each}
                      </div>
                    </details>
                  {/each}
                </div>
              {/if}
            </div>

            <div class="local-overrides-block">
              <div class="local-overrides-heading">
                <div>
                  <span class="setting-label">Quote locali</span>
                  <span class="setting-meta">{localOverrideSummary}</span>
                </div>
              </div>

              <div class="local-override-picker">
                <label>
                  Ambito
                  <select
                    bind:value={selectedLocalOverrideScope}
                    onchange={() => {
                      localOverrideSearch = '';
                      selectedLocalOverrideLocationCode = '';
                    }}
                    aria-label="Ambito quota locale"
                  >
                    <option value="municipality">Comune</option>
                    <option value="province">Provincia</option>
                    <option value="region">Regione</option>
                  </select>
                </label>

                <div class="municipality-picker">
                  <label>
                    {localOverrideScopeLabel(selectedLocalOverrideScope)}
                    <input
                      type="search"
                      value={localOverrideSearch}
                      oninput={(event) => {
                        localOverrideSearch = (event.currentTarget as HTMLInputElement).value;
                        selectedLocalOverrideLocationCode = '';
                      }}
                      autocomplete="off"
                      aria-label="Cerca localita"
                    />
                  </label>
                  {#if selectedLocalOverrideLocation}
                    <span class="selected-location">{selectedLocalOverrideLocation.sublabel}</span>
                  {/if}
                  {#if localOverrideSearchResults.length > 0 && !selectedLocalOverrideLocationCode}
                    <div class="municipality-results" role="listbox" aria-label="Localita trovate">
                      {#each localOverrideSearchResults as option (localOverrideLocationOptionKey(option.scope, option.code))}
                        <button
                          type="button"
                          class:active={option.code === selectedLocalOverrideLocationCode}
                          onclick={() => selectLocalOverrideLocation(option)}
                        >
                          <span>{option.label}</span>
                          <span>{option.sublabel}</span>
                        </button>
                      {/each}
                    </div>
                  {/if}
                </div>

                <label>
                  Lista
                  <select
                    value={localOverrideListSelection}
                    onchange={(event) => (selectedLocalOverrideList = (event.currentTarget as HTMLSelectElement).value)}
                    aria-label="Lista quota locale"
                  >
                    {#each scenarioDraft.lists as list}
                      <option value={list.name}>{list.name}</option>
                    {/each}
                  </select>
                </label>

                <label>
                  Quota
                  <input
                    type="number"
                    min="0"
                    max="100"
                    step="0.1"
                    value={localOverrideShare}
                    oninput={(event) => (localOverrideShare = Number((event.currentTarget as HTMLInputElement).value))}
                    aria-label="Quota locale da aggiungere"
                  />
                </label>

                <button
                  type="button"
                  class="text-button"
                  onclick={addSelectedLocalOverride}
                  disabled={!resolveLocalOverrideLocationCode() || scenarioDraft.lists.length === 0}
                  aria-label="Aggiungi quota locale"
                >
                  <Plus size={16} aria-hidden="true" />
                  <span>Aggiungi</span>
                </button>
              </div>

              {#if localOverrideGroups.length === 0}
                <p class="advanced-note">Nessuna quota locale</p>
              {:else}
                <div class="local-override-groups">
                  {#each localOverrideGroups as group (group.key)}
                    <details class="local-override-group" open>
                      <summary>
                        <span>
                          {localOverrideLocationDisplay(group.scope, group.locationCode)}
                          {#if localOverrideLocationSubLabel(group.scope, group.locationCode)}
                            <small>{localOverrideLocationSubLabel(group.scope, group.locationCode)}</small>
                          {/if}
                        </span>
                        <span>{group.overrides.length} liste / {group.totalShare.toFixed(1)}%</span>
                      </summary>

                      <div class="local-override-rows">
                        {#each group.overrides as override (override.id)}
                          <div class="local-override-row">
                            <label>
                              Lista
                              <select
                                value={override.list}
                                onchange={(event) =>
                                  updateLocalOverride(override.id, {
                                    list: (event.currentTarget as HTMLSelectElement).value
                                  })}
                                aria-label={`Lista quota locale ${localOverrideLocationDisplay(group.scope, group.locationCode)}`}
                              >
                                {#each scenarioDraft.lists as list}
                                  <option value={list.name}>{list.name}</option>
                                {/each}
                                {#if !scenarioDraft.lists.some((list) => list.name === override.list)}
                                  <option value={override.list}>{override.list}</option>
                                {/if}
                              </select>
                            </label>
                            <label>
                              Quota
                              <input
                                type="number"
                                min="0"
                                max="100"
                                step="0.1"
                                value={override.startingShare}
                                oninput={(event) =>
                                  updateLocalOverride(override.id, {
                                    startingShare: Number((event.currentTarget as HTMLInputElement).value)
                                  })}
                                aria-label={`Quota locale ${override.list}`}
                              />
                            </label>
                            <button
                              type="button"
                              class="icon-button danger"
                              onclick={() => removeLocalOverride(override.id)}
                              title="Rimuovi quota locale"
                              aria-label={`Rimuovi quota locale ${override.list}`}
                            >
                              <Trash2 size={18} aria-hidden="true" />
                            </button>
                          </div>
                        {/each}
                      </div>

                      <div class="location-actions">
                        <button
                          type="button"
                          class="icon-button"
                          onclick={() => addLocalOverrideForLocation(group.scope, group.locationCode)}
                          title="Aggiungi lista locale"
                          aria-label={`Aggiungi lista locale ${localOverrideLocationDisplay(group.scope, group.locationCode)}`}
                          disabled={group.overrides.length >= scenarioDraft.lists.length}
                        >
                          <Plus size={18} aria-hidden="true" />
                        </button>
                        <button
                          type="button"
                          class="icon-button danger"
                          onclick={() => removeLocalOverridesForLocation(group.scope, group.locationCode)}
                          title="Rimuovi localita"
                          aria-label={`Rimuovi localita ${localOverrideLocationDisplay(group.scope, group.locationCode)}`}
                        >
                          <Trash2 size={18} aria-hidden="true" />
                        </button>
                      </div>
                    </details>
                  {/each}
                </div>
              {/if}
            </div>

            <div class="correspondence-block">
              <div class="correspondence-heading">
                <div>
                  <span class="setting-label">Corrispondenze storiche</span>
                  <span class="setting-meta">{bundledListCorrespondenceCount} bundled / {manualListCorrespondenceCount} manuali</span>
                </div>
              </div>

              {#if historicalCorrespondenceGroups.length === 0}
                <p class="advanced-note">Nessuna corrispondenza storica</p>
              {:else}
                <div class="correspondence-editor">
                  {#each historicalCorrespondenceGroups as group (group.key)}
                    <details class="correspondence-election" open={group.pastElection === 'europee 2024'}>
                      <summary>
                        <span>{group.pastElection}</span>
                        <span>{group.sources.length} liste</span>
                      </summary>
                      <div class="correspondence-sources">
                        {#each group.sources as source (source.key)}
                          <div class="correspondence-source">
                            <div class="past-list-cell">
                              <span>{source.pastList}</span>
                              {#if source.customized}
                                <span class="source-status">manuale</span>
                              {:else}
                                <span class="source-status">default</span>
                              {/if}
                            </div>

                            <div class="correspondence-mappings">
                              {#if source.correspondences.length === 0}
                                <p class="advanced-note">Nessuna destinazione</p>
                              {:else}
                                {#each source.correspondences as correspondence (correspondence.id)}
                                  <div class="correspondence-row">
                                    <label>
                                      Destinazione
                                      <select
                                        value={correspondence.futureList}
                                        onchange={(event) =>
                                          updateListCorrespondence(correspondence.id, {
                                            futureList: (event.currentTarget as HTMLSelectElement).value
                                          })}
                                        aria-label="Destinazione corrispondenza"
                                      >
                                        {#each correspondenceDestinations as destination}
                                          <option value={destination}>{destination}</option>
                                        {/each}
                                        {#if !correspondenceDestinations.includes(correspondence.futureList)}
                                          <option value={correspondence.futureList}>{correspondence.futureList}</option>
                                        {/if}
                                      </select>
                                    </label>
                                    <label>
                                      Fattore
                                      <input
                                        type="number"
                                        min="0.001"
                                        step="0.001"
                                        value={correspondence.factor}
                                        oninput={(event) =>
                                          updateListCorrespondence(correspondence.id, {
                                            factor: Number((event.currentTarget as HTMLInputElement).value)
                                          })}
                                        aria-label="Fattore corrispondenza"
                                      />
                                    </label>
                                    <button
                                      type="button"
                                      class="icon-button danger"
                                      onclick={() => removeListCorrespondence(correspondence.id)}
                                      title="Rimuovi corrispondenza"
                                      aria-label="Rimuovi corrispondenza"
                                    >
                                      <Trash2 size={18} aria-hidden="true" />
                                    </button>
                                  </div>
                                {/each}
                              {/if}
                            </div>

                            <div class="source-actions">
                              <button
                                type="button"
                                class="icon-button"
                                onclick={() => addListCorrespondenceForSource(source.pastElection, source.pastDate, source.pastList)}
                                title="Aggiungi destinazione"
                                aria-label="Aggiungi destinazione"
                                disabled={scenarioDraft.lists.length === 0}
                              >
                                <Plus size={18} aria-hidden="true" />
                              </button>
                              <button
                                type="button"
                                class="icon-button"
                                onclick={() => resetListCorrespondenceSource(source.pastElection, source.pastList)}
                                title="Ripristina default"
                                aria-label="Ripristina default"
                                disabled={source.defaultCorrespondences.length === 0}
                              >
                                <RotateCcw size={18} aria-hidden="true" />
                              </button>
                            </div>
                          </div>
                        {/each}
                      </div>
                    </details>
                  {/each}
                </div>
              {/if}
            </div>
          </div>
        {/if}
      </div>
    </div>

    <div class="panel result-panel">
      <div class="panel-heading">
        <h2>Risultati</h2>
        <div class="result-heading-actions">
          <button
            type="button"
            class="text-button"
            onclick={chooseResultFile}
            disabled={isBusy}
            aria-label="Carica risultati compressi"
          >
            <Upload size={16} aria-hidden="true" />
            <span>Carica JSON.gz</span>
          </button>
          {#if hasResult}
            <button
              type="button"
              class="text-button"
              onclick={downloadResultsGzip}
              disabled={isBusy}
              aria-label="Scarica risultati compressi"
            >
              <Download size={16} aria-hidden="true" />
              <span>JSON.gz</span>
            </button>
            <button
              type="button"
              class="text-button"
              onclick={downloadResultsCsv}
              disabled={isBusy}
              aria-label="Scarica risultati CSV"
            >
              <Download size={16} aria-hidden="true" />
              <span>CSV</span>
            </button>
          {/if}
          <span data-testid="elapsed-ms" data-phase={phase}>{elapsedLabel}</span>
        </div>
      </div>

      <input
        class="hidden-file"
        type="file"
        accept="application/gzip,.json.gz"
        bind:this={resultFileInput}
        onchange={loadResultFile}
        data-testid="result-file-input"
      />

      {#if infoMessages.length > 0}
        <div class="messages info-messages" aria-label="Note simulazione">
          {#each infoMessages as message}
            <p><strong>{message.code}</strong>: {message.message}</p>
          {/each}
        </div>
      {/if}

      {#if warningMessages.length > 0}
        <div class="messages warning-messages" aria-label="Avvisi simulazione">
          {#each warningMessages as message}
            <p><strong>{message.code}</strong>: {message.message}</p>
          {/each}
        </div>
      {/if}

      {#if resultCharts.length > 0 || plurinominalChart}
        <PoliticsResultCharts
          charts={resultCharts}
          plurinominalOptions={plurinominalOptions}
          selectedPlurinominalOption={selectedPlurinominalOption}
          {plurinominalChart}
          onSelectPlurinominalOption={selectPlurinominalOption}
        />
      {/if}

      {#each primaryTables as table}
        <table>
          <caption>{table.name}</caption>
          <thead>
            <tr>
              {#each table.columns as column}
                <th>{column}</th>
              {/each}
            </tr>
          </thead>
          <tbody>
            {#each table.rows as row}
              <tr>
                {#each table.columns as column}
                  <td>{row[column]}</td>
                {/each}
              </tr>
            {/each}
          </tbody>
        </table>
      {/each}

      {#if diagnosticTables.length > 0}
        <div class="diagnostics">
          <button
            class="diagnostic-toggle"
            type="button"
            onclick={() => (showDiagnostics = !showDiagnostics)}
            aria-expanded={showDiagnostics}
            aria-controls="diagnostic-tables"
          >
            {#if showDiagnostics}
              <ChevronUp size={18} aria-hidden="true" />
            {:else}
              <ChevronDown size={18} aria-hidden="true" />
            {/if}
            <span>{diagnosticsToggleLabel}</span>
          </button>

          {#if showDiagnostics}
            <div id="diagnostic-tables">
              {#each diagnosticTables as table}
                <table>
                  <caption>{table.name}</caption>
                  <thead>
                    <tr>
                      {#each table.columns as column}
                        <th>{column}</th>
                      {/each}
                    </tr>
                  </thead>
                  <tbody>
                    {#each table.rows as row}
                      <tr>
                        {#each table.columns as column}
                          <td>{row[column]}</td>
                        {/each}
                      </tr>
                    {/each}
                  </tbody>
                </table>
              {/each}
            </div>
          {/if}
        </div>
      {/if}
    </div>
  </section>
</main>

<style>
  :global(body) {
    margin: 0;
    background: #f4f6f7;
    color: #182026;
    font-family:
      Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  }

  button,
  input,
  select {
    font: inherit;
  }

  .workspace {
    min-height: 100vh;
    padding: 24px;
  }

  .toolbar {
    display: grid;
    grid-template-columns: minmax(220px, 1fr) 160px 220px auto;
    gap: 16px;
    align-items: end;
    margin: 0 auto 20px;
    max-width: 1280px;
  }

  h1,
  h2,
  p {
    margin: 0;
  }

  h1 {
    font-size: 28px;
    font-weight: 750;
  }

  h2 {
    font-size: 16px;
    font-weight: 700;
  }

  h3 {
    margin: 0;
    color: #4d5963;
    font-size: 13px;
    font-weight: 750;
  }

  .toolbar p,
  .panel-heading span {
    color: #697681;
    font-size: 13px;
  }

  label {
    display: grid;
    gap: 6px;
    color: #4d5963;
    font-size: 13px;
    font-weight: 650;
  }

  input,
  select {
    min-height: 38px;
    border: 1px solid #c9d0d6;
    border-radius: 6px;
    background: #ffffff;
    color: #182026;
    padding: 0 10px;
  }

  .stack {
    display: grid;
    gap: 20px;
    max-width: 1280px;
    margin: 0 auto;
  }

  .busy-status {
    max-width: 1280px;
    margin: 0 auto 20px;
    border-left: 4px solid #2f6f57;
    background: #eef7f3;
    padding: 10px 12px;
    color: #244f40;
    font-size: 13px;
    font-weight: 700;
  }

  .panel {
    border: 1px solid #d8dee3;
    border-radius: 8px;
    background: #ffffff;
    min-width: 0;
  }

  .panel-heading {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    min-height: 52px;
    border-bottom: 1px solid #e5e9ed;
    padding: 0 16px;
  }

  .panel-actions {
    display: flex;
    gap: 8px;
  }

  .result-heading-actions {
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .hidden-file {
    display: none;
  }

  .scenario-meta {
    display: grid;
    grid-template-columns: minmax(160px, 1fr) 160px;
    gap: 12px;
    padding: 16px 16px 0;
  }

  .validation {
    margin: 16px 16px 0;
    border-left: 4px solid #9d2d2d;
    background: #fff1f1;
    padding: 10px 12px;
    color: #7c2020;
    font-size: 13px;
  }

  .validation p + p {
    margin-top: 4px;
  }

  .advanced {
    padding: 16px 16px 0;
  }

  .advanced-toggle {
    width: 100%;
    justify-content: flex-start;
    background: #f7f9fa;
    color: #4d5963;
    font-weight: 700;
  }

  .advanced-toggle span {
    flex: 1;
    text-align: left;
  }

  .advanced-content {
    display: grid;
    gap: 12px;
    border-bottom: 1px solid #e5e9ed;
    padding: 12px 0 16px;
  }

  .setting-row {
    display: grid;
    grid-template-columns: minmax(140px, 1fr) auto;
    gap: 12px;
    align-items: center;
  }

  .candidate-generation-block {
    display: grid;
    gap: 10px;
    border-top: 1px solid #e5e9ed;
    padding-top: 12px;
  }

  .candidate-template-block {
    display: grid;
    gap: 10px;
    border-top: 1px solid #e5e9ed;
    padding-top: 12px;
  }

  .candidate-template-heading {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
  }

  .candidate-template-picker,
  .candidate-template-row {
    display: grid;
    grid-template-columns: minmax(180px, 0.8fr) minmax(280px, 1.35fr) minmax(240px, 1.1fr) auto;
    gap: 8px;
    align-items: end;
  }

  .candidate-template-meta,
  .candidate-template-target,
  .candidate-template-person {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 8px;
    align-items: end;
  }

  .candidate-template-target {
    grid-template-columns: minmax(130px, 0.9fr) minmax(170px, 1.1fr) 74px 62px;
  }

  .candidate-template-person {
    grid-template-columns: minmax(150px, 1fr) 128px;
  }

  .candidate-slot-picker {
    position: relative;
    display: grid;
    gap: 4px;
  }

  .slot-results {
    position: absolute;
    z-index: 5;
    top: calc(100% + 4px);
    right: 0;
    left: 0;
    display: grid;
    max-height: 280px;
    overflow: auto;
    border: 1px solid #bdc7d0;
    border-radius: 6px;
    background: #ffffff;
    box-shadow: 0 10px 24px rgb(24 32 38 / 14%);
  }

  .slot-results button {
    display: grid;
    justify-content: stretch;
    min-height: 48px;
    border: 0;
    border-bottom: 1px solid #e5e9ed;
    border-radius: 0;
    padding: 7px 10px;
    text-align: left;
  }

  .slot-results button:last-child {
    border-bottom: 0;
  }

  .slot-results button:hover {
    background: #eef7f3;
  }

  .slot-results span:first-child {
    color: #182026;
    font-size: 13px;
    font-weight: 750;
  }

  .slot-results span:last-child {
    color: #697681;
    font-size: 12px;
    font-weight: 650;
  }

  .candidate-template-groups {
    display: grid;
    gap: 10px;
  }

  .candidate-template-group {
    border-top: 1px solid #e5e9ed;
    padding-top: 8px;
  }

  .candidate-template-group summary {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    min-height: 34px;
    color: #4d5963;
    cursor: pointer;
    font-size: 13px;
    font-weight: 750;
  }

  .candidate-template-group summary span:last-child {
    color: #697681;
    font-size: 12px;
    font-weight: 650;
  }

  .candidate-template-rows {
    display: grid;
    gap: 8px;
    padding-top: 8px;
  }

  .local-overrides-block {
    display: grid;
    gap: 10px;
    border-top: 1px solid #e5e9ed;
    padding-top: 12px;
  }

  .local-overrides-heading {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
  }

  .local-override-picker {
    display: grid;
    grid-template-columns: 130px minmax(220px, 1.3fr) minmax(150px, 0.9fr) 88px auto;
    gap: 8px;
    align-items: end;
  }

  .municipality-picker {
    position: relative;
    display: grid;
    gap: 4px;
  }

  .selected-location {
    color: #697681;
    font-size: 12px;
    font-weight: 650;
  }

  .municipality-results {
    position: absolute;
    z-index: 5;
    top: calc(100% + 4px);
    right: 0;
    left: 0;
    display: grid;
    max-height: 280px;
    overflow: auto;
    border: 1px solid #bdc7d0;
    border-radius: 6px;
    background: #ffffff;
    box-shadow: 0 10px 24px rgb(24 32 38 / 14%);
  }

  .municipality-results button {
    display: grid;
    justify-content: stretch;
    min-height: 48px;
    border: 0;
    border-bottom: 1px solid #e5e9ed;
    border-radius: 0;
    padding: 7px 10px;
    text-align: left;
  }

  .municipality-results button:last-child {
    border-bottom: 0;
  }

  .municipality-results button.active,
  .municipality-results button:hover {
    background: #eef7f3;
  }

  .municipality-results span:first-child {
    color: #182026;
    font-size: 13px;
    font-weight: 750;
  }

  .municipality-results span:last-child {
    color: #697681;
    font-size: 12px;
    font-weight: 650;
  }

  .local-override-groups {
    display: grid;
    gap: 10px;
  }

  .local-override-group {
    border-top: 1px solid #e5e9ed;
    padding-top: 8px;
  }

  .local-override-group summary {
    display: grid;
    grid-template-columns: minmax(180px, 1fr) auto;
    gap: 12px;
    min-height: 34px;
    color: #4d5963;
    cursor: pointer;
    font-size: 13px;
    font-weight: 750;
  }

  .local-override-group summary small,
  .local-override-group summary span:last-child {
    color: #697681;
    font-size: 12px;
    font-weight: 650;
  }

  .local-override-group summary small {
    display: block;
    margin-top: 2px;
  }

  .local-override-rows {
    display: grid;
    gap: 8px;
    padding-top: 8px;
  }

  .local-override-row {
    display: grid;
    grid-template-columns: minmax(180px, 1fr) 92px 40px;
    gap: 8px;
    align-items: end;
  }

  .location-actions {
    display: flex;
    justify-content: flex-end;
    gap: 6px;
    padding-top: 8px;
  }

  .range-setting {
    display: grid;
    grid-template-columns: minmax(180px, 1fr) 96px;
    gap: 10px;
    align-items: end;
  }

  .range-setting input[type='range'] {
    min-height: 38px;
    padding: 0;
  }

  .compact-number,
  .candidacy-share-grid input {
    text-align: right;
  }

  .candidacy-share-grid {
    display: grid;
    grid-template-columns: repeat(5, minmax(0, 1fr));
    gap: 8px;
  }

  .setting-label {
    color: #4d5963;
    font-size: 13px;
    font-weight: 700;
  }

  .setting-meta {
    display: block;
    margin-top: 3px;
    color: #697681;
    font-size: 12px;
    font-weight: 650;
  }

  .correspondence-block {
    display: grid;
    gap: 10px;
  }

  .correspondence-heading {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
  }

  .advanced-note {
    color: #697681;
    font-size: 13px;
  }

  .correspondence-editor {
    display: grid;
    gap: 10px;
  }

  .correspondence-election {
    border-top: 1px solid #e5e9ed;
    padding-top: 8px;
  }

  .correspondence-election:first-child {
    border-top: 0;
    padding-top: 0;
  }

  .correspondence-election summary {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    min-height: 34px;
    color: #4d5963;
    cursor: pointer;
    font-size: 13px;
    font-weight: 750;
  }

  .correspondence-election summary span:last-child {
    color: #697681;
    font-size: 12px;
    font-weight: 650;
  }

  .correspondence-sources {
    display: grid;
    gap: 8px;
    padding-top: 8px;
  }

  .correspondence-source {
    display: grid;
    grid-template-columns: minmax(180px, 0.8fr) minmax(280px, 1.4fr) 84px;
    gap: 8px;
    align-items: start;
  }

  .past-list-cell {
    display: grid;
    gap: 4px;
    min-height: 38px;
    align-content: center;
    color: #182026;
    font-size: 13px;
    font-weight: 650;
  }

  .source-status {
    color: #697681;
    font-size: 11px;
    font-weight: 700;
    text-transform: uppercase;
  }

  .correspondence-mappings {
    display: grid;
    gap: 8px;
  }

  .correspondence-row {
    display: grid;
    grid-template-columns: minmax(150px, 1fr) 92px 40px;
    gap: 8px;
    align-items: end;
  }

  .source-actions {
    display: flex;
    gap: 6px;
    justify-content: flex-end;
  }

  .section-heading {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    padding: 16px 16px 0;
  }

  .coalition-editor {
    display: grid;
    gap: 10px;
    padding: 12px 16px 0;
  }

  .coalition-row {
    display: grid;
    grid-template-columns: 40px minmax(130px, 1fr) 40px;
    gap: 8px;
    align-items: center;
  }

  .list-editor {
    display: grid;
    gap: 10px;
    padding: 12px 16px 16px;
  }

  .list-row {
    display: grid;
    grid-template-columns: 40px minmax(130px, 1fr) minmax(130px, 0.8fr) 86px 64px 40px;
    gap: 8px;
    align-items: center;
  }

  .color {
    width: 40px;
    padding: 3px;
  }

  .share {
    text-align: right;
  }

  .override-toggle {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 6px;
    min-height: 38px;
    color: #4d5963;
    font-size: 12px;
    font-weight: 700;
  }

  .override-toggle input {
    min-height: auto;
    width: 16px;
    height: 16px;
    margin: 0;
    padding: 0;
  }

  button {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    gap: 8px;
    min-height: 38px;
    border: 1px solid #bdc7d0;
    border-radius: 6px;
    background: #ffffff;
    color: #182026;
    cursor: pointer;
  }

  button:disabled {
    cursor: not-allowed;
    opacity: 0.7;
  }

  .primary {
    min-width: 116px;
    border-color: #2f6f57;
    background: #2f6f57;
    color: #ffffff;
    font-weight: 700;
  }

  .icon-button {
    width: 38px;
    padding: 0;
  }

  .text-button {
    min-height: 32px;
    padding: 0 10px;
    color: #4d5963;
    font-size: 12px;
    font-weight: 700;
  }

  .text-button span {
    color: inherit;
    font-size: inherit;
  }

  .danger {
    color: #9d2d2d;
  }

  .result-panel {
    overflow: hidden;
  }

  .messages {
    margin: 16px;
    padding: 10px 12px;
    font-size: 13px;
  }

  .messages p + p {
    margin-top: 4px;
  }

  .info-messages {
    border-left: 4px solid #5f7f92;
    background: #f1f6f8;
    color: #304b5a;
  }

  .warning-messages {
    border-left: 4px solid #c4822e;
    background: #fff7ec;
    color: #6f4315;
  }

  .diagnostics {
    border-top: 1px solid #e5e9ed;
    padding: 12px 16px 0;
  }

  .diagnostic-toggle {
    width: 100%;
    justify-content: flex-start;
    background: #f7f9fa;
    color: #4d5963;
    font-weight: 700;
  }

  .diagnostic-toggle span {
    flex: 1;
    text-align: left;
  }

  table {
    width: calc(100% - 32px);
    margin: 16px;
    border-collapse: collapse;
    font-size: 13px;
  }

  .diagnostics table {
    width: 100%;
    margin: 12px 0 16px;
  }

  caption {
    margin-bottom: 8px;
    text-align: left;
    color: #4d5963;
    font-weight: 700;
  }

  th,
  td {
    border-bottom: 1px solid #e5e9ed;
    padding: 8px 6px;
    text-align: left;
  }

  th {
    color: #5c6872;
    font-weight: 700;
  }

  @media (max-width: 860px) {
    .workspace {
      padding: 16px;
    }

    .toolbar,
    .scenario-meta {
      grid-template-columns: 1fr;
    }

    .list-row {
      grid-template-columns: 40px minmax(0, 1fr) 78px 58px 40px;
    }

    .setting-row,
    .range-setting,
    .candidate-template-picker,
    .candidate-template-row,
    .candidate-template-meta,
    .candidate-template-target,
    .candidate-template-person,
    .local-override-picker,
    .local-override-group summary,
    .local-override-row,
    .correspondence-source,
    .correspondence-row {
      grid-template-columns: 1fr;
    }

    .candidacy-share-grid {
      grid-template-columns: repeat(2, minmax(0, 1fr));
    }

    .source-actions {
      justify-content: flex-start;
    }

    .list-row select {
      grid-column: 2 / -1;
    }
  }
</style>
