export type ElectionKind = 'politiche' | 'regionali-er' | 'comunali';

export interface ScenarioList {
  id: string;
  name: string;
  coalition: string | null;
  color: string;
  startingShare: number;
  shareOverride: boolean;
}

export interface ScenarioCoalition {
  id: string;
  name: string;
  color: string;
}

export type ScenarioDefaultSourceKind = 'bundled' | 'last-election' | 'manual';

export interface ScenarioDefaultSource {
  kind: ScenarioDefaultSourceKind;
  electionKind: ElectionKind;
  territory: string;
  dataVersion: string;
  snapshotId?: string;
}

export type ScenarioGlobalShareMode = 'mean';

export type ScenarioListCorrespondenceSource = 'bundled' | 'manual';

export interface ScenarioListCorrespondence {
  id: string;
  futureList: string;
  pastElection: string;
  pastDate: string;
  pastList: string;
  factor: number;
  source: ScenarioListCorrespondenceSource;
}

export type ScenarioLocalShareOverrideScope = 'municipality';

export interface ScenarioLocalShareOverride {
  id: string;
  scope: ScenarioLocalShareOverrideScope;
  locationCode: string;
  list: string;
  startingShare: number;
}

export type ScenarioCandidateTemplateRamo = 'camera' | 'senato';
export type ScenarioCandidateTemplateKind = 'uninominal' | 'plurinominal';

export interface ScenarioCandidateTemplate {
  id: string;
  ramo: ScenarioCandidateTemplateRamo;
  kind: ScenarioCandidateTemplateKind;
  candidateName: string;
  birthDate: string | null;
  coalition?: string | null;
  uninominalCode?: string | null;
  list?: string | null;
  plurinominalCode?: string | null;
  candidateNumber?: number | null;
  minority?: boolean | null;
}

export type ScenarioPlurinominalCandidacyCountShares = [number, number, number, number, number];

export interface ScenarioCandidateGeneration {
  uninominalToPlurinominalShare: number;
  plurinominalCandidacyCountShares: ScenarioPlurinominalCandidacyCountShares;
}

export interface Scenario {
  id: string;
  name: string;
  electionDate: string;
  defaultSource: ScenarioDefaultSource;
  globalShareMode: ScenarioGlobalShareMode;
  abstentionShare: number;
  abstentionOverride: boolean;
  lists: ScenarioList[];
  coalitions: ScenarioCoalition[];
  listCorrespondences: ScenarioListCorrespondence[];
  localShareOverrides: ScenarioLocalShareOverride[];
  candidateTemplates: ScenarioCandidateTemplate[];
  candidateGeneration: ScenarioCandidateGeneration;
}

export interface SimulationRequest {
  kind: ElectionKind;
  scenario: Scenario;
  electionDate: string;
  simulations: number;
  seed: string;
  dataVersion: string;
  scrutinyAlgorithmId?: string;
}

export interface SimulationProgress {
  type: 'progress';
  phase: 'validate' | 'prepare' | 'simulate' | 'scrutinize' | 'summarize';
  completed: number;
  total: number;
  elapsedMs: number;
}

export interface ScrutinyWarning {
  code: string;
  electionKind: ElectionKind;
  simulationId?: number;
  severity?: 'info' | 'warning' | 'error';
  message: string;
  lawReference?: string;
  todoReference?: string;
}

export interface ResultTable {
  name: string;
  columns: string[];
  rows: Array<Record<string, string | number | boolean | null>>;
}

export interface SimulationBenchmark {
  startedAt: string;
  elapsedMs: number;
  simulations: number;
  dataVersion: string;
  scrutinyAlgorithmId?: string;
}

export interface SimulationResult {
  type: 'result';
  status: 'completed' | 'not_implemented';
  tables: ResultTable[];
  warnings: ScrutinyWarning[];
  benchmark: SimulationBenchmark;
}

export type SimulationWorkerMessage = SimulationProgress | SimulationResult;
