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

export type ScenarioGlobalShareMode = 'mean' | 'fixed';

export type ScenarioListCorrespondenceSource = 'bundled' | 'homonymous' | 'manual';

export interface ScenarioListCorrespondence {
  id: string;
  futureList: string;
  pastElection: string;
  pastDate: string;
  pastList: string;
  factor: number;
  source: ScenarioListCorrespondenceSource;
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
