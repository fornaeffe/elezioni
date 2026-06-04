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

export interface Scenario {
  id: string;
  name: string;
  electionDate: string;
  lists: ScenarioList[];
  coalitions: ScenarioCoalition[];
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
