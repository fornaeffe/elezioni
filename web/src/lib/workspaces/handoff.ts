import type { SimulationResultExport } from '../core/result-export';
import type { ReleasedScenario } from '../politics/reform-2026/vote-share/release';
import type { WorkspaceId } from './registry';

export interface WorkspaceImport { file?: File; payload?: SimulationResultExport; scenario?: ReleasedScenario | import('../politics/scenario/schema').PoliticsScenario }
// Browser-only one-shot navigation handoff. Files stay owned in memory; there
// is no persistent global draft or reinterpretation through another schema.
let pending: { target: WorkspaceId; input: WorkspaceImport } | undefined;
export function queueWorkspaceImport(target: WorkspaceId, input: WorkspaceImport) { pending = { target, input }; }
export function takeWorkspaceImport(target: WorkspaceId): WorkspaceImport | undefined {
  if (pending?.target !== target) return;
  const input = pending.input; pending = undefined; return input;
}
