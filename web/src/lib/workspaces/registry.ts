import type { ElectionKind, SimulationRequest } from '../core/types';
import { REFORM_ALGORITHM_ID } from '../politics/reform-2026/input';
import { generationModels } from '../politics/reform-2026/vote-share/model-registry';

/** A workspace is a supported combination, not the product of all selectors. */
export const workspaces = [
  { id: 'politics-legacy', kind: 'politiche', route: '/politics', name: 'Politiche',
    label: 'Legge precedente (R parity)', territory: 'Italia',
    algorithm: 'politiche-r-parity-v1', scenarioFamily: 'legacy',
    data: 'politics', models: [], maximumSimulations: 1000, fitting: false, storedResults: false },
  { id: 'politics-reform', kind: 'politiche', route: '/politics', name: 'Politiche',
    label: 'Riforma 2026', territory: 'Italia', algorithm: REFORM_ALGORITHM_ID,
    scenarioFamily: 'reform', data: 'reform', models: generationModels,
    maximumSimulations: null, fitting: true, storedResults: true },
  { id: 'regional-er', kind: 'regionali-er', route: '/emilia-romagna', name: 'Regionali Emilia-Romagna',
    label: 'Regionali Emilia-Romagna', territory: 'Emilia-Romagna',
    algorithm: 'regionali-er-r-parity-v1', scenarioFamily: 'legacy',
    data: 'regional-er', models: [], maximumSimulations: null, fitting: false, storedResults: false },
] as const;
export type WorkspaceId = typeof workspaces[number]['id'];
export type WorkspaceDefinition = typeof workspaces[number];
export const politicsWorkspaces = workspaces.filter(w => w.kind === 'politiche');
export const electionWorkspaces = [workspaces[0], workspaces[2]];

export function workspace(id: WorkspaceId): WorkspaceDefinition {
  const definition = workspaces.find(w => w.id === id);
  if (!definition) throw new Error(`Unsupported workspace ${id}`);
  return definition;
}

export function workspaceFor(kind: ElectionKind, algorithm?: string): WorkspaceDefinition {
  const definition = workspaces.find(w => w.kind === kind &&
    (algorithm ? w.algorithm === algorithm : w.id !== 'politics-reform'));
  if (!definition) throw new Error(`Unsupported election/algorithm ${kind}/${algorithm ?? 'default'}`);
  return definition;
}

/** Repeated at the worker boundary: UI validation is not an execution contract. */
export function validateWorkspaceRequest(request: SimulationRequest): WorkspaceDefinition {
  const definition = workspaceFor(request.kind, request.scrutinyAlgorithmId);
  const reform = 'kind' in request.scenario;
  if (reform !== (definition.scenarioFamily === 'reform')) throw new Error('Scenario/workspace mismatch');
  if ('defaultSource' in request.scenario && request.scenario.defaultSource.electionKind !== definition.kind) throw new Error('Scenario/election mismatch');
  if (request.electionDate !== request.scenario.electionDate) throw new Error('Scenario/election date mismatch');
  if (!Number.isSafeInteger(request.simulations) || request.simulations < 1 ||
    definition.maximumSimulations !== null && request.simulations > definition.maximumSimulations)
    throw new RangeError(`Simulazioni non valide per ${definition.label}${definition.maximumSimulations ? ` (massimo ${definition.maximumSimulations})` : ''}`);
  return definition;
}
