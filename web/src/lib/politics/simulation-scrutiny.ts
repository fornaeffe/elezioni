import { REFORM_ALGORITHM_ID, type ReformScrutinyInput, type Chamber } from './reform-2026/input';
import { runPoliticsReformScrutiny, type ComposedReformScrutinyResult } from './reform-2026/scrutiny';
import type { ScrutinyExecutionContext } from './reform-2026/result';
import { runPoliticsScrutiny } from './scrutiny';
import type { PoliticsScrutinyInput, PoliticsScrutinyContext, PoliticsScrutinyOutput } from './types';

export const legacyPoliticsScrutinyAlgorithmId = 'politiche-r-parity-v1' as const;
export type PoliticsSimulationRequest = {
  readonly algorithmId: typeof legacyPoliticsScrutinyAlgorithmId;
  readonly simulationId: number;
  readonly chambers: readonly { readonly input: PoliticsScrutinyInput; readonly context: PoliticsScrutinyContext }[];
} | {
  readonly algorithmId: typeof REFORM_ALGORITHM_ID;
  readonly simulationId: number;
  readonly input: ReformScrutinyInput;
};
export type PoliticsSimulationResult = {
  readonly algorithmId: typeof legacyPoliticsScrutinyAlgorithmId;
  readonly simulationId: number;
  readonly chambers: readonly { readonly chamber: Chamber; readonly output: PoliticsScrutinyOutput }[];
} | {
  readonly algorithmId: typeof REFORM_ALGORITHM_ID;
  readonly simulationId: number;
  readonly result: ComposedReformScrutinyResult;
};
export interface PoliticsSimulationAlgorithm {
  readonly id: PoliticsSimulationRequest['algorithmId'];
  readonly name: string;
  readonly description: string;
  readonly lawReference: string;
  readonly productionAvailable: boolean;
  execute(request: PoliticsSimulationRequest, execution?: ScrutinyExecutionContext): PoliticsSimulationResult;
}
export class PoliticsAlgorithmUnavailableError extends Error {
  constructor(readonly algorithmId: string, readonly reason: 'unknown-algorithm' | 'production-unavailable') {
    super(`Politics scrutiny algorithm ${algorithmId}: ${reason}`);
    this.name = 'PoliticsAlgorithmUnavailableError';
  }
}
export const politicsScrutinyAlgorithms: readonly PoliticsSimulationAlgorithm[] = [
  { id: legacyPoliticsScrutinyAlgorithmId, name: 'R parity', productionAvailable: true,
    description: 'Legacy scrutiny adapter preserving the R golden-master behavior.',
    lawReference: 'R/politiche/scrutinio.R and translated comments in scrutiny.ts',
    execute(request) {
      if (request.algorithmId !== legacyPoliticsScrutinyAlgorithmId) throw new RangeError('Legacy adapter requires legacy input');
      if (!request.chambers.length || request.chambers.length > 2 || new Set(request.chambers.map(c => c.context.ramo)).size !== request.chambers.length) {
        throw new RangeError('Supply one legacy communication per renewed chamber');
      }
      for (const c of request.chambers) for (const r of [...c.input.liste_uni, ...c.input.candidati_uni, ...c.input.candidati_pluri]) {
        if (r.SIM !== undefined && r.SIM !== request.simulationId) throw new RangeError('Legacy input belongs to a different simulation');
      }
      return { algorithmId: request.algorithmId, simulationId: request.simulationId,
        chambers: [...request.chambers].sort((a, b) => a.context.ramo < b.context.ramo ? -1 : 1)
          .map(c => ({ chamber: c.context.ramo, output: runPoliticsScrutiny(c.input, c.context) })) };
    } },
  { id: REFORM_ALGORITHM_ID, name: 'Reform 2026', productionAvailable: false,
    description: 'Paired domestic initial scrutiny from complete direct legal inputs; production generation is pending R26-19/20.',
    lawReference: 'docs/politics-reform-2026-specification.md; stage-local law comments',
    execute(request, execution) {
      if (request.algorithmId !== REFORM_ALGORITHM_ID || request.input.algorithmId !== REFORM_ALGORITHM_ID || request.input.simulationId !== request.simulationId) {
        throw new RangeError('Reform request and envelope must identify the same algorithm and simulation');
      }
      return { algorithmId: REFORM_ALGORITHM_ID, simulationId: request.simulationId, result: runPoliticsReformScrutiny(request.input, execution) };
    } }
];

/** Strict resolution for the common direct-input boundary: no fallback. The
 * requested ID is always the executed/result ID. Production support is explicit. */
export function resolvePoliticsSimulationAlgorithm(id: string, mode: 'direct' | 'production' = 'direct'): PoliticsSimulationAlgorithm {
  const algorithm = politicsScrutinyAlgorithms.find(a => a.id === id);
  if (!algorithm) throw new PoliticsAlgorithmUnavailableError(id, 'unknown-algorithm');
  if (mode === 'production' && !algorithm.productionAvailable) throw new PoliticsAlgorithmUnavailableError(id, 'production-unavailable');
  return algorithm;
}
export function runPoliticsSimulation(request: PoliticsSimulationRequest, execution?: ScrutinyExecutionContext): PoliticsSimulationResult {
  if (!Number.isSafeInteger(request.simulationId) || request.simulationId < 0) throw new RangeError('Expected one nonnegative safe simulation ID');
  return resolvePoliticsSimulationAlgorithm(request.algorithmId).execute(request, execution);
}
