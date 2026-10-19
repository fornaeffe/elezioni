import { runPoliticsScrutiny } from './scrutiny';
import type { PoliticsScrutinyContext, PoliticsScrutinyInput, PoliticsScrutinyOutput } from './types';
import { REFORM_ALGORITHM_ID } from './reform-2026/input';
import { PoliticsAlgorithmUnavailableError, legacyPoliticsScrutinyAlgorithmId } from './simulation-scrutiny';
export * from './simulation-scrutiny';

export const defaultPoliticsScrutinyAlgorithmId = legacyPoliticsScrutinyAlgorithmId;

export interface PoliticsScrutinyAlgorithm {
  id: string;
  name: string;
  description: string;
  lawReference: string;
  run(input: PoliticsScrutinyInput, context: PoliticsScrutinyContext): PoliticsScrutinyOutput;
}

export interface PoliticsScrutinyAlgorithmResolution {
  algorithm: PoliticsScrutinyAlgorithm;
  requestedId: string;
  fallback: boolean;
}

const legacyAlgorithms: readonly PoliticsScrutinyAlgorithm[] = [
  {
    id: defaultPoliticsScrutinyAlgorithmId,
    name: 'R parity',
    description:
      'Current TypeScript translation of the R politics scrutiny path, preserving golden-master behavior and law-review TODOs.',
    lawReference: 'R/politiche/scrutinio.R and translated law comments in web/src/lib/politics/scrutiny.ts',
    run: runPoliticsScrutiny
  }
];

const politicsScrutinyAlgorithmById = new Map(legacyAlgorithms.map((algorithm) => [algorithm.id, algorithm]));

export function resolvePoliticsScrutinyAlgorithm(requestedId?: string): PoliticsScrutinyAlgorithmResolution {
  const normalizedId = requestedId?.trim() || defaultPoliticsScrutinyAlgorithmId;
  // This compatibility resolver serves the legacy generator/worker only.
  // A known reform request must never execute the old law through fallback.
  if (normalizedId === REFORM_ALGORITHM_ID) throw new PoliticsAlgorithmUnavailableError(normalizedId, 'production-unavailable');
  const algorithm = politicsScrutinyAlgorithmById.get(normalizedId);

  if (algorithm) {
    return {
      algorithm,
      requestedId: normalizedId,
      fallback: false
    };
  }

  const defaultAlgorithm = politicsScrutinyAlgorithmById.get(defaultPoliticsScrutinyAlgorithmId);
  if (!defaultAlgorithm) {
    throw new Error(`Default politics scrutiny algorithm ${defaultPoliticsScrutinyAlgorithmId} is not registered.`);
  }

  return {
    algorithm: defaultAlgorithm,
    requestedId: normalizedId,
    fallback: true
  };
}
