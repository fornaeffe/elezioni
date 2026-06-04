import { runPoliticsScrutiny } from './scrutiny';
import type { PoliticsScrutinyContext, PoliticsScrutinyInput, PoliticsScrutinyOutput } from './types';

export const defaultPoliticsScrutinyAlgorithmId = 'politiche-r-parity-v1';

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

export const politicsScrutinyAlgorithms: readonly PoliticsScrutinyAlgorithm[] = [
  {
    id: defaultPoliticsScrutinyAlgorithmId,
    name: 'R parity',
    description:
      'Current TypeScript translation of the R politics scrutiny path, preserving golden-master behavior and law-review TODOs.',
    lawReference: 'R/politiche/scrutinio.R and translated law comments in web/src/lib/politics/scrutiny.ts',
    run: runPoliticsScrutiny
  }
];

const politicsScrutinyAlgorithmById = new Map(politicsScrutinyAlgorithms.map((algorithm) => [algorithm.id, algorithm]));

export function resolvePoliticsScrutinyAlgorithm(requestedId?: string): PoliticsScrutinyAlgorithmResolution {
  const normalizedId = requestedId?.trim() || defaultPoliticsScrutinyAlgorithmId;
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
