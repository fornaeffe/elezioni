import { REFORM, ROSATELLUM, type GeneratorId, type LawId, type PoliticsScenario } from './schema';
import { modelRegistry } from '../reform-2026/vote-share/model-registry';

/** The common execution and worker boundary use this exact supported-pair registry. */
export const generators = {
  M01: { id: modelRegistry.M01.id, version: modelRegistry.M01.version, referenceSd: false, uncertaintyMultipliers: false,
    historySelection: 'latest-eligible-pair', sourceCompositions: true, partialPolls: true, pooling: false },
  M02: { id: modelRegistry.M02.id, version: modelRegistry.M02.version, referenceSd: true, uncertaintyMultipliers: true,
    historySelection: 'selected', sourceCompositions: false, partialPolls: false, pooling: true }
} as const;
/** First explicit scrutiny metadata version; no change to the legal algorithms. */
export const scrutinies = {
  [ROSATELLUM]: { id: ROSATELLUM, version: '1.0.0', blockVersion: 1, bonusLinks: false, suppliedInputs: false,
    filingLevels: ['plurinominal', 'uninominal'] }, // VdA and Senate TAA exceptions are checked by the law adapter.
  [REFORM]: { id: REFORM, version: '1.0.0', blockVersion: 1, bonusLinks: true, suppliedInputs: true,
    filingLevels: ['college'] }
} as const;
export const supportedPairs = [
  { law: ROSATELLUM, generator: 'M01', adapterId: 'rosatellum-m01-v1' },
  { law: REFORM, generator: 'M01', adapterId: 'reform-m01-v1' },
  { law: REFORM, generator: 'M02', adapterId: 'reform-m02-v1' }
] as const;
export function requirePair(law: LawId, generator: GeneratorId) {
  const pair = supportedPairs.find(p => p.law === law && p.generator === generator);
  if (!pair) throw new Error(`UNSUPPORTED_PAIR: ${law}/${generator}`);
  return pair;
}
export function activeGenerator(s: PoliticsScenario) { return s.selection.generatorsByLaw[s.selection.scrutiny.id]; }
export function initialSelection(): PoliticsScenario['selection'] {
  return { scrutiny: { id: REFORM, version: '1.0.0' }, generatorsByLaw: {
    [ROSATELLUM]: { id: 'M01', version: '2.0.0' }, [REFORM]: { id: 'M02', version: '2.0.0' } } };
}
export function capabilities(s: PoliticsScenario) {
  const law = s.selection.scrutiny.id, model = activeGenerator(s);
  return { scrutiny: scrutinies[law], generator: generators[model.id], pair: requirePair(law, model.id) };
}
