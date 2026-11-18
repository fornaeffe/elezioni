import { assembleReformSimulation } from './assembly';
import type { ReformScrutinyInput } from './input';
import type { GenerationTrace, ReformGenerationScenario, ReformGenerationSnapshot } from './generation-types';
import { generatePairedMunicipalVotes, preparePairedVoteReferences, type GeneratedMunicipalVotes } from './paired-vote-generation';
import { requireReformScenario, validateReformSnapshot } from './generation-scenario';
import { prepareReferenceBonusPlan, type PreparedBonusPlan } from './reference-bonus-plan';

export type { CollegeCounts } from '../generation/types';
import { aggregateM01Votes } from '../generation/m01-counts';
import { m01Data } from '../generation/types';
export function aggregateReformCollegeVotes(s: ReformGenerationScenario, snapshot: ReformGenerationSnapshot,
  votes: readonly GeneratedMunicipalVotes[], simulationId: number) {
  return aggregateM01Votes(s, m01Data(snapshot), votes, simulationId);
}

export interface PreparedReformGeneration {
  scenario: ReformGenerationScenario;
  snapshot: ReformGenerationSnapshot;
  references: ReturnType<typeof preparePairedVoteReferences>;
  bonusPlan?: PreparedBonusPlan;
}
export function prepareReformGeneration(value: unknown, snapshot: ReformGenerationSnapshot): PreparedReformGeneration {
  const errors = validateReformSnapshot(snapshot);
  if (errors.length) throw new RangeError(`Invalid reform snapshot: ${errors.join('; ')}`);
  const scenario = requireReformScenario(value, snapshot), owned = structuredClone(snapshot);
  const references = preparePairedVoteReferences(scenario, owned);
  const bonusPlan = scenario.candidateGeneration.bonusPolicy ? prepareReferenceBonusPlan(scenario, owned, references.units) : undefined;
  if (bonusPlan) {
    references.trace.bonusSelection = { policy: bonusPlan.policy, referenceDate: bonusPlan.referenceDate,
      slates: bonusPlan.slates.map(({ chamber, territoryId, subject, allocations }) => ({ chamber, territoryId, subject, allocations })) };
    if (bonusPlan.slates.some(b => b.allocations.some(a => a.slots !== a.referenceSlots))) references.trace.warnings.push({
      code: 'BONUS_SELECTION_CONSTRAINTS', message: 'Composizione del premio adattata ai candidati forniti, ai prefissi disponibili o ai vincoli di genere; le quote di riferimento restano distinte dalle candidature.' });
  }
  return { scenario, snapshot: owned, references, ...(bonusPlan ? { bonusPlan } : {}) };
}
export function generateReformSimulation(prepared: PreparedReformGeneration, simulationId: number): { input: ReformScrutinyInput; trace: GenerationTrace } {
  if (!Number.isSafeInteger(simulationId) || simulationId < 1) throw new RangeError('Expected a positive simulation ID');
  const { scenario: s, snapshot } = prepared, trace = structuredClone(prepared.references.trace);
  const votes = generatePairedMunicipalVotes(s, snapshot, prepared.references.units, simulationId);
  const counts = aggregateReformCollegeVotes(s, snapshot, votes, simulationId);
  return assembleReformSimulation(prepared, simulationId, counts, trace);
}
export { assembleReformSimulation } from './assembly';
