/** Worker-owned generation inputs: no source columns, catalogue, fit cache or I/O. */
import type { PoliticsScenario } from '../scenario/schema';
import { REFORM, ROSATELLUM } from '../scenario/schema';
import { requirePair, activeGenerator } from '../scenario/registry';
import type { M01Settings, M01Data, GeneratedVotes } from '../generation/types';
import type { ProjectedMunicipality } from '../generation/m01';
import { generatePairedMunicipalVotes } from '../generation/m01';
import { aggregateM01Votes } from '../generation/m01-counts';
import { prepareM02Votes, type M02Settings } from '../generation/m02';
import type { FastVoteAssets } from '../reform-2026/vote-share/forecast-types';
import type { FitState, ProfileState } from '../reform-2026/vote-share/release';
import type { ReformAssemblyContext, ReformLegalSettings } from '../reform-2026/generation-types';
import type { PreparedBonusPlan } from '../reform-2026/reference-bonus-plan';
import { assembleReformSimulation, type AssemblySource } from '../reform-2026/assembly';
import type { RosatellumSeats } from './rosatellum';
import { prepareRosatellumAssembly } from './rosatellum';
import type { DomainContext } from '../scenario/validation';
import type { PoliticsSimulationRequest } from '../simulation-scrutiny';

export interface PoliticsExecutionKeys { preparationKey: string; generationKey: string; assemblyKey: string }
export type PoliticsGeneratorInputs = {
  id: 'M01'; settings: M01Settings; data: M01Data;
  units: ProjectedMunicipality[]; trace: GeneratedVotes['trace'];
} | {
  id: 'M02'; settings: M02Settings; assets: FastVoteAssets;
  parents: { fit: FitState['parents']; profiles: ProfileState['parents'] };
};
export type PoliticsLegalInputs = {
  id: typeof REFORM; settings: ReformLegalSettings; context: ReformAssemblyContext;
  bonusPlan?: PreparedBonusPlan; source: AssemblySource;
} | {
  id: typeof ROSATELLUM; seats: RosatellumSeats;
  geographyRelations: DomainContext['geographyRelations']; references: GeneratedVotes['trace']['references'];
};
export interface PoliticsComputeInputs {
  generator: PoliticsGeneratorInputs;
  legal: PoliticsLegalInputs;
}

/** Both coordinator and compute workers assemble the exact same law inputs. */
export function bindPoliticsGeneration(scenario: PoliticsScenario, implementationId: string,
  keys: PoliticsExecutionKeys, legal: PoliticsLegalInputs, generateVotes: (id: number) => GeneratedVotes) {
  const pair = requirePair(scenario.selection.scrutiny.id, activeGenerator(scenario).id);
  const context = legal.id === REFORM ? legal.context : undefined;
  const legalSettings = legal.id === REFORM ? legal.settings : undefined;
  const rosatellum = legal.id === ROSATELLUM ? prepareRosatellumAssembly(scenario, legal.seats,
    { geographyRelations: legal.geographyRelations }, legal.references) : undefined;
  const generate = (simulation: number) => {
    if (!Number.isSafeInteger(simulation) || simulation < 1) throw new Error('Expected a positive simulation ID');
    const votes = generateVotes(simulation);
    const request: PoliticsSimulationRequest = legal.id === REFORM ? {
      algorithmId: REFORM, simulationId: simulation,
      input: assembleReformSimulation({ scenario: legal.settings, snapshot: legal.context, bonusPlan: legal.bonusPlan },
        simulation, votes.counts, votes.trace, legal.source).input,
    } : rosatellum!(simulation, votes);
    return { request, votes };
  };
  return { scenario, pair, implementationId, keys, context, legalSettings, generateVotes, generate };
}

/** The caller owns the packet's arrays. No second numeric copy or checkpoint
 * parents/cache handle is retained by the generator. No historical compilation. */
export function preparePoliticsComputeInputs(scenario: PoliticsScenario, implementationId: string,
  keys: PoliticsExecutionKeys, inputs: PoliticsComputeInputs) {
  const g = inputs.generator, legal = inputs.legal;
  if (g.id !== activeGenerator(scenario).id || legal.id !== scenario.selection.scrutiny.id ||
    g.settings.seed !== scenario.execution.simulationSeed || g.settings.electionDate !== scenario.electionDate ||
    g.settings.renewal !== scenario.renewal) throw new Error('POLITICS_COMPUTE_SELECTION_MISMATCH');
  let generateVotes: (id: number) => GeneratedVotes;
  if (g.id === 'M01') {
    generateVotes = id => ({ counts: aggregateM01Votes(g.settings, g.data,
      generatePairedMunicipalVotes(g.settings, g.data, g.units, id), id), trace: structuredClone(g.trace) });
  } else {
    if (legal.id !== REFORM || g.parents.fit.implementationId !== implementationId ||
      g.parents.profiles.implementationId !== implementationId || g.parents.fit.fitSeed !== scenario.execution.fittingSeed ||
      g.parents.profiles.referenceDate !== scenario.referenceDate || g.settings.fitSeed !== scenario.execution.fittingSeed ||
      g.assets.fittedStateId !== keys.preparationKey || g.assets.profileStateId !== keys.preparationKey ||
      !g.parents.fit.nationalInputKey || !g.parents.profiles.profileInputKey || !g.parents.profiles.pairedInputKey)
      throw new Error('POLITICS_NUMERIC_STATE_DEPENDENCY_MISMATCH');
    const prepared = prepareM02Votes(g.settings, g.assets, { geography: legal.context.seats.geography, models: legal.context.models },
      { copyAssets: false });
    // Only the generator and the small bonus plan belong in this closure.
    const bonusPlan = legal.bonusPlan;
    generateVotes = id => {
      const result = prepared.run(id);
      if (bonusPlan) result.trace.bonusSelection = { policy: bonusPlan.policy, referenceDate: bonusPlan.referenceDate,
        slates: bonusPlan.slates.map(({ chamber, territoryId, subject, allocations }) => ({ chamber, territoryId, subject, allocations })) };
      return result;
    };
  }
  return bindPoliticsGeneration(scenario, implementationId, keys, legal, generateVotes);
}
export type PreparedPoliticsCompute = ReturnType<typeof preparePoliticsComputeInputs>;
