/** The common preparation â†’ generation â†’ law assembly â†’ scrutiny dispatch boundary. */
import { ROSATELLUM, REFORM, chambers, type PoliticsScenario } from '../scenario/schema';
import { activeGenerator, requirePair } from '../scenario/registry';
import { parsePoliticsScenario, validateActiveScenario, ScenarioValidationError } from '../scenario/validation';
import { effectivePlacements, renewed } from '../scenario/effective';
import { loadExecutionData, type ExecutionReaders } from './assets';
import { commonVoteInputs } from './inputs';
import { rosatellumCoverage, type RosatellumSeats } from './rosatellum';
import { reformCoverage, reformLegalSettings } from './reform';
import { compileM01 } from '../generation/m01-compiler';
import { preparePairedVoteReferences, generatePairedMunicipalVotes } from '../generation/m01';
import { aggregateM01Votes } from '../generation/m01-counts';
import { prepareM02Votes } from '../generation/m02';
import type { GeneratedVotes } from '../generation/types';
import { physicalCells } from '../reform-2026/vote-share/physical-cells';
import { compileSource } from '../reform-2026/vote-share/source-compiler';
import { compilePairedHistory } from '../reform-2026/vote-share/paired-source';
import { cooperativeSteps, throwIfCancelled, type FitProgress } from '../reform-2026/vote-share/execution';
import { fitReferenceSteps } from '../reform-2026/vote-share/information';
import { fitProfilesSteps } from '../reform-2026/vote-share/profiles';
import { prepareSenateAdapter } from '../reform-2026/vote-share/senate-adapter';
import { FAST_SHARE_MODEL, FAST_SHARE_NUMERICS } from '../reform-2026/vote-share/types';
import { sourceKey } from '../reform-2026/vote-share/source-codec';
import type { FastVoteAssets } from '../reform-2026/vote-share/forecast-types';
import type { ReformAssemblyContext } from '../reform-2026/generation-types';
import type { SeatTableBundle } from '../reform-2026/endowments';
import { prepareReferenceBonusPlan, type PreparedBonusPlan } from '../reform-2026/reference-bonus-plan';
import type { AssemblySource } from '../reform-2026/assembly';
import { m02AssemblySource } from '../reform-2026/vote-share/generation';
import { runPoliticsSimulation } from '../simulation-scrutiny';
import { commonPoliticsImplementation } from './build';
import type { FitState, ProfileState } from '../reform-2026/vote-share/release';
import { bindPoliticsGeneration, type PoliticsComputeInputs, type PoliticsGeneratorInputs, type PoliticsLegalInputs } from './compute-inputs';
import { m02PreparationKey, m02GenerationKey } from './keys';

export interface PoliticsNumericStates { fit: FitState; profiles: ProfileState }
/** One entry per consumed stage: cache size does not grow with edits or runs. */
export class PoliticsPreparationCache {
  fit?: { key: string; state: FitState };
  profiles?: { key: string; state: ProfileState };
  m01?: { key: string; compiled: Awaited<ReturnType<typeof compileM01>>; prepared: ReturnType<typeof preparePairedVoteReferences> };
  clear() { this.fit = undefined; this.profiles = undefined; this.m01 = undefined; }
}

export interface PreparationOptions {
  signal?: AbortSignal;
  checkpoint?: (progress: FitProgress) => Promise<void>;
  maximumWorkspaceBytes?: number;
  cache?: PoliticsPreparationCache;
  numericStates?: PoliticsNumericStates;
}
export async function preparePoliticsExecution(value: unknown, readers: ExecutionReaders, options: PreparationOptions = {}) {
  if (!readers.implementationId || commonPoliticsImplementation && readers.implementationId !== commonPoliticsImplementation)
    throw new Error('POLITICS_IMPLEMENTATION_MISMATCH: supply the actual common execution build identity');
  const s = parsePoliticsScenario(value), model = activeGenerator(s), pair = requirePair(s.selection.scrutiny.id, model.id);
  const checkpoint = async (p: FitProgress) => { throwIfCancelled(options.signal); await options.checkpoint?.(p); throwIfCancelled(options.signal); };
  await checkpoint({ phase: 'load-politics-dependencies', completed: 0, total: 1 });
  const data = await loadExecutionData(s, readers);
  const issues = validateActiveScenario(s, data.domain);
  if (issues.length) throw new ScenarioValidationError(issues);
  const law = s.selection.scrutiny.id;
  const context: ReformAssemblyContext | undefined = law === REFORM ? { version: s.data.lawAssets[law].seats.version,
    sourceVersions: [...data.source.sourceVersions, s.data.historical.sha256, s.data.lawAssets[law].geography.sha256],
    assumptions: [...data.source.assumptions, ...s.provenance.assumptions],
    seats: data.seats as SeatTableBundle, models: data.parameters.models } : undefined;
  const coverage = law === ROSATELLUM ? rosatellumCoverage(s, data.seats as RosatellumSeats) : reformCoverage(s, context!);
  if (law === REFORM && (context!.seats.version !== s.data.lawAssets[law].seats.version || context!.seats.geography.version !== data.geography.version))
    throw new Error('REFORM_SEATS_MISMATCH');
  const activeChambers = chambers.filter(ch => renewed(s, ch));
  physicalCells(data.geography, data.electorates, activeChambers, s.execution.simulationSeed);
  for (const ch of activeChambers) {
    const legal = coverage.filter(c => c.chamber === ch), physical = data.geography.colleges.filter(c => c.chamber === ch && c.kind !== 'taa-proportional');
    if (physical.some(c => !legal.some(l => l.collegeId === c.id)) ||
      legal.some(c => !data.geography.colleges.some(p => p.id === c.collegeId && p.chamber === ch) && !(law === REFORM && context!.seats.geography.colleges.some(p => p.id === c.collegeId && p.chamber === ch && p.kind === 'taa-proportional'))))
      throw new Error('LAW_PHYSICAL_GEOGRAPHY_MISMATCH');
  }
  if (law === ROSATELLUM) for (const cell of data.geography.cells) {
    const ballot = (data.seats as RosatellumSeats).ballots.find(b => b.id === cell.collegeId && b.chamber === cell.chamber);
    if (!ballot || data.geography.targets.find(t => t.code === cell.municipality)?.region !== ballot.regionId)
      throw new Error('ROSATELLUM_MUNICIPAL_REGION_CROSSWALK');
  }
  const chamberProfileId = law === REFORM ? s.lawSettings[REFORM].chamberProfileId : data.parameters.rosatellumChamberProfileId;
  if (!data.parameters.models.chambers.profiles.some(p => p.id === chamberProfileId)) throw new Error('CHAMBER_PROFILE_UNAVAILABLE');
  const inputs = commonVoteInputs(s, data, chamberProfileId, coverage);
  let generateVotes: (simulation: number) => GeneratedVotes;
  let bonusPlan: PreparedBonusPlan | undefined, source: AssemblySource | undefined;
  let legal = law === REFORM ? reformLegalSettings(s, data.domain, inputs.common) : undefined;
  let preparationKey: string, generationKey: string;
  let references: GeneratedVotes['trace']['references'];
  let numericStates: PoliticsNumericStates | undefined;
  let generatorInputs: PoliticsGeneratorInputs;
  const reuse = { fit: false, profiles: false };
  if (model.id === 'M01') {
    // SDs, fitting seed, presentation and candidature are not M01 statistical inputs.
    const { lists, components, references: refs, nonValidReferences, ...input } = inputs.common;
    preparationKey = await sourceKey('politics-m01-preparation-v1', { ...input,
      lists: Object.fromEntries(chambers.map(ch => [ch, lists[ch].map(l => ({ id: l.id, componentIds: l.componentIds }))])),
      components: components.map(c => ({ id: c.id })),
      references: refs.map(({ sdPercentagePoints, ...r }) => r), nonValidReferences: nonValidReferences.map(({ sdPercentagePoints, ...r }) => r),
      source: s.data.historical.sha256, geography: s.data.lawAssets[law].geography.sha256,
      electorate: s.data.electorate.sha256, models: data.parameters.models.chambers });
    const cached = options.cache?.m01?.key === `${readers.implementationId}/${preparationKey}` ? options.cache.m01 : undefined;
    const compiled = cached?.compiled ?? await compileM01(inputs.common, data.source, readers.sourceChunk, data,
      { geography: law === REFORM ? context!.seats.geography : data.geography, models: data.parameters.models }, preparationKey);
    const prepared = cached ? { ...cached.prepared, trace: structuredClone(cached.prepared.trace) } : preparePairedVoteReferences(compiled.scenario, compiled.snapshot);
    if (!cached && options.cache) options.cache.m01 = { key: `${readers.implementationId}/${preparationKey}`, compiled,
      prepared: { ...prepared, trace: structuredClone(prepared.trace) } };
    prepared.trace.modelVersion = `${model.id}@${model.version}`;
    references = prepared.trace.references;
    if (legal) {
      legal = { ...legal, pollReferences: compiled.scenario.pollReferences };
      bonusPlan = prepareReferenceBonusPlan({ ...legal, projectionDate: compiled.scenario.projectionDate },
        { ...context!, memberships: compiled.snapshot.memberships }, prepared.units);
      // Retain the existing bonus/capacity validator, including CP-01's restricted M01 blocker.
      if (bonusPlan) prepared.trace.bonusSelection = { policy: bonusPlan.policy, referenceDate: bonusPlan.referenceDate,
        slates: bonusPlan.slates.map(({ chamber, territoryId, subject, allocations }) => ({ chamber, territoryId, subject, allocations })) };
    }
    source = { voteParameterVersion: `${model.id}@${model.version}/${data.parameters.models.chambers.parameterVersion}`,
      chamberModelId: 'paired-chamber-votes-v1', chamberParameterVersion: data.parameters.models.chambers.parameterVersion,
      sourceVersions: [preparationKey], assumptions: ['M01 2.0.0: latest eligible common-18+ history, conditional electorates, turnout-preserving political support and labelled regional/equal fallback.'] };
    generationKey = preparationKey;
    generatorInputs = { id: 'M01', settings: compiled.scenario, data: compiled.snapshot, units: prepared.units, trace: prepared.trace };
    generateVotes = simulation => ({ counts: aggregateM01Votes(compiled.scenario, compiled.snapshot,
      generatePairedMunicipalVotes(compiled.scenario, compiled.snapshot, prepared.units, simulation), simulation), trace: structuredClone(prepared.trace) });
  } else {
    const compiled = await compileSource(data.source, readers.sourceChunk, inputs.selection, { polls: inputs.common.polls.added, checkpoint });
    const paired = await compilePairedHistory(data.source, readers.sourceChunk, inputs.selection,
      { camera: s.history.pairedCamera!, senato: s.history.pairedSenato!, electoratePolicy: 'equal-only' }, checkpoint);
    const pairedInputKey = await sourceKey('common-paired-v1', paired.history);
    preparationKey = await m02PreparationKey(s, { nationalInputKey: compiled.nationalInputKey,
      profileInputKey: compiled.profileInputKey, pairedInputKey });
    const fitKey = await sourceKey('common-fit-v1', { implementation: readers.implementationId, model,
      input: compiled.nationalInputKey, seed: s.execution.fittingSeed, policies: s.modelSettings.M02.policies });
    const profileKey = await sourceKey('common-profiles-v1', { implementation: readers.implementationId,
      input: compiled.profileInputKey, paired: paired.history, referenceDate: s.referenceDate });
    const fitParents = { nationalInputKey: compiled.nationalInputKey, implementationId: readers.implementationId,
      fitSeed: s.execution.fittingSeed };
    const profileParents = { profileInputKey: compiled.profileInputKey,
      pairedInputKey, implementationId: readers.implementationId, referenceDate: s.referenceDate };
    const restored = options.numericStates;
    if (restored && (Object.entries(fitParents).some(([k, v]) => restored.fit.parents[k as keyof typeof fitParents] !== v) ||
      Object.entries(profileParents).some(([k, v]) => restored.profiles.parents[k as keyof typeof profileParents] !== v)))
      throw new Error('POLITICS_NUMERIC_STATE_DEPENDENCY_MISMATCH');
    let fitState = restored?.fit ?? (options.cache?.fit?.key === fitKey ? options.cache.fit.state : undefined);
    let profileState = restored?.profiles ?? (options.cache?.profiles?.key === profileKey ? options.cache.profiles.state : undefined);
    reuse.fit = !!fitState; reuse.profiles = !!profileState;
    if (!fitState) {
      const fit = await cooperativeSteps(fitReferenceSteps(compiled.national, s.execution.fittingSeed,
        { maximumWorkspaceBytes: options.maximumWorkspaceBytes }), checkpoint);
      fitState = { parents: { ...fitParents, solver: fit.audit.iterations.some(i => i.solver === 'matrix-free') ? 'matrix-free' : 'dense' }, fit, provenanceId: compiled.provenanceId,
        warnings: compiled.warnings, sampler: { chains: 4, burn: 1000, thin: 2, retention: 'all-accepted-joint-context-draws' } };
      if (options.cache) options.cache.fit = { key: fitKey, state: fitState };
    }
    if (!profileState) {
      const cameraProfile = await cooperativeSteps(fitProfilesSteps(compiled.profiles, options.maximumWorkspaceBytes), checkpoint);
      profileState = { parents: profileParents, cameraProfile,
        senateAdapter: prepareSenateAdapter(cameraProfile, s.referenceDate, paired.history), pairedAudit: paired.audit };
      if (options.cache) options.cache.profiles = { key: profileKey, state: profileState };
    }
    numericStates = { fit: fitState, profiles: profileState };
    const { fit } = fitState, { cameraProfile, senateAdapter } = profileState;
    const assets: FastVoteAssets = { fit, cameraProfile, senateAdapter, modelId: FAST_SHARE_MODEL, numericalVersion: FAST_SHARE_NUMERICS,
      fitSeed: s.execution.fittingSeed, profileReferenceDate: s.referenceDate, sourceBundleId: data.source.id,
      fittedStateId: preparationKey, profileStateId: preparationKey, geography: data.geography, electorates: data.electorates };
    inputs.fast.data.fittedStateId = preparationKey; inputs.fast.data.profileStateId = preparationKey;
    const prepared = prepareM02Votes(inputs.fast, assets, { geography: context!.seats.geography, models: data.parameters.models },
      { maximumWorkspaceBytes: options.maximumWorkspaceBytes, copyAssets: false });
    references = prepared.trace.references;
    generationKey = await m02GenerationKey(preparationKey, inputs.fast);
    bonusPlan = prepareReferenceBonusPlan(legal!, context!, [], { referenceDate: s.referenceDate, support: prepared.support });
    source = m02AssemblySource(inputs.fast, assets);
    generatorInputs = { id: 'M02', settings: inputs.fast, assets, parents: { fit: fitState.parents, profiles: profileState.parents } };
    generateVotes = simulation => {
      const result = prepared.run(simulation);
      if (bonusPlan) result.trace.bonusSelection = { policy: bonusPlan.policy, referenceDate: bonusPlan.referenceDate,
        slates: bonusPlan.slates.map(({ chamber, territoryId, subject, allocations }) => ({ chamber, territoryId, subject, allocations })) };
      return result;
    };
  }
  const assemblyKey = await sourceKey('politics-assembly-v1', { pair, generationKey, settings: s.lawSettings[law],
    placements: effectivePlacements(s, law, data.domain.geographyRelations), candidates: s.candidates,
    affiliation: s.subjects.lists.map(({ name, color, ...l }) => l), seats: s.data.lawAssets[law].seats.sha256 });
  const legalInputs: PoliticsLegalInputs = law === REFORM ? { id: REFORM, settings: legal!, context: context!, bonusPlan, source: source! } :
    { id: ROSATELLUM, seats: data.seats as RosatellumSeats, geographyRelations: data.domain.geographyRelations, references };
  const keys = { preparationKey, generationKey, assemblyKey };
  const execution = bindPoliticsGeneration(s, readers.implementationId, keys, legalInputs, generateVotes);
  const computeInputs: PoliticsComputeInputs = { generator: generatorInputs, legal: legalInputs };
  await checkpoint({ phase: 'politics-ready', completed: 1, total: 1 });
  return { ...execution, computeInputs, numericStates, reuse,
    run: (simulation: number) => { const generated = execution.generate(simulation); return { ...generated, result: runPoliticsSimulation(generated.request) }; } };
}
export type PreparedPoliticsExecution = Awaited<ReturnType<typeof preparePoliticsExecution>>;
