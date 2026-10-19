import { determineCameraAdmission, determineSenateAdmission, type AdmissionResult } from './admission';
import { distributeBonusOnlyVotes, type BonusAttributionResult } from './attribution';
import { determineGovernabilityBonus, type GovernabilityBonusResult } from './bonus';
import { allocateCameraNationalSeats, type CameraNationalAllocationResult } from './camera-national';
import { allocateCameraTerritorialSeats, type CameraCollegeResult } from './camera-territorial';
import { aggregateElectoralFigures, type AggregatedElectoralFigures } from './figures';
import type { ChamberInput, ReformScrutinyInput } from './input';
import { resolveMultipleElections, type MultipleElectionsResult } from './multiple-elections';
import { proclaimCandidates, type CandidateProclamations } from './proclamations';
import { recoverInitialSeats, type InitialRecoveryResult } from './recovery';
import { assembleRecoveryRemainders } from './recovery-communications';
import type { NormId, ReformScrutinyResult, ScrutinyExecutionContext, StageOutcome } from './result';
import { allocateSenateRegions, type SenateRegionalAllocationResult } from './senate-regional';
import { allocateSenateListsAndColleges, type SenateCollegeResult } from './senate-territorial';
import type { ValidationIssue } from './shape';
import { attributeSpecialVotes, type SpecialAttributionResult } from './special-attribution';
import { proclaimSpecialUninominalWinners, summarizeSpecialSeats, type SpecialUninominalResult } from './special-seats';
import { determineSpecialWinners, type SpecialWinnersResult } from './special-winners';
import { allocateCameraTaaSeats, type CameraTaaAllocationResult } from './taa-allocation';
import { createSeededDrawContext } from './ties';
import { validateReformScrutinyInput } from './validation';
import { rankPreferenceCandidates, type PreferenceRanking } from './rankings';
import { scrutinyMetadata } from './scrutiny-metadata';
import { allocationTransfers } from './scrutiny-transfers';

/** All communications belong to the enclosing simulation. No caller can splice
 * an admission, figure or allocation from a different run into this execution. */
export interface ReformChamberExecution {
  readonly chamber: ChamberInput['chamber'];
  readonly simulationId: number;
  readonly attribution: readonly BonusAttributionResult[];
  readonly specialAttribution: readonly SpecialAttributionResult[];
  readonly preliminary: SpecialWinnersResult;
  readonly special: SpecialUninominalResult;
  readonly figures?: AggregatedElectoralFigures;
  readonly rankings?: readonly PreferenceRanking[];
  readonly admission?: AdmissionResult;
  readonly taa?: CameraTaaAllocationResult;
  readonly national?: CameraNationalAllocationResult;
  readonly cameraTerritorial?: ReturnType<typeof allocateCameraTerritorialSeats>;
  readonly regional?: SenateRegionalAllocationResult;
  readonly senateTerritorial?: ReturnType<typeof allocateSenateListsAndColleges>;
  readonly proclamations?: CandidateProclamations;
  readonly multiple?: MultipleElectionsResult;
  readonly recovery?: InitialRecoveryResult;
}
export interface ComposedReformScrutinyResult extends ReformScrutinyResult {
  readonly randomness: ReformScrutinyInput['randomness'];
  readonly bonus: GovernabilityBonusResult;
  readonly chambers: readonly ReformChamberExecution[];
}
export class ReformScrutinyInputError extends Error {
  constructor(readonly issues: readonly ValidationIssue[]) {
    super(`Invalid reform scrutiny input: ${issues.map(i => `${i.path}: ${i.message}`).join('; ')}`);
    this.name = 'ReformScrutinyInputError';
  }
}

/** Compose the existing legal stages for ONE domestic initial simulation.
 * Validation precedes all draws. Legal stages retain their own law comments;
 * this function supplies their communications, never alternative legal rules.
 * The default private stream includes the global simulation identity. */
export function runPoliticsReformScrutiny(value: unknown, execution?: ScrutinyExecutionContext): ComposedReformScrutinyResult {
  const checked = validateReformScrutinyInput(value);
  if (!checked.valid) throw new ReformScrutinyInputError(checked.issues);
  const input = checked.input;
  const context = execution ?? createSeededDrawContext(input.randomness.seed,
    JSON.stringify([input.randomness.streamVersion, input.algorithmId, input.simulationId]));
  const renewed = input.renewal.kind === 'joint' ? [input.renewal.camera, input.renewal.senato] : [input.renewal.chamber];
  const stages: StageOutcome[] = [], draws: ReformScrutinyResult['draws'][number][] = [], warnings: ReformScrutinyResult['warnings'][number][] = [];
  const events = (s: { readonly draws: ReformScrutinyResult['draws']; readonly warnings: ReformScrutinyResult['warnings'] }) => {
    draws.push(...s.draws); warnings.push(...s.warnings);
  };
  function record(chamber: ChamberInput['chamber'] | null, norm: NormId, status: 'complete' | 'partial' | 'non-computable' | 'not-run', reason = '', affectedIds: readonly string[] = []) {
    stages.push(status === 'complete' ? { norm, chamber, status } : { norm, chamber, status, reason, affectedIds });
  }
  const chambers: ReformChamberExecution[] = renewed.map(chamber => {
    const args = { chamber, candidates: input.candidates, simulationId: input.simulationId };
    for (const norm of ['N01', 'N02', 'N03'] as const) record(chamber.chamber, norm, 'complete');
    const preliminary = determineSpecialWinners(args, context); events(preliminary);
    record(chamber.chamber, 'N09', 'complete');
    const special = proclaimSpecialUninominalWinners({ ...args, preliminary });
    const attribution = [...chamber.ordinaryVotes].filter(r => chamber.colleges.find(c => c.id === r.collegeId)!.kind === 'ordinary')
      .sort((a, b) => a.collegeId < b.collegeId ? -1 : 1)
      .map(r => distributeBonusOnlyVotes({ chamber: chamber.chamber, simulationId: input.simulationId,
        collegeId: r.collegeId, coalitions: chamber.coalitions, votes: r.votes }, context));
    const specialAttribution = [...chamber.specialVotes].sort((a, b) => a.collegeId < b.collegeId ? -1 : 1)
      .map(r => attributeSpecialVotes({ ...args, collegeId: r.collegeId }, context));
    attribution.forEach(events); specialAttribution.forEach(events);
    const failed = attribution.filter(r => r.status !== 'complete');
    record(chamber.chamber, 'N04', failed.length ? 'non-computable' : 'complete', 'zero-direct-support', failed.map(r => r.collegeId));
    const specialFailed = specialAttribution.filter(r => r.status !== 'complete');
    record(chamber.chamber, 'N10', specialFailed.length ? 'non-computable' : special.status, specialFailed.length ? 'zero-direct-support' : 'special-candidate-unavailable', specialFailed.map(r => r.collegeId));
    const base = { chamber: chamber.chamber, simulationId: input.simulationId, preliminary, special, attribution, specialAttribution };
    if (failed.length || specialFailed.length) {
      for (const norm of ['N05', 'N06', chamber.chamber === 'camera' ? 'N07' : 'N08', ...(chamber.chamber === 'camera' ? ['N11'] : [])] as NormId[]) {
        record(chamber.chamber, norm, 'not-run', 'attribution-non-computable');
      }
      return base;
    }
    const figures = aggregateElectoralFigures(chamber, attribution.flatMap(r => r.status === 'complete' ? [r] : []),
      specialAttribution.flatMap(r => r.status === 'complete' ? [r] : []));
    record(chamber.chamber, 'N06', 'complete');
    const rankings = chamber.ordinarySlates.map(s => rankPreferenceCandidates(s,
      [...figures.colleges, ...figures.taaProportional].find(f => f.collegeId === s.collegeId)!.figures));
    record(chamber.chamber, 'N05', rankings.some(r => r.status !== 'complete') ? 'partial' : 'complete', 'missing-preferences');
    const admission = chamber.chamber === 'camera' ? determineCameraAdmission({ ...args, figures, specialWinners: preliminary }, context) :
      determineSenateAdmission({ ...args, figures }, context);
    events(admission); record(chamber.chamber, admission.norm, admission.status, admission.status === 'non-computable' ? admission.reason : '');
    const taa = chamber.chamber === 'camera' ? allocateCameraTaaSeats({ ...args, figures, admission,
      collegeId: chamber.colleges.find(c => c.kind === 'taa-proportional')!.id }, context) : undefined;
    if (taa) { events(taa); record(chamber.chamber, 'N11', taa.status, taa.status === 'non-computable' ? taa.reason : 'provisional-candidate-identities'); }
    return { ...base, figures, rankings, admission, ...(taa ? { taa } : {}) };
  });
  const bonus = determineGovernabilityBonus({ simulationId: input.simulationId, renewedChambers: renewed.map(c => c.chamber),
    bonusLinks: input.bonusLinks, chambers: chambers.flatMap(s => s.admission && s.figures ? [{ chamber: s.chamber, admission: s.admission, national: s.figures.national }] : []) }, context);
  events(bonus); record(null, 'N12', bonus.status === 'non-computable' ? 'non-computable' : 'complete', 'bonus-prerequisites-non-computable');
  for (let i = 0; i < chambers.length; i++) {
    const s = chambers[i], chamber = renewed[i];
    if (!s.figures || !s.admission) {
      for (const norm of (s.chamber === 'camera' ? ['N13', 'N14', 'N15', 'N16', 'N17', 'N18'] : ['N19', 'N20', 'N21', 'N22']).concat(['N23', 'N24', 'N25', 'N26', 'N27']) as NormId[]) {
        record(s.chamber, norm, 'not-run', 'figures-unavailable');
      }
      continue;
    }
    const args = { chamber, candidates: input.candidates, simulationId: input.simulationId,
      figures: s.figures, admission: s.admission, specialSeats: summarizeSpecialSeats(s.special, s.taa), bonus };
    let ordinary: CameraCollegeResult | SenateCollegeResult;
    let allocation: Partial<ReformChamberExecution>;
    let remainders;
    if (s.chamber === 'camera') {
      const national = allocateCameraNationalSeats(args, context); events(national);
      record(s.chamber, 'N13', national.status, national.status === 'non-computable' ? national.reason : '');
      record(s.chamber, 'N14', national.status === 'complete' ? 'complete' : 'not-run', 'national-allocation-non-computable');
      const territorial = allocateCameraTerritorialSeats({ ...args, national }, context);
      const cap = national.status === 'complete' && national.path === 'bonus-with-cap';
      for (const [norm, t] of [[cap ? 'N17' : 'N15', territorial.constituencies], ['N16', territorial.lists], ['N18', territorial.colleges]] as const) {
        events(t); record(s.chamber, norm, t.status, t.status === 'non-computable' ? t.reason : '');
      }
      record(s.chamber, cap ? 'N15' : 'N17', 'not-run', 'alternative-allocation-path');
      ordinary = territorial.colleges; allocation = { national, cameraTerritorial: territorial };
      remainders = assembleRecoveryRemainders(chamber, { ...territorial, taa: s.taa });
    } else {
      const regional = allocateSenateRegions(args, context); events(regional);
      record(s.chamber, 'N19', regional.status, regional.status === 'non-computable' ? regional.reason : '');
      record(s.chamber, 'N20', regional.cap?.status ?? 'not-run', regional.cap?.status === 'non-computable' ? regional.cap.reason : 'no-cap-correction');
      const territorial = allocateSenateListsAndColleges({ ...args, regional }, context);
      events(territorial.lists); events(territorial.colleges);
      record(s.chamber, 'N21', territorial.status, territorial.colleges.status === 'non-computable' ? territorial.colleges.reason : '');
      record(s.chamber, 'N22', regional.status === 'complete' && regional.path === 'bonus-with-cap' ? territorial.colleges.status : 'not-run', 'alternative-allocation-path');
      ordinary = territorial.colleges; allocation = { regional, senateTerritorial: territorial };
      remainders = assembleRecoveryRemainders(chamber, { ...territorial, regional });
    }
    const proclamations = proclaimCandidates({ ...args, ordinary, special: s.special, taa: s.taa });
    record(s.chamber, 'N23', proclamations.status, proclamations.blockers.join(',') || 'provisional-candidate-identities');
    const multiple = resolveMultipleElections(proclamations, context); events(multiple);
    record(s.chamber, 'N24', multiple.status, multiple.blockers.join(',') || 'released-or-unresolved-seats');
    // Admission, rather than positive targets, qualifies recovery destinations.
    const admission = s.admission;
    const eligibleLists = admission.status === 'complete' ? chamber.territories.flatMap(t =>
      admission.lists.filter(l => l.admitted || admission.taaLocalExceptions.some(e => e.territoryId === t.id && e.listId === l.listId))
        .map(l => ({ territoryId: t.id, listId: l.listId }))) : [];
    const recovery = recoverInitialSeats({ ...args, initial: multiple, eligibleLists, remainders }, context); events(recovery);
    for (const norm of ['N25', 'N26', 'N27'] as const) {
      const applicable = s.chamber === 'senato' ? norm === 'N27' : norm !== 'N27';
      record(s.chamber, norm, applicable ? recovery.status : 'not-run', applicable ? recovery.terminalReason : 'other-chamber-rule');
    }
    chambers[i] = { ...s, ...allocation, proclamations, multiple, recovery };
  }
  const seats = chambers.flatMap(s => s.recovery && s.recovery.status !== 'non-computable' ? s.recovery.seats :
    [...s.special.seats, ...(s.taa && s.taa.status !== 'non-computable' ? s.taa.seats : [])]);
  const status = chambers.some(s => !s.recovery || s.recovery.status === 'non-computable') ? 'non-computable' :
    seats.every(s => s.status === 'filled') ? 'complete' : 'partial';
  record(null, 'N28', status, status === 'partial' ? 'unfilled-or-unresolved-entitlements' : 'stage-prerequisite-non-computable');
  // A successful terminal ledger accounts for all domestic entitlements exactly
  // once. Filled identities are globally unique, including both chambers.
  if (new Set(seats.map(s => s.seatId)).size !== seats.length ||
      new Set(seats.flatMap(s => s.status === 'filled' ? [s.candidateId] : [])).size !== seats.filter(s => s.status === 'filled').length) {
    throw new Error('Composed scrutiny produced duplicate seat/candidate identities');
  }
  for (const c of chambers) if (c.recovery && c.recovery.status !== 'non-computable' && c.recovery.seats.length !== (c.chamber === 'camera' ? 392 : 196)) {
    throw new Error('Composed scrutiny lost domestic seat entitlements');
  }
  const metadata = scrutinyMetadata(input, renewed, chambers, warnings);
  for (const c of chambers) {
    const first = bonus.firstSubjects.find(f => f.chamber === c.chamber);
    const award = bonus.awards.find(a => a.chamber === c.chamber);
    if (first) metadata.subjectDecisions.push({ norm: 'N12', chamber: c.chamber, subject: first.subject,
      decision: award ? 'bonus-awarded' : bonus.status === 'non-computable' ? 'unresolved' : 'bonus-absent',
      reason: bonus.status === 'awarded' ? 'linked-first-subjects-at-least-42-percent' :
        bonus.status === 'not-awarded' ? bonus.reasons.map(r => r.code).join(',') : bonus.blockers.map(b => b.reason).join(','), lawReference: bonus.lawReference });
  }
  return structuredClone({ schemaVersion: 1, algorithmId: input.algorithmId, simulationId: input.simulationId,
    status, scope: 'domestic-initial', renewedChambers: renewed.map(c => c.chamber), legalVersion: input.legalVersion,
    geographyVersion: input.geographyVersion, chamberModel: input.chamberModel, randomness: input.randomness,
    seatTables: renewed.map(c => ({ chamber: c.chamber, source: c.seatTable })), ...metadata,
    warnings, stages, draws, seats, transfers: allocationTransfers(renewed, chambers),
    recoveries: chambers.flatMap(s => s.recovery?.recoveries ?? []), bonus, chambers });
}
