import { exactFraction } from './exact';
import { calculatePercentages, type AggregatedElectoralFigures, type Percentage } from './figures';
import type { Chamber, ChamberInput, PoliticalList, Subject } from './input';
import type { DrawTrace, ReformWarning, ScrutinyExecutionContext } from './result';
import type { SpecialWinnersResult } from './special-winners';
import { resolveTie } from './ties';

export type ListAdmissionReason = 'national-3-percent' | 'regional-20-percent' |
  'minority-regional-20-percent' | 'minority-quarter-uninominal-wins' | 'best-excluded-list' |
  'below-access-thresholds' | 'not-best-excluded-list' | 'failed-coalition-no-independent-access';
export interface ListAdmissionDecision {
  readonly listId: string;
  readonly coalitionId: string | null;
  readonly admitted: boolean;
  readonly admittedAs: 'standalone' | 'coalition-member' | null;
  readonly reasons: readonly ListAdmissionReason[];
  readonly lawReference: string;
  readonly nationalVotes: number;
  readonly nationalPercentage: Percentage;
  /** Senate: every qualifying region; Camera: only the protected region. */
  readonly qualifyingRegionIds: readonly string[];
  readonly minorityQualification: null | {
    readonly regionId: string;
    readonly percentage: Percentage;
    readonly uninominalTerritories: readonly {
      readonly territoryId: string; readonly colleges: number; readonly requiredWins: number; readonly wins: number;
    }[];
  };
}
export interface CoalitionAdmissionDecision {
  readonly coalitionId: string;
  readonly admitted: boolean;
  readonly reasons: readonly ('national-10-percent-with-qualifying-list' | 'below-10-percent' | 'no-qualifying-list')[];
  readonly lawReference: string;
  readonly nationalVotes: number;
  readonly nationalPercentage: Percentage;
  readonly qualifyingListIds: readonly string[];
  readonly admittedListIds: readonly string[];
  readonly bestExcludedListId: string | null;
}
export interface AdmissionInput {
  readonly chamber: ChamberInput;
  readonly figures: AggregatedElectoralFigures;
  readonly simulationId: number;
}
export interface CameraAdmissionInput extends AdmissionInput {
  readonly specialWinners: SpecialWinnersResult;
}
interface AdmissionBase {
  readonly chamber: Chamber;
  readonly norm: 'N07' | 'N08';
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}
export type AdmissionResult = AdmissionBase & (
  | { readonly status: 'non-computable'; readonly reason: 'zero-valid-votes' }
  | {
      readonly status: 'complete';
      readonly coalitions: readonly CoalitionAdmissionDecision[];
      readonly lists: readonly ListAdmissionDecision[];
      /** Only e)(1)/(2) subjects: members of admitted coalitions are not repeated. */
      readonly admittedSubjects: readonly Subject[];
      /** C83(1)(e-bis) communication, without performing the local allocation. */
      readonly taaLocalExceptions: readonly {
        readonly territoryId: string; readonly listId: string;
        readonly reason: 'minority-taa-local-20-percent';
        readonly interpretationId: 'I01'; readonly lawReference: 'C93-septies(2)';
      }[];
    }
);

/** Inclusive comparisons use counts, never a rounded/display percentage. */
function reaches(votes: number, valid: number, percent: number): boolean {
  return valid > 0 && BigInt(votes) * 100n >= BigInt(valid) * BigInt(percent);
}

/** Legal recognition is upstream; verify the declared exclusive presentation scope. */
function minorityRegion(list: PoliticalList, chamber: ChamberInput): string | null {
  const region = list.protectedMinority?.regionId;
  if (!region || !chamber.regions.some(r => r.id === region && r.autonomous)) return null;
  const collegeIds = [...chamber.ordinarySlates.filter(s => s.listId === list.id).map(s => s.collegeId),
    ...chamber.specialCandidatures.filter(c => c.linkedListIds.includes(list.id)).map(c => c.collegeId)];
  const territoryIds = collegeIds.map(id => chamber.colleges.find(c => c.id === id)!.territoryId);
  return territoryIds.length > 0 && territoryIds.every(id => chamber.territories.find(t => t.id === id)!.regionId === region)
    ? region : null;
}

/**
 * C83(1)(e)(1),(2),(2-bis),(2-ter): "almeno il 10 per cento", "almeno il
 * 3 per cento"; protected minorities "almeno il 20 per cento".
 * Only unlinked minorities may alternatively use "almeno un quarto dei collegi
 * uninominali della circoscrizione, con arrotondamento all'unità superiore".
 * Requires validated links/coverage, N06 national INCLUDING special figures,
 * and the SAME preliminary N09 winners that will be reused by R26-10.
 */
export function determineCameraAdmission(input: CameraAdmissionInput, execution: ScrutinyExecutionContext): AdmissionResult {
  if (input.chamber.chamber !== 'camera') throw new RangeError('Camera admission requires Camera input');
  const preliminary = input.specialWinners;
  const colleges = input.chamber.colleges.filter(c => c.kind === 'special-uninominal');
  if (preliminary.chamber !== 'camera' || preliminary.simulationId !== input.simulationId ||
      preliminary.winners.length !== colleges.length || new Set(preliminary.winners.map(w => w.collegeId)).size !== colleges.length ||
      colleges.some(c => !preliminary.winners.some(w => w.collegeId === c.id && w.territoryId === c.territoryId))) {
    throw new RangeError('Supply the complete preliminary Camera winners for this simulation');
  }
  return determineAdmission(input, execution, preliminary);
}

/**
 * S16-bis(1)(e)(1),(2),(2-bis),(2-ter): "almeno il 20 per cento dei voti
 * validi espressi almeno in una regione". This route is NOT minority-only and
 * does not restrict admission to that region. The 10% coalition still needs
 * a qualifying member; (2-ter) adds just one otherwise excluded member.
 */
export function determineSenateAdmission(input: AdmissionInput, execution: ScrutinyExecutionContext): AdmissionResult {
  if (input.chamber.chamber !== 'senato') throw new RangeError('Senate admission requires Senate input');
  return determineAdmission(input, execution);
}

function determineAdmission(input: AdmissionInput, execution: ScrutinyExecutionContext, preliminary?: SpecialWinnersResult): AdmissionResult {
  const { chamber, figures, simulationId } = input;
  const camera = chamber.chamber === 'camera';
  const norm = camera ? 'N07' : 'N08';
  const law = camera ? 'C83(1)(e)' : 'S16-bis(1)(e)';
  const base: AdmissionBase = { chamber: chamber.chamber, norm, draws: [], warnings: [] };
  // I04: 0/0 is undefined, rather than a threshold pass or an exclusion.
  if (figures.national.validVotes === 0) return { ...base, status: 'non-computable', reason: 'zero-valid-votes' };
  const draws: DrawTrace[] = [];
  const warnings: ReformWarning[] = [];
  const lists = [...chamber.lists].sort((a, b) => a.id < b.id ? -1 : 1);
  const evidence = new Map(lists.map(list => {
    const votes = figures.national.lists.find(l => l.listId === list.id)!.votes;
    const region = camera ? minorityRegion(list, chamber) : null;
    const regional = region === null ? null : figures.regions.find(r => r.regionId === region)!.figures;
    const qualifyingRegionIds = figures.regions.filter(r => (!camera || r.regionId === region) &&
      reaches(r.figures.lists.find(l => l.listId === list.id)!.votes, r.figures.validVotes, 20)).map(r => r.regionId).sort();
    const uninominalTerritories = region === null ? [] : chamber.territories.filter(t => t.regionId === region).map(t => {
      const colleges = chamber.colleges.filter(c => c.territoryId === t.id && c.kind === 'special-uninominal').length;
      return { territoryId: t.id, colleges, requiredWins: Math.ceil(colleges / 4),
        wins: preliminary!.winners.filter(w => w.territoryId === t.id && w.linkedListIds.includes(list.id)).length };
    }).sort((a, b) => a.territoryId < b.territoryId ? -1 : 1);
    const reasons: ListAdmissionReason[] = [];
    if (reaches(votes, figures.national.validVotes, 3)) reasons.push('national-3-percent');
    if (qualifyingRegionIds.length) reasons.push(camera ? 'minority-regional-20-percent' : 'regional-20-percent');
    return [list.id, { votes, qualifyingRegionIds, reasons, quarterWins: uninominalTerritories.some(t => t.colleges > 0 && t.wins >= t.requiredWins),
      minorityQualification: region === null ? null : { regionId: region,
        percentage: calculatePercentages([regional!.lists.find(l => l.listId === list.id)!.votes], regional!.validVotes)[0], uninominalTerritories } }];
  }));
  const coalitions: CoalitionAdmissionDecision[] = [...chamber.coalitions].sort((a, b) => a.id < b.id ? -1 : 1).map(coalition => {
    const votes = figures.national.coalitions.find(c => c.coalitionId === coalition.id)!.votes;
    const qualifyingListIds = coalition.listIds.filter(id => evidence.get(id)!.reasons.length > 0).slice().sort();
    const threshold = reaches(votes, figures.national.validVotes, 10);
    const admitted = threshold && qualifyingListIds.length > 0;
    const excluded = admitted ? coalition.listIds.filter(id => !qualifyingListIds.includes(id)).slice().sort() : [];
    let bestExcludedListId: string | null = null;
    if (excluded.length) {
      // (2-ter) specifies national figure only. No old-law 1% floor or
      // second-best rescue; remaining equal figures use the warned I03 policy.
      const tie = resolveTie(excluded.map(id => ({ id, values: [exactFraction(evidence.get(id)!.votes)] })),
        [{ name: 'national-list-votes', direction: 'descending' }], { simulationId,
          request: { contextId: JSON.stringify([norm, chamber.chamber, coalition.id, '2-ter']),
            chamber: chamber.chamber, norm, rule: 'I03', lawReference: `${law}(2-ter)` } }, execution);
      bestExcludedListId = tie.chosenId;
      draws.push(...tie.draws);
      warnings.push(...tie.warnings);
    }
    return { coalitionId: coalition.id, admitted,
      reasons: admitted ? ['national-10-percent-with-qualifying-list'] :
        [...(!threshold ? ['below-10-percent' as const] : []), ...(!qualifyingListIds.length ? ['no-qualifying-list' as const] : [])],
      lawReference: `${law}(1)`, nationalVotes: votes,
      nationalPercentage: calculatePercentages([votes], figures.national.validVotes)[0], qualifyingListIds,
      admittedListIds: admitted ? [...qualifyingListIds, ...(bestExcludedListId === null ? [] : [bestExcludedListId])].sort() : [], bestExcludedListId };
  });
  const decisions: ListAdmissionDecision[] = lists.map(list => {
    const e = evidence.get(list.id)!;
    const coalition = coalitions.find(c => c.coalitionId === list.coalitionId);
    let reasons = [...e.reasons];
    let admittedAs: ListAdmissionDecision['admittedAs'] = null;
    let lawReference = `${law}(2)`;
    if (coalition?.admitted) {
      lawReference = `${law}(2-bis)`;
      if (reasons.length) admittedAs = 'coalition-member';
      else if (coalition.bestExcludedListId === list.id) {
        admittedAs = 'coalition-member'; reasons = ['best-excluded-list']; lawReference = `${law}(2-ter)`;
      } else reasons = ['not-best-excluded-list'];
    } else {
      // Camera's regional/wins route in (2) is reserved to UNLINKED lists.
      // A minority in a failed coalition has only independent national 3%.
      if (camera && coalition) reasons = reasons.filter(r => r === 'national-3-percent');
      if (camera && list.coalitionId === null && e.quarterWins) reasons.push('minority-quarter-uninominal-wins');
      if (reasons.length) admittedAs = 'standalone';
      else reasons = [coalition ? 'failed-coalition-no-independent-access' : 'below-access-thresholds'];
    }
    return { listId: list.id, coalitionId: list.coalitionId, admitted: admittedAs !== null, admittedAs, reasons, lawReference,
      nationalVotes: e.votes, nationalPercentage: calculatePercentages([e.votes], figures.national.validVotes)[0],
      qualifyingRegionIds: e.qualifyingRegionIds, minorityQualification: e.minorityQualification };
  });
  const taaLocalExceptions: Extract<AdmissionResult, { status: 'complete' }>['taaLocalExceptions'][number][] = [];
  if (camera) {
    // C93-septies(2) explicitly says "sono considerate liste singole anche le
    // liste ... collegate in coalizioni che non abbiano raggiunto la percentuale"
    // and cites (2-bis) anomalously. I01 gives this exception LOCAL effect only.
    for (const list of lists) {
      const region = evidence.get(list.id)!.minorityQualification?.regionId;
      if (!region || !coalitions.some(c => c.coalitionId === list.coalitionId && !c.admitted)) continue;
      for (const t of chamber.territories.filter(t => t.kind === 'taa' && t.regionId === region).slice().sort((a, b) => a.id < b.id ? -1 : 1)) {
        const local = figures.territories.find(row => row.territoryId === t.id)!.figures;
        if (reaches(local.lists.find(l => l.listId === list.id)!.votes, local.validVotes, 20)) {
          taaLocalExceptions.push({ territoryId: t.id, listId: list.id, reason: 'minority-taa-local-20-percent',
            interpretationId: 'I01', lawReference: 'C93-septies(2)' });
        }
      }
    }
  }
  return { ...base, status: 'complete', draws, warnings, coalitions, lists: decisions, taaLocalExceptions,
    admittedSubjects: [...coalitions.filter(c => c.admitted).map(c => ({ kind: 'coalition' as const, id: c.coalitionId })),
      ...decisions.filter(l => l.admittedAs === 'standalone').map(l => ({ kind: 'list' as const, id: l.listId }))] };
}
