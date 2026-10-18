import { exactFraction } from './exact';
import { sumVoteCounts } from './figures';
import type { Candidate, Chamber, ChamberInput, Subject } from './input';
import type { DrawTrace, ReformWarning, ScrutinyExecutionContext } from './result';
import { resolveTie, type TieResolution } from './ties';

/** Preliminary N09 ledger; admission and later special scrutiny reuse it. */
export interface SpecialWinner {
  readonly collegeId: string;
  readonly territoryId: string;
  readonly regionId: string;
  readonly candidateId: string;
  readonly votes: number;
  readonly subject: Subject;
  readonly linkedListIds: readonly string[];
  readonly affiliationListId: string | null;
  readonly lawReference: string;
  readonly tie: TieResolution;
}
export interface SpecialWinnersResult {
  readonly status: 'complete';
  readonly chamber: Chamber;
  readonly simulationId: number;
  readonly winners: readonly SpecialWinner[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}

/**
 * C93(2)-(3), C93-septies(1)(a)-(b), S21(1)-(2):
 * "il candidato che ha ottenuto il maggior numero di voti validi";
 * Camera "più giovane di età", Senato "più anziano di età".
 * Election is independent of linked-list admission. Equal dates use I03.
 * Requires validated candidatures and adjudicated candidate totals, which are
 * available in BOTH vote representations before N10 list-vote attribution.
 */
export function determineSpecialWinners(
  input: { readonly chamber: ChamberInput; readonly candidates: readonly Candidate[]; readonly simulationId: number },
  execution: ScrutinyExecutionContext
): SpecialWinnersResult {
  const { chamber, simulationId } = input;
  const colleges = chamber.colleges.filter(c => c.kind === 'special-uninominal')
    .slice().sort((a, b) => a.id < b.id ? -1 : 1);
  const rows = new Map(chamber.specialVotes.map(row => [row.collegeId, row.votes.candidateTotals]));
  if (rows.size !== chamber.specialVotes.length || rows.size !== colleges.length || colleges.some(c => !rows.has(c.id))) {
    throw new RangeError('Supply exactly one candidate-total row per special college');
  }
  const people = new Map(input.candidates.map(c => [c.id, c]));
  // Preflight all colleges before consuming any draw.
  const prepared = colleges.map(college => {
    const territory = chamber.territories.find(t => t.id === college.territoryId)!;
    const totals = rows.get(college.id)!;
    const filed = chamber.specialCandidatures.filter(c => c.collegeId === college.id);
    const ids = new Set(totals.map(t => t.candidateId));
    if (!filed.length || ids.size !== totals.length || ids.size !== filed.length || filed.some(c => !ids.has(c.candidateId))) {
      throw new RangeError('Supply all filed special candidate totals, including zeros');
    }
    sumVoteCounts(totals.map(t => t.votes));
    for (const c of filed) {
      if (!people.has(c.candidateId) || !c.linkedListIds.length ||
          c.linkedListIds.some(id => !chamber.lists.some(l => l.id === id))) {
        throw new RangeError('Special winners require validated candidate and list references');
      }
    }
    const lawReference = chamber.chamber === 'senato' ? 'S21(1)-(2)' :
      territory.kind === 'vda' ? 'C93(2)-(3)' : 'C93-septies(1)(a)-(b)';
    return { college, territory, totals, filed, lawReference };
  });
  const winners = prepared.map(({ college, territory, totals, filed, lawReference }): SpecialWinner => {
    const tie = resolveTie(totals.map(row => ({ id: row.candidateId,
      values: [exactFraction(row.votes), exactFraction(Number(people.get(row.candidateId)!.birthDate.replaceAll('-', '')))]
    })), [{ name: 'candidate-votes', direction: 'descending' },
      { name: 'birth-date', direction: chamber.chamber === 'camera' ? 'descending' : 'ascending' }], {
      simulationId, request: { contextId: JSON.stringify(['N09', chamber.chamber, college.id]),
        chamber: chamber.chamber, norm: 'N09', rule: 'I03', lawReference }
    }, execution);
    const winner = filed.find(c => c.candidateId === tie.chosenId)!;
    const linkedListIds = [...winner.linkedListIds].sort();
    const list = chamber.lists.find(l => l.id === linkedListIds[0])!;
    return { collegeId: college.id, territoryId: territory.id, regionId: territory.regionId,
      candidateId: winner.candidateId, votes: totals.find(t => t.candidateId === winner.candidateId)!.votes,
      subject: list.coalitionId === null ? { kind: 'list', id: list.id } : { kind: 'coalition', id: list.coalitionId },
      linkedListIds, affiliationListId: winner.affiliationListId, lawReference, tie };
  });
  return { status: 'complete', chamber: chamber.chamber, simulationId, winners,
    draws: winners.flatMap(w => w.tie.draws), warnings: winners.flatMap(w => w.tie.warnings) };
}
