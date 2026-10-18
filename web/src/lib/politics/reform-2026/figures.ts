import { exactFraction } from './exact';
import type { ChamberInput, ListVotes, PoliticalList, Coalition } from './input';
import type { ExactFraction } from './result';

export type Percentage =
  | { readonly status: 'defined'; readonly value: ExactFraction }
  | { readonly status: 'undefined'; readonly reason: 'zero-valid-votes' };

/** Counts stay JSON-safe; intermediate sums must never silently lose integers. */
export function sumVoteCounts(values: readonly number[]): number {
  let total = 0n;
  for (const value of values) {
    if (!Number.isSafeInteger(value) || value < 0) throw new RangeError('Expected nonnegative safe vote counts');
    total += BigInt(value);
  }
  if (total > BigInt(Number.MAX_SAFE_INTEGER)) throw new RangeError('Vote total exceeds the safe integer domain');
  return Number(total);
}

/** C77(1)(e),(e-bis), S16(1)(e),(e-bis), C93-septies(1)(f)-(h):
 * "per il totale dei voti validi ... moltiplicato per cento".
 * I04: an empty electorate's percentage is undefined, never zero percent.
 */
export function calculatePercentages(counts: readonly number[], validVotes: number): readonly Percentage[] {
  sumVoteCounts([validVotes]);
  return counts.map(count => {
    sumVoteCounts([count]);
    return validVotes === 0
      ? { status: 'undefined', reason: 'zero-valid-votes' }
      : { status: 'defined', value: exactFraction(BigInt(count) * 100n, validVotes) };
  });
}

export interface ElectoralFigures {
  readonly validVotes: number;
  readonly lists: readonly (ListVotes & { readonly percentage: Percentage })[];
  /** Includes every linked list, with no admission or sub-1% filtering. */
  readonly coalitions: readonly { readonly coalitionId: string; readonly votes: number; readonly percentage: Percentage }[];
}

export interface CollegeListFigure {
  readonly collegeId: string;
  readonly listVotes: readonly ListVotes[];
}

export interface AggregatedElectoralFigures {
  /** These disjoint ballot partitions alone are counted in all upper levels. */
  readonly colleges: readonly { readonly collegeId: string; readonly figures: ElectoralFigures }[];
  readonly territories: readonly { readonly territoryId: string; readonly figures: ElectoralFigures }[];
  readonly regions: readonly { readonly regionId: string; readonly figures: ElectoralFigures }[];
  /** Admission, first place and the 42% denominator; includes VdA and TAA. */
  readonly national: ElectoralFigures;
  /** Excludes both special territories, for ordinary allocation only. */
  readonly ordinaryAllocation: ElectoralFigures;
  /** C93-septies: views of TAA territorial ballots, never extra votes. */
  readonly taaProportional: readonly { readonly collegeId: string; readonly figures: ElectoralFigures }[];
}

/** Reject duplicate/missing/unknown college or list rows rather than losing votes. */
function indexRows(rows: readonly CollegeListFigure[], expected: readonly string[]): Map<string, CollegeListFigure> {
  const result = new Map(rows.map(row => [row.collegeId, row]));
  if (result.size !== rows.length || result.size !== expected.length || expected.some(id => !result.has(id))) {
    throw new RangeError('Supply exactly one attributed figure for each expected college');
  }
  return result;
}

function figures(rows: readonly CollegeListFigure[], lists: readonly PoliticalList[], coalitions: readonly Coalition[]): ElectoralFigures {
  const totals = new Map(lists.map(list => [list.id, 0]));
  for (const row of rows) {
    const seen = new Set<string>();
    for (const item of row.listVotes) {
      if (!totals.has(item.listId) || seen.has(item.listId)) throw new RangeError('Unknown or duplicate list figure');
      seen.add(item.listId);
      totals.set(item.listId, sumVoteCounts([totals.get(item.listId)!, item.votes]));
    }
  }
  const listVotes = lists.map(list => ({ listId: list.id, votes: totals.get(list.id)! }));
  const validVotes = sumVoteCounts(listVotes.map(row => row.votes));
  const percentages = calculatePercentages(listVotes.map(row => row.votes), validVotes);
  const coalitionVotes = coalitions.map(coalition => ({ coalitionId: coalition.id,
    votes: sumVoteCounts(coalition.listIds.map(id => {
      if (!totals.has(id)) throw new RangeError('Unknown coalition member');
      return totals.get(id)!;
    })) }));
  const coalitionPercentages = calculatePercentages(coalitionVotes.map(row => row.votes), validVotes);
  return { validVotes, lists: listVotes.map((row, i) => ({ ...row, percentage: percentages[i] })),
    coalitions: coalitionVotes.map((row, i) => ({ ...row, percentage: coalitionPercentages[i] })) };
}

/**
 * C77(1)(f),(i), C83(1)(a)-(d-bis),(1-sexies); S16(1)(f),(i),
 * S16-bis(1)(a)-(d),(1-sexies): "somma delle cifre elettorali";
 * C83(1)(d-bis): "escludendo i voti espressi nelle circoscrizioni"
 * VdA/TAA for ordinary allocation. Special ballots still count nationally.
 *
 * Requires validated chamber references and completed attribution at the legal
 * college level. specialFigures is N10's future output (or supplied attributed
 * input), NOT candidate totals added to list votes. No special scrutiny runs here.
 * Missing contributions are errors, not partial national totals.
 */
export function aggregateElectoralFigures(
  chamber: ChamberInput,
  ordinaryFigures: readonly CollegeListFigure[],
  specialFigures: readonly CollegeListFigure[]
): AggregatedElectoralFigures {
  const ordinary = indexRows(ordinaryFigures, chamber.colleges.filter(c => c.kind === 'ordinary').map(c => c.id));
  const special = indexRows(specialFigures, chamber.colleges.filter(c => c.kind === 'special-uninominal').map(c => c.id));
  const rows = [...ordinary.values(), ...special.values()];
  for (const row of rows) {
    const expected = ordinary.has(row.collegeId)
      ? chamber.ordinarySlates.filter(s => s.collegeId === row.collegeId).map(s => s.listId)
      : [...new Set(chamber.specialCandidatures.filter(c => c.collegeId === row.collegeId).flatMap(c => c.linkedListIds))];
    const ids = row.listVotes.map(v => v.listId);
    if (ids.length !== expected.length || new Set(ids).size !== ids.length || expected.some(id => !ids.includes(id))) {
      throw new RangeError('Supply all contesting list figures, including explicit zeros');
    }
  }
  const make = (selected: readonly CollegeListFigure[]) => figures(selected, chamber.lists, chamber.coalitions);
  const territoryOf = new Map(chamber.colleges.map(c => [c.id, c.territoryId]));
  const territories = chamber.territories.map(t => ({ territoryId: t.id,
    figures: make(rows.filter(row => territoryOf.get(row.collegeId) === t.id)) }));
  const territoryById = new Map(territories.map(t => [t.territoryId, t.figures]));
  return {
    colleges: rows.map(row => ({ collegeId: row.collegeId, figures: make([row]) })),
    territories,
    regions: chamber.regions.map(region => {
      const ids = new Set(chamber.territories.filter(t => t.regionId === region.id).map(t => t.id));
      return { regionId: region.id, figures: make(rows.filter(row => ids.has(territoryOf.get(row.collegeId)!))) };
    }),
    national: make(rows),
    ordinaryAllocation: make(rows.filter(row => chamber.territories.find(t => t.id === territoryOf.get(row.collegeId))!.kind === 'ordinary')),
    taaProportional: chamber.colleges.filter(c => c.kind === 'taa-proportional').map(c => ({
      collegeId: c.id, figures: structuredClone(territoryById.get(c.territoryId)!)
    }))
  };
}
