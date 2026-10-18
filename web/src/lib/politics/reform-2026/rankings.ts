import { calculatePercentages, sumVoteCounts, type ElectoralFigures, type Percentage } from './figures';
import type { OrdinarySlate } from './input';

interface RankingBase {
  readonly collegeId: string;
  readonly listId: string;
  readonly capolistaId: string;
  readonly listPercentage: Percentage;
}
export type PreferenceRanking = RankingBase & (
  | { readonly status: 'missing-preferences'; readonly reason: string }
  | { readonly status: 'complete'; readonly candidates: readonly {
      readonly candidateId: string;
      readonly listPosition: number;
      readonly preferences: number;
      readonly percentage: Percentage;
    }[] }
);

/**
 * C77(1)(d),(e-bis), S16(1)(d),(e-bis), C93-septies(1)(e),(g):
 * "escluso il capolista"; "A parità ... prevale l'ordine di presentazione".
 * All valid college votes form the percentage denominator, not list votes or
 * summed preferences. For Camera TAA pass the constituency figures view.
 * Filed unavailable candidates remain in the ranking; election/recovery is later.
 */
export function rankPreferenceCandidates(slate: OrdinarySlate, figures: ElectoralFigures): PreferenceRanking {
  const list = figures.lists.find(row => row.listId === slate.listId);
  if (!list || !slate.candidateIds.length) throw new RangeError('Ranking requires the filed slate and its list figure');
  const base: RankingBase = { collegeId: slate.collegeId, listId: slate.listId,
    capolistaId: slate.candidateIds[0], listPercentage: structuredClone(list.percentage) };
  if (slate.preferences.kind === 'missing') return { ...base, status: 'missing-preferences', reason: slate.preferences.reason };
  const eligible = slate.candidateIds.slice(1);
  const counts = new Map(slate.preferences.counts.map(row => [row.candidateId, row.count]));
  if (counts.size !== slate.preferences.counts.length || counts.size !== eligible.length || eligible.some(id => !counts.has(id))) {
    throw new RangeError('Supply exactly the non-capolista preference counts, including zeros');
  }
  const candidates = eligible.map((id, index) => {
    const count = counts.get(id)!;
    sumVoteCounts([count]);
    return { candidateId: id, listPosition: index + 2, preferences: count,
      percentage: calculatePercentages([count], figures.validVotes)[0] };
  }).sort((a, b) => b.preferences - a.preferences || a.listPosition - b.listPosition);
  return { ...base, status: 'complete', candidates };
}
