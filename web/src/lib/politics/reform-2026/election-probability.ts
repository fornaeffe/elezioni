import type { ResultTable } from '../../core/types';
import type { ReformScrutinyInput } from './input';
import type { ComposedReformScrutinyResult } from './scrutiny';
import type { ReformGenerationSnapshot } from './generation-types';
import { rankPreferenceCandidates } from './rankings';

export const positionTable = 'Reform college election positions v1';
export const positionColumns = ['Capolista', ...Array.from({ length: 6 }, (_, i) => `Preference ${i + 1}`)];
export const electionPositionColumns = ['Simulation', 'Chamber', 'List', 'College', 'Name', ...positionColumns];
type Row = ResultTable['rows'][number];

/** Position means rank within this run, not a fixed person's identity. Count
 * only the final election location, including recoveries into this slate. */
export function electionPositionRows(input: ReformScrutinyInput, result: ComposedReformScrutinyResult, snapshot: ReformGenerationSnapshot): Row[] {
  const sources = input.renewal.kind === 'joint' ? [input.renewal.camera, input.renewal.senato] : [input.renewal.chamber];
  return result.chambers.flatMap(chamber => {
    const source = sources.find(c => c.chamber === chamber.chamber)!;
    const seats = result.seats.filter(s => s.entitlement.chamber === chamber.chamber);
    const complete = chamber.recovery !== undefined && chamber.recovery.status !== 'non-computable' && seats.every(s => s.status !== 'unresolved');
    const locations = new Map<string, Set<string>>();
    for (const seat of seats) if (seat.status === 'filled') {
      const key = JSON.stringify([seat.electedAt.collegeId, seat.electedAt.listId]);
      const candidates = locations.get(key) ?? new Set<string>();
      candidates.add(seat.candidateId); locations.set(key, candidates);
    }
    const collegeFigures = new Map([...chamber.figures?.colleges ?? [], ...chamber.figures?.taaProportional ?? []].map(f => [f.collegeId, f.figures]));
    return source.ordinarySlates.map(slate => {
      const figures = collegeFigures.get(slate.collegeId);
      const ranking = figures ? rankPreferenceCandidates(slate, figures) : null;
      const ranked = [slate.candidateIds[0], ...(ranking?.status === 'complete' ? ranking.candidates.map(c => c.candidateId) : [])];
      const elected = locations.get(JSON.stringify([slate.collegeId, slate.listId]));
      const row: Row = { Simulation: input.simulationId, Chamber: source.chamber, List: slate.listId, College: slate.collegeId,
        Name: snapshot.seats.geography.colleges.find(c => c.chamber === source.chamber && c.id === slate.collegeId)?.name ?? slate.collegeId };
      positionColumns.forEach((column, i) => {
        row[column] = i >= slate.candidateIds.length ? 'absent' : ranked[i] && elected?.has(ranked[i]) ? 1 : ranked[i] && complete ? 0 : null;
      });
      return row;
    });
  });
}

export interface ProbabilityCell { wins: number; known: number; absent: number }
export interface CollegeProbability { id: string; name: string; positions: ProbabilityCell[] }
export interface PositionProbabilities { available: boolean; requested: number; rows: CollegeProbability[] }
export function positionAccumulator(requested: number) {
  const colleges = new Map<string, CollegeProbability>();
  const last = new Map<string, number>();
  return {
    add(row: Row) {
      const id = String(row.College), college = colleges.get(id) ?? { id, name: String(row.Name), positions: positionColumns.map(() => ({ wins: 0, known: 0, absent: 0 })) };
      const simulation = Number(row.Simulation);
      if (!Number.isSafeInteger(simulation) || simulation <= (last.get(id) ?? 0) || simulation > requested) throw new Error('Duplicate, unordered or invalid election position observation');
      last.set(id, simulation);
      positionColumns.forEach((column, i) => {
        const value = row[column], cell = college.positions[i];
        if (value === 0 || value === 1) { cell.known++; cell.wins += value; }
        else if (value === 'absent') cell.absent++;
        else if (value !== null) throw new Error('Invalid election position outcome');
      });
      colleges.set(id, college);
    },
    rows: () => [...colleges.values()].sort((a, b) => a.name.localeCompare(b.name, 'it'))
  };
}

export function probabilityLabel(cell: ProbabilityCell, requested: number): string {
  if (cell.absent === requested) return '—';
  if (!cell.known) return 'n.d.';
  return `${cell.known + cell.absent < requested ? '≥ ' : ''}${(100 * cell.wins / requested).toLocaleString('it-IT', { maximumFractionDigits: 1 })}%`;
}
