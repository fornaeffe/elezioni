import type { ResultTable } from '../../core/types';
import type { ReformScrutinyInput } from './input';
import type { ComposedReformScrutinyResult } from './scrutiny';
import type { ReformAssemblyContext } from './generation-types';
import { rankPreferenceCandidates } from './rankings';

export const positionTable = 'Reform college election positions v1';
export const positionColumns = ['Capolista', ...Array.from({ length: 6 }, (_, i) => `Preference ${i + 1}`)];
export const electionPositionColumns = ['Simulation', 'Chamber', 'List', 'College', 'Name', ...positionColumns];
type Row = ResultTable['rows'][number];

/** Position means rank within this run, not a fixed person's identity. Count
 * only the final election location, including recoveries into this slate. */
export function electionPositionRows(input: ReformScrutinyInput, result: ComposedReformScrutinyResult, snapshot: ReformAssemblyContext): Row[] {
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

interface RgbColor { red: number; green: number; blue: number }

const faintColorContrast = 3;

function parseHexColor(color: string): RgbColor {
  const match = /^#([0-9a-f]{6})$/i.exec(color);
  if (!match) throw new Error(`Invalid election probability color: ${color}`);
  const value = Number.parseInt(match[1], 16);
  return { red: value >> 16, green: (value >> 8) & 0xff, blue: value & 0xff };
}

function toHexColor({ red, green, blue }: RgbColor): string {
  return `#${[red, green, blue].map(channel => channel.toString(16).padStart(2, '0')).join('')}`;
}

function linearChannel(channel: number): number {
  const value = channel / 255;
  return value <= 0.04045 ? value / 12.92 : ((value + 0.055) / 1.055) ** 2.4;
}

function luminance(color: RgbColor): number {
  return 0.2126 * linearChannel(color.red) + 0.7152 * linearChannel(color.green) + 0.0722 * linearChannel(color.blue);
}

function contrast(first: RgbColor, second: RgbColor): number {
  const [lighter, darker] = [luminance(first), luminance(second)].sort((a, b) => b - a);
  return (lighter + 0.05) / (darker + 0.05);
}

function darkenFaintColor(color: RgbColor): RgbColor {
  const white = { red: 255, green: 255, blue: 255 };
  if (contrast(color, white) >= faintColorContrast) return color;

  let low = 0, high = 1;
  for (let i = 0; i < 24; i++) {
    const scale = (low + high) / 2;
    const candidate = { red: Math.floor(color.red * scale), green: Math.floor(color.green * scale), blue: Math.floor(color.blue * scale) };
    if (contrast(candidate, white) >= faintColorContrast) low = scale;
    else high = scale;
  }
  return { red: Math.floor(color.red * low), green: Math.floor(color.green * low), blue: Math.floor(color.blue * low) };
}

export function probabilityCellColors(color: string, probability: number): { background: string; foreground: string } {
  if (!Number.isFinite(probability)) throw new Error('Invalid election probability');
  const target = darkenFaintColor(parseHexColor(color));
  const amount = Math.max(0, Math.min(1, probability));
  const background = {
    red: Math.round(255 + (target.red - 255) * amount),
    green: Math.round(255 + (target.green - 255) * amount),
    blue: Math.round(255 + (target.blue - 255) * amount)
  };
  const black = { red: 0, green: 0, blue: 0 };
  const white = { red: 255, green: 255, blue: 255 };
  return {
    background: toHexColor(background),
    foreground: contrast(background, black) >= contrast(background, white) ? '#000000' : '#ffffff'
  };
}

type PositionOutcome = 0 | 1 | null | 'absent';

/** Literal OR over final local winners, without attributing a cause to skips.
 * Keep threshold evidence separate from the exact last position: a later
 * unknown can obscure the latter without erasing a confirmed earlier reach. */
export function slatePositionOutcomes(row: Row): { reached: PositionOutcome[]; last: number | null } {
  const values = positionColumns.map(column => {
    const value = row[column];
    if (value !== 0 && value !== 1 && value !== null && value !== 'absent') throw new Error('Invalid election position outcome');
    return value;
  });
  const reached: PositionOutcome[] = [];
  let hasWinner = false, hasUnknown = false;
  let last: number | null = 0;
  let absent = false;
  for (const value of values) {
    if (value === 'absent') absent = true;
    else if (absent) throw new Error('Absent election positions must form a trailing suffix');
  }
  for (let i = values.length - 1; i >= 0; i--) {
    const value = values[i];
    if (value === 'absent') { reached[i] = 'absent'; continue; }
    if (last === 0 && value === null) last = null;
    if (last === 0 && value === 1) last = i + 1;
    hasWinner ||= value === 1;
    hasUnknown ||= value === null;
    reached[i] = hasWinner ? 1 : hasUnknown ? null : 0;
  }
  return { reached, last };
}

export function positionAccumulator(requested: number) {
  const colleges = new Map<string, CollegeProbability>();
  const last = new Map<string, number>();
  return {
    add(row: Row) {
      const id = String(row.College), college = colleges.get(id) ?? { id, name: String(row.Name), positions: positionColumns.map(() => ({ wins: 0, known: 0, absent: 0 })) };
      const simulation = Number(row.Simulation);
      if (!Number.isSafeInteger(simulation) || simulation <= (last.get(id) ?? 0) || simulation > requested) throw new Error('Duplicate, unordered or invalid election position observation');
      const { reached } = slatePositionOutcomes(row);
      last.set(id, simulation);
      positionColumns.forEach((column, i) => {
        const value = reached[i], cell = college.positions[i];
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
