import { storedRows, type StoredResult } from '../../core/result-store';
import type { ResultTable } from '../../core/types';
import { positionAccumulator, positionTable } from './election-probability';
import { ensureStatistics } from './stored-statistics';
import type { SeriesSummary } from '../../results/statistics';

async function* rows(record: StoredResult, name: string) {
  const index = record.result.storage!.tables.findIndex(t => t.name === name);
  if (index < 0) return;
  const columns = record.result.storage!.tables[index].columns;
  for await (const cells of storedRows(record.id, index)) yield Object.fromEntries(columns.map((c, i) => [c, cells[i]])) as ResultTable['rows'][number];
}
type StoredRow = ResultTable['rows'][number];
export interface BonusPairProbability {
  id: string;
  name: string;
  color: string;
  wins: number;
  known: number;
  absent: number;
}

const requiredText = (row: StoredRow, column: string): string => {
  const value = row[column];
  if (typeof value !== 'string') throw new Error(`Invalid ${column} in stored bonus probabilities`);
  return value;
};
const requiredSimulation = (row: StoredRow): number => {
  const value = row.Simulation;
  if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < 1) throw new Error('Invalid simulation in stored bonus probabilities');
  return value;
};

/** Counts the joint award once per simulation, preserving its linked chamber pair. */
export function aggregateBonusPairProbabilities(series: readonly SeriesSummary[], runs: readonly StoredRow[],
  eligibility: readonly StoredRow[]): BonusPairProbability[] {
  const knownSimulations = new Set<number>(), awardedSimulations = new Set<number>();
  for (const row of runs) {
    const simulation = requiredSimulation(row), status = requiredText(row, 'Bonus');
    if (knownSimulations.has(simulation)) throw new Error('Duplicate simulation in stored bonus probabilities');
    if (status === 'awarded' || status === 'not-awarded') knownSimulations.add(simulation);
    if (status === 'awarded') awardedSimulations.add(simulation);
  }

  const eligibilityBySimulation = new Map<number, Map<string, string>>();
  for (const row of eligibility) {
    const simulation = requiredSimulation(row), status = requiredText(row, 'Status');
    if (status !== 'awarded') continue;
    if (!awardedSimulations.has(simulation)) throw new Error('Bonus eligibility is awarded for a simulation without an awarded bonus');
    const chamber = requiredText(row, 'Chamber');
    if (chamber !== 'camera' && chamber !== 'senato') throw new Error('Invalid chamber in stored bonus probabilities');
    const subjects = eligibilityBySimulation.get(simulation) ?? new Map<string, string>();
    if (subjects.has(chamber)) throw new Error('Duplicate chamber in stored bonus eligibility');
    subjects.set(chamber, requiredText(row, 'Subject'));
    eligibilityBySimulation.set(simulation, subjects);
  }

  const descriptors = new Map(series.filter(s => s.kind === 'subject').map(s => [`${s.chamber}:${s.owner}`, s]));
  const pairs = new Map<string, BonusPairProbability>();
  for (const simulation of awardedSimulations) {
    const subjects = eligibilityBySimulation.get(simulation);
    if (!subjects || subjects.size !== 2 || !subjects.has('camera') || !subjects.has('senato')) {
      throw new Error('Awarded simulation is missing its Camera/Senato bonus pair');
    }
    const cameraOwner = subjects.get('camera')!, senateOwner = subjects.get('senato')!;
    const camera = descriptors.get(`camera:${cameraOwner}`), senate = descriptors.get(`senato:${senateOwner}`);
    if (!camera || !senate) throw new Error('Bonus pair has no matching subject series');
    const id = JSON.stringify([cameraOwner, senateOwner]);
    const name = camera.name === senate.name ? camera.name : `${camera.name} ↔ ${senate.name}`;
    const pair = pairs.get(id) ?? { id, name, color: camera.color, wins: 0, known: knownSimulations.size, absent: 0 };
    pair.wins++;
    pairs.set(id, pair);
  }
  return [...pairs.values()].sort((a, b) => b.wins - a.wins || a.name.localeCompare(b.name, 'it'));
}

export async function storedPositionProbabilities(record: StoredResult, chamber: string, list: string) {
  const accumulator = positionAccumulator(record.result.benchmark.simulations);
  for await (const row of rows(record, positionTable)) if (row.Chamber === chamber && row.List === list) accumulator.add(row);
  return { available: record.result.storage!.tables.some(t => t.name === positionTable), requested: record.result.benchmark.simulations, rows: accumulator.rows() };
}
export async function storedBonusProbabilities(record: StoredResult) {
  const { overview } = await ensureStatistics(record);
  // Reform runs own the election-wide decision; eligibility supplies its two linked subjects.
  const runs: StoredRow[] = [], eligibility: StoredRow[] = [];
  for await (const row of rows(record, 'Reform runs')) runs.push(row);
  for await (const row of rows(record, 'Bonus eligibility')) eligibility.push(row);
  return aggregateBonusPairProbabilities(overview.series, runs, eligibility);
}
