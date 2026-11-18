import { createNormalSampler, createSeededRng, type NormalSampler } from './rng';

export type VoteGenerationValue = string | number | boolean | null;

export interface VoteGenerationListRow extends Record<string, VoteGenerationValue> {
  LISTA: string;
  DATA: string;
  LOGIT_P: number;
  SIGMA_GLOBAL: number;
}

export interface VoteGenerationLocalRow extends Record<string, VoteGenerationValue> {
  LISTA: string;
  DATA: string;
  DELTA: number;
  SIGMA_DELTA: number;
  ELETTORI: number;
}

export interface VoteGenerationOutputRow extends VoteGenerationLocalRow {
  SIM: number;
  PERCENTUALE_SIM: number;
  VOTI_LISTA_SIM: number;
}

export interface VoteGenerationOptions {
  electionDate: string;
  simulations: number;
  localityColumn?: string;
  normal?: NormalSampler;
  seed?: string | number;
}

interface ExpandedLocalRow extends VoteGenerationLocalRow {
  SIM: number;
  DT: number;
}

interface GlobalDrawRow {
  LISTA: string;
  SIM: number;
  LOGIT_P_SIM_GLOBAL: number;
}

interface JoinedLocalRow extends ExpandedLocalRow {
  LOGIT_P_SIM_GLOBAL: number;
  DELTA_SIM: number;
  LOGIT_P_SIM: number;
  p: number;
  PERCENTUALE_SIM: number;
  VOTI_LISTA_SIM: number;
}

function daysBetween(leftIso: string, rightIso: string): number {
  const left = Date.parse(leftIso);
  const right = Date.parse(rightIso);

  if (!Number.isFinite(left) || !Number.isFinite(right)) {
    throw new Error(`Invalid date in vote generation: ${leftIso} / ${rightIso}`);
  }

  return (left - right) / 86_400_000;
}

function temporalDistance(electionDate: string, referenceDate: string): number {
  const days = daysBetween(electionDate, referenceDate);
  if (days < 0) {
    throw new Error(`Vote generation reference date is after election date: ${referenceDate}`);
  }

  return Math.sqrt(days);
}

function logistic(value: number): number {
  return 1 / (1 + Math.exp(-value));
}

function rRound(value: number): number {
  if (!Number.isFinite(value)) return value;
  if (value < 0) return -rRound(-value);

  const lower = Math.floor(value);
  const fraction = value - lower;
  const epsilon = Number.EPSILON * Math.max(1, Math.abs(value));

  if (Math.abs(fraction - 0.5) <= epsilon) {
    return lower % 2 === 0 ? lower : lower + 1;
  }

  return Math.round(value);
}

function rowKey(list: string, simulation: number): string {
  return `${list}\u001f${simulation}`;
}

function groupKey(row: JoinedLocalRow, localityColumn: string): string {
  const locality = row[localityColumn];
  if (locality === undefined || locality === null) {
    throw new Error(`Missing locality column ${localityColumn} while generating votes`);
  }

  return `${row.SIM}\u001f${String(locality)}`;
}

function defaultNormal(options: VoteGenerationOptions): NormalSampler {
  return createNormalSampler(createSeededRng(options.seed ?? 'vote-generation-default'));
}

export function generateVotes(
  localRows: readonly VoteGenerationLocalRow[],
  listRows: readonly VoteGenerationListRow[],
  options: VoteGenerationOptions
): VoteGenerationOutputRow[] {
  if (!Number.isInteger(options.simulations) || options.simulations < 1) {
    throw new Error('simulations must be a positive integer');
  }

  const localityColumn = options.localityColumn ?? 'CODICE_COMUNE';
  const normal = options.normal ?? defaultNormal(options);

  const expandedRows: ExpandedLocalRow[] = [];
  for (let simulation = 1; simulation <= options.simulations; simulation += 1) {
    for (const row of localRows) {
      expandedRows.push({
        ...row,
        SIM: simulation,
        DT: temporalDistance(options.electionDate, row.DATA)
      });
    }
  }

  const globalRows: GlobalDrawRow[] = [];
  for (const row of listRows) {
    const standardDeviation = row.SIGMA_GLOBAL * temporalDistance(options.electionDate, row.DATA);
    for (let simulation = 1; simulation <= options.simulations; simulation += 1) {
      globalRows.push({
        LISTA: row.LISTA,
        SIM: simulation,
        LOGIT_P_SIM_GLOBAL: normal(row.LOGIT_P, standardDeviation)
      });
    }
  }

  const expandedByListSimulation = new Map<string, ExpandedLocalRow[]>();
  for (const row of expandedRows) {
    const key = rowKey(row.LISTA, row.SIM);
    const rows = expandedByListSimulation.get(key) ?? [];
    rows.push(row);
    expandedByListSimulation.set(key, rows);
  }

  const joinedRows: JoinedLocalRow[] = [];
  for (const globalRow of globalRows) {
    const rows = expandedByListSimulation.get(rowKey(globalRow.LISTA, globalRow.SIM));
    if (!rows) continue;

    for (const row of rows) {
      const localStandardDeviation = row.SIGMA_DELTA * row.DT;
      const deltaSimulation = normal(row.DELTA, localStandardDeviation);
      const simulatedLogit = globalRow.LOGIT_P_SIM_GLOBAL + deltaSimulation;

      joinedRows.push({
        ...row,
        LOGIT_P_SIM_GLOBAL: globalRow.LOGIT_P_SIM_GLOBAL,
        DELTA_SIM: deltaSimulation,
        LOGIT_P_SIM: simulatedLogit,
        p: logistic(simulatedLogit),
        PERCENTUALE_SIM: 0,
        VOTI_LISTA_SIM: 0
      });
    }
  }

  const groupTotals = new Map<string, number>();
  for (const row of joinedRows) {
    const key = groupKey(row, localityColumn);
    groupTotals.set(key, (groupTotals.get(key) ?? 0) + row.p);
  }

  return joinedRows.map((row) => {
    const total = groupTotals.get(groupKey(row, localityColumn));
    if (total === undefined || total === 0) {
      throw new Error(`Cannot normalize generated votes for ${localityColumn}`);
    }

    const percentage = row.p / total;
    const { DT: _dt, LOGIT_P_SIM_GLOBAL: _global, DELTA_SIM: _delta, LOGIT_P_SIM: _logit, p: _p, ...output } = row;

    return {
      ...output,
      PERCENTUALE_SIM: percentage,
      VOTI_LISTA_SIM: rRound(percentage * row.ELETTORI)
    };
  });
}
