import { hareNiemeyer } from '$lib/core/allocation';
import { createSeededRng, type Rng } from '$lib/core/rng';
import type {
  GeneratedCandidatoPluriRow,
  GeneratedCandidatoUniRow,
  PoliticsCandidateGenerationListRow,
  PoliticsCandidateGenerationRamoSource,
  PoliticsCandidateGenerationSource,
  PoliticsCandidatePluriGenerationTemplateRow,
  PoliticsCandidateUniTemplateRow,
  PoliticsGeneratedCandidateTables,
  PoliticsGeneratedCandidates,
  Ramo
} from './types';

export type CandidateSampleValue = string | number | null;
export type CandidateSampler = <T extends CandidateSampleValue>(
  values: readonly T[],
  size: number,
  replace: boolean,
  context: Record<string, string | number | boolean | null>
) => T[];

interface NormalizedListRow extends PoliticsCandidateGenerationListRow {
  PERC_NORM: number;
}

interface WorkingUniCandidate extends PoliticsCandidateUniTemplateRow {
  CANDIDATO_ID: string;
}

interface UniPluriListRow extends WorkingUniCandidate {
  LISTA: string;
  PERC_NORM: number;
}

function roundHalfToEven(value: number): number {
  if (value < 0) return -roundHalfToEven(-value);

  const lower = Math.floor(value);
  const fraction = value - lower;
  const epsilon = Number.EPSILON * Math.max(1, Math.abs(value));

  if (Math.abs(fraction - 0.5) <= epsilon) {
    return lower % 2 === 0 ? lower : lower + 1;
  }

  return Math.round(value);
}

function validateCandidateGenerationInput(source: PoliticsCandidateGenerationSource): void {
  const fractionSum = source.frazioni_pluricandidature.reduce((total, value) => total + value, 0);
  if (Math.abs(fractionSum - 1) > 1e-12) {
    throw new Error(`La somma di frazioni_pluricandidature e ${fractionSum}`);
  }

  for (let index = 1; index < source.frazioni_pluricandidature.length; index += 1) {
    if (source.frazioni_pluricandidature[index] > source.frazioni_pluricandidature[index - 1]) {
      throw new Error('frazioni_pluricandidature non e decrescente');
    }
  }
}

function createSeededSampler(seed: string | number): { sample: CandidateSampler; rng: Rng } {
  const rng = createSeededRng(seed);

  const sample: CandidateSampler = (values, size, replace) => {
    if (!Number.isInteger(size) || size < 0) {
      throw new Error('sample size must be a non-negative integer');
    }

    if (!replace && size > values.length) {
      throw new Error('cannot sample more values than available without replacement');
    }

    const result: CandidateSampleValue[] = [];

    if (replace) {
      for (let index = 0; index < size; index += 1) {
        result.push(values[Math.floor(rng() * values.length)]);
      }
      return result as typeof values[number][];
    }

    const available = [...values];
    for (let index = 0; index < size; index += 1) {
      const selected = Math.floor(rng() * available.length);
      result.push(available[selected]);
      available.splice(selected, 1);
    }

    return result as typeof values[number][];
  };

  return { sample, rng };
}

function normalizeLists(lists: readonly PoliticsCandidateGenerationListRow[]): NormalizedListRow[] {
  const filtered = lists.filter((row) => row.LISTA !== 'astensione');
  const totalsByCoalition = new Map<string, number>();

  for (const row of filtered) {
    const key = row.COALIZIONE ?? '<NA>';
    totalsByCoalition.set(key, (totalsByCoalition.get(key) ?? 0) + row.PERCENTUALE);
  }

  return filtered.map((row) => ({
    ...row,
    PERC_NORM: row.PERCENTUALE / (totalsByCoalition.get(row.COALIZIONE ?? '<NA>') ?? 1)
  }));
}

function fillUninominalCandidates(
  rows: readonly PoliticsCandidateUniTemplateRow[],
  simulation: number
): { rows: WorkingUniCandidate[]; missingIndexes: number[] } {
  let generated = 0;
  const missingIndexes: number[] = [];

  const filledRows = rows.map((row, index) => {
    if (row.CANDIDATO_ID !== null) {
      return { ...row, CANDIDATO_ID: row.CANDIDATO_ID };
    }

    generated += 1;
    missingIndexes.push(index + 1);
    return { ...row, CANDIDATO_ID: `UNI_${simulation}_${generated}` };
  });

  return { rows: filledRows, missingIndexes };
}

function joinUninominalCandidatesToLists(
  uniPluri: readonly WorkingUniCandidate[],
  lists: readonly NormalizedListRow[]
): UniPluriListRow[] {
  const rows: UniPluriListRow[] = [];

  for (const list of lists) {
    const matching = uniPluri.filter((row) => row.COALIZIONE === list.COALIZIONE);

    if (matching.length === 0) {
      rows.push({
        COALIZIONE: list.COALIZIONE,
        UNI_COD: null as never,
        LISTA_MINORANZA: null,
        CANDIDATO_ID: null as never,
        DATA_NASCITA: null,
        LISTA: list.LISTA,
        PERC_NORM: list.PERC_NORM
      });
      continue;
    }

    for (const candidate of matching) {
      rows.push({
        ...candidate,
        LISTA: list.LISTA,
        PERC_NORM: list.PERC_NORM
      });
    }
  }

  return rows;
}

function assignUninominalCandidatesToLists(
  joinedRows: readonly UniPluriListRow[],
  sample: CandidateSampler,
  context: { ramo: Ramo; sim: number }
): UniPluriListRow[] {
  const groups = new Map<string, UniPluriListRow[]>();

  for (const row of joinedRows) {
    const key = `${row.COALIZIONE ?? '<NA>'}\u001f${row.LISTA}`;
    const rows = groups.get(key) ?? [];
    rows.push(row);
    groups.set(key, rows);
  }

  const assigned: UniPluriListRow[] = [];

  for (const rows of groups.values()) {
    const selected = sample(
      rows.map((_, index) => index + 1),
      roundHalfToEven(rows.length * rows[0].PERC_NORM),
      false,
      {
        phase: 'assign_uni_to_list',
        ramo: context.ramo,
        sim: context.sim,
        coalizione: rows[0].COALIZIONE,
        lista: rows[0].LISTA
      }
    );

    for (const position of selected) {
      assigned.push(rows[Number(position) - 1]);
    }
  }

  return assigned;
}

function uniqueLists(rows: readonly PoliticsCandidatePluriGenerationTemplateRow[]): string[] {
  const seen = new Set<string>();
  const lists: string[] = [];

  for (const row of rows) {
    if (seen.has(row.LISTA)) continue;
    seen.add(row.LISTA);
    lists.push(row.LISTA);
  }

  return lists;
}

function fillPlurinominalCandidates(
  rows: readonly PoliticsCandidatePluriGenerationTemplateRow[],
  uniPluriLists: readonly UniPluriListRow[],
  source: PoliticsCandidateGenerationSource,
  sample: CandidateSampler,
  hnRng: Rng,
  context: { ramo: Ramo; sim: number }
): GeneratedCandidatoPluriRow[] {
  const fixedRows = rows.filter((row) => row.CANDIDATO_ID !== null);
  const missingRows = rows.filter((row) => row.CANDIDATO_ID === null);
  const filledRows: GeneratedCandidatoPluriRow[] = [];

  for (const list of uniqueLists(rows)) {
    const listSlots = missingRows.filter((row) => row.LISTA === list);
    const slotCount = listSlots.length;
    if (slotCount === 0) continue;

    const fractions = hareNiemeyer(source.frazioni_pluricandidature, slotCount, hnRng);
    const availableUninominalCandidates = uniPluriLists
      .filter((row) => row.LISTA === list)
      .map((row) => row.CANDIDATO_ID);
    const candidates: Array<string | null> = [];

    const firstFraction = fractions[0];
    const uninominalToUse = Math.min(availableUninominalCandidates.length, firstFraction);

    if (uninominalToUse > 0) {
      candidates.push(
        ...sample(availableUninominalCandidates, uninominalToUse, false, {
          phase: 'fill_first_fraction_from_uni',
          ramo: context.ramo,
          sim: context.sim,
          lista: list
        })
      );
    }

    if (uninominalToUse < firstFraction) {
      for (let index = 1; index <= firstFraction - uninominalToUse; index += 1) {
        candidates.push(`PLURI_${context.sim}_${list}_${index}`);
      }
    }

    let previous: Array<string | null> =
      candidates.length > 0 ? [...candidates] : [`PLURI_${context.sim}_${list}_base`];

    for (let fraction = 2; fraction <= 5; fraction += 1) {
      const seats = fractions[fraction - 1];
      if (seats === 0) continue;

      previous = sample(previous, seats, previous.length < seats, {
        phase: 'fill_repeated_fraction',
        ramo: context.ramo,
        sim: context.sim,
        lista: list,
        fraction
      });
      candidates.push(...previous);
    }

    for (let index = 0; index < listSlots.length; index += 1) {
      const row = listSlots[index];
      filledRows.push({
        SIM: context.sim,
        LISTA: row.LISTA,
        PLURI_COD: row.PLURI_COD,
        NUMERO_CANDIDATO: row.NUMERO_CANDIDATO,
        MINORANZA: row.MINORANZA,
        CANDIDATO_ID: candidates[index],
        DATA_NASCITA: row.DATA_NASCITA ?? source.default_data_nascita
      });
    }
  }

  return [
    ...fixedRows.map((row) => ({
      SIM: context.sim,
      LISTA: row.LISTA,
      PLURI_COD: row.PLURI_COD,
      NUMERO_CANDIDATO: row.NUMERO_CANDIDATO,
      MINORANZA: row.MINORANZA,
      CANDIDATO_ID: row.CANDIDATO_ID as string,
      DATA_NASCITA: row.DATA_NASCITA ?? source.default_data_nascita
    })),
    ...filledRows
  ];
}

function generateRamoCandidates(
  ramo: Ramo,
  source: PoliticsCandidateGenerationSource,
  ramoSource: PoliticsCandidateGenerationRamoSource,
  sample: CandidateSampler,
  hnRng: Rng
): PoliticsGeneratedCandidateTables {
  const normalizedLists = normalizeLists(source.liste);
  const outUni: GeneratedCandidatoUniRow[] = [];
  const outPluri: GeneratedCandidatoPluriRow[] = [];

  for (let simulation = 1; simulation <= source.simulazioni; simulation += 1) {
    const { rows: uninominalRows, missingIndexes } = fillUninominalCandidates(ramoSource.candidati_uni, simulation);
    const uninominalForPlurinominalCount = Math.floor(missingIndexes.length * source.frazione_uni_in_pluri);
    const selectedIndexes =
      uninominalForPlurinominalCount > 0
        ? sample(missingIndexes, uninominalForPlurinominalCount, false, {
            phase: 'select_uni_for_pluri',
            ramo,
            sim: simulation
          })
        : [];
    const uninominalForPlurinominal = selectedIndexes.map((index) => uninominalRows[Number(index) - 1]);
    const uniPluriLists = assignUninominalCandidatesToLists(
      joinUninominalCandidatesToLists(uninominalForPlurinominal, normalizedLists),
      sample,
      { ramo, sim: simulation }
    );

    outUni.push(
      ...uninominalRows.map((row) => ({
        SIM: simulation,
        COALIZIONE: row.COALIZIONE,
        UNI_COD: row.UNI_COD,
        LISTA_MINORANZA: row.LISTA_MINORANZA,
        CANDIDATO_ID: row.CANDIDATO_ID,
        DATA_NASCITA: row.DATA_NASCITA ?? source.default_data_nascita
      }))
    );

    outPluri.push(
      ...fillPlurinominalCandidates(ramoSource.candidati_pluri, uniPluriLists, source, sample, hnRng, {
        ramo,
        sim: simulation
      })
    );
  }

  return {
    candidati_uni_sim: outUni,
    candidati_pluri_sim: outPluri
  };
}

export function generatePoliticsCandidates(
  source: PoliticsCandidateGenerationSource,
  options: { sample?: CandidateSampler; seed?: string | number } = {}
): PoliticsGeneratedCandidates {
  validateCandidateGenerationInput(source);
  const seeded = createSeededSampler(options.seed ?? 'politics-candidate-generation-default');
  const sample = options.sample ?? seeded.sample;

  return {
    camera: generateRamoCandidates('camera', source, source.camera, sample, seeded.rng),
    senato: generateRamoCandidates('senato', source, source.senato, sample, seeded.rng)
  };
}
