import type {
  AdministrativeCode,
  GeneratedCandidatoUniRow,
  GeneratedCandidatoUniVotesRow,
  GeneratedUniListVotesRow,
  PoliticsCandidatePluriTemplateRow,
  PoliticsVotePreparationSource,
  PreparedPoliticsVoteTables
} from './types';

interface EnrichedUniListVotesRow extends GeneratedUniListVotesRow {
  PLURI_COD: AdministrativeCode | null;
  CIRC_COD: AdministrativeCode;
  COALIZIONE: string | null;
  CANDIDATO_ID: string | null;
  CAND_MINORANZA: boolean | null;
}

interface CandidateVoteTotal {
  SIM: number;
  UNI_COD: AdministrativeCode;
  COALIZIONE: string | null;
  CANDIDATO_ID: string | null;
  VOTI_CANDIDATO: number;
}

function codeKey(code: AdministrativeCode | string | null | undefined): string {
  return code === null || code === undefined ? '<NA>' : String(code);
}

function joinKey(...parts: (AdministrativeCode | string | null | undefined)[]): string {
  return parts.map(codeKey).join('\u001f');
}

function requireCode(value: AdministrativeCode | null | undefined, field: string, sourceRow: object): AdministrativeCode {
  if (value === null || value === undefined) {
    throw new Error(`Missing ${field} while preparing politics vote tables: ${JSON.stringify(sourceRow)}`);
  }

  return value;
}

function requireNullableCode(
  value: AdministrativeCode | null | undefined,
  field: string,
  sourceRow: object
): AdministrativeCode | null {
  if (value === undefined) {
    throw new Error(`Missing ${field} while preparing politics vote tables: ${JSON.stringify(sourceRow)}`);
  }

  return value;
}

function uniqueValidPlurinominalRows(
  rows: readonly PoliticsCandidatePluriTemplateRow[]
): PoliticsCandidatePluriTemplateRow[] {
  const seen = new Set<string>();
  const validRows: PoliticsCandidatePluriTemplateRow[] = [];

  for (const row of rows) {
    const key = joinKey(row.CIRC_COD, row.PLURI_COD, row.LISTA, String(row.MINORANZA));
    if (seen.has(key)) continue;

    seen.add(key);
    validRows.push(row);
  }

  return validRows;
}

function enrichListVotes(source: PoliticsVotePreparationSource): EnrichedUniListVotesRow[] {
  const uniByCode = new Map(source.uni.map((row) => [codeKey(row.UNI_COD), row]));
  const coalitionByList = new Map(source.liste.map((row) => [row.LISTA, row.COALIZIONE]));
  const candidateBySimUniCoalition = new Map(
    source.candidati_uni_sim.map((row) => [joinKey(row.SIM, row.UNI_COD, row.COALIZIONE), row])
  );

  return source.uni_liste_sim
    .filter((row) => row.LISTA !== 'astensione')
    .map((row) => {
      const uni = uniByCode.get(codeKey(row.UNI_COD));
      const coalition = coalitionByList.get(row.LISTA) ?? null;
      const candidate = candidateBySimUniCoalition.get(joinKey(row.SIM, row.UNI_COD, coalition));

      return {
        SIM: row.SIM,
        UNI_COD: row.UNI_COD,
        LISTA: row.LISTA,
        VOTI_LISTA_SIM: row.VOTI_LISTA_SIM,
        PLURI_COD: requireNullableCode(uni?.PLURI_COD, 'PLURI_COD', row),
        CIRC_COD: requireCode(uni?.CIRC_COD, 'CIRC_COD', row),
        COALIZIONE: coalition,
        CANDIDATO_ID: candidate?.CANDIDATO_ID ?? null,
        CAND_MINORANZA: candidate === undefined ? null : false
      };
    });
}

function aggregateCandidateVotes(rows: readonly EnrichedUniListVotesRow[]): CandidateVoteTotal[] {
  const totals = new Map<string, CandidateVoteTotal>();

  for (const row of rows) {
    const key = joinKey(row.SIM, row.UNI_COD, row.COALIZIONE, row.CANDIDATO_ID);
    const existing = totals.get(key);

    if (existing) {
      existing.VOTI_CANDIDATO += row.VOTI_LISTA_SIM;
    } else {
      totals.set(key, {
        SIM: row.SIM,
        UNI_COD: row.UNI_COD,
        COALIZIONE: row.COALIZIONE,
        CANDIDATO_ID: row.CANDIDATO_ID,
        VOTI_CANDIDATO: row.VOTI_LISTA_SIM
      });
    }
  }

  return [...totals.values()];
}

function prepareCandidateVotes(
  sourceCandidates: readonly GeneratedCandidatoUniRow[],
  voteTotals: readonly CandidateVoteTotal[]
): GeneratedCandidatoUniVotesRow[] {
  const candidatesByKey = new Map(
    sourceCandidates.map((row) => [joinKey(row.SIM, row.UNI_COD, row.COALIZIONE, row.CANDIDATO_ID), row])
  );
  const rows: GeneratedCandidatoUniVotesRow[] = [];

  for (const total of voteTotals) {
    if (total.CANDIDATO_ID === null) continue;

    const candidate = candidatesByKey.get(joinKey(total.SIM, total.UNI_COD, total.COALIZIONE, total.CANDIDATO_ID));
    if (!candidate) continue;

    rows.push({
      SIM: candidate.SIM,
      COALIZIONE: candidate.COALIZIONE,
      UNI_COD: candidate.UNI_COD,
      LISTA_MINORANZA: candidate.LISTA_MINORANZA,
      CANDIDATO_ID: candidate.CANDIDATO_ID,
      DATA_NASCITA: candidate.DATA_NASCITA,
      PLURI_COD: candidate.PLURI_COD,
      CIRC_COD: candidate.CIRC_COD,
      VOTI_CANDIDATO: total.VOTI_CANDIDATO
    });
  }

  return rows;
}

function prepareListVotes(
  enrichedRows: readonly EnrichedUniListVotesRow[],
  validRows: readonly PoliticsCandidatePluriTemplateRow[]
): GeneratedUniListVotesRow[] {
  const rowsByValidKey = new Map<string, EnrichedUniListVotesRow[]>();

  for (const row of enrichedRows) {
    const key = joinKey(row.CIRC_COD, row.PLURI_COD, row.LISTA);
    const rows = rowsByValidKey.get(key) ?? [];
    rows.push(row);
    rowsByValidKey.set(key, rows);
  }

  const prepared: GeneratedUniListVotesRow[] = [];

  for (const valid of validRows) {
    const rows = rowsByValidKey.get(joinKey(valid.CIRC_COD, valid.PLURI_COD, valid.LISTA)) ?? [];

    for (const row of rows) {
      prepared.push({
        SIM: row.SIM,
        UNI_COD: row.UNI_COD,
        LISTA: row.LISTA,
        VOTI_LISTA_SIM: row.VOTI_LISTA_SIM,
        PLURI_COD: row.PLURI_COD,
        CIRC_COD: row.CIRC_COD,
        COALIZIONE: row.COALIZIONE,
        CANDIDATO_ID: row.CANDIDATO_ID,
        CAND_MINORANZA: row.CAND_MINORANZA,
        MINORANZA: valid.MINORANZA
      });
    }
  }

  return prepared;
}

export function preparePoliticsVoteTables(source: PoliticsVotePreparationSource): PreparedPoliticsVoteTables {
  const enrichedListVotes = enrichListVotes(source);
  const candidateVoteTotals = aggregateCandidateVotes(enrichedListVotes);

  return {
    uni_liste_sim: prepareListVotes(enrichedListVotes, uniqueValidPlurinominalRows(source.candidati_pluri_template)),
    candidati_uni_sim: prepareCandidateVotes(source.candidati_uni_sim, candidateVoteTotals)
  };
}
