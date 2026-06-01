import type { CandidatoUniInputRow, CandidatoUniResultRow, PoliticsScrutinyInput } from './types';

function compareAscending(left: string | number, right: string | number): number {
  if (left === right) return 0;
  return left < right ? -1 : 1;
}

function compareDescending(left: string | number, right: string | number): number {
  return -compareAscending(left, right);
}

function projectUninominalResult(row: CandidatoUniInputRow, elected: boolean): CandidatoUniResultRow {
  return {
    CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
    COLLEGIOPLURINOMINALE: row.COLLEGIOPLURINOMINALE,
    COLLEGIOUNINOMINALE: row.COLLEGIOUNINOMINALE,
    CANDIDATO: row.CANDIDATO,
    ELETTO: elected
  };
}

export function electUninominalCandidates(rows: readonly CandidatoUniInputRow[]): CandidatoUniResultRow[] {
  const sorted = [...rows].sort((left, right) => {
    const byCirc = compareAscending(left.CIRCOSCRIZIONE, right.CIRCOSCRIZIONE);
    if (byCirc !== 0) return byCirc;

    const byPluri = compareAscending(left.COLLEGIOPLURINOMINALE, right.COLLEGIOPLURINOMINALE);
    if (byPluri !== 0) return byPluri;

    const byUni = compareAscending(left.COLLEGIOUNINOMINALE, right.COLLEGIOUNINOMINALE);
    if (byUni !== 0) return byUni;

    const byVotes = compareDescending(left.VOTI_CANDIDATO, right.VOTI_CANDIDATO);
    if (byVotes !== 0) return byVotes;

    return compareDescending(left.DATA_NASCITA, right.DATA_NASCITA);
  });

  const electedColleges = new Set<string>();

  return sorted.map((row) => {
    const elected = !electedColleges.has(row.COLLEGIOUNINOMINALE);
    electedColleges.add(row.COLLEGIOUNINOMINALE);
    return projectUninominalResult(row, elected);
  });
}

export interface InitialPoliticsScrutinyState {
  candidati_uni: CandidatoUniResultRow[];
}

export function runInitialPoliticsScrutiny(input: PoliticsScrutinyInput): InitialPoliticsScrutinyState {
  /*
   * Camera, DPR 361/1957 art. 77; Senate, D.Lgs. 533/1993 art. 16.
   * The uninominal candidate with the most valid votes is elected; on ties, the
   * younger candidate is elected. This mirrors the first block of
   * R/politiche/scrutinio.R before proportional-list vote attribution starts.
   */
  return {
    candidati_uni: electUninominalCandidates(input.candidati_uni)
  };
}
