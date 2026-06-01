import type {
  AdministrativeCode,
  CoalCircCifreTraceRow,
  CoalNazSoglieTraceRow,
  CandidatoUniAttributionTraceRow,
  CandidatoUniGraduatoriaTraceRow,
  CandidatoUniInputRow,
  CandidatoUniResultRow,
  ListaNazRow,
  ListeCircSoglieTraceRow,
  ListeCircCifreTraceRow,
  ListeNazSoglieTraceRow,
  ListePluriCifreTraceRow,
  ListeUniCifreTraceRow,
  ListeUniRow,
  PoliticsEarlyTrace,
  PoliticsScrutinyContext,
  PoliticsScrutinyInput,
  PoliticsScrutinyTrace,
  TraceBoolean,
  TotaliCircTraceRow
} from './types';

interface CandidatoUniElectionWorkingRow extends CandidatoUniInputRow {
  ELETTO: boolean;
}

interface CandidatoUniAttributionWorkingRow extends CandidatoUniElectionWorkingRow {
  VOTI_LISTA: number;
  VOTI_SOLO_CANDIDATO: number;
  QUOZIENTE: number;
  PARTE_INTERA: number;
  DA_ASSEGNARE: number;
}

interface ListeUniCifreWorkingRow extends ListeUniRow {
  ELETTO: boolean;
  QUOZIENTE: number;
  PARTE_INTERA: number;
  RESTO: number;
  DA_ASSEGNARE: number;
  ORDINE: number;
  VOTO_DA_RESTO: number;
  CIFRA: number;
}

type GroupedRow<T> = T & { CIFRA: number };

function compareAscending(left: string | number, right: string | number): number {
  if (left === right) return 0;
  return left < right ? -1 : 1;
}

function compareDescending(left: string | number, right: string | number): number {
  return -compareAscending(left, right);
}

function compareNullableNumberAscending(left: number, right: number): number {
  const leftMissing = Number.isNaN(left);
  const rightMissing = Number.isNaN(right);

  if (leftMissing && rightMissing) return 0;
  if (leftMissing) return 1;
  if (rightMissing) return -1;
  return compareAscending(left, right);
}

function keyOf(...parts: AdministrativeCode[]): string {
  return parts.map((part) => String(part)).join('\u001f');
}

function candidateKey(row: { COLLEGIOUNINOMINALE: AdministrativeCode; CANDIDATO: string }): string {
  return keyOf(row.COLLEGIOUNINOMINALE, row.CANDIDATO);
}

function collegeKey(row: { COLLEGIOUNINOMINALE: AdministrativeCode }): string {
  return keyOf(row.COLLEGIOUNINOMINALE);
}

function listKey(row: { LISTA: string }): string {
  return row.LISTA;
}

function traceNumber(value: number): number | null {
  return Number.isFinite(value) ? value : null;
}

function rIntegerDivide(left: number, right: number): number {
  const result = Math.floor(left / right);
  return result < 0 || !Number.isFinite(result) ? 0 : result;
}

function sumBy<T>(rows: readonly T[], key: (row: T) => string, value: (row: T) => number): Map<string, number> {
  const totals = new Map<string, number>();
  for (const row of rows) {
    const groupKey = key(row);
    totals.set(groupKey, (totals.get(groupKey) ?? 0) + value(row));
  }
  return totals;
}

function anyBy<T>(rows: readonly T[], key: (row: T) => string, value: (row: T) => boolean): Map<string, boolean> {
  const result = new Map<string, boolean>();
  for (const row of rows) {
    const groupKey = key(row);
    result.set(groupKey, (result.get(groupKey) ?? false) || value(row));
  }
  return result;
}

function rLogicalOr(left: boolean, right: TraceBoolean): TraceBoolean {
  if (left) return true;
  return right;
}

function rLogicalAnd(left: TraceBoolean, right: boolean): TraceBoolean {
  if (left === false) return false;
  if (left === null) return right ? null : false;
  return right;
}

function projectUninominalResult(row: CandidatoUniElectionWorkingRow): CandidatoUniResultRow {
  return {
    CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
    COLLEGIOPLURINOMINALE: row.COLLEGIOPLURINOMINALE,
    COLLEGIOUNINOMINALE: row.COLLEGIOUNINOMINALE,
    CANDIDATO: row.CANDIDATO,
    ELETTO: row.ELETTO
  };
}

function electUninominalCandidateRows(rows: readonly CandidatoUniInputRow[]): CandidatoUniElectionWorkingRow[] {
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
    const rowCollegeKey = collegeKey(row);
    const elected = !electedColleges.has(rowCollegeKey);
    electedColleges.add(rowCollegeKey);
    return { ...row, ELETTO: elected };
  });
}

export function electUninominalCandidates(rows: readonly CandidatoUniInputRow[]): CandidatoUniResultRow[] {
  return electUninominalCandidateRows(rows).map(projectUninominalResult);
}

function addCandidateListVoteAttribution(
  candidates: readonly CandidatoUniElectionWorkingRow[],
  lists: readonly ListeUniRow[]
): CandidatoUniAttributionWorkingRow[] {
  const listVotesByCandidate = sumBy(
    lists,
    (row) => candidateKey(row),
    (row) => row.VOTI_LISTA
  );

  const partialCandidates = candidates.map((candidate) => {
    const listVotes = listVotesByCandidate.get(candidateKey(candidate)) ?? 0;
    const onlyCandidateVotes = candidate.VOTI_CANDIDATO - listVotes;
    return {
      ...candidate,
      VOTI_LISTA: listVotes,
      VOTI_SOLO_CANDIDATO: onlyCandidateVotes,
      QUOZIENTE: listVotes / onlyCandidateVotes
    };
  });

  const partialCandidateByKey = new Map(partialCandidates.map((candidate) => [candidateKey(candidate), candidate]));
  const listRowsWithIntegerPart = lists.map((list) => {
    const candidate = partialCandidateByKey.get(candidateKey(list));
    if (!candidate) {
      throw new Error(`Missing uninominal candidate for list vote attribution: ${candidateKey(list)}`);
    }
    return {
      key: candidateKey(list),
      PARTE_INTERA: rIntegerDivide(list.VOTI_LISTA, candidate.QUOZIENTE)
    };
  });
  const integerPartByCandidate = sumBy(
    listRowsWithIntegerPart,
    (row) => row.key,
    (row) => row.PARTE_INTERA
  );

  return partialCandidates.map((candidate) => {
    const integerPart = integerPartByCandidate.get(candidateKey(candidate)) ?? 0;
    return {
      ...candidate,
      PARTE_INTERA: integerPart,
      DA_ASSEGNARE: candidate.VOTI_SOLO_CANDIDATO - integerPart
    };
  });
}

function buildListeUniCifre(
  lists: readonly ListeUniRow[],
  candidates: readonly CandidatoUniAttributionWorkingRow[]
): ListeUniCifreWorkingRow[] {
  const candidateByKey = new Map(candidates.map((candidate) => [candidateKey(candidate), candidate]));
  const rows = lists.map((list) => {
    const candidate = candidateByKey.get(candidateKey(list));
    if (!candidate) {
      throw new Error(`Missing uninominal candidate for list row: ${candidateKey(list)}`);
    }

    return {
      ...list,
      ELETTO: candidate.ELETTO,
      QUOZIENTE: candidate.QUOZIENTE,
      PARTE_INTERA: rIntegerDivide(list.VOTI_LISTA, candidate.QUOZIENTE),
      RESTO: list.VOTI_LISTA % candidate.QUOZIENTE,
      DA_ASSEGNARE: candidate.DA_ASSEGNARE,
      ORDINE: 0,
      VOTO_DA_RESTO: 0,
      CIFRA: 0
    };
  });

  rows.sort((left, right) => {
    const byCirc = compareAscending(left.CIRCOSCRIZIONE, right.CIRCOSCRIZIONE);
    if (byCirc !== 0) return byCirc;

    const byPluri = compareAscending(left.COLLEGIOPLURINOMINALE, right.COLLEGIOPLURINOMINALE);
    if (byPluri !== 0) return byPluri;

    const byUni = compareAscending(left.COLLEGIOUNINOMINALE, right.COLLEGIOUNINOMINALE);
    if (byUni !== 0) return byUni;

    const byCandidate = compareAscending(left.CANDIDATO, right.CANDIDATO);
    if (byCandidate !== 0) return byCandidate;

    /*
     * TODO(law-review): the law comment in R/politiche/scrutinio.R says the
     * highest remainders should receive the residual candidate-only votes, but
     * the current R order() call has six `decreasing` flags for five keys.
     * R ignores the final TRUE, so RESTO is sorted ascending. Preserve that
     * behavior for golden-master parity until Luca decides on the correction.
     */
    return compareNullableNumberAscending(left.RESTO, right.RESTO);
  });

  const orderByCandidate = new Map<string, number>();
  for (const row of rows) {
    const key = candidateKey(row);
    const order = (orderByCandidate.get(key) ?? 0) + 1;
    orderByCandidate.set(key, order);
    row.ORDINE = order;
    row.VOTO_DA_RESTO = order <= row.DA_ASSEGNARE ? 1 : 0;
    row.CIFRA = row.VOTI_LISTA + row.PARTE_INTERA + row.VOTO_DA_RESTO;
  }

  return rows;
}

function sortGroupedRows<T>(rows: T[], selectors: ((row: T) => string | number)[]): T[] {
  return rows.sort((left, right) => {
    for (const selector of selectors) {
      const compared = compareAscending(selector(left), selector(right));
      if (compared !== 0) return compared;
    }
    return 0;
  });
}

function aggregateCifra<Row extends { CIFRA: number }, T extends object>(
  rows: readonly Row[],
  group: (row: Row) => string,
  create: (row: Row) => T
): GroupedRow<T>[] {
  const grouped = new Map<string, GroupedRow<T>>();
  for (const row of rows) {
    const groupKey = group(row);
    const existing = grouped.get(groupKey);
    if (existing) {
      existing.CIFRA += row.CIFRA;
    } else {
      grouped.set(groupKey, { ...create(row), CIFRA: row.CIFRA });
    }
  }
  return [...grouped.values()];
}

function buildListePluriCifre(lists: readonly ListeUniCifreWorkingRow[]): ListePluriCifreTraceRow[] {
  const pluriRows = aggregateCifra(
    lists,
    (row) => keyOf(row.CIRCOSCRIZIONE, row.COLLEGIOPLURINOMINALE, row.LISTA),
    (row) => ({
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      COLLEGIOPLURINOMINALE: row.COLLEGIOPLURINOMINALE,
      LISTA: row.LISTA
    })
  );
  const totalsByPluri = sumBy(
    pluriRows,
    (row) => keyOf(row.COLLEGIOPLURINOMINALE),
    (row) => row.CIFRA
  );

  return sortGroupedRows(
    pluriRows.map((row) => {
      const total = totalsByPluri.get(keyOf(row.COLLEGIOPLURINOMINALE)) ?? 0;
      return {
        ...row,
        CIFRA_TOT: total,
        CIFRA_PERCENTUALE: (row.CIFRA / total) * 100
      };
    }),
    [(row) => row.COLLEGIOPLURINOMINALE, (row) => row.CIRCOSCRIZIONE, (row) => row.LISTA]
  );
}

function buildListeCircCifre(lists: readonly ListePluriCifreTraceRow[]): ListeCircCifreTraceRow[] {
  return sortGroupedRows(
    aggregateCifra(
      lists,
      (row) => keyOf(row.CIRCOSCRIZIONE, row.LISTA),
      (row) => ({
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
        LISTA: row.LISTA
      })
    ),
    [(row) => row.CIRCOSCRIZIONE, (row) => row.LISTA]
  );
}

function buildCandidatiUniGraduatoria(
  candidates: readonly CandidatoUniAttributionWorkingRow[]
): CandidatoUniGraduatoriaTraceRow[] {
  const totalVotesByCollege = sumBy(
    candidates,
    (candidate) => collegeKey(candidate),
    (candidate) => candidate.VOTI_CANDIDATO
  );

  return sortGroupedRows(
    candidates.map((candidate) => {
      const total = totalVotesByCollege.get(collegeKey(candidate)) ?? 0;
      return {
        CIRCOSCRIZIONE: candidate.CIRCOSCRIZIONE,
        COLLEGIOPLURINOMINALE: candidate.COLLEGIOPLURINOMINALE,
        COLLEGIOUNINOMINALE: candidate.COLLEGIOUNINOMINALE,
        CANDIDATO: candidate.CANDIDATO,
        ELETTO: candidate.ELETTO,
        VOTI_CANDIDATO: candidate.VOTI_CANDIDATO,
        VOTI_CANDIDATO_TOT: total,
        CIFRA_PERCENTUALE: (candidate.VOTI_CANDIDATO / total) * 100
      };
    }),
    [(row) => row.COLLEGIOUNINOMINALE, (row) => row.CANDIDATO]
  );
}

function buildTotaliCirc(lists: readonly ListeCircCifreTraceRow[]): TotaliCircTraceRow[] {
  return sortGroupedRows(
    aggregateCifra(
      lists,
      (row) => keyOf(row.CIRCOSCRIZIONE),
      (row) => ({
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE
      })
    ),
    [(row) => row.CIRCOSCRIZIONE]
  );
}

function buildListeNazSoglie(
  listeNazInput: readonly ListaNazRow[],
  listeCirc: readonly ListeCircCifreTraceRow[],
  totaliCirc: readonly TotaliCircTraceRow[],
  listeUniInput: readonly ListeUniRow[],
  candidatiUniElezione: readonly CandidatoUniResultRow[],
  ramo: PoliticsScrutinyContext['ramo']
): {
  totaleNaz: number;
  listeNaz: ListeNazSoglieTraceRow[];
  listeCirc: ListeCircSoglieTraceRow[];
  coalNaz: CoalNazSoglieTraceRow[];
  coalCirc: CoalCircCifreTraceRow[];
} {
  const nationalCifraByList = sumBy(
    listeCirc,
    (row) => row.LISTA,
    (row) => row.CIFRA
  );
  const listeNazBase = sortGroupedRows(
    listeNazInput
      .map((row) => {
        const cifra = nationalCifraByList.get(row.LISTA);
        return cifra === undefined ? null : { ...row, CIFRA: cifra };
      })
      .filter((row): row is ListaNazRow & { CIFRA: number } => row !== null),
    [(row) => row.LISTA]
  );
  const totaleNaz = listeNazBase.reduce((total, row) => total + row.CIFRA, 0);
  const totaliCircByCirc = new Map(totaliCirc.map((row) => [keyOf(row.CIRCOSCRIZIONE), row.CIFRA]));
  const electedByCandidate = new Map(candidatiUniElezione.map((row) => [candidateKey(row), row.ELETTO]));
  const hasMinorityCandidates = listeUniInput.some((row) => row.CAND_MINORANZA);
  const minorityElectedByCircList = new Map<string, number>();

  if (hasMinorityCandidates) {
    for (const row of listeUniInput) {
      if (!row.CAND_MINORANZA) continue;
      const groupKey = keyOf(row.CIRCOSCRIZIONE, row.LISTA);
      const elected = electedByCandidate.get(candidateKey(row)) ? 1 : 0;
      minorityElectedByCircList.set(groupKey, (minorityElectedByCircList.get(groupKey) ?? 0) + elected);
    }
  }

  const collegesByCirc = new Map<string, Set<string>>();
  for (const row of listeUniInput) {
    const groupKey = keyOf(row.CIRCOSCRIZIONE);
    const colleges = collegesByCirc.get(groupKey) ?? new Set<string>();
    colleges.add(keyOf(row.COLLEGIOUNINOMINALE));
    collegesByCirc.set(groupKey, colleges);
  }

  const listeCircWithThresholds = sortGroupedRows(
    listeCirc.map((row) => {
      const circKey = keyOf(row.CIRCOSCRIZIONE);
      const circTotal = totaliCircByCirc.get(circKey) ?? 0;
      const electedMinority = hasMinorityCandidates
        ? (minorityElectedByCircList.get(keyOf(row.CIRCOSCRIZIONE, row.LISTA)) ?? 0)
        : 0;
      const colleges = collegesByCirc.get(circKey)?.size ?? 0;
      const cifraPercentuale = (row.CIFRA / circTotal) * 100;

      return {
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
        LISTA: row.LISTA,
        CIFRA: row.CIFRA,
        CIFRA_TOT: circTotal,
        CIFRA_PERCENTUALE: cifraPercentuale,
        ELETTI_MINORANZA: electedMinority,
        COLLEGI_UNI: colleges,
        SOGLIA20: cifraPercentuale >= 20,
        SOGLIA_MINORANZA: electedMinority >= Math.ceil(colleges / 4)
      };
    }),
    [(row) => row.CIRCOSCRIZIONE, (row) => row.LISTA]
  );

  const soglia20ByList = anyBy(
    listeCircWithThresholds,
    (row) => row.LISTA,
    (row) => row.SOGLIA20
  );
  const sogliaMinoranzaByList = anyBy(
    listeCircWithThresholds,
    (row) => row.LISTA,
    (row) => row.SOGLIA_MINORANZA
  );

  const listeNazWithFirstThresholds = listeNazBase.map((row) => {
    const cifraPercentuale = (row.CIFRA / totaleNaz) * 100;
    const soglia20 = soglia20ByList.get(row.LISTA) ?? false;
    const sogliaMinoranza = sogliaMinoranzaByList.get(row.LISTA) ?? false;
    const soglia1M =
      cifraPercentuale >= 1 || (soglia20 && (row.MINORANZA || ramo === 'senato')) || sogliaMinoranza;
    const soglia3 = cifraPercentuale >= 3;
    const soglia3M = soglia3 || (soglia20 && (row.MINORANZA || ramo === 'senato')) || sogliaMinoranza;

    return {
      ...row,
      CIFRA_PERCENTUALE: cifraPercentuale,
      SOGLIA20: soglia20,
      SOGLIA_MINORANZA: sogliaMinoranza,
      SOGLIA1M: soglia1M,
      SOGLIA3: soglia3,
      SOGLIA3M: soglia3M
    };
  });

  const listeNazByList = new Map(listeNazWithFirstThresholds.map((row) => [row.LISTA, row]));
  const listeCircSoglie = sortGroupedRows(
    listeCircWithThresholds.map((row) => {
      const list = listeNazByList.get(row.LISTA);
      if (!list) {
        throw new Error(`Missing national list threshold row for ${row.LISTA}`);
      }

      return {
        ...row,
        SOGLIA1M: list.SOGLIA1M,
        COALIZIONE: list.COALIZIONE,
        MINORANZA: list.MINORANZA
      };
    }),
    [(row) => row.CIRCOSCRIZIONE, (row) => row.LISTA]
  );

  const coalNazBase = aggregateCifra(
    listeNazWithFirstThresholds.filter(
      (row): row is typeof row & { COALIZIONE: string } => row.SOGLIA1M && row.COALIZIONE !== null
    ),
    (row) => row.COALIZIONE,
    (row) => ({
      COALIZIONE: row.COALIZIONE
    })
  );
  const soglia3MByCoalition = anyBy(
    listeNazWithFirstThresholds.filter((row): row is typeof row & { COALIZIONE: string } => row.COALIZIONE !== null),
    (row) => row.COALIZIONE,
    (row) => row.SOGLIA3M
  );
  const coalNaz = sortGroupedRows(
    coalNazBase.map((row) => {
      const soglia3M = soglia3MByCoalition.get(row.COALIZIONE) ?? false;
      const cifraPercentuale = (row.CIFRA / totaleNaz) * 100;
      return {
        COALIZIONE: row.COALIZIONE,
        CIFRA: row.CIFRA,
        CIFRA_PERCENTUALE: cifraPercentuale,
        SOGLIA3M: soglia3M,
        SOGLIA_COALIZIONE: cifraPercentuale >= 10 && soglia3M
      };
    }),
    [(row) => row.COALIZIONE]
  );
  const coalNazByCoalition = new Map(coalNaz.map((row) => [row.COALIZIONE, row]));

  const listeNaz = sortGroupedRows(
    listeNazWithFirstThresholds.map((row) => {
      const sogliaCoalizione = row.COALIZIONE === null ? null : (coalNazByCoalition.get(row.COALIZIONE)?.SOGLIA_COALIZIONE ?? null);
      const sogliaSolaBase = rLogicalOr(row.COALIZIONE === null, sogliaCoalizione === null ? null : !sogliaCoalizione);

      return {
        LISTA: row.LISTA,
        COALIZIONE: row.COALIZIONE,
        MINORANZA: row.MINORANZA,
        CIFRA: row.CIFRA,
        CIFRA_PERCENTUALE: row.CIFRA_PERCENTUALE,
        SOGLIA20: row.SOGLIA20,
        SOGLIA_MINORANZA: row.SOGLIA_MINORANZA,
        SOGLIA1M: row.SOGLIA1M,
        SOGLIA3: row.SOGLIA3,
        SOGLIA3M: row.SOGLIA3M,
        SOGLIA_COALIZIONE: sogliaCoalizione,
        SOGLIA_SOLA: rLogicalAnd(sogliaSolaBase, row.SOGLIA3M)
      };
    }),
    [(row) => row.LISTA]
  );

  const coalCirc = sortGroupedRows(
    aggregateCifra(
      listeCircSoglie.filter(
        (row): row is typeof row & { COALIZIONE: string } => row.SOGLIA1M && row.COALIZIONE !== null
      ),
      (row) => keyOf(row.CIRCOSCRIZIONE, row.COALIZIONE),
      (row) => ({
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
        COALIZIONE: row.COALIZIONE
      })
    ),
    [(row) => row.CIRCOSCRIZIONE, (row) => row.COALIZIONE]
  );

  return {
    totaleNaz,
    listeNaz,
    listeCirc: listeCircSoglie,
    coalNaz,
    coalCirc
  };
}

function projectCandidateAttribution(row: CandidatoUniAttributionWorkingRow): CandidatoUniAttributionTraceRow {
  return {
    CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
    COLLEGIOPLURINOMINALE: row.COLLEGIOPLURINOMINALE,
    COLLEGIOUNINOMINALE: row.COLLEGIOUNINOMINALE,
    CANDIDATO: row.CANDIDATO,
    VOTI_CANDIDATO: row.VOTI_CANDIDATO,
    VOTI_LISTA: row.VOTI_LISTA,
    VOTI_SOLO_CANDIDATO: row.VOTI_SOLO_CANDIDATO,
    QUOZIENTE: traceNumber(row.QUOZIENTE),
    PARTE_INTERA: row.PARTE_INTERA,
    DA_ASSEGNARE: row.DA_ASSEGNARE
  };
}

function projectListeUniCifre(row: ListeUniCifreWorkingRow): ListeUniCifreTraceRow {
  return {
    CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
    COLLEGIOPLURINOMINALE: row.COLLEGIOPLURINOMINALE,
    COLLEGIOUNINOMINALE: row.COLLEGIOUNINOMINALE,
    CANDIDATO: row.CANDIDATO,
    LISTA: row.LISTA,
    VOTI_LISTA: row.VOTI_LISTA,
    ELETTO: row.ELETTO,
    QUOZIENTE: traceNumber(row.QUOZIENTE),
    PARTE_INTERA: row.PARTE_INTERA,
    RESTO: traceNumber(row.RESTO),
    DA_ASSEGNARE: row.DA_ASSEGNARE,
    ORDINE: row.ORDINE,
    VOTO_DA_RESTO: row.VOTO_DA_RESTO,
    CIFRA: row.CIFRA
  };
}

export interface InitialPoliticsScrutinyState {
  candidati_uni: CandidatoUniResultRow[];
}

export function runEarlyPoliticsScrutiny(input: PoliticsScrutinyInput): PoliticsEarlyTrace {
  /*
   * Legal basis: Camera DPR 361/1957 art. 77; Senate D.Lgs. 533/1993 art. 16.
   *
   * - The uninominal candidate with the most valid votes is elected; on ties,
   *   the younger candidate is elected.
   * - A list's uninominal-college figure is its list votes plus the
   *   candidate-only votes attributed to linked lists by quotient, integer
   *   parts, and residual votes by remainder order.
   * - Plurinominal and circumscription figures are sums of the lower-level
   *   figures; candidate percentages divide individual votes by total valid
   *   votes in the uninominal college.
   */
  const electedCandidates = electUninominalCandidateRows(input.candidati_uni);
  const candidatesWithAttribution = addCandidateListVoteAttribution(electedCandidates, input.liste_uni);
  const listeUniCifre = buildListeUniCifre(input.liste_uni, candidatesWithAttribution);
  const listePluriCifre = buildListePluriCifre(listeUniCifre);
  const listeCircCifre = buildListeCircCifre(listePluriCifre);

  return {
    candidati_uni_elezione: electedCandidates.map(projectUninominalResult),
    candidati_uni_attribuzione: candidatesWithAttribution.map(projectCandidateAttribution),
    liste_uni_cifre: listeUniCifre.map(projectListeUniCifre),
    liste_pluri_cifre: listePluriCifre,
    liste_circ_cifre: listeCircCifre,
    candidati_uni_graduatoria: buildCandidatiUniGraduatoria(candidatesWithAttribution),
    totali_circ: buildTotaliCirc(listeCircCifre)
  };
}

export function runPoliticsScrutinyTrace(
  input: PoliticsScrutinyInput,
  context: PoliticsScrutinyContext
): PoliticsScrutinyTrace {
  /*
   * Legal basis: Camera DPR 361/1957 art. 83; Senate D.Lgs. 533/1993
   * art. 16-bis.
   *
   * This stage determines national list figures, total national valid votes,
   * national/circumscription coalition figures, and the admission thresholds:
   * 1% for coalition contribution, 3% for list admission, 10% for coalition
   * admission, plus the regional/minority safeguards described in the law.
   */
  const earlyTrace = runEarlyPoliticsScrutiny(input);
  const thresholds = buildListeNazSoglie(
    context.liste_naz,
    earlyTrace.liste_circ_cifre,
    earlyTrace.totali_circ,
    input.liste_uni,
    earlyTrace.candidati_uni_elezione,
    context.ramo
  );

  return {
    totale_naz: thresholds.totaleNaz,
    ...earlyTrace,
    liste_naz_soglie: thresholds.listeNaz,
    liste_circ_soglie: thresholds.listeCirc,
    coal_naz_soglie: thresholds.coalNaz,
    coal_circ_cifre: thresholds.coalCirc
  };
}

export function runInitialPoliticsScrutiny(input: PoliticsScrutinyInput): InitialPoliticsScrutinyState {
  /*
   * Camera, DPR 361/1957 art. 77; Senate, D.Lgs. 533/1993 art. 16.
   * The uninominal candidate with the most valid votes is elected; on ties, the
   * younger candidate is elected. This mirrors the first block of
   * R/politiche/scrutinio.R before proportional-list vote attribution starts.
   */
  return {
    candidati_uni: runEarlyPoliticsScrutiny(input).candidati_uni_elezione
  };
}
