import type {
  AdministrativeCode,
  CameraAmmesseNazTraceRow,
  CameraListeNazRipartoTraceRow,
  CameraRipartoNazTraceRow,
  CameraRipartoTrace,
  CircRipartoListaTraceRow,
  CircRipartoNazTraceRow,
  CircRipartoTotaleTraceRow,
  CircRipartoTrace,
  CircRipartoTraceRow,
  CoalCircCifreTraceRow,
  CoalNazSoglieTraceRow,
  CandidatoPluriInputRow,
  CandidatoPluriResultRow,
  CandidatoUniAttributionTraceRow,
  CandidatoUniGraduatoriaTraceRow,
  CandidatoUniInputRow,
  CandidatoUniResultRow,
  InternalCircAmmesseNazTraceRow,
  InternalCircAmmesseTraceRow,
  InternalCircRipartoListaTraceRow,
  InternalCircRipartoSubjectTraceRow,
  InternalCircRipartoTrace,
  ListaNazRow,
  ListeCircSoglieTraceRow,
  ListeCircCifreTraceRow,
  ListeNazSoglieTraceRow,
  ListePluriCifreTraceRow,
  ListePluriResultRow,
  ListeUniCifreTraceRow,
  ListeUniRow,
  PluriRipartoAmmesseTraceRow,
  PluriRipartoCircTraceRow,
  PluriRipartoListaTraceRow,
  PluriRipartoTotaleTraceRow,
  PluriRipartoTrace,
  PoliticsEarlyTrace,
  PoliticsScrutinyContext,
  PoliticsScrutinyInput,
  PoliticsScrutinyOutput,
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

interface CandidatoUniNomineWorkingRow extends CandidatoUniInputRow {
  ELETTO: boolean;
  CIFRA_PERCENTUALE: number;
  RIPESCATO: boolean;
}

interface CandidatoPluriNomineWorkingRow extends CandidatoPluriInputRow {
  DISPONIBILE: boolean;
  ELETTI: number;
  CIFRA_PERCENTUALE: number | null;
  ORDINE: number | null;
  ELETTO: boolean;
  ELETTO_QUI_O_ALTROVE: boolean;
}

interface PluriNomineWorkingRow extends PluriRipartoAmmesseTraceRow {
  DECIMALI_USATI: boolean;
  CANDIDATI: number;
  ELETTI: number;
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

function requiredTraceNumber(value: number | null, label: string): number {
  if (value === null) {
    throw new Error(`Missing numeric trace value: ${label}`);
  }
  return value;
}

function rIntegerDivide(left: number, right: number): number {
  const result = Math.floor(left / right);
  return result < 0 || !Number.isFinite(result) ? 0 : result;
}

function rQuotient(left: number, right: number): number {
  return Math.floor(left / right);
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
  totaliCirc: TotaliCircTraceRow[];
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
    totaliCirc: [...totaliCirc],
    listeNaz,
    listeCirc: listeCircSoglie,
    coalNaz,
    coalCirc
  };
}

function emptyCameraRipartoTrace(): CameraRipartoTrace {
  return {
    seggi_proporzionale: null,
    totale_naz_riparto: null,
    quoziente_elettorale_naz: null,
    ancora_da_attribuire: null,
    riparto_naz: [],
    ammesse_naz: [],
    liste_naz_riparto: []
  };
}

function buildCameraRiparto(
  thresholds: ReturnType<typeof buildListeNazSoglie>,
  electedCandidates: readonly CandidatoUniResultRow[],
  context: PoliticsScrutinyContext
): CameraRipartoTrace {
  if (context.ramo !== 'camera') {
    return emptyCameraRipartoTrace();
  }

  const seggiProporzionale =
    context.totale_seggi - electedCandidates.filter((candidate) => candidate.ELETTO).length;
  const totalPluriSeats = context.totali_pluri.reduce((total, row) => total + row.SEGGI, 0);

  if (seggiProporzionale !== totalPluriSeats) {
    throw new Error(
      `seggi_proporzionale = ${seggiProporzionale} ma sum(totali_pluri$SEGGI) = ${totalPluriSeats}`
    );
  }

  const listeNazRiparto = sortGroupedRows(
    thresholds.listeNaz.map((row): CameraListeNazRipartoTraceRow => {
      let soggettoRiparto: string | null = null;

      if (row.SOGLIA_COALIZIONE === true && row.COALIZIONE !== null) {
        soggettoRiparto = row.COALIZIONE;
      }

      if (row.SOGLIA_SOLA === true) {
        soggettoRiparto = row.LISTA;
      }

      return {
        LISTA: row.LISTA,
        COALIZIONE: row.COALIZIONE,
        SOGLIA1M: row.SOGLIA1M,
        SOGLIA3M: row.SOGLIA3M,
        SOGLIA_COALIZIONE: row.SOGLIA_COALIZIONE,
        SOGLIA_SOLA: row.SOGLIA_SOLA,
        SOGGETTO_RIPARTO: soggettoRiparto
      };
    }),
    [(row) => row.LISTA]
  );

  const nationalCifraByList = new Map(thresholds.listeNaz.map((row) => [row.LISTA, row.CIFRA]));
  const repartitionableRows = listeNazRiparto
    .map((row) => ({
      ...row,
      CIFRA: nationalCifraByList.get(row.LISTA) ?? 0
    }))
    .filter((row): row is typeof row & { SOGGETTO_RIPARTO: string } => row.SOGLIA1M && row.SOGGETTO_RIPARTO !== null);

  const ripartoInitial = aggregateCifra(
    repartitionableRows,
    (row) => row.SOGGETTO_RIPARTO,
    (row) => ({
      SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO
    })
  );
  const totaleNazRiparto = ripartoInitial.reduce((total, row) => total + row.CIFRA, 0);
  const quozienteElettoraleNaz = rIntegerDivide(totaleNazRiparto, seggiProporzionale);
  const ripartoWithSeats = ripartoInitial.map((row) => ({
    ...row,
    PARTE_INTERA: rIntegerDivide(row.CIFRA, quozienteElettoraleNaz),
    RESTO: row.CIFRA % quozienteElettoraleNaz,
    ORDINE: 0,
    SEGGIO_DA_RESTO: false,
    SEGGI: 0
  }));
  const ancoraDaAttribuire =
    seggiProporzionale - ripartoWithSeats.reduce((total, row) => total + row.PARTE_INTERA, 0);

  ripartoWithSeats.sort((left, right) => {
    const byRemainder = compareDescending(left.RESTO, right.RESTO);
    if (byRemainder !== 0) return byRemainder;

    const byCifra = compareDescending(left.CIFRA, right.CIFRA);
    if (byCifra !== 0) return byCifra;

    /*
     * TODO(law-review): art. 83 mentions sorteggio after equal remainders and
     * equal national figures. The current R path has no explicit draw here, so
     * we preserve stable ordering for golden-master parity.
     */
    return 0;
  });

  for (const [index, row] of ripartoWithSeats.entries()) {
    row.ORDINE = index + 1;
    row.SEGGIO_DA_RESTO = row.ORDINE <= ancoraDaAttribuire;
    row.SEGGI = row.PARTE_INTERA + (row.SEGGIO_DA_RESTO ? 1 : 0);
  }

  const ammesseInitial = sortGroupedRows(
    listeNazRiparto
      .map((row) => ({
        SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
        LISTA: row.LISTA,
        CIFRA: nationalCifraByList.get(row.LISTA) ?? 0,
        SOGLIA3M: row.SOGLIA3M
      }))
      .filter((row): row is { SOGGETTO_RIPARTO: string; LISTA: string; CIFRA: number; SOGLIA3M: boolean } =>
        row.SOGLIA3M && row.SOGGETTO_RIPARTO !== null
      ),
    [(row) => row.SOGGETTO_RIPARTO, (row) => row.LISTA]
  );
  const admittedCifraBySubject = sumBy(
    ammesseInitial,
    (row) => row.SOGGETTO_RIPARTO,
    (row) => row.CIFRA
  );
  const ripartoBySubject = new Map(ripartoWithSeats.map((row) => [row.SOGGETTO_RIPARTO, row]));
  const ammesseWithQuotient = ammesseInitial.map((row) => {
    const riparto = ripartoBySubject.get(row.SOGGETTO_RIPARTO);
    if (!riparto) {
      throw new Error(`Missing national repartition subject: ${row.SOGGETTO_RIPARTO}`);
    }

    const quotient = rIntegerDivide(admittedCifraBySubject.get(row.SOGGETTO_RIPARTO) ?? 0, riparto.SEGGI);
    return {
      SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
      LISTA: row.LISTA,
      CIFRA: row.CIFRA,
      QUOZIENTE: quotient,
      PARTE_INTERA: rIntegerDivide(row.CIFRA, quotient),
      RESTO: row.CIFRA % quotient
    };
  });
  const admittedIntegerBySubject = sumBy(
    ammesseWithQuotient,
    (row) => row.SOGGETTO_RIPARTO,
    (row) => row.PARTE_INTERA
  );

  const ripartoNaz = ripartoWithSeats.map((row): CameraRipartoNazTraceRow => {
    const admittedCifra = admittedCifraBySubject.get(row.SOGGETTO_RIPARTO) ?? 0;
    const integerTotal = admittedIntegerBySubject.get(row.SOGGETTO_RIPARTO) ?? 0;
    return {
      SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
      CIFRA: row.CIFRA,
      PARTE_INTERA: row.PARTE_INTERA,
      RESTO: row.RESTO,
      ORDINE: row.ORDINE,
      SEGGIO_DA_RESTO: row.SEGGIO_DA_RESTO,
      SEGGI: row.SEGGI,
      CIFRA_AMMESSE_AL_RIPARTO: admittedCifra,
      QUOZIENTE: rIntegerDivide(admittedCifra, row.SEGGI),
      PARTE_INTERA_TOT: integerTotal,
      DA_ASSEGNARE: row.SEGGI - integerTotal
    };
  });
  const daAssegnareBySubject = new Map(ripartoNaz.map((row) => [row.SOGGETTO_RIPARTO, row.DA_ASSEGNARE]));
  const ammesseNaz = ammesseWithQuotient.map((row) => ({
    ...row,
    DA_ASSEGNARE: daAssegnareBySubject.get(row.SOGGETTO_RIPARTO) ?? 0,
    ORDINE: 0,
    SEGGIO_DA_RESTO: false,
    SEGGI: 0
  }));

  ammesseNaz.sort((left, right) => {
    const bySubject = compareAscending(left.SOGGETTO_RIPARTO, right.SOGGETTO_RIPARTO);
    if (bySubject !== 0) return bySubject;

    const byRemainder = compareDescending(left.RESTO, right.RESTO);
    if (byRemainder !== 0) return byRemainder;

    const byCifra = compareDescending(left.CIFRA, right.CIFRA);
    if (byCifra !== 0) return byCifra;

    /*
     * TODO(law-review): art. 83 also mentions sorteggio for the internal
     * coalition riparto after equal remainders and equal figures. R has no
     * explicit draw here, so stable ordering is preserved.
     */
    return 0;
  });

  const orderBySubject = new Map<string, number>();
  for (const row of ammesseNaz) {
    const order = (orderBySubject.get(row.SOGGETTO_RIPARTO) ?? 0) + 1;
    orderBySubject.set(row.SOGGETTO_RIPARTO, order);
    row.ORDINE = order;
    row.SEGGIO_DA_RESTO = order <= row.DA_ASSEGNARE;
    row.SEGGI = row.PARTE_INTERA + (row.SEGGIO_DA_RESTO ? 1 : 0);
  }

  return {
    seggi_proporzionale: seggiProporzionale,
    totale_naz_riparto: totaleNazRiparto,
    quoziente_elettorale_naz: quozienteElettoraleNaz,
    ancora_da_attribuire: ancoraDaAttribuire,
    riparto_naz: ripartoNaz,
    ammesse_naz: ammesseNaz.map((row): CameraAmmesseNazTraceRow => ({
      SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
      LISTA: row.LISTA,
      CIFRA: row.CIFRA,
      QUOZIENTE: row.QUOZIENTE,
      PARTE_INTERA: row.PARTE_INTERA,
      RESTO: row.RESTO,
      DA_ASSEGNARE: row.DA_ASSEGNARE,
      ORDINE: row.ORDINE,
      SEGGIO_DA_RESTO: row.SEGGIO_DA_RESTO,
      SEGGI: row.SEGGI
    })),
    liste_naz_riparto: listeNazRiparto
  };
}

function buildCircRiparto(
  thresholds: ReturnType<typeof buildListeNazSoglie>,
  cameraRiparto: CameraRipartoTrace,
  context: PoliticsScrutinyContext
): CircRipartoTrace {
  /*
   * Legal basis: Camera DPR 361/1957 art. 83 letter h; Senate D.Lgs.
   * 533/1993 art. 17 letter a.
   *
   * This stage assigns seats to admitted coalitions or single lists inside
   * each circumscription. Camera then reconciles provisional circumscription
   * seats back to the national allocation with the "flipper" transfer loop.
   */
  const seatsByCirc = sumBy(
    context.totali_pluri,
    (row) => keyOf(row.CIRCOSCRIZIONE),
    (row) => row.SEGGI
  );
  const listeNazByList = new Map(thresholds.listeNaz.map((row) => [row.LISTA, row]));
  const cameraSubjectByList = new Map(cameraRiparto.liste_naz_riparto.map((row) => [row.LISTA, row.SOGGETTO_RIPARTO]));
  const cameraRipartoBySubject = new Map(cameraRiparto.riparto_naz.map((row) => [row.SOGGETTO_RIPARTO, row]));
  const coalByCoalition = new Map(thresholds.coalNaz.map((row) => [row.COALIZIONE, row]));

  const listeCirc = sortGroupedRows(
    thresholds.listeCirc.map((row): CircRipartoListaTraceRow => {
      const nationalList = listeNazByList.get(row.LISTA);
      if (!nationalList) {
        throw new Error(`Missing national threshold row for ${row.LISTA}`);
      }

      if (context.ramo === 'camera') {
        return {
          CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
          LISTA: row.LISTA,
          CIFRA: row.CIFRA,
          SOGLIA1M: nationalList.SOGLIA1M,
          SOGLIA3: nationalList.SOGLIA3,
          SOGLIA20: row.SOGLIA20,
          SOGLIA_MINORANZA: row.SOGLIA_MINORANZA,
          SOGLIA_COALIZIONE: null,
          SOGLIA_SOLA: null,
          SOGGETTO_RIPARTO: cameraSubjectByList.get(row.LISTA) ?? null
        };
      }

      const coalitionThreshold =
        row.COALIZIONE === null ? null : (coalByCoalition.get(row.COALIZIONE)?.SOGLIA_COALIZIONE ?? null);
      const sogliaSolaBase = rLogicalOr(row.COALIZIONE === null, coalitionThreshold === null ? null : !coalitionThreshold);
      const admissionBase = nationalList.SOGLIA3 || row.SOGLIA20 || row.SOGLIA_MINORANZA;
      const sogliaSola = rLogicalAnd(sogliaSolaBase, admissionBase);
      let subject: string | null = null;

      if (coalitionThreshold === true && row.COALIZIONE !== null) {
        subject = row.COALIZIONE;
      }

      if (sogliaSola === true) {
        subject = row.LISTA;
      }

      return {
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
        LISTA: row.LISTA,
        CIFRA: row.CIFRA,
        SOGLIA1M: nationalList.SOGLIA1M,
        SOGLIA3: nationalList.SOGLIA3,
        SOGLIA20: row.SOGLIA20,
        SOGLIA_MINORANZA: row.SOGLIA_MINORANZA,
        SOGLIA_COALIZIONE: coalitionThreshold,
        SOGLIA_SOLA: sogliaSola,
        SOGGETTO_RIPARTO: subject
      };
    }),
    [(row) => row.CIRCOSCRIZIONE, (row) => row.LISTA]
  );

  const baseRiparto = aggregateCifra(
    listeCirc.filter(
      (row): row is CircRipartoListaTraceRow & { SOGGETTO_RIPARTO: string } =>
        row.SOGLIA1M && row.SOGGETTO_RIPARTO !== null
    ),
    (row) => keyOf(row.CIRCOSCRIZIONE, row.SOGGETTO_RIPARTO),
    (row) => ({
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO
    })
  );
  const admittedCifraByCirc = sumBy(
    baseRiparto,
    (row) => keyOf(row.CIRCOSCRIZIONE),
    (row) => row.CIFRA
  );
  const totaliCircWithQuotient = thresholds.totaliCirc.map((row) => {
    const seats = seatsByCirc.get(keyOf(row.CIRCOSCRIZIONE)) ?? 0;
    const admittedCifra = admittedCifraByCirc.get(keyOf(row.CIRCOSCRIZIONE)) ?? 0;
    return {
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      CIFRA: row.CIFRA,
      SEGGI: seats,
      CIFRA_AMMESSE_AL_RIPARTO: admittedCifra,
      QUOZIENTE: rIntegerDivide(admittedCifra, seats)
    };
  });
  const quotientByCirc = new Map(totaliCircWithQuotient.map((row) => [keyOf(row.CIRCOSCRIZIONE), row.QUOZIENTE]));
  const ripartoWithIntegers = baseRiparto.map((row) => ({
    CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
    SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
    CIFRA: row.CIFRA,
    QUOZIENTE: quotientByCirc.get(keyOf(row.CIRCOSCRIZIONE)) ?? 0,
    PARTE_INTERA: rIntegerDivide(row.CIFRA, quotientByCirc.get(keyOf(row.CIRCOSCRIZIONE)) ?? 0)
  }));
  const integerByCirc = sumBy(
    ripartoWithIntegers,
    (row) => keyOf(row.CIRCOSCRIZIONE),
    (row) => row.PARTE_INTERA
  );
  const totaliCirc = sortGroupedRows(
    totaliCircWithQuotient.map((row): CircRipartoTotaleTraceRow => {
      const integerPart = integerByCirc.get(keyOf(row.CIRCOSCRIZIONE)) ?? 0;
      return {
        ...row,
        PARTE_INTERA: integerPart,
        DA_ASSEGNARE: row.SEGGI - integerPart
      };
    }),
    [(row) => row.CIRCOSCRIZIONE]
  );
  const daAssegnareByCirc = new Map(totaliCirc.map((row) => [keyOf(row.CIRCOSCRIZIONE), row.DA_ASSEGNARE]));

  if (context.ramo === 'camera') {
    const ripartoNazBySubject = new Map<string, CircRipartoNazTraceRow>();
    const integerBySubject = sumBy(
      ripartoWithIntegers,
      (row) => row.SOGGETTO_RIPARTO,
      (row) => row.PARTE_INTERA
    );

    for (const [subject, riparto] of cameraRipartoBySubject) {
      const integerPart = integerBySubject.get(subject) ?? 0;
      ripartoNazBySubject.set(subject, {
        SOGGETTO_RIPARTO: subject,
        CIFRA: riparto.CIFRA,
        SEGGI: riparto.SEGGI,
        PARTE_INTERA_CIRC: integerPart,
        ESCLUSE: integerPart >= riparto.SEGGI,
        SEGGI_CIRC: 0,
        SEGGI_ECCEDENTI: 0,
        SEGGI_ECCEDENTI_CONTATORE: 0
      });
    }

    const rows = ripartoWithIntegers.map((row) => {
      const national = ripartoNazBySubject.get(row.SOGGETTO_RIPARTO);
      if (!national) {
        throw new Error(`Missing Camera national riparto row for ${row.SOGGETTO_RIPARTO}`);
      }

      return {
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
        SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
        CIFRA: row.CIFRA,
        QUOZIENTE: row.QUOZIENTE,
        PARTE_INTERA: row.PARTE_INTERA,
        DA_ASSEGNARE: daAssegnareByCirc.get(keyOf(row.CIRCOSCRIZIONE)) ?? 0,
        DECIMALI: (row.CIFRA / row.QUOZIENTE) % 1,
        RESTO: null,
        CIFRA_NAZ: national.CIFRA,
        ESCLUSE: national.ESCLUSE,
        ORDINE: null as number | null,
        SEGGIO_DA_DECIMALI: false,
        SEGGIO_DA_RESTO: null,
        FLIPPER: 0,
        SEGGI: 0
      };
    });

    rows.sort((left, right) => {
      const byCirc = compareAscending(left.CIRCOSCRIZIONE, right.CIRCOSCRIZIONE);
      if (byCirc !== 0) return byCirc;

      const byExcluded = compareAscending(Number(left.ESCLUSE), Number(right.ESCLUSE));
      if (byExcluded !== 0) return byExcluded;

      const byDecimals = compareDescending(left.DECIMALI, right.DECIMALI);
      if (byDecimals !== 0) return byDecimals;

      const byNationalCifra = compareDescending(left.CIFRA_NAZ, right.CIFRA_NAZ);
      if (byNationalCifra !== 0) return byNationalCifra;

      return 0;
    });

    const orderByCirc = new Map<string, number>();
    for (const row of rows) {
      if (!row.ESCLUSE) {
        const circKey = keyOf(row.CIRCOSCRIZIONE);
        const order = (orderByCirc.get(circKey) ?? 0) + 1;
        orderByCirc.set(circKey, order);
        row.ORDINE = order;
      }

      row.SEGGIO_DA_DECIMALI = row.ORDINE !== null && row.ORDINE <= row.DA_ASSEGNARE;
      row.SEGGI = row.PARTE_INTERA + (row.SEGGIO_DA_DECIMALI ? 1 : 0);
    }

    const seatsBySubject = sumBy(
      rows,
      (row) => row.SOGGETTO_RIPARTO,
      (row) => row.SEGGI
    );
    let ripartoNaz = [...ripartoNazBySubject.values()].map((row) => {
      const circSeats = seatsBySubject.get(row.SOGGETTO_RIPARTO) ?? 0;
      const excess = circSeats - row.SEGGI;
      return {
        ...row,
        SEGGI_CIRC: circSeats,
        SEGGI_ECCEDENTI: excess,
        SEGGI_ECCEDENTI_CONTATORE: excess
      };
    });

    ripartoNaz.sort((left, right) => {
      const byExcess = compareDescending(left.SEGGI_ECCEDENTI, right.SEGGI_ECCEDENTI);
      if (byExcess !== 0) return byExcess;

      const byCifra = compareDescending(left.CIFRA, right.CIFRA);
      if (byCifra !== 0) return byCifra;

      return 0;
    });

    const ripartoNazCounterBySubject = new Map(ripartoNaz.map((row) => [row.SOGGETTO_RIPARTO, row]));

    for (const nationalRow of ripartoNaz) {
      if (nationalRow.SEGGI_ECCEDENTI < 1) break;

      const subject = nationalRow.SOGGETTO_RIPARTO;
      for (let index = 0; index < nationalRow.SEGGI_ECCEDENTI; index += 1) {
        const deficitSubjects = new Set(
          [...ripartoNazCounterBySubject.values()]
            .filter((row) => row.SEGGI_ECCEDENTI_CONTATORE < 0)
            .map((row) => row.SOGGETTO_RIPARTO)
        );
        const deficitRows = rows.filter(
          (row) =>
            deficitSubjects.has(row.SOGGETTO_RIPARTO) &&
            !row.SEGGIO_DA_DECIMALI &&
            row.FLIPPER === 0
        );
        let donorCandidates = rows.filter(
          (row) => row.SOGGETTO_RIPARTO === subject && row.SEGGIO_DA_DECIMALI && row.FLIPPER === 0
        );

        if (donorCandidates.length < 1) {
          throw new Error(
            'Devo togliere un seggio eccedente ma non ci sono circoscrizioni dove questo sia stato ottenuto con i resti'
          );
        }

        const rankedDonorCandidates = donorCandidates
          .map((row) => ({
            ...row,
            DEFICIT_PRESENTE: deficitRows.some((deficit) => deficit.CIRCOSCRIZIONE === row.CIRCOSCRIZIONE)
          }))
          .sort((left, right) => {
            const byDeficit = compareDescending(Number(left.DEFICIT_PRESENTE), Number(right.DEFICIT_PRESENTE));
            if (byDeficit !== 0) return byDeficit;

            const byDecimals = compareAscending(left.DECIMALI, right.DECIMALI);
            if (byDecimals !== 0) return byDecimals;

            return 0;
          });

        const donor = rankedDonorCandidates[0];
        const recipientPool = donor.DEFICIT_PRESENTE
          ? deficitRows.filter((row) => row.CIRCOSCRIZIONE === donor.CIRCOSCRIZIONE)
          : deficitRows;

        if (recipientPool.length < 1) {
          throw new Error('Non ho a chi dare il seggio eccedente');
        }

        const recipient = [...recipientPool].sort((left, right) => {
          const byDecimals = compareDescending(left.DECIMALI, right.DECIMALI);
          if (byDecimals !== 0) return byDecimals;

          const byNationalCifra = compareDescending(left.CIFRA_NAZ, right.CIFRA_NAZ);
          if (byNationalCifra !== 0) return byNationalCifra;

          return 0;
        })[0];
        const source = rows.find(
          (row) => row.SOGGETTO_RIPARTO === subject && row.CIRCOSCRIZIONE === donor.CIRCOSCRIZIONE
        );
        const target = rows.find(
          (row) =>
            row.SOGGETTO_RIPARTO === recipient.SOGGETTO_RIPARTO &&
            row.CIRCOSCRIZIONE === recipient.CIRCOSCRIZIONE
        );

        if (!source || !target) {
          throw new Error('Internal Camera flipper row lookup failed');
        }

        source.FLIPPER = -1;
        nationalRow.SEGGI_ECCEDENTI_CONTATORE -= 1;
        target.FLIPPER = 1;
        const recipientNational = ripartoNazCounterBySubject.get(recipient.SOGGETTO_RIPARTO);
        if (recipientNational) {
          recipientNational.SEGGI_ECCEDENTI_CONTATORE += 1;
        }
      }
    }

    for (const row of rows) {
      row.SEGGI += row.FLIPPER;
    }

    return {
      totali_circ: totaliCirc,
      liste_circ: listeCirc,
      riparto_circ: rows.map((row): CircRipartoTraceRow => ({
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
        SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
        CIFRA: row.CIFRA,
        QUOZIENTE: row.QUOZIENTE,
        PARTE_INTERA: row.PARTE_INTERA,
        DA_ASSEGNARE: row.DA_ASSEGNARE,
        DECIMALI: row.DECIMALI,
        RESTO: null,
        CIFRA_NAZ: row.CIFRA_NAZ,
        ESCLUSE: row.ESCLUSE,
        ORDINE: row.ORDINE,
        SEGGIO_DA_DECIMALI: row.SEGGIO_DA_DECIMALI,
        SEGGIO_DA_RESTO: null,
        FLIPPER: row.FLIPPER,
        SEGGI: row.SEGGI
      })),
      riparto_naz: ripartoNaz
    };
  }

  const rows = ripartoWithIntegers.map((row) => ({
    CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
    SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
    CIFRA: row.CIFRA,
    QUOZIENTE: row.QUOZIENTE,
    PARTE_INTERA: row.PARTE_INTERA,
    DA_ASSEGNARE: daAssegnareByCirc.get(keyOf(row.CIRCOSCRIZIONE)) ?? 0,
    DECIMALI: null,
    RESTO: row.CIFRA % row.QUOZIENTE,
    CIFRA_NAZ: null,
    ESCLUSE: null,
    ORDINE: 0,
    SEGGIO_DA_DECIMALI: null,
    SEGGIO_DA_RESTO: false,
    FLIPPER: null,
    SEGGI: 0
  }));

  rows.sort((left, right) => {
    const byCirc = compareAscending(left.CIRCOSCRIZIONE, right.CIRCOSCRIZIONE);
    if (byCirc !== 0) return byCirc;

    const byRemainder = compareDescending(left.RESTO, right.RESTO);
    if (byRemainder !== 0) return byRemainder;

    const byCifra = compareDescending(left.CIFRA, right.CIFRA);
    if (byCifra !== 0) return byCifra;

    /*
     * TODO(law-review): art. 17 mentions sorteggio for equal remainders and
     * equal regional figures. R does not draw explicitly here; preserve stable
     * ordering for parity.
     */
    return 0;
  });

  const orderByCirc = new Map<string, number>();
  for (const row of rows) {
    const circKey = keyOf(row.CIRCOSCRIZIONE);
    const order = (orderByCirc.get(circKey) ?? 0) + 1;
    orderByCirc.set(circKey, order);
    row.ORDINE = order;
    row.SEGGIO_DA_RESTO = row.ORDINE <= row.DA_ASSEGNARE;
    row.SEGGI = row.PARTE_INTERA + (row.SEGGIO_DA_RESTO ? 1 : 0);
  }

  return {
    totali_circ: totaliCirc,
    liste_circ: listeCirc,
    riparto_circ: rows.map((row): CircRipartoTraceRow => ({
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
      CIFRA: row.CIFRA,
      QUOZIENTE: row.QUOZIENTE,
      PARTE_INTERA: row.PARTE_INTERA,
      DA_ASSEGNARE: row.DA_ASSEGNARE,
      DECIMALI: null,
      RESTO: row.RESTO,
      CIFRA_NAZ: null,
      ESCLUSE: null,
      ORDINE: row.ORDINE,
      SEGGIO_DA_DECIMALI: null,
      SEGGIO_DA_RESTO: row.SEGGIO_DA_RESTO,
      FLIPPER: null,
      SEGGI: row.SEGGI
    })),
    riparto_naz: []
  };
}

function buildInternalCircRiparto(
  thresholds: ReturnType<typeof buildListeNazSoglie>,
  circRiparto: CircRipartoTrace,
  cameraRiparto: CameraRipartoTrace,
  context: PoliticsScrutinyContext
): InternalCircRipartoTrace {
  /*
   * Legal basis: Camera DPR 361/1957 art. 83 letter i; Senate D.Lgs.
   * 533/1993 art. 17 letter b.
   *
   * This stage distributes each coalition's circumscription seats to admitted
   * lists, then for Camera reconciles provisional list seats back to the
   * national internal-coalition allocation with the second flipper loop.
   */
  const thresholdByCircList = new Map(
    thresholds.listeCirc.map((row) => [keyOf(row.CIRCOSCRIZIONE, row.LISTA), row])
  );
  const listeCirc = sortGroupedRows(
    circRiparto.liste_circ.map((row): InternalCircRipartoListaTraceRow => {
      const threshold = thresholdByCircList.get(keyOf(row.CIRCOSCRIZIONE, row.LISTA));
      if (!threshold) {
        throw new Error(`Missing circ threshold row for ${row.CIRCOSCRIZIONE}/${row.LISTA}`);
      }

      const admitted =
        row.SOGLIA3 ||
        (row.SOGLIA20 && (threshold.MINORANZA || context.ramo === 'senato')) ||
        row.SOGLIA_MINORANZA;

      return {
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
        LISTA: row.LISTA,
        CIFRA: row.CIFRA,
        MINORANZA: threshold.MINORANZA,
        SOGLIA3: row.SOGLIA3,
        SOGLIA20: row.SOGLIA20,
        SOGLIA_MINORANZA: row.SOGLIA_MINORANZA,
        SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
        AMMESSA: admitted
      };
    }),
    [(row) => row.CIRCOSCRIZIONE, (row) => row.LISTA]
  );
  const admittedRows = listeCirc.filter(
    (row): row is InternalCircRipartoListaTraceRow & { SOGGETTO_RIPARTO: string } =>
      row.AMMESSA && row.SOGGETTO_RIPARTO !== null
  );
  const admittedCifraByCircSubject = sumBy(
    admittedRows,
    (row) => keyOf(row.CIRCOSCRIZIONE, row.SOGGETTO_RIPARTO),
    (row) => row.CIFRA
  );
  const subjectRowsWithQuotient = circRiparto.riparto_circ.map((row) => {
    const admittedCifra = admittedCifraByCircSubject.get(keyOf(row.CIRCOSCRIZIONE, row.SOGGETTO_RIPARTO)) ?? 0;
    const quotient = rQuotient(admittedCifra, row.SEGGI);
    return {
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
      SEGGI: row.SEGGI,
      CIFRA_AMMESSE_AL_RIPARTO: admittedCifra,
      QUOZIENTE_COAL_RAW: quotient
    };
  });
  const quotientByCircSubject = new Map(
    subjectRowsWithQuotient.map((row) => [
      keyOf(row.CIRCOSCRIZIONE, row.SOGGETTO_RIPARTO),
      row.QUOZIENTE_COAL_RAW
    ])
  );
  const ammesseWithIntegerPart = admittedRows.map((row) => {
    const quotient = quotientByCircSubject.get(keyOf(row.CIRCOSCRIZIONE, row.SOGGETTO_RIPARTO)) ?? 0;
    return {
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
      LISTA: row.LISTA,
      CIFRA: row.CIFRA,
      QUOZIENTE_COAL_RAW: quotient,
      PARTE_INTERA: rIntegerDivide(row.CIFRA, quotient)
    };
  });
  const integerPartByCircSubject = sumBy(
    ammesseWithIntegerPart,
    (row) => keyOf(row.CIRCOSCRIZIONE, row.SOGGETTO_RIPARTO),
    (row) => row.PARTE_INTERA
  );
  const subjectRows = sortGroupedRows(
    subjectRowsWithQuotient.map((row): InternalCircRipartoSubjectTraceRow & { QUOZIENTE_COAL_RAW: number } => {
      const integerPart = integerPartByCircSubject.get(keyOf(row.CIRCOSCRIZIONE, row.SOGGETTO_RIPARTO)) ?? 0;
      return {
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
        SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
        SEGGI: row.SEGGI,
        CIFRA_AMMESSE_AL_RIPARTO: row.CIFRA_AMMESSE_AL_RIPARTO,
        QUOZIENTE_COAL: traceNumber(row.QUOZIENTE_COAL_RAW),
        QUOZIENTE_COAL_RAW: row.QUOZIENTE_COAL_RAW,
        PARTE_INTERA_TOT: integerPart,
        DA_ASSEGNARE_COAL: row.SEGGI - integerPart
      };
    }),
    [(row) => row.CIRCOSCRIZIONE, (row) => row.SOGGETTO_RIPARTO]
  );
  const subjectByCircSubject = new Map(
    subjectRows.map((row) => [keyOf(row.CIRCOSCRIZIONE, row.SOGGETTO_RIPARTO), row])
  );

  if (context.ramo === 'camera') {
    const nationalListByList = new Map(cameraRiparto.ammesse_naz.map((row) => [row.LISTA, row]));
    const integerByList = sumBy(
      ammesseWithIntegerPart,
      (row) => row.LISTA,
      (row) => row.PARTE_INTERA
    );
    const nationalRowsByList = new Map<string, InternalCircAmmesseNazTraceRow>();

    for (const national of cameraRiparto.ammesse_naz) {
      const integerPart = integerByList.get(national.LISTA) ?? 0;
      nationalRowsByList.set(national.LISTA, {
        SOGGETTO_RIPARTO: national.SOGGETTO_RIPARTO,
        LISTA: national.LISTA,
        CIFRA: national.CIFRA,
        SEGGI: national.SEGGI,
        PARTE_INTERA_CIRC: integerPart,
        ESCLUSE: integerPart >= national.SEGGI,
        SEGGI_CIRC: 0,
        SEGGI_ECCEDENTI: 0,
        SEGGI_ECCEDENTI_CONTATORE: 0
      });
    }

    const rows = ammesseWithIntegerPart.map((row) => {
      const subject = subjectByCircSubject.get(keyOf(row.CIRCOSCRIZIONE, row.SOGGETTO_RIPARTO));
      const national = nationalListByList.get(row.LISTA);
      const nationalCounter = nationalRowsByList.get(row.LISTA);
      if (!subject || !national || !nationalCounter) {
        throw new Error(`Missing internal Camera allocation context for ${row.CIRCOSCRIZIONE}/${row.LISTA}`);
      }

      return {
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
        SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
        LISTA: row.LISTA,
        CIFRA: row.CIFRA,
        QUOZIENTE_COAL_RAW: row.QUOZIENTE_COAL_RAW,
        PARTE_INTERA: row.PARTE_INTERA,
        DA_ASSEGNARE_COAL: subject.DA_ASSEGNARE_COAL,
        DECIMALI: (row.CIFRA / row.QUOZIENTE_COAL_RAW) % 1,
        RESTO: null,
        CIFRA_NAZ: national.CIFRA,
        ESCLUSE: nationalCounter.ESCLUSE,
        ORDINE: null as number | null,
        SEGGIO_DA_DECIMALI: false,
        SEGGIO_DA_RESTO: null,
        FLIPPER: 0,
        SEGGI: 0
      };
    });

    rows.sort((left, right) => {
      const byCirc = compareAscending(left.CIRCOSCRIZIONE, right.CIRCOSCRIZIONE);
      if (byCirc !== 0) return byCirc;

      const bySubject = compareAscending(left.SOGGETTO_RIPARTO, right.SOGGETTO_RIPARTO);
      if (bySubject !== 0) return bySubject;

      const byExcluded = compareAscending(Number(left.ESCLUSE), Number(right.ESCLUSE));
      if (byExcluded !== 0) return byExcluded;

      const byDecimals = compareDescending(left.DECIMALI, right.DECIMALI);
      if (byDecimals !== 0) return byDecimals;

      const byCifra = compareDescending(left.CIFRA, right.CIFRA);
      if (byCifra !== 0) return byCifra;

      return 0;
    });

    const orderByCircSubject = new Map<string, number>();
    for (const row of rows) {
      if (!row.ESCLUSE) {
        const groupKey = keyOf(row.CIRCOSCRIZIONE, row.SOGGETTO_RIPARTO);
        const order = (orderByCircSubject.get(groupKey) ?? 0) + 1;
        orderByCircSubject.set(groupKey, order);
        row.ORDINE = order;
      }

      row.SEGGIO_DA_DECIMALI = row.ORDINE !== null && row.ORDINE <= row.DA_ASSEGNARE_COAL;
      row.SEGGI = row.PARTE_INTERA + (row.SEGGIO_DA_DECIMALI ? 1 : 0);
    }

    const seatsByList = sumBy(
      rows,
      (row) => row.LISTA,
      (row) => row.SEGGI
    );
    let nationalRows = [...nationalRowsByList.values()].map((row) => {
      const circSeats = seatsByList.get(row.LISTA) ?? 0;
      const excess = circSeats - row.SEGGI;
      return {
        ...row,
        SEGGI_CIRC: circSeats,
        SEGGI_ECCEDENTI: excess,
        SEGGI_ECCEDENTI_CONTATORE: excess
      };
    });

    nationalRows.sort((left, right) => {
      const byExcess = compareDescending(left.SEGGI_ECCEDENTI, right.SEGGI_ECCEDENTI);
      if (byExcess !== 0) return byExcess;

      const byCifra = compareDescending(left.CIFRA, right.CIFRA);
      if (byCifra !== 0) return byCifra;

      return 0;
    });

    const nationalCounterByList = new Map(nationalRows.map((row) => [row.LISTA, row]));

    for (const nationalRow of nationalRows) {
      if (nationalRow.SEGGI_ECCEDENTI < 1) break;

      const subject = nationalRow.SOGGETTO_RIPARTO;
      const list = nationalRow.LISTA;
      for (let index = 0; index < nationalRow.SEGGI_ECCEDENTI; index += 1) {
        const deficitLists = new Set(
          [...nationalCounterByList.values()]
            .filter((row) => row.SEGGI_ECCEDENTI_CONTATORE < 0)
            .map((row) => row.LISTA)
        );
        const deficitRows = rows.filter(
          (row) =>
            row.SOGGETTO_RIPARTO === subject &&
            deficitLists.has(row.LISTA) &&
            !row.SEGGIO_DA_DECIMALI &&
            row.FLIPPER === 0
        );
        const donorCandidates = rows.filter(
          (row) => row.LISTA === list && row.SEGGIO_DA_DECIMALI && row.FLIPPER === 0
        );

        if (donorCandidates.length < 1) {
          throw new Error(
            `Devo togliere un seggio a ${list} ma non ci sono circoscrizioni dove questo sia stato ottenuto con i resti.`
          );
        }

        const rankedDonorCandidates = donorCandidates
          .map((row) => ({
            ...row,
            DEFICIT_PRESENTE: deficitRows.some((deficit) => deficit.CIRCOSCRIZIONE === row.CIRCOSCRIZIONE)
          }))
          .sort((left, right) => {
            const byDeficit = compareDescending(Number(left.DEFICIT_PRESENTE), Number(right.DEFICIT_PRESENTE));
            if (byDeficit !== 0) return byDeficit;

            const byDecimals = compareAscending(left.DECIMALI, right.DECIMALI);
            if (byDecimals !== 0) return byDecimals;

            return 0;
          });

        const donor = rankedDonorCandidates[0];
        const recipientPool = donor.DEFICIT_PRESENTE
          ? deficitRows.filter(
              (row) => row.CIRCOSCRIZIONE === donor.CIRCOSCRIZIONE && row.SOGGETTO_RIPARTO === subject
            )
          : deficitRows.filter((row) => row.SOGGETTO_RIPARTO === subject);

        if (recipientPool.length < 1) {
          throw new Error(`Devo togliere un seggio a ${list} ma non ho a chi darlo.`);
        }

        const recipient = [...recipientPool].sort((left, right) => {
          const byDecimals = compareDescending(left.DECIMALI, right.DECIMALI);
          if (byDecimals !== 0) return byDecimals;

          const byNationalCifra = compareDescending(left.CIFRA_NAZ, right.CIFRA_NAZ);
          if (byNationalCifra !== 0) return byNationalCifra;

          return 0;
        })[0];
        const source = rows.find((row) => row.LISTA === list && row.CIRCOSCRIZIONE === donor.CIRCOSCRIZIONE);
        const target = rows.find(
          (row) => row.LISTA === recipient.LISTA && row.CIRCOSCRIZIONE === recipient.CIRCOSCRIZIONE
        );

        if (!source || !target) {
          throw new Error('Internal list flipper row lookup failed');
        }

        source.FLIPPER = -1;
        nationalRow.SEGGI_ECCEDENTI_CONTATORE -= 1;
        target.FLIPPER = 1;
        const recipientNational = nationalCounterByList.get(recipient.LISTA);
        if (recipientNational) {
          /*
           * TODO(law-review): the R implementation decrements the recipient
           * counter here. That appears counterintuitive for a deficit list, but
           * preserving it is necessary for golden-master parity.
           */
          recipientNational.SEGGI_ECCEDENTI_CONTATORE -= 1;
        }
      }
    }

    for (const row of rows) {
      row.SEGGI += row.FLIPPER;
    }

    return {
      liste_circ: listeCirc,
      riparto_circ: subjectRows.map((row): InternalCircRipartoSubjectTraceRow => ({
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
        SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
        SEGGI: row.SEGGI,
        CIFRA_AMMESSE_AL_RIPARTO: row.CIFRA_AMMESSE_AL_RIPARTO,
        QUOZIENTE_COAL: row.QUOZIENTE_COAL,
        PARTE_INTERA_TOT: row.PARTE_INTERA_TOT,
        DA_ASSEGNARE_COAL: row.DA_ASSEGNARE_COAL
      })),
      ammesse_circ: rows.map((row): InternalCircAmmesseTraceRow => ({
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
        SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
        LISTA: row.LISTA,
        CIFRA: row.CIFRA,
        QUOZIENTE_COAL: traceNumber(row.QUOZIENTE_COAL_RAW),
        PARTE_INTERA: row.PARTE_INTERA,
        DA_ASSEGNARE_COAL: row.DA_ASSEGNARE_COAL,
        DECIMALI: traceNumber(row.DECIMALI),
        RESTO: null,
        CIFRA_NAZ: row.CIFRA_NAZ,
        ESCLUSE: row.ESCLUSE,
        ORDINE: row.ORDINE,
        SEGGIO_DA_DECIMALI: row.SEGGIO_DA_DECIMALI,
        SEGGIO_DA_RESTO: null,
        FLIPPER: row.FLIPPER,
        SEGGI: row.SEGGI
      })),
      ammesse_naz: nationalRows
    };
  }

  const rows = ammesseWithIntegerPart.map((row) => {
    const subject = subjectByCircSubject.get(keyOf(row.CIRCOSCRIZIONE, row.SOGGETTO_RIPARTO));
    if (!subject) {
      throw new Error(`Missing internal Senate allocation subject for ${row.CIRCOSCRIZIONE}/${row.SOGGETTO_RIPARTO}`);
    }

    return {
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
      LISTA: row.LISTA,
      CIFRA: row.CIFRA,
      QUOZIENTE_COAL_RAW: row.QUOZIENTE_COAL_RAW,
      PARTE_INTERA: row.PARTE_INTERA,
      DA_ASSEGNARE_COAL: subject.DA_ASSEGNARE_COAL,
      DECIMALI: null,
      RESTO: row.CIFRA % row.QUOZIENTE_COAL_RAW,
      CIFRA_NAZ: null,
      ESCLUSE: null,
      ORDINE: 0,
      SEGGIO_DA_DECIMALI: null,
      SEGGIO_DA_RESTO: false,
      FLIPPER: null,
      SEGGI: 0
    };
  });

  rows.sort((left, right) => {
    const byCirc = compareAscending(left.CIRCOSCRIZIONE, right.CIRCOSCRIZIONE);
    if (byCirc !== 0) return byCirc;

    const bySubject = compareAscending(left.SOGGETTO_RIPARTO, right.SOGGETTO_RIPARTO);
    if (bySubject !== 0) return bySubject;

    const byRemainder = compareDescending(left.RESTO, right.RESTO);
    if (byRemainder !== 0) return byRemainder;

    const byCifra = compareDescending(left.CIFRA, right.CIFRA);
    if (byCifra !== 0) return byCifra;

    /*
     * TODO(law-review): art. 17 letter b mentions sorteggio for equal
     * remainders and equal regional figures. R has no explicit draw here.
     */
    return 0;
  });

  const orderByCircSubject = new Map<string, number>();
  for (const row of rows) {
    const groupKey = keyOf(row.CIRCOSCRIZIONE, row.SOGGETTO_RIPARTO);
    const order = (orderByCircSubject.get(groupKey) ?? 0) + 1;
    orderByCircSubject.set(groupKey, order);
    row.ORDINE = order;
    row.SEGGIO_DA_RESTO = row.ORDINE <= row.DA_ASSEGNARE_COAL;
    row.SEGGI = row.PARTE_INTERA + (row.SEGGIO_DA_RESTO ? 1 : 0);
  }

  return {
    liste_circ: listeCirc,
    riparto_circ: subjectRows.map((row): InternalCircRipartoSubjectTraceRow => ({
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
      SEGGI: row.SEGGI,
      CIFRA_AMMESSE_AL_RIPARTO: row.CIFRA_AMMESSE_AL_RIPARTO,
      QUOZIENTE_COAL: row.QUOZIENTE_COAL,
      PARTE_INTERA_TOT: row.PARTE_INTERA_TOT,
      DA_ASSEGNARE_COAL: row.DA_ASSEGNARE_COAL
    })),
    ammesse_circ: rows.map((row): InternalCircAmmesseTraceRow => ({
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      SOGGETTO_RIPARTO: row.SOGGETTO_RIPARTO,
      LISTA: row.LISTA,
      CIFRA: row.CIFRA,
      QUOZIENTE_COAL: traceNumber(row.QUOZIENTE_COAL_RAW),
      PARTE_INTERA: row.PARTE_INTERA,
      DA_ASSEGNARE_COAL: row.DA_ASSEGNARE_COAL,
      DECIMALI: null,
      RESTO: traceNumber(row.RESTO),
      CIFRA_NAZ: null,
      ESCLUSE: null,
      ORDINE: row.ORDINE,
      SEGGIO_DA_DECIMALI: null,
      SEGGIO_DA_RESTO: row.SEGGIO_DA_RESTO,
      FLIPPER: null,
      SEGGI: row.SEGGI
    })),
    ammesse_naz: []
  };
}

function buildPluriRiparto(
  earlyTrace: PoliticsEarlyTrace,
  internalCircRiparto: InternalCircRipartoTrace,
  context: PoliticsScrutinyContext
): PluriRipartoTrace {
  /*
   * Legal basis: Camera DPR 361/1957 art. 83-bis; Senate D.Lgs. 533/1993
   * art. 17 letter c.
   *
   * This stage distributes each circumscription list's seats to its
   * plurinominal colleges and then reconciles college totals back to the
   * seats already assigned at circumscription level. Candidate availability
   * and subentro handling happen in the next stage.
   */
  const admissionByCircList = new Map(
    internalCircRiparto.liste_circ.map((row) => [keyOf(row.CIRCOSCRIZIONE, row.LISTA), row.AMMESSA])
  );
  const listePluri = sortGroupedRows(
    earlyTrace.liste_pluri_cifre.map((row): PluriRipartoListaTraceRow => {
      const admitted = admissionByCircList.get(keyOf(row.CIRCOSCRIZIONE, row.LISTA));
      if (admitted === undefined) {
        throw new Error(`Missing list admission for plurinominal row ${row.CIRCOSCRIZIONE}/${row.LISTA}`);
      }

      return {
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
        COLLEGIOPLURINOMINALE: row.COLLEGIOPLURINOMINALE,
        LISTA: row.LISTA,
        CIFRA: row.CIFRA,
        CIFRA_PERCENTUALE: row.CIFRA_PERCENTUALE,
        AMMESSA: admitted
      };
    }),
    [(row) => row.CIRCOSCRIZIONE, (row) => row.LISTA, (row) => row.COLLEGIOPLURINOMINALE]
  );
  const admittedRows = listePluri.filter((row) => row.AMMESSA);
  const admittedCifraByPluri = sumBy(
    admittedRows,
    (row) => keyOf(row.CIRCOSCRIZIONE, row.COLLEGIOPLURINOMINALE),
    (row) => row.CIFRA
  );
  const rawTotals = sortGroupedRows(
    context.totali_pluri
      .filter((row) => admittedCifraByPluri.has(keyOf(row.CIRCOSCRIZIONE, row.COLLEGIOPLURINOMINALE)))
      .map((row) => {
        const cifra = admittedCifraByPluri.get(keyOf(row.CIRCOSCRIZIONE, row.COLLEGIOPLURINOMINALE)) ?? 0;
        return {
          CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
          COLLEGIOPLURINOMINALE: row.COLLEGIOPLURINOMINALE,
          SEGGI: row.SEGGI,
          CIFRA: cifra,
          QUOZIENTE_RAW: rQuotient(cifra, row.SEGGI),
          PARTE_INTERA: 0,
          DA_ASSEGNARE: 0
        };
      }),
    [(row) => row.CIRCOSCRIZIONE, (row) => row.COLLEGIOPLURINOMINALE]
  );
  const totalByPluri = new Map(rawTotals.map((row) => [keyOf(row.CIRCOSCRIZIONE, row.COLLEGIOPLURINOMINALE), row]));
  const circByCircList = new Map(
    internalCircRiparto.ammesse_circ.map((row) => [keyOf(row.CIRCOSCRIZIONE, row.LISTA), row])
  );
  const rowsWithIntegerPart = admittedRows.map((row) => {
    const total = totalByPluri.get(keyOf(row.CIRCOSCRIZIONE, row.COLLEGIOPLURINOMINALE));
    if (!total) {
      throw new Error(`Missing plurinominal total for ${row.CIRCOSCRIZIONE}/${row.COLLEGIOPLURINOMINALE}`);
    }

    return {
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      COLLEGIOPLURINOMINALE: row.COLLEGIOPLURINOMINALE,
      LISTA: row.LISTA,
      CIFRA: row.CIFRA,
      CIFRA_PERCENTUALE: row.CIFRA_PERCENTUALE,
      QUOZIENTE_RAW: total.QUOZIENTE_RAW,
      PARTE_INTERA: rIntegerDivide(row.CIFRA, total.QUOZIENTE_RAW),
      DECIMALI: (row.CIFRA / total.QUOZIENTE_RAW) % 1
    };
  });
  const integerPartByCircList = sumBy(
    rowsWithIntegerPart,
    (row) => keyOf(row.CIRCOSCRIZIONE, row.LISTA),
    (row) => row.PARTE_INTERA
  );
  const circRows = sortGroupedRows(
    internalCircRiparto.ammesse_circ
      .filter((row) => integerPartByCircList.has(keyOf(row.CIRCOSCRIZIONE, row.LISTA)))
      .map((row) => {
        const integerPart = integerPartByCircList.get(keyOf(row.CIRCOSCRIZIONE, row.LISTA)) ?? 0;
        return {
          CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
          LISTA: row.LISTA,
          CIFRA: row.CIFRA,
          SEGGI: row.SEGGI,
          PARTE_INTERA_PLURI: integerPart,
          ESCLUSE_PLURI: integerPart >= row.SEGGI,
          SEGGI_PLURI: 0,
          SEGGI_ECCEDENTI: 0
        };
      }),
    [(row) => row.CIRCOSCRIZIONE, (row) => row.LISTA]
  );
  const circWorkingByCircList = new Map(circRows.map((row) => [keyOf(row.CIRCOSCRIZIONE, row.LISTA), row]));
  const rows = rowsWithIntegerPart.map((row) => {
    const circRow = circByCircList.get(keyOf(row.CIRCOSCRIZIONE, row.LISTA));
    const circWorking = circWorkingByCircList.get(keyOf(row.CIRCOSCRIZIONE, row.LISTA));
    if (!circRow || !circWorking) {
      throw new Error(`Missing circ allocation for plurinominal row ${row.CIRCOSCRIZIONE}/${row.LISTA}`);
    }

    return {
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      COLLEGIOPLURINOMINALE: row.COLLEGIOPLURINOMINALE,
      LISTA: row.LISTA,
      CIFRA: row.CIFRA,
      CIFRA_PERCENTUALE: row.CIFRA_PERCENTUALE,
      QUOZIENTE_RAW: row.QUOZIENTE_RAW,
      PARTE_INTERA: row.PARTE_INTERA,
      DECIMALI: row.DECIMALI,
      ESCLUSE_PLURI: circWorking.ESCLUSE_PLURI,
      CIFRA_CIRC: circRow.CIFRA,
      DA_ASSEGNARE: 0,
      ORDINE: null as number | null,
      SEGGIO_DA_DECIMALI: false,
      SEGGI: 0,
      SEGGI_ECCEDENTI: 0,
      CEDE: false,
      RICEVE: false,
      ORDINE_CEDE: null as number | null,
      CEDUTO: false,
      ORDINE_RICEVE: null as number | null,
      RICEVUTO: false,
      SEGGI_PRE_SUBENTRI: 0
    };
  });
  const integerPartByPluri = sumBy(
    rows,
    (row) => keyOf(row.CIRCOSCRIZIONE, row.COLLEGIOPLURINOMINALE),
    (row) => row.PARTE_INTERA
  );

  for (const total of rawTotals) {
    const integerPart = integerPartByPluri.get(keyOf(total.CIRCOSCRIZIONE, total.COLLEGIOPLURINOMINALE)) ?? 0;
    total.PARTE_INTERA = integerPart;
    total.DA_ASSEGNARE = total.SEGGI - integerPart;
  }

  const totalByPluriWithRemainders = new Map(
    rawTotals.map((row) => [keyOf(row.CIRCOSCRIZIONE, row.COLLEGIOPLURINOMINALE), row])
  );
  for (const row of rows) {
    const total = totalByPluriWithRemainders.get(keyOf(row.CIRCOSCRIZIONE, row.COLLEGIOPLURINOMINALE));
    if (!total) {
      throw new Error(`Missing plurinominal remainder total for ${row.CIRCOSCRIZIONE}/${row.COLLEGIOPLURINOMINALE}`);
    }
    row.DA_ASSEGNARE = total.DA_ASSEGNARE;
  }

  rows.sort((left, right) => {
    const byCirc = compareAscending(left.CIRCOSCRIZIONE, right.CIRCOSCRIZIONE);
    if (byCirc !== 0) return byCirc;

    const byPluri = compareAscending(left.COLLEGIOPLURINOMINALE, right.COLLEGIOPLURINOMINALE);
    if (byPluri !== 0) return byPluri;

    const byExcluded = compareAscending(Number(left.ESCLUSE_PLURI), Number(right.ESCLUSE_PLURI));
    if (byExcluded !== 0) return byExcluded;

    const byDecimals = compareDescending(left.DECIMALI, right.DECIMALI);
    if (byDecimals !== 0) return byDecimals;

    const byCifra =
      context.ramo === 'camera'
        ? compareDescending(left.CIFRA_CIRC, right.CIFRA_CIRC)
        : compareDescending(left.CIFRA, right.CIFRA);
    if (byCifra !== 0) return byCifra;

    /*
     * TODO(law-review): both art. 83-bis and art. 17 letter c mention
     * sorteggio after equal decimal remainders and equal relevant figures.
     * The R implementation relies on stable order here.
     */
    return 0;
  });

  const orderByPluri = new Map<string, number>();
  for (const row of rows) {
    if (!row.ESCLUSE_PLURI) {
      const groupKey = keyOf(row.CIRCOSCRIZIONE, row.COLLEGIOPLURINOMINALE);
      const order = (orderByPluri.get(groupKey) ?? 0) + 1;
      orderByPluri.set(groupKey, order);
      row.ORDINE = order;
    }

    row.SEGGIO_DA_DECIMALI = row.ORDINE !== null && row.ORDINE <= row.DA_ASSEGNARE;
    row.SEGGI = row.PARTE_INTERA + (row.SEGGIO_DA_DECIMALI ? 1 : 0);
  }

  const seatsByCircList = sumBy(
    rows,
    (row) => keyOf(row.CIRCOSCRIZIONE, row.LISTA),
    (row) => row.SEGGI
  );
  for (const circRow of circRows) {
    const seats = seatsByCircList.get(keyOf(circRow.CIRCOSCRIZIONE, circRow.LISTA)) ?? 0;
    circRow.SEGGI_PLURI = seats;
    circRow.SEGGI_ECCEDENTI = seats - circRow.SEGGI;
  }

  rows.sort((left, right) => {
    const byCirc = compareAscending(left.CIRCOSCRIZIONE, right.CIRCOSCRIZIONE);
    if (byCirc !== 0) return byCirc;

    const byList = compareAscending(left.LISTA, right.LISTA);
    if (byList !== 0) return byList;

    return compareAscending(left.COLLEGIOPLURINOMINALE, right.COLLEGIOPLURINOMINALE);
  });

  for (const row of rows) {
    const circRow = circWorkingByCircList.get(keyOf(row.CIRCOSCRIZIONE, row.LISTA));
    if (!circRow) {
      throw new Error(`Missing circ excess row for plurinominal row ${row.CIRCOSCRIZIONE}/${row.LISTA}`);
    }
    row.SEGGI_ECCEDENTI = circRow.SEGGI_ECCEDENTI;
    row.CEDE = row.SEGGI_ECCEDENTI > 0 && row.SEGGIO_DA_DECIMALI;
    row.RICEVE = row.SEGGI_ECCEDENTI < 0 && !row.SEGGIO_DA_DECIMALI;
  }

  rows.sort((left, right) => {
    const byCirc = compareAscending(left.CIRCOSCRIZIONE, right.CIRCOSCRIZIONE);
    if (byCirc !== 0) return byCirc;

    const byDecimalSeat = compareDescending(Number(left.SEGGIO_DA_DECIMALI), Number(right.SEGGIO_DA_DECIMALI));
    if (byDecimalSeat !== 0) return byDecimalSeat;

    const byExcess = compareDescending(left.SEGGI_ECCEDENTI, right.SEGGI_ECCEDENTI);
    if (byExcess !== 0) return byExcess;

    return compareAscending(left.DECIMALI, right.DECIMALI);
  });

  const donorOrderByCircList = new Map<string, number>();
  for (const row of rows) {
    if (row.CEDE) {
      const groupKey = keyOf(row.CIRCOSCRIZIONE, row.LISTA);
      const order = (donorOrderByCircList.get(groupKey) ?? 0) + 1;
      donorOrderByCircList.set(groupKey, order);
      row.ORDINE_CEDE = order;
    }
    row.CEDUTO = row.ORDINE_CEDE !== null && row.ORDINE_CEDE <= row.SEGGI_ECCEDENTI;
  }

  rows.sort((left, right) => {
    const byCirc = compareAscending(left.CIRCOSCRIZIONE, right.CIRCOSCRIZIONE);
    if (byCirc !== 0) return byCirc;

    const byDecimalSeat = compareAscending(Number(left.SEGGIO_DA_DECIMALI), Number(right.SEGGIO_DA_DECIMALI));
    if (byDecimalSeat !== 0) return byDecimalSeat;

    const byExcess = compareAscending(left.SEGGI_ECCEDENTI, right.SEGGI_ECCEDENTI);
    if (byExcess !== 0) return byExcess;

    return compareDescending(left.DECIMALI, right.DECIMALI);
  });

  const recipientOrderByCircList = new Map<string, number>();
  for (const row of rows) {
    if (row.RICEVE) {
      const groupKey = keyOf(row.CIRCOSCRIZIONE, row.LISTA);
      const order = (recipientOrderByCircList.get(groupKey) ?? 0) + 1;
      recipientOrderByCircList.set(groupKey, order);
      row.ORDINE_RICEVE = order;
    }
    row.RICEVUTO = row.ORDINE_RICEVE !== null && row.ORDINE_RICEVE <= -row.SEGGI_ECCEDENTI;
    row.SEGGI = row.SEGGI - (row.CEDUTO ? 1 : 0) + (row.RICEVUTO ? 1 : 0);
    row.SEGGI_PRE_SUBENTRI = row.SEGGI;
  }

  return {
    liste_pluri: listePluri,
    totali_pluri: rawTotals.map((row): PluriRipartoTotaleTraceRow => ({
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      COLLEGIOPLURINOMINALE: row.COLLEGIOPLURINOMINALE,
      SEGGI: row.SEGGI,
      CIFRA: row.CIFRA,
      QUOZIENTE: traceNumber(row.QUOZIENTE_RAW),
      PARTE_INTERA: row.PARTE_INTERA,
      DA_ASSEGNARE: row.DA_ASSEGNARE
    })),
    ammesse_circ: circRows.map((row): PluriRipartoCircTraceRow => ({
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      LISTA: row.LISTA,
      SEGGI: row.SEGGI,
      PARTE_INTERA_PLURI: row.PARTE_INTERA_PLURI,
      ESCLUSE_PLURI: row.ESCLUSE_PLURI,
      SEGGI_PLURI: row.SEGGI_PLURI,
      SEGGI_ECCEDENTI: row.SEGGI_ECCEDENTI
    })),
    ammesse_pluri: rows.map((row): PluriRipartoAmmesseTraceRow => ({
      CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
      COLLEGIOPLURINOMINALE: row.COLLEGIOPLURINOMINALE,
      LISTA: row.LISTA,
      CIFRA: row.CIFRA,
      CIFRA_PERCENTUALE: row.CIFRA_PERCENTUALE,
      QUOZIENTE: traceNumber(row.QUOZIENTE_RAW),
      PARTE_INTERA: row.PARTE_INTERA,
      DECIMALI: traceNumber(row.DECIMALI),
      ESCLUSE_PLURI: row.ESCLUSE_PLURI,
      CIFRA_CIRC: row.CIFRA_CIRC,
      DA_ASSEGNARE: row.DA_ASSEGNARE,
      ORDINE: row.ORDINE,
      SEGGIO_DA_DECIMALI: row.SEGGIO_DA_DECIMALI,
      SEGGI_ECCEDENTI: row.SEGGI_ECCEDENTI,
      CEDE: row.CEDE,
      RICEVE: row.RICEVE,
      ORDINE_CEDE: row.ORDINE_CEDE,
      CEDUTO: row.CEDUTO,
      ORDINE_RICEVE: row.ORDINE_RICEVE,
      RICEVUTO: row.RICEVUTO,
      SEGGI: row.SEGGI,
      SEGGI_PRE_SUBENTRI: row.SEGGI_PRE_SUBENTRI
    }))
  };
}

function buildPoliticsScrutinyOutput(
  input: PoliticsScrutinyInput,
  trace: PoliticsScrutinyTrace,
  context: PoliticsScrutinyContext
): PoliticsScrutinyOutput {
  /*
   * Legal basis: Camera DPR 361/1957 artt. 84-85; Senate D.Lgs. 533/1993
   * artt. 17-bis and 17-ter.
   *
   * This stage fills the plurinominal seats with available candidates, applies
   * the subentro cascade for exhausted lists, and resolves pluricandidature.
   */
  const electedUniCandidates = new Set(
    trace.candidati_uni_elezione.filter((row) => row.ELETTO).map((row) => row.CANDIDATO)
  );
  const uniGraduatoriaByKey = new Map(
    trace.candidati_uni_graduatoria.map((row) => [candidateKey(row), row])
  );
  const candidatiUni: CandidatoUniNomineWorkingRow[] = input.candidati_uni.map((row) => {
    const graduatoria = uniGraduatoriaByKey.get(candidateKey(row));
    if (!graduatoria) {
      throw new Error(`Missing uninominal ranking row for ${row.COLLEGIOUNINOMINALE}/${row.CANDIDATO}`);
    }

    return {
      ...row,
      ELETTO: graduatoria.ELETTO,
      CIFRA_PERCENTUALE: graduatoria.CIFRA_PERCENTUALE,
      RIPESCATO: false
    };
  });
  let candidatiPluri: CandidatoPluriNomineWorkingRow[] = input.candidati_pluri.map((row) => ({
    ...row,
    DISPONIBILE: !electedUniCandidates.has(row.CANDIDATO),
    ELETTI: 0,
    CIFRA_PERCENTUALE: null,
    ORDINE: null,
    ELETTO: false,
    ELETTO_QUI_O_ALTROVE: false
  }));
  let candidatiPluriHasCifraPercentuale = false;
  const ammesse: PluriNomineWorkingRow[] = trace.pluri_riparto.ammesse_pluri.map((row) => ({
    ...row,
    DECIMALI_USATI: row.SEGGIO_DA_DECIMALI,
    CANDIDATI: 0,
    ELETTI: 0
  }));
  const coalitionByList = new Map(context.liste_naz.map((row) => [row.LISTA, row.COALIZIONE]));
  const listsByCoalition = new Map<string, Set<string>>();
  for (const row of context.liste_naz) {
    if (row.COALIZIONE === null) continue;
    const lists = listsByCoalition.get(row.COALIZIONE) ?? new Set<string>();
    lists.add(row.LISTA);
    listsByCoalition.set(row.COALIZIONE, lists);
  }
  const uniCandidatesByList = new Map<string, Set<string>>();
  for (const row of input.liste_uni) {
    const candidates = uniCandidatesByList.get(row.LISTA) ?? new Set<string>();
    candidates.add(row.CANDIDATO);
    uniCandidatesByList.set(row.LISTA, candidates);
  }

  const ammesseKey = (row: {
    CIRCOSCRIZIONE: AdministrativeCode;
    COLLEGIOPLURINOMINALE: AdministrativeCode;
    LISTA: string;
  }) => keyOf(row.CIRCOSCRIZIONE, row.COLLEGIOPLURINOMINALE, row.LISTA);
  const listSet = (list: string, coal: boolean): Set<string> => {
    if (!coal) return new Set([list]);

    const coalition = coalitionByList.get(list);
    return coalition === undefined || coalition === null
      ? new Set<string>()
      : new Set(listsByCoalition.get(coalition) ?? []);
  };

  const recomputeCandidateCounts = () => {
    const counts = new Map<string, number>();
    for (const candidate of candidatiPluri) {
      if (!candidate.DISPONIBILE) continue;

      const key = ammesseKey(candidate);
      counts.set(key, (counts.get(key) ?? 0) + 1);
    }

    for (const row of ammesse) {
      row.CANDIDATI = counts.get(ammesseKey(row)) ?? 0;
      row.ELETTI = Math.min(row.SEGGI, row.CANDIDATI);
    }

    ammesse.sort((left, right) => {
      const byCirc = compareAscending(String(left.CIRCOSCRIZIONE), String(right.CIRCOSCRIZIONE));
      if (byCirc !== 0) return byCirc;

      const byPluri = compareAscending(String(left.COLLEGIOPLURINOMINALE), String(right.COLLEGIOPLURINOMINALE));
      if (byPluri !== 0) return byPluri;

      return compareAscending(left.LISTA, right.LISTA);
    });
  };
  const updateElectedForRows = (indexes: number[]) => {
    for (const index of indexes) {
      const row = ammesse[index];
      row.ELETTI = Math.min(row.SEGGI, row.CANDIDATI);
    }
  };
  const decimalValue = (row: { DECIMALI: number | null }) => requiredTraceNumber(row.DECIMALI, 'DECIMALI');

  const cercaAccettori = (
    donorIndex: number,
    circ: AdministrativeCode = ammesse[donorIndex].CIRCOSCRIZIONE,
    coal = false,
    pluri = false
  ): boolean => {
    const donor = ammesse[donorIndex];
    const candidateLists = listSet(donor.LISTA, coal);
    const acceptors = ammesse
      .map((row, index) => ({ row, index }))
      .filter(({ row }) => {
        const ambito = pluri
          ? row.COLLEGIOPLURINOMINALE === donor.COLLEGIOPLURINOMINALE
          : row.CIRCOSCRIZIONE === circ;

        return ambito && candidateLists.has(row.LISTA) && row.ELETTI < row.CANDIDATI;
      })
      .sort((left, right) => {
        const byUsedDecimal = compareAscending(Number(left.row.DECIMALI_USATI), Number(right.row.DECIMALI_USATI));
        if (byUsedDecimal !== 0) return byUsedDecimal;

        return compareDescending(decimalValue(left.row), decimalValue(right.row));
      });

    const recipient = acceptors[0];
    if (!recipient) return false;

    donor.SEGGI -= 1;
    recipient.row.SEGGI += 1;
    recipient.row.DECIMALI_USATI = true;
    updateElectedForRows([donorIndex, recipient.index]);
    return true;
  };

  const cercaAccettoriUni = (
    donorIndex: number,
    circ: AdministrativeCode = ammesse[donorIndex].CIRCOSCRIZIONE,
    pluri = false
  ): boolean => {
    const donor = ammesse[donorIndex];
    const listCandidates = uniCandidatesByList.get(donor.LISTA) ?? new Set<string>();
    const acceptors = candidatiUni
      .map((row, index) => ({ row, index }))
      .filter(({ row }) => {
        const ambito = pluri
          ? row.COLLEGIOPLURINOMINALE === donor.COLLEGIOPLURINOMINALE
          : row.CIRCOSCRIZIONE === circ;

        return ambito && !row.ELETTO && listCandidates.has(row.CANDIDATO) && !row.RIPESCATO;
      })
      .sort((left, right) => {
        const byFigure = compareDescending(left.row.CIFRA_PERCENTUALE, right.row.CIFRA_PERCENTUALE);
        if (byFigure !== 0) return byFigure;

        return compareDescending(left.row.DATA_NASCITA, right.row.DATA_NASCITA);
      });

    const recipient = acceptors[0];
    if (!recipient) return false;

    const maxNumber = Math.max(
      0,
      ...candidatiPluri
        .filter(
          (row) => row.LISTA === donor.LISTA && row.COLLEGIOPLURINOMINALE === donor.COLLEGIOPLURINOMINALE
        )
        .map((row) => row.NUMERO)
    );
    candidatiPluri.push({
      LISTA: donor.LISTA,
      CIRCOSCRIZIONE: donor.CIRCOSCRIZIONE,
      COLLEGIOPLURINOMINALE: donor.COLLEGIOPLURINOMINALE,
      NUMERO: maxNumber + 1,
      CANDIDATO: recipient.row.CANDIDATO,
      DISPONIBILE: true,
      ELETTI: 0,
      CIFRA_PERCENTUALE: null,
      ORDINE: null,
      ELETTO: false,
      ELETTO_QUI_O_ALTROVE: false
    });

    donor.CANDIDATI += 1;
    donor.ELETTI += 1;
    recipient.row.RIPESCATO = true;
    return true;
  };

  const cercaNaz = (donorIndex: number, decimaliUsati = false, coal = false, uni = false): void => {
    const donor = ammesse[donorIndex];
    const candidateLists = listSet(donor.LISTA, coal);
    const candidateRows = ammesse.filter(
      (row) => candidateLists.has(row.LISTA) && row.DECIMALI_USATI === decimaliUsati
    );

    if (candidateRows.length === 0) {
      return;
    }

    const maxDecimalByCirc = new Map<AdministrativeCode, number>();
    for (const row of candidateRows) {
      const current = maxDecimalByCirc.get(row.CIRCOSCRIZIONE);
      const rowDecimal = decimalValue(row);
      if (current === undefined || rowDecimal > current) {
        maxDecimalByCirc.set(row.CIRCOSCRIZIONE, rowDecimal);
      }
    }

    const acceptingCircs = [...maxDecimalByCirc.entries()].sort((left, right) => {
      const byDecimal = compareDescending(left[1], right[1]);
      if (byDecimal !== 0) return byDecimal;

      return compareAscending(left[0], right[0]);
    });
    for (const [candidateCirc] of acceptingCircs) {
      const moved = uni
        ? cercaAccettoriUni(donorIndex, candidateCirc)
        : cercaAccettori(donorIndex, candidateCirc, coal);
      if (moved) break;
    }
  };

  const subentro = (livello: 'pluri' | 'circ' | 'naz' = 'circ', uni = false, coal = false): void => {
    const donors = ammesse
      .map((row, index) => ({ row, index }))
      .filter(({ row }) => row.SEGGI - row.ELETTI > 0)
      .map(({ index }) => index);

    for (const donorIndex of donors) {
      let daSpostare = ammesse[donorIndex].SEGGI - ammesse[donorIndex].ELETTI;
      if (daSpostare <= 0) continue;

      if (livello === 'naz') {
        for (let count = 0; count < daSpostare; count += 1) {
          cercaNaz(donorIndex, false, coal, uni);
        }

        daSpostare = ammesse[donorIndex].SEGGI - ammesse[donorIndex].ELETTI;
        if (daSpostare <= 0) continue;

        for (let count = 0; count < daSpostare; count += 1) {
          cercaNaz(donorIndex, true, coal, uni);
        }
        continue;
      }

      if (uni) {
        for (let count = 0; count < daSpostare; count += 1) {
          cercaAccettoriUni(donorIndex, ammesse[donorIndex].CIRCOSCRIZIONE, livello === 'pluri');
        }
        continue;
      }

      /*
       * TODO(law-review): the R subentro() signature accepts `livello` and
       * `coal`, and its messages describe coalition/plurinominal searches.
       * For non-national plurinominal-candidate searches it still calls
       * cerca_accettori(i) without passing either flag. Preserve that behavior
       * for parity before deciding whether the law requires a correction.
       */
      for (let count = 0; count < daSpostare; count += 1) {
        cercaAccettori(donorIndex);
      }
    }
  };

  const runInitialSubentri = () => {
    subentro();
    subentro('pluri', true);
    subentro('circ', true);
    if (context.ramo === 'camera') subentro('naz');
    subentro('pluri', false, true);
    if (context.ramo === 'camera') subentro('naz', true);
    if (context.ramo === 'camera') subentro('naz', false, true);

    /*
     * TODO(law-review): the R implementation notes that these Senate national
     * fallback passes are not provided by the law text, but were used in 2018.
     */
    if (context.ramo === 'senato') subentro('naz');
    if (context.ramo === 'senato') subentro('naz', true);
    if (context.ramo === 'senato') subentro('naz', false, true);
  };
  const runDuplicateSubentri = () => {
    subentro();
    subentro('pluri', true);
    subentro('circ', true);
    if (context.ramo === 'camera') subentro('naz');
    subentro('pluri', false, true);
    subentro('circ', false, true);
    if (context.ramo === 'senato') subentro('naz');
  };
  const assignPluriElected = () => {
    const ammesseByKey = new Map(ammesse.map((row) => [ammesseKey(row), row]));
    candidatiPluri = candidatiPluri.map((candidate) => {
      const baseRow = ammesseByKey.get(ammesseKey(candidate));
      /*
       * TODO(law-review): after the first R merge, CIFRA_PERCENTUALE remains
       * on candidati_pluri and silently becomes part of later merge keys.
       * Uninominal candidates ripescati after that point carry NA and therefore
       * fail to match the plurinominal row. Preserve this behavior for parity.
       */
      const row =
        !candidatiPluriHasCifraPercentuale ||
        (candidate.CIFRA_PERCENTUALE !== null &&
          baseRow !== undefined &&
          candidate.CIFRA_PERCENTUALE === baseRow.CIFRA_PERCENTUALE)
          ? baseRow
          : undefined;
      return {
        ...candidate,
        ELETTI: row?.ELETTI ?? 0,
        CIFRA_PERCENTUALE: row?.CIFRA_PERCENTUALE ?? candidate.CIFRA_PERCENTUALE
      };
    });
    candidatiPluriHasCifraPercentuale = true;

    candidatiPluri.sort((left, right) => {
      const byUnavailable = compareAscending(Number(!left.DISPONIBILE), Number(!right.DISPONIBILE));
      if (byUnavailable !== 0) return byUnavailable;

      const byCirc = compareAscending(left.CIRCOSCRIZIONE, right.CIRCOSCRIZIONE);
      if (byCirc !== 0) return byCirc;

      const byPluri = compareAscending(left.COLLEGIOPLURINOMINALE, right.COLLEGIOPLURINOMINALE);
      if (byPluri !== 0) return byPluri;

      const byList = compareAscending(left.LISTA, right.LISTA);
      if (byList !== 0) return byList;

      return compareAscending(left.NUMERO, right.NUMERO);
    });

    const orderByPluriList = new Map<string, number>();
    for (const candidate of candidatiPluri) {
      candidate.ORDINE = null;
      if (candidate.DISPONIBILE) {
        const groupKey = ammesseKey(candidate);
        const order = (orderByPluriList.get(groupKey) ?? 0) + 1;
        orderByPluriList.set(groupKey, order);
        candidate.ORDINE = order;
      }

      candidate.ELETTO =
        candidate.DISPONIBILE && candidate.ORDINE !== null && candidate.ORDINE <= candidate.ELETTI;
    }
  };

  recomputeCandidateCounts();
  runInitialSubentri();

  for (let iteration = 0; iteration < 100; iteration += 1) {
    assignPluriElected();

    candidatiPluri.sort((left, right) => {
      const byAvailable = compareDescending(Number(left.DISPONIBILE), Number(right.DISPONIBILE));
      if (byAvailable !== 0) return byAvailable;

      const byCandidate = compareAscending(left.CANDIDATO, right.CANDIDATO);
      if (byCandidate !== 0) return byCandidate;

      const byElected = compareDescending(Number(left.ELETTO), Number(right.ELETTO));
      if (byElected !== 0) return byElected;

      return compareNullableNumberAscending(
        left.CIFRA_PERCENTUALE ?? Number.NaN,
        right.CIFRA_PERCENTUALE ?? Number.NaN
      );
    });

    const seenCandidates = new Set<string>();
    const unavailableIndexes: number[] = [];
    for (const [index, candidate] of candidatiPluri.entries()) {
      const duplicated = seenCandidates.has(candidate.CANDIDATO);
      seenCandidates.add(candidate.CANDIDATO);
      if (candidate.ELETTO && candidate.DISPONIBILE && duplicated) {
        unavailableIndexes.push(index);
      }
    }

    if (unavailableIndexes.length === 0) break;

    for (const index of unavailableIndexes) {
      candidatiPluri[index].ELETTO = false;
      candidatiPluri[index].DISPONIBILE = false;
    }

    recomputeCandidateCounts();
    runDuplicateSubentri();
  }

  const electedPluriCandidates = new Set(
    candidatiPluri.filter((candidate) => candidate.ELETTO).map((candidate) => candidate.CANDIDATO)
  );
  const electedHereOrElsewhere = new Set([...electedPluriCandidates, ...electedUniCandidates]);
  for (const candidate of candidatiPluri) {
    candidate.ELETTO_QUI_O_ALTROVE = electedHereOrElsewhere.has(candidate.CANDIDATO);
  }

  const ammesseByKey = new Map(ammesse.map((row) => [ammesseKey(row), row]));
  const maxElectedNumberByPluriList = new Map<string, number>();
  for (const candidate of candidatiPluri) {
    if (!candidate.ELETTO) continue;

    const key = ammesseKey(candidate);
    maxElectedNumberByPluriList.set(key, Math.max(maxElectedNumberByPluriList.get(key) ?? 0, candidate.NUMERO));
  }

  return {
    liste_pluri: trace.pluri_riparto.liste_pluri.map((row): ListePluriResultRow => {
      const ammesseRow = ammesseByKey.get(ammesseKey(row));
      const seatsBeforeSubentri = ammesseRow?.SEGGI_PRE_SUBENTRI ?? 0;
      return {
        CIRCOSCRIZIONE: row.CIRCOSCRIZIONE,
        COLLEGIOPLURINOMINALE: row.COLLEGIOPLURINOMINALE,
        LISTA: row.LISTA,
        ELETTI: ammesseRow?.ELETTI ?? 0,
        NUMERO_MAX: Math.max(maxElectedNumberByPluriList.get(ammesseKey(row)) ?? 0, seatsBeforeSubentri),
        SEGGI_PRE_SUBENTRI: seatsBeforeSubentri
      };
    }),
    candidati_uni: trace.candidati_uni_elezione,
    candidati_pluri: candidatiPluri.map((candidate): CandidatoPluriResultRow => ({
      CIRCOSCRIZIONE: candidate.CIRCOSCRIZIONE,
      COLLEGIOPLURINOMINALE: candidate.COLLEGIOPLURINOMINALE,
      LISTA: candidate.LISTA,
      NUMERO: candidate.NUMERO,
      CANDIDATO: candidate.CANDIDATO,
      ELETTO: candidate.ELETTO,
      ELETTO_QUI_O_ALTROVE: candidate.ELETTO_QUI_O_ALTROVE
    }))
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
  const cameraRiparto = buildCameraRiparto(thresholds, earlyTrace.candidati_uni_elezione, context);
  const circRiparto = buildCircRiparto(thresholds, cameraRiparto, context);
  const internalCircRiparto = buildInternalCircRiparto(thresholds, circRiparto, cameraRiparto, context);
  const pluriRiparto = buildPluriRiparto(earlyTrace, internalCircRiparto, context);

  return {
    totale_naz: thresholds.totaleNaz,
    ...earlyTrace,
    liste_naz_soglie: thresholds.listeNaz,
    liste_circ_soglie: thresholds.listeCirc,
    coal_naz_soglie: thresholds.coalNaz,
    coal_circ_cifre: thresholds.coalCirc,
    camera_riparto: cameraRiparto,
    circ_riparto: circRiparto,
    internal_circ_riparto: internalCircRiparto,
    pluri_riparto: pluriRiparto
  };
}

export function runPoliticsScrutiny(input: PoliticsScrutinyInput, context: PoliticsScrutinyContext): PoliticsScrutinyOutput {
  const trace = runPoliticsScrutinyTrace(input, context);
  return buildPoliticsScrutinyOutput(input, trace, context);
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
