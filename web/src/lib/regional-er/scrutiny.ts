import { hareNiemeyerDetails } from '$lib/core/allocation';
import { createSeededRng, type Rng } from '$lib/core/rng';
import type {
  RegionalErCoalitionResultRow,
  RegionalErLegalPopulationRow,
  RegionalErListParameterRow,
  RegionalErListResultRow,
  RegionalErProvinceListResultRow,
  RegionalErScrutinyInputRow,
  RegionalErScrutinyOutput
} from './types';

interface ProvinceWorkRow {
  CODICE_PROVINCIA: string | number;
  PROVINCIA: string;
  POPOLAZIONE: number;
  seggi_proporzionali: number;
  VOTI_UTILI: number;
  QUOZIENTE_1: number;
  SEGGI_1: number;
  QUOZIENTE_2: number;
  USA_2: boolean;
  SEGGI_CIRC: number;
}

interface ListWorkRow {
  LISTA: string;
  COALIZIONE: string | null;
  VOTI_LISTA_ITER: number;
  PERCENTUALE_DEI_VOTI_VALIDI: number;
  SOGLIA_3: boolean;
  SOGLIA_COALIZIONE: boolean;
  SOGLIA: boolean;
  VOTI_UTILI: number;
  VOTI_RESIDUATI: number;
  SEGGI_DA_VOTI_RESIDUATI: number;
  RESTI_VOTI_RESIDUATI: number;
  SEGGI_DA_RESTI_VOTI_RESIDUATI: number;
  CLASSIFICA: number;
  SEGGI_CIRC: number;
  SEGGI_40: number;
  SEGGI_BONUS: number;
  SEGGI_BONUS_RESTI: number;
  SEGGI_BONUS_DA_RESTI: number;
  SEGGI_CIRC_DA_TOGLIERE: number;
}

interface CoalitionWorkRow {
  COALIZIONE: string;
  VOTI_LISTA_ITER: number;
  PERCENTUALE_DEI_VOTI_VALIDI: number;
  SOGLIA_COALIZIONE: boolean;
  CLASSIFICA: number;
  VOTI_UTILI: number;
  SEGGI_40: number;
  PRESIDENTE: boolean;
  MIGLIOR_PERDENTE: boolean;
}

interface ProvinceListWorkRow {
  CODICE_PROVINCIA: string | number;
  PROVINCIA: string;
  LISTA: string;
  VOTI_LISTA_ITER: number;
  SOGLIA: boolean;
  VOTI_UTILI: number;
  QUOZIENTE_1: number;
  SEGGI_1: number;
  QUOZIENTE_2: number;
  SEGGI_2: number;
  USA_2: boolean;
  SEGGI_CIRC: number;
  VOTI_RESIDUATI: number;
  CANDIDATI: number;
  VOTI_RESIDUATI_RELATIVI: number;
  SEGGI_DA_VOTI_RESIDUATI: number;
  ORDINE_VOTI_RESIDUATI: number;
  ORDINE_INVERSO_VR: number;
  ELETTI: number;
}

function key(...values: readonly unknown[]): string {
  return values.map((value) => String(value)).join('\u001f');
}

function sum(values: readonly number[]): number {
  return values.reduce((total, value) => total + value, 0);
}

function groupSum<T>(rows: readonly T[], keyFor: (row: T) => string, valueFor: (row: T) => number): Map<string, number> {
  const totals = new Map<string, number>();
  for (const row of rows) totals.set(keyFor(row), (totals.get(keyFor(row)) ?? 0) + valueFor(row));
  return totals;
}

function coalitionName(value: string | null): string {
  return value ?? '';
}

function sampleIndex(indexes: readonly number[], rng: Rng): number {
  if (indexes.length === 0) throw new Error('Cannot sample from an empty regional tie set');
  if (indexes.length === 1) return indexes[0];
  return indexes[Math.min(Math.floor(rng() * indexes.length), indexes.length - 1)];
}

function rankDescendingRandom(values: readonly number[], rng: Rng): number[] {
  return values
    .map((value, index) => ({ value, index, draw: rng() }))
    .sort((left, right) => {
      if (right.value !== left.value) return right.value - left.value;
      return right.draw - left.draw;
    })
    .reduce((ranks, row, index) => {
      ranks[row.index] = index + 1;
      return ranks;
    }, Array.from({ length: values.length }, () => 0));
}

function aggregateProvinceListVotes(rows: readonly RegionalErScrutinyInputRow[]): ProvinceListWorkRow[] {
  const totals = new Map<string, ProvinceListWorkRow>();

  for (const row of rows) {
    if (row.LISTA === 'astensione') continue;
    const rowKey = key(row.CODICE_PROVINCIA, row.PROVINCIA, row.LISTA);
    const existing = totals.get(rowKey);
    if (existing) {
      existing.VOTI_LISTA_ITER += row.VOTI_LISTA_SIM;
    } else {
      totals.set(rowKey, {
        CODICE_PROVINCIA: row.CODICE_PROVINCIA,
        PROVINCIA: row.PROVINCIA,
        LISTA: row.LISTA,
        VOTI_LISTA_ITER: row.VOTI_LISTA_SIM,
        SOGLIA: false,
        VOTI_UTILI: 0,
        QUOZIENTE_1: 0,
        SEGGI_1: 0,
        QUOZIENTE_2: 0,
        SEGGI_2: 0,
        USA_2: false,
        SEGGI_CIRC: 0,
        VOTI_RESIDUATI: 0,
        CANDIDATI: 0,
        VOTI_RESIDUATI_RELATIVI: 0,
        SEGGI_DA_VOTI_RESIDUATI: 0,
        ORDINE_VOTI_RESIDUATI: 0,
        ORDINE_INVERSO_VR: 0,
        ELETTI: 0
      });
    }
  }

  return [...totals.values()].sort((left, right) =>
    key(left.CODICE_PROVINCIA, left.PROVINCIA, left.LISTA).localeCompare(key(right.CODICE_PROVINCIA, right.PROVINCIA, right.LISTA), 'it')
  );
}

function buildProvinceRows(popLegale: readonly RegionalErLegalPopulationRow[], rng: Rng): ProvinceWorkRow[] {
  const grouped = new Map<string, ProvinceWorkRow>();

  for (const row of popLegale) {
    const rowKey = key(row.CODICE_PROVINCIA, row.PROVINCIA);
    const existing = grouped.get(rowKey);
    if (existing) {
      existing.POPOLAZIONE += row.POPOLAZIONE;
    } else {
      grouped.set(rowKey, {
        CODICE_PROVINCIA: row.CODICE_PROVINCIA,
        PROVINCIA: row.PROVINCIA,
        POPOLAZIONE: row.POPOLAZIONE,
        seggi_proporzionali: 0,
        VOTI_UTILI: 0,
        QUOZIENTE_1: 0,
        SEGGI_1: 0,
        QUOZIENTE_2: 0,
        USA_2: false,
        SEGGI_CIRC: 0
      });
    }
  }

  const rows = [...grouped.values()].sort((left, right) =>
    key(left.CODICE_PROVINCIA, left.PROVINCIA).localeCompare(key(right.CODICE_PROVINCIA, right.PROVINCIA), 'it')
  );
  const seats = hareNiemeyerDetails(rows.map((row) => row.POPOLAZIONE), 40, rng).assigned;
  rows.forEach((row, index) => {
    row.seggi_proporzionali = seats[index];
  });

  return rows;
}

function buildListRows(
  liste: readonly RegionalErListParameterRow[],
  provLista: readonly ProvinceListWorkRow[]
): ListWorkRow[] {
  const votesByList = groupSum(provLista, (row) => row.LISTA, (row) => row.VOTI_LISTA_ITER);
  const validVotes = sum([...votesByList.values()]);

  return liste
    .filter((row) => votesByList.has(row.LISTA))
    .map((row) => {
      const votes = votesByList.get(row.LISTA) ?? 0;
      const percentage = validVotes > 0 ? votes / validVotes : 0;
      return {
        LISTA: row.LISTA,
        COALIZIONE: row.COALIZIONE,
        VOTI_LISTA_ITER: votes,
        PERCENTUALE_DEI_VOTI_VALIDI: percentage,
        SOGLIA_3: percentage >= 0.03,
        SOGLIA_COALIZIONE: false,
        SOGLIA: false,
        VOTI_UTILI: 0,
        VOTI_RESIDUATI: 0,
        SEGGI_DA_VOTI_RESIDUATI: 0,
        RESTI_VOTI_RESIDUATI: 0,
        SEGGI_DA_RESTI_VOTI_RESIDUATI: 0,
        CLASSIFICA: 0,
        SEGGI_CIRC: 0,
        SEGGI_40: 0,
        SEGGI_BONUS: 0,
        SEGGI_BONUS_RESTI: 0,
        SEGGI_BONUS_DA_RESTI: 0,
        SEGGI_CIRC_DA_TOGLIERE: 0
      };
    })
    .sort((left, right) => key(left.COALIZIONE, left.LISTA).localeCompare(key(right.COALIZIONE, right.LISTA), 'it'));
}

function buildCoalitionRows(liste: readonly ListWorkRow[], rng: Rng): CoalitionWorkRow[] {
  const votesByCoalition = groupSum(liste, (row) => coalitionName(row.COALIZIONE), (row) => row.VOTI_LISTA_ITER);
  const totalVotes = sum([...votesByCoalition.values()]);
  const rows = [...votesByCoalition.entries()]
    .map(([coalition, votes]) => ({
      COALIZIONE: coalition,
      VOTI_LISTA_ITER: votes,
      PERCENTUALE_DEI_VOTI_VALIDI: totalVotes > 0 ? votes / totalVotes : 0,
      SOGLIA_COALIZIONE: totalVotes > 0 ? votes / totalVotes >= 0.05 : false,
      CLASSIFICA: 0,
      VOTI_UTILI: 0,
      SEGGI_40: 0,
      PRESIDENTE: false,
      MIGLIOR_PERDENTE: false
    }))
    .sort((left, right) => left.COALIZIONE.localeCompare(right.COALIZIONE, 'it'));
  const ranks = rankDescendingRandom(rows.map((row) => row.VOTI_LISTA_ITER), rng);
  rows.forEach((row, index) => {
    row.CLASSIFICA = ranks[index];
  });
  return rows;
}

function rowByList(rows: readonly ListWorkRow[]): Map<string, ListWorkRow> {
  return new Map(rows.map((row) => [row.LISTA, row]));
}

function rowByCoalition(rows: readonly CoalitionWorkRow[]): Map<string, CoalitionWorkRow> {
  return new Map(rows.map((row) => [row.COALIZIONE, row]));
}

function rowByProvince(rows: readonly ProvinceWorkRow[]): Map<string, ProvinceWorkRow> {
  return new Map(rows.map((row) => [row.PROVINCIA, row]));
}

function applyThresholds(liste: ListWorkRow[], coalizioni: readonly CoalitionWorkRow[], provLista: ProvinceListWorkRow[]): void {
  const coalitionByName = rowByCoalition(coalizioni);
  const listByName = rowByList(liste);

  for (const row of liste) {
    row.SOGLIA_COALIZIONE = coalitionByName.get(coalitionName(row.COALIZIONE))?.SOGLIA_COALIZIONE ?? false;
    row.SOGLIA = row.SOGLIA_3 || row.SOGLIA_COALIZIONE;
    row.VOTI_UTILI = row.SOGLIA ? row.VOTI_LISTA_ITER : 0;
  }

  for (const row of provLista) {
    row.SOGLIA = listByName.get(row.LISTA)?.SOGLIA ?? false;
    row.VOTI_UTILI = row.SOGLIA ? row.VOTI_LISTA_ITER : 0;
  }
}

function applyProvincialSeats(province: ProvinceWorkRow[], provLista: ProvinceListWorkRow[]): number {
  const provinceByName = rowByProvince(province);
  const usefulByProvince = groupSum(provLista, (row) => row.PROVINCIA, (row) => row.VOTI_UTILI);

  for (const row of province) {
    row.VOTI_UTILI = usefulByProvince.get(row.PROVINCIA) ?? 0;
    row.QUOZIENTE_1 = Math.floor(row.VOTI_UTILI / (row.seggi_proporzionali + 1));
    row.QUOZIENTE_2 = Math.floor(row.VOTI_UTILI / row.seggi_proporzionali);
  }

  for (const row of provLista) {
    const provinceRow = provinceByName.get(row.PROVINCIA);
    row.QUOZIENTE_1 = provinceRow?.QUOZIENTE_1 ?? 0;
    row.QUOZIENTE_2 = provinceRow?.QUOZIENTE_2 ?? 0;
    row.SEGGI_1 = row.QUOZIENTE_1 > 0 ? Math.floor(row.VOTI_UTILI / row.QUOZIENTE_1) : 0;
    row.SEGGI_2 = row.QUOZIENTE_2 > 0 ? Math.floor(row.VOTI_UTILI / row.QUOZIENTE_2) : 0;
  }

  const seats1ByProvince = groupSum(provLista, (row) => row.PROVINCIA, (row) => row.SEGGI_1);
  for (const row of province) {
    row.SEGGI_1 = seats1ByProvince.get(row.PROVINCIA) ?? 0;
    row.USA_2 = row.SEGGI_1 > row.seggi_proporzionali;
  }

  for (const row of provLista) {
    const provinceRow = provinceByName.get(row.PROVINCIA);
    row.USA_2 = provinceRow?.USA_2 ?? false;
    row.SEGGI_CIRC = row.USA_2 ? row.SEGGI_2 : row.SEGGI_1;
    row.VOTI_RESIDUATI =
      row.VOTI_UTILI - (row.USA_2 ? row.QUOZIENTE_2 * row.SEGGI_2 : row.QUOZIENTE_1 * row.SEGGI_1);
  }

  const seatsCircByProvince = groupSum(provLista, (row) => row.PROVINCIA, (row) => row.SEGGI_CIRC);
  for (const row of province) row.SEGGI_CIRC = seatsCircByProvince.get(row.PROVINCIA) ?? 0;

  return sum(province.map((row) => row.seggi_proporzionali)) - sum(province.map((row) => row.SEGGI_CIRC));
}

function applyResidualSeats(liste: ListWorkRow[], provLista: readonly ProvinceListWorkRow[], seats: number, rng: Rng): void {
  const residualByList = groupSum(provLista, (row) => row.LISTA, (row) => row.VOTI_RESIDUATI);
  for (const row of liste) row.VOTI_RESIDUATI = residualByList.get(row.LISTA) ?? 0;

  const details = hareNiemeyerDetails(liste.map((row) => row.VOTI_RESIDUATI), seats, rng);
  liste.forEach((row, index) => {
    row.SEGGI_DA_VOTI_RESIDUATI = details.assigned[index];
    row.RESTI_VOTI_RESIDUATI = details.remainders[index];
    row.SEGGI_DA_RESTI_VOTI_RESIDUATI = details.remainderSeats[index];
  });
}

function applyCoalitionTotals(liste: ListWorkRow[], coalizioni: CoalitionWorkRow[], provLista: readonly ProvinceListWorkRow[]): void {
  const coalitionByName = rowByCoalition(coalizioni);
  const seatsCircByList = groupSum(provLista, (row) => row.LISTA, (row) => row.SEGGI_CIRC);

  for (const row of liste) {
    row.SEGGI_CIRC = seatsCircByList.get(row.LISTA) ?? 0;
    row.SEGGI_40 = row.SEGGI_CIRC + row.SEGGI_DA_VOTI_RESIDUATI;
  }

  const usefulByCoalition = groupSum(liste, (row) => coalitionName(row.COALIZIONE), (row) => row.VOTI_UTILI);
  const seatsByCoalition = groupSum(liste, (row) => coalitionName(row.COALIZIONE), (row) => row.SEGGI_40);
  for (const row of coalizioni) {
    row.VOTI_UTILI = usefulByCoalition.get(row.COALIZIONE) ?? 0;
    row.SEGGI_40 = seatsByCoalition.get(row.COALIZIONE) ?? 0;
  }

  for (const row of liste) row.CLASSIFICA = coalitionByName.get(coalitionName(row.COALIZIONE))?.CLASSIFICA ?? 0;
}

function applySubsetHareNiemeyer(
  rows: ListWorkRow[],
  rowIndexes: readonly number[],
  voteSelector: (row: ListWorkRow) => number,
  seats: number,
  rng: Rng,
  target: 'bonus' | 'winner-bonus'
): void {
  const details = hareNiemeyerDetails(rowIndexes.map((index) => voteSelector(rows[index])), seats, rng);
  rowIndexes.forEach((rowIndex, detailIndex) => {
    rows[rowIndex].SEGGI_BONUS = details.assigned[detailIndex];
    if (target === 'bonus') {
      rows[rowIndex].SEGGI_BONUS_RESTI = details.remainders[detailIndex];
      rows[rowIndex].SEGGI_BONUS_DA_RESTI = details.remainderSeats[detailIndex];
    }
  });
}

function listIndexes(rows: readonly ListWorkRow[], predicate: (row: ListWorkRow) => boolean): number[] {
  const indexes: number[] = [];
  rows.forEach((row, index) => {
    if (predicate(row)) indexes.push(index);
  });
  return indexes;
}

function indexWithMinimum(
  rows: readonly ListWorkRow[],
  indexes: readonly number[],
  valueSelector: (row: ListWorkRow) => number,
  rng: Rng
): number {
  const minimum = Math.min(...indexes.map((index) => valueSelector(rows[index])));
  return sampleIndex(indexes.filter((index) => valueSelector(rows[index]) === minimum), rng);
}

function removeSeatForMajorityGuarantee(liste: ListWorkRow[], rng: Rng): void {
  if (sum(liste.filter((row) => row.CLASSIFICA !== 1).map((row) => row.SEGGI_DA_RESTI_VOTI_RESIDUATI)) > 0) {
    const index = indexWithMinimum(
      liste,
      listIndexes(liste, (row) => row.CLASSIFICA !== 1 && row.SEGGI_DA_RESTI_VOTI_RESIDUATI > 0),
      (row) => row.RESTI_VOTI_RESIDUATI,
      rng
    );
    liste[index].SEGGI_DA_RESTI_VOTI_RESIDUATI -= 1;
    liste[index].SEGGI_DA_VOTI_RESIDUATI -= 1;
  } else if (sum(liste.filter((row) => row.CLASSIFICA !== 1).map((row) => row.SEGGI_DA_VOTI_RESIDUATI)) > 0) {
    const index = indexWithMinimum(
      liste,
      listIndexes(liste, (row) => row.CLASSIFICA !== 1 && row.SEGGI_DA_VOTI_RESIDUATI > 0),
      (row) => row.VOTI_RESIDUATI,
      rng
    );
    liste[index].SEGGI_DA_VOTI_RESIDUATI -= 1;
  } else {
    const index = indexWithMinimum(
      liste,
      listIndexes(liste, (row) => row.CLASSIFICA !== 1 && row.SEGGI_CIRC > 0),
      (row) => row.VOTI_UTILI,
      rng
    );
    liste[index].SEGGI_CIRC -= 1;
    liste[index].SEGGI_CIRC_DA_TOGLIERE += 1;
  }
}

function reserveRunnerUpSeat(liste: ListWorkRow[], rng: Rng): void {
  if (sum(liste.filter((row) => row.CLASSIFICA === 2).map((row) => row.SEGGI_BONUS_DA_RESTI)) > 0) {
    const index = indexWithMinimum(
      liste,
      listIndexes(liste, (row) => row.CLASSIFICA === 2 && row.SEGGI_BONUS_DA_RESTI > 0),
      (row) => row.SEGGI_BONUS_RESTI,
      rng
    );
    liste[index].SEGGI_BONUS_DA_RESTI -= 1;
    liste[index].SEGGI_BONUS -= 1;
  } else if (sum(liste.filter((row) => row.CLASSIFICA === 2).map((row) => row.SEGGI_DA_RESTI_VOTI_RESIDUATI)) > 0) {
    const index = indexWithMinimum(
      liste,
      listIndexes(liste, (row) => row.CLASSIFICA === 2 && row.SEGGI_DA_RESTI_VOTI_RESIDUATI > 0),
      (row) => row.RESTI_VOTI_RESIDUATI,
      rng
    );
    liste[index].SEGGI_DA_RESTI_VOTI_RESIDUATI -= 1;
    liste[index].SEGGI_DA_VOTI_RESIDUATI -= 1;
  } else if (sum(liste.filter((row) => row.CLASSIFICA === 2).map((row) => row.SEGGI_DA_VOTI_RESIDUATI)) > 0) {
    const index = indexWithMinimum(
      liste,
      listIndexes(liste, (row) => row.CLASSIFICA === 2 && row.SEGGI_DA_VOTI_RESIDUATI > 0),
      (row) => row.VOTI_RESIDUATI,
      rng
    );
    liste[index].SEGGI_DA_VOTI_RESIDUATI -= 1;
  } else {
    const index = indexWithMinimum(
      liste,
      listIndexes(liste, (row) => row.CLASSIFICA === 2 && row.SEGGI_CIRC > 0),
      (row) => row.VOTI_UTILI,
      rng
    );
    liste[index].SEGGI_CIRC -= 1;
    liste[index].SEGGI_CIRC_DA_TOGLIERE += 1;
  }
}

function applyMajorityBonus(liste: ListWorkRow[], coalizioni: readonly CoalitionWorkRow[], rng: Rng): void {
  const winner = coalizioni.find((row) => row.CLASSIFICA === 1);
  if (!winner) return;

  let bonusVincitori = winner.SEGGI_40 > 24 ? 4 : 9;
  const bonusVinti = winner.SEGGI_40 > 24 ? 5 : 0;

  applySubsetHareNiemeyer(
    liste,
    listIndexes(liste, (row) => row.CLASSIFICA !== 1),
    (row) => row.VOTI_UTILI,
    bonusVinti,
    rng,
    'bonus'
  );

  if (
    winner.VOTI_UTILI < 0.4 * sum(coalizioni.map((row) => row.VOTI_UTILI)) &&
    winner.SEGGI_40 + bonusVincitori < 27
  ) {
    let seatsToMove = 27 - (winner.SEGGI_40 + bonusVincitori);
    bonusVincitori += seatsToMove;

    while (seatsToMove > 0) {
      removeSeatForMajorityGuarantee(liste, rng);
      seatsToMove -= 1;
    }
  }

  applySubsetHareNiemeyer(
    liste,
    listIndexes(liste, (row) => row.CLASSIFICA === 1),
    (row) => row.VOTI_UTILI,
    bonusVincitori,
    rng,
    'winner-bonus'
  );

  reserveRunnerUpSeat(liste, rng);
}

function applyProvincePlacement(
  liste: readonly ListWorkRow[],
  province: readonly ProvinceWorkRow[],
  provLista: ProvinceListWorkRow[],
  rng: Rng
): void {
  const provinceByName = rowByProvince(province);
  const provinceCount = province.length;

  for (const row of provLista) {
    row.CANDIDATI = provinceByName.get(row.PROVINCIA)?.seggi_proporzionali ?? 0;
    const quota = row.USA_2 ? row.QUOZIENTE_2 : row.QUOZIENTE_1;
    row.VOTI_RESIDUATI_RELATIVI = quota > 0 ? row.VOTI_RESIDUATI / quota : 0;
  }

  const rowsByList = new Map<string, ProvinceListWorkRow[]>();
  for (const row of provLista) {
    const rows = rowsByList.get(row.LISTA) ?? [];
    rows.push(row);
    rowsByList.set(row.LISTA, rows);
  }

  for (const rows of rowsByList.values()) {
    const ranks = rankDescendingRandom(rows.map((row) => row.VOTI_RESIDUATI_RELATIVI), rng);
    rows.forEach((row, index) => {
      row.ORDINE_VOTI_RESIDUATI = ranks[index];
      row.ORDINE_INVERSO_VR = 1 + provinceCount - ranks[index];
    });
  }

  for (const listRow of liste) {
    let seatsToRemove = listRow.SEGGI_CIRC_DA_TOGLIERE;
    let cursor = 0;

    while (seatsToRemove > 0) {
      const order = (cursor % provinceCount) + 1;
      const row = provLista.find((candidate) => candidate.LISTA === listRow.LISTA && candidate.ORDINE_INVERSO_VR === order);
      if (row && row.SEGGI_CIRC > 0) {
        row.SEGGI_CIRC -= 1;
        seatsToRemove -= 1;
        row.VOTI_RESIDUATI += row.USA_2 ? row.QUOZIENTE_2 : row.QUOZIENTE_1;
      }
      cursor += 1;
      if (cursor > 1000) {
        throw new Error('Art. 13 comma 1 lettera c: impossibile togliere tutti i seggi alle circoscrizioni');
      }
    }

    let seatsToAssign = listRow.SEGGI_DA_VOTI_RESIDUATI + listRow.SEGGI_BONUS;
    cursor = 0;

    while (seatsToAssign > 0) {
      const order = (cursor % provinceCount) + 1;
      const row = provLista.find((candidate) => candidate.LISTA === listRow.LISTA && candidate.ORDINE_VOTI_RESIDUATI === order);
      if (row && row.CANDIDATI - row.SEGGI_CIRC - row.SEGGI_DA_VOTI_RESIDUATI > 0) {
        row.SEGGI_DA_VOTI_RESIDUATI += 1;
        seatsToAssign -= 1;
      }
      cursor += 1;
      if (cursor > 1000) {
        throw new Error('Art. 13 comma 1 lettera c: impossibile assegnare tutti i seggi alle circoscrizioni');
      }
    }
  }

  for (const row of provLista) row.ELETTI = row.SEGGI_CIRC + row.SEGGI_DA_VOTI_RESIDUATI;
}

function buildFinalProvinceListRows(provLista: readonly ProvinceListWorkRow[]): RegionalErProvinceListResultRow[] {
  const totalByProvince = groupSum(provLista, (row) => row.PROVINCIA, (row) => row.VOTI_LISTA_ITER);
  return provLista
    .map((row) => ({
      PROVINCIA: row.PROVINCIA,
      LISTA: row.LISTA,
      ELETTI: row.ELETTI,
      VOTI_LISTA_ITER: row.VOTI_LISTA_ITER,
      PERCENTUALE: (totalByProvince.get(row.PROVINCIA) ?? 0) > 0 ? row.VOTI_LISTA_ITER / (totalByProvince.get(row.PROVINCIA) as number) : 0
    }))
    .sort((left, right) => key(left.PROVINCIA, left.LISTA).localeCompare(key(right.PROVINCIA, right.LISTA), 'it'));
}

function buildFinalListRows(liste: readonly ListWorkRow[], provLista: readonly ProvinceListWorkRow[]): RegionalErListResultRow[] {
  const coalitionByList = new Map(liste.map((row) => [row.LISTA, row.COALIZIONE]));
  const totals = new Map<string, { votes: number; seats: number }>();

  for (const row of provLista) {
    const existing = totals.get(row.LISTA) ?? { votes: 0, seats: 0 };
    existing.votes += row.VOTI_LISTA_ITER;
    existing.seats += row.ELETTI;
    totals.set(row.LISTA, existing);
  }

  const totalVotes = sum([...totals.values()].map((row) => row.votes));
  return [...totals.entries()]
    .map(([list, row]) => ({
      LISTA: list,
      COALIZIONE: coalitionByList.get(list) ?? null,
      VOTI_LISTA_ITER: row.votes,
      PERCENTUALE: totalVotes > 0 ? row.votes / totalVotes : 0,
      ELETTI: row.seats
    }))
    .sort((left, right) => left.LISTA.localeCompare(right.LISTA, 'it'));
}

function buildFinalCoalitionRows(
  coalizioni: readonly CoalitionWorkRow[],
  liste: readonly RegionalErListResultRow[]
): RegionalErCoalitionResultRow[] {
  const flagsByCoalition = rowByCoalition(coalizioni);
  const totals = new Map<string, { votes: number; seats: number }>();

  for (const row of liste) {
    const coalition = coalitionName(row.COALIZIONE);
    const existing = totals.get(coalition) ?? { votes: 0, seats: 0 };
    existing.votes += row.VOTI_LISTA_ITER;
    existing.seats += row.ELETTI;
    totals.set(coalition, existing);
  }

  const totalVotes = sum([...totals.values()].map((row) => row.votes));
  return [...totals.entries()]
    .map(([coalition, row]) => {
      const flags = flagsByCoalition.get(coalition);
      const president = flags?.CLASSIFICA === 1;
      const runnerUp = flags?.CLASSIFICA === 2;
      return {
        COALIZIONE: coalition,
        PRESIDENTE: president,
        MIGLIOR_PERDENTE: runnerUp,
        VOTI_LISTA_ITER: row.votes,
        PERCENTUALE: totalVotes > 0 ? row.votes / totalVotes : 0,
        ELETTI: row.seats,
        ELETTI_TOT: row.seats + (president ? 1 : 0) + (runnerUp ? 1 : 0)
      };
    })
    .sort((left, right) => left.COALIZIONE.localeCompare(right.COALIZIONE, 'it'));
}

export function runRegionalErScrutiny(
  input: {
    comuni_liste: readonly RegionalErScrutinyInputRow[];
  },
  context: {
    pop_legale: readonly RegionalErLegalPopulationRow[];
    liste: readonly RegionalErListParameterRow[];
  },
  options: {
    rng?: Rng;
    seed?: string | number;
  } = {}
): RegionalErScrutinyOutput {
  const rng = options.rng ?? createSeededRng(options.seed ?? 'regional-er-scrutiny');
  const provLista = aggregateProvinceListVotes(input.comuni_liste);

  /*
   * Emilia-Romagna regional law, art. 3: forty councillors are assigned
   * proportionally across provincial districts by legal population.
   */
  const province = buildProvinceRows(context.pop_legale, rng);

  /*
   * Art. 11: lists below 3% regionally are excluded unless linked to a
   * presidential coalition/list group reaching 5%.
   */
  const liste = buildListRows(context.liste, provLista);
  const coalizioni = buildCoalitionRows(liste, rng);
  applyThresholds(liste, coalizioni, provLista);

  /*
   * Art. 12 and art. 13 comma 1: provincial quotient allocation first,
   * then regional recovery of residual votes.
   */
  const residualSeats = applyProvincialSeats(province, provLista);
  applyResidualSeats(liste, provLista, residualSeats, rng);
  applyCoalitionTotals(liste, coalizioni, provLista);

  /*
   * Art. 13 comma 2 and 3: presidential ranking, majority bonus/guarantee,
   * and the reserved seat for the best losing presidential candidate.
   */
  applyMajorityBonus(liste, coalizioni, rng);

  /*
   * Art. 13 comma 1 letter c: residual and bonus seats are placed in
   * provincial lists by decreasing relative residual votes, capped by the
   * available candidate slots. The current R code uses the province's
   * proportional seat count as candidate availability; preserve that behavior.
   */
  applyProvincePlacement(liste, province, provLista, rng);

  const finalProvLista = buildFinalProvinceListRows(provLista);
  const finalListe = buildFinalListRows(liste, provLista);
  const finalCoalizioni = buildFinalCoalitionRows(coalizioni, finalListe);

  return {
    coalizioni: finalCoalizioni,
    liste: finalListe,
    prov_lista: finalProvLista
  };
}
