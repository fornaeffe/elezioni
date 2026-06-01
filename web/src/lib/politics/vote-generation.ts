import {
  generateVotes,
  type VoteGenerationListRow,
  type VoteGenerationLocalRow,
  type VoteGenerationOutputRow
} from '$lib/core/vote-generation';
import type { NormalSampler } from '$lib/core/rng';
import { preparePoliticsVoteTables } from './vote-preparation';
import type {
  AdministrativeCode,
  PoliticsBaseDataRow,
  PoliticsGeneratedVoteTables,
  PoliticsMunicipalListParameterRow,
  PoliticsVoteGenerationRamoSource,
  PoliticsVoteGenerationSource,
  RawUniListVotesRow
} from './types';

interface PoliticsUnitListInputRow extends VoteGenerationLocalRow {
  CODITA_20N: AdministrativeCode;
  CU20_COD: AdministrativeCode;
  SU20_COD: AdministrativeCode;
}

interface PoliticsUnitListOutputRow extends VoteGenerationOutputRow {
  CODITA_20N: AdministrativeCode;
  CU20_COD: AdministrativeCode;
  SU20_COD: AdministrativeCode;
}

function codeKey(code: AdministrativeCode): string {
  return String(code);
}

function aggregationKey(simulation: number, uninominalCode: AdministrativeCode, list: string): string {
  return `${simulation}\u001f${String(uninominalCode)}\u001f${list}`;
}

function buildUnitListRows(
  municipalListRows: readonly PoliticsMunicipalListParameterRow[],
  baseRows: readonly PoliticsBaseDataRow[]
): PoliticsUnitListInputRow[] {
  const municipalRowsByCode = new Map<string, PoliticsMunicipalListParameterRow[]>();

  for (const row of municipalListRows) {
    const key = codeKey(row.CODICE_COMUNE);
    const rows = municipalRowsByCode.get(key) ?? [];
    rows.push(row);
    municipalRowsByCode.set(key, rows);
  }

  const unitRows: PoliticsUnitListInputRow[] = [];

  for (const baseRow of baseRows) {
    const municipalRows = municipalRowsByCode.get(codeKey(baseRow.CODICE_COMUNE)) ?? [];

    for (const row of municipalRows) {
      unitRows.push({
        CODITA_20N: baseRow.CODITA_20N,
        CU20_COD: baseRow.CU20_COD,
        SU20_COD: baseRow.SU20_COD,
        LISTA: row.LISTA,
        DATA: row.DATA,
        DELTA: row.DELTA,
        SIGMA_DELTA: row.SIGMA_DELTA,
        ELETTORI: baseRow.ELETTORI
      });
    }
  }

  return unitRows;
}

function aggregateUninominalListVotes(
  rows: readonly PoliticsUnitListOutputRow[],
  codeColumn: 'CU20_COD' | 'SU20_COD'
): RawUniListVotesRow[] {
  const totals = new Map<string, RawUniListVotesRow>();

  for (const row of rows) {
    const uninominalCode = row[codeColumn];
    const key = aggregationKey(row.SIM, uninominalCode, row.LISTA);
    const existing = totals.get(key);

    if (existing) {
      existing.VOTI_LISTA_SIM += row.VOTI_LISTA_SIM;
    } else {
      totals.set(key, {
        SIM: row.SIM,
        UNI_COD: uninominalCode,
        LISTA: row.LISTA,
        VOTI_LISTA_SIM: row.VOTI_LISTA_SIM
      });
    }
  }

  return [...totals.values()];
}

function prepareRamo(
  source: PoliticsVoteGenerationRamoSource,
  listRows: PoliticsVoteGenerationSource['liste'],
  rawListVotes: RawUniListVotesRow[]
) {
  return preparePoliticsVoteTables({
    uni_liste_sim: rawListVotes,
    uni: source.uni,
    liste: listRows,
    candidati_uni_sim: source.candidati_uni_sim,
    candidati_pluri_template: source.candidati_pluri_template
  });
}

export function generatePoliticsVotes(
  source: PoliticsVoteGenerationSource,
  options: { normal?: NormalSampler; seed?: string | number } = {}
): PoliticsGeneratedVoteTables {
  const listRows: VoteGenerationListRow[] = source.liste.map((row) => ({
    LISTA: row.LISTA,
    DATA: row.DATA,
    LOGIT_P: row.LOGIT_P,
    SIGMA_GLOBAL: row.SIGMA_GLOBAL
  }));
  const unitRows = generateVotes(buildUnitListRows(source.comuni_liste, source.base_dati), listRows, {
    electionDate: source.data_elezione,
    simulations: source.simulazioni,
    localityColumn: 'CODITA_20N',
    normal: options.normal,
    seed: options.seed
  }) as PoliticsUnitListOutputRow[];

  return {
    camera: prepareRamo(source.camera, source.liste, aggregateUninominalListVotes(unitRows, 'CU20_COD')),
    senato: prepareRamo(source.senato, source.liste, aggregateUninominalListVotes(unitRows, 'SU20_COD'))
  };
}
