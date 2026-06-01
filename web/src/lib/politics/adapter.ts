import type {
  AdministrativeCode,
  CandidatoPluriInputRow,
  CandidatoUniInputRow,
  GeneratedCandidatoPluriRow,
  GeneratedCandidatoUniVotesRow,
  GeneratedUniListVotesRow,
  ListaNazRow,
  ListeUniRow,
  PoliticsGeneratedAdapterFixture,
  PoliticsGeneratedRamoSource,
  PoliticsScrutinyContext,
  PoliticsScrutinyInput,
  Ramo,
  TotaliPluriRow
} from './types';

export interface AdaptedPoliticsSimulation {
  sim: number;
  input: PoliticsScrutinyInput;
}

export interface AdaptedPoliticsRamo {
  context: PoliticsScrutinyContext;
  simulations: AdaptedPoliticsSimulation[];
}

export type AdaptedPoliticsFixture = Record<Ramo, AdaptedPoliticsRamo>;

function codeKey(code: AdministrativeCode | null | undefined): string {
  return code === null || code === undefined ? '<NA>' : String(code);
}

function requireCode(
  value: AdministrativeCode | null | undefined,
  field: string,
  sourceRow: object
): AdministrativeCode {
  if (value === null || value === undefined) {
    throw new Error(`Missing ${field} while adapting generated politics input: ${JSON.stringify(sourceRow)}`);
  }

  return value;
}

function requireNullableCode(
  value: AdministrativeCode | null | undefined,
  field: string,
  sourceRow: object
): AdministrativeCode {
  if (value === undefined) {
    throw new Error(`Missing ${field} while adapting generated politics input: ${JSON.stringify(sourceRow)}`);
  }

  return value as AdministrativeCode;
}

function requireSimulationNumber(value: number | undefined, sourceRow: object): number {
  if (value === undefined) {
    throw new Error(`Missing SIM while adapting generated politics input: ${JSON.stringify(sourceRow)}`);
  }

  return value;
}

function requireString(
  value: string | null | undefined,
  field: string,
  sourceRow: object
): string {
  if (value === null || value === undefined || value === '') {
    throw new Error(`Missing ${field} while adapting generated politics input: ${JSON.stringify(sourceRow)}`);
  }

  return value;
}

function buildListMetadata(ramo: Ramo, source: PoliticsGeneratedRamoSource): ListaNazRow[] {
  const minorityStats = new Map<string, { regions: Set<string>; minorityCandidates: number }>();

  for (const row of source.candidati_pluri_template) {
    const circCode = String(row.CIRC_COD);
    const regionCode = ramo === 'camera' ? circCode.slice(0, -2) : circCode;
    const stats = minorityStats.get(row.LISTA) ?? { regions: new Set<string>(), minorityCandidates: 0 };
    stats.regions.add(regionCode);
    stats.minorityCandidates += row.MINORANZA ? 1 : 0;
    minorityStats.set(row.LISTA, stats);
  }

  const minorityLists = new Set(
    [...minorityStats.entries()]
      .filter(([, stats]) => stats.regions.size === 1 && stats.minorityCandidates > 0)
      .map(([list]) => list)
  );

  return source.liste.map((row) => ({
    LISTA: row.LISTA,
    COALIZIONE: row.COALIZIONE,
    MINORANZA: minorityLists.has(row.LISTA)
  }));
}

function buildTotaliPluri(source: PoliticsGeneratedRamoSource): TotaliPluriRow[] {
  return source.pluri.map((row) => ({
    CIRCOSCRIZIONE: row.CIRC_COD,
    COLLEGIOPLURINOMINALE: row.PLURI_COD as AdministrativeCode,
    SEGGI: row.SEGGI_PLURI
  }));
}

function adaptListVotes(
  rows: readonly GeneratedUniListVotesRow[],
  source: PoliticsGeneratedRamoSource
): ListeUniRow[] {
  const uniByCode = new Map(source.uni.map((row) => [codeKey(row.UNI_COD), row]));

  return rows.map((row) => {
    const uni = uniByCode.get(codeKey(row.UNI_COD));
    const circ = row.CIRC_COD ?? uni?.CIRC_COD;
    const pluri = row.PLURI_COD === undefined ? uni?.PLURI_COD : row.PLURI_COD;

    return {
      CIRCOSCRIZIONE: requireCode(circ, 'CIRC_COD', row),
      COLLEGIOPLURINOMINALE: requireNullableCode(pluri, 'PLURI_COD', row),
      COLLEGIOUNINOMINALE: row.UNI_COD,
      CANDIDATO: requireString(row.CANDIDATO_ID, 'CANDIDATO_ID', row),
      CAND_MINORANZA: row.CAND_MINORANZA === true,
      LISTA: row.LISTA,
      MINORANZA: row.MINORANZA === true,
      VOTI_LISTA: row.VOTI_LISTA_SIM,
      SIM: row.SIM
    };
  });
}

function adaptUninominalCandidates(
  rows: readonly GeneratedCandidatoUniVotesRow[],
  source: PoliticsGeneratedRamoSource
): CandidatoUniInputRow[] {
  const uniByCode = new Map(source.uni.map((row) => [codeKey(row.UNI_COD), row]));

  return rows.map((row) => {
    const uni = uniByCode.get(codeKey(row.UNI_COD));

    return {
      CIRCOSCRIZIONE: requireCode(uni?.CIRC_COD ?? row.CIRC_COD, 'CIRC_COD', row),
      COLLEGIOPLURINOMINALE: requireNullableCode(
        uni === undefined ? row.PLURI_COD : uni.PLURI_COD,
        'PLURI_COD',
        row
      ),
      COLLEGIOUNINOMINALE: row.UNI_COD,
      CANDIDATO: row.CANDIDATO_ID,
      DATA_NASCITA: row.DATA_NASCITA,
      VOTI_CANDIDATO: row.VOTI_CANDIDATO,
      SIM: row.SIM
    };
  });
}

function adaptPlurinominalCandidates(
  rows: readonly GeneratedCandidatoPluriRow[],
  source: PoliticsGeneratedRamoSource
): CandidatoPluriInputRow[] {
  const pluriByCode = new Map(source.pluri.map((row) => [codeKey(row.PLURI_COD), row]));

  return rows.map((row) => {
    const pluri = pluriByCode.get(codeKey(row.PLURI_COD));

    return {
      CIRCOSCRIZIONE: requireCode(pluri?.CIRC_COD ?? row.CIRC_COD, 'CIRC_COD', row),
      COLLEGIOPLURINOMINALE: row.PLURI_COD,
      LISTA: row.LISTA,
      NUMERO: row.NUMERO_CANDIDATO,
      CANDIDATO: row.CANDIDATO_ID as string,
      SIM: row.SIM
    };
  });
}

function splitAdaptedSimulations(
  listeUni: readonly ListeUniRow[],
  candidatiUni: readonly CandidatoUniInputRow[],
  candidatiPluri: readonly CandidatoPluriInputRow[]
): AdaptedPoliticsSimulation[] {
  const simulations = new Map<number, PoliticsScrutinyInput>();

  const simulation = (sim: number): PoliticsScrutinyInput => {
    const existing = simulations.get(sim);
    if (existing) return existing;

    const created: PoliticsScrutinyInput = {
      liste_uni: [],
      candidati_uni: [],
      candidati_pluri: []
    };
    simulations.set(sim, created);
    return created;
  };

  for (const row of listeUni) simulation(requireSimulationNumber(row.SIM, row)).liste_uni.push(row);
  for (const row of candidatiUni) simulation(requireSimulationNumber(row.SIM, row)).candidati_uni.push(row);
  for (const row of candidatiPluri) simulation(requireSimulationNumber(row.SIM, row)).candidati_pluri.push(row);

  return [...simulations.entries()]
    .map(([sim, input]) => ({ sim, input }))
    .sort((left, right) => left.sim - right.sim);
}

export function adaptGeneratedPoliticsRamo(ramo: Ramo, source: PoliticsGeneratedRamoSource): AdaptedPoliticsRamo {
  const listeUni = adaptListVotes(source.uni_liste_sim, source);
  const candidatiUni = adaptUninominalCandidates(source.candidati_uni_sim, source);
  const candidatiPluri = adaptPlurinominalCandidates(source.candidati_pluri_sim, source);

  return {
    context: {
      ramo,
      liste_naz: buildListMetadata(ramo, source),
      totali_pluri: buildTotaliPluri(source),
      totale_seggi: ramo === 'camera' ? 392 : 196
    },
    simulations: splitAdaptedSimulations(listeUni, candidatiUni, candidatiPluri)
  };
}

export function adaptGeneratedPoliticsFixture(fixture: PoliticsGeneratedAdapterFixture): AdaptedPoliticsFixture {
  return {
    camera: adaptGeneratedPoliticsRamo('camera', fixture.rami.camera),
    senato: adaptGeneratedPoliticsRamo('senato', fixture.rami.senato)
  };
}
