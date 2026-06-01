export type Ramo = 'camera' | 'senato';

export interface ListeUniRow {
  CIRCOSCRIZIONE: string;
  COLLEGIOPLURINOMINALE: string;
  COLLEGIOUNINOMINALE: string;
  CANDIDATO: string;
  CAND_MINORANZA: boolean;
  LISTA: string;
  MINORANZA: boolean;
  VOTI_LISTA: number;
  SIM?: number;
}

export interface CandidatoUniInputRow {
  CIRCOSCRIZIONE: string;
  COLLEGIOPLURINOMINALE: string;
  COLLEGIOUNINOMINALE: string;
  CANDIDATO: string;
  DATA_NASCITA: string;
  VOTI_CANDIDATO: number;
  SIM?: number;
}

export interface CandidatoUniResultRow {
  CIRCOSCRIZIONE: string;
  COLLEGIOPLURINOMINALE: string;
  COLLEGIOUNINOMINALE: string;
  CANDIDATO: string;
  ELETTO: boolean;
}

export interface CandidatoPluriInputRow {
  CIRCOSCRIZIONE: string;
  COLLEGIOPLURINOMINALE: string;
  LISTA: string;
  NUMERO: number;
  CANDIDATO: string;
  SIM?: number;
}

export interface CandidatoPluriResultRow {
  CIRCOSCRIZIONE: string;
  COLLEGIOPLURINOMINALE: string;
  LISTA: string;
  NUMERO: number;
  CANDIDATO: string;
  ELETTO: boolean;
  ELETTO_QUI_O_ALTROVE: boolean;
}

export interface TotaliPluriRow {
  CIRCOSCRIZIONE: string;
  COLLEGIOPLURINOMINALE: string;
  SEGGI: number;
}

export interface ListaNazRow {
  LISTA: string;
  COALIZIONE: string | null;
  MINORANZA: boolean;
}

export interface ListePluriResultRow {
  CIRCOSCRIZIONE: string;
  COLLEGIOPLURINOMINALE: string;
  LISTA: string;
  ELETTI: number;
  NUMERO_MAX: number;
  SEGGI_PRE_SUBENTRI: number;
}

export interface PoliticsScrutinyInput {
  liste_uni: ListeUniRow[];
  candidati_uni: CandidatoUniInputRow[];
  candidati_pluri: CandidatoPluriInputRow[];
}

export interface PoliticsScrutinyOutput {
  liste_pluri: ListePluriResultRow[];
  candidati_uni: CandidatoUniResultRow[];
  candidati_pluri: CandidatoPluriResultRow[];
}

export interface PoliticsGoldenSimulation {
  sim: number;
  input: PoliticsScrutinyInput;
  expected: PoliticsScrutinyOutput;
  warnings: string[];
  messages: string[];
}

export interface PoliticsGoldenRamoFixture {
  totali_pluri: TotaliPluriRow[];
  liste_naz: ListaNazRow[];
  totale_seggi: number;
  simulations: PoliticsGoldenSimulation[];
}

export interface PoliticsGoldenFixture {
  metadata: {
    schema_version: number;
    source: string;
    purpose: string;
    random_seed: number;
    warning_policy: string;
  };
  rami: Record<Ramo, PoliticsGoldenRamoFixture>;
}
