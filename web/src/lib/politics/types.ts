export type Ramo = 'camera' | 'senato';
export type AdministrativeCode = string | number;
export type TraceNumber = number | null;
export type TraceBoolean = boolean | null;

export interface ListeUniRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  COLLEGIOUNINOMINALE: AdministrativeCode;
  CANDIDATO: string;
  CAND_MINORANZA: boolean;
  LISTA: string;
  MINORANZA: boolean;
  VOTI_LISTA: number;
  SIM?: number;
}

export interface CandidatoUniInputRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  COLLEGIOUNINOMINALE: AdministrativeCode;
  CANDIDATO: string;
  DATA_NASCITA: string;
  VOTI_CANDIDATO: number;
  SIM?: number;
}

export interface CandidatoUniResultRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  COLLEGIOUNINOMINALE: AdministrativeCode;
  CANDIDATO: string;
  ELETTO: boolean;
}

export interface CandidatoPluriInputRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  LISTA: string;
  NUMERO: number;
  CANDIDATO: string;
  SIM?: number;
}

export interface CandidatoPluriResultRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  LISTA: string;
  NUMERO: number;
  CANDIDATO: string;
  ELETTO: boolean;
  ELETTO_QUI_O_ALTROVE: boolean;
}

export interface TotaliPluriRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  SEGGI: number;
}

export interface ListaNazRow {
  LISTA: string;
  COALIZIONE: string | null;
  MINORANZA: boolean;
}

export interface ListePluriResultRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  LISTA: string;
  ELETTI: number;
  NUMERO_MAX: number;
  SEGGI_PRE_SUBENTRI: number;
}

export interface CandidatoUniAttributionTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  COLLEGIOUNINOMINALE: AdministrativeCode;
  CANDIDATO: string;
  VOTI_CANDIDATO: number;
  VOTI_LISTA: number;
  VOTI_SOLO_CANDIDATO: number;
  QUOZIENTE: TraceNumber;
  PARTE_INTERA: number;
  DA_ASSEGNARE: number;
}

export interface ListeUniCifreTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  COLLEGIOUNINOMINALE: AdministrativeCode;
  CANDIDATO: string;
  LISTA: string;
  VOTI_LISTA: number;
  ELETTO: boolean;
  QUOZIENTE: TraceNumber;
  PARTE_INTERA: number;
  RESTO: TraceNumber;
  DA_ASSEGNARE: number;
  ORDINE: number;
  VOTO_DA_RESTO: number;
  CIFRA: number;
}

export interface ListePluriCifreTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  LISTA: string;
  CIFRA: number;
  CIFRA_TOT: number;
  CIFRA_PERCENTUALE: number;
}

export interface ListeCircCifreTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  LISTA: string;
  CIFRA: number;
}

export interface CandidatoUniGraduatoriaTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  COLLEGIOUNINOMINALE: AdministrativeCode;
  CANDIDATO: string;
  ELETTO: boolean;
  VOTI_CANDIDATO: number;
  VOTI_CANDIDATO_TOT: number;
  CIFRA_PERCENTUALE: number;
}

export interface TotaliCircTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  CIFRA: number;
}

export interface ListeNazSoglieTraceRow {
  LISTA: string;
  COALIZIONE: string | null;
  MINORANZA: boolean;
  CIFRA: number;
  CIFRA_PERCENTUALE: number;
  SOGLIA20: boolean;
  SOGLIA_MINORANZA: boolean;
  SOGLIA1M: boolean;
  SOGLIA3: boolean;
  SOGLIA3M: boolean;
  SOGLIA_COALIZIONE: TraceBoolean;
  SOGLIA_SOLA: TraceBoolean;
}

export interface ListeCircSoglieTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  LISTA: string;
  CIFRA: number;
  CIFRA_TOT: number;
  CIFRA_PERCENTUALE: number;
  ELETTI_MINORANZA: number;
  COLLEGI_UNI: number;
  SOGLIA20: boolean;
  SOGLIA_MINORANZA: boolean;
  SOGLIA1M: boolean;
  COALIZIONE: string | null;
  MINORANZA: boolean;
}

export interface CoalNazSoglieTraceRow {
  COALIZIONE: string;
  CIFRA: number;
  CIFRA_PERCENTUALE: number;
  SOGLIA3M: boolean;
  SOGLIA_COALIZIONE: boolean;
}

export interface CoalCircCifreTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COALIZIONE: string;
  CIFRA: number;
}

export interface PoliticsEarlyTrace {
  candidati_uni_elezione: CandidatoUniResultRow[];
  candidati_uni_attribuzione: CandidatoUniAttributionTraceRow[];
  liste_uni_cifre: ListeUniCifreTraceRow[];
  liste_pluri_cifre: ListePluriCifreTraceRow[];
  liste_circ_cifre: ListeCircCifreTraceRow[];
  candidati_uni_graduatoria: CandidatoUniGraduatoriaTraceRow[];
  totali_circ: TotaliCircTraceRow[];
}

export interface PoliticsScrutinyTrace extends PoliticsEarlyTrace {
  totale_naz: number;
  liste_naz_soglie: ListeNazSoglieTraceRow[];
  liste_circ_soglie: ListeCircSoglieTraceRow[];
  coal_naz_soglie: CoalNazSoglieTraceRow[];
  coal_circ_cifre: CoalCircCifreTraceRow[];
}

export interface PoliticsScrutinyInput {
  liste_uni: ListeUniRow[];
  candidati_uni: CandidatoUniInputRow[];
  candidati_pluri: CandidatoPluriInputRow[];
}

export interface PoliticsScrutinyContext {
  ramo: Ramo;
  liste_naz: ListaNazRow[];
}

export interface PoliticsScrutinyOutput {
  liste_pluri: ListePluriResultRow[];
  candidati_uni: CandidatoUniResultRow[];
  candidati_pluri: CandidatoPluriResultRow[];
}

export interface PoliticsGoldenSimulation {
  sim: number;
  input: PoliticsScrutinyInput;
  trace: PoliticsScrutinyTrace;
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
