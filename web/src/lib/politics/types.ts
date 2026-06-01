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

export interface CameraRipartoNazTraceRow {
  SOGGETTO_RIPARTO: string;
  CIFRA: number;
  PARTE_INTERA: number;
  RESTO: number;
  ORDINE: number;
  SEGGIO_DA_RESTO: boolean;
  SEGGI: number;
  CIFRA_AMMESSE_AL_RIPARTO: number;
  QUOZIENTE: number;
  PARTE_INTERA_TOT: number;
  DA_ASSEGNARE: number;
}

export interface CameraAmmesseNazTraceRow {
  SOGGETTO_RIPARTO: string;
  LISTA: string;
  CIFRA: number;
  QUOZIENTE: number;
  PARTE_INTERA: number;
  RESTO: number;
  DA_ASSEGNARE: number;
  ORDINE: number;
  SEGGIO_DA_RESTO: boolean;
  SEGGI: number;
}

export interface CameraListeNazRipartoTraceRow {
  LISTA: string;
  COALIZIONE: string | null;
  SOGLIA1M: boolean;
  SOGLIA3M: boolean;
  SOGLIA_COALIZIONE: TraceBoolean;
  SOGLIA_SOLA: TraceBoolean;
  SOGGETTO_RIPARTO: string | null;
}

export interface CameraRipartoTrace {
  seggi_proporzionale: TraceNumber;
  totale_naz_riparto: TraceNumber;
  quoziente_elettorale_naz: TraceNumber;
  ancora_da_attribuire: TraceNumber;
  riparto_naz: CameraRipartoNazTraceRow[];
  ammesse_naz: CameraAmmesseNazTraceRow[];
  liste_naz_riparto: CameraListeNazRipartoTraceRow[];
}

export interface CircRipartoTotaleTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  CIFRA: number;
  SEGGI: number;
  CIFRA_AMMESSE_AL_RIPARTO: number;
  QUOZIENTE: number;
  PARTE_INTERA: number;
  DA_ASSEGNARE: number;
}

export interface CircRipartoListaTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  LISTA: string;
  CIFRA: number;
  SOGLIA1M: boolean;
  SOGLIA3: boolean;
  SOGLIA20: boolean;
  SOGLIA_MINORANZA: boolean;
  SOGLIA_COALIZIONE: TraceBoolean;
  SOGLIA_SOLA: TraceBoolean;
  SOGGETTO_RIPARTO: string | null;
}

export interface CircRipartoTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  SOGGETTO_RIPARTO: string;
  CIFRA: number;
  QUOZIENTE: number;
  PARTE_INTERA: number;
  DA_ASSEGNARE: number;
  DECIMALI: TraceNumber;
  RESTO: TraceNumber;
  CIFRA_NAZ: TraceNumber;
  ESCLUSE: TraceBoolean;
  ORDINE: TraceNumber;
  SEGGIO_DA_DECIMALI: TraceBoolean;
  SEGGIO_DA_RESTO: TraceBoolean;
  FLIPPER: TraceNumber;
  SEGGI: number;
}

export interface CircRipartoNazTraceRow {
  SOGGETTO_RIPARTO: string;
  CIFRA: number;
  SEGGI: number;
  PARTE_INTERA_CIRC: number;
  ESCLUSE: boolean;
  SEGGI_CIRC: number;
  SEGGI_ECCEDENTI: number;
  SEGGI_ECCEDENTI_CONTATORE: number;
}

export interface CircRipartoTrace {
  totali_circ: CircRipartoTotaleTraceRow[];
  liste_circ: CircRipartoListaTraceRow[];
  riparto_circ: CircRipartoTraceRow[];
  riparto_naz: CircRipartoNazTraceRow[];
}

export interface InternalCircRipartoListaTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  LISTA: string;
  CIFRA: number;
  MINORANZA: boolean;
  SOGLIA3: boolean;
  SOGLIA20: boolean;
  SOGLIA_MINORANZA: boolean;
  SOGGETTO_RIPARTO: string | null;
  AMMESSA: boolean;
}

export interface InternalCircRipartoSubjectTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  SOGGETTO_RIPARTO: string;
  SEGGI: number;
  CIFRA_AMMESSE_AL_RIPARTO: number;
  QUOZIENTE_COAL: TraceNumber;
  PARTE_INTERA_TOT: number;
  DA_ASSEGNARE_COAL: number;
}

export interface InternalCircAmmesseTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  SOGGETTO_RIPARTO: string;
  LISTA: string;
  CIFRA: number;
  QUOZIENTE_COAL: TraceNumber;
  PARTE_INTERA: number;
  DA_ASSEGNARE_COAL: number;
  DECIMALI: TraceNumber;
  RESTO: TraceNumber;
  CIFRA_NAZ: TraceNumber;
  ESCLUSE: TraceBoolean;
  ORDINE: TraceNumber;
  SEGGIO_DA_DECIMALI: TraceBoolean;
  SEGGIO_DA_RESTO: TraceBoolean;
  FLIPPER: TraceNumber;
  SEGGI: number;
}

export interface InternalCircAmmesseNazTraceRow {
  SOGGETTO_RIPARTO: string;
  LISTA: string;
  CIFRA: number;
  SEGGI: number;
  PARTE_INTERA_CIRC: number;
  ESCLUSE: boolean;
  SEGGI_CIRC: number;
  SEGGI_ECCEDENTI: number;
  SEGGI_ECCEDENTI_CONTATORE: number;
}

export interface InternalCircRipartoTrace {
  liste_circ: InternalCircRipartoListaTraceRow[];
  riparto_circ: InternalCircRipartoSubjectTraceRow[];
  ammesse_circ: InternalCircAmmesseTraceRow[];
  ammesse_naz: InternalCircAmmesseNazTraceRow[];
}

export interface PluriRipartoListaTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  LISTA: string;
  CIFRA: number;
  CIFRA_PERCENTUALE: number;
  AMMESSA: boolean;
}

export interface PluriRipartoTotaleTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  SEGGI: number;
  CIFRA: number;
  QUOZIENTE: TraceNumber;
  PARTE_INTERA: number;
  DA_ASSEGNARE: number;
}

export interface PluriRipartoCircTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  LISTA: string;
  SEGGI: number;
  PARTE_INTERA_PLURI: number;
  ESCLUSE_PLURI: boolean;
  SEGGI_PLURI: number;
  SEGGI_ECCEDENTI: number;
}

export interface PluriRipartoAmmesseTraceRow {
  CIRCOSCRIZIONE: AdministrativeCode;
  COLLEGIOPLURINOMINALE: AdministrativeCode;
  LISTA: string;
  CIFRA: number;
  CIFRA_PERCENTUALE: number;
  QUOZIENTE: TraceNumber;
  PARTE_INTERA: number;
  DECIMALI: TraceNumber;
  ESCLUSE_PLURI: boolean;
  CIFRA_CIRC: number;
  DA_ASSEGNARE: number;
  ORDINE: TraceNumber;
  SEGGIO_DA_DECIMALI: boolean;
  SEGGI_ECCEDENTI: number;
  CEDE: boolean;
  RICEVE: boolean;
  ORDINE_CEDE: TraceNumber;
  CEDUTO: boolean;
  ORDINE_RICEVE: TraceNumber;
  RICEVUTO: boolean;
  SEGGI: number;
  SEGGI_PRE_SUBENTRI: number;
}

export interface PluriRipartoTrace {
  liste_pluri: PluriRipartoListaTraceRow[];
  totali_pluri: PluriRipartoTotaleTraceRow[];
  ammesse_circ: PluriRipartoCircTraceRow[];
  ammesse_pluri: PluriRipartoAmmesseTraceRow[];
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
  camera_riparto: CameraRipartoTrace;
  circ_riparto: CircRipartoTrace;
  internal_circ_riparto: InternalCircRipartoTrace;
  pluri_riparto: PluriRipartoTrace;
}

export interface PoliticsScrutinyInput {
  liste_uni: ListeUniRow[];
  candidati_uni: CandidatoUniInputRow[];
  candidati_pluri: CandidatoPluriInputRow[];
}

export interface PoliticsScrutinyContext {
  ramo: Ramo;
  liste_naz: ListaNazRow[];
  totali_pluri: TotaliPluriRow[];
  totale_seggi: number;
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

export interface PoliticsDirectScrutinySnapshotSimulation {
  sim: number;
  input: PoliticsScrutinyInput;
}

export interface PoliticsDirectScrutinySnapshotRamo {
  totali_pluri: TotaliPluriRow[];
  liste_naz: ListaNazRow[];
  totale_seggi: number;
  simulations: PoliticsDirectScrutinySnapshotSimulation[];
}

export interface PoliticsDirectScrutinySnapshot {
  schema_version: number;
  source_schema_version: number;
  source: string;
  purpose: string;
  rami: Record<Ramo, PoliticsDirectScrutinySnapshotRamo>;
}

export interface GeneratedUniListVotesRow {
  SIM: number;
  UNI_COD: AdministrativeCode;
  LISTA: string;
  VOTI_LISTA_SIM: number;
  PLURI_COD?: AdministrativeCode | null;
  CIRC_COD?: AdministrativeCode | null;
  COALIZIONE?: string | null;
  CANDIDATO_ID?: string | null;
  CAND_MINORANZA?: boolean | null;
  MINORANZA?: boolean | null;
}

export interface GeneratedCandidatoUniVotesRow {
  SIM: number;
  COALIZIONE?: string | null;
  UNI_COD: AdministrativeCode;
  LISTA_MINORANZA?: string | boolean | null;
  CANDIDATO_ID: string;
  DATA_NASCITA: string;
  VOTI_CANDIDATO: number;
  PLURI_COD?: AdministrativeCode | null;
  CIRC_COD?: AdministrativeCode | null;
}

export interface GeneratedCandidatoPluriRow {
  SIM: number;
  LISTA: string;
  PLURI_COD: AdministrativeCode;
  NUMERO_CANDIDATO: number;
  CANDIDATO_ID: string;
  CIRC_COD?: AdministrativeCode | null;
}

export interface PoliticsCollegeUniRow {
  CIRC_COD: AdministrativeCode;
  PLURI_COD: AdministrativeCode | null;
  UNI_COD: AdministrativeCode;
}

export interface PoliticsCollegePluriRow {
  CIRC_COD: AdministrativeCode;
  PLURI_COD: AdministrativeCode | null;
  SEGGI_PLURI: number;
}

export interface PoliticsGeneratedListRow {
  LISTA: string;
  COALIZIONE: string | null;
}

export interface PoliticsCandidatePluriTemplateRow {
  LISTA: string;
  PLURI_COD: AdministrativeCode | null;
  CIRC_COD: AdministrativeCode;
  MINORANZA: boolean;
}

export interface PoliticsGeneratedRamoSource {
  uni_liste_sim: GeneratedUniListVotesRow[];
  candidati_uni_sim: GeneratedCandidatoUniVotesRow[];
  candidati_pluri_sim: GeneratedCandidatoPluriRow[];
  uni: PoliticsCollegeUniRow[];
  pluri: PoliticsCollegePluriRow[];
  liste: PoliticsGeneratedListRow[];
  candidati_pluri_template: PoliticsCandidatePluriTemplateRow[];
}

export interface PoliticsGeneratedAdapterFixture {
  metadata: {
    schema_version: number;
    source: string;
    purpose: string;
  };
  rami: Record<Ramo, PoliticsGeneratedRamoSource>;
}

export interface RawUniListVotesRow {
  SIM: number;
  UNI_COD: AdministrativeCode;
  LISTA: string;
  VOTI_LISTA_SIM: number;
}

export interface GeneratedCandidatoUniRow {
  SIM: number;
  COALIZIONE: string | null;
  UNI_COD: AdministrativeCode;
  LISTA_MINORANZA: string | boolean | null;
  CANDIDATO_ID: string;
  DATA_NASCITA: string;
  PLURI_COD?: AdministrativeCode | null;
  CIRC_COD?: AdministrativeCode | null;
}

export interface PoliticsVotePreparationSource {
  uni_liste_sim: RawUniListVotesRow[];
  uni: PoliticsCollegeUniRow[];
  liste: PoliticsGeneratedListRow[];
  candidati_uni_sim: GeneratedCandidatoUniRow[];
  candidati_pluri_template: PoliticsCandidatePluriTemplateRow[];
}

export interface PreparedPoliticsVoteTables {
  uni_liste_sim: GeneratedUniListVotesRow[];
  candidati_uni_sim: GeneratedCandidatoUniVotesRow[];
}

export interface PoliticsVotePreparationFixtureRamo {
  source: PoliticsVotePreparationSource;
  expected: PreparedPoliticsVoteTables;
}

export interface PoliticsVotePreparationFixture {
  metadata: {
    schema_version: number;
    source: string;
    purpose: string;
  };
  rami: Record<Ramo, PoliticsVotePreparationFixtureRamo>;
}

export interface PoliticsVoteListParameterRow extends PoliticsGeneratedListRow {
  DATA: string;
  LOGIT_P: number;
  SIGMA_GLOBAL: number;
}

export interface PoliticsMunicipalListParameterRow {
  CODICE_COMUNE: AdministrativeCode;
  LISTA: string;
  DATA: string;
  DELTA: number;
  SIGMA_DELTA: number;
}

export interface PoliticsBaseDataRow {
  CODICE_COMUNE: AdministrativeCode;
  CODITA_20N: AdministrativeCode;
  ELETTORI: number;
  CU20_COD: AdministrativeCode;
  SU20_COD: AdministrativeCode;
}

export interface PoliticsVoteGenerationRamoSource {
  uni: PoliticsCollegeUniRow[];
  candidati_uni_sim: GeneratedCandidatoUniRow[];
  candidati_pluri_template: PoliticsCandidatePluriTemplateRow[];
}

export interface PoliticsVoteGenerationSource {
  data_elezione: string;
  simulazioni: number;
  liste: PoliticsVoteListParameterRow[];
  comuni_liste: PoliticsMunicipalListParameterRow[];
  base_dati: PoliticsBaseDataRow[];
  camera: PoliticsVoteGenerationRamoSource;
  senato: PoliticsVoteGenerationRamoSource;
}

export interface PoliticsGeneratedVoteTables {
  camera: PreparedPoliticsVoteTables;
  senato: PreparedPoliticsVoteTables;
}

export interface PoliticsVoteGenerationFixture {
  metadata: {
    schema_version: number;
    source: string;
    random_seed: number;
    purpose: string;
  };
  input: PoliticsVoteGenerationSource;
  normal_draws: Array<{
    phase: 'global' | 'local';
    LISTA: string;
    SIM: number;
    LOCALITA: AdministrativeCode | null;
    mean: number;
    sd: number;
    value: number;
  }>;
  expected: PoliticsGeneratedVoteTables;
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
