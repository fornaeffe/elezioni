import type { Scenario } from '$lib/core/types';

export type RegionalErAdministrativeCode = string | number;

export interface RegionalErListParameterRow {
  LISTA: string;
  COALIZIONE: string | null;
  COLORE?: string | null;
  PERCENTUALE: number;
  DATA: string;
  LOGIT_P: number;
  SIGMA_GLOBAL: number;
  SIGMA_DELTA?: number | null;
}

export interface RegionalErMunicipalListParameterRow {
  CODICE_COMUNE: RegionalErAdministrativeCode;
  COMUNE: string;
  CODICE_PROVINCIA: RegionalErAdministrativeCode;
  PROVINCIA: string;
  CODICE_REGIONE: RegionalErAdministrativeCode;
  REGIONE: string;
  LISTA: string;
  DATA: string;
  DELTA: number;
  ELETTORI: number;
  SIGMA_DELTA: number;
}

export interface RegionalErMunicipalityRow {
  CODICE_COMUNE: RegionalErAdministrativeCode;
  COMUNE: string;
  CODICE_PROVINCIA: RegionalErAdministrativeCode;
  PROVINCIA: string;
  CODICE_REGIONE: RegionalErAdministrativeCode;
  REGIONE: string;
  ELETTORI: number;
}

export interface RegionalErHistoricalMunicipalListVoteRow {
  DATA: string;
  ELEZIONE: string;
  CODICE_COMUNE: RegionalErAdministrativeCode;
  LISTA: string;
  VOTI: number;
}

export interface RegionalErLegalPopulationRow {
  CODICE_COMUNE: RegionalErAdministrativeCode;
  POPOLAZIONE: number;
  COMUNE: string;
  CODICE_PROVINCIA: RegionalErAdministrativeCode;
  PROVINCIA: string;
  CODICE_REGIONE: RegionalErAdministrativeCode;
  REGIONE: string;
}

export interface RegionalErScenarioCoalitionSnapshotRow {
  COALIZIONE: string;
  COLORE?: string | null;
}

export interface RegionalErScenarioListElectionSnapshotRow {
  DATA: string;
  ELEZIONE: string;
  LISTA: string;
  VOTI: number;
  PERCENTUALE: number;
  LOGIT_P: number;
}

export interface RegionalErScenarioListCorrespondenceSnapshotRow {
  DATA: string;
  ELEZIONE: string;
  LISTA_ORIGINALE: string;
  LISTA: string;
  FATTORE: number;
}

export interface RegionalErScenarioSnapshot {
  id: string;
  name: string;
  data_elezione: string;
  liste: RegionalErListParameterRow[];
  comuni_liste: RegionalErMunicipalListParameterRow[];
  coalizioni?: RegionalErScenarioCoalitionSnapshotRow[];
  liste_elezioni?: RegionalErScenarioListElectionSnapshotRow[];
  corrispondenza_liste?: RegionalErScenarioListCorrespondenceSnapshotRow[];
}

export interface RegionalErStaticSnapshot {
  metadata: {
    schema_version: number;
    source: string;
    purpose: string;
    created?: string;
    cache_path?: string;
    scenario_path?: string;
    region?: string;
  };
  data: {
    municipalities: RegionalErMunicipalityRow[];
    comuni_liste_elezioni?: RegionalErHistoricalMunicipalListVoteRow[];
    pop_legale: RegionalErLegalPopulationRow[];
  };
  default_scenario: RegionalErScenarioSnapshot;
}

export interface RegionalErPipelineSource {
  data_elezione: string;
  simulazioni: number;
  liste: RegionalErListParameterRow[];
  comuni_liste: RegionalErMunicipalListParameterRow[];
  pop_legale: RegionalErLegalPopulationRow[];
}

export interface RegionalErGeneratedVoteRow extends RegionalErMunicipalListParameterRow {
  SIM: number;
  PERCENTUALE_SIM: number;
  VOTI_LISTA_SIM: number;
}

export interface RegionalErScrutinyInputRow {
  CODICE_COMUNE: RegionalErAdministrativeCode;
  COMUNE: string;
  LISTA: string;
  CODICE_PROVINCIA: RegionalErAdministrativeCode;
  PROVINCIA: string;
  VOTI_LISTA_SIM: number;
}

export interface RegionalErCoalitionResultRow {
  COALIZIONE: string;
  PRESIDENTE: boolean;
  MIGLIOR_PERDENTE: boolean;
  VOTI_LISTA_ITER: number;
  PERCENTUALE: number;
  ELETTI: number;
  ELETTI_TOT: number;
}

export interface RegionalErListResultRow {
  LISTA: string;
  COALIZIONE: string | null;
  VOTI_LISTA_ITER: number;
  PERCENTUALE: number;
  ELETTI: number;
}

export interface RegionalErProvinceListResultRow {
  PROVINCIA: string;
  LISTA: string;
  ELETTI: number;
  VOTI_LISTA_ITER: number;
  PERCENTUALE: number;
}

export interface RegionalErScrutinyOutput {
  coalizioni: RegionalErCoalitionResultRow[];
  liste: RegionalErListResultRow[];
  prov_lista: RegionalErProvinceListResultRow[];
}

export interface RegionalErGoldenSimulation {
  sim: number;
  input: {
    comuni_liste: RegionalErScrutinyInputRow[];
  };
  expected: RegionalErScrutinyOutput;
}

export interface RegionalErGoldenFixture {
  metadata: {
    schema_version: number;
    source: string;
    purpose: string;
    random_seed: number;
    simulations: number;
    data_elezione: string;
  };
  context: {
    pop_legale: RegionalErLegalPopulationRow[];
    liste: RegionalErListParameterRow[];
  };
  simulations: RegionalErGoldenSimulation[];
}

export interface RegionalErScenarioProjectionRow {
  list: string;
  coalition: string | null;
  scenarioShare: number | null;
  shareOverride: boolean;
  projectedShare: number | null;
  parameterSource: 'historical' | 'static-snapshot' | 'synthetic';
  status: 'active';
}

export interface RegionalErScenarioProjectionWarning {
  code: string;
  message: string;
  todoReference?: string;
}

export interface RegionalErScenarioProjection {
  source: RegionalErPipelineSource;
  rows: RegionalErScenarioProjectionRow[];
  warnings: RegionalErScenarioProjectionWarning[];
}

export interface RegionalErRunPresentationSummary {
  sim: number;
  elapsedMs: number;
  councilSeats: number;
  presidentCoalition: string | null;
  runnerUpCoalition: string | null;
  seatsByList: Map<string, number>;
  seatsByCoalition: Map<string, number>;
  totalSeatsByCoalition: Map<string, number>;
  validVoteShareByList: Map<string, number>;
  validVoteShareByCoalition: Map<string, number>;
  provinceSeatsByList: Map<string, number>;
}

export type RegionalErScenario = Scenario;
