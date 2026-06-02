import type { PoliticsPipelineSource, PoliticsScenarioSnapshot, PoliticsStaticSnapshot } from './types';

export function buildPoliticsPipelineSourceFromSnapshot(
  snapshot: PoliticsStaticSnapshot,
  options: {
    scenario?: PoliticsScenarioSnapshot;
    simulations?: number;
    electionDate?: string;
  } = {}
): PoliticsPipelineSource {
  const scenario = options.scenario ?? snapshot.default_scenario;

  return {
    data_elezione: options.electionDate ?? scenario.data_elezione,
    simulazioni: options.simulations ?? 1,
    frazione_uni_in_pluri: scenario.frazione_uni_in_pluri,
    frazioni_pluricandidature: scenario.frazioni_pluricandidature,
    default_data_nascita: scenario.default_data_nascita,
    liste: scenario.liste,
    comuni_liste: scenario.comuni_liste,
    base_dati: snapshot.data.base_dati,
    camera: {
      uni: snapshot.data.camera.uni,
      pluri: snapshot.data.camera.pluri,
      candidati_uni: scenario.camera.candidati_uni,
      candidati_pluri: scenario.camera.candidati_pluri
    },
    senato: {
      uni: snapshot.data.senato.uni,
      pluri: snapshot.data.senato.pluri,
      candidati_uni: scenario.senato.candidati_uni,
      candidati_pluri: scenario.senato.candidati_pluri
    }
  };
}
