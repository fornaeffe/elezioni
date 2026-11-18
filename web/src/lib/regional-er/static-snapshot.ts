import type { RegionalErPipelineSource, RegionalErScenarioSnapshot, RegionalErStaticSnapshot } from './types';

export function buildRegionalErPipelineSourceFromSnapshot(
  snapshot: RegionalErStaticSnapshot,
  options: {
    scenario?: RegionalErScenarioSnapshot;
    simulations?: number;
    electionDate?: string;
  } = {}
): RegionalErPipelineSource {
  const scenario = options.scenario ?? snapshot.default_scenario;

  return {
    data_elezione: options.electionDate ?? scenario.data_elezione,
    simulazioni: options.simulations ?? 1,
    liste: scenario.liste,
    comuni_liste: scenario.comuni_liste,
    pop_legale: snapshot.data.pop_legale
  };
}
