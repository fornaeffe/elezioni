/** Acceptance preserves fixed-vote scrutiny behavior, including approved errors.
 * This policy never turns a failed legal outcome into a successful election. */
export interface CampaignLegalError {
  code: string;
  simulationId: number;
  message: string;
  chamber: string;
  reason: string;
  scope?: string;
}
export function assessCampaignLegalOutcomes(law: string,
  simulations: readonly { id: number; outcome: string }[], errors: readonly CampaignLegalError[]) {
  const expected = (error: CampaignLegalError) => law === 'politiche-r-parity-v1'
    && error.code === 'ROSATELLUM_EXECUTION_ERROR' && error.chamber === 'camera'
    && error.reason === 'scrutiny-failed' && error.message === 'Non ho a chi dare il seggio eccedente';
  const acceptedFailureIds: number[] = [], unexpectedSimulationIds: number[] = [];
  for (const simulation of simulations) {
    const reasons = errors.filter(error => error.simulationId === simulation.id);
    if (simulation.outcome === 'complete' && reasons.length === 0) continue;
    if (['failed', 'partial'].includes(simulation.outcome) && reasons.length > 0 && reasons.every(expected))
      acceptedFailureIds.push(simulation.id);
    else unexpectedSimulationIds.push(simulation.id);
  }
  const ids = new Set(simulations.map(simulation => simulation.id));
  const unmatchedErrorSimulationIds = [...new Set(errors.filter(error => !ids.has(error.simulationId)).map(error => error.simulationId))];
  return { accepted: unexpectedSimulationIds.length === 0 && unmatchedErrorSimulationIds.length === 0,
    acceptedFailureIds, unexpectedSimulationIds, unmatchedErrorSimulationIds };
}
