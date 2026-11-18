import type { Scenario, ScenarioCoalition, ScenarioList } from '../core/types';

export function isAutonomousPoliticsList(list: ScenarioList): boolean {
  return list.kind !== 'other' && list.filing !== undefined && list.coalition === null;
}

/** The old legal adapter groups candidates by a named ticket. Keep that
 * representation out of the scenario's political-coalition collection. */
export function politicsTicketName(list: ScenarioList): string | null {
  return isAutonomousPoliticsList(list) ? list.name : list.coalition;
}

export function politicsCandidateTickets(scenario: Scenario): Array<ScenarioCoalition & { independent?: boolean }> {
  return [...scenario.coalitions, ...scenario.lists.filter(isAutonomousPoliticsList)
    .map(l => ({ id: `independent-${l.id}`, name: l.name, color: l.color, independent: true }))];
}
