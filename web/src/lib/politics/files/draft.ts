import type { PoliticsScenario } from '../scenario/schema';
import { parseScenarioJson, serializeScenario } from '../scenario/serialization';

export const politicsDraftKey = 'elezioni:politics:common-draft:v1';
type DraftStorage = Pick<Storage, 'getItem' | 'setItem'>;

/** Only this key is accessed. Old politics drafts are neither read nor removed. */
export function readPoliticsDraft(storage: DraftStorage): PoliticsScenario | null {
  const text = storage.getItem(politicsDraftKey);
  return text === null ? null : parseScenarioJson(text);
}
export function writePoliticsDraft(storage: DraftStorage, scenario: PoliticsScenario): void {
  // Complete validation/serialization before touching persistent state.
  const text = serializeScenario(scenario);
  storage.setItem(politicsDraftKey, text);
}
