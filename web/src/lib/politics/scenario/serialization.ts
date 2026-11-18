import { gzipSync, gunzipSync } from 'fflate';
import { canonicalJson } from './dependencies';
import { parsePoliticsScenario, type DomainContext } from './validation';
import type { PoliticsScenario } from './schema';

export function serializeScenario(scenario: PoliticsScenario, context?: DomainContext): string {
  return canonicalJson(parsePoliticsScenario(scenario, context));
}
export function parseScenarioJson(json: string, context?: DomainContext): PoliticsScenario {
  let value: unknown;
  try { value = JSON.parse(json); } catch { throw new Error('INVALID_SCENARIO_JSON'); }
  return parsePoliticsScenario(value, context);
}
export function encodeScenario(scenario: PoliticsScenario, gzip = false, context?: DomainContext): Uint8Array<ArrayBuffer> {
  const bytes = new TextEncoder().encode(serializeScenario(scenario, context));
  return gzip ? gzipSync(bytes, { mtime: 0 }) : bytes;
}
export function decodeScenario(bytes: Uint8Array<ArrayBuffer>, context?: DomainContext): PoliticsScenario {
  try {
    const decoded = bytes[0] === 0x1f && bytes[1] === 0x8b ? gunzipSync(bytes) : bytes;
    return parseScenarioJson(new TextDecoder('utf-8', { fatal: true }).decode(decoded), context);
  } catch (error) {
    if (error instanceof Error && (error.name === 'ScenarioValidationError' || error.message === 'INVALID_SCENARIO_JSON')) throw error;
    throw new Error('INVALID_SCENARIO_ENCODING', { cause: error });
  }
}
