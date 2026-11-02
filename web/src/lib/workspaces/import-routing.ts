import type { ExportScenario } from '../core/result-export';
import { JsonStreamReader, jsonFileStream } from '../core/json-stream';
import { workspaceFor, type WorkspaceDefinition } from './registry';

export function scenarioWorkspace(value: unknown): WorkspaceDefinition {
  if (!value || typeof value !== 'object') throw new Error('Missing scenario identity');
  const s = value as Partial<ExportScenario> & { kind?: string; algorithmId?: string; defaultSource?: { electionKind?: unknown } };
  if (s.kind === 'politics-reform-scenario') return workspaceFor('politiche', s.algorithmId);
  const kind = s.defaultSource?.electionKind;
  if (kind !== 'politiche' && kind !== 'regionali-er') throw new Error('Unsupported scenario election');
  return workspaceFor(kind, s.algorithmId);
}

/** Inspect metadata in bounded streams; never parse a complete large archive. */
export async function fileWorkspace(file: File): Promise<WorkspaceDefinition> {
  const reader = new JsonStreamReader(jsonFileStream(file));
  const stop = Symbol('metadata');
  let scenario: unknown, standalone: Record<string, unknown> = {};
  try {
    await reader.object(async key => {
      if (key === 'scenario') { scenario = await reader.value(); throw stop; }
      if (['kind', 'algorithmId', 'defaultSource'].includes(key)) {
        standalone[key] = await reader.value();
        if (standalone.defaultSource || standalone.kind && standalone.algorithmId) throw stop;
      } else await reader.skip();
    });
  } catch (e) { if (e !== stop) throw e; }
  finally { await reader.close(); }
  return scenarioWorkspace(scenario ?? standalone);
}
