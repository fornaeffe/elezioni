import type { ExportScenario } from '../core/result-export';
import { JsonStreamReader, jsonFileStream } from '../core/json-stream';
import { workspaceFor, type WorkspaceDefinition } from './registry';
import { inspectPoliticsFile } from '../politics/files/metadata';
import { parsePoliticsScenario } from '../politics/scenario/validation';
import { OLD_POLITICS_FORMAT } from '../politics/files/validation';

export function scenarioWorkspace(value: unknown): WorkspaceDefinition {
  if (!value || typeof value !== 'object') throw new Error('Missing scenario identity');
  const s = value as Partial<ExportScenario> & { kind?: string; algorithmId?: string; defaultSource?: { electionKind?: unknown } };
  if (s.kind === 'politics-scenario') return workspaceFor('politiche', parsePoliticsScenario(value).selection.scrutiny.id);
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
        if (standalone.defaultSource || standalone.kind) throw stop;
      } else await reader.skip();
    });
  } catch (e) { if (e !== stop) throw e; }
  finally { await reader.close(); }
  const s = (scenario ?? standalone) as Record<string, unknown>;
  // Regional files keep the independent metadata routing and format contract.
  if (s.defaultSource && (s.defaultSource as { electionKind?: string }).electionKind === 'regionali-er') return scenarioWorkspace(s);
  if (['politics-scenario', 'politics-result', 'politics-result-archive'].includes(String(s.kind))) {
    const metadata = await inspectPoliticsFile(file);
    return workspaceFor('politiche', metadata.selection.scrutiny.id);
  }
  throw new Error(OLD_POLITICS_FORMAT);
}

/** Removed with the transitional editors in CP-06/07. Never feed common files
 * to an old editor, or switch/destroy the current editor before full import. */
export async function transitionalFileWorkspace(file: File): Promise<WorkspaceDefinition> {
  const target = await fileWorkspace(file);
  if (target.kind === 'politiche') throw new Error('Il file usa il nuovo formato comune. Il caricamento sarà disponibile nel nuovo editor politiche.');
  return target;
}
export function transitionalScenarioWorkspace(value: unknown): WorkspaceDefinition {
  const s = value as { kind?: string; defaultSource?: { electionKind?: string } } | null;
  if (s?.defaultSource?.electionKind === 'regionali-er') return scenarioWorkspace(value);
  if (s?.kind === 'politics-scenario') {
    parsePoliticsScenario(value);
    throw new Error('Il file usa il nuovo formato comune. Il caricamento sarà disponibile nel nuovo editor politiche.');
  }
  throw new Error(OLD_POLITICS_FORMAT);
}
