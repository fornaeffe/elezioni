/** Routing only: bounded selected values, no result rows, fitted arrays or state changes. */
import { JsonStreamReader, jsonFileStream } from '../../core/json-stream';
import { parsePoliticsScenario } from '../scenario/validation';
import { selectionShape, type PoliticsScenario } from '../scenario/schema';
import { requirePair } from '../scenario/registry';
import { OLD_POLITICS_FORMAT } from './validation';

export interface PoliticsFileMetadata {
  kind: 'politics-scenario' | 'politics-result' | 'politics-result-archive';
  schemaVersion: 1;
  selection: PoliticsScenario['selection'];
}
export async function inspectPoliticsFile(file: File): Promise<PoliticsFileMetadata> {
  const reader = new JsonStreamReader(jsonFileStream(file)), stop = Symbol('metadata');
  let kind: unknown, version: unknown, selection: unknown;
  let scenarioReady = false, runReady = false;
  const done = () => {
    if (typeof kind === 'string' && !['politics-scenario', 'politics-result', 'politics-result-archive'].includes(kind)) throw new Error(OLD_POLITICS_FORMAT);
    if (version !== undefined && version !== 1) throw new Error('UNSUPPORTED_POLITICS_FILE_VERSION');
    if (kind && version === 1 && selection && (kind === 'politics-scenario' ||
      kind === 'politics-result' && scenarioReady || kind === 'politics-result-archive' && scenarioReady && runReady)) throw stop;
  };
  async function scenario() {
    let scenarioKind: unknown, scenarioVersion: unknown;
    await reader.object(async key => {
      if (key === 'kind') scenarioKind = await reader.value();
      else if (key === 'schemaVersion') scenarioVersion = await reader.value();
      else if (key === 'selection') selection = await reader.value();
      else await reader.skip();
      if (scenarioKind !== undefined && scenarioKind !== 'politics-scenario') throw new Error(OLD_POLITICS_FORMAT);
      if (scenarioVersion !== undefined && scenarioVersion !== 1) throw new Error('UNSUPPORTED_POLITICS_SCENARIO_VERSION');
      if (scenarioKind && scenarioVersion === 1 && selection) { scenarioReady = true; done(); }
    });
  }
  async function run() {
    let runKind: unknown, runVersion: unknown;
    await reader.object(async key => {
      if (key === 'kind') runKind = await reader.value();
      else if (key === 'schemaVersion') runVersion = await reader.value();
      else if (key === 'executedScenario') await scenario();
      else await reader.skip();
      if (runKind !== undefined && runKind !== 'politics-result') throw new Error(OLD_POLITICS_FORMAT);
      if (runVersion !== undefined && runVersion !== 1) throw new Error('UNSUPPORTED_POLITICS_RESULT_VERSION');
      if (runKind && runVersion === 1) { runReady = true; done(); }
    });
  }
  try {
    await reader.object(async key => {
      if (key === 'kind') kind = await reader.value();
      else if (key === 'schemaVersion') version = await reader.value();
      else if (key === 'selection') selection = await reader.value();
      else if (key === 'executedScenario') await scenario();
      else if (key === 'run') await run();
      else if (['schema_version', 'scenario', 'defaultSource'].includes(key)) throw new Error(OLD_POLITICS_FORMAT);
      else await reader.skip();
      done();
    });
  } catch (error) { if (error !== stop) throw error; }
  finally { await reader.close(); }
  if (!kind || version !== 1 || !selection || kind !== 'politics-scenario' && !scenarioReady || kind === 'politics-result-archive' && !runReady)
    throw new Error('MISSING_POLITICS_FILE_METADATA');
  // Reuse the selection grammar without silently restoring a missing per-law choice.
  const issues: import('../../core/json-shape').ValidationIssue[] = [];
  if (!selectionShape.check(selection, '$.selection', issues)) throw new Error('UNSUPPORTED_POLITICS_SELECTION');
  const s = selection as PoliticsScenario['selection'];
  for (const law of Object.keys(s.generatorsByLaw) as (keyof typeof s.generatorsByLaw)[]) requirePair(law, s.generatorsByLaw[law].id);
  return { kind: kind as PoliticsFileMetadata['kind'], schemaVersion: 1, selection: s };
}

export async function readPoliticsScenarioFile(file: File): Promise<PoliticsScenario> {
  const reader = new JsonStreamReader(jsonFileStream(file));
  try { const value = await reader.value(); await reader.finish(); return parsePoliticsScenario(value); }
  finally { await reader.close(); }
}
