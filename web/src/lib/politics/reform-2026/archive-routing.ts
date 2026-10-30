/** Read only the archive header. Large result tables remain streamed by their owner. */
import { JsonStreamReader, jsonFileStream } from '../../core/json-stream';
export type ReformWorkflow = 'installed' | 'fast';
export function scenarioWorkflow(value: unknown): ReformWorkflow {
  if (
    !value ||
    typeof value !== 'object' ||
    !('kind' in value) ||
    value.kind !== 'politics-reform-scenario' ||
    !('schemaVersion' in value)
  )
    throw new Error('Expected an explicit reform scenario');
  if (value.schemaVersion === 4) return 'fast';
  if (
    [1, 2, 3].includes(Number(value.schemaVersion)) &&
    typeof value.schemaVersion === 'number'
  )
    return 'installed';
  throw new Error('Unsupported reform scenario version');
}
export async function archiveWorkflow(file: File): Promise<ReformWorkflow> {
  const reader = new JsonStreamReader(jsonFileStream(file)),
    stop = Symbol('header');
  let kind: unknown, version: unknown, workflow: ReformWorkflow | undefined;
  try {
    await reader.object(async (key) => {
      if (key === 'kind') kind = await reader.value();
      else if (key === 'schemaVersion') version = await reader.value();
      else if (key === 'scenario') {
        workflow = scenarioWorkflow(await reader.value());
        throw stop;
      } else await reader.skip();
      if (kind === 'politics-reform-session') {
        workflow = 'fast';
        throw stop;
      }
      if (kind === 'politics-reform-scenario' && version !== undefined) {
        workflow = scenarioWorkflow({ kind, schemaVersion: version });
        throw stop;
      }
    });
  } catch (e) {
    if (e !== stop) throw e;
  } finally {
    await reader.close();
  }
  if (!workflow) throw new Error('Missing supported reform archive header');
  return workflow;
}
