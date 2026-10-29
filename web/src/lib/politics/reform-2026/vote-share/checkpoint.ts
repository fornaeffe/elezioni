/** Self-contained immutable generation state; original refit chunks remain demand-loaded. */
import { encodeState } from './state-codec';
import { fittedStateShapes, profileStateShapes } from './state-shapes';
import { canonicalJson } from './source-codec';
import { requireFastScenario } from './scenario';
import type { AcceptedFit } from './fit-types';
import type {
  CandidatePointer,
  ReleasedScenario,
  ScenarioState,
  ReleaseCatalog,
} from './release';
export interface PortableFitFile {
  kind: 'politics-reform-session';
  schemaVersion: 1;
  scenario: ReleasedScenario;
  pointer: CandidatePointer;
  assets: Record<string, string[]>;
}
const maxBytes = 64 * 1024 * 1024;
export function requirePortableFitFile(value: unknown): PortableFitFile {
  if (!value || typeof value !== 'object' || Array.isArray(value))
    throw new Error('Expected a fitted reform session.');
  const file = value as PortableFitFile;
  if (
    Object.keys(file).sort().join('|') !==
      'assets|kind|pointer|scenario|schemaVersion' ||
    file.kind !== 'politics-reform-session' ||
    file.schemaVersion !== 1 ||
    !file.assets ||
    typeof file.assets !== 'object' ||
    Array.isArray(file.assets) ||
    Object.keys(file.assets).length > 16 ||
    !file.pointer ||
    typeof file.pointer !== 'object'
  )
    throw new Error('Unsupported fitted session.');
  requireFastScenario(file.scenario);
  let bytes = 0;
  for (const [path, chunks] of Object.entries(file.assets)) {
    if (
      !/^[\w.-]+-[a-f0-9]{64}\.gz$/.test(path) ||
      !Array.isArray(chunks) ||
      chunks.length > 4096 ||
      chunks.some(
        (c) =>
          typeof c !== 'string' ||
          c.length > 65536 ||
          !/^[A-Za-z0-9+/]*={0,2}$/.test(c),
      )
    )
      throw new Error('Invalid fitted session asset.');
    bytes += chunks.reduce((n, c) => n + (c.length * 3) / 4, 0);
    if (bytes > maxBytes)
      throw new Error('Fitted session exceeds the supported size.');
  }
  return file;
}
export function checkpointReader(file: PortableFitFile) {
  requirePortableFitFile(file);
  return async (path: string) => {
    const chunks = file.assets[path];
    if (!chunks) throw new Error('Missing fitted session asset ' + path);
    const decoded = chunks.map((c) =>
        Uint8Array.from(atob(c), (s) => s.charCodeAt(0)),
      ),
      bytes = new Uint8Array(decoded.reduce((n, b) => n + b.length, 0));
    let at = 0;
    for (const b of decoded) {
      bytes.set(b, at);
      at += b.length;
    }
    return bytes;
  };
}
export async function createCheckpoint(
  accepted: AcceptedFit,
  draft: ReleasedScenario = accepted.scenario,
): Promise<PortableFitFile> {
  requireFastScenario(draft);
  const p = accepted.portableStates,
    fit = await encodeState(
      'fast-share-fit',
      p.fit.state,
      p.fit.metadata,
      fittedStateShapes(accepted.assets.fit),
    ),
    profiles = await encodeState(
      'fast-share-profiles',
      p.profiles.state,
      p.profiles.metadata,
      profileStateShapes(accepted.assets.cameraProfile),
    ),
    runtime = await encodeState(
      'fast-share-runtime',
      p.runtime.state,
      p.runtime.metadata,
    );
  if (
    fit.envelope.id !== accepted.assets.fittedStateId ||
    profiles.envelope.id !== accepted.assets.profileStateId ||
    runtime.envelope.id !== accepted.sourceCatalog.runtimeId
  )
    throw new Error('Fitted session state binding mismatch.');
  const metadata = {
      ...p.runtime.metadata,
      methodVersions: [
        ...p.runtime.metadata.methodVersions,
        accepted.implementations.generation,
      ],
    },
    scenario = await encodeState(
      'fast-share-scenario',
      {
        scenario: accepted.scenario,
        selection: accepted.selection,
        defaultPolicy: { id: 'browser-session-v1' },
        keys: accepted.keys,
        electorateKey: accepted.electorateKey,
      } satisfies ScenarioState,
      metadata,
    ),
    state: ReleaseCatalog = {
      ...accepted.sourceCatalog,
      policyId: 'browser-session-v1',
      referenceDate: accepted.scenario.referenceDate,
      electionDate: accepted.scenario.electionDate,
      fittedStateId: fit.envelope.id,
      profileStateId: profiles.envelope.id,
      runtimeId: runtime.envelope.id,
      scenarioId: scenario.envelope.id,
      nationalInputKey: accepted.nationalInputKey,
      profileInputKey: accepted.profileInputKey,
      pairedInputKey: accepted.pairedInputKey,
      ...accepted.keys,
      assets: {
        ...accepted.sourceCatalog.assets,
        fit: fit.descriptor,
        profiles: profiles.descriptor,
        runtime: runtime.descriptor,
        scenario: scenario.descriptor,
      },
    },
    catalog = await encodeState('fast-share-catalog', state, metadata),
    files = [fit, profiles, runtime, scenario, catalog],
    assets: Record<string, string[]> = {};
  let total = 0;
  for (const f of files)
    for (const [path, bytes] of f.assets) {
      total += bytes.length;
      if (total > maxBytes)
        throw new Error('Fitted session exceeds the supported size.');
      assets[path] = [];
      for (let at = 0; at < bytes.length; at += 49152)
        assets[path].push(
          btoa(String.fromCharCode(...bytes.subarray(at, at + 49152))),
        );
    }
  return {
    kind: 'politics-reform-session',
    schemaVersion: 1,
    scenario: structuredClone(draft),
    pointer: {
      schemaVersion: 1,
      kind: 'fast-share-candidate',
      assetRoot: 'data/reform-2026/fast-share/',
      catalogId: catalog.envelope.id,
      catalog: catalog.descriptor,
    },
    assets,
  };
}
export function sameCheckpointScenario(
  a: ReleasedScenario,
  b: ReleasedScenario,
) {
  return canonicalJson(a) === canonicalJson(b);
}
