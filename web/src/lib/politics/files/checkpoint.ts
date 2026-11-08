/** Bind existing v1 numeric state payloads to the complete common scenario. */
import { canonicalJson, requiredAssets } from '../scenario/dependencies';
import { parsePoliticsScenario } from '../scenario/validation';
import type { PoliticsScenario } from '../scenario/schema';
import { activeGenerator } from '../scenario/registry';
import { checkpointShape, type ExecutionProvenance, type PoliticsCheckpoint } from '../scenario/envelopes';
import { byteHash } from '../reform-2026/vote-share/source-codec';
import { decodeState, type encodeState } from '../reform-2026/vote-share/state-codec';
import type { FitState, ProfileState } from '../reform-2026/vote-share/release';
import { fittedStateShapes } from '../reform-2026/vote-share/state-shapes';
import { checkShape, record, requireHeader } from './validation';

export const scenarioFingerprint = (scenario: PoliticsScenario) => byteHash(new TextEncoder().encode(canonicalJson(parsePoliticsScenario(scenario))));
export function parsePoliticsCheckpoint(value: unknown): PoliticsCheckpoint {
  requireHeader(value, 'politics-checkpoint');
  return structuredClone(checkShape(checkpointShape, value));
}

export async function createPoliticsCheckpoint(scenario: PoliticsScenario,
  execution: Pick<ExecutionProvenance, 'implementationSha256' | 'preparationKey'>,
  states: readonly (Awaited<ReturnType<typeof encodeState>> & { envelope: { kind: 'fast-share-fit' | 'fast-share-profiles' } })[]): Promise<PoliticsCheckpoint> {
  const s = parsePoliticsScenario(scenario), assets = new Map<string, Uint8Array>();
  for (const state of states) for (const [path, bytes] of state.assets) {
    if (assets.has(path) && await byteHash(assets.get(path)!) !== await byteHash(bytes)) throw new Error('CONFLICTING_POLITICS_CHECKPOINT_ASSET');
    assets.set(path, bytes);
  }
  const file: PoliticsCheckpoint = { kind: 'politics-checkpoint', schemaVersion: 1,
    scenarioSha256: await scenarioFingerprint(s), dependencies: requiredAssets(s), implementationSha256: execution.implementationSha256,
    preparationKey: execution.preparationKey, generator: activeGenerator(s), numericalVersion: s.modelSettings.M02.policies.numerical,
    fittingSeed: s.execution.fittingSeed,
    states: states.map(s => ({ kind: s.envelope.kind, descriptor: s.descriptor })),
    assets: [...assets].map(([path, bytes]) => {
      const chunks: string[] = [];
      for (let i = 0; i < bytes.length; i += 49152) chunks.push(btoa(String.fromCharCode(...bytes.subarray(i, i + 49152))));
      return { path, chunks };
    }) };
  return validatePoliticsCheckpoint(file, s, execution);
}

/** Validation never restores a draft or reads a former scenario-bearing state.
 * Execution also checks the fitted component/geography basis against loaded assets. */
export async function validatePoliticsCheckpoint(value: unknown, scenario: PoliticsScenario,
  execution: Pick<ExecutionProvenance, 'implementationSha256' | 'preparationKey'>) {
  const s = parsePoliticsScenario(scenario), file = parsePoliticsCheckpoint(value), generator = activeGenerator(s);
  if (generator.id !== 'M02' || canonicalJson(file.generator) !== canonicalJson(generator) ||
    file.scenarioSha256 !== await scenarioFingerprint(s) ||
    canonicalJson(file.dependencies) !== canonicalJson(requiredAssets(s)) ||
    file.implementationSha256 !== execution.implementationSha256 || file.preparationKey !== execution.preparationKey ||
    file.numericalVersion !== s.modelSettings.M02.policies.numerical || file.fittingSeed !== s.execution.fittingSeed)
    throw new Error('POLITICS_CHECKPOINT_IDENTITY_MISMATCH');
  if (file.states.length !== 2 || new Set(file.states.map(s => s.kind)).size !== 2)
    throw new Error('POLITICS_CHECKPOINT_STATES: require fit and profiles only');
  const assets = new Map<string, Uint8Array>();
  let total = 0;
  for (const asset of file.assets) {
    if (assets.has(asset.path) || !/^[\w.-]+-[a-f0-9]{64}\.gz$/.test(asset.path) || !asset.chunks.length)
      throw new Error('INVALID_POLITICS_CHECKPOINT_ASSET');
    const chunks: Uint8Array[] = [];
    for (const chunk of asset.chunks) {
      if (chunk.length > 65536 || !/^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$/.test(chunk))
        throw new Error('INVALID_POLITICS_CHECKPOINT_ASSET');
      total += chunk.length * 3 / 4 - (chunk.endsWith('==') ? 2 : chunk.endsWith('=') ? 1 : 0);
      if (total > 64 * 1024 * 1024) throw new Error('POLITICS_CHECKPOINT_SIZE_LIMIT');
      chunks.push(Uint8Array.from(atob(chunk), c => c.charCodeAt(0)));
    }
    const bytes = new Uint8Array(chunks.reduce((n, c) => n + c.length, 0));
    let offset = 0;
    for (const chunk of chunks) { bytes.set(chunk, offset); offset += chunk.length; }
    assets.set(asset.path, bytes);
  }
  const used = new Set<string>();
  const read = async (path: string) => {
    const bytes = assets.get(path);
    if (!bytes) throw new Error(`MISSING_POLITICS_CHECKPOINT_ASSET: ${path}`);
    used.add(path); return bytes;
  };
  for (const state of file.states) {
    const decoded = await decodeState(state.descriptor, read, state.kind);
    if (!decoded.envelope.methodVersions.includes(file.implementationSha256) ||
      !decoded.envelope.sourceVersions.includes(s.data.historical.version))
      throw new Error('POLITICS_CHECKPOINT_STATE_BINDING_MISMATCH');
    validateNumericState(decoded, state.kind, s);
  }
  if (used.size !== assets.size) throw new Error('UNUSED_POLITICS_CHECKPOINT_ASSET');
  return file;
}

function validateNumericState(decoded: Awaited<ReturnType<typeof decodeState>>, kind: 'fast-share-fit' | 'fast-share-profiles', s: PoliticsScenario) {
  const state = record(decoded.state), parents = record(state.parents);
  const keys = kind === 'fast-share-fit' ? ['parents', 'fit', 'provenanceId', 'warnings', 'sampler'] : ['parents', 'cameraProfile', 'senateAdapter', 'pairedAudit'];
  if (Object.keys(state).sort().join('|') !== keys.sort().join('|') ||
    typeof parents.implementationId !== 'string' || !decoded.envelope.methodVersions.includes(parents.implementationId))
    throw new Error('POLITICS_CHECKPOINT_NUMERIC_STATE');
  function shape(path: string, expected: readonly number[]) {
    if (canonicalJson(decoded.envelope.payload.columns.find(c => c.name === path)?.shape) !== canonicalJson(expected))
      throw new Error(`POLITICS_CHECKPOINT_NUMERIC_SHAPE: ${path}`);
  }
  if (kind === 'fast-share-fit') {
    const f = state as unknown as FitState;
    if (f.parents.fitSeed !== s.execution.fittingSeed || !['dense', 'matrix-free'].includes(f.parents.solver) ||
      f.fit.referenceDate !== s.referenceDate || f.sampler.chains !== 4 || f.sampler.burn !== 1000 || f.sampler.thin !== 2 ||
      f.sampler.retention !== 'all-accepted-joint-context-draws') throw new Error('POLITICS_CHECKPOINT_FIT_POLICY');
    const shapes = fittedStateShapes(f.fit);
    for (const key of ['political', 'participation'] as const) {
      const sample = f.fit[key];
      if (sample.chains !== 4 || ![2000, 4000, 8000].includes(sample.drawsPerChain) || !Number.isSafeInteger(sample.dimensions) ||
        sample.dimensions < 1 || !(sample.values instanceof Float64Array) || !sample.diagnostics.passed)
        throw new Error('POLITICS_CHECKPOINT_FIT_DIAGNOSTICS');
      shape(`/fit/${key}/values`, shapes[`/fit/${key}/values`]);
    }
  } else {
    const p = state as unknown as ProfileState;
    if (p.parents.referenceDate !== s.referenceDate || p.senateAdapter.referenceDate !== s.referenceDate ||
      p.senateAdapter.version !== 'paired-18plus-logratio-adapter-v1') throw new Error('POLITICS_CHECKPOINT_PROFILE_POLICY');
    for (const [path, profile] of [['/cameraProfile', p.cameraProfile], ['/senateAdapter/profile', p.senateAdapter.profile]] as const) {
      if (!profile.componentIds.length || new Set(profile.componentIds).size !== profile.componentIds.length ||
        !profile.municipalityIds.length || new Set(profile.municipalityIds).size !== profile.municipalityIds.length ||
        !(profile.means instanceof Float64Array) || !(profile.profileSd instanceof Float64Array) || !(profile.fallback instanceof Uint8Array))
        throw new Error('POLITICS_CHECKPOINT_PROFILE_BASIS');
      for (const field of ['means', 'profileSd', 'fallback']) shape(`${path}/${field}`, [profile.municipalityIds.length, profile.componentIds.length + 1]);
    }
  }
}
