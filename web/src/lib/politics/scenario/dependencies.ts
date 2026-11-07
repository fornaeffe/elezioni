import { activeGenerator } from './registry';
import { type Asset, type PoliticsScenario } from './schema';
import { laws } from './schema';

export function declaredAssets(s: PoliticsScenario): readonly Asset[] {
  return [s.data.historical, s.data.catalogue, s.data.referenceGeography, s.data.electorate,
    ...laws.flatMap(law => { const a = s.data.lawAssets[law]; return [a.geography, a.seats, ...a.models.M01, ...a.models.M02]; })];
}

/** Descriptors identify exact decoded bundles. Compressed transport checksums belong to packaging. */
export function requiredAssets(s: PoliticsScenario): readonly Asset[] {
  const law = s.data.lawAssets[s.selection.scrutiny.id];
  const assets = [s.data.historical, s.data.catalogue, s.data.referenceGeography, s.data.electorate,
    law.geography, law.seats, ...law.models[activeGenerator(s).id]];
  const result = new Map<string, Asset>();
  for (const asset of assets) {
    const prior = result.get(asset.id);
    if (prior && assetIdentity(prior) !== assetIdentity(asset)) throw new Error(`CONFLICTING_DEPENDENCY: ${asset.id}`);
    result.set(asset.id, asset);
  }
  return [...result.values()];
}
export function assetIdentity(a: Asset): string {
  return JSON.stringify([a.id, a.version, a.schemaVersion, a.sha256, a.bytes]);
}
export function resolveDependencies(s: PoliticsScenario, available: readonly Asset[]): readonly Asset[] {
  return requiredAssets(s).map(required => {
    const matches = available.filter(a => assetIdentity(a) === assetIdentity(required));
    if (matches.length !== 1) throw new Error(`EXACT_DEPENDENCY_UNAVAILABLE: ${required.id}@${required.version} (${required.sha256}); load the pinned bundle from ${required.uri}`);
    return matches[0];
  });
}
/** Sort object keys, preserving meaningful array/slot order. No name/color dependencies are inferred here. */
export function canonicalJson(value: unknown): string {
  return JSON.stringify(value, (_key, row: unknown) => row && typeof row === 'object' && !Array.isArray(row)
    ? Object.fromEntries(Object.entries(row).sort(([a], [b]) => a < b ? -1 : a > b ? 1 : 0)) : row);
}
export async function dependencyFingerprint(s: PoliticsScenario): Promise<string> {
  const bytes = new TextEncoder().encode(canonicalJson(requiredAssets(s).map(assetIdentity).sort()));
  const hash = await crypto.subtle.digest('SHA-256', bytes);
  return [...new Uint8Array(hash)].map(n => n.toString(16).padStart(2, '0')).join('');
}
export async function verifyAssetBytes(descriptor: Asset, bytes: Uint8Array<ArrayBuffer>): Promise<void> {
  const hash = await crypto.subtle.digest('SHA-256', bytes);
  const checksum = [...new Uint8Array(hash)].map(n => n.toString(16).padStart(2, '0')).join('');
  if (descriptor.bytes !== bytes.byteLength || descriptor.sha256 !== checksum)
    throw new Error(`DEPENDENCY_CHECKSUM_MISMATCH: ${descriptor.id}`);
}
