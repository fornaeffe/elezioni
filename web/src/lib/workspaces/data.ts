import { runtimeDataUrl } from '../core/runtime-data';
import type { PoliticsStaticSnapshot } from '../politics/types';
import type { RegionalErStaticSnapshot } from '../regional-er/types';
import { snapshotDescriptors } from './snapshots.generated';

export const reformPointerPath = 'data/reform-2026/fast-share/candidate.json';
export interface SnapshotDescriptor {
  path: string;
  bytes: number;
  sha256: string;
  schemaVersion: number;
  version: string;
}
export type SnapshotKind = 'politics' | 'regional-er';
type Snapshots = { politics: PoliticsStaticSnapshot; 'regional-er': RegionalErStaticSnapshot };

export function validateSnapshot<K extends SnapshotKind>(kind: K, value: unknown, schemaVersion: number): Snapshots[K] {
  if (schemaVersion !== (kind === 'politics' ? 4 : 1)) throw new Error('Unsupported workspace snapshot schema');
  if (!value || typeof value !== 'object') throw new Error('Invalid workspace snapshot');
  const s = value as { metadata?: { schema_version?: number }; data?: Record<string, unknown>; default_scenario?: Record<string, unknown> };
  if (s.metadata?.schema_version !== schemaVersion || !s.data || !s.default_scenario)
    throw new Error('Workspace snapshot schema mismatch');
  for (const key of ['comuni_liste_elezioni', 'municipalities'])
    if (!Array.isArray(s.data[key]) || !s.data[key].length) throw new Error(`Missing snapshot ${key}`);
  for (const key of ['liste', 'comuni_liste'])
    if (!Array.isArray(s.default_scenario[key]) || !s.default_scenario[key].length) throw new Error(`Missing snapshot default ${key}`);
  if (kind === 'politics') {
    if (!Array.isArray(s.data.base_dati) || !s.data.base_dati.length) throw new Error('Missing politics geography');
    for (const ch of ['camera', 'senato']) {
      const geography = s.data[ch] as Record<string, unknown> | undefined;
      const candidates = s.default_scenario[ch] as Record<string, unknown> | undefined;
      if (!geography || !['uni', 'pluri'].every(k => Array.isArray(geography[k]) && geography[k].length) ||
        !candidates || !['candidati_uni', 'candidati_pluri'].every(k => Array.isArray(candidates[k])))
        throw new Error(`Invalid politics ${ch} snapshot`);
    }
  } else if (!Array.isArray(s.data.pop_legale) || !s.data.pop_legale.length) throw new Error('Missing regional census');
  return value as Snapshots[K];
}

export async function loadWorkspaceSnapshot<K extends SnapshotKind>(kind: K, base: string): Promise<Snapshots[K]> {
  const descriptor = snapshotDescriptors[kind];
  if (!descriptor) throw new Error(`Missing production descriptor for ${kind}. Run npm run data:workspaces:generate.`);
  const url = runtimeDataUrl(base, descriptor.path), response = await fetch(url);
  if (!response.ok) throw new Error(`Missing production snapshot ${url}. Generate the required production data.`);
  const bytes = new Uint8Array(await response.arrayBuffer());
  const hash = Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256', bytes)), b => b.toString(16).padStart(2, '0')).join('');
  if (bytes.length !== descriptor.bytes || hash !== descriptor.sha256) throw new Error(`Workspace snapshot checksum mismatch: ${kind}`);
  return validateSnapshot(kind, JSON.parse(new TextDecoder().decode(bytes)), descriptor.schemaVersion);
}
