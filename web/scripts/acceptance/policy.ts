import { requireBrowserAcceptance } from './reports';

export const acceptancePolicy = 'focused-browser-v2';
export type AcceptanceProfile = 'development' | 'release' | 'extended';
export function acceptanceProfile(value = 'release'): AcceptanceProfile {
  if (value !== 'development' && value !== 'release' && value !== 'extended') throw new Error(`Unknown acceptance profile: ${value}`);
  return value;
}
// Expected membership is explicit: a partial matrix or accidental broad grep
// must fail, even when every executed test happened to pass.
export const browserCases = { development: 66, release: 70, editor: 1, representative: 2, fullUnified: 8, rootSmoke: 1, staged: 6 };
export function requireBrowserCases(report: Parameters<typeof requireBrowserAcceptance>[0], expected: number) {
  const counts = requireBrowserAcceptance(report);
  if (counts.passed !== expected) throw new Error(`Expected ${expected} browser checks, received ${counts.passed}`);
  return counts;
}

export interface ArtifactReceipt { path: string; bytes: number; sha256: string }
export function requireIdenticalArtifacts(cold: ArtifactReceipt[], warm: ArtifactReceipt[]) {
  const canonical = (rows: ArtifactReceipt[]) => {
    if (!rows.length || new Set(rows.map(r => r.path)).size !== rows.length) throw new Error('Missing or duplicate artifact receipts');
    return JSON.stringify([...rows].sort((a, b) => a.path.localeCompare(b.path))
      .map(({ path, bytes, sha256 }) => ({ path, bytes, sha256 })));
  };
  if (canonical(cold) !== canonical(warm)) throw new Error('Cold/warm preparation artifacts differ');
  return { compared: cold.length, identical: true };
}
