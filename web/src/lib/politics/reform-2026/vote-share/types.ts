/** Numerical inputs only. No source acquisition, held-out truth or legal inputs. */
export const FAST_SHARE_MODEL = 'fast-share-kind-common-conservative-v1';
export const FAST_SHARE_NUMERICS = 'fast-share-float64-v1';
export const FIT_STREAM_VERSION = 'fast-share-fit-streams-v1';
export type Vector = Float64Array<ArrayBuffer>;
export type FitContext = 'camera' | 'european';
export interface NationalElection {
  id: string; date: string; context: FitContext; electors: number;
  groups: string[][];
  /** Non-valid followed by the original observed group counts. */
  counts: number[];
}
export interface InformationPoll {
  id: string; surveyId?: string; date: string; context: FitContext; groups: string[][]; shares: number[];
  sample: number; errorGroup: string; availableDate: string; fieldworkEnd: string;
}
export interface NationalSource {
  components: string[]; referenceDate: string;
  elections: NationalElection[]; polls: InformationPoll[];
}
export interface GaussianTarget {
  components: number; blocks: number; precision: Vector; linear: Vector;
}
export interface ModeResult {
  point: Vector; objective: number; iterations: number;
  equalityResidual: number; scaledKktResidual: number; stationarityResidual: number;
}
export interface CoordinateDiagnostic {
  rhat: number | null; bulkEss: number; tailEss: number;
  constant: false | 'fixed' | 'stuck';
}
export interface ChainDiagnostics {
  passed: boolean; maxRhat: number | null; minEss: number; minTailEss: number;
  coordinates: CoordinateDiagnostic[];
}
export interface GaussianSamples {
  /** Chain-major, then retained draw, then explicitly ordered coordinate. */
  values: Vector; chains: number; drawsPerChain: number; dimensions: number;
  diagnostics: ChainDiagnostics;
}
export interface SamplerOptions {
  seed: string; draws?: number; burn?: number; thin?: number; chains?: number;
  maximumWorkspaceBytes?: number;
}
export interface SolverOptions {
  solver?: 'auto' | 'dense' | 'matrix-free';
  maximumWorkspaceBytes?: number;
  relativeTolerance?: number; maximumIterations?: number;
}
export const DEFAULT_WORKSPACE_BYTES = 256 * 1024 * 1024;
export function requireWorkspace(bytes: number, limit = DEFAULT_WORKSPACE_BYTES): void {
  if (!Number.isSafeInteger(bytes) || bytes < 0 || !Number.isSafeInteger(limit) || limit <= 0 || bytes > limit)
    throw new RangeError('FIT_WORKSPACE_LIMIT: requires ' + bytes + ' bytes; limit ' + limit);
}
export function sum(values: ArrayLike<number>): number {
  let total = 0; for (let i = 0; i < values.length; i++) total += values[i]; return total;
}
export function median(values: readonly number[]): number {
  if (!values.length) throw new RangeError('Empty median');
  const sorted = [...values].sort((a, b) => a - b), n = sorted.length;
  return (sorted[(n - 1) >> 1] + sorted[n >> 1]) / 2;
}
export function day(date: string): number {
  if (!/^\d{4}-\d{2}-\d{2}$/.test(date)) throw new RangeError('Invalid model date: ' + date);
  const ms = Date.parse(date + 'T00:00:00Z');
  if (!Number.isFinite(ms) || new Date(ms).toISOString().slice(0, 10) !== date) throw new RangeError('Invalid model date: ' + date);
  return ms / 86400000;
}
export const years = (after: string, before: string) => (day(after) - day(before)) / 365.25;
export const clip = (x: number, low: number, high: number) => Math.max(low, Math.min(high, x));
