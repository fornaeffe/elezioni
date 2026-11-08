import type { Schema, ValidationIssue } from '../../core/json-shape';

export const OLD_POLITICS_FORMAT = 'Formato politiche precedente non supportato. Caricare un file nel nuovo formato comune; non è disponibile una conversione automatica.';
export function checkShape<T>(shape: Schema<T>, value: unknown, path = '$'): T {
  const issues: ValidationIssue[] = [];
  if (!shape.check(value, path, issues)) throw new Error(`INVALID_POLITICS_FILE: ${issues.map(e => `${e.path}: ${e.message}`).join('\n')}`);
  return value as T;
}
export function record(value: unknown): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) throw new Error('INVALID_POLITICS_FILE: expected an object');
  return value as Record<string, unknown>;
}
export function requireHeader(value: unknown, kind: string): Record<string, unknown> {
  const r = record(value);
  if (r.kind !== kind) throw new Error(OLD_POLITICS_FORMAT);
  if (r.schemaVersion !== 1) throw new Error(`UNSUPPORTED_POLITICS_FILE_VERSION: ${kind}/${String(r.schemaVersion)}`);
  return r;
}
