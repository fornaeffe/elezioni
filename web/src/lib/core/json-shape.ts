export interface ValidationIssue {
  readonly code: 'INVALID_SHAPE' | 'DUPLICATE_ID' | 'UNKNOWN_REFERENCE' | 'INVALID_LINK' |
    'INVALID_CANDIDATURE' | 'INVALID_COVERAGE' | 'INVALID_ENDOWMENT' | 'INVALID_VOTES' |
    'INVALID_PREFERENCES' | 'INCOMPATIBLE_RENEWAL' | 'INVALID_METADATA';
  readonly path: string;
  readonly message: string;
  readonly lawReference: string | null;
}

export interface Schema<T> {
  readonly optional?: boolean;
  readonly check: (value: unknown, path: string, issues: ValidationIssue[]) => boolean;
  readonly type?: T;
}
type Value<S> = S extends Schema<infer T> ? T : never;
const issue = (path: string, message: string): ValidationIssue =>
  ({ code: 'INVALID_SHAPE', path, message, lawReference: null });
function scalar<T>(test: (v: unknown) => boolean, expected: string): Schema<T> {
  return { check(v, path, issues) {
    if (test(v)) return true;
    issues.push(issue(path, `Expected ${expected}.`)); return false;
  } };
}
const text = scalar<string>(v => typeof v === 'string' && v.trim().length > 0, 'a nonempty string');
const boolean = scalar<boolean>(v => typeof v === 'boolean', 'a boolean');
const count = scalar<number>(v => typeof v === 'number' && Number.isSafeInteger(v) && v >= 0, 'a nonnegative safe integer');
const positive = scalar<number>(v => typeof v === 'number' && Number.isSafeInteger(v) && v > 0, 'a positive safe integer');
const share = scalar<number>(v => typeof v === 'number' && Number.isFinite(v) && v >= 0 && v <= 1, 'a finite share in [0,1]');
export function isCalendarDate(v: unknown): v is string {
  if (typeof v !== 'string' || !/^\d{4}-\d{2}-\d{2}$/.test(v)) return false;
  const d = new Date(`${v}T00:00:00Z`);
  return Number.isFinite(d.valueOf()) && d.toISOString().slice(0, 10) === v;
}
const date = scalar<string>(isCalendarDate, 'a real YYYY-MM-DD calendar date');
function literal<const T extends string | number>(...values: T[]): Schema<T> {
  return scalar<T>(v => values.includes(v as T), values.map(String).join(' | '));
}
function nullable<T>(s: Schema<T>): Schema<T | null> {
  return { check: (v, p, e) => v === null || s.check(v, p, e) };
}
function optional<T>(s: Schema<T>): Schema<T | undefined> & { readonly optional: true } {
  return { optional: true, check: (v, p, e) => v === undefined || s.check(v, p, e) };
}
function array<T>(s: Schema<T>): Schema<readonly T[]> {
  return { check(v, p, e) {
    if (!Array.isArray(v)) { e.push(issue(p, 'Expected an array.')); return false; }
    let valid = true;
    // Index iteration also rejects sparse arrays, not just JSON-created arrays.
    for (let i = 0; i < v.length; i++) if (!s.check(v[i], `${p}[${i}]`, e)) valid = false;
    return valid;
  } };
}
type OptionalKeys<S> = { [K in keyof S]: S[K] extends { readonly optional: true } ? K : never }[keyof S];
function object<S extends Record<string, Schema<unknown>>>(fields: S): Schema<
  { readonly [K in Exclude<keyof S, OptionalKeys<S>>]: Value<S[K]> } &
  { readonly [K in OptionalKeys<S>]?: Value<S[K]> }
> {
  return { check(v, p, e) {
    if (typeof v !== 'object' || v === null || Array.isArray(v) ||
        ![Object.prototype, null].includes(Object.getPrototypeOf(v))) {
      e.push(issue(p, 'Expected a plain object.')); return false;
    }
    const row = v as Record<string, unknown>; let valid = true;
    for (const key of Object.keys(row)) if (!Object.hasOwn(fields, key)) {
      e.push(issue(`${p}.${key}`, 'Unexpected field; representations cannot be mixed.')); valid = false;
    }
    for (const [key, schema] of Object.entries(fields)) {
      if (!Object.hasOwn(row, key)) { if (!schema.optional) { e.push(issue(`${p}.${key}`, 'Required field is missing.')); valid = false; } }
      else if (!schema.check(row[key], `${p}.${key}`, e)) valid = false;
    }
    return valid;
  } };
}
function union<S extends readonly Schema<unknown>[]>(...branches: S): Schema<Value<S[number]>> {
  return { check(v, p, e) {
    let best: ValidationIssue[] | undefined;
    for (const b of branches) {
      const errors: ValidationIssue[] = [];
      if (b.check(v, p, errors)) return true;
      if (!best || errors.length < best.length) best = errors;
    }
    e.push(...(best ?? [issue(p, 'Invalid variant.')])); return false;
  } };
}


export type Infer<S> = S extends Schema<infer T> ? T : never;
export { scalar, text, boolean, count, positive, share, date, literal, nullable, optional, array, object, union };
