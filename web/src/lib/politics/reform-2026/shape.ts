import type { ReformScrutinyInput } from './input';
import { REFORM_ALGORITHM_ID, REFORM_INPUT_VERSION } from './input';

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
function optional<T>(s: Schema<T>): Schema<T | undefined> {
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
function object<S extends Record<string, Schema<unknown>>>(fields: S): Schema<{ readonly [K in keyof S]: Value<S[K]> }> {
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

const chamber = literal('camera', 'senato');
const subject = object({ kind: literal('list', 'coalition'), id: text });
const provenance = union(
  object({ kind: literal('supplied'), reference: text }),
  object({ kind: literal('generated'), modelId: text, parameterVersion: text, profileId: text,
    sourceVersions: array(text), seed: text, assumptions: array(text) })
);
const listVote = object({ listId: text, votes: count });
const candidateVote = object({ candidateId: text, votes: count });
const electorate = object({ minimumAge: literal(18), electors: nullable(count) });
const preferences = union(
  object({ kind: literal('missing'), reason: text }),
  object({ kind: literal('counts'), counts: array(object({ candidateId: text, count })), provenance,
    validity: union(
      object({ kind: literal('adjudicated'), reference: text }),
      object({ kind: literal('combinations'), ballots: array(object({ candidateIds: array(text), count })) })
    ) })
);
const ordinaryVotes = union(
  object({ kind: literal('components'), directListVotes: array(listVote),
    bonusOnlyVotes: array(object({ subject, votes: count })), provenance }),
  object({ kind: literal('attributed'), listVotes: array(listVote), provenance })
);
const specialVotes = union(
  object({ kind: literal('components'), directListVotes: array(listVote),
    candidateOnlyVotes: array(candidateVote), candidateTotals: array(candidateVote), provenance }),
  object({ kind: literal('attributed'), listVotes: array(listVote), candidateTotals: array(candidateVote), provenance })
);
const chamberInput = object({
  chamber,
  regions: array(object({ id: text, autonomous: boolean })),
  territories: array(object({ id: text, regionId: text, kind: literal('ordinary', 'vda', 'taa'), bonusSeats: count })),
  colleges: array(object({ id: text, territoryId: text, kind: literal('ordinary', 'taa-proportional', 'special-uninominal'),
    seatsWithoutBonus: count, seatsWithBonus: count })),
  seatTable: union(
    object({ kind: literal('official-decree', 'constructed'), reference: text, version: text, populationNotes: optional(array(text)) }),
    object({ kind: literal('provisional-census'), reference: text, version: text, censusDate: date,
      geographyVersion: text, method: literal('I08-v1'), populationNotes: optional(array(text)) })
  ),
  lists: array(object({ kind: optional(literal('list', 'other')), id: text, name: text, coalitionId: nullable(text),
    protectedMinority: nullable(object({ regionId: text, legalReference: text })) })),
  coalitions: array(object({ id: text, listIds: array(text) })),
  otherListCoverage: optional(array(object({ collegeId: text, listId: text }))),
  ordinarySlates: array(object({ collegeId: text, listId: text, candidateIds: array(text), preferences })),
  bonusSlates: array(object({ territoryId: text, subject,
    candidates: array(object({ candidateId: text, affiliationListId: text })) })),
  specialCandidatures: array(object({ collegeId: text, candidateId: text, linkedListIds: array(text),
    affiliationListId: nullable(text) })),
  ordinaryVotes: array(object({ collegeId: text, electorate, votes: ordinaryVotes })),
  specialVotes: array(object({ collegeId: text, electorate, votes: specialVotes }))
});
const reference = { share, referenceDate: date };
const resolved = object({ ...reference, origin: literal('supplied', 'model-derived') });

/** Type assignment checks that the runtime schema covers the public input shape. */
export const inputShape: Schema<ReformScrutinyInput> = object({
  schemaVersion: literal(REFORM_INPUT_VERSION), algorithmId: literal(REFORM_ALGORITHM_ID),
  simulationId: positive, electionDate: date, legalVersion: text, geographyVersion: text,
  randomness: object({ seed: text, streamVersion: text }),
  candidates: array(object({ id: text, name: text, birthDate: date, gender: literal('female', 'male'),
    availability: union(object({ kind: literal('available') }), object({ kind: literal('unavailable'), reason: text })),
    externalCandidacies: array(object({ chamber, kind: literal('domestic', 'foreign') })) })),
  renewal: union(object({ kind: literal('joint'), camera: chamberInput, senato: chamberInput }),
    object({ kind: literal('single'), chamber: chamberInput })),
  bonusLinks: array(object({ camera: subject, senato: subject, cameraDeclares: boolean, senatoDeclares: boolean })),
  chamberModel: union(object({ kind: literal('supplied'), reference: text }),
    object({ kind: literal('generated'), modelId: literal('paired-chamber-votes-v1', 'fast-share-paired-v1'), parameterVersion: text,
      covarianceProfileVersion: text, sourceVersions: array(text), seed: text, assumptions: array(text), mode: literal('mean'),
      references: array(object({ scope: union(object({ kind: literal('national') }),
        object({ kind: literal('region', 'province', 'municipality'), code: text })),
        listIds: object({ camera: nullable(text), senato: nullable(text) }), common: nullable(object(reference)),
        camera: nullable(resolved), senato: nullable(resolved) })) })
  )
});

// Reuse the same strict JSON grammar for versioned generation envelopes.
export { scalar, text, boolean, count, positive, share, date, literal, nullable,
  optional, array, object, union, chamber, subject, provenance, preferences,
  ordinaryVotes, specialVotes };
