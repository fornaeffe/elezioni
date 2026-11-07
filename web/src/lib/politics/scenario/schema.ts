/** Authoritative authored-input grammar. Inferred types and parsing share this schema. */
import { array, boolean, count, date, literal, nullable, object, optional, positive,
  scalar, share, text, union, type Infer, type Schema } from '../../core/json-shape';
import { ordinaryVotes, specialVotes, preferences } from '../reform-2026/shape';

export const ROSATELLUM = 'politiche-r-parity-v1';
export const REFORM = 'politiche-reform-2026-v1';
export const chamberShape = literal('camera', 'senato');
export const lawShape = literal(ROSATELLUM, REFORM);
export const generatorShape = literal('M01', 'M02');
export type Chamber = Infer<typeof chamberShape>;
export type LawId = Infer<typeof lawShape>;
export type GeneratorId = Infer<typeof generatorShape>;
export const chambers: readonly Chamber[] = ['camera', 'senato'];
export const laws: readonly LawId[] = [ROSATELLUM, REFORM];
export const nonnegative = scalar<number>(v => typeof v === 'number' && Number.isFinite(v) && v >= 0, 'a finite nonnegative number');
const finite = scalar<number>(v => typeof v === 'number' && Number.isFinite(v), 'a finite number');
const color = scalar<string>(v => typeof v === 'string' && /^#[a-f\d]{6}$/i.test(v), 'a six-digit hex color');
const sha256 = scalar<string>(v => typeof v === 'string' && /^[a-f\d]{64}$/.test(v), 'a lowercase SHA-256');
export function dictionary<T>(schema: Schema<T>): Schema<Readonly<Record<string, T>>> {
  return { check(value, path, issues) {
    if (!value || typeof value !== 'object' || Array.isArray(value) ||
      ![Object.prototype, null].includes(Object.getPrototypeOf(value))) {
      issues.push({ code: 'INVALID_SHAPE', path, message: 'Expected a plain dictionary.', lawReference: null });
      return false;
    }
    return Object.entries(value).map(([k, v]) => text.check(k, path, issues) && schema.check(v, `${path}.${k}`, issues)).every(Boolean);
  } };
}
export const subjectShape = object({ kind: literal('coalition', 'list'), id: text });
export type Subject = Infer<typeof subjectShape>;
export const assetShape = object({ id: text, version: text, schemaVersion: positive, sha256, bytes: positive, uri: text });
export type Asset = Infer<typeof assetShape>;
const lawAssets = object({ geography: assetShape, seats: assetShape, models: object({ M01: array(assetShape), M02: array(assetShape) }) });
export const locationShape = object({ law: lawShape, geographyVersion: text, chamber: chamberShape,
  level: literal('constituency', 'plurinominal', 'uninominal', 'territory', 'college'), code: text });
export type Location = Infer<typeof locationShape>;
const localScope = object({ kind: literal('region', 'province', 'municipality'), namespace: text, code: text });
export const scopeShape = union(object({ kind: literal('national') }), localScope);
export type ReferenceScope = Infer<typeof scopeShape>;
const uncertainty = optional(nullable(object({ sdPercentagePoints: nonnegative,
  origin: literal('explicit', 'default'), policyVersion: nullable(literal('reference-defaults-v1')) })));
const referenceFields = { id: text, scope: scopeShape, channel: literal('common', 'camera', 'senato'), mean: share,
  appliesTo: array(generatorShape), uncertainty };
export const referenceShape = object({ ...referenceFields, denominator: literal('valid-votes'),
  subject: object({ kind: literal('list', 'component'), id: text }) });
export const nonValidReferenceShape = object({ ...referenceFields, denominator: literal('electors') });
export type Reference = Infer<typeof referenceShape>;
export type NonValidReference = Infer<typeof nonValidReferenceShape>;
const placementFields = { id: text, candidateId: text, location: locationShape, owner: subjectShape,
  position: positive, affiliationListId: nullable(text) };
const rosatellumPlacement = object({ ...placementFields, role: literal('uninominal', 'plurinominal'), minority: boolean });
const reformPlacement = object({ ...placementFields, role: literal('ordinary', 'bonus', 'special') });
const lawFields = { schemaVersion: literal(1), filings: array(object({ listId: text, locations: array(locationShape) })),
  externalCandidacies: array(object({ candidateId: text, chamber: chamberShape, kind: literal('domestic', 'foreign') })) };
export const rosatellumShape = object({ ...lawFields, placements: array(rosatellumPlacement),
  candidateGeneration: object({ uninominalToPlurinominalShare: share, plurinominalCandidacyCountShares: array(share) }) });
export const reformShape = object({ ...lawFields, placements: array(reformPlacement),
  candidateGeneration: object({ reuseProbability: share,
    maximumOrdinaryCandidacies: scalar<number>(v => Number.isInteger(v) && Number(v) >= 1 && Number(v) <= 5, 'an integer from 1 to 5'),
    bonusPolicy: literal('reference-proportional-v1') }),
  bonusLinks: array(object({ camera: subjectShape, senato: subjectShape })),
  preferenceProfileId: text, chamberProfileId: text,
  suppliedVotes: array(object({ location: locationShape, votes: union(ordinaryVotes, specialVotes) })),
  suppliedPreferences: array(object({ location: locationShape, listId: text, preferences })) });
export const candidateShape = object({ id: text, name: text, gender: literal('female', 'male'), birthDate: date,
  birthDateOrigin: literal('declared', 'assumed'),
  availability: union(object({ kind: literal('available') }), object({ kind: literal('unavailable'), reason: text })) });
export type Candidate = Infer<typeof candidateShape>;
const component = object({ id: text, name: text, provenance: text, support: text,
  fallback: object({ M01: object({ policy: literal('regional-or-equal-list-v1'), provenance: text }),
    M02: object({ geographicLogProfile: finite, profileSd: nonnegative, localInnovationSd: nonnegative,
      nationalInnovationSd: nonnegative, provenance: text }) }) });
export const pollShape = object({
  id: text, surveyId: text, context: literal('camera', 'european', 'senato'), date, fieldworkStart: date,
  fieldworkEnd: date, availableDate: date, citation: text, sampleSize: positive, questionSampleSize: nullable(positive),
  decidedFraction: share, effectiveSample: nonnegative, designEffect: nonnegative, effectiveSamplePolicy: text,
  errorGroup: text, roundingWidth: nonnegative, original: array(object({ label: text, percentage: nonnegative, kind: text })),
  normalized: array(object({ label: text, members: array(text), share, other: boolean })),
  otherDefinition: literal('offered-complement'), status: literal('supported', 'excluded'), reasons: array(text), method: text });
const selection = object({ id: generatorShape, version: literal('2.0.0') });
const policies = object({ numerical: literal('fast-share-float64-v1'), references: literal('fixed-turnout-reference-v1'),
  chambers: literal('fast-share-paired-v1'), rng: literal('fast-share-forecast-streams-v1'),
  fittingRng: literal('fast-share-fit-streams-v1') });
export const scenarioShape = object({
  kind: literal('politics-scenario'), schemaVersion: literal(1), id: text, name: text, electionDate: date, referenceDate: date,
  renewal: literal('joint', 'camera', 'senato'),
  selection: object({ scrutiny: object({ id: lawShape, version: literal('1.0.0') }),
    generatorsByLaw: object({ [ROSATELLUM]: selection, [REFORM]: selection }) }),
  data: object({ historical: assetShape, catalogue: assetShape, referenceGeography: assetShape, electorate: assetShape,
    lawAssets: object({ [ROSATELLUM]: lawAssets, [REFORM]: lawAssets }) }),
  history: object({ electionIds: array(text), pairedCamera: nullable(text), pairedSenato: nullable(text),
    polls: object({ selected: array(text), added: array(pollShape), excluded: dictionary(text) }),
    sourceMemberships: array(object({ sourceId: text, questionId: text, choiceId: text, members: array(text), provenance: text })),
    pooling: array(object({ id: text, members: array(text), provenance: text })) }),
  subjects: object({ coalitions: array(object({ id: text, name: text, color, chambers: array(chamberShape) })),
    components: array(object({ id: text, name: text, support: literal('fitted', 'reference') })),
    lists: array(object({ id: text, name: text, color, kind: literal('list', 'other'), coalitionId: nullable(text),
      minority: nullable(object({ regionId: text, namespace: text, legalReference: nullable(text) })) })) }),
  memberships: array(object({ listId: text, camera: array(text), senato: array(text), locked: boolean,
    newComponentId: text })),
  newComponents: array(component), references: array(referenceShape), nonValidReferences: array(nonValidReferenceShape),
  candidates: array(candidateShape), lawSettings: object({ [ROSATELLUM]: rosatellumShape, [REFORM]: reformShape }),
  modelSettings: object({
    M01: object({ schemaVersion: literal(1), probabilityLawVersion: literal('2.0.0'), policies,
      parameterDate: date, compositions: array(object({ sourceId: text, questionId: text, choiceId: text,
        fractions: dictionary(nonnegative), provenance: text, proxy: boolean })),
      supplementalPolls: array(object({ id: text, date, available: date, end: date, context: literal('camera', 'european', 'senato'),
        groups: array(object({ label: text, members: array(text), share })), sample: nonnegative, source: text, enabled: boolean })) }),
    M02: object({ schemaVersion: literal(1), probabilityLawVersion: literal('2.0.0'), policies,
      statistical: literal('fast-share-kind-common-conservative-v1'),
      forecast: literal('filing-strata-beta-v1', 'bounded-pair-field-v1', 'bounded-mass-pair-field-v2'),
      forecastScale: nonnegative, profileRegionCorrelation: share, forecastReference: literal('conditional', 'country-calibrated'),
      uncertaintyMultipliers: array(object({ listId: text, chamber: chamberShape,
        national: nullable(nonnegative), local: nullable(nonnegative) })) }) }),
  execution: object({ simulations: positive, fittingSeed: text, simulationSeed: text,
    workers: union(object({ kind: literal('auto'), policy: literal('model-aware-v1') }), object({ kind: literal('fixed'), count: positive })) }),
  provenance: object({ origin: nullable(text), sourceHashes: array(sha256), assumptions: array(text), warnings: array(text),
    referenceDefaultPolicy: literal('reference-defaults-v1') })
});
export type PoliticsScenario = Infer<typeof scenarioShape>;
export type ExplicitBonusLink = PoliticsScenario['lawSettings'][typeof REFORM]['bonusLinks'][number];
export type Placement = PoliticsScenario['lawSettings'][LawId]['placements'][number];
