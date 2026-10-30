/** Strict v4 parser. No conversion to factor correspondences or older scenarios. */
import { reformLegalFields } from "../generation-scenario";
import {
  object,
  array,
  optional,
  literal,
  text,
  date,
  nullable,
  share,
  scalar,
  union,
  chamber,
  type Schema,
  type ValidationIssue,
} from "../shape";
import { otherListErrors } from "../other-list";
import { OTHER_LIST_COLOR } from "../../../core/other-list";
import { validateEditedPoll } from "./source-compiler";
import { FAST_SHARE_MODEL, FAST_SHARE_NUMERICS } from "./types";
import { REFERENCE_POLICY } from "./reference-family";
import { FORECAST_STREAM_VERSION } from "./forecast-random";
import type { ReleasedScenario } from "./release";
import type { SourcePoll } from "./source-types";
const nonnegative = scalar<number>(
  (v) => typeof v === "number" && Number.isFinite(v) && v >= 0,
  "a finite nonnegative number",
);
const finite = scalar<number>(
  (v) => typeof v === "number" && Number.isFinite(v),
  "a finite number",
);
const bool = scalar<boolean>((v) => typeof v === "boolean", "a boolean");
const integer = scalar<number>(
  (v) => Number.isSafeInteger(v) && Number(v) > 0,
  "a positive integer",
);
const color = scalar<string>(
  (v) => typeof v === "string" && /^#[a-f0-9]{6}$/i.test(v),
  "a hex color",
);
const scope = union(
  object({ kind: literal("national") }),
  object({ kind: literal("region", "province", "municipality"), code: text }),
);
export function dictionary<T>(schema: Schema<T>): Schema<Record<string, T>> {
  return {
    check(v, p, issues) {
      if (v === null || typeof v !== "object" || Array.isArray(v)) {
        issues.push({
          code: "INVALID_SHAPE",
          path: p,
          message: "Expected a dictionary",
          lawReference: null,
        });
        return false;
      }
      return Object.entries(v)
        .map(([key, value]) => schema.check(value, p + "." + key, issues))
        .every(Boolean);
    },
  };
}
export const pollShape = object({
  id: text,
  surveyId: text,
  context: literal("camera", "european", "senato"),
  date,
  fieldworkStart: date,
  fieldworkEnd: date,
  availableDate: date,
  citation: text,
  sampleSize: integer,
  questionSampleSize: nullable(integer),
  decidedFraction: share,
  effectiveSample: nonnegative,
  designEffect: nonnegative,
  effectiveSamplePolicy: text,
  errorGroup: text,
  roundingWidth: nonnegative,
  original: array(object({ label: text, percentage: nonnegative, kind: text })),
  normalized: array(
    object({ label: text, members: array(text), share, other: bool }),
  ),
  otherDefinition: literal("offered-complement"),
  status: literal("supported", "excluded"),
  reasons: array(text),
  method: text,
});
const shape = object({
  ...reformLegalFields,
  schemaVersion: literal(4),
  referenceDate: date,
  fitSeed: text,
  models: object({
    statistical: literal(FAST_SHARE_MODEL),
    numerical: literal(FAST_SHARE_NUMERICS),
    references: literal(REFERENCE_POLICY),
    chambers: literal("fast-share-paired-v1"),
    rng: literal(FORECAST_STREAM_VERSION),
    generationImplementationId: text,
    forecast: optional(
      literal(
        "filing-strata-beta-v1",
        "bounded-pair-field-v1",
        "bounded-mass-pair-field-v2",
      ),
    ),
    forecastScale: optional(nonnegative),
    profileRegionCorrelation: optional(share),
    forecastReference: optional(literal("conditional", "country-calibrated")),
  }),
  data: object({
    sourceBundleId: text,
    fittedStateId: text,
    profileStateId: text,
    geographyVersion: text,
    seatTableVersion: text,
    preferenceModelVersion: text,
  }),
  components: array(
    object({
      id: text,
      kind: literal("fitted", "new"),
      provenance: text,
      fallback: optional(
        object({
          geographicLogProfile: finite,
          profileSd: nonnegative,
          localInnovationSd: nonnegative,
          nationalInnovationSd: nonnegative,
        }),
      ),
    }),
  ),
  lists: object(
    Object.fromEntries(
      ["camera", "senato"].map((ch) => [
        ch,
        array(
          object({
            kind: optional(literal("list", "other")),
            id: text,
            name: text,
            coalitionId: nullable(text),
            protectedMinority: nullable(
              object({ regionId: text, legalReference: text }),
            ),
            componentIds: array(text),
            nationalUncertaintyMultiplier: optional(nonnegative),
            localUncertaintyMultiplier: optional(nonnegative),
          }),
        ),
      ]),
    ),
  ),
  references: array(
    object({
      id: text,
      scope,
      channel: literal("common", "camera", "senato"),
      subject: object({ kind: literal("list", "component"), id: text }),
      mean: share,
      sdPercentagePoints: nonnegative,
    }),
  ),
  nonValidReferences: array(
    object({
      id: text,
      scope,
      channel: literal("common", "camera", "senato"),
      mean: share,
      sdPercentagePoints: nonnegative,
    }),
  ),
  historySelection: object({
    electionIds: array(text),
    pairedCamera: text,
    pairedSenato: text,
  }),
  sourceMemberships: dictionary(
    object({ members: array(text), provenance: text }),
  ),
  componentMap: dictionary(text),
  polls: object({
    selected: array(text),
    added: array(pollShape),
    excluded: dictionary(text),
  }),
  presentation: object({
    name: optional(text),
    coalitions: optional(
      array(
        object({
          id: text,
          name: text,
          color,
          chambers: optional(array(chamber)),
        }),
      ),
    ),
    lists: array(
      object({ chamber, id: text, name: text, color: optional(color) }),
    ),
    components: array(
      object({ id: text, name: text, support: literal("fitted", "reference") }),
    ),
  }),
});
export function validateFastScenario(value: unknown): string[] {
  const issues: ValidationIssue[] = [];
  if (!shape.check(value, "$", issues))
    return issues.map((i) => i.path + ": " + i.message);
  const s = value as ReleasedScenario,
    errors: string[] = [],
    ids = new Set(s.components.map((c) => c.id));
  const unique = (rows: unknown[], label: string) => {
    if (new Set(rows.map((x) => JSON.stringify(x))).size !== rows.length)
      errors.push("Identità duplicata: " + label);
  };
  unique(
    s.components.map((c) => c.id),
    "componenti",
  );
  if (!ids.size || ids.size > 128)
    errors.push("Servono da 1 a 128 componenti.");
  if (s.referenceDate > s.electionDate)
    errors.push("La data di riferimento segue la data elettorale.");
  unique(s.polls.selected, "sondaggi");
  unique(
    s.polls.added.map((p) => p.id),
    "sondaggi manuali",
  );
  unique(s.historySelection.electionIds, "elezioni");
  for (const p of s.polls.added)
    try {
      validateEditedPoll(p as SourcePoll);
    } catch {
      errors.push("INVALID_USER_POLL: Sondaggio non supportato: " + p.id);
    }
  for (const ch of ["camera", "senato"] as const) {
    unique(
      s.lists[ch].map((l) => l.id),
      "liste " + ch,
    );
    errors.push(...otherListErrors(s.lists[ch]));
    for (const l of s.lists[ch]) {
      unique(l.componentIds, "componenti lista " + l.name);
      if (!l.componentIds.length || l.componentIds.some((id) => !ids.has(id)))
        errors.push("Composizione non valida: " + l.name);
      if (
        l.kind === "other" &&
        (s.candidateTemplates.some(
          (t) =>
            t.chamber === ch &&
            (t.ownerId === l.id || t.affiliationListId === l.id),
        ) ||
          s.suppliedPreferences.some(
            (p) => p.chamber === ch && p.listId === l.id,
          ))
      )
        errors.push("Altre liste non ammette candidature o preferenze.");
    }
    for (const c of s.coverage.filter((c) => c.chamber === ch)) {
      unique(c.listIds, "depositi");
      if (c.listIds.some((id) => !s.lists[ch].some((l) => l.id === id)))
        errors.push("Deposito con lista assente: " + c.collegeId);
    }
    for (const l of s.lists[ch].filter((l) => l.kind === "other"))
      if (
        s.presentation.lists.find((p) => p.chamber === ch && p.id === l.id)
          ?.color !== OTHER_LIST_COLOR
      )
        errors.push("Altre liste richiede il colore grigio.");
  }
  unique(
    s.references.map((r) => r.id),
    "riferimenti",
  );
  unique(
    s.nonValidReferences.map((r) => r.id),
    "riferimenti non validi",
  );
  unique(
    s.presentation.lists.map((l) => [l.chamber, l.id]),
    "etichette liste",
  );
  for (const r of s.references)
    if (
      r.subject.kind === "component"
        ? !ids.has(r.subject.id)
        : !["camera", "senato"].some((ch) =>
            s.lists[ch as "camera" | "senato"].some(
              (l) => l.id === r.subject.id,
            ),
          )
    )
      errors.push("Riferimento senza soggetto: " + r.id);
  for (const c of s.components.filter((c) => c.kind === "new"))
    if (!c.fallback)
      errors.push("Componente da riferimento senza ipotesi: " + c.id);
  return errors;
}
export function requireFastScenario(value: unknown): ReleasedScenario {
  const errors = validateFastScenario(value);
  if (errors.length) throw new Error(errors.join("\n"));
  return structuredClone(value) as ReleasedScenario;
}
export function parseFastScenario(text: string) {
  return requireFastScenario(JSON.parse(text));
}
