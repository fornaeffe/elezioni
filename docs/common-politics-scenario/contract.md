# CP-02 — Shared politics domain contract

Implemented under `web/src/lib/politics/scenario/`; import its public API from
`index.ts`. This domain has no Svelte state, workers, storage ownership or old-file
conversion. The [common executor and compute worker](execution.md) consume it;
editor and session integration remain CP-06/07. Use the
[guidelines](guidelines.md) for approved behavior and the
[inventory](inventory.md) for the preserved old-input meanings.

| Task | Module |
| --- | --- |
| Authored JSON fields and inferred readonly types | `schema.ts` |
| Independent generators, scrutinies, capabilities, target pairs | `registry.ts` |
| Whole-document invariants and selected-domain preflight | `validation.ts` |
| Effective filings, membership fallback, candidate pins, D11 links | `effective.ts` |
| Atomic edits, creation, deletion and explicit replacement | `edits.ts` |
| SD conventions and existing M02 family feasibility | `references.ts` |
| Exact decoded asset dependencies/checksums | `dependencies.ts` |
| Scenario-only JSON and deterministic gzip | `serialization.ts` |
| Separately versioned result/archive design | `envelopes.ts` |
| Constructed examples and executable checks | `scenario.fixture.ts`, `scenario.test.ts` |

## Format and versions

`PoliticsScenario` is inferred from the strict `scenarioShape`: one grammar owns
both the public type and runtime parsing. The shared grammar primitives live in
`core/json-shape.ts`; the existing reform grammar reexports them with unchanged
runtime behavior. Unknown fields, unsupported schema/block/policy versions,
missing blocks/selections, unsupported pairs and old politics formats fail.
Import never fills creation defaults, migrates inputs or substitutes a model.

The scenario is `kind: politics-scenario`, `schemaVersion: 1`. Both law blocks and
both model blocks have independent `schemaVersion: 1`. Each law stores its exact
generator selection; only `selection.scrutiny` selects the active law. The first
explicit scrutiny metadata version is `1.0.0` for each existing algorithm ID;
this labels the unchanged scrutiny implementation, not a legal correction.
M01/M02 retain exact `2.0.0` probability-law identities here. CP-03's
[version assessment](execution.md#model-version-assessment-and-comparison)
retains M01 2.0.0 for both laws, independently of the new adapter identity.

The supported domain pairs are Rosatellum/M01, reform/M01 and reform/M02.
`initialSelection()` creates reform/M02 with saved Rosatellum/M01; switching
uses saved choices. It does not construct CP-04's authored production default.
`envelopes.ts` reserves independent result and archive v1 headers, immutable
executed scenario, actual execution provenance and portable-fit ownership.
Its generic payload interfaces are design contracts, not archive parsers;
strict payloads, streaming integration and checkpoint matching remain CP-05/07.

## Ownership and encoding

- `subjects` owns coalition/list identities and presentation, shared affiliation
  and minority declarations. Its component entries are historical/pooled label
  overrides only; no historical component definitions or observations are copied.
- `memberships` stores binary Camera/Senato sets, lock metadata and an owned
  `newComponentId`. The owned fallback is used only where historical membership
  is empty; adding history retains its assumptions without activating both.
  Historical memberships, M01 fractional compositions and whole-component pooling
  remain distinct. Conflicts are chamber-scoped, including inactive declarations.
- `history` retains selected/paired sources, all user poll metadata, exclusions
  with reasons, source/question/choice memberships and pooling provenance. M01's
  automatic historical-selection policy and partial observations stay separate
  from M02's selected history. Model/source feasibility is also checked by the
  existing model preparation boundary when CP-03 integrates it.
- References use fractions of valid votes; non-valid references use fractions
  of electors. SDs are percentage points; multipliers are dimensionless. A missing
  row means no override; mean/SD/count zero is explicit. `uncertainty` may be
  absent or null (unspecified), or a concrete SD with explicit/default origin.
  Default SDs carry `reference-defaults-v1`; explicit SDs have null policy version.
  A null M02 multiplier inherits 1, as does an absent multiplier row. Empty arrays
  are explicit empty declarations. No parser restores defaults over these values.
- Each location carries law ID, exact geography version, chamber, level and code.
  Rosatellum list filings use plurinominal colleges, with actual uninominal-only
  locations for VdA and Senate TAA; the Rosatellum adapter checks those exceptions.
  Reform uses physical colleges.
  Node/adapter catalogues supply parent-child geography relationships for effective
  uninominal/bonus pins. Equal codes alone never establish a relationship. Existing
  constituency-wide Rosatellum defaults can be expanded to their actual colleges
  in CP-04; law switching performs no conversion.
- Candidate identities own biography and availability. Law blocks own ordered
  pins and external candidacies. New identities require selected gender and resolve
  a blank birth date once to January 1 of election year minus 50, labelled assumed.
  Same names never merge. Deletion removes all law/chamber pins and external
  declarations; removing one pin retains identities and other slot numbers.
- Reform supplied votes/preferences preserve their legal discriminated unions,
  provenance, validity certificates, missing reasons and zero counts. Candidate
  keys in supplied evidence can address generated candidates; resolve them against
  the assembled ballot, rather than embedding generated biographies in the draft.
  Removing a referenced identity/subject drops affected supplied declarations
  atomically, preserving unrelated evidence; it does not fabricate replacement zeros.
- `execution` stores separate concrete fitting/simulation seeds, a positive safe
  simulation count and explicit auto/fixed worker preference. Actual workers,
  timings, generated objects, fits and worker/storage handles belong to run records.

## Validation and edit boundaries

`parsePoliticsScenario` validates the full document and returns a detached copy.
Without `DomainContext` it can validate structure and internal relationships;
external catalogue IDs require the pinned context. Context supplies historical
component/source/poll identities, reference geography, law locations/relationships
and exact available asset descriptors. `validateScenario` checks inactive structure
and relationships too, but does not demand that inactive inputs are executable.

`validateActiveScenario` adds selected-pair assets, selected historical requirements,
M01 normalized compositions, M02 persisted SDs, Rosatellum candidate probabilities,
active candidate biography/availability/incompatibilities and separate counterpart
participation checks. It is a **domain preflight**, not authorization to bypass
the existing model, candidature or scrutiny validators. CP-03 must invoke those
validators after resolving the selected model's filed sets/reference precedence
and assembling the actual ballot. `resolveReferenceFamily` reuses M02's existing
feasibility engine on an already-resolved, disjoint family, proposes complementary
defaults without changing explicit SDs and rejects infeasible moments. No universal
reference precedence or new probability law is introduced here.

Edits are pure and atomic: validate a detached next value before returning it.
Conflicting Camera edits/relocks fail without changing either set or moving
another list's components. Creation initializes every law's filings from its
eligible catalogue; later empty/restricted declarations remain saved. Explicit
uncertainty initialization returns changed IDs for a visible review; loading and
selection do not silently repair missing SDs. Call the family helper before
accepting joint default proposals where complementary groups need matching SDs.

## D11 resolver and dependencies

Use `resolveBonusLinks` for UI status, validation and adapter decisions. Persist
only typed Camera–Senato pairs. The resolver derives automatic relationships from
coalition scope or autonomous-list filings, retains ineligible declarations with
reasons, and reactivates their original endpoints. An empty coalition counterpart
does not change scope/automatic linking; participation is a separate preflight
error. Endpoint uniqueness includes inactive pairs and ignores JSON property order.
`replaceBonusLink` requires eligible endpoints, removes all pairs using either
endpoint and returns the replaced declarations for review. Subject deletion removes
pairs; names never restore them. `materializeBonusLinks` emits only automatic and
eligible explicit relationships, and emits none for a single-chamber renewal.

Each asset pin includes ID, exact version/schema, decoded byte count, SHA-256 and
retrieval URI. Conflicting pins anywhere in the document fail. `requiredAssets`
selects shared history/catalogue/reference-geography/electorate and the selected
law's geography, seats and selected model assets. Inactive assets remain saved
but need not be loaded. `resolveDependencies` requires a unique exact match;
`verifyAssetBytes` checks decoded bytes before use. There is no latest/debug fallback.
`dependencyFingerprint` fingerprints these exact dependencies only; it is not a
fit-cache key. The [executor](execution.md) adds each stage's consumed inputs to
separate preparation/generation/assembly identities, keeping names/colors and
inactive edits out of unrelated statistical dependencies. CP-07 owns cache/session
lifecycle integration.
