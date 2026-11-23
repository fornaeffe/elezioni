## 2026-11-07 — Common politics generator and session decisions

Updated the [workspace guidelines](../../common-politics-scenario/guidelines.md) with
approved D01/D02/D08/D17: reusable generator identities with validated law
adapters; replace M00 with M01 for Rosatellum; retain reform + M01/M02; select
M01 on switching to Rosatellum and M02 on switching to reform. Historical M01
compositions remain distinct from binary future memberships. Exact versions and
cross-law version-impact assessment remain explicit. M00 retirement avoids a
third maintained model and intentionally replaces its statistical semantics;
Rosatellum candidature policies, legal scrutiny and frozen evidence are preserved.

Specified common durable bounded execution, multiworker determinism and result
sessions independent of draft selection. Remove the Rosatellum-specific
1,000-run ceiling in the target design, while acceptance campaigns remain at
1,000 simulations per approved pair: Luca explicitly excluded larger runs.
Updated dependent implementation steps and the page draft's comments for these
rules. The merger remains unimplemented and its migration item stays open.

Verification: decision-register consistency and the acceptance-size restriction
passed; all 15 local guideline links/anchors resolved; the Svelte draft parsed,
and comparison with HEAD proved its markup unchanged after removing comments
and whitespace. `git diff --check` passed. Documentation only; no runtime tests,
resource campaigns or model/legal implementation changes were performed.

### Follow-up — Remember generator selection per law

Revised D02, the common scenario contract, authored-default instructions,
implementation checks and page-draft comments: store an exact generator/version
selection for each law. New scenarios initialize Rosatellum to M01 and reform
to M02. Model changes update only the active law's choice; law changes restore
the destination law's saved choice, superseding the unconditional switching
defaults above. Persist both choices through reload and scenario JSON/gzip
roundtrips, deriving the active generator instead of duplicating it. Missing or
unsupported choices remain validation errors. Acceptance still excludes runs
above 1,000; session-selection rules are unchanged.

Verification: per-law selection contract, removal of the prior switching rule,
unique decision IDs and unchanged acceptance restriction passed. The page draft
parsed and its changes were comments only; `git diff --check` passed. No runtime
implementation or acceptance campaigns were performed.

### Follow-up — Shared list declarations and linked chamber memberships

Recorded approved D05: shared coalition/list names, list coalition affiliation
and minority declaration; distinct lists represent differences between chambers.
List filings declare chamber-only participation, without a separate list-presence
toggle. Explicit reform bonus links are available in advanced coalition and
autonomous-list options for subjects filed in one chamber only, with eligible
coalition or autonomous-list counterparts. D11's automatic-link serialization
and precedence choices remain open.

Expanded D08 and the annotated page draft with separate Camera/Senato future
membership lists, side by side when space permits. Each future list has a
default-locked chain control: Camera edits both sets; unlocking preserves them
and allows independent edits; re-locking replaces Senato with Camera, including
removals and empty sets. Specified atomic conflict validation, saved lock state,
inactive-data retention and per-chamber fallback activation. Historical-source
editing and M01 source fractions remain separate from binary future memberships.

Verification: the Svelte autofixer reported no issues or suggestions; the draft
parsed, guideline links/anchors and decision-register consistency passed, and
`git diff --check` passed. Documentation and annotated draft only; no production
UI, generator, scrutiny or data changes, and no runtime acceptance campaign.
The workspace-merger migration item remains open.

### Follow-up — Coalition link eligibility follows declared chamber scope

Corrected the guidelines and page draft to distinguish a coalition's declared
scope from effective member-list participation. A coalition declared in both
chambers automatically links its two counterparts, even if one side has no
filed members; it cannot declare another link or be selected as another
subject's explicit counterpart. Only a coalition declared in one chamber can
use the corresponding explicit-link controls. Member-list effective filings
respect that scope; removing the last filed member never changes link eligibility.
Empty counterparts are handled separately during execution validation/assembly,
without changing the declared relationship or substituting another counterpart.

Updated both coalition and autonomous-list counterpart selectors and narrowed
D11 to remaining serialization/retained-declaration handling. Verification:
Svelte autofixer, draft parsing, guideline links/anchors, decision-register
consistency and `git diff --check` passed. Changes are documentation and draft
comments only; no runtime implementation or acceptance campaign.

### Follow-up — D06 references and D09 candidate declarations approved (2026-11-07)

Recorded Luca's approval of D06's reference representation, uncertainty defaults,
feasibility and persistence policy, and D09's candidate identity rules in
`common-politics-scenario/guidelines.md`. Declared identities, biography and
availability are shared; placements and external candidacies are law-specific.
Reuse is explicit, edits propagate, identity deletion removes all dependent
candidate declarations, and removing one placement retains the identity and
other pins. Required name/gender and a persisted, labelled assumed birth date
(January 1 of election year minus 50) replace the earlier incomplete-identity
recommendation. Generated-candidate policies and immutable results are preserved.

Updated the contract ownership table, approved/open register and dependent CP-02
and CP-06 instructions, removing obsolete D06/D09 approval gates. Verification:
reviewed the documentation diff, checked decision-register consistency and ran
`git diff --check`. Documentation only; no runtime implementation or acceptance
campaign. D11/D14 and the workspace-merger migration item remain open.

### Follow-up — D11 bonus-link persistence approved (2026-11-07)

Recorded Luca's approval in the common-scenario guidelines and annotated page
draft. Automatic links are derived; explicit reciprocal Camera–Senato pairs are
saved once with typed stable-ID endpoints. Scope, filing and affiliation edits
retain ineligible pairs as inactive, with automatic reactivation when eligibility
returns and no rebinding or chamber reversal. Each chamber endpoint has at most
one saved pair, including inactive declarations; explicit replacement removes
affected pairs and subject deletion removes its pairs. Advanced options retain
counterpart/status/reason and remove/replace controls, with reactivation and
replacement disclosures. Strict imports reject malformed or conflicting pairs.
One resolver serves validation, UI and assembly; adapters emit only effective
links and no joint links for single-chamber renewal. Empty coalition counterparts
retain automatic relationships and receive separate participation validation.

Moved D11 to the approved register and updated CP-02/CP-03/CP-06 checks. D14 is
the only remaining open decision. Verification: Svelte autofixer reported no
issues or suggestions, the draft parsed, non-comment markup was unchanged apart
from blank lines, decision IDs were unique and register checks passed.
The initial comment-stripping comparison retained the new comment's blank line;
excluding blank-only lines resolved that verification mismatch. `git diff --check`
passed. Documentation and draft comments only; no runtime implementation or
acceptance campaign. The workspace-merger migration item remains open.

### Follow-up — D14 editor scope approved (2026-11-07)

Recorded Luca's D14 decisions in the common-scenario guidelines and annotated
page draft. Keep historical membership/M01 composition controls, fallback and
component-reference editors. Add a basic optional “Non valido / astensione”
reference immediately below lists, visible by default, with attached collapsed
national uncertainty, chamber references and common/chamber local-reference
trees. Show electorate data under advanced settings without editing it.
Preserve inactive values and model-specific meanings, including settings that
have no UI editor during migration.

Historical-source/poll editors, model-specific historical selection,
supplied-vote/preference editors and model/profile editors (including M01
partial observations) are deferred to the end of the post-migration plan.
Their UI editability is explicitly excluded from CP-06/CP-08 acceptance;
contract validation, execution and roundtrip preservation remain required.
Updated CP-01/CP-06 and the decision register; no decisions remain open there,
but the merger itself remains unimplemented and its migration item stays open.

Pooling is retained in CP-06 because the current default uses it. Traced the
active policy through `fast-share-default.ts`, `fast-share-future-default.ts`
and `fast-share-release.ts`; decoded the local candidate catalog/scenario and
confirmed 154 component-map entries, including 126 original IDs mapped to
`historical-rest`. This is whole-component pooling, not merely an alias.
CP-01 must recheck the authored default before implementation and defer the
editor only if pooling is no longer used. No data was regenerated or modified.

Verification: all 40 local links/anchors in the guidelines and migration plan
resolved; the register has 18 unique approved decision IDs, with D13 superseded;
obsolete D14 gates are absent and deferred tasks follow migration completion.
The Svelte autofixer reported no issues or suggestions, the annotated draft
parsed, and its non-comment markup matched HEAD after whitespace normalization.
`git diff --check` passed. Documentation and draft comments only; no runtime
implementation or acceptance campaign.

### CP-01 — Inventory and preserve behavior (2026-11-07)

Completed the first common-politics implementation step against source revision
`a700d19b04272f39fad17ddd3ceb9e48048ea623`. Added the task-oriented
[`inventory`](../../common-politics-scenario/inventory.md),
[`baseline guide`](../../common-politics-scenario/baselines.md) and
[`observed default/asset report`](../../common-politics-scenario/default-inventory.json).
The mapping covers all original `Scenario` and released v5 inherited fields,
candidate identity/placements/external declarations, per-chamber values, supplied
votes/preferences and validity evidence, source/poll metadata, model settings,
runtime/checkpoint/session ownership, exact dependencies and intentional M00
retirement. It separates retained inputs, CP-06 editors and post-migration D14
editors. No production schema, UI, generation, scrutiny or defaults changed.

Rechecked the authored-choice/policy/compiler chain and the checksummed local
release scenario: 154 component-map entries, 126 to `historical-rest`, 34
components, 53 M01 source compositions, two partial named observations, 17
political references and four non-valid references. Pooling remains required in
CP-06. The read-only `common-politics-inventory.ts` collector records both
compiled defaults, source/manifest hashes, release pointer/asset pins and exact
filing/reference choices. It verifies compressed and decoded catalogue/scenario
descriptors; it neither prepares assets nor supplies runtime defaults.

Added a separate constructed M01/M02 2.0.0 generation/candidature corpus and
replay test in `web/scripts/acceptance/common-politics-baseline{,.test}.ts`, with
tracked output in `web/tests/fixtures/common-politics-baseline.json`. Fourteen
cases cover joint and single renewals, distinct chamber references, territorial
filing, manual identity pins, residual and new-component support, VdA/TAA, and
trace diagnostics. Thirteen successful cases record 43 draws, national and
college-level values, first-list sample moments, separate ballot/candidature
hashes and declared biography. The fixed input recipe, source revision and
input hashes permit replay; these small samples do not establish predictive
acceptance. Exact old draws/IDs remain supporting evidence, not an architectural
constraint. Rosatellum+M01 remains CP-03 work, with retained M01 statistical
evidence and separate existing Rosatellum legal/candidature regressions.

The reduced M01 restricted-filing case exposes the existing preparation error
`Reference votes conflict with filing/capacity camera/990001/A`; M02 completes
the equivalent case. Recorded the error verbatim rather than changing validation,
rerolling or claiming successful execution. During corpus construction, an
individual-college restriction failed the legal two-thirds coverage rule; the
final case removes the list from a whole territory. The residual case uses the
reserved identity/grey color and distinct component membership. These were test
input corrections, not application fixes. The frozen Rosatellum Camera flipper
584 input and all R/legal expectation/checksum manifests remain unchanged.

Verification on Windows/PowerShell, Node v24.20.0, from `web/`:

- Both read-only collectors completed; default catalogue/scenario checksums
  verified. The final report also pins the four preparation source manifests.
- `npm run check:data` passed; `npm run check` reported zero errors/warnings.
- `npm test -- --maxWorkers=2`: 106 files, 1,224 tests passed, no skips, 49.14 s;
  includes frozen R/checksum/legal failures, model-policy and portable-session
  tests. After adding college-level output, the focused baseline replay passed
  again (one test, 6.02 s including startup).
- `npm run test:e2e -- workspace-uniformity.spec.ts reform-production-worker.spec.ts --workers=2`:
  production build followed sequentially by nine passing Chromium tests at `/`,
  53.6 s total, no retries/skips. Covered current draft/reset/import/navigation,
  corrupt dependencies, cancellation/closed workers, 50-run boundaries,
  1/2/4-worker exported rows/diagnostics, worker crashes and quota failures with
  durable committed prefixes. Existing build timing/chunk-size and color-env
  warnings were nonblocking.
- Local documentation links/anchors, corpus counts and `git diff --check` passed.

Added CP-01–08 to the active plan and checked only CP-01. CP-02–08, the parent
workspace merger and final deployment gate remain open. No project-base release
campaign, 1,000-run campaign, schema migration, commit or publication is claimed.

### CP-02 — Establish the shared domain contract (2026-11-07)

Added the independent common-politics domain under
`web/src/lib/politics/scenario/`, with a task-oriented
[contract guide](../../common-politics-scenario/contract.md). `scenarioShape` is the
authoritative strict v1 runtime grammar and supplies the inferred readonly
`PoliticsScenario` type. Independent law/model blocks are versioned; generator
and scrutiny registries expose the three approved target pairs, capabilities,
exact versions and per-law saved selections. Initial selection metadata is
reform/M02 with Rosatellum/M01 saved. The first explicit scrutiny metadata
version is `1.0.0` for the existing algorithm IDs, without a legal change.
M01/M02 remain exactly `2.0.0` at this domain boundary; CP-03 must assess and
apply any probability-law version impact before cross-law execution is enabled.

The contract retains shared stable subjects, component presentation overrides,
chamber memberships/lock metadata, owned fallback assumptions, historical
source/poll selection and provenance, pooling, M01 compositions/partial polls,
M02 methods/profiles, references with explicit units, shared candidate biography,
per-law pins/external candidacies, both candidate-generation parameterizations,
supplied legal evidence and requested seeds/workers. It excludes generated
objects, fits and execution/storage ownership. Unknown/old formats, extra fields,
missing selections/blocks and unsupported versions/pairs fail without conversion
or replacement defaults. Scenario-only JSON/gzip roundtrips retain inactive
declarations and distinguish absent/null uncertainty, empty declarations and
explicit zero support/SD/counts. Result/archive headers and executed provenance
have separate design interfaces; strict payloads and lifecycle integration remain
later steps, not accepted import formats at CP-02.

Added detached, atomic edit operations for selections, renewal, subjects,
filings, memberships/locking, shared identities, ordered pins and links.
Locked Camera edits update both sets; independent edits and re-locking validate
both chambers before returning. New subjects initialize every law; new lists
receive explicit eligible filings and owned fallback identities. Shared candidate
creation requires selected gender and persists the labelled assumed birth date
once; identity deletion removes inactive-law pins/external declarations, while
individual pin removal preserves other ordered slots. A failed edit never moves
another list's members or partially mutates the original draft.

One D11 resolver derives automatic/active/retained links and reasons. Coalition
scope remains independent of member participation; affiliation/scope/filing edits
retain original explicit endpoints and automatically reactivate eligible pairs.
Endpoint uniqueness includes inactive pairs and is independent of JSON property
order. Explicit replacement reports every removed pair; subject deletion removes
pairs without name-based restoration. Materialization excludes retained competitors
and emits no joint links for single-chamber renewal. Empty counterparts retain
their declared relationship and receive separate participation validation.

Whole-document validation checks structure, internal IDs and supplied pinned
catalogue relationships across active and inactive blocks. Selected-domain
preflight adds required exact assets, active settings/history and candidate
biography/availability/incompatibilities. Law/chamber/location namespaces and
explicit catalogue hierarchy prevent equal-looking college codes or unrelated
territories from activating candidate pins. Rosatellum filings are explicit
plurinominal colleges; later default preparation expands old constituency scopes.
Incomplete inactive semantics can remain saved, then fail when selected.
Existing model/candidature/legal validators still belong at CP-03's preparation
and assembly boundary. In particular, supplied candidate keys can refer to
runtime-generated candidates and must be resolved against the assembled ballot;
the scenario must not invent their biographies just to satisfy import validation.

Reference helpers implement D06's scoped capped SD defaults, origin/policy
metadata, explicit reviewable initialization and multiplier inheritance. The
family helper calls the existing M02 feasibility/calibration engine after the
selected adapter resolves precedence/physical filed sets. It proposes matching
complementary defaults without overwriting explicit SDs, including explicit zero,
and rejects infeasible families without clipping. This does not introduce common
cross-model precedence or assign M02 SD semantics to M01.

Asset descriptors pin exact decoded bytes/schema/version/SHA-256 and retrieval
URIs. Conflicting pins anywhere fail; required dependencies select shared assets
and only the active law/model's assets. Resolution has no latest/debug fallback;
byte verification and dependency fingerprints are exposed. These fingerprints
identify assets, not complete fit-cache inputs; stage-specific consumed-input
keys remain CP-03/07 work. Extracted the existing strict JSON grammar primitives
to `core/json-shape.ts`, retaining reform reexports and unchanged runtime checks.
Initial compilation exposed TypeScript's union-array assignment inference;
typed reads and deletion on detached arrays fixed it without weakening the
law-specific placement types.

Verification from `web/` on Windows/PowerShell:

- `npm run check:data` passed; `npm run check` reported zero errors/warnings.
- The new contract suite passed 40 tests, including all three target pairs,
  inactive settings, exact dependencies/checksums, chamber conflict/relock behavior,
  identity lifecycle, all retained supplied-input unions, source/poll metadata,
  geography hierarchy, reference feasibility and D11 transitions/imports.
- Final `npm test -- --maxWorkers=2`: 107 files, 1,264 tests passed, no skips,
  47.62 s. Existing legal/checksum fixtures, CP-01 replay and model-policy tests
  passed without updating expectations.
- `npm run build` passed with the existing plugin-timing/chunk-size warnings.
- Documentation links and `git diff --check` passed.

Checked only CP-02 in the active merger plan and linked the implemented API from
workspace guidance. No existing worker, route, default compiler, import routing,
draft/result storage or legal algorithm was switched to this contract. Frozen
fixtures/manifests and generated defaults/assets remain unchanged. CP-03–08,
the parent merger and final publication gate remain open; no browser release or
1,000-run campaign, commit or publication is claimed.
