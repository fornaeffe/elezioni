# Common politics workspace, scenario and UI

Design proposal for the open workspace-merger item in
[MIGRATION_PLAN.md](../../MIGRATION_PLAN.md). This document defines the target;
it does not describe an implemented contract. The
[decision register](#decision-register) separates approved decisions from open
questions. The [page draft](politics-page-draft.svelte) is approved with the
changes recorded here; component boundaries remain implementation choices.

| Task | Read |
| --- | --- |
| Understand scope and supported selections | [Scope and compatibility](#scope-and-compatibility) |
| Design schemas, parsers or adapters | [Common scenario contract](#common-scenario-contract) |
| Build the editor and controls | [UI behavior](#ui-behavior) |
| Integrate workers, fitting and results | [Architecture and lifecycle](#architecture-and-lifecycle) |
| Review approved decisions or resolve open choices | [Decision register](#decision-register) |
| Implement and verify the merger | [Implementation plan](#implementation-plan) |

## Scope and compatibility

Provide one `/politics` workspace, one editable scenario and independent selects
for vote generator and scrutiny algorithm. Changing a selection changes the
active capabilities and execution adapter; it must not open a separate draft,
reset shared inputs, or discard settings for an inactive model or law.

Priorities, in order: strict adherence to election law and accepted
interpretations with the legal core unchanged; a clean, simple, rational and
uniform architecture and UI; equivalent results where practical. Exact seeded
parity with the previous workspace is not a requirement. Preserve the model
definitions, candidate-generation policies and available scenario controls;
document execution changes rather than retaining complexity solely to reproduce
old random draws. Keep law text close to scrutiny code.
Regional elections remain outside this implementation, but reusable scenario
primitives, controls and lifecycle services
should accommodate them without assuming every election has two chambers.
Municipal simulation remains deferred.

Current entry points are recorded in [web workspaces](../web-workspaces.md).
Today, the previous-law workspace uses `Scenario` and its original generator;
the reform workspace uses `ReleasedScenario` v5 with
[M01 2.0.0](../models/M01.md) or [M02 2.0.0](../models/M02.md).
The original previous-law generator is named **M00**, separately from M01.
Use the same explicit probability-law version policy as M01/M02: exact
supported model/version selection, with model-law versions independent of
scenario/data schemas, numerical/RNG policies and implementation hashes.
Assign M00's initial public version when registering its existing law; the name
does not imply that its version number must equal M01/M02's current version.
The new default is **reform + M02 + joint renewal**.

| Scrutiny | Generator | Initial compatibility baseline |
| --- | --- | --- |
| Previous law, `politiche-r-parity-v1` | M00 | Preserve the workflow and model semantics; seek equivalent results |
| Reform, `politiche-reform-2026-v1` | M01 2.0.0 | Preserve the workflow and model semantics; seek equivalent results |
| Reform, `politiche-reform-2026-v1` | M02 2.0.0 | Preserve the workflow and model semantics; seek equivalent results |
| Previous law | M01 or M02 | Requires an explicit scope decision and validated vote/geography adapters |
| Reform | M00 | Requires an explicit scope decision and validated vote/geography adapters |

Independent selectors do not establish that every combination is executable.
A registry must declare supported pairs, required data and capabilities; the
worker must enforce the same matrix. Explain unavailable options in Italian.
Never fall back silently to another model, version or scrutiny algorithm.
If switching law requires a different generator, show the compatible choices;
whether to propose or automatically select a replacement is decision D02.
Archived model versions remain outside the active selector.

## Common scenario contract

### Ownership and versioning

Introduce a distinct JSON contract, provisionally `kind: 'politics-scenario'`,
`schemaVersion: 1`. Its version is independent of the old scenario schemas,
model-law versions, scrutiny versions, numerical policies, RNG streams, data
versions and implementation hashes. The paths below are proposed vocabulary;
CP-02 must turn them into one authoritative TypeScript type and runtime schema.

The document contains authored inputs and reproducible run settings. Historical
observations and the historical component catalogue stay in pinned data assets.
Memberships refer to catalogue IDs; they do not embed copies of that catalogue.
Scenario-created components, candidate identities, user-added observations and
editable membership declarations belong to the scenario.

| Field/group | Required content and boundary |
| --- | --- |
| `kind`, `schemaVersion`, `id` | Unambiguous format identity, schema version and stable scenario ID |
| `name`, `electionDate`, `referenceDate` | Display name; election and evidence/projection cutoff dates as `YYYY-MM-DD`. Preserve any additional model-specific parameter date rather than conflating dates |
| `renewal` | Selected chambers, expressible as joint, Camera only or Senate only. Retain the other chamber's declarations while inactive |
| `selection.scrutiny` | Stable algorithm ID and exact supported version |
| `selection.generator` | M00, M01 or M02 and exact supported probability-law version |
| `data` | A pinned data manifest, or equivalent exact bundle descriptors: historical source/catalogue, geography, electorate policy, and per-law seat/model assets, with versions and checksums. A historical-data version alone cannot identify all these dependencies |
| `history` | Selected elections, paired Camera/Senate sources, selected polls, exclusions with reasons, source-membership edits and whole-component pooling declarations. Preserve provenance and honor source availability metadata in the pinned assets |
| `subjects` | Shared coalition/list identities and presentation, plus chamber declarations described below |
| `memberships` | Future list-to-component relations by chamber, referencing historical catalogue IDs or `newComponents` IDs |
| `newComponents` | Scenario-owned identities and explicit fallback/support assumptions for lists without historical components; no copied historical definitions |
| `references`, `nonValidReferences` | National/local, common/chamber-specific reference declarations with explicit units and model applicability |
| `candidates` | Shared candidate identities and biography; placements belong to the applicable law |
| `lawSettings[lawId]` | Law-specific filings, candidate placements, candidature-generation settings, links, legal declarations and supplied legal inputs |
| `modelSettings[generatorId]` | Model-specific compositions, partial observations, uncertainty controls, profiles, numerical/RNG policy versions and other settings without a sound shared meaning |
| `execution` | Requested simulation count, fitting seed, simulation seed and requested compute-worker count or explicit automatic policy |
| `provenance` | Default origin, source hashes or manifest references, assumptions and warnings about approximations; no old-format migration metadata is required |

Every registered model/law block has its own versioned schema. Only the selected
blocks affect execution; inactive blocks survive import, export and switching.
Do not duplicate a shared value in those blocks merely to suit an old adapter.
Keep a model-specific field where existing controls have no equivalent shared
meaning, and document its precedence. This preserves capabilities, not support
for old file formats.

Fitted matrices, checkpoints, generated votes/candidates, result rows, progress,
worker ownership and browser storage handles are not scenario fields. Put those
in separately versioned runtime/checkpoint/result envelopes. A scenario may
identify a reusable checkpoint, but its authored meaning must not depend on a
cached fit being present. An unavailable exact data dependency must produce an
actionable error, never substitution with the latest bundle.

### Subjects, memberships and historical sources

- Give each coalition and list one stable ID, name, color and, where applicable,
  list kind. Use IDs for every relation; renaming must not break filings,
  memberships, references, candidates or results.
- Store chamber presence and list coalition membership explicitly. A coalition
  or list can exist in the editor without being filed. Preserve existing
  per-chamber differences, including minority declarations; do not merge them
  merely because names match. Put a declaration in a law block if its legal
  meaning differs between laws.
- Autonomous lists have a null coalition. Singleton candidate tickets needed
  by the previous-law adapter are derived execution objects, never additional
  political coalitions in the scenario or editor.
- Future memberships are set membership, scoped by chamber. Within that scope,
  a component cannot simultaneously belong to conflicting future lists.
  Historical-source memberships are scoped by source/question/choice, with
  stable IDs and provenance; M01 fractional compositions are a separate model
  input, not fractional M02 membership or invented observed counts.
- Poll-option “other” is the complement of the other offered options for that
  question. It is distinct from the future residual list kind `other`, a new
  component, and non-valid votes. Retain the existing residual-list controls.
- A new list initially needs a scenario-owned component if it has no historical
  members. Derive that need from membership, using a stable identity while it
  exists. Do not display an artificial historical component in the membership
  editor. Adding/removing historical membership must not erase saved fallback
  assumptions or create duplicate active components.
- Preserve historical selection, poll editing/additions/exclusions, pooling,
  fallback assumptions and all current controls. User-added polls are explicit
  scenario observations with citation/method/availability metadata, not copies
  of the bundled history. M01 partial named observations stay in its own block
  and must not become complete M02 observations or be counted twice.

### References and uncertainty

A political-reference row identifies `id`, subject (`list` or `component`), scope
(`national`, `region`, `province`, `municipality`), channel (`common`, `camera`,
`senato`), mean and optional uncertainty. Scope codes carry the pinned geographic
namespace. Means use fractions of valid votes in `[0,1]`; display percentages.
Use an explicit `sdPercentagePoints` unit for reference SDs and dimensionless
national/local uncertainty multipliers. Non-valid references use the same
scope/channel structure, with means expressed as fractions of electors.

Absence means no override. A zero mean is an explicit zero, not absence. Missing
uncertainty is not silently zero; use the proposed policy below.
Reject duplicate/conflicting rows and explain normalization
or overlap treatment using the selected model's existing rules. Keep common
and chamber-specific declarations distinct. Do not replace current geographic
precedence, chamber resolution or stochastic reference semantics with a new
universal rule during this merger. CP-02 must specify nullable fields explicitly:
where allowed, null means unspecified/inherit, never zero. Empty collections
represent explicit empty declarations, not a request to restore defaults.

Shared encoding does not imply shared statistical meaning:

| Generator | Semantics to retain |
| --- | --- |
| M00 | Mean-mode global/local overrides, non-valid handling, historical correspondence factors and existing normalization/precedence rules in [Scenario Semantics](../../MIGRATION_PLAN.md#scenario-semantics), with the corrected UI label described below |
| M01 2.0.0 | Original historical compositions, named list polls and list shocks; list/singleton-component references; reference SDs and M02 uncertainty multipliers do not acquire an effect |
| M02 2.0.0 | Grouped-source fitting, identifiable component basis, physical filed sets and calibrated reference uncertainty; preserve existing reference admissibility and movement policies |

Use [the current model boundary](../vote-generation-unified-workflow.md#model-boundary-and-restored-semantics)
for detailed semantics. Use the reform definition and label for equivalent M00
controls too: **“Non valido / astensione”**, explained as abstentions plus invalid
ballots, with an elector denominator. Do not call the combined quantity simply
“astensione”. Where M00 also routes unmapped historical votes into this category,
disclose that modeling treatment separately; do not imply they are observed
abstentions or change their treatment to achieve a label change. Do not turn unmapped
support into zero, add raking, reinterpret reference dates, or introduce fixed
percentage modes as part of the merger. Unsupported controls are inactive and
hidden, with a concise explanation of the active model's interpretation.

#### Defaults for missing reference uncertainty

Only M02 currently consumes reference SDs. Use the following base SDs for a
new reference without an explicit uncertainty, for lists, components and
non-valid support alike. These are editable scenario conventions, not measured
sampling errors, calibrated coverage claims or changes to the M02 probability
law. More local defaults are wider because a generic local declaration has no
associated sample-size evidence. Explicit authored SDs always take precedence.

| Scope | Base SD $b_s$ (percentage points) |
| --- | --- |
| National | 1 |
| Region | 2 |
| Province | 3 |
| Municipality | 5 |

For mean $m$ expressed as a fraction, propose

$$
d(m,s)=\min\{b_s,\;50m,\;50(1-m)\}\quad\text{percentage points}.
$$

The relative cap limits default SD to half the smaller of the target and its
complement: a 1% reference gets 0.5 pp at every scope; a 10% reference gets
1/2/3/5 pp at national/region/province/municipality scope. Boundary means 0 and 1
require zero SD mathematically; make that constraint visible rather than treating
missing uncertainty as an arbitrary exact reference. Missing national/local
uncertainty multipliers default to 1, independently of reference SDs.

Resolve these defaults when a mean/reference is created or edited, show the
effective SD and mark it as a default, and persist the concrete value
and its default/explicit origin. Never overwrite an explicit value or change
an executed scenario when a future default policy changes. Keep the default
policy version in scenario provenance. M00/M01 retain stored SDs while ignoring
them; initialize any still-missing M02 SD visibly on switching to M02.

Validate the complete reference family with the existing M02 feasibility checks.
For two exhaustive complementary groups, SDs must match: with one explicit SD,
propose the same value for its missing counterpart; with both missing, propose
the smaller of their two defaults. Account for any fixed reference mass when
determining the complementary pair. Do not modify explicit values, clip moments,
or introduce reference noise outside the current family. If the joint defaults
are infeasible, show the affected references for correction instead of silently
adjusting them.

### Geography, filings and candidates

Each law block owns its chamber-specific list filings and candidate slots.
Every location reference includes law/geography, chamber and territory level;
equal-looking codes from different namespaces are not interchangeable. Keep
geographic catalogues and seat tables in data assets, including labelled census
reconstructions and the reform-only Bolzano overlay.

Initialize every supported law block when creating a subject, including inactive
laws. A new list has explicit filings in every eligible college of both chambers
under every law; initialize coalition presence in both chambers too. Apply
coalition/chamber restrictions to effective filings without erasing declarations.
Default lists carry their authored default filings for every law, including
intentional territorial restrictions; they are not automatically filed everywhere.
An explicit empty filing set means filed nowhere and must stay empty on switching
or loading. A required missing law block is an incomplete/invalid new-format
document, not an instruction to restore defaults. The editor tree derives its
parent/partial states from filings. Switching law never remaps them implicitly.
Defer the cross-law filing conversion helper.

Candidate identities contain the biography and availability needed by the
existing legal validators, including external candidacies. A placement refers
to a candidate ID, law, chamber, owner, role, location and ordered slot. Ordinary,
bonus, special and previous-law uninominal roles retain their different legal
requirements. Political affiliation belongs to the relevant candidature context;
an independent uninominal candidate must remain representable.

Unfiling a branch, disabling a chamber or restricting coalition presence makes
dependent placements inactive without erasing them. Active placements must obey
age, availability, cross-chamber, ordinary-filing, bonus-eligibility and exclusivity
rules already implemented for the selected law. Do not fabricate missing gender
or birth dates for incomplete candidate declarations: D09 must define how those
identities remain editable and when execution is blocked. Old templates are not
an accepted import format.

Keep candidate-generation settings per law where they differ. The previous-law
uninominal-to-plurinominal reuse share and five-bin candidature-count distribution
cannot be replaced losslessly by the reform's reuse probability and maximum
ordinary filings. Preserve both controls and existing generation policies;
no new candidate-generation policy is wanted. Missing
slot pins continue to allow generation under the current policy; explicitly
pinned candidates continue to constrain it.

Reform reciprocal bonus links apply to eligible coalition/autonomous-list
subjects. Derive the automatic link for a populated subject present in both
chambers; retain explicit links between different subjects. Empty editor
declarations create no legal links. Materialize effective links at the adapter
boundary and preserve declarations for inactive chambers.

### Run settings and serialization

Persist requested simulation count, fitting seed, simulation seed and worker
preference in the scenario. Keep requested and actual worker counts separate:
actual capacity, effective seed, runtime versions/hashes and timings belong in
the executed run record. Automatic worker choice must be represented explicitly,
not inferred from a missing value. Different pool widths must not change global
simulation IDs, RNG streams or deterministic output.

Keep fitting and simulation seeds as separate visible inputs. Next to each,
provide a **“Genera seed casuale”** button that fills only that field with a new
visible seed. The click is an ordinary draft edit: save the concrete value with
the scenario, invalidate only the affected preparation/run stages, and use that
value unchanged on execution. Do not add a randomization checkbox, launch-time
randomization mode, or automatic seed replacement on loading or running.

Scenario-only JSON/JSON.gz roundtrips must preserve inactive settings. Result
archives must include the immutable executed scenario and worker result, actual
execution provenance and any required portable fit state; CSV remains oriented
to result tables. Continue generic export through
`web/src/lib/core/result-export.ts`. Large archives remain streamed.

Accept only the new common politics scenario schema and its new result/archive
schema, with exact supported model versions. Reject all old politics scenario
and archive schemas explicitly, without converting them or replacing the current
draft. This also excludes loading old politics drafts or restoring old scenario
schemas through checkpoints. A reusable checkpoint must match the new scenario's
exact identity/dependencies; do not require a new independent checkpoint-payload
version solely for this merger if its contract remains suitable. Frozen historical
fixtures remain test evidence, not runtime import formats. Regional compatibility
is outside this change.

### One authored default

Replace the multiple politics-default compilers' independent scenario choices
with one versioned, validated authored JSON file, provisionally
`data/politics-default-scenario.json`. Include all shared declarations and complete
blocks for the three baseline workflows, exact data references, both seeds and
run preferences. Select reform + M02 + joint renewal (D03 approved). D04 still
must reconcile default shares, dates, per-law filings and assumptions.

The authored file contains no historical observation tables or fitted arrays.
Retain Node/TypeScript preparation of historical/geographic/model bundles,
fitted states and release catalogues. Technical packaging, validation, checksums
and descriptors may be generated from the authored scenario; they must not infer
or maintain another set of scenario choices. Do not edit generated defaults by
hand. After snapshot regeneration, regenerate affected descriptors/catalogues
and verify all pinned dependencies.

## UI behavior

The draft is approved with the changes recorded here, including the status box.
Use its progressive disclosure and Italian labels. Build smaller
components around shared contracts rather than placing a full old workspace
inside each selector branch.

1. **Scenario bar:** home/breadcrumbs, load JSON/JSON.gz, download scenario and
   restore default. Follow with name, election date, scrutiny selector and
   chambers to renew. Preserve the existing reset confirmation and keep results
   tied to their executed scenario.
2. **Coalitions:** compact color/name controls; advanced chamber presence and
   reform links; collapsed candidate trees for reform bonus slates or previous-law
   uninominal slots. Candidate choices show list affiliation where relevant,
   reasons for ineligibility, creation and editing actions.
3. **Lists:** compact color/name/coalition and optional national reference.
   Advanced sections hold applicable uncertainty, filings/candidates, links,
   minority declarations and local references. Retain chamber-specific values
   and common/chamber reference controls even when the compact row shows only
   the common value. Show a badge for differing chamber values.
4. **Filing trees:** chamber → constituency → applicable colleges, with tri-state
   parent checkboxes and candidate badges. Do not add a partial-coverage badge
   when the tri-state checkbox already conveys partial filing. Use the
   selected law's actual geography rather than forcing every law into the same
   levels. Names are visible; IDs remain in serialized data. Candidate selectors
   disable ineligible options with a reason and offer create/edit; reform
   ordinary slots can expose the eligible bonus-slate toggle.
5. **Local references:** region → province → municipality, with reference and
   applicable SD controls; delete one declaration or all declarations in a
   subtree. Badges reveal declarations inside collapsed descendants. Removal
   of an override restores the active model's inherited behavior.
6. **Advanced scenario:** generator selector; M01 partial observations only when
   M01 is active; collapsed source/future membership editor, historical/poll
   selection and applicable model/profile controls. Future lists are the initial
   membership view; fractions appear only for applicable M01 source compositions.
   Hide poll “other” members and derive the complement. Disable components used
   by conflicting choices within the same source; retain existing valid source
   grouping/pooling semantics. Include every existing setting through a suitable
   control, including a component-reference editor with the same national/local
   and common/chamber controls as lists. New components' references are active
   only while their lists lack historical memberships. Do not add an embedded
   raw JSON editor.
7. **Simulation controls:** count and Run; collapsed fitting/simulation seed
   inputs, each with its own “Genera seed casuale” button, and worker count.
   Load the default/imported/selected model automatically,
   reusing valid preparation or computing only required work. No separate “load
   default model” action. Keep fitting progress, diagnostics, cancellation and
   portable checkpoint access available where applicable.
8. **Results and status:** common table/chart/session access with law-specific
   views supplied by adapters. Use a small fixed, collapsible status box near the
   top right, avoiding controls and mobile content; include state, progress and
   applicable Cancel. Keep detailed diagnostics accessible and distinguish
   informational notes, warnings and errors by severity.

For newly created subjects, initialize both chambers and every supported law;
a new list is filed in every eligible college and starts without historical
members. Default lists retain their authored filings for all laws. A coalition's
presence limits effective member-list filings while preserving inactive
declarations. Creation defaults never overwrite explicitly saved declarations.
Keep unrelated metric/export changes separate from this approved UI scope.

All hidden sections remain stored. Showing a section again restores its values.
Use lazy tree expansion/search/paging so national candidate and municipality
trees do not mount in full. Keep keyboard operation, labelled controls, visible
focus and accessible partial-selection/status announcements.

## Architecture and lifecycle

Keep four boundaries, with reusable primitives shared with the regional
workspace only where meanings agree:

| Boundary | Responsibility |
| --- | --- |
| Scenario domain | Authoritative new-format schema, validation, immutable editing operations, effective declarations and dependency fingerprints; no legacy schema migrations, Svelte or worker ownership |
| Capabilities and adapters | Separate generator and scrutiny registries plus supported-pair matrix; typed preparation → vote generation → legal assembly → scrutiny interfaces. Preserve model policies and the legal core; simplify orchestration without requiring old seeded traversal |
| Workspace controller | One draft and task lifecycle; lazy assets, fitting/cache reuse, worker pool, cancellation, durable result sessions and immutable executed snapshots |
| Presentation | Capability-driven Svelte components, tested TypeScript result-table/chart helpers and shared export/result access |

Reuse the existing shell, controls, draft/controller, fit service and result
services where sound. Extract generation interfaces from the reform namespace
when they become common; do not require the previous law to construct a fake
reform legal scenario. Avoid a large all-purpose component and duplicated
model/law selection logic in UI, import routing and workers.

Validate structure and references for the complete document. Validate active
model/law semantics, legal constraints and required assets before execution,
repeating execution validation at the worker boundary. Incomplete inactive
blocks can remain editable; they must be reported when selected, not silently
repaired. Reject unknown schema/extension versions rather than adding compatibility
readers under D12.

Capture an immutable scenario and execution identity before starting a task.
Use revisions/fingerprints to reject late replies after edits or selection
changes. Lock law/model switching during active execution and provide explicit
cancellation. On selection, load/reuse preparation automatically and make any
required preparation visible and cancellable. Do not silently cancel a running
execution because a selector changed. Dispose owned workers/services on
navigation and failure.

Compute cache dependencies from consumed inputs, not from the entire document.
Name/color edits and inactive-block edits must not refit. Evidence, membership,
model/version, dates, geography and electorate changes invalidate only the
preparation stages that consume them. Candidate or legal changes invalidate
legal previews/results without automatically invalidating a reusable statistical
fit. Include all consumed law-specific filing/geography inputs in generation
keys. Cross-law reuse is allowed only when those dependencies actually agree.

Keep result presentation bound to the executed scenario, even after selecting
another law or model. Show stale-result status without relabelling old results.
Retain current lazy stored-result queries, portable checkpoints, streaming
archives, diagnostic simulation membership, requested denominators and durable
prefixes after interruption. Do not reduce the reform workflow to the legacy
in-memory limit, or remove previous-law limits without resource evidence.

## Decision register

### Approved decisions

These decisions are settled. Apply them in the contract, UI and implementation
plan; do not reopen them as implementation gates.

| ID | Approved direction |
| --- | --- |
| D01 | **Generator identity/version:** the previous-law generator is M00, using the same explicit model/version policy as M01/M02; keep data, schema, numerical/RNG and implementation versions separate |
| D03 | **Initial selection:** reform + M02 + joint renewal |
| D07 | **Filings for all laws:** initialize every law block for new subjects; a new list is filed everywhere eligible in both chambers. Default lists have authored default filings for every law. Defer the conversion helper. Use tri-state checkboxes without redundant partial-coverage badges; retain candidate badges |
| D10 | **Candidate controls:** preserve both law-specific generation parameterizations in their respective law blocks, independent candidates, availability/external candidacies and manual pin behavior. No new candidate-generation policy |
| D12 | **File compatibility:** no support for old politics scenario/archive schemas; reject them without migration. Use the new common-format draft/session and validate checkpoints against its exact scenario/dependencies |
| D15 | **Seeds and workers:** a “Genera seed casuale” button beside each seed input fills that field visibly. No fit-level or simulation-level randomization mode or automatic seed replacement. Save requested workers/auto policy and record actual workers |
| D16 | **Switching/preparation:** lock during active execution, explicit cancellation, automatic loading/reuse on selection and visible cancellable preparation when required |
| D18 | **UI scope:** the page draft is approved with the changes in this document, including the status placement. Keep unrelated metric/export changes separate |
| D19 | **Acceptance priorities:** legal core unchanged, with strict adherence to law and accepted interpretations first; clean, simple, rational and uniform architecture/UI next; seek equivalent results, but exact seeded parity is not required. AGENTS.md requirements still hold |

D13 (adopting multiple old drafts) is superseded by D12: do not read or combine
old politics drafts. Start from the new default or an explicitly loaded new-format
scenario and persist under a new common draft key. Leave old browser keys
untouched and unused; deleting old stored data is outside this merger.

### Decisions still needed

IDs remain stable after approvals. Resolve only the open choices below before
their dependent step. Recommendations, including the numerical SD proposal in
D06, are not approved changes. Old-file migration is not an option under D12.

| ID | Open decision and recommended direction | Needed before |
| --- | --- | --- |
| D02 | **Combination scope and selection transitions:** are only the three baseline workflows required, or also cross-law use of M00/M01/M02? Which generator should be selected when the current one is incompatible with a new law? Recommend explicit supported pairs and a visible compatible choice; no silent substitution | CP-02/CP-03 |
| D04 | **Authored default and data pins:** approve filename, exact manifest and remaining values: colors, dates, references, memberships, per-law default filings, candidate parameters, run count/seeds. Law/model/renewal are settled by D03 and every law must have default filings under D07. Decide whether the separate party-color task is included | CP-04 |
| D05 | **Shared/chamber declarations:** finalize coalition/list identities and per-chamber coalition membership, minority declarations and optional names. Recommend stable IDs with explicit chamber differences, not name-based matching | CP-02 |
| D06 | **References:** approve common/chamber representation, precedence disclosures, joint feasibility and persistence policy above. Preserve each model's interpretation. The corrected non-valid label is settled and needs no further approval | CP-02/CP-03 |
| D08 | **M00 correspondence controls:** define how M00's arbitrary factor correspondences coexist with component memberships when factorization is impossible. Recommend model-specific M00 controls preserving those capabilities and chamber differences; never disguise equal splits as observations. This concerns the new contract, not importing old files | CP-02/CP-03 |
| D09 | **Candidate identity and incomplete declarations:** finalize cross-law identity sharing, deduplication, edit/delete propagation and treatment of missing birth date/gender in new-format drafts. Recommend explicit incomplete identities, no inference from names and active-law validation before execution | CP-02/CP-06 |
| D11 | **Automatic bonus links:** approve deriving automatic reciprocal links versus serializing them, explicit-link precedence and changes after coalition/chamber edits. Recommend declarations plus deterministic effective links, without linking empty subjects | CP-02/CP-03 |
| D14 | **History and advanced controls:** confirm placement of source/poll additions/exclusions, pooling, fallback assumptions, non-valid/electorate inputs, supplied votes/preferences and model/profile settings. Recommend a control inventory with no lost settings, using model-specific controls where meanings differ | CP-01/CP-06 |
| D17 | **Result sessions and resources:** decide whether M00 adopts durable storage/multiworker execution immediately, retains its 1,000-run limit, and how result sessions are selected. Recommend preserving each adapter's current limits/capabilities first, with one common access layer | CP-03/CP-07 |

Scrutiny version metadata must be explicit in CP-02; D01's naming decision changes
the generator identity only, not existing scrutiny algorithm IDs or legal behavior.
The exact campaign matrix follows D02/D17; D19 fixes the acceptance priorities
and does not waive the existing performance or legal verification requirements.

## Implementation plan

Implement in dependency order. Each step should leave a reviewable, independently
verified result. Apply approved decisions and resolve remaining choices before
committing to the relevant schema or behavior. Add the agreed steps to the active
migration plan when implementation
starts; record completed changes, rationale, verification and problems in
[migration history](../migration-history.md). Keep the workspace-merger item open
until CP-08 passes. This documentation proposal alone does not complete it.

### CP-01 — Inventory and preserve behavior

Read the current entry points listed below. Build a field/control/asset mapping
from original `Scenario` and reform `ReleasedScenario` to the proposed contract,
including data not visible in the draft. Identify non-equivalent defaults,
arbitrary correspondence factors, candidate identities and per-chamber values.
Resolve D14 and record reproducible baselines for all three existing workflows,
single-chamber runs, restricted/special territories, residual/new
lists, manual candidates, diagnostics and interrupted/exported sessions.

Exit: a complete mapping, supported-pair scope proposal and reproducible comparison
inputs/results. Separate generator/distribution comparisons from fixed-input legal
regressions. Preserve frozen fixtures/checksum manifests and known legal failures;
never regenerate legal expectations to make the merger pass. Whole-pipeline exact
seeded matching is useful evidence, not a condition that dictates architecture.

### CP-02 — Establish the shared domain contract

Apply D01/D07/D12/D15; resolve D02/D05/D06/D08/D09/D11 as required by the schema.
Implement common types, strict runtime parsing/serialization, independent
registries, capabilities,
effective-presence/link helpers and pure edit operations. Keep law/model blocks
typed and versioned. Define active versus inactive validation and exact data
dependency resolution. Design the result/archive envelope's versioning separately.

Exit: domain checks prove units, null/absence/zero distinctions, stable IDs,
membership constraints, law-specific geography, inactive-data retention and
scenario-only JSON/gzip roundtrips. Unsupported versions/pairs fail explicitly.

### CP-03 — Adapt existing execution paths

Apply D10/D19, resolve D08/D17 and implement one preparation/generation/assembly
dispatch boundary. Adapt each workflow while keeping model definitions,
candidate-generation policies and the legal core unchanged. Prefer clean shared
contracts to preserving incidental old RNG traversal or generated IDs. Keep
reproducibility within the new implementation, legally meaningful order and
accepted tie behavior. Normalize common input encoding at the adapter boundary.
Implement additional generator/law pairs only if D02 includes them, with explicit geography and
candidate/legal assembly contracts and their own acceptance cases.

Exit: the same fixed legal inputs retain legal outcomes, legally meaningful
ordering and diagnostics. Compare generation, candidature and simulation results
with CP-01 for equivalence; explain material differences, isolate RNG-order
changes from semantic regressions, and verify the model policies/distributions.
Exact old seeded draws/IDs are not a gate. Existing legal-failure fixtures remain
valid. Worker validation uses the same supported-pair registry.

### CP-04 — Replace authored-default construction

Apply D03/D07 and resolve D04. Create the single authored JSON with complete
baseline law/model blocks; make preparation, browser startup, reset and release packaging consume
it. Replace scripts that independently choose scenario values with validation
or deterministic packaging. Regenerate affected scenario descriptors/catalogues
from the complete production pipeline. Remove obsolete default compilers only
after tracing every active consumer, including regional preparation.

Exit: clean preparation produces all required production assets; validation
detects missing/incompatible pins; one authored input determines every default
choice, including default filings under every law. Verify model/algorithm inputs
and approved defaults directly; no old-format import or exact seeded default
parity is required. No generated default is edited by hand.

### CP-05 — Enforce new-format files and draft isolation

Apply D12 and its D13 consequence. Implement strict new common-scenario and
result/archive readers/writers, plus a new shared draft key. Inspect metadata
without parsing a complete large archive. Reject old politics formats with a
clear message and leave the current draft/results intact; add no converters or
readers for old drafts/scenario-bearing checkpoints. Validate reusable checkpoints
against the new scenario. Leave independent regional routing unchanged.

Exit: new-format scenarios/results roundtrip with inactive settings, executed
identity and session evidence intact. Old politics files are rejected, old draft
keys are ignored and untouched, and corrupt/unsupported inputs cannot replace
current state. Historical regression fixtures remain separate test evidence.

### CP-06 — Build the common editor

Apply D07/D10/D15/D16/D18 and resolve D09/D14. Compose a single politics editor
from subject, reference, membership, filing/candidate, model and run-control components.
Drive visibility/applicability through capabilities and use pure domain edits;
use Svelte 5 `$state` and `$derived`, with lifecycle hooks or event handlers for
owned tasks. Keep expensive trees lazy. Automatically initialize model loading
and expose relevant fit/checkpoint controls without a load-default-model button.

Exit: law/model/chamber switching retains shared and inactive values, selected
candidate pins and tree declarations. Tri-state filings, disabled candidates,
new-component transitions, references, both seed-generation buttons,
reset/new-format import/export and error paths work. New subjects initialize
all law blocks, redundant partial-coverage badges are absent and candidate
badges remain. Check keyboard/mobile layout and run the Svelte tooling on
changed components.

### CP-07 — Unify lifecycle, persistence and results

Integrate the editor with one draft/controller and the execution adapters.
Apply D15/D16: implement execution locking/explicit cancellation, automatic
loading/reuse with visible cancellable preparation, dependency-based fit reuse,
button-generated visible seeds, requested/effective workers and safe disposal.
Reuse existing durable result/session and streaming export services; keep result
tables/charts in tested presentation helpers. Preserve the immutable executed
scenario and stale-result indication across subsequent edits and selections.

Exit: delayed replies cannot update a newer revision; cosmetic/inactive edits do
not refit; relevant edits invalidate the correct stages. Reload, cancellation,
checkpoint restore, failures, stored queries and archive import/export retain
correct simulation membership, counts, seeds and original requested denominator.
Running or loading never replaces seeds; each seed button edits only its field.

### CP-08 — Acceptance, cleanup and documentation

Run focused domain/adapter checks during their steps, then the repository's
current production acceptance gate from `web/` after full preparation. Cover
every approved pair at `/` and `/elezioni`: direct loads, navigation, workers,
assets, imports, switching, results and exports. Verify regional smoke cases.
Release checks require every bundle and reject absent-data skips/debug fallbacks.

Run the agreed browser fitting + 1,000-simulation campaigns sequentially, including
generation and scrutiny, against the existing under-60-minute target on this
machine. Record model/data/version, actual workers, timing and memory evidence.
Retain distinct legal/non-computable and infrastructure failure reporting; the
known previous-law dead end requires separate legal review, not rerolling,
skipping or claiming a successful campaign.

Apply D19 when assessing comparisons: document equivalence and any differences
from old draws, but do not retain architectural duplication to force exact
seeded parity. Fixed-input legal regression tests and law/interpretation
requirements remain blocking. A passing expected-failure regression is not a
successful 1,000-run campaign.

After domain, legal and new-format checks pass, remove superseded politics
workspace components, route branches, draft writers, old schema readers and
default logic. Retain independent regional functionality. Update current
workspace, preparation and user docs, record evidence in history and close the
migration item only when its agreed acceptance criteria are met. Public
deployment and legal/model corrections remain separate tasks.

### Current implementation entry points

Paths are relative to `web/`. Read only the row relevant to the current step;
this is a navigation map, not a mandate to retain existing boundaries.

| Concern | Entry points |
| --- | --- |
| Routes, registry and current lifecycle | `src/routes/politics/+page.svelte`; `src/lib/workspaces/{registry.ts,controller.svelte.ts,draft.svelte.ts,import-routing.ts}` |
| Original scenario/generation/scrutiny | `src/lib/core/types.ts`; `src/lib/scenario/politics.ts`; `src/lib/politics/{pipeline.ts,vote-generation.ts,candidate-generation.ts,scrutiny-algorithms.ts}` |
| Reform scenario, edits, candidates and assembly | `src/lib/politics/reform-2026/{generation-types.ts,generation-scenario.ts,scenario-editor.ts,candidature-validation.ts,assembly.ts}` |
| M01/M02 schema, dispatch, fit and edit services | `src/lib/politics/reform-2026/vote-share/{release.ts,scenario.ts,model-registry.ts,unified-generation.ts,fit-service.ts,editor-model.ts}` |
| Existing UI to consolidate | `src/lib/workspaces/LegacyPoliticsWorkspace.svelte`; `src/lib/politics/reform-2026/ReformWorkspace.svelte`; `src/lib/politics/reform-2026/vote-share/{FastReformWorkspace.svelte,FastScenarioEditor.svelte}` |
| Authored/default preparation | `../data/fast-share-future-scenario.json`; `scripts/workspaces/{defaults.ts,generate.ts}`; `scripts/data-preparation/{unified-share-default.ts,fast-share-release.ts}` |
| Result contracts, access and export | `src/lib/core/{types.ts,result-export.ts,result-store.ts}`; `src/lib/workspaces/result-access.ts`; `src/lib/politics/result-presentation.ts`; `src/lib/politics/reform-2026/result-presentation.ts` |
