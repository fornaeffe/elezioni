# Common politics workspace, scenario and UI

Design proposal for the open workspace-merger item in
[MIGRATION_PLAN.md](../../MIGRATION_PLAN.md). This document defines the target;
it does not describe an implemented contract. Recommendations in the
[decision register](#decisions-still-needed) remain proposals until resolved.
The [page draft](politics-page-draft.svelte) illustrates the intended interaction,
without prescribing component boundaries or every detail of the layout.

| Task | Read |
| --- | --- |
| Understand scope and supported selections | [Scope and compatibility](#scope-and-compatibility) |
| Design schemas, parsers or adapters | [Common scenario contract](#common-scenario-contract) |
| Build the editor and controls | [UI behavior](#ui-behavior) |
| Integrate workers, fitting and results | [Architecture and lifecycle](#architecture-and-lifecycle) |
| Resolve product or semantic choices | [Decisions still needed](#decisions-still-needed) |
| Implement and verify the merger | [Implementation plan](#implementation-plan) |

## Scope and compatibility

Provide one `/politics` workspace, one editable scenario and independent selects
for vote generator and scrutiny algorithm. Changing a selection changes the
active capabilities and execution adapter; it must not open a separate draft,
reset shared inputs, or discard settings for an inactive model or law.

Simplify the application without changing probability laws, scrutiny rules,
tie-breaking, candidate-generation behavior or available scenario controls.
Keep law text close to scrutiny code. Regional elections remain outside this
implementation, but reusable scenario primitives, controls and lifecycle services
should accommodate them without assuming every election has two chambers.
Municipal simulation remains deferred.

Current entry points are recorded in [web workspaces](../web-workspaces.md).
Today, the previous-law workspace uses `Scenario` and its original generator;
the reform workspace uses `ReleasedScenario` v5 with
[M01 2.0.0](../models/M01.md) or [M02 2.0.0](../models/M02.md).
The original previous-law generator is **not** another name for M01.

| Scrutiny | Generator | Initial compatibility baseline |
| --- | --- | --- |
| Previous law, `politiche-r-parity-v1` | Original previous-law generator | Preserve the existing workflow and seeded behavior |
| Reform, `politiche-reform-2026-v1` | M01 2.0.0 | Preserve the existing workflow and seeded behavior |
| Reform, `politiche-reform-2026-v1` | M02 2.0.0 | Preserve the existing workflow and seeded behavior |
| Previous law | M01 or M02 | Requires an explicit scope decision and validated vote/geography adapters |
| Reform | Original previous-law generator | Requires an explicit scope decision and validated vote/geography adapters |

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
| `selection.generator` | Stable generator ID and exact supported probability-law version; register the original generator separately from M01/M02 |
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
| `provenance` | Default origin, migration/conversion source, source hashes or manifest references, assumptions and warnings about approximations |

Every registered model/law block has its own versioned schema. Only the selected
blocks affect execution; inactive blocks survive import, export and switching.
Do not duplicate a shared value in those blocks merely to suit an old adapter.
Use a specific compatibility field when an old input cannot be represented
losslessly by the shared concept, and document its precedence.

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
uncertainty is not silently zero; define the selected model's default or require
an explicit value. Reject duplicate/conflicting rows and explain normalization
or overlap treatment using the selected model's existing rules. Keep common
and chamber-specific declarations distinct. Do not replace current geographic
precedence, chamber resolution or stochastic reference semantics with a new
universal rule during this merger. CP-02 must specify nullable fields explicitly:
where allowed, null means unspecified/inherit, never zero. Empty collections
represent explicit empty declarations, not a request to restore defaults.

Shared encoding does not imply shared statistical meaning:

| Generator | Semantics to retain |
| --- | --- |
| Original previous-law generator | Mean-mode global/local overrides, abstention handling, historical correspondence factors and existing normalization/precedence rules in [Scenario Semantics](../../MIGRATION_PLAN.md#scenario-semantics) |
| M01 2.0.0 | Original historical compositions, named list polls and list shocks; list/singleton-component references; reference SDs and M02 uncertainty multipliers do not acquire an effect |
| M02 2.0.0 | Grouped-source fitting, identifiable component basis, physical filed sets and calibrated reference uncertainty; preserve existing reference admissibility and movement policies |

Use [the current model boundary](../vote-generation-unified-workflow.md#model-boundary-and-restored-semantics)
for detailed semantics. Retain the legacy distinction between non-valid votes
and its UI label “astensione” in adapter documentation. Do not turn unmapped
support into zero, add raking, reinterpret reference dates, or introduce fixed
percentage modes as part of the merger. Unsupported controls are inactive and
hidden, with a concise explanation of the active model's interpretation.

### Geography, filings and candidates

Each law block owns its chamber-specific list filings and candidate slots.
Every location reference includes law/geography, chamber and territory level;
equal-looking codes from different namespaces are not interchangeable. Keep
geographic catalogues and seat tables in data assets, including labelled census
reconstructions and the reform-only Bolzano overlay.

Represent declared filings explicitly, including an empty filing set. Distinguish
“not yet configured for this law” from “configured and filed nowhere”; do not
silently expand either into filing everywhere. The editor's tree is a view over
that data, with derived parent/partial states. Switching law never remaps filings
implicitly. A later import-from-other-law helper must preview assumptions and
report unmappable/approximated locations.

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
or birth dates when importing older templates: D09 must define how incomplete
identities remain editable and when execution is blocked.

Keep candidate-generation settings per law where they differ. The previous-law
uninominal-to-plurinominal reuse share and five-bin candidature-count distribution
cannot be replaced losslessly by the reform's reuse probability and maximum
ordinary filings. Preserve both controls and their seeded generation. Missing
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

“Use a random seed” is a transient launch preference, default false. At launch,
resolve and save the concrete seed before capturing the executed scenario;
loading a saved scenario disables randomization. Keep fitting and simulation
seeds separate and define which seeds randomization affects in D15.

Scenario-only JSON/JSON.gz roundtrips must preserve inactive settings. Result
archives must include the immutable executed scenario and worker result, actual
execution provenance and any required portable fit state; CSV remains oriented
to result tables. Continue generic export through
`web/src/lib/core/result-export.ts`. Large archives remain streamed.

### One authored default

Replace the multiple politics-default compilers' independent scenario choices
with one versioned, validated authored JSON file, provisionally
`data/politics-default-scenario.json`. Include all shared declarations and complete
blocks for the three baseline workflows, exact data references, both seeds and
run preferences. D03/D04 select the default law/model and reconcile current
differences in default shares, dates, filings and assumptions.

The authored file contains no historical observation tables or fitted arrays.
Retain Node/TypeScript preparation of historical/geographic/model bundles,
fitted states and release catalogues. Technical packaging, validation, checksums
and descriptors may be generated from the authored scenario; they must not infer
or maintain another set of scenario choices. Do not edit generated defaults by
hand. After snapshot regeneration, regenerate affected descriptors/catalogues
and verify all pinned dependencies.

## UI behavior

Use the draft's progressive disclosure and Italian labels. Build smaller
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
   parent checkboxes, partial-coverage badges and candidate badges. Use the
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
   control; do not add an embedded raw JSON editor.
7. **Simulation controls:** count and Run; collapsed seeds, random-seed preference
   and worker count. Load the default/imported/selected model automatically,
   reusing valid preparation or computing only required work. No separate “load
   default model” action. Keep fitting progress, diagnostics, cancellation and
   portable checkpoint access available where applicable.
8. **Results and status:** common table/chart/session access with law-specific
   views supplied by adapters. Use a small fixed, collapsible status box near the
   top right, avoiding controls and mobile content; include state, progress and
   applicable Cancel. Keep detailed diagnostics accessible and distinguish
   informational notes, warnings and errors by severity.

For newly created subjects, the draft proposes both chambers and filing in every
eligible college of the selected law; a new list starts without historical
members. A coalition's presence limits effective member-list filings while
preserving inactive declarations. Confirm initialization of the other law in
D07; these UI defaults must never overwrite imported or previously saved values.

All hidden sections remain stored. Showing a section again restores its values.
Use lazy tree expansion/search/paging so national candidate and municipality
trees do not mount in full. Keep keyboard operation, labelled controls, visible
focus and accessible partial-selection/status announcements.

## Architecture and lifecycle

Keep four boundaries, with reusable primitives shared with the regional
workspace only where meanings agree:

| Boundary | Responsibility |
| --- | --- |
| Scenario domain | Authoritative schema, migrations, validation, immutable editing operations, effective declarations and dependency fingerprints; no Svelte or worker ownership |
| Capabilities and adapters | Separate generator and scrutiny registries plus supported-pair matrix; typed preparation → vote generation → legal assembly → scrutiny interfaces. Adapt to existing engines without changing kernels or RNG traversal |
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
repaired. D12 settles forward-version handling for unknown inactive extensions.

Capture an immutable scenario and execution identity before starting a task.
Use revisions/fingerprints to reject late replies after edits or selection
changes. Initially disable law/model changes during incompatible running tasks,
retaining explicit Cancel; D16 settles whether later switching should cancel
automatically. Dispose owned workers/services on navigation and failure.

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

## Decisions still needed

The target above supplies design recommendations. Resolve this register before
the dependent implementation step; record the answer here, not in scattered
code comments. “Recommended” is not an approved semantic change.

| ID | Decision and recommended direction | Needed before |
| --- | --- | --- |
| D01 | **Generator identity/version:** choose a stable ID and version policy for the original previous-law generator and scrutiny versions. Keep M01/M02 canonical identities; do not rename the original generator M01 | CP-02 |
| D02 | **Combination scope and selection transitions:** are only the three baseline workflows required, or also cross-law reuse of M01/M02/original generation? Which generator should be selected when the current one is incompatible with a new law? Recommend explicit supported pairs and a visible compatible choice; no silent substitution | CP-02/CP-03 |
| D03 | **Initial selection:** the draft proposes reform 2026 and both chambers; the current page defaults to previous law and reform defaults to M02. Recommend reform + M02 + joint renewal for the new default, retaining imported selections | CP-04 |
| D04 | **Authored default and data pins:** approve filename, exact source/seat/geography manifest and all default values, including colors, dates, references, memberships, per-law filings, candidate settings and run count/seeds. Reconcile today's differing defaults explicitly; decide whether the separate party-color task is included | CP-04 |
| D05 | **Shared/chamber declarations:** approve canonical coalition/list identity matching and per-chamber coalition membership, minority declarations and names. Recommend stable identity plus explicit chamber differences; require user resolution when importing ambiguous name-based identities | CP-02/CP-05 |
| D06 | **References:** approve common/chamber representation, missing-SD policy, precedence disclosures and handling of legacy inputs without a lossless shared meaning. Recommend explicit compatibility settings and existing model semantics, with no invented uncertainty effects | CP-02/CP-03 |
| D07 | **Filings on another law:** initialize all law blocks for new subjects, or leave unvisited laws unconfigured? Decide effective presence rules, badges and whether the optional cross-law filing import is required now. Recommend complete authored defaults, explicit state for user-created missing blocks and defer the conversion helper | CP-02/CP-06 |
| D08 | **Membership compatibility:** decide how arbitrary old factor correspondences map to the component architecture when factorization is impossible. Recommend lossless original-generator settings plus explicit conversion reports, never equal-split assumptions disguised as observations; retain chamber differences | CP-03/CP-05 |
| D09 | **Candidate identity and incomplete imports:** define merging/deduplication, cross-law identity sharing, edit/delete propagation and treatment of missing birth date/gender. Recommend explicit incomplete identities, no inference from names and active-law validation before execution | CP-02/CP-05 |
| D10 | **Candidate controls:** approve preservation/location of both law-specific generation parameterizations, independent candidates, availability/external candidacies and manual pin behavior. Decide whether any new candidate-generation policy is wanted; recommend none in this merger | CP-03/CP-06 |
| D11 | **Automatic bonus links:** approve deriving automatic reciprocal links versus serializing them, explicit-link precedence and changes after coalition/chamber edits. Recommend declarations plus deterministic effective links, without linking empty subjects | CP-02/CP-03 |
| D12 | **File compatibility:** enumerate accepted old scenario/archive schemas, unsupported versions, migration guarantees and handling of unknown inactive extensions. Recommend migrating currently supported inputs losslessly and retaining old archives as executed evidence; archived models do not become runnable | CP-02/CP-05 |
| D13 | **Multiple saved drafts:** choose how to adopt the separate legacy/reform local drafts when both exist. Recommend choosing one as the initial common draft with exportable originals retained; do not silently combine conflicting scenarios or delete old keys | CP-05/CP-07 |
| D14 | **History and advanced controls:** confirm access to source/poll additions/exclusions, pooling, fallback assumptions, non-valid/electorate inputs, supplied votes/preferences and model/profile settings. Recommend a control inventory with no lost settings; decide which are dedicated controls versus retained compatibility blocks | CP-01/CP-06 |
| D15 | **Seeds and workers:** decide whether randomization affects simulation only or fitting too, when seeds are saved, and how worker preferences behave across machines/adapters. Recommend simulation-only randomization, explicit fit seed and saved requested workers/auto policy with actual count recorded | CP-02/CP-07 |
| D16 | **Switching during work and automatic preparation:** decide locking versus cancel-on-switch and when expensive refits start. Recommend lock during active execution, explicit cancellation, automatic loading/reuse on selection and visible cancellable preparation when required | CP-06/CP-07 |
| D17 | **Result sessions and resources:** decide whether the original workflow adopts durable storage/multiworker execution immediately, retains its 1,000-run limit, and how result sessions are selected. Recommend preserve each adapter's current limits/capabilities first, with one common access layer | CP-03/CP-07 |
| D18 | **UI scope:** approve the draft's compact rows, collapsed trees, advanced generator placement and status box, and decide whether adjacent migration-plan UI tasks join this change. Recommend include the status placement described here and keep unrelated metric/export changes separate | CP-06 |
| D19 | **Acceptance threshold:** approve parity scenarios, legal-failure expectations, full campaign matrix and timing/resource evidence for any new pair. Recommend exact seeded parity for existing workflows and a separate measured 1,000-run campaign per supported pair; do not count known legal dead ends as passes | CP-01/CP-08 |

## Implementation plan

Implement in dependency order. Each step should leave a reviewable, independently
verified result. Resolve its decisions before committing to the relevant schema
or behavior. Add the agreed steps to the active migration plan when implementation
starts; record completed changes, rationale, verification and problems in
[migration history](../migration-history.md). Keep the workspace-merger item open
until CP-08 passes. This documentation proposal alone does not complete it.

### CP-01 — Inventory and preserve behavior

Read the current entry points listed below. Build a field/control/asset mapping
from original `Scenario` and reform `ReleasedScenario` to the proposed contract,
including data not visible in the draft. Identify non-equivalent defaults,
arbitrary correspondence factors, candidate identities and per-chamber values.
Resolve D14/D19 and record representative seeded baselines for all three existing
workflows, single-chamber runs, restricted/special territories, residual/new
lists, manual candidates, diagnostics and interrupted/exported sessions.

Exit: a complete mapping, supported-pair scope proposal and reproducible parity
inputs/results. Preserve frozen fixtures/checksum manifests and known legal
failures; never regenerate expectations to make the merger pass.

### CP-02 — Establish the shared domain contract

Resolve D01/D02/D05–D07/D09/D11/D12/D15 as required by the schema. Implement common
types, strict runtime parsing/serialization, independent registries, capabilities,
effective-presence/link helpers and pure edit operations. Keep law/model blocks
typed and versioned. Define active versus inactive validation and exact data
dependency resolution. Design the result/archive envelope's versioning separately.

Exit: domain checks prove units, null/absence/zero distinctions, stable IDs,
membership constraints, law-specific geography, inactive-data retention and
scenario-only JSON/gzip roundtrips. Unsupported versions/pairs fail explicitly.

### CP-03 — Adapt existing execution paths

Resolve D08/D10/D17 and implement one preparation/generation/assembly dispatch
boundary. First adapt each existing workflow without altering numerical or
legal kernels, RNG streams, order-sensitive candidate IDs or tie behavior.
Normalize common input encoding at the adapter boundary. Implement additional
generator/law pairs only if D02 includes them, with explicit geography and
candidate/legal assembly contracts and their own acceptance cases.

Exit: seeded comparisons match CP-01 votes, candidate placements, seats, ordering
and diagnostics for every existing workflow. Compare semantic output with only
documented envelope/provenance changes excluded. Existing expected failures
remain failures. Worker validation uses the same supported-pair registry.

### CP-04 — Replace authored-default construction

Resolve D03/D04. Create the single authored JSON with complete baseline law/model
blocks; make preparation, browser startup, reset and release packaging consume
it. Replace scripts that independently choose scenario values with validation
or deterministic packaging. Regenerate affected scenario descriptors/catalogues
from the complete production pipeline. Remove obsolete default compilers only
after tracing every active consumer, including regional preparation.

Exit: clean preparation produces all required production assets; validation
detects missing/incompatible pins; one authored input determines every default
choice. Tests distinguish an approved new default from parity of imported old
scenarios. No generated default is edited by hand.

### CP-05 — Migrate files and saved drafts

Resolve D12/D13 and implement explicit versioned migrations for approved old
formats. Preserve unsupported/non-factorizable settings where required, emit
conversion reports, and never claim a lossy migration is equivalent. Route
scenario/result imports through metadata without reading a complete large
archive. Keep regional import routing and old result evidence valid.

Exit: representative old files and local drafts migrate without source deletion;
conflicts/incomplete candidates are visible; rejected versions leave the current
draft intact. Archived results retain their executed scenario/model identity
and roundtrip without being relabelled as a new run.

### CP-06 — Build the common editor

Resolve D07/D10/D14/D16/D18. Compose a single politics editor from subject,
reference, membership, filing/candidate, model and run-control components.
Drive visibility/applicability through capabilities and use pure domain edits;
use Svelte 5 `$state` and `$derived`, with lifecycle hooks or event handlers for
owned tasks. Keep expensive trees lazy. Automatically initialize model loading
and expose relevant fit/checkpoint controls without a load-default-model button.

Exit: law/model/chamber switching retains shared and inactive values, selected
candidate pins and tree declarations. Tri-state filings, disabled candidates,
new-component transitions, references, reset/import/export and error paths work.
Check keyboard/mobile layout and run the Svelte tooling on changed components.

### CP-07 — Unify lifecycle, persistence and results

Integrate the editor with one draft/controller and the execution adapters.
Implement agreed switching/cancellation, dependency-based fit reuse, concrete
random seeds, requested/effective worker settings and safe disposal. Reuse
existing durable result/session and streaming export services; keep result
tables/charts in tested presentation helpers. Preserve the immutable executed
scenario and stale-result indication across subsequent edits and selections.

Exit: delayed replies cannot update a newer revision; cosmetic/inactive edits do
not refit; relevant edits invalidate the correct stages. Reload, cancellation,
checkpoint restore, failures, stored queries and archive import/export retain
correct simulation membership, counts, seeds and original requested denominator.

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

After parity and compatibility checks pass, remove superseded politics workspace
components, route branches, draft writers and default logic. Keep required
migrations/readers and independent regional functionality. Update current
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
