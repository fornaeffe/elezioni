# Common politics workspace, scenario and UI

Design proposal for the open workspace-merger item in
[MIGRATION_PLAN.md](../../MIGRATION_PLAN.md). This document defines the target;
the [CP-02 contract guide](contract.md) and [CP-03 execution guide](execution.md)
record the implemented domain and execution boundaries. Workspace integration
remains in the later steps. The
[decision register](#decision-register) separates approved decisions from open
questions. The [page draft](politics-page-draft.svelte) is approved with the
changes recorded here; component boundaries remain implementation choices.

| Task | Read |
| --- | --- |
| Understand scope and supported selections | [Scope and compatibility](#scope-and-compatibility) |
| Design schemas, parsers or adapters | [Common scenario contract](#common-scenario-contract) |
| Use the implemented domain API (CP-02) | [Contract guide](contract.md) |
| Build the editor and controls | [UI behavior](#ui-behavior) |
| Integrate workers, fitting and results | [Architecture and lifecycle](#architecture-and-lifecycle) |
| Review approved decisions or resolve open choices | [Decision register](#decision-register) |
| Implement and verify the merger | [Implementation plan](#implementation-plan) |
| Map existing fields, controls and assets before changing a boundary | [CP-01 inventory](inventory.md), [comparison baselines](baselines.md) |

## Scope and compatibility

Provide one `/politics` workspace, one editable scenario and independent selects
for vote generator and scrutiny algorithm. Changing a selection changes the
active capabilities and execution adapter; it must not open a separate draft,
reset shared inputs, or discard settings for an inactive model or law.

Priorities, in order: strict adherence to election law and accepted
interpretations with the legal core unchanged; a clean, simple, rational and
uniform architecture and UI; equivalent results where practical. Exact seeded
parity with the previous workspace is not a requirement. Preserve the model
definitions of retained models, candidate-generation policies and applicable
scenario inputs, except for the approved M00 retirement below. D14 distinguishes
editors required during migration from editors deferred until after migration;
preserving an input does not require exposing its editor now. Document
execution changes rather than retaining complexity solely to reproduce
old random draws. Keep law text close to scrutiny code.
Regional elections remain outside this implementation, but reusable scenario
primitives, controls and lifecycle services
should accommodate them without assuming every election has two chambers.
Municipal simulation remains deferred.

Current entry points are recorded in [web workspaces](../web-workspaces.md).
Today, the previous-law workspace uses `Scenario` and its original generator;
the reform workspace uses `ReleasedScenario` v5 with
[M01 2.0.0](../models/M01.md) or [M02 2.0.0](../models/M02.md).
The original previous-law generator is referred to as **M00** only to identify
the implementation being retired. Replace it with M01 for Rosatellum; do not
register M00 as an active generator. This intentionally replaces M00's turnout,
unmapped-support, poll, reference and uncertainty semantics with M01's. It does
not assert better predictive performance or require M00 distribution parity.
Preserve Rosatellum's candidature policies and legal core, and frozen historical
regression evidence.
The new default is **reform + M02 + joint renewal**.

| Scrutiny | Generator | Required target workflow |
| --- | --- | --- |
| Rosatellum, `politiche-r-parity-v1` | M01 | Shared statistical model with a validated Rosatellum ballot/geography adapter |
| Reform, `politiche-reform-2026-v1` | M01 | Preserve the current M01 statistical specification through the reform adapter |
| Reform, `politiche-reform-2026-v1` | M02 | Preserve the current M02 statistical specification through the reform adapter |

### Generator identity and extensibility

Use separate generator and scrutiny registries plus an explicit supported-pair
matrix, enforced by UI and workers. Allow new generators for either law and
reuse the same generator identity across laws when its statistical specification
is shared. Rosatellum + M02 is outside the initial implementation. New pairs
require declared capabilities, data dependencies, validated adapters and their
own acceptance cases; independent selectors do not imply universal compatibility.

Keep the statistical model separate from law-specific electorate, geography,
filings and ballot relationships, then from candidature/legal assembly and
scrutiny. Law context can affect generated distributions. Reuse does not promise
identical cross-law votes or reusable preparation. Use a law-specific model
identity such as `M01-rosatellum` only if assumptions, reference meanings or
stochastic behavior require a genuinely different statistical specification;
geography and scrutiny differences alone do not justify duplicating models.

Only current supported model versions are selectable/executable, with exact
version checks. Keep probability-law versions independent of scrutiny, adapter,
data, schema, numerical/RNG and implementation identities. An internal refactor
preserving the published contract does not itself change the model version.
An optional capability preserving existing configurations requires MINOR;
changes to existing statistical meanings/behavior require MAJOR. PATCH covers
clarifications or implementation corrections to the same specified law. CP-03 must
determine the version impact of cross-law support rather than assume M01 remains
at 2.0.0. Apply the resulting exact supported version consistently to both pairs.
Keep D12's exact-version import/checkpoint rules; viewing results from retired
versions is a separate compatibility decision, not permission to execute them.

### Selection transitions

Store one selected generator and exact version for each law in the scenario.
Initialize new scenarios with M01 for Rosatellum and M02 for reform; these are
creation defaults, not switching defaults. Changing the generator updates only
the selected law's saved choice. Switching law activates that law's saved choice
atomically, without changing either law's choice. For example, selecting M01
under reform, switching to Rosatellum, then back to reform restores M01.

Preserve shared inputs, inactive settings and all per-law selections through
switching, draft persistence and scenario import/export. Derive the active
generator from the selected law's entry rather than storing a second active
choice. Show the resulting selection and explain unavailable options in Italian.
Honor valid loaded choices; missing or unsupported law/model/version entries
are validation errors, not requests to restore defaults or substitute another
model. Never fall back silently during import or execution. D16's active
execution lock and explicit cancellation still apply.

## Common scenario contract

### Ownership and versioning

Introduce a distinct JSON contract, provisionally `kind: 'politics-scenario'`,
`schemaVersion: 1`. Its version is independent of the old scenario schemas,
model-law versions, scrutiny versions, numerical policies, RNG streams, data
versions and implementation hashes. The paths below describe the target;
the [CP-02 contract guide](contract.md) maps them to the authoritative runtime schema and inferred TypeScript type.

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
| `selection.generatorsByLaw[lawId]` | Saved generator ID and exact supported probability-law version for each supported law; default M01 for Rosatellum and M02 for reform. Derive the active generator from `selection.scrutiny`; do not duplicate an active choice |
| `data` | A pinned data manifest, or equivalent exact bundle descriptors: historical source/catalogue, geography, electorate policy, and per-law seat/model assets, with versions and checksums. A historical-data version alone cannot identify all these dependencies |
| `history` | Selected elections, paired Camera/Senate sources, selected polls, exclusions with reasons, source-membership edits and whole-component pooling declarations. Preserve provenance and honor source availability metadata in the pinned assets |
| `subjects` | Stable coalition/list identities and shared presentation; each list has one coalition affiliation and minority declaration across chambers |
| `memberships` | Explicit Camera/Senato future list-to-component sets, referencing historical catalogue IDs or `newComponents` IDs; retain each list's linked-editing state as editor metadata |
| `newComponents` | Scenario-owned identities and explicit fallback/support assumptions for lists without historical components; no copied historical definitions |
| `references`, `nonValidReferences` | National/local, common/chamber-specific reference declarations with explicit units and model applicability |
| `candidates` | Shared declared candidate identities, biography and availability; placements and external candidacy declarations belong to the applicable law |
| `lawSettings[lawId]` | Law-specific filings, candidate placements, external candidacy declarations referencing shared candidate IDs, candidature-generation settings, links, legal declarations and supplied legal inputs |
| `modelSettings[generatorId]` | Model-specific compositions, partial observations, uncertainty controls, profiles, numerical/RNG policy versions and other settings without a sound shared meaning |
| `execution` | Requested simulation count, fitting seed, simulation seed and requested compute-worker count or explicit automatic policy |
| `provenance` | Default origin, source hashes or manifest references, assumptions and warnings about approximations; no old-format migration metadata is required |

Every registered model/law block has its own versioned schema. Only the selected
blocks affect execution; inactive blocks survive import, export and switching.
Do not duplicate a shared value in those blocks merely to suit an old adapter.
Keep a model-specific field where existing controls have no equivalent shared
meaning, and document its precedence. Preserve retained capabilities; M00-only
controls are retired rather than translated. Old file formats remain unsupported.

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
- Each coalition has one shared name; each list has one shared name, coalition
  affiliation and minority declaration. Do not add chamber overrides for those
  fields. Represent differing names, affiliations or minority declarations by
  distinct lists, filed in their respective chambers. Equal names never imply
  equal identities. Interpret the shared minority declaration under the selected
  law without changing eligibility rules or conflating non-equivalent old inputs.
- A list participates in a chamber through its per-law filings, not a separate
  list-presence toggle. File it only in Camera or only in Senato to declare
  chamber-only participation. Retain coalition chamber restrictions and saved
  inactive filings; a coalition or list may exist in the editor without effective
  participation.
- Autonomous lists have a null coalition. Singleton candidate tickets needed
  by the previous-law adapter are derived execution objects, never additional
  political coalitions in the scenario or editor.
- Future memberships are set membership, scoped by chamber. Within that scope,
  a component cannot simultaneously belong to conflicting future lists.
  The same historical component may feed different chamber-only lists; this is
  not a scenario-wide uniqueness constraint. Save both chamber sets even when
  their list is unfiled or its chamber is not being renewed.
  Historical-source memberships are scoped by source/question/choice, with
  stable IDs and provenance; M01 fractional compositions are a separate model
  input, not fractional future membership in either model or invented observed
  counts. Use M01's normalized historical compositions and binary future
  memberships for Rosatellum too. Keep composition provenance and explicit
  equal-allocation assumptions; no arbitrary M00-factor coexistence or conversion
  is required under D08/D12.
- Poll-option “other” is the complement of the other offered options for that
  question. It is distinct from the future residual list kind `other`, a new
  component, and non-valid votes. Retain the existing residual-list controls.
- A new list needs a scenario-owned component in each chamber where it has no
  historical members. Derive that need per chamber, retaining a stable component
  identity and saved fallback assumptions. Do not display an artificial historical
  component in the membership editor. Adding/removing historical membership in
  one chamber must not erase the other chamber's support or create duplicate
  active components.
- Preserve historical selection, poll editing/additions/exclusions, pooling,
  fallback assumptions and applicable retained-model inputs. Follow
  [D14's editor scope](#history-and-advanced-controls-d14) for UI delivery;
  deferred editors do not remove scenario fields or their execution semantics.
  User-added polls are explicit scenario observations with citation/method/availability metadata, not copies
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
uncertainty is not silently zero; use the approved D06 policy below.
Reject duplicate/conflicting rows and explain normalization
or overlap treatment using the selected model's existing rules. Keep common
and chamber-specific declarations distinct. Do not replace current geographic
precedence, chamber resolution or stochastic reference semantics with a new
universal rule during this merger. The [CP-02 encoding](contract.md#ownership-and-encoding) specifies nullable fields explicitly:
where allowed, null means unspecified/inherit, never zero. Empty collections
represent explicit empty declarations, not a request to restore defaults.

Shared encoding does not imply shared statistical meaning:

| Generator | Semantics to retain |
| --- | --- |
| M01 (current specification: 2.0.0) | Original historical compositions, named list polls and list shocks; list/singleton-component references; reference SDs and M02 uncertainty multipliers do not acquire an effect. Apply these semantics to Rosatellum too, subject to explicit version assessment |
| M02 (current specification: 2.0.0) | Grouped-source fitting, identifiable component basis, physical filed sets and calibrated reference uncertainty; preserve existing reference admissibility and movement policies |

Use [the current model boundary](../vote-generation-unified-workflow.md#model-boundary-and-restored-semantics)
for detailed semantics. Use **“Non valido / astensione”** for both laws, explained
as abstentions plus invalid ballots, with an elector denominator. Do not call the combined quantity simply
“astensione”. Apply the selected retained model's unmapped-support policy;
M00's unmapped-to-non-valid treatment is retired. Do not turn unmapped support
into zero, add raking, reinterpret reference dates, or introduce fixed
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

For mean $m$ expressed as a fraction, use

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
policy version in scenario provenance. M01 retains stored SDs while ignoring
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

#### Declared candidate identities (D09)

Declared candidate identities are shared across laws and chambers and contain
biography and availability. External candidacy declarations belong to the
applicable law block and reference the shared candidate ID. A placement refers
to a candidate ID, law, chamber, owner, role, location and ordered slot. Ordinary,
bonus, special and previous-law uninominal roles retain their different legal
requirements. Political affiliation belongs to the relevant candidature context;
an independent uninominal candidate must remain representable.

Use stable IDs, never names, to identify people. Creating a new candidate always
creates a separate identity, even when its name or biography matches another
candidate. Reuse requires explicitly selecting an existing identity from the
select/combobox; never merge or deduplicate automatically. Distinguish equal-name
options with available biography and a stable ID where needed.

Require a nonblank name and an explicitly selected gender before saving a new
identity. Do not preselect or infer gender. A blank birth date uses January 1 of
the election year minus 50, following the reform generation convention. Resolve
and persist the concrete date when creating the identity, mark it as assumed,
and allow editing. Switching laws or changing the election date must not
recalculate it. Reject malformed supplied dates instead of replacing them with
the default. Explain that an assumed date affects age validation and applicable
tie-breaks. This convention for declared identities does not change either law's
generated-candidate policy; generated candidates remain runtime objects.

Editing shared biography or availability propagates to every placement using
that identity, including inactive laws and chambers. Deleting an identity removes
all its candidate placements and external candidacy declarations across laws and
chambers, including inactive ones, without removing the lists' own filings.
Removing one placement retains the identity and its other placements; retain
identities without placements for later reuse. Removing a placement removes its
manual pin: handle the vacant slot through the existing generation policy on the
next run, without silently shifting other pinned positions. These draft edits
never alter immutable executed scenarios or result sessions.

Unfiling a branch, disabling a chamber or restricting coalition presence makes
dependent placements inactive without erasing them. Validate shared identity
data structurally throughout the scenario. Apply candidature compatibility rules
only to the selected law's effective placements and applicable external
declarations; filings under alternative laws are not simultaneous candidacies.
Active placements must obey
age, availability, cross-chamber, ordinary-filing, bonus-eligibility and exclusivity
rules already implemented for the selected law. Block execution on violations,
leaving declarations editable for correction. Old templates are not an accepted
import format.

Keep candidate-generation settings per law where they differ. The previous-law
uninominal-to-plurinominal reuse share and five-bin candidature-count distribution
cannot be replaced losslessly by the reform's reuse probability and maximum
ordinary filings. Preserve both controls and existing generation policies;
no new candidate-generation policy is wanted. Missing
slot pins continue to allow generation under the current policy; explicitly
pinned candidates continue to constrain it.

#### Reform bonus links (D11)

Reform reciprocal bonus links apply to eligible coalition/autonomous-list
subjects. A coalition's declared chamber scope determines its link eligibility,
independently of member-list filings:

| Coalition declaration | Bonus-link behavior |
| --- | --- |
| Camera only | Allow an explicit reciprocal link to an eligible Senate subject |
| Senato only | Allow an explicit reciprocal link to an eligible Camera subject |
| Both chambers | Automatically link its two chamber counterparts; prohibit a competing explicit link, including selection as another subject's explicit counterpart |

Member-list effective filings must respect the coalition's declared scope.
Removing the last filed member in a chamber never changes that scope, breaks
its declared automatic relationship or makes it eligible for another link.
Keep an empty counterpart's declared relationship intact; handle absent
participating lists separately during execution validation/assembly, without
inventing participation or substituting another counterpart.

Expose explicit declarations in coalition/list advanced options. An eligible
counterpart may be a coalition declared only in the opposite chamber or an
autonomous list filed only in that chamber; do not require matching IDs, names
or subject kinds. Autonomous-list participation is determined by its filings;
a list filed in both chambers has the automatic link and cannot have a competing
explicit counterpart. A list within a coalition uses that coalition's bonus
link, not a separate list-level link.

Persist only explicit reciprocal declarations in the reform law block: one
Camera–Senato pair with typed subject references (`kind` and stable `id`). A pair
represents mutual agreement; do not save independent directional choices.
Derive automatic links from coalition scope or autonomous-list filings, without
serializing a duplicate automatic-link declaration. Derive status and reasons
(automatic, explicit active, explicit retained/inactive); do not persist them or
an additional activation flag.

Scope, filing and affiliation edits preserve each explicit pair and its original
chamber endpoints. If either endpoint becomes ineligible, retain the pair as
inactive. Never transfer a list's declaration to its coalition, substitute a
counterpart or reverse the pair when subjects change chambers. Automatically
reactivate the original pair when both endpoints become eligible again. Explain
this consequence during editing and show the resulting status. Automatic links
always exclude retained competing declarations from effective links.

Allow at most one saved explicit pair per chamber-specific endpoint, including
inactive pairs. Selecting a new counterpart explicitly replaces saved pairs
involving either endpoint; explain the replacements in the control. Never
resolve conflicts by array order. Deleting a subject removes its explicit pairs;
recreating its name under a new ID cannot restore them.

| Edit | Saved declaration | Effective relationship |
| --- | --- | --- |
| Camera-only coalition A links to Senate-only autonomous list B | A–B | A–B |
| A becomes a both-chamber coalition | Retain A–B, inactive | Automatic A–A only |
| A returns to Camera-only scope | Keep A–B | A–B resumes if B is eligible |
| B joins coalition C | Retain A–B, inactive | No A–B; do not substitute C |
| A loses its last Camera member | Keep A–B; declared scope still governs eligibility | Validate absent participation separately |
| A changes from Camera-only to Senate-only | Retain the original Camera A–Senato B pair, inactive | No automatic reversal; a reversed pair requires explicit selection |
| Either endpoint is deleted | Remove A–B | No relationship |

Show retained declarations in advanced options with their counterpart, inactive
reason and remove/replace actions, even when the ordinary selector is unavailable.
Replacement still requires eligible endpoints. Reject malformed imports, unknown
subject IDs, duplicate pairs and overlapping saved endpoints, including inactive
ones. Accept structurally valid declarations that are currently ineligible.

Use one shared resolver for validation, UI and assembly. Materialize only
automatic and eligible explicit links at the adapter boundary; retained inactive
declarations never enter scrutiny input. Single-chamber renewal preserves saved
declarations but supplies no joint bonus links. Keep an empty coalition's
automatic relationship intact and address absent participation separately during
execution validation, without inventing lists or substituting a counterpart.

### Run settings and serialization

Persist requested simulation count, fitting seed, simulation seed and worker
preference in the scenario. Keep requested and actual worker counts separate:
actual capacity, effective seed, runtime versions/hashes and timings belong in
the executed run record. Automatic worker choice must be represented explicitly,
not inferred from a missing value. Different pool widths must not change global
simulation IDs, RNG streams or deterministic output.

Default to 10 simulations and an explicit automatic worker policy using the
model-aware capacity recommendation. After a successful, completed 10-simulation
run, recommend increasing the count to 1,000 to obtain meaningful results. Keep
this as a visible recommendation; do not change the draft count or start another
run automatically. Failed, cancelled or partial runs do not trigger it.

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
with one versioned, validated authored JSON file,
`data/politics-default-scenario.json`. Include all shared declarations and complete
blocks for the three baseline workflows, exact data references, both seeds and
run preferences. Store the initial per-law generator choices (Rosatellum M01,
reform M02), and select reform + joint renewal (D02/D03 approved).

#### Approved default values (D04)

The existing source/seat/geography manifest and existing default values are
approved wherever shared, including dates, references, memberships, filings,
candidate settings and seeds. Where existing defaults differ and have equivalent
meaning, use the reform M02 default. Preserve law/model-specific meanings and
geographic namespaces; do not copy the reform's Bolzano overlay into Rosatellum.
Materialize the exact approved dependencies, versions and checksums rather than
resolving to whichever bundle is latest.

Use the existing authored choice set in
[`fast-share-future-scenario.json`](../../data/fast-share-future-scenario.json),
M02 policy in
[`fast-share-default-policy.json`](../../data/fast-share-default-policy.json)
and released M02 scenario as the baseline to consolidate into the new file.
Retain selected/excluded history and polls, source compositions and their
provenance, electorate and fallback assumptions, and existing empty manual
candidate/supplied-input declarations. Replace the old Rosatellum compiler's
independent shared reference choices with this baseline. Preserve distinct
Camera/Senate reference values and all explicit SDs, including zero. Apply the
separately approved D06 uncertainty policy to new or edited references and D09
to declared candidate identities. Apply D14's approved editor scope below.

| Setting | Approved authored value |
| --- | --- |
| Scenario ID | `politics-default-v1` |
| Display name | `Politiche — scenario nazionale` |
| Election date | `2027-09-25` |
| Reference date | `2026-10-19` |
| Fitting seed | `fast-share-default-fit-v1` |
| Simulation seed | `fast-share-default-v1` |
| Requested simulations | `10`; recommend `1000` after a successful 10-simulation run as specified above |
| Worker preference | Explicit automatic policy using the model-aware capacity recommendation; actual count belongs to execution provenance |
| Rosatellum candidate generation | Preserve `uninominalToPlurinominalShare = 0.35` and `plurinominalCandidacyCountShares = [0.875, 0.0625, 0.025, 0.0125, 0.025]` in its law block |
| Reform candidate generation | Preserve `reuseProbability = 0.2`, `maximumOrdinaryCandidacies = 5`, `bonusPolicy = reference-proportional-v1` in its law block |
| M02 implicit settings made explicit | National/local uncertainty multipliers `1`; `forecastReference = conditional`. Preserve the current effective behavior |
| Other M02 forecast settings | Preserve `forecast = bounded-mass-pair-field-v2`, `forecastScale = 1.5`, `profileRegionCorrelation = 0.25` |

#### Approved subject colors (D04)

Include the separate party-color task from `MIGRATION_PLAN.md` in CP-04. Store
each subject's color once and retain it across chambers, laws and generators.
These are approved application colors inspired by political presentation;
composite and scenario-specific tickets use editorial choices, not claimed
official brand codes. Default list colors must be explicit rather than derived
from an ID hash or list order.

| Subject ID | Color |
| --- | --- |
| Coalition `centrodestra` | `#2563EB` |
| Coalition `centrosinistra` | `#DC2626` |
| List `fdi` | `#003366` |
| List `fi` | `#0088CC` |
| List `lega` | `#008040` |
| List `noi-moderati` | `#2456A6` |
| List `udc` | `#5DADE2` |
| List `sud-chiama-nord` | `#F28C28` |
| List `cdx-vda` | `#315A8A` |
| List `pd` | `#D71920` |
| List `m5s` | `#F5C400` |
| List `avs` | `#4A9F38` |
| List `italia-viva` | `#D6348A` |
| List `europa-piu-uno-cd` | `#E5B700` |
| List `progetto-civico-psi` | `#E65B50` |
| List `csx-vda` | `#B83B45` |
| List `azione-libdem` | `#0066A1` |
| List `futuro-nazionale` | `#172B50` |
| List `pap-prc` | `#B5121B` |
| List `ora` | `#FFE000` |
| List `dsp` | `#800020` |
| List `uv` | `#A6192E` |
| List `svp-patt` | `#252525` |
| List `team-k` | `#F2B800` |
| List `freiheitlichen` | `#1976BC` |
| List `suedtiroler-freiheit` | `#D52B2E` |

#### Preparation boundary

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
   explicit reform links for coalitions declared in one chamber only. Coalitions
   declared in both chambers are automatically linked and excluded from explicit
   counterpart choices, regardless of member-list filings. Offer eligible
   coalition/autonomous-list counterparts. Show automatic/explicit active/retained
   inactive status and reasons using the shared resolver. Retained pairs remain
   visible with remove/replace actions when the ordinary selector is unavailable;
   explain automatic reactivation and any pairs replaced by a new selection.
   Use the D11 persistence rules above. Keep collapsed candidate trees for reform
   bonus slates or previous-law uninominal slots. Candidate choices show list affiliation where relevant,
   reasons for ineligibility, creation and editing actions.
3. **Lists:** compact color/name/coalition and optional national reference.
   Advanced sections hold applicable uncertainty, filings/candidates, links,
   a shared minority declaration and local references. Name and affiliation are
   shared too; use distinct lists for chamber differences in those declarations.
   Declare chamber-only participation in the filing tree. Show explicit reform
   bonus links for autonomous lists filed in one chamber only, with eligible
   coalition/autonomous-list counterparts. Apply the same D11 retained-pair
   visibility, status and remove/replace behavior as coalitions, including after
   affiliation or filing edits make the list ineligible. Retain common/chamber
   reference controls and show a badge for differing reference values.
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
6. **Non-valid support:** immediately below the list editors, show a basic,
   optional **“Non valido / astensione”** national common reference input,
   visible by default. Attach a collapsed advanced section for national
   uncertainty, per-chamber national references and applicable uncertainty,
   and a region → province → municipality tree for common/per-chamber local
   references and applicable uncertainty. Reuse list reference-tree behavior,
   including inherited values, deletion and badges. Apply D06's elector
   denominator, defaults, feasibility checks and absence-versus-zero rules.
7. **Advanced scenario:** generator selector; collapsed source/future membership
   editor, pooling editor and fallback/component-reference controls under
   [D14's scope](#history-and-advanced-controls-d14). Future lists are the initial
   membership view; use the [future-membership editing rules below](#future-membership-editing).
   Fractions appear only for applicable M01 source compositions.
   Hide poll “other” members and derive the complement. Disable components used
   by conflicting choices within the same source; retain existing valid source
   grouping/pooling semantics. Include a component-reference editor with the same national/local
   and common/chamber controls as lists. New components' references are active
   only in chambers where their lists lack historical memberships. Show pinned
   electorate data read-only. Defer historical/poll selection and observation
   editors, supplied-input editors and model/profile editors until after migration;
   those settings remain preserved without requiring UI editability now.
   Do not add an embedded raw JSON editor.
8. **Simulation controls:** count and Run; collapsed fitting/simulation seed
   inputs, each with its own “Genera seed casuale” button, and worker count.
   Default to 10 simulations and automatic workers. After a successful
   10-simulation run, visibly recommend increasing the count to 1,000 for
   meaningful results; leave changing the count and starting the run to the user.
   Load the default/imported/selected model automatically,
   reusing valid preparation or computing only required work. No separate “load
   default model” action. Keep fitting progress, diagnostics, cancellation and
   portable checkpoint access available where applicable.
9. **Results and status:** one session list across all three workflows, optional
   filters and law-specific views supplied by adapters. Follow
   [result-session selection](#result-sessions-and-resources); browsing results
   never implicitly replaces the draft. Use a small fixed, collapsible status box
   near the top right, avoiding controls and mobile content; include state, progress and
   applicable Cancel. Keep detailed diagnostics accessible and distinguish
   informational notes, warnings and errors by severity.

For newly created subjects, initialize both chambers and every supported law;
a new list is filed in every eligible college and starts without historical
members. Default lists retain their authored filings for all laws. A coalition's
presence limits effective member-list filings while preserving inactive
declarations. Creation defaults never overwrite explicitly saved declarations.
Keep unrelated metric/export changes separate from this approved UI scope.

All hidden sections remain stored. Showing a section again restores its values.
Show only applicable controls, with a concise explanation of the active model;
reveal saved declarations or active overrides in collapsed summaries. Deferred
editors remain absent until their post-migration tasks are implemented.
Use lazy tree expansion/search/paging so national candidate and municipality
trees do not mount in full. Keep keyboard operation, labelled controls, visible
focus and accessible partial-selection/status announcements.

### History and advanced controls (D14)

This is the approved editor perimeter for the merger. Preserve every retained
input in the new-format contract, validation, defaults, execution and roundtrips,
including inputs without an editor. Scenario download/edit/upload remains
available; no embedded raw JSON editor or UI-editability acceptance gate is
required for deferred settings. Deferred tasks are tracked at the end of
[the post-migration plan](../../MIGRATION_PLAN.md#deferred-common-politics-editors-d14).

| Concern | Migration delivery |
| --- | --- |
| Historical sources and polls | Defer source/poll additions, edits, selection and exclusions editors. Preserve citations, methods, availability metadata, exclusion reasons and scenario-owned observations; bundled observations stay unchanged |
| Model-specific historical selection | Defer selection editors. Preserve M01's automatic latest eligible Camera/Senato pair and eligible Camera polls, and M02's selected elections, paired sources and supported poll contexts; do not impose one model's selection rules on the other |
| Historical membership and M01 composition | Keep the already approved source/option membership editor, M01-only source compositions, provenance and conflict rules. This edits membership declarations, not source selection or observation values |
| Pooling | Implement the proposed M02 whole-component pooling editor because the current default uses pooling, including `historical-rest`. Place “Raggruppamento dei componenti” alongside membership controls; select whole original components, inspect the resulting basis and remove declarations. Keep aliases, future memberships and M01 fractional compositions distinct. An unidentified basis fails with an actionable error, never invented counts or automatic splitting. CP-01 verifies default usage; if the default no longer uses pooling before implementation, defer this editor to the post-migration plan instead |
| Fallback and component references | Implement “Ipotesi senza storico” beside new-component references, with applicable parameters, units, effective values and provenance. Expose M02 geographic log-profile, profile SD and local/national innovation SDs. Explain M01's existing regional/equal-list fallback without assigning M02 parameters an M01 effect. Retain inactive assumptions when memberships change; use the same national/local and common/chamber reference controls as lists |
| Non-valid support | Implement the basic visible input and attached advanced section described above. A blank reference inherits the active model; zero is explicit. M01 ignores stored reference SDs, while M02 consumes them under D06 |
| Electorate | Show pinned source/date, chamber totals and reconstruction/conditional assumptions under advanced settings, with lazy geographic detail where useful. Do not introduce electorate editing |
| Supplied votes and preferences | Defer editors. Preserve law-specific supported representations, replacement semantics, validity evidence, provenance and missing-versus-zero distinctions. Reform declarations stay inactive and retained under Rosatellum; do not add Rosatellum override capabilities |
| Model settings and profiles | Defer editors, including M01 partial named observations and preference/chamber-profile selection. Preserve forecast submethods, scale, profile correlation, reference mode and other retained settings without changing their meaning. This deferral does not remove the generator selector, D06 reference/uncertainty controls, fallback editor, candidature-generation controls or D15 seeds/workers. Versions remain provenance, not freely editable strings |

Use labelled structured forms, searchable/paged selectors and explicit
remove/restore actions for the editors delivered now. Preserve inactive values
through switching, reload and scenario import/export; collapsing never resets
values. M00-only controls remain retired under D01/D08.

### Future membership editing

Each future list entry shows labelled **Camera** and **Senato** membership lists,
side by side when space permits and stacked on narrow screens. Each contains
the draft's existing member names, remove actions, add-component selector and
add confirmation; memberships remain binary under both models. Historical-source
editing retains its existing single source/option view and M01-only fractions.

Give every future list its own accessible lock/chain toggle, initially locked
for new entries. Its behavior is independent of chamber renewal and filings:

| Action/state | Required behavior |
| --- | --- |
| Locked | Camera is editable; Senato is visible and read-only, inheriting Camera's membership set. Each Camera add/remove updates both sets atomically |
| Unlock | Retain both sets unchanged and enable independent editing of each chamber |
| Unlocked edit | Change only the edited chamber; retain the other set, including when that chamber is inactive or unfiled |
| Lock again | Replace the entire Senato membership set with Camera's, including removals or an empty set. Do not merge sets or copy in the other direction |

Validate shared edits and re-locking against conflicts in both chambers before
applying them. Disable conflicting additions with a reason; if a re-lock would
conflict with another list, keep the pair unlocked and report the conflict
without partially changing either set or moving another list's components.
Keep current-member exclusion and same-source conflict disabling in historical
editors; future-list conflicts are chamber-scoped, never global across chambers.
Retain hidden scenario-owned fallback assumptions when membership changes.

Persist the per-list lock state with the editable scenario as editor metadata;
it is not a statistical parameter. Locked sets must agree. Loading/switching
must restore saved sets and lock state, never apply the new-entry default over
existing data or silently overwrite differing loaded sets. Changing lock state
alone does not change model inputs; copying changed memberships does.

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
services where sound. Extract shared statistical generation interfaces from the
reform namespace and separate its current reform-assembled simulation return
from generation; do not require the previous law to construct a fake
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

### Result sessions and resources

All three supported pairs use durable chunked results, deterministic multiworker
execution, bounded processing, lazy queries and streaming exports. Remove the
Rosatellum-specific 1,000-run ceiling; retain validated numeric/resource bounds
rather than promise unlimited execution. Validate requested resources, distinguish
requested/effective workers and calibrate estimates for the Rosatellum adapter.
Keep stable global simulation IDs and RNG streams across pool widths. Preserve
portable checkpoints, diagnostic simulation membership, requested denominators
and durable committed prefixes after interruption. A storage quota failure must
report the interruption and retain committed results.

Keep draft selection and the viewed result-session ID independent. Results use
their immutable executed scenario and exact model, scrutiny, adapter, data,
numerical/RNG and implementation provenance. Never relabel a session after edits.

| Action | Session and draft behavior |
| --- | --- |
| Edit the draft or switch law/model | Keep the viewed session; show its original identity and whether it differs from the draft |
| Start a run | Create and select a new session with an immutable executed scenario |
| Browse a saved session or import a new-format result archive | Select/display the session without replacing the draft |
| Explicitly load a session's scenario | Use the ordinary scenario-loading action to replace the draft after validation |
| Reload | Restore the selected session if available; report an unavailable session without substituting another or replacing the draft |

Use one session list across workflows with optional law/model filters. Identify
each session by law, generator/version, creation time, renewal and
completed/requested runs. Switching law neither deletes results nor automatically
selects a different session. Keep D12's supported-format/exact-version validation
for imports and checkpoints, without automatic upgrades or compatibility promises
for retired model versions. Resource acceptance follows CP-08 and does not
include runs above 1,000 simulations.

## Decision register

### Approved decisions

These decisions are settled. Apply them in the contract, UI and implementation
plan; do not reopen them as implementation gates.

| ID | Approved direction |
| --- | --- |
| D01 | **Generator identity/version:** retire M00 and reuse M01 for Rosatellum through a validated law adapter. Models may serve multiple laws; law-specific identities require different statistical specifications. Use exact current versions and assess version impact as described above; keep scrutiny, adapter, data, schema, numerical/RNG and implementation identities separate |
| D02 | **Combination scope and selection transitions:** require Rosatellum + M01, reform + M01 and reform + M02. Store a selected generator/version for each law in the scenario, initially M01 for Rosatellum and M02 for reform. Model changes update only that law's choice; law changes activate its saved choice. Preserve all choices and inactive settings; reject missing/unsupported choices without substitution |
| D03 | **Initial selection:** reform + M02 + joint renewal |
| D04 | **Authored default and data pins:** use `data/politics-default-scenario.json`, ID `politics-default-v1`, name “Politiche — scenario nazionale”, and the approved values/colors above. Existing shared values and the source/seat/geography manifest are approved; prefer reform M02 defaults where equivalent and preserve Rosatellum candidate parameters. Author 10 simulations, explicit auto workers and current implicit M02 settings; recommend 1,000 after a successful 10-simulation run. Include the separate party-color task in CP-04 |
| D05 | **Shared subject declarations:** coalition/list names, list coalition affiliation and minority declaration are shared across chambers. Use distinct lists for differences, and list filings to declare chamber-only participation; no list-presence toggle or chamber overrides for these fields. Preserve stable IDs and inactive declarations |
| D06 | **References:** approve the reference representation, model-specific precedence disclosures, numerical uncertainty defaults, joint feasibility checks and persistence/provenance policy above. Preserve each model's interpretation and explicit SDs; use the corrected “Non valido / astensione” label |
| D07 | **Filings for all laws:** initialize every law block for new subjects; a new list is filed everywhere eligible in both chambers. Default lists have authored default filings for every law. Defer the conversion helper. Use tri-state checkboxes without redundant partial-coverage badges; retain candidate badges |
| D08 | **Correspondence controls:** use M01 historical compositions with provenance and binary chamber-scoped future memberships for Rosatellum too. Show Camera/Senato membership lists per future list, side by side when possible, with a default-locked chain toggle: Camera edits both while locked; unlocking enables independent edits; re-locking replaces Senato with Camera. Follow the future-membership editing rules above. Retire M00's arbitrary-factor controls; no coexistence or old-format conversion. M02 memberships remain non-fractional |
| D09 | **Candidate identity and incomplete declarations:** approve the declared-candidate rules above: shared identity, biography and availability; law-specific placements and external candidacies; explicit reuse without automatic merging; propagated edits and identity deletion; independent placement removal with retained identities and existing slot generation. Require name and explicit gender; persist an editable, labelled assumed birth date using January 1 of election year minus 50 when absent. Validate effective candidatures under the selected law; preserve immutable executed snapshots |
| D10 | **Candidate controls:** preserve both law-specific generation parameterizations in their respective law blocks, independent candidates, availability/external candidacies and manual pin behavior. No new candidate-generation policy |
| D11 | **Bonus-link persistence:** apply the eligibility and persistence rules above. Derive automatic links; save explicit reciprocal pairs once with typed stable-ID endpoints. Preserve ineligible pairs as inactive and reactivate when eligible; never rebind or reverse them. Enforce one saved pair per chamber endpoint, including inactive pairs; explicit replacement removes affected pairs and subject deletion removes its pairs. Derive visible status/reasons, reject malformed/conflicting imports and materialize only effective links at the adapter boundary |
| D12 | **File compatibility:** no support for old politics scenario/archive schemas; reject them without migration. Use the new common-format draft/session and validate checkpoints against its exact scenario/dependencies |
| D14 | **History and advanced controls:** apply the [approved editor perimeter](#history-and-advanced-controls-d14). Keep source memberships/M01 compositions, fallback/component-reference editors, default-used M02 pooling, basic visible non-valid reference with attached advanced national/chamber/local controls, and read-only advanced electorate data. Defer source/poll and model-specific selection editors, supplied-input editors and model/profile editors until after migration; UI editability of those retained settings is not a merger gate. Preserve inactive values and model-specific meanings; retire M00-only controls |
| D15 | **Seeds and workers:** a “Genera seed casuale” button beside each seed input fills that field visibly. No fit-level or simulation-level randomization mode or automatic seed replacement. Save requested workers/auto policy and record actual workers |
| D16 | **Switching/preparation:** lock during active execution, explicit cancellation, automatic loading/reuse on selection and visible cancellable preparation when required |
| D17 | **Result sessions and resources:** all three pairs use durable bounded storage, deterministic multiworker execution and common session access; remove the Rosatellum 1,000-run ceiling. Draft selection and viewed session remain independent under the session rules above. Acceptance includes no run above 1,000 |
| D18 | **UI scope:** the page draft is approved with the changes in this document, including the status placement. Keep unrelated metric/export changes separate |
| D19 | **Acceptance priorities:** legal core unchanged, with strict adherence to law and accepted interpretations first; clean, simple, rational and uniform architecture/UI next; seek equivalent results, but exact seeded parity is not required. AGENTS.md requirements still hold |

D13 (adopting multiple old drafts) is superseded by D12: do not read or combine
old politics drafts. Start from the new default or an explicitly loaded new-format
scenario and persist under a new common draft key. Leave old browser keys
untouched and unused; deleting old stored data is outside this merger.

### Decision status

No decisions remain open in this register. IDs remain stable after approvals;
D13 is superseded as described above. Apply D14's post-migration editor deferrals
without treating them as missing merger requirements. Old-file migration is
not an option under D12.

Scrutiny version metadata must be explicit in CP-02; generator replacement does
not change existing scrutiny algorithm IDs or legal behavior. D02/D17 settle
the three-pair campaign scope and exclude runs above 1,000. D19 does not waive
the existing performance or legal verification requirements.

## Implementation plan

Implement in dependency order. Each step should leave a reviewable, independently
verified result. Apply approved decisions and resolve remaining choices before
committing to the relevant schema or behavior. Add the agreed steps to the active
migration plan when implementation
starts; record completed changes, rationale, verification and problems in
[migration history](../migration-history.md). Keep the workspace-merger item open
until CP-08 passes. This documentation proposal alone does not complete it.

### CP-01 — Inventory and preserve behavior

Completed evidence: [field/control/asset inventory](inventory.md) and
[reproducible comparison baselines](baselines.md). Verification is recorded in
[migration history](../migration-history.md#cp-01--inventory-and-preserve-behavior-2026-11-07).

Read the current entry points listed below. Build a field/control/asset mapping
from original `Scenario` and reform `ReleasedScenario` to the proposed contract,
including data not visible in the draft. Identify non-equivalent defaults,
M00-only controls to retire, candidate identities and per-chamber values.
Apply D14's editor perimeter and record
baselines for retained M01/M02 behavior and Rosatellum
candidature/legal behavior. Document the intentional M00-to-M01 differences;
do not require M00 distribution parity. Cover
single-chamber runs, restricted/special territories, residual/new
lists, manual candidates, diagnostics and interrupted/exported sessions.

Exit: a complete mapping for the approved pairs, distinguishing preserved inputs,
editors required now and post-migration editors, and reproducible comparison
inputs/results. Separate generator/distribution comparisons from fixed-input legal
regressions. Preserve frozen fixtures/checksum manifests and known legal failures;
never regenerate legal expectations to make the merger pass. Whole-pipeline exact
seeded matching is useful evidence, not a condition that dictates architecture.

### CP-02 — Establish the shared domain contract

Completed domain API: [contract guide](contract.md). Verification is recorded in
[migration history](../migration-history.md#cp-02--establish-the-shared-domain-contract-2026-11-07).

Apply D01/D02/D05/D06/D07/D08/D09/D11/D12/D15/D17.
Implement common types, strict runtime parsing/serialization, independent
registries, capabilities,
effective-presence/link helpers and pure edit operations. Keep law/model blocks
typed and versioned. Define active versus inactive validation and exact data
dependency resolution. Design the result/archive envelope's versioning separately.

Exit: domain checks prove units, null/absence/zero distinctions, stable IDs,
membership constraints and lock-state consistency, law-specific geography, inactive-data retention and
scenario-only JSON/gzip roundtrips, including both per-law generator selections.
Missing selections and unsupported versions/pairs fail explicitly.
Verify D11 explicit-pair retention/reactivation, endpoint uniqueness, replacement,
deletion and strict import/roundtrip behavior; automatic links and derived status
are not serialized. Use one link resolver across validation, UI and assembly.

### CP-03 — Adapt existing execution paths

Completed execution API: [dispatch, adapters and model-version assessment](execution.md).
Verification is recorded in
[migration history](../migration-history.md#cp-03--adapt-existing-execution-paths-2026-11-07).

Apply D01/D02/D08/D10/D11/D17/D19 and implement one preparation/generation/assembly
dispatch boundary. Replace M00 with shared M01 generation and a validated
Rosatellum adapter. Assess/document model-version impact; preserve existing
M01/M02 configurations, candidate-generation policies and the legal core. Prefer
clean shared contracts to preserving incidental old RNG traversal or generated IDs. Keep
reproducibility within the new implementation, legally meaningful order and
accepted tie behavior. Normalize common input encoding at the adapter boundary.
Implement all three D02 pairs with explicit electorate, filing, ballot/geography
and candidate/legal assembly contracts. Other pairs require a separate scope
decision and their own acceptance cases.

Exit: the same fixed legal inputs retain legal outcomes, legally meaningful
ordering and diagnostics. Compare generation, candidature and simulation results
with the retained-behavior baselines in CP-01; explain approved M00 replacement
differences separately, isolate RNG-order changes from semantic regressions,
and verify the model policies/distributions.
Verify that link assembly emits only automatic and eligible explicit pairs,
never competing retained declarations; empty counterparts receive separate
participation validation and single-chamber renewal emits no joint bonus links.
Exact old seeded draws/IDs are not a gate. Existing legal-failure fixtures remain
valid. Worker validation uses the same supported-pair registry.

### CP-04 — Replace authored-default construction

Implemented default API and preparation boundaries: [authored defaults guide](defaults.md).
Verification is recorded in
[migration history](../migration-history.md#cp-04--replace-authored-default-construction-2026-11-08).

Apply D03/D04/D07, including the approved subject palette. Create the single
authored JSON with complete baseline law/model blocks; make preparation, browser
startup, reset and release packaging consume it. Replace scripts that
independently choose scenario values with validation
or deterministic packaging. Regenerate affected scenario descriptors/catalogues
from the complete production pipeline. Remove obsolete default compilers only
after tracing every active consumer, including regional preparation.

Exit: clean preparation produces all required production assets; validation
detects missing/incompatible pins; one authored input determines every default
choice, including default filings under every law. Verify model/algorithm inputs
and approved defaults directly; no old-format import or exact seeded default
parity is required. No generated default is edited by hand.

### CP-05 — Enforce new-format files and draft isolation

Implemented APIs and transitional-browser boundary: [files and drafts guide](files.md).
Verification is recorded in
[migration history](../migration-history.md#cp-05--enforce-new-format-files-and-draft-isolation-2026-11-08).

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

Implemented editor components and review route: [editor guide](editor.md).
Verification and the CP-07 integration boundary are recorded in
[migration history](../migration-history.md#cp-06--build-the-common-editor-2026-11-14).

Apply D02/D06/D07/D08/D09/D10/D14/D15/D16/D17/D18. Compose a single
politics editor from subject, reference, membership, filing/candidate, generator-selection and
run-control components.
Drive visibility/applicability through capabilities and use pure domain edits;
use Svelte 5 `$state` and `$derived`, with lifecycle hooks or event handlers for
owned tasks. Keep expensive trees lazy. Automatically initialize model loading
and expose relevant fit/checkpoint controls without a load-default-model button.

Exit: model changes update only the active law's saved selection; law switching
atomically restores the destination law's selection. Verify reform M01 survives
a switch to Rosatellum and back, and both choices survive reload and JSON/gzip
roundtrips. Valid imported selections remain unchanged. Law/model/chamber
switching retains shared and inactive values, selected candidate pins and tree declarations.
Tri-state filings, disabled candidates,
new-component transitions, locked Camera-to-Senato edits, independent unlocked
edits, re-lock replacement and conflict rejection, membership-state roundtrips,
filing-only list participation, declaration-based coalition links even with an
empty counterpart, explicit counterpart filtering for one-chamber subjects,
retained-link visibility/reasons, automatic reactivation, explicit replacement
disclosures and deletion cleanup under D11,
references, both seed-generation buttons,
the visible basic non-valid reference and attached advanced national/chamber/local
tree controls, fallback/component-reference editors, read-only electorate data
and the default-used pooling editor,
reset/new-format import/export and error paths work. New subjects initialize
all law blocks. Check keyboard/mobile layout and run the Svelte tooling on
changed components.
Deferred D14 settings must survive validation, switching and file roundtrips,
but their editors are not required for CP-06 or CP-08 acceptance.

### CP-07 — Unify lifecycle, persistence and results

Implemented lifecycle: [controller, persistence and result guide](lifecycle.md).
Verification and remaining CP-08 acceptance are recorded in
[migration history](../migration-history.md#cp-07--unify-lifecycle-persistence-and-results-2026-11-14).

Integrate the editor with one draft/controller and the execution adapters.
Apply D15/D16/D17: implement execution locking/explicit cancellation, automatic
loading/reuse with visible cancellable preparation, dependency-based fit reuse,
button-generated visible seeds, requested/effective workers and safe disposal.
Use durable bounded execution/storage for all three pairs without the Rosatellum
1,000-run ceiling. Reuse existing result/session and streaming export services;
keep result tables/charts in tested presentation helpers. Preserve the immutable executed
scenario and stale-result indication across subsequent edits and selections.

Exit: delayed replies cannot update a newer revision; cosmetic/inactive edits do
not refit; relevant edits invalidate the correct stages. Reload, cancellation,
checkpoint restore, failures, stored queries and archive import/export retain
correct simulation membership, counts, seeds and original requested denominator.
Verify the session action table, resource validation, worker-count determinism
and quota-failure retention. Browsing/importing results never replaces the draft;
only explicit scenario loading does. Running or loading never replaces seeds;
each seed button edits only its field. Follow CP-08's campaign-size restriction.

### CP-08 — Acceptance, cleanup and documentation

Run focused domain/adapter checks during their steps, then the repository's
current production acceptance gate from `web/` after full preparation. Cover
every approved pair at `/` and `/elezioni`: direct loads, navigation, workers,
assets, imports, switching, results and exports. Verify regional smoke cases.
Release checks require every bundle and reject absent-data skips/debug fallbacks.

Run browser fitting + 1,000-simulation campaigns for each of the three approved
pairs sequentially, including generation and scrutiny, against the existing
under-60-minute target on this
machine. Record model/data/adapter/version, actual workers, timing and memory
evidence. Include bounded processing, resource/quota failures, cancellation,
interrupted-result recovery, export/import, session transitions and worker-count
determinism. Do not include a run above 1,000 simulations in acceptance; removal
of the old ceiling does not imply measured performance above that campaign size.
Retain distinct legal/non-computable and infrastructure failure reporting; the
known previous-law dead end requires separate legal review, not rerolling,
skipping or claiming a successful campaign.

Apply D19 when assessing comparisons: document equivalence and any differences
from old draws, but do not retain architectural duplication to force exact
seeded parity. Fixed-input legal regression tests and law/interpretation
requirements remain blocking. A passing expected-failure regression is not a
successful 1,000-run campaign.

After domain, legal and new-format checks pass, remove superseded politics
workspace components, active M00 generation/control paths, route branches,
draft writers, old schema readers and default logic. Retain independent regional functionality. Update current
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
| Authored/default preparation | `../data/politics-default-scenario.json`; `scripts/workspaces/{politics-default.ts,politics-assets.ts,generate.ts}`; `scripts/data-preparation/fast-share-release.ts`; [defaults guide](defaults.md) |
| Result contracts, access and export | `src/lib/core/{types.ts,result-export.ts,result-store.ts}`; `src/lib/workspaces/result-access.ts`; `src/lib/politics/result-presentation.ts`; `src/lib/politics/reform-2026/result-presentation.ts` |
