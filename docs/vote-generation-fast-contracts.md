# Fast vote generation: production data and scenario contracts

FV-01 contract revision **1**, frozen on **28 October 2026** for the 2026 reform
workflow. The scenario v4 parser/generator is not installed. FV-02 implements
numerical modules and FV-03 the [compact source boundary](vote-generation-fast-data.md);
FV-05 prepares production states/assets; FV-06/07 implement the
worker/editor. The [model](vote-generation-fast-model.md),
[reference rules](vote-generation-fast-references.md) and
[validation protocol](vote-generation-fast-validation.md) are separate contracts.

## Version and ownership boundaries

Reserve reform scenario `schemaVersion: 4`, `kind: politics-reform-scenario` and
legal `algorithmId: politiche-reform-2026-v1`. Current versions 1/2/3 are not
silently upgraded. New parsers reject unsupported versions and unknown fields;
no compatibility adapter or legacy/new vote-engine switch is required.

| Identity | Frozen value or responsibility |
| --- | --- |
| Statistical model | `fast-share-kind-common-conservative-v1` |
| Numerical implementation | Separate content/version identity; FV-02 must not change the model ID merely for storage/execution changes |
| Ballot representation | `final-attributed-list-totals-v1`; not the statistical engine identity |
| Reference interventions | `fixed-turnout-reference-v1`; approved turnout clarification below |
| Fitted-state/compact-source/profile/catalog schema | Each starts at 1 with its own `kind`; reject mismatches |
| Chamber adaptation/coupling | Separate version, source/assumption labels and sensitivity profile; pending FV-04 validation |
| Preferences/candidatures/seats/legal RNG | Retain their own existing IDs/settings; no statistical meaning inferred from a common version string |

Do not change the installed scenario constant, registry or catalog in FV-01.
Keep names/colors in presentation metadata. IDs, memberships and legal filing
are semantic data. Scenario edits never modify the authoritative observation
records or the reviewed source JSON.

## Scenario v4 field contract

All numbers are finite. Counts are nonnegative safe integers. Dates are real
ISO civil dates `YYYY-MM-DD`; shares are fractions, SDs are **percentage points**,
movement scales are per square-root year. `null` means inherit; explicit zero is
a value. Every collection has unique stable row IDs or a unique composite key.

| Field | Required contents and validation |
| --- | --- |
| `schemaVersion`, `kind`, `algorithmId` | Literal identities above |
| `data` | `sourceBundleId`, `profileBundleId`, `seatTableVersion`, `preferenceModelVersion`; each resolves to verified immutable content |
| `models` | Statistical, numerical, reference-policy, chamber-adapter/coupling and RNG identities |
| `referenceDate`, `electionDate`, `renewal` | Reference <= election; renewal `joint`, `camera` or `senato`; no synthetic second chamber for single renewal |
| `components` | `{id, kind: fitted|new, provenance}`; new components also declare a required reference and fallback profile/scale policy; opaque historical atoms cannot be split by renaming |
| `historySelection` | Explicit selected election IDs and eligible source window, never a held-out-case ID or hidden current clock |
| `sourceMemberships` | Complete rows keyed by observation/source-choice ID, with disjoint component IDs, offered-set IDs, Other-complement definition and provenance; no numerical split factors |
| `polls` | Selected bundled question IDs plus complete user-added/edited observations; explicit exclusions; preserve original and normalized views |
| `lists.camera`, `lists.senato` | Existing legal list metadata plus `componentIds`; nonempty disjoint memberships on every physical ballot, with common component identities across chambers |
| `coverage` | Chamber/physical college/list IDs; candidate-free Other has separate legal coverage; components inherit union of member-list physical support |
| `references` | Unique `(scope, subject, input channel)` rows; subject is a list or new component; channel `common`, `camera`, `senato`; `{mean, sdPercentagePoints}` or inherit; reference-policy revision required |
| `nonValidReferences` | Unique scope/channel rows on the **elector denominator**; separate from political references; the same mean/SD unit convention |
| `uncertainty` | Nonnegative national/local list multipliers and optional new-component scales; default 1 for multipliers, no removal of uncertainty by implication |
| `chamberProfileId`, `preferenceProfileId` | Explicit compatible parameter profiles, including dependence assumptions |
| `candidateGeneration`, `candidateTemplates`, `suppliedPreferences`, `suppliedVotes`, `bonusLinks` | Retain existing typed R26 meaning/validation, supplied-count precedence and candidate-free Other restrictions |
| `seed`, `fitSeed`, `presentation` | Simulation seed separate from fit seed; presentation optional and non-semantic |

Scopes are tagged `national`, `region + regionId`, `province + provinceId`, or
`municipality + municipalityId`, pinned to the target geography version. No
ambiguous free-text geographic lookup at run time. Mixed-scope resolution and
conflicting references follow the reference contract, not JSON row order.

User poll records contain: stable question/survey IDs, source citation and
user-input provenance, fieldwork start/end, publication/availability date,
representative date, ballot context, reported political groups/shares, offered
set/Other complement, sample size, decided fraction or question sample, design
effect/effective-sample policy, rounding width and shared-error group. Retain
original units. Do not multiply design effect twice or treat nonresponse as
non-valid votes. Duplicate surveys are one information source; conditional
questions and printed-zero answers retain explicit unsupported/excluded status
until their observation treatment is designed.

## Supported source range and contexts

The initial pinned history is Camera 2008/2013/2018/2022 and European
2009/2014/2019/2024 from
[`elections-manifest.json`](../data/sources/polls/history/elections-manifest.json).
Reform preparation separately pins Senate 2018/2022 and the preference sources in
[`reform-model-source-manifest.json`](../data/reform-model-source-manifest.json).
Senate 2022 is the paired 18+ baseline; Senate 2018 is a 25+ audit source, not
unadjusted central 18+ share information. Regional/European preference data do
not become additional independent national vote observations.

Compact-source schema contexts are `camera`, `european`, `senato`. Data availability
and fitted support are separate: the approved numerical reference covers C/E;
Senate adaptation must pass FV-04 and cannot relabel European draws. No newly
downloaded election or poll is implied by schema support. The supported reference
window starts 2008-04-13; later reference dates may exceed the latest bundled
observation, with explicit extrapolation/vintage warnings and no invented data.
An admissible requested fitted basis still requires distinguishing information;
no-information/new support needs a declared new-component reference instead.

Cutoff is inclusive at the end of the named civil day. Elapsed model times use
UTC date-day differences / 365.25, independent of the computer's timezone. An
election must have event and result-availability dates <= cutoff. Poll fieldwork
end, publication and representative date must all be <= cutoff. Date-only source
availability uses this explicit daily convention. Moving the reference date
always resets the anchored fit/prior, even if eligible record IDs do not change.
Known historical-result availability dates take precedence; otherwise the approved
fallback is availability by the end of the first day after the final polling day,
labelled as assumed. This does not authorize using incomplete live returns as
complete historical observations. Current-election final-count forecasting from
partial scrutiny is a separate post-migration task.
The source bundle declares its available range and exclusions; unsupported dates,
source IDs or geographic revisions fail validation rather than fetching live data.

## Compact-source, profile and fitted-state envelopes

Each envelope has `{schemaVersion: 1, kind, id, modelId, sourceVersions,
methodVersions, assumptions, payload}`. `id` addresses canonical semantic content;
asset descriptors additionally pin exact compressed/decoded bytes and hashes.

| `kind` | Mandatory payload |
| --- | --- |
| `fast-share-source` | Dictionaries; original grouped national/municipal counts/electors/non-valid counts; observed-zero/missing/offered masks; source dates/contexts and availability; original/normalized poll views; complete source memberships and provenance; target crosswalk/cell metadata |
| `fast-share-profiles` | Component/context/reference-date ordering; physical-cell and parent municipality IDs; electors, regions/provinces/college membership; profile means/SDs, filter/future scales, fallback reason/source and admissible support; independent profile key |
| `fast-share-fit` | Reference date; component/context order; national semantic key; numerical version and sampler/retention policy; accepted **joint-context** political draws and separate joint-context non-valid draws; calibrated scales and fixed covariance conventions; per-chain diagnostics, warnings and fit seed |
| `fast-share-catalog` | Selected compatible scenario/model/fit/profile/source versions and descriptors; no mutable pointer as the sole replay identity |

Missing municipal observations are missing rows/masks, not zero counts. Keep
original grouped counts sufficient to rebuild a changed source-membership table,
scale estimates and empirical profiles. Precomputed default-component profiles,
national means or a Hessian alone are insufficient refit data. Source records
exclude held-out truth, source PDFs, unused workbook columns and geometry shapes.

Dense fitting arrays use Float64; count/offset/index dtypes must be range-checked.
Numeric chunks explicitly specify shape, little-endian byte order, dtype,
dictionary/order references, compressed/decoded size/hash and valid missing masks.
Do not encode missing numeric values as NaN/zero or infinity in JSON. Structural
zero is represented by support masks; log minus-infinity is an internal value.
Posterior draw ordering is semantic and never re-sorted independently by context;
thinning/selection must preserve joint draws and pass the retention moment/tail gate.

## Semantic identity and invalidation matrix

Canonical input uses UTF-8 JSON with recursively sorted object keys, finite
ECMAScript number serialization (`-0` becomes `0`), and canonical stable-ID order
for set-like rows. Meaningful vectors/draws retain explicit declared order.
Exclude display labels, timestamps of computation and local paths. Hash the
canonical bytes with SHA-256 plus an explicit key-kind/version domain separator.
Missing versus explicit zero remains different. Include a source's normalized
numeric/membership/date/policy content, not merely its display ID or selected IDs.
Track citations, original-byte/source pins and provenance-only changes separately
in `provenanceId`. Asset/envelope identities may change for updated provenance
while numerical arrays remain reusable against unchanged numerical input keys.

| Key | Semantic parents/content |
| --- | --- |
| `nationalInputKey` | Normalized numeric source fingerprints, eligible grouped observations, poll effective samples/error groups, source memberships/basis, history selection, model/covariance/prior/context policies and reference date |
| `profileInputKey` | Municipal histories/counts, memberships/basis, target context/cell geography, reference date and empirical profile/filter policy |
| `fittedStateId` | National key, implementation version, fit seed/sampler settings, accepted retained draws and diagnostic/retention policy |
| `profileStateId` | Profile key and exact fitted profile arrays/assumptions |
| `compositionKey` | Fit/profile IDs, reference-policy inputs, future date/electorates, lists/membership/support, local overrides, uncertainty and chamber adaptation/coupling |
| `runKey` | Composition key, candidate/preference/supplied/legal settings, simulation seed and all RNG stream versions; a simulation is additionally addressed by its ID |
| `provenanceId` | Original/normalized source pins, citations, review/method records and their bindings to the numerical input/state identities; a provenance-only revision does not require numerical re-estimation |

| Edit | National fit | Profiles | Composition/run |
| --- | --- | --- | --- |
| Name/color/display order or requested run count | Reuse | Reuse | Presentation/request bookkeeping only; retain semantic list IDs |
| Simulation seed | Reuse | Reuse | New run, unchanged composition |
| Fit seed/retention/implementation | New fitted-state identity | Reuse if policy unchanged | Rebuild using new state |
| Future grouping/references/filing/new referenced component | Reuse compatible fitted basis | Reuse fitted atoms; build declared new fallback state | Recompile support/interventions/candidate reference plan |
| Future election date/electorates/geography | Reuse reference fit | Rebuild affected context/cell profiles | Recompile projection/forecast |
| Eligible poll content/effective sample/error policy | Refit | Reuse only if basis/profile policy unchanged | Recompile |
| Historical membership/identity/selection or model policy | Refit affected national state | Refit affected profile state | Recompile |
| Reference date, including unchanged eligible IDs | Refit | Refit date-dependent state | Recompile |
| Source label/citation correction only | No numeric refit if normalized semantic content unchanged; new provenance identity | Same rule | Preserve both semantic and provenance identities |

User edits never install a partial fit. A request/reply binds both input keys,
scenario revision and engine version. Only passing matching state becomes ready;
cancelled/failed/stale replies leave the last accepted state owned and intact.
No run may use an incompatible fit or silently substitute the default one.

Export the full scenario (including added polls/memberships), accepted fitted
state, relevant profile state or permanently available immutable content IDs,
and all seeds/source/diagnostic policies alongside the worker result. New result
envelopes are explicitly versioned; CSV remains a table export. Do not change
the generic export implementation until its scheduled integration step.

## Frozen default and physical-domain inventory

The FV-01 [inventory index](../research/vote-generation/README.md) pins the actual
installed default's lists, provisional component proposals and every required
target physical college/municipality. It is an implementation coverage contract,
not a newly generated scenario or approval of all provisional historical splits.
Opaque special-ballot groups remain separately flagged for source review.
The old default dates are retained as a candidate: reference 2026-10-19 and
election 2027-09-25; FV-05 must explicitly choose/pin the released dates and poll
vintage rather than using the build clock. Full default admissibility and Senate/
special coverage are FV-03/04/08 release gates, not claims made by this inventory.
