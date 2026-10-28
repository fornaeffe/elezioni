# Fast vote generation: 2026 reform production plan

## Scope and reading map

On **28 October 2026**, Luca selected the model described in
[the fast research report](vote-generation-fast-research.md) for production and
requested browser parameter estimation. This plan covers **only the 2026 reform
workflow**. Model selection is settled; browser implementation and production
acceptance remain open. Complete one requested step at a time. The authoritative
checklist is in [MIGRATION_PLAN.md](../MIGRATION_PLAN.md); record evidence in
[migration history](migration-history.md).

| Task | Read |
| --- | --- |
| Implement the numerical model | Approved report, sections National reference estimation through Municipal predictive module; FV-01/02/04 below |
| Prepare or load production assets | Delivery and bundle contracts; FV-03/05/06 |
| Edit scenarios or integrate workers/results | Reuse table and FV-06/07; existing [application guide](politics-reform-2026-application.md) |
| Validate and switch defaults | FV-08 |
| Archive research or reclaim generated files | Separate [archive plan](vote-generation-research-archive-plan.md) and FV-09 |

The earlier latent-state/Stan model, its posterior gate and its staged research
comparisons are no longer prerequisites. Keep their conclusions as historical
evidence. Do not transplant their likelihood, logit prior or prohibition on
country-margin projection into the selected fast model.

## Delivery and browser workflow

Preparation runs periodically on a PC or in CI. Its dependency order is
`npm ci -> acquire/verify sources -> normalize/compact -> fit default -> validate
assets -> build static app -> package/check`. The final build must consume the
matching generated catalog, defaults and fit; building before preparation cannot
produce the final release. Source refresh and source-pinned offline replay are
separate operations. An ordinary UI rebuild may reuse unchanged accepted assets.

One Node/TypeScript numerical engine serves offline prefitting and browser
workers. Python/SciPy/C++ remain research references, not production preparation
or browser requirements. Start with Float64 TypeScript and bounded typed arrays;
consider a numerical WebAssembly kernel only if FV-02/08 measurements require it,
using the same API and numerical gates. Do not ship Python, Stan, raw election
archives, PDFs, research chains or historical backtest truth to the browser.

1. Open the reform simulator. Load its small catalog/default scenario and the
   accepted default fitted state; no browser estimation runs automatically.
2. Arrange fitted political components into future lists, edit reference shares,
   filings, candidates and election settings. Compile these interventions against
   the reusable fit and profiles, validating geographic feasibility.
3. Open advanced estimation to change polls, source memberships, historical
   selection or reference date. Fetch the compact refitting data only now. Show
   why the fit is stale and offer **Ricalcola parametri**.
4. Run estimation in a dedicated worker. Keep the previous accepted fit until a
   matching replacement passes diagnostics. A stale fit cannot run the edited
   scenario. Failure/cancellation leaves the draft editable and allows restoring
   the last compatible scenario/fit.
5. Run paired simulations against a frozen scenario/fit revision, stream results
   through the existing bounded worker/storage pipeline, and present/export them.

**Loading decision for this implementation:** include demand loading and explicit
loading/ready/error/retry states now. Load generation profiles when first needed
for preview or simulation, and refit history only for advanced estimation. Reuse
the compact activity overlay and interaction toasts. Defer speculative background
prefetch, a separate background-files-ready notification, service-worker offline
support and a persistent fit-cache manager. No startup download for other election
kinds is introduced. This division needs no new simulator route.

## Model and scenario contracts

Preserve the selected `kind-common-conservative` formulation: bounded Gaussian
share-information fitting, three covariance/mode updates, separate non-valid
estimation, regularized election-kind contexts, truncated-normal transfer
sampling, mean-preserving Beta-stick transitions, the 1.5 political discrepancy
multiplier, empirical-Bayes municipal profiles and common future local scale.
Keep reference, future country movement, election discrepancy, profile,
regional/local movement and integer count uncertainty distinguishable. At zero
calendar horizon profile and election-realization uncertainty still exist.

Country draws constrain **expected elector-share probability margins**, projected
before integer counts. Actual country results are sums of generated local counts.
Never independently fix their integer totals. Keep the report's `1e-9` margin
tolerance and 120-iteration limit; unavailable positive targets and non-convergence
are explicit errors. Model performance does not establish unbiased means or
universal list/seat calibration.

Introduce a new strict reform scenario/model schema and separate identities for
the vote-share engine, final-attributed ballot representation, preference model,
chamber coupling, fitting algorithm, RNG streams and data. Keep
`final-attributed-list-totals-v1` as representation semantics; it does not identify
the new statistical model. Preserve the legal algorithm ID and legal core.
The earlier approved early-development transition permits breaking old reform
scenario/result compatibility: do not build adapters or a dual-generator UI for
this replacement. Reject unsupported versions clearly; old research/tests can
be replayed in their recorded repository revision. Pre-2026 politics and regional
workflows retain their own contracts and generators.

The new scenario owns:

- Stable component IDs and **complete source-specific memberships**, including
  poll choices and explicit Other complements. Store user edits as scenario data,
  separate from immutable source counts and bundled correspondence defaults.
- Future lists as disjoint sets of fitted components, with chamber presence and
  geographic filing. Joint lists sum the same component counts. Do not translate
  the old factor-based correspondence schema into invented fractional component
  counts. Unsupported retrospective children remain indivisible or are rejected;
  a finite proper posterior alone does not prove identifiability.
- New components with explicit reference support and labelled territorial/scale
  assumptions; never fabricate historical zeros. Distinguish a missing history,
  an observed zero and a future structural absence. Review the provisional
  membership table before making it a production default.
- Separate reference and election dates, reference shares and uncertainty units
  (valid-vote fractions, SD in percentage points), turnout/non-valid inputs,
  national/local uncertainty controls and common/chamber/geographic overrides.
  Reference inputs are interventions on latent reference support, not fictitious
  polls or claims that historical information was reconditioned.
- Bundled poll selections plus user-entered poll observations and provenance:
  question/choices, fieldwork and publication dates, election context, survey
  identity/error group, sample/effective sample policy and normalization.
  Poll nonresponse is not an observation of the combined non-valid outcome.
  Preserve explicit exclusion/validation of unsupported conditional questions
  and printed-zero political answers until their observation treatment is
  designed; never replace a reported zero by an invented positive measurement.

Retain common-anchor derivation, explicit per-chamber precedence and geographic
specificity (national, region, province, municipality), blank versus explicit
zero, and supplied vote/preference precedence. Translate these into documented
fast-model operations; do not accidentally retain old log-odds noise on top of
the new uncertainty budget. An exact reference constrains the aggregate latent
reference share, not every municipal share or later integer count. The research
prototype handles disjoint exact references and only one uncertain reference;
FV-01 must specify and FV-04 must validate the production combination of multiple
uncertain references, common/chamber constraints and local scopes. Reject
inconsistent constraints explicitly; do not silently ignore an input or impose
that prototype limitation on the existing multi-list editor.

Treat the observable residual, a poll's Other answer and the simulator's optional
candidate-free **Altre liste** as different concepts. Follow
[the Other contract](other-list.md) for candidate exclusion and denominators;
its composition must be explicit, and no component may be counted twice.
The old unmapped-to-abstention convention does not authorize turning missing
memberships into non-valid counts in the fast model.

## Assets, reuse and provenance

Use a small versioned JSON manifest and dictionary metadata plus compressed
columnar numeric chunks under `web/static/data/reform-2026/`. Specify dtypes,
dimensions, ordering and byte order; use Float64 for numerical fitting. Integer
counts remain exact. Choose count widths from validated ranges. Quantization of
profiles is optional only after an explicit error/coverage comparison.

| Asset | Contents and loading boundary |
| --- | --- |
| Catalog and scenario | Selected engine/data versions, compatible defaults, asset descriptors, component/list/source labels; small simulator-entry payload |
| Default fitted state | Reference date/basis/contexts, accepted joint-context reference draws, fitted scales/covariance policy, diagnostics, fit seed and fingerprint; load on reform entry |
| Generation profiles | Complete target physical geography/electorates, regional links, profile means/SDs, scales, fallback flags and college reconstruction; load for preview/simulation |
| National refit records | All selectable historical grouped observations/count totals, dates/contexts and compact poll questions with eligibility/error metadata |
| Municipal refit records | Dictionary-indexed original observed source-group counts, electors/non-valid totals, dates, region/geographic crosswalks and missing/availability masks; fetch for profile re-estimation |

A research posterior/profile export **cannot** support changed correspondences
or arbitrary supported reference dates by itself. Preserve the compact original
grouped records required to rebuild memberships, coarsening, scale statistics
and profiles. Do not retain only default-component sufficient statistics. Avoid
shipping redundant raw labels per row, geometry polygons, unused workbook
columns, images or evaluation truth. The supported historical source range and
contexts must be explicit; dates outside the range cannot summon unbundled data.
A reference date before some records excludes them by publication/fieldwork and
election availability, and always constructs a new date-anchored fit/prior.

Content-address the assets, recording source/method hashes, schema/model/RNG
versions, compressed and decoded hashes/sizes, dimensions, applicability and
missing-data assumptions. Publish catalog/defaults/fit/profiles as one consistent
set only after verification. Never edit generated defaults by hand. A cache miss
must be reproducible from a clean clone and pinned sources/reviewed poll JSON.
No use of the held-out-election-based research case exporter for real defaults.
Default dates/poll vintage are explicit preparation inputs, never the build clock;
there is no automatic claim of polls current to October 2026.

Use separate semantic cache keys, not a hash of presentation JSON:

| Edit | Required work |
| --- | --- |
| Name, color, order, simulation count or run seed | No estimation; regenerate only affected run/presentation state |
| Regroup existing atoms into disjoint future lists | Reuse national fit/profiles; recompile list aggregation, references and candidate planning |
| Reference share/SD, turnout intervention, future filing or new referenced component | Reuse compatible historical fit; rebuild interventions/support, affected fallback profiles and feasibility checks |
| Election date, target electorate/geography or forecast uncertainty controls | Reuse compatible reference fit; rebuild applicable profiles/projection/coupling and forecast state |
| Poll addition/removal/edit or shared-error policy | Refit national information/scales; reuse profiles only if their inputs and admissible component basis are unchanged |
| Historical selection/source membership, component identity/basis or model policy | Refit affected national and geographic modules; recheck admissibility |
| Reference date | Invalidate national fit and date-dependent profiles/prior, even with no new observations; estimate again |

The national key includes eligible observations, effective samples, memberships,
component order, context/priors/covariance policy and reference date. The profile
key includes eligible municipal history, memberships, target context/geography,
reference date and profile policy. The composition key adds reference
interventions, future date/electorates, filing and coupling. The run key adds
candidatures/preferences/legal settings and deterministic seed/stream versions.
Canonical ordering must not accidentally change an atom's random stream when a
list is renamed or a display row is moved.

Keep the fit sampler policy/seed and numerical-engine version in the fitted-state
identity, separate from the simulation seed. If the portable state retains fewer
draws than the diagnostic chains, validate that selection's moments/tails and
record its policy; do not replace posterior uncertainty with a small set of modes.

Export the actual fitted state used, diagnostics and user-added observations
with the scenario/result provenance, including scenario-specific profile data
needed for replay or immutable bundle identities with guaranteed availability.
A mutable catalog pointer alone is insufficient. Keep generic export code in
`core/result-export.ts`. Quota/cache eviction must not silently substitute the
default fit for a user fit; importing an incompatible state reports the version
problem. Persisting a scenario must not lose its custom polls or memberships.

## Implementation steps

### FV-01: Freeze production contracts and preserve research evidence

Specify the schema above, supported observation contexts/source range, multiple
reference intervention algebra, and paired-chamber extension boundary. Extract
the approved equations into an active `docs/vote-generation-fast-model.md`
contract, retaining research evidence separately. Label inherited approximation
and assumption choices; any necessary substantive model change needs a recorded
decision before implementation. Freeze the accuracy/performance protocol and
prepare the research retention/deletion inventory before later moves.

Acceptance: worked grouped/Other, joint/new-list, hard/soft/common/local-reference
and non-valid examples; fingerprint/invalidation matrix; inventory of default
components and all required physical territories. No unresolved old Stan gate.

### FV-02: Port the fit and profile engine; pass reduced browser gates

Create browser-safe pure TypeScript modules under
`web/src/lib/politics/reform-2026/vote-share/`, imported by offline wrappers.
Port covariance/scales, constrained active-set mode, native transfer sampler,
truncated-normal tails, diagnostics and geographic feature/filter construction.
Use linear solves, not explicit large matrix inverses. Bound observation-matrix
workspace; exploit the defined covariance's structure if required, without
dropping poll correlations or silently thinning observations.

Use a small browser-worker harness immediately, before full-country fitting.
Check interior, near-boundary, coupled-context and weak grouped-split fixtures
against Python/independent quadrature. Require mean/90% endpoint errors no worse
than the recorded research tolerances (0.151/0.356 pp, allowing predeclared Monte
Carlo error), and separately test modes, covariance and tail sampling. Retain
four-chain rank-normalized split/folded R-hat <= 1.05, bulk ESS >= 400 and tail
ESS >= 200; adapt retained draws from 2,000 to 4,000/8,000 per chain. Failure is
not an accepted fit. Store accepted draws, not just the constrained mode/Hessian.

Acceptance: independent numerical checks, Node/browser distributional agreement,
deterministic repeatability within the new engine, reduced timing/peak-workspace
measurements and full-data extrapolation. Python's different RNG does not imply
byte-identical draws. Validate the means/tails rather than only convergence.

### FV-03: Deliver compact, editable production history

Promote required research acquisition/normalization/membership/poll-view helpers
into active preparation modules with focused tests. Reuse pinned acquisition,
geographic reconciliation and historical attribution. Add required Senate and
special-territory observations/labelled fallbacks; do not inherit the research
exclusion of Valle d'Aosta or unmatched rows. Audit older 25+ Senate electorates
separately from the 18+ target. Preserve the reviewed early FDI/FI poll repair,
same-survey deduplication and effective-sample/shared-error semantics.

Generate the assets above, including enough original grouped information for
alternate supported memberships/dates. Add non-interactive `data:reform:shares:*`
commands for acquisition, preparation, prefitting and checking (names proposed,
not existing commands). Keep detailed audits under `data/derived/` and reviewed
source JSON/manifests under `data/sources/`.

Acceptance: empty-cache acquisition, offline deterministic regeneration,
conservation, observed-zero/missing distinctions, alternate-membership/date
rebuilds and full domestic coverage. Measure transfer/decoded sizes and peak
decode memory before locking the chunk format. No manual R/WSL/Python dependency.

### FV-04: Generate votes and integrate the paired reform adapter

Port Beta transitions, scale mixtures, profile/regional/local noise, structural
support projection and multinomial sampling. Stream one simulation and bounded
geographic chunks; never allocate simulations x municipalities x components.
Global raking still needs every physical cell's weighted margins: chunked passes
must solve that same global target, not independent per-chunk targets.

Resolve filing at **physical ballot cells**, splitting municipalities where
college coverage differs before feasibility/projection. Reconcile dated
electorates/crosswalks and preserve municipal totals through labelled integer
reconstruction. Cover VdA/TAA and derive Camera TAA's overlapping proportional
view from its physical ballots once. New/missing local histories use the model's
labelled fallback and uncertainty, not an arbitrary support epsilon.

Implement a versioned Camera/Senato coupling using paired historical contrasts
and declared uncertainty assumptions under R26-04. The research Camera/European
contexts are not Camera/Senato fits. Validate each chamber's marginal fast-model
distribution and preserve its variance budget; a shared seed alone is not a
dependence model. Do not simply add the old paired shocks to the new draws.
Report sensitivity where paired observations cannot estimate future covariance.

Feed final attributed list totals into the existing candidate/preference and
legal boundaries. Rebuild reference-based bonus candidature planning from the
new compiled reference support, preserving supplied people/counts and preference
budgets. Keep vote, fit, candidate, preference and legal tie streams separate.
Record errors rather than rerolling failed projections or legal runs.

Acceptance: integer/elector conservation, structural zeros and capacity failures,
exact joint-list aggregation, country probability margins, zero-horizon behavior,
multiple/scoped references, new-list support, chamber marginals/dependence,
special-ballot accounting and full-list scrutiny input validation.

### FV-05: Prefit defaults and package a reproducible static release

Run the same accepted engine on the complete explicit default scenario; generate
default fit, both chamber profiles, metadata/defaults/catalog and runtime chunks.
Do not reuse a European-2024 backtest export as an upcoming-election fit. Reuse
unchanged preference/seat preparation with their separate provenance.

Provide one local/CI orchestration command for source-pinned preparation,
verification, final static build and production-asset checks; source refresh is
optional and deliberate. Cache using source/model/semantic hashes. This step
may produce candidate release assets but changes the selected app defaults only
at FV-08. Public Actions/Pages deployment remains under the final migration gate.

Acceptance: clean-clone cold-cache and warm-cache runs, byte-identical repeated
assets, matching scenario/fit fingerprints and atomic catalog selection. Missing
or mismatched required assets fail release; no accepted skip/debug fallback.

### FV-06: Add browser estimation and bounded worker ownership

Add `reform-fit.worker.ts` and a typed client for load/validate/fit/progress/
cancel/result/error, using request IDs and scenario revisions. Yield between
bounded numeric batches so cancellation is serviced; termination/recreation is
the fallback for an uninterruptible batch. Publish the completed fit atomically
only if its revision/fingerprints still match. Reuse unaffected profile state.
Loading/estimation failure must leave the previous accepted state intact.

Keep refit-history arrays in the fitting worker, then release them when unused.
Pass compact fitted buffers to generation workers; a transferred buffer has one
owner, so retain an authoritative state or deliberately copy per consumer.
Fit once per scenario, never once per simulation worker. Adapt worker capacity
to **aggregate** memory, including decoding, profiles and the existing results
pipeline. Do not require SharedArrayBuffer or cross-origin-isolation headers.

Acceptance: default start performs zero fits; custom polls/memberships/date
changes fit in browser; stale replies, cancellation, failure, retry and rapid
edits cannot install an incompatible state. Serial/parallel simulation output
is identical for the same new-engine inputs and simulation IDs.

### FV-07: Implement the scenario workflow

Extend the existing reform workspace/editor with component-to-list composition,
reference uncertainty and collapsed advanced estimation controls for polls,
source memberships, history and reference date. Add poll entry/edit/removal now
for this workflow; it is no longer deferred until the whole migration. Browser
poll entry is manual scenario data; scraping government sources remains offline.
Explain cutoff exclusions, admissibility and fallback estimates at the edited
input. Never silently split an unresolved historical unit.

Show separate data availability, fit compatibility and run status. Keep editing
available while data load; disable only dependent previews/estimation/run.
Preserve default restore, supplied overrides, candidate-free Other, candidate
planning, result staleness and existing interaction/result message conventions.
Store a frozen scenario/fit snapshot per run; later draft edits cannot alter it.
Use Svelte 5 runes and tested TypeScript presentation helpers.

Acceptance: default edit-and-run without refitting; joint/new list and references;
custom poll/membership/date refit-and-run; cancel/fail/retry; save/load and result
export/import with the fitted state; keyboard/mobile states and base-path routes.

### FV-08: Accept the full workflow and switch reform defaults

Freeze test cases, seeds, metrics and gates before inspecting production results.
Reproduce the corrected research panel with its original eligibility/domain for
port parity, then separately validate the expanded default components, complete
domestic geography, Senate/paired behavior and actual default date/poll vintage.
Keep observed-group scores separate from unobserved within-group splits. Compare
the current reform generator and report plausibility first, interval coverage
second, then mean errors, nationally, municipally and regionally; report sparse
events, new-party/long-horizon sensitivity and interval widths. Do not label the
corrected research confirmation as an untouched holdout.

Run at least 1,000 complete paired simulations through scrutiny, storage, plots
and exports. Compare thresholds/bonus probabilities, seat distributions,
candidate results and non-computable-run rates, retaining all requested runs in
denominators. Distribution changes are expected; exact old-generator parity is
not the adoption criterion. Independently fixed-input legal regressions must
remain identical. A material statistical deterioration requires diagnosis and a
recorded decision, not silent tolerance changes or automatic retuning.

Initial engineering targets, to confirm/freeze after FV-02/03 measurements:
default generation assets <= 10 MiB compressed; advanced refit data <= 25 MiB;
complete refit <= 30 seconds on a documented desktop and <= 120 seconds with
4x CPU throttling; cancellation feedback <= 1 second; fit workspace <= 256 MiB
and aggregate active compute/data memory <= 512 MiB, excluding persisted result
storage. Report transfer, decompression, fit and generation separately. Measure
the full current default, maximum supported components/polls and worker counts;
bound supported input sizes explicitly if necessary. These are targets, not
claims derived from Python timings. Record changes before accepting defaults.

Require `check`, `check:data`, meaningful unit/numerical tests, production data
checks, static build and browser workflow/result/export checks at `/` and
`/elezioni`, with direct loads, refreshes, worker/data paths and compressed versus
HTTP-decoded asset verification. Exercise supported Chromium/Firefox/WebKit
targets and a constrained-memory single-worker case; record actual coverage.
Measure Linux CI preparation independently of the final all-election hosting
reassessment. Required release tests cannot skip missing bundles.

Only after these gates pass, select the new reform model/default catalog and
remove the superseded reform vote path from active dispatch. Record versions,
timings, sizes, statistical comparisons and limitations in history. Other
election workflows and public publication retain their existing step boundaries.

### FV-09: Archive research and remove regenerable leftovers

Execute the [archive plan](vote-generation-research-archive-plan.md) after the
production dependency split and FV-08 acceptance. Preserve evidence/inventories
from FV-01 first. Archive all superseded research documentation, implementations
and the selected small evidence set, then remove only manifest-listed disposable
outputs. Recheck active imports, documentation links, clean-clone preparation and
production tests after cleanup. Record exact moved/deleted paths and reclaimed
bytes; do not equate a proposed cleanup with executed deletion.

## Browser implementation references

Use the installed SvelteKit/Vite versions and the repository's base-aware
`core/runtime-data.ts` boundary. Verify current framework APIs when implementing.
[SvelteKit static hosting](https://svelte.dev/docs/kit/adapter-static) documents the
static/base-path constraints; [Vite workers](https://vite.dev/guide/features.html#web-workers)
documents separately emitted module-worker bundles; [MDN worker ownership](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers#passing_data_by_transferring_ownership_transferable_objects)
explains transferred-buffer ownership. These support the packaging/worker design,
not the unmeasured numerical performance targets.
