## Post-FV-08 correspondence-policy audit — 1 November 2026

Completed the audit requested in the migration plan without changing generation,
scrutiny, defaults, source compositions or archive policies. Findings and proposed
corrections live in [the focused audit](../../../research/vote-generation/docs/correspondence-policy-audit.md); the plan
closes the audit and separately tracks review/implementation of corrections.

Added eight constructed characterization cases covering incomplete destinations,
selected-versus-legacy non-valid conditioning, regional/equal fallback and its
zero-mass discontinuity, incomplete poll groups and disjoint territorial tickets,
unsupported/new lists, physical submunicipal capacity and order-dependent greedy
college allocation. The latter reproduces a feasible 50/50 allocation rejected
when the flexible list consumes restricted capacity first. Existing joint-capacity
tests verify that selected projection detects overlapping capacity conflicts.
These tests deliberately document current defects; they do not bless them as
desired future behavior.

The read-only `web/scripts/acceptance/correspondence-policy-audit.ts` recompiles
legacy from the installed v5 release and isolates historical preparation before
poll/manual references. Curated output is retained in
`test/fixtures/benchmarks/correspondence_policy_audit.json`, with catalog/source/
compiled-input bindings. All 7,904 municipalities in each chamber have baseline
history. Raw mapping/filing loss is 1,359,094 Camera and 1,238,443 Senate valid
votes; each chamber uses equal-list fallback for 74 municipalities. The default
has no heterogeneous filing within split municipalities, so constructed stress
cases are essential. The report includes all national list deltas and the largest
integer baseline college deltas against the original projection rule applied to
the same current inputs. These are deterministic preparation sensitivity results,
not archived-result replays, final forecast/seat effects or empirical calibration.

Corrected the unified workflow's documentation: selected full-elector-vector
conditioning does not generally preserve the non-valid fraction, while explicit
political reference interventions do preserve the resulting cell turnout. Legacy
historical-turnout restoration is a separate substantive transfer assumption.
Further numerical corrections, including physical-cell poll allocation and a
joint transportation allocator, remain proposals awaiting Luca's decisions.

Verification: the audit reader completed against the bound production bundles;
the audit, unified-generation and forecast-numerics suites passed **25 tests in
3 files**, with no skips; `npm run check:data`, `npm run check` (zero errors and
warnings) and `git diff --check` passed.
The first audit-reader college probe incorrectly supplied fractional weights to
the integer-only apportioner; it was corrected to use the existing $10^{12}$
integer probability-weight convention before recording the successful report.
No full acceptance campaign, publication, commit or deployment was performed.

### Correspondence semantics decision — 1 November 2026

Luca agreed to one conditioning policy for known absent components, regardless
of whether an identity ceased to exist or is simply not offered. Updated the
[correction list](../../../research/vote-generation/docs/correspondence-policy-audit.md#proposed-corrections-before-implementation)
to record full offered-choice-set proportional conditioning as the central
assumption, including electoral non-valid support, with substitution uncertainty
still to be designed. Polls retain their represented response denominator;
political percentages do not identify missing nonresponse probabilities.

Components without historical records or genuinely unidentifiable must have an
explicit scenario reference, also within joint lists; missing references must
invalidate the scenario rather than trigger an implicit zero/equal/donor
replacement. Remaining reference/profile and fallback design stays open.
The plan records Luca's implementation boundary: settle all correspondence-policy
decisions first, then implement them in one coordinated pass. This update changes
documentation only; no algorithm, validation code, default or data was changed.

### Local historical support and geographic inference — 1 November 2026

Recorded Luca's agreed component-level rules in correction 4 of the
[correspondence audit](../../../research/vote-generation/docs/correspondence-policy-audit.md#proposed-corrections-before-implementation):
use identifiable local positive/zero evidence; infer never-offered local support
from regional, then national evidence; require explicit legacy joint-list
compositions; and retain selected-model group evidence with references for
otherwise unidentified components. Geographic evidence availability is distinct
from positivity, and historical offered-set denominators must be respected.
This settles the discontinuous fallback policy apart from list packaging.

Specified national or complete territorial/chamber reference coverage for new
components and selected-model unidentified components, also inside future joint
tickets. Documented fixed legacy historical inputs, its existing zero branch,
selected-model smoothing, and the implementation gaps identified during the
preceding read-only review. Exact estimators and uncertainty design remain open.
All changes await the single coordinated pass after remaining policy decisions
are settled. This update changes only the audit and history; `git diff --check`
passed. No code, data or defaults were changed and no tests were rerun for this
documentation-only update.

### Legacy observed-zero regularization decision — 1 November 2026

Added agreed correction 5 to the
[correspondence audit](../../../research/vote-generation/docs/correspondence-policy-audit.md#proposed-corrections-before-implementation):
use a half-vote probability for identifiable historical-zero local logits after
correspondence compilation and before poll/reference reconciliation. Raw counts
remain zero; positive fractional counts remain unchanged. Missing observations,
unfiled choices, explicit zero interventions and capacity-required zeros retain
their separate meanings. Final integer allocation conserves the electorate and
may still produce zero votes in individual draws. Documented the original R and
TypeScript half-vote-floor precedent and its difference from this zero-only rule.

Also recorded the already-discussed resolution of the equal-list fallback's
packaging issue: establish component support first and remove the equal-list
emergency allocation; territorial-ticket projection remains a separate issue.
Implementation of these and all other settled decisions stays deferred to the
single coordinated pass after remaining decisions are settled. Updated only the
audit and history; `git diff --check` passed. No code, data, defaults or tests
were changed, and no tests were rerun for this documentation update.

### Component-based legacy generation decision — 1 November 2026

Luca approved component-based legacy historical preparation, probabilities,
movement shocks and count generation, followed by aggregation to filed lists.
Recorded this as correction 2 in the correspondence audit, with physical filing
conditioning before counts, independent component movement innovations and the
existing shock-scale settings. The adopted poll-error treatment draws one
measurement realization per observed group and splits its share by fixed legacy
composition; shared measurement error is distinct from independent movements.

Both models' scenario political references now target components, including
historical/fitted components. Mandatory-reference cases retain national or full
territorial/chamber coverage; a future list-level control/translation remains
undecided. Reworded correction 5 to regularize historical-zero component logits
before list aggregation, retaining its other boundaries.

Luca explicitly superseded backward archive compatibility in the web app:
latest generator algorithms only, with earlier production code optionally
archived outside browser/worker dependencies for terminal comparison tests.
Updated the audit, active-plan wording and unified workflow's design-status note
to prevent the earlier v1–v3 routing requirement from carrying into the update.
The direct-cell redesign also narrows correction 3's allocation perimeter while
retaining physical reference-capacity checks. Implementation remains deferred
to the coordinated pass after remaining decisions are settled. Documentation
only; no code, data, defaults, tests or archives changed. `git diff --check`
passed; no tests rerun for this documentation update.

### Physical capacity and allocation perimeter decision — 1 November 2026

Updated correction 3 in the correspondence audit following Luca's agreement:
physical-cell conditioning and direct component count generation resolve the
municipal filing-union and greedy reconstruction defects. The active path needs
no municipal-to-college vote relocation or replacement transportation allocator;
aggregate cell counts by summation. Retain individual and joint physical
reference-capacity validation with the correct scopes/denominators, explicit
infeasibility reporting, structural filing zeros and electorate conservation.
A flow-based constraint feasibility check remains distinct from vote relocation.

Implementation remains deferred to the coordinated pass. Changed only the audit
and history; `git diff --check` passed. No code, data, defaults or tests changed;
no tests rerun for this documentation update.

### Pre-filing reference-date support decision — 1 November 2026

Luca clarified that component references anchor support at the reference date
before future filing. Recorded the sequence in correction 2 of the audit:
reference support, future movement, local support, physical filing conditioning,
counts and list aggregation. Final filed shares need not reproduce the original
or shocked pre-filing national shares. Both installed generators need adaptation:
their current reference application already incorporates filing restrictions,
although both subsequently allow future movement. The selected model can retain
its distinct movement mechanism, with distributional revalidation after reordering.

Revised correction 3 to supersede the previous individual/joint physical
reference-capacity validation requirement. No such solver is required for dated
pre-filing support references. Retain valid-distribution/metadata/coverage checks,
positive remaining probability mass after conditioning, filing zeros and
electorate conservation. An empty remaining-support vector is an explicit error,
not an invented allocation. Aligned correction 5's regularization boundary and
the active-plan wording. Implementation remains deferred to the coordinated pass.
Documentation only: audit, plan and history updated; no code, data, defaults or
tests changed. `git diff --check` passed; no tests rerun for this update.

### Conditioning and scenario-reference uncertainty decisions — 1 November 2026

Luca settled two further correspondence-policy decisions. Proportional
conditioning is deterministic within each simulated support vector, with no
additional substitution-error draw. Scenario-reference uncertainty applies only
to the selected model: legacy uses fixed reference-date political/non-valid
shares followed by national, regional and local movement shocks. Original
poll-group measurement uncertainty remains separate and unchanged.

Recorded these decisions in the focused audit. Geographic inference and
reference-backed unidentified splits remain under discussion; no proposed
estimator or split rule was adopted in this update. Model implementation remains
deferred to the coordinated pass. Documentation only; no model, data or default
changes. `git diff --check` passed; no tests rerun for this update.

### Geographic inference and unidentified-split adoption — 1 November 2026

Luca adopted the proposed model-specific geographic inference and selected-model
reference-backed split procedures. Updated the focused correspondence audit with
the legacy conditional-frequency pooling objective, national/regional borrowing,
local conditional-proportion completion and fixed preparation inputs. Retained
the selected profile estimator and its uncertainty conventions, adapting historical
comparisons to original offered sets rather than interpreting unavailable choices
as observed zeros.

Recorded the selected group-plus-contrast representation: intact grouped history,
component-reference calibration, local/regional/shared-group profile hierarchy,
shared group uncertainty and separate missing-history contrast uncertainty.
Current references may intervene on the group total without altering historical
counts. Overlapping evidence and identification checks remain required; no legacy
composition becomes a selected-model measurement.

Removed stale remaining-policy wording and updated the active plan to distinguish
settled policy from pending coordinated implementation and acceptance. Changes are
limited to the audit, plan and history; no model code, generated data or defaults
changed. `git diff --check` passed; no model tests rerun for documentation only.


### Adopted correspondence corrections implemented — 1 November 2026

Implemented the complete approved policy for the selected model and the revised
legacy derivative. Both now construct unrestricted component support at the
reference date, apply their own future movement, deterministically condition on
each physical ballot including non-valid support, and allocate counts directly
in that cell. Component counts aggregate to filed lists; municipal greedy vote
relocation and filing-capacity validation of pre-filing references leave the
active path. Exports distinguish dated reference support, future support before
filing, filed probabilities and realized list counts.

Legacy preparation preserves original counts/electorates, mandatory explicit
joint compositions and actual historical/poll offered sets. Conditional
frequency pooling and regional/national completion replace the all-or-nothing
municipality fallback. Only observed political zeros receive the half-vote
logit input; tiny positive decompositions remain unchanged. Disconnected positive
choice sets cannot be identified through an observed-zero alternative: national
relative masses require references, while regional unidentified masses borrow
the national donor. Legacy movement innovations are component-specific and
independent between components, retaining within-component chamber dependence.
One poll-group measurement error is shared by its fixed split, geography and
reused chambers. Complete smaller-choice-set questions constrain only their
original conditional denominator; partial questions keep their unreported
residual. Scenario-reference means are fixed for legacy, with all future shocks
starting at the reference date; their SDs remain selected-model inputs only.

The selected compiler keeps original heterogeneous ballot strata, grouped
observations and profile choice sets. Identical observation columns fit group
totals; reference-backed internal contrasts and other overlapping unidentified
directions stay separate. Group-first reference calibration changes the current
group total where requested, then splits each cell without altering that cell's
group mass or historical counts. Shared group-profile uncertainty and separate
unidentified contrast uncertainty avoid fabricated component measurements.

Keeping original strata exposed an ill-conditioned Gaussian covariance solve in
full-country preparation. Bounded noise whitening and Givens elimination compute
the same sufficient statistics without a new ridge; an independent reduced
covariance calculation agrees within 1e-8. The 256 MiB workspace gate, original
matrix-free fallback and residual check remain. Orthogonality and covariance
residuals are separately labelled, and elimination yields cooperatively for
worker cancellation. Unrestricted reference projection also bypasses an
unnecessary transport-flow test whose finite tolerance incorrectly rejected
positive residuals around machine precision; tiny support remains positive.

Luca explicitly chose selected-model reference estimates as the six missing
legacy default anchors (IdV, Futuro Nazionale, PLD/LibDem, +Uno, Progetto Civico,
PSI/civici). Default preparation records reference-only means and sample SDs from
256 deterministic draws before adding the anchors, plus seed and parent
fit/profile IDs. They are editable model-based assumptions, with selected-only
uncertainty and fixed legacy means. No missing runtime reference is silently
replaced. Default poll compositions, including dated Other complements, are
explicit authored inputs. Generated release assets were regenerated through the
release pipeline rather than edited manually.

Versioned provenance uses `component-prefiling-conditioning-v2`,
`fast-share-float64-v2`, `fast-share-prefiling-physical-cells-v2`, a revised profile
policy and composition key, plus transitive implementation identities. The browser
accepts only the current v5 reform workflow. Old scenario/result imports are
rejected, old browser research/comparison entry points are retired, and retained
historical comparators live outside runtime dependencies. Shared legal assembly
was extracted without changing scrutiny rules. Current worker/storage/statistics
regressions use frozen v5 sessions, including cancellation, ordered IDs, parallel
replay, crashes, quota failures and empty saved results. The current plain scenario
import and empty-result table selection were corrected during browser acceptance.
A startup regression also caught reference-component IDs in the historical
evidence key: seed/reference-only edits needlessly downloaded original history.
`browser-fit-evidence-v2` excludes intervention inputs, while generation still
validates mandatory reference coverage. The regression verifies no source reads
or fits after adding a component reference. This cache correction does not alter
the generation implementation identity or vote law.

Functional acceptance passed on the final model implementation
`implementation-v1-c3c448cf68330e5fd097c15a6bd86958da529e072939360e7915a41f62f9adac`,
catalog `fast-share-catalog-v1-27d815959e9a6a1f4193e4e68297577972e2899d56afc1a4daaaa3477f88b1fc`:

- `npm run acceptance:reform`: production asset integrity/vertical slice,
  preparation types, Svelte checks (zero errors/warnings), **1,296 unit tests**,
  and static builds at `/` and `/elezioni`. Each base passed **43** Chromium
  application regressions, **4** current-editor checks and **10** unified workflow
  checks across Chromium/Firefox, with no accepted skips or retries. Root build
  restored at the end.
- Final concurrent Chromium campaigns: **1,000/1,000 complete per model**, all
  **588** seats filled with **588 distinct candidates** in every run. Ten compute
  workers per model; measured run elapsed **725,693 ms selected** and
  **1,190,924 ms legacy**. Both passed full JSON.gz export, reimport, ledger
  comparison and frozen-scenario restoration, with no page errors. Archives were
  **178,103,484** and **208,724,721** bytes. Total campaign-suite wall time was
  **27.9 minutes**, including the archive checks.
- Six full default browser refits passed with stable fitted identities and
  elapsed times **12,784–14,482 ms**. The first maximum-dimension synthetic probe
  hit **38,076 ms** during concurrent work, exceeding its unchanged 30-second
  gate. The failed measurement and trace are retained at
  `data/derived/correspondence-corrections/contended-*` and `resource-artifacts/`.
  The isolated repeat passed all six normal-speed probes (**28,286–28,696 ms**,
  median **28,378 ms**) and the real quarter-CPU run (**117,423 ms**, below the
  unchanged 120-second gate). Maximum reported numerical workspace was
  **46,383,008 bytes**, below 256 MiB. The earlier failure is consistent with
  concurrent workload contention; the isolated pass does not erase that receipt.
- Current production checks passed three selected and three legacy legal runs,
  required-reference coverage, source integrity and matching semantic keys;
  release preparation checked **7,904 municipalities**, **15,810 physical cells**,
  **28 fitted / 34 future components**, 2,000 retained draws per chain and a
  complete 588-seat allocation. Built JavaScript contains none of the retired
  generation/greedy-filing implementation markers. The final acceptance curator
  rejects missing/failed reports or stale campaign identities; preparation types
  passed again after curation tooling was added. `git diff --check` passed.

Detailed local receipts: `data/derived/r26-21-acceptance/report.json`,
`data/derived/correspondence-corrections/node-summary.json`,
`data/derived/fv08-unified/browser/*-root-1000.json.gz.receipt.json`, and
`data/derived/fv08-unified/resources/`. The maintained compact receipt is
[test/fixtures/benchmarks/correspondence_corrections_acceptance.json](../../../test/fixtures/benchmarks/correspondence_corrections_acceptance.json).
These are functional and engineering checks, not predictive calibration.
FV-09 generated-output cleanup and public deployment remain separate work.


### M01/M02 specifications and semantic identities — 1 November 2026

Named the corrected component-based legacy law **M01** and the default joint-fit,
bounded-transfer law **M02**. Created self-contained current specifications and
equations in `docs/models/M01.md` and `docs/models/M02.md`; historical derivations
now point to them instead of presenting old Beta forecasts as current equations.
The specifications cover original offered sets, identification, fixed versus
uncertain references, geographical borrowing/profiles, chamber coupling,
movement, discrepancy, physical conditioning, multinomial counts, defaults,
provenance and limits. R's original generators and older research candidates do
not retroactively become these corrected laws.

Adopted independent **1.0.0** model-contract versions. SemVer describes changes
to each model's accepted inputs, probability law and output meanings: law/meaning
changes are MAJOR; optional compatible extensions are MINOR; law-preserving
clarifications/corrections are PATCH. This is contract stability, not predictive
calibration. Source/scenario/fitted-state/numerical/RNG/file-format/implementation
identities remain separate, and the runtime accepts exact supported model versions.

A shared registry supplies M01/M02 names, versions and labels. Current scenarios
store `generation.model` and `generation.modelVersion`; controls, worker dispatch,
resource estimates, default preparation, traces and acceptance workflows use
those identities. Model selection changes the model/version together. Unsupported
names/versions or inconsistent preparation fail. Current defaults were regenerated
through the release pipeline. Historical reports, archived comparisons, RNG
namespaces and reference-row IDs retain their original spelling for traceability
and seeded behavior; they are not accepted current selector aliases.

Before renaming, saved full-country ballot/candidate hashes for two simulation
IDs per model. The corresponding four runs after regeneration are byte-identical
when provenance metadata is excluded. No statistical equation, random stream,
legal scrutiny rule or scenario probability setting changed. The new model/version
metadata intentionally changes implementation/state binding identities.

Verification passed:

- **112 test files / 1,298 tests**, without skips, including exact model-version
  validation and M01 preparation matching.
- **20 browser workflow checks**: ten each at `/` and `/elezioni`, across Chromium
  and Firefox. Covered both model selections, exported/reimported version metadata,
  frozen restore, composition controls, M01 reference uncertainty controls and
  resource settings. The root production build was restored afterward.
- Full release regeneration passed source integrity, semantic bindings and the
  complete legal vertical slice: **7,904 municipalities**, **15,810 physical cells**,
  **28 fitted / 34 future components**, 2,000 retained draws per chain and 588 seats.
  Preparation TypeScript, Svelte check (zero errors/warnings) and production builds
  passed. Modified Svelte components also passed the official component checker.
- The four full-country pre/post naming comparisons preserved sampled ballots and
  candidate data exactly. This is not a fresh 1,000-simulation campaign per model;
  the earlier numerical acceptance remains historical evidence.
- Specification math delimiters and relative links were checked. Initial stale
  asset/consumer checks were resolved by completing the rename and regenerating
  defaults through the release pipeline.

The compact maintained receipt is
[model_identity_acceptance.json](../../../test/fixtures/benchmarks/model_identity_acceptance.json).
Detailed local logs use `data/derived/model-naming-*`. No publication or FV-09
cleanup was performed.
