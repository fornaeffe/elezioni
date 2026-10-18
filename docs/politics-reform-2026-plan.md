# Politics reform 2026: implementation plan

This is the operational plan for the reform item in
[`../MIGRATION_PLAN.md`](../MIGRATION_PLAN.md). It specifies future work; an
unchecked step is not implemented or verified. Work on one requested step at a
time and leave subsequent application features inactive.

## Reading map

- For the next task, use the checklist and its corresponding section below.
- For legal wording, use [`politics-reform-2026.md`](politics-reform-2026.md)
  and the referenced PDF; read only the articles needed for that step.
- For verified source versions, legal function/test boundaries and unresolved
  interpretations, use the [R26-01 specification](politics-reform-2026-specification.md).
  Use the current scoped transcription for operative wording; promulgated law
  and official corrections control conflicting official sources.
- For the approved vote, preference and chamber models, use the
  relevant section
  of [`politics-reform-2026-modelling.md`](politics-reform-2026-modelling.md).
- For completed work, decisions, verification, and limitations, use
  [`migration-history.md`](migration-history.md). Keep this plan directive-only.
- Stable repository instructions remain in [`../AGENTS.md`](../AGENTS.md).

## Architecture and completion rules

- Keep `politiche-r-parity-v1`, its generated inputs, golden behavior, and
  law-review exceptions available as the old-law alternative. The reform is a
  new implementation, not a correction of the parity algorithm.
- Put new legal stages under `web/src/lib/politics/reform-2026/`. Use small pure
  functions with explicit typed inputs, outputs, and injected seeded tie draws.
  Names below describe intended boundaries and may be refined during design.
- Keep literal applicable legal text and article/paragraph references close to
  each translated operation. Expose intermediate figures, quotas, remainders,
  seat movements, and reasons for admission or rejection for focused tests.
- Introduce a simulation-level entry point receiving both Camera and Senato.
  Adapt the old implementation through this entry point without changing its
  internal law behavior or random draw sequence. The reform coordinator must
  pair the same simulation across chambers before deciding the bonus.
- Keep statistical vote/candidate generation outside legal scrutiny. The core
  must accept constructed inputs independently of all three modelling choices.
- Keep ordinary seats, bonus seats, special-territory seats, and candidate
  identities distinguishable. A coalition bonus slate is not an arbitrary
  proportional distribution of bonus seats between member lists.
- Apply the provisional I03/I05/I06/I07/I08 policies in the specification, keeping
  them distinguishable from express statutory rules and revisable on further
  legal evidence. Warn in affected simulations when an I03 fallback draw or an
  I07 cross-region recovery is used.
- Do not silently turn missing preferences into zero or absent chamber data
  into independent draws. Record assumptions and unresolved outputs explicitly.
- Use small tracked fixtures with independently worked expected results for
  the reform. R golden fixtures prove old-law parity, not new-law correctness.
- Complete each implementation step only after its specified tests and relevant
  regression checks pass. Record evidence and limitations in history, then tick
  its checklist item. A decision step needs a recorded choice and evidence; it
  does not imply that its dependent generator has been implemented.

## Checklist and dependencies

The default order is below. R26-02/03/04 model choices, R26-05 contracts, R26-06
numerical primitives, R26-07 geography/seat tables, R26-08 electoral figures
and rankings, R26-09 admission/preliminary special winners and R26-10 special
territories are complete; continue with R26-11 joint bonus decision. Model
selection does not
imply fitted parameters, implemented generators or production acceptance.
R26-05 through R26-18 can use constructed legal inputs independently of R26-19
calibration and generation.

| Done | Step | Deliverable | Dependencies |
| --- | --- | --- | --- |
| [x] | [R26-01](#r26-01-normative-specification) | Norm-to-function/test specification | None |
| [x] | [R26-01a](#r26-01a-senate-recovery-precedent) | Historical Senate recovery evidence and provisional interpretation | 01 |
| [x] | [R26-02](#r26-02-bonus-only-vote-modelling) | Bonus-only vote decision and bias protocol | 01a |
| [x] | [R26-03](#r26-03-preference-modelling) | Preference decision and supported outputs | 01 |
| [x] | [R26-04](#r26-04-camera-senato-vote-modelling) | Chamber differentiation decision | 01 |
| [x] | [R26-05](#r26-05-scrutiny-contracts) | Typed legal inputs, outputs, validation | 01-04 |
| [x] | [R26-06](#r26-06-numerical-primitives) | Quotas, truncation, remainders, ties | 05 |
| [x] | [R26-07](#r26-07-territories-and-seat-endowments) | Versioned geography and seat tables | 01, 05 |
| [x] | [R26-08](#r26-08-electoral-figures-and-rankings) | Vote attribution, totals, rankings | 05-07 |
| [x] | [R26-09](#r26-09-admission) | Camera and Senato admission | 08 |
| [x] | [R26-10](#r26-10-special-territories) | Special elections and seat totals | 09 |
| [ ] | [R26-11](#r26-11-joint-bonus-decision) | Joint bonus eligibility and reasons | 09 |
| [ ] | [R26-12](#r26-12-camera-national-allocation) | Camera national allocation and cap | 10-11 |
| [ ] | [R26-13](#r26-13-camera-territorial-allocation) | Camera constituencies and colleges | 12 |
| [ ] | [R26-14](#r26-14-senato-regional-allocation) | Senato regional allocation and cap | 10-11 |
| [ ] | [R26-15](#r26-15-senato-list-and-college-allocation) | Senato lists and colleges | 14 |
| [ ] | [R26-16](#r26-16-proclamations-and-multiple-elections) | Candidate proclamations and precedence | 10, 13, 15 |
| [ ] | [R26-17](#r26-17-exhaustion-and-replacements) | Exhaustion, replacements, bonus shortfalls | 01a, 16 |
| [ ] | [R26-18](#r26-18-composition-and-registry) | Composed scrutiny and algorithm registration | 06-17 |
| [ ] | [R26-19](#r26-19-scenarios-and-generation) | Scenario compatibility and generators | 02-04 resolved, 07, 18 |
| [ ] | [R26-20](#r26-20-worker-results-and-ui) | Worker, selector, results, exports | 19 |
| [ ] | [R26-21](#r26-21-acceptance-and-performance) | Application and performance acceptance | 20 |

### R26-01: Normative specification

Use the [normative specification](politics-reform-2026-specification.md) for
sources, operation/test boundaries, scope and provisional interpretation policies.
Maintain the [transcription](politics-reform-2026.md) as the current consolidated
text of all and only provisions translated by this simulation, including
constitutional inputs. Incorporate verified legal corrections directly and do
not retain a transcription error/omission register. Keep statutory wording
separate from provisional simulator interpretations.

Recheck later official corrections and the announced republication with notes
before production acceptance. Update the transcription, source version and
affected specification/test boundaries together. Apply approved provisional
policies and keep genuinely unresolved inputs and outputs explicit.

### R26-01a: Senate recovery precedent

Use the approved [I07 recovery protocol and worked expectations](politics-reform-2026-specification.md#senate-initial-recovery-i07)
and the [precedent review](migration-history.md#2026-10-16-r26-01a-senate-recovery-precedents-and-approved-i07).
Keep initial exhaustion, later vacancies and the new bonus-shortfall route
distinct. Reassess I07 if further legal evidence arises; do not import the
historical unsuccessful-uninominal recovery abolished by the reform.

The research decision does not implement recovery. Translate the approved
protocol and its fixture expectations in R26-17, retaining warnings and traces
through R26-18/20.

### R26-02: Bonus-only vote modelling

Apply the [approved final-total semantics and implementation checks](politics-reform-2026-modelling.md#bonus-only-votes-r26-02).
Generate final attributed list totals; preserve explicit component inputs and
their separately testable legal attribution functions. Apply the same convention
separately to special candidate-only votes at uninominal level. Keep unknown
ballot composition explicit and prevent double attribution.

Use the [decision evidence and limits](migration-history.md#2026-10-17-r26-02-approved-final-attributed-vote-model)
when validating R26-08/19. Historical-source reconciliation is an implementation
requirement; this completed modelling decision does not implement a generator,
settle other modelling choices or prove empirical absence of bias.

### R26-03: Preference modelling

Apply the [approved model and validation contract](politics-reform-2026-modelling.md#preferences-r26-03):
fit synthetic preferences from historical patterns with supplied valid counts
taking precedence. Preserve shared candidate strength/local variation and generate
valid preference-combination counts. Keep source reconciliation, fitting and
held-out/sensitivity validation in R26-19, using the
[decision evidence](migration-history.md#2026-10-17-r26-03-approved-historical-pattern-preference-model).
Report candidate/recovery outcomes as conditional on the preference profile;
distinguish missing from zero. Completing this decision does not fit the model
or implement its generator.

### R26-04: Camera-Senato vote modelling

Apply the [approved paired-chamber model and precedence contract](politics-reform-2026-modelling.md#camera-senato-votes-r26-04):
shared shocks, historical contrasts and correlated residuals. Common national/
local shares anchor model-derived chamber references; explicit chamber shares
win at the same scope. Preserve geographic specificity, mean-mode uncertainty,
abstention and normalization. Prepare historical Senate inputs within R26-19,
using the [source review](migration-history.md#2026-10-17-r26-04-approved-paired-chamber-vote-model).
Keep the 2022 same-age baseline distinct from older 25+ Senate electorates.
Model selection does not establish fitted covariance or generator behavior.

### R26-05: Scrutiny contracts

Use the [implemented contract boundary](../web/src/lib/politics/reform-2026/README.md)
for input construction, validation and result types. Retain its isolated versioned
envelope until scenario/worker integration in R26-18/19/20.

Define reform-specific contracts for per-chamber votes, coalition links, list
coverage, candidate identities and affiliations, ordinary/bonus slates,
preferences, special contests, and seat endowments. Distinguish raw direct and
bonus-only inputs from already attributed list totals through explicit variants.
Carry approved R26-02/03 representation, supplied/generated preference provenance,
model/profile/source versions and conditional-output metadata. Add R26-04's
common/per-chamber references, resolved geographic scope, paired simulation ID,
covariance/profile metadata and supplied/model-derived provenance.

Validate identities, territorial/political references, nonnegative finite vote
counts, and candidacy constraints, including seven-slot lists, gender rules,
multiple candidacies, and required ordinary/bonus slate links where applicable.
Apply chamber-specific coverage and single-chamber no-bonus-slate rules from
the specification; keep special-territory compatibility explicit. Include the
constitutional electorate/candidate age constraints and reserve provisional
policy identifiers, draw traces/warnings and seat-table provenance in outputs.
Distinguish malformed candidature input from a legally handled later shortfall.
Version scenario/snapshot contracts as needed without reinterpreting old files.

Tests: valid minimal examples, invalid references, duplicate slots, incompatible
candidacies, and prevention of double vote attribution. Keep legacy contracts usable.

### R26-06: Numerical primitives

Use the [implemented numerical boundary](../web/src/lib/politics/reform-2026/README.md#numerical-execution-r26-06):
`allocateByQuotaAndRemainders()`, explicit tie resolvers, and
`truncateToSixDecimals()` with the precise numeric policy of each provision.
Support truncated integer quotas and non-truncated quotas where prescribed;
do not assume the existing Hare-Niemeyer helper is interchangeable. Avoid hidden
global RNG state and arbitrary floating-point tolerances that change legal ties.
Carry returned draw traces and warnings into composed results; record applied
interpretation versions at the owning legal stage.

Tests: independently worked allocations where truncation matters, successive
tie criteria, deterministic injected draws, zero seats, no eligible subjects,
zero quotas, and impossible allocations with explicit outcomes. Preserve the
legacy helpers' behavior. Exercise I03's final seeded-draw fallback after all
express legal criteria and require a warning in every affected simulation.

### R26-07: Territories and seat endowments

Use the versioned catalog of constituencies, colleges, and special territories;
check the changed Bolzano Senate boundaries. Supply bonus-seat distributions and
ordinary college capacities both with and without the bonus (Camera art. 3;
Senato art. 1). Check the geographic resolution of existing source data before
reusing it for revised boundaries.

Use prescribed decree tables when available. Until then generate census-based
provisional endowments under specification I08, using the latest legally declared
population census, constitutional constraints and the documented interim quota/
remainder method for bonus seats and college capacities. Version and label these
tables as provisional; do not describe that method as an official decree formula.
Replace them with verified official tables when available, preserving replay of
older runs with their original versions. A missing decree alone does not block
the provisional path; statutory/territorial conflicts still need resolution.
Use the shared Node/TypeScript acquisition and geography/census pipeline in
[`../data/README.md`](../data/README.md). Keep originals in `data/sources` and
intermediates in `data/derived`; remove legacy `dati` only after the whole
data-preparation migration is complete and its consumers have migrated.
Use definitive machine-readable geography, auditing the presentation-oriented
2020 spreadsheets. Prefer available fine census data; when it is unavailable,
retain usability through explicit, versioned population estimates conserving the
legal municipal total. Preserve those disclosures in inputs/results. Historical
vote/preference importers and calibration remain subsequent work, including R26-19.
Regenerate dependent defaults and catalogs through scripts, never by editing
generated files. Keep the reform application path inactive until its integration steps.

Tests: geographic coverage, unambiguous joins, chamber/territory seat totals,
70/35 bonus totals, and consistent alternative college capacities.

### R26-08: Electoral figures and rankings

Use the [implemented figures boundary](../web/src/lib/politics/reform-2026/README.md#electoral-figures-and-rankings-r26-08).
Supply completed ordinary and special college list figures separately. Treat
Camera TAA proportional figures as a view of the same territorial ballots;
do not count its proportional row again. Raw special attribution remains R26-10.

Translate Camera art. 77 and Senato art. 16 into functions including
`distributeBonusOnlyVotes()`, `aggregateElectoralFigures()`, and
`rankPreferenceCandidates()`. Test the attribution rule with explicit raw inputs
alongside R26-02's approved final-total generation path. Accept already
attributed totals through a path that performs no second allocation. Test exact
downstream representation equivalence with matched candidate/preference inputs
and isolated attribution/downstream legal draws.

Keep national figures for admission, first place, and bonus eligibility separate
from ordinary allocation figures excluding special territories where required.
Calculate list and individual percentages with the legally specified denominator.

Tests: vote conservation, integer remainders at the prescribed geographic level,
zero-support cases as specified in R26-01, all aggregation levels, special-vote
inclusion/exclusion, capolista exclusion from preference rankings, and list-order
preference ties. Do not require statistical preference generation for these tests.

### R26-09: Admission

Implement `determineCameraAdmission()` and `determineSenateAdmission()` (Camera
art. 83(1)(e); Senato art. 16-bis(1)(e)). Return reasons and distinguish admitted
coalitions, standalone lists, and lists admitted inside coalitions. Include the
best otherwise excluded list of each admitted coalition. Do not carry the old
law's admission rules into the reform by default.

Tests: just below/at/above 3%, 10%, and applicable 20% thresholds; linguistic
minorities and special uninominal wins; Senato regional access; failed coalition
with independently admitted list; best subthreshold list and provisional warned
tie draws under I03.
Stage any special-territory winner calculation needed for admission before the
special proportional allocation, avoiding a circular dependency with R26-10.

### R26-10: Special territories

Use the [implemented special-territory boundary](../web/src/lib/politics/reform-2026/README.md#special-territories-r26-10)
for stage order, provisional proclamations and cap-ready entitlement totals.

Translate Camera arts. 92-93-octies and Senato arts. 20-21-ter as applicable to
initial scrutiny. Separate special uninominal proclamations, attribution of
candidate-only votes to lists, and Camera Trentino-Alto Adige proportional seats.
Reuse preliminary winners from R26-09 rather than recomputing them inconsistently.

Tests: winners linked to excluded lists; younger-candidate Camera versus
older-candidate Senato ties; special vote totals for national admission/bonus;
special seats counted in the caps but excluded from ordinary territorial seats;
Camera special proportional allocation and candidacy restrictions. Delimit later
supplementary elections rather than claiming to simulate them here.

### R26-11: Joint bonus decision

Implement `determineGovernabilityBonus()` (Camera art. 83 and Senato art.
16-bis). Receive both chambers' admitted subjects, national totals, declared
cross-chamber links, and which chambers are renewed. Return the linked winners
and 70/35 awards or a structured reason for no award. This stage needs no seat
cap computation; special seats become necessary for the later cap checks.

Tests: exactly 42%, one or both chambers below it, different or unlinked first
subjects, missing links, single-chamber renewal, and first-place ties according
to R26-01. Include constructed chamber outcomes that the current shared vote
generator cannot naturally produce.

### R26-12: Camera national allocation

Translate art. 83 national allocation into separate paths: no bonus, bonus with
no cap correction, and bonus with the 220-seat cap. Isolate provisional subject
allocation, cap verification, the new majority/minority allocation, and seats
among coalition lists. Preserve the bonus when the cap changes ordinary seats.

Tests: all paths, exactly/over 220 including special seats and excluding foreign
seats, majority ordinary target `150 - special seats`, minority-seat total, and
quota/remainder traces. Verify the statutory 314-seat scope against R26-07.

### R26-13: Camera territorial allocation

Translate art. 83 constituency distribution and art. 83-bis college distribution.
Separate provisional allocations from corrections for surplus/deficit subjects,
and subject allocation from allocation among coalition lists. Implement the cap
path's majority/minority indices and six-decimal truncations explicitly.
Apply I05 symmetrically to the majority and all minority indices before local
normalization, with boundary tests and a recorded provisional policy version.

Tests: already reconciled matrices, transfers within a constituency, prescribed
fallback transfers across constituencies, used versus unused remainders,
exclusion of subjects already at their target, and every tie criterion. Assert
the specific national and territorial totals required at each correction stage;
do not assume every intermediate transfer preserves every local total.

### R26-14: Senato regional allocation

Translate art. 16-bis provisional regional allocation and the 113-seat cap.
Isolate ranking of used remainders across regions, removal of excess ordinary
seats, repeated regional-percentage passes, and corresponding regional
reassignment. Preserve the 35 bonus seats and count special seats in the cap.
Apply I06: resolve equal removal ranks by seeded draw with the I03 warning,
trace every used/unused transition and consume each remainder only once.

Tests: no bonus, bonus without correction, exactly/over 113, ordinary majority
target including special seats of 78, exhausted remainder ranking, repeated
passes, one-seat protection, and recipient selection from unused remainders or
regional vote totals. Check preservation of regional endowments.

### R26-15: Senato list and college allocation

Translate art. 17 regional coalition-to-list and college allocation, separating
ordinary and cap-corrected majority/minority paths. Share primitives with Camera
only where their formulas and tie policies are equivalent.

Tests: single/multiple colleges, regional list targets, surplus/deficit
corrections, six-decimal indices, and the Senato-specific vote totals used to
break ties. Preserve each region's seat total.

### R26-16: Proclamations and multiple elections

Translate Camera arts. 83-bis, 84-85, 93-octies and Senato arts. 17-17-bis.
Separate bonus-slate election, capolista priority, preference ranking, and
resolution of multiple elections. Apply bonus/ordinary and special
uninominal/plurinominal precedence as prescribed; retain actual candidate list
affiliation without inventing a coalition bonus split.

Tests: every precedence, capolista elected in multiple colleges with the lowest
list percentage, non-capolista elected with the highest individual percentage,
mixed roles, and no duplicate definitive elected identity. Use supplied
preferences in legal fixtures independently of R26-03 fitting and generation.

### R26-17: Exhaustion and replacements

Translate Camera arts. 84, 86, 83(1-quinquies) and Senato arts. 17-bis, 19,
16-bis(1-quinquies). Model ordered recovery operations, exhaustion after multiple
election resolution, and missing bonus candidates. Apply the approved
[I07 protocol](politics-reform-2026-specification.md#senate-initial-recovery-i07):
ordinary initial recovery exhausts regional C84(2),(5) remedies before residual
same-list cross-region recovery through S19(2)/C86(2)/C84(4). Keep the express
S16-bis(1-quinquies) bonus route separate and do not enable C84(7) through the
ordinary route. Compose proclamation and recovery until the legally
defined terminal state with an explicit progress condition.

Tests: every recovery level and remainder priority, coalition recovery, bonus
shortfall, repeated subentro, fully exhausted candidates, and the worked I07
priority/boundary cases. Preserve original entitlement and actual election
territories, seat identity and candidate/remainder state in recovery traces;
warn on each I07 cross-region recovery. Distinguish seats entitled from seats
filled and report unfilled seats without inventing further redistribution or
recomputing the ordinary allocation to hide a geographic shift. Do not extend
into future vacancies or supplementary elections beyond the agreed scope.

### R26-18: Composition and registry

Compose `runPoliticsReformScrutiny()` from tested stages. Introduce the common
simulation-level execution boundary and register `politiche-reform-2026-v1`
alongside the legacy adapter. Keep a complete direct-input execution path before
activating production generation or the UI selector.

Tests: independently verified complete fixtures for all bonus/cap branches,
special territories, replacements, paired simulation identity, deterministic
draws, and meaningful conservation invariants. Run existing old-law golden and
registry checks. Explicitly test how unknown/unavailable algorithm IDs are
handled; never represent a fallback run as the requested reform algorithm.

### R26-19: Scenarios and generation

Implement the recorded R26-02/03/04 decisions with separate seeded streams for
shared/chamber-specific vote components, candidates, synthetic preferences and
legal draws. Preserve approved mean-mode shares, abstention, correspondences, and
local overrides. Adapt list coverage, candidate templates, seven-slot lists,
bonus slates, affiliations, and compatible multiple candidacies.

Add explicit version conversions and validation for scenario/snapshot changes;
preserve old algorithm scenarios and prevent silent model switches. Persist
model identifiers, assumptions, seeds, and data version so runs can be reproduced.
Reuse existing historical/static inputs only at their verified resolution.
Reconcile each source's direct-list/candidate-only categories to the reform's
final-total semantics using the historical attribution rules; record finer
geographic reconstruction assumptions and verify totals before calibration.
Version this reform preparation without altering the legacy parity path.
Fit R26-03 using its source-reconciliation, roster/gender, pooling, valid-subset,
held-out validation and paired sensitivity protocol. Honour supplied counts and
the direct-versus-attributed preference budget. Version fitted parameters and
profiles separately from model code; preserve candidate identity across colleges.
Acquire and prepare paired Camera/Senato history for R26-04, starting with the
reviewed 2022 common-age baseline. Audit older electorate-age differences and
reconcile geography, attribution, coverage and special-territory inputs. Fit or
explicitly label joint covariance/contrast assumptions; preserve marginal
uncertainty rather than adding unbudgeted chamber noise. Implement common-anchor
derivation, per-chamber precedence and geographic specificity with traces.

Tests: each decision's algebraic, statistical, and sensitivity protocol; valid
candidacies and preferences; chamber-specific geographic aggregation; scenario
round trips; stream isolation; and a production-data vertical slice. Any
unresolved decision must remain an explicit blocker for its dependent generator.

### R26-20: Worker, results, and UI

Dispatch paired chambers per simulation with consistent global IDs, progress,
existing chunking, and the 1000-run limit. Expose the two real algorithms in a
selector only when both work end to end. Make the default and old saved-file
behavior explicit; do not silently reinterpret a legacy run as the reform.

Update typed presentation helpers and derive charts from their tables. Include
bonus eligibility/reasons, bonus seats, cap effects, special seats, model
assumptions, applied provisional interpretation IDs, I03 draw warnings, I07
cross-region recovery warnings and entitlement/election territory traces, census
versus decree seat-table provenance, and any candidate-output limitations.
Keep ordinary list seats and coalition bonus identities distinguishable.
Expose R26-02's vote model ID, representation and conversion provenance; keep
bonus-only composition unspecified rather than reporting it as zero.
Expose R26-03's supplied/generated provenance, model/profile/source versions and
assumptions. Separate vote-based entitlement from preference-dependent filled
seats, recovery and candidate outcomes; label synthetic probabilities as
conditional. Present profile sensitivity separately from calibrated uncertainty.
Expose R26-04's paired model/profile/source versions, covariance assumptions and
common/supplied/model-derived chamber reference provenance, including resolved
geographic scope. Explain the common anchor, explicit chamber precedence and
continuing mean-mode uncertainty; report joint-bonus dependence sensitivity.
Persist scenario, algorithm/model
identity, and worker result in structured exports through the generic export
module; keep CSV table-oriented. Apply the existing severity conventions.

Tests: both algorithm selections, paired worker execution, old/new scenario and
result imports, replay/export, candidate limitations, explanatory messages,
and politics/regional browser regressions. Add comparison UI only as a separately
requested feature; selecting the alternative is sufficient for this plan.

### R26-21: Acceptance and performance

Run focused tests during each step. At application completion run, from `web/`,
`npm run check`, `npm run test`, `npm run build`, then `npm run test:e2e`.
Build and e2e must run sequentially. Verify both politics algorithms and the
completed regional workflow, including required generated-artifact availability.
Small reform law fixtures must run without the large ignored artifacts; clearly
identify skips in tests which genuinely depend on those artifacts.

Benchmark 10, 100, and 1000 simulations with data/model/algorithm versions,
seed, environment, elapsed times, and peak memory where measurable. Separate
generation, scrutiny, and presentation costs. Keep the existing comparable
old-law R/browser gate; do not invent a reform R oracle or claim equal legal
outputs across laws. If no matching R reform workflow exists, state the limit
of performance comparisons and assess reform costs against the browser baseline.
Apply the existing 10x stop rule where a matching full R workflow is available.

Acceptance requires the promised output scope to be implemented and accurately
labelled. A candidate-limited model is not full candidate-level acceptance.
Recheck available official corrections and decree tables. Census-based I08
tables permit acceptance explicitly labelled provisional until a decree exists;
they do not establish that official decree allocation has been implemented.
Record unresolved limits in history and leave affected checklist items open.
