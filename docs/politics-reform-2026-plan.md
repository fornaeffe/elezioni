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
- For the three unresolved statistical choices, use only the relevant section
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

The default order is below. Complete R26-01a before starting R26-02.
R26-02 through R26-04 are explicit discussions with
Luca, not choices already authorized by approval of this document. If a choice
is deferred, R26-05 through R26-18 may proceed using constructed legal inputs;
keep dependent production generation and claims of completeness blocked.

| Done | Step | Deliverable | Dependencies |
| --- | --- | --- | --- |
| [x] | [R26-01](#r26-01-normative-specification) | Norm-to-function/test specification | None |
| [x] | [R26-01a](#r26-01a-senate-recovery-precedent) | Historical Senate recovery evidence and provisional interpretation | 01 |
| [ ] | [R26-02](#r26-02-bonus-only-vote-modelling) | Bonus-only vote decision and bias protocol | 01a |
| [ ] | [R26-03](#r26-03-preference-modelling) | Preference decision and supported outputs | 01 |
| [ ] | [R26-04](#r26-04-camera-senato-vote-modelling) | Chamber differentiation decision | 01 |
| [ ] | [R26-05](#r26-05-scrutiny-contracts) | Typed legal inputs, outputs, validation | 01; interfaces reserved for 02-04 |
| [ ] | [R26-06](#r26-06-numerical-primitives) | Quotas, truncation, remainders, ties | 05 |
| [ ] | [R26-07](#r26-07-territories-and-seat-endowments) | Versioned geography and seat tables | 01, 05 |
| [ ] | [R26-08](#r26-08-electoral-figures-and-rankings) | Vote attribution, totals, rankings | 05-07 |
| [ ] | [R26-09](#r26-09-admission) | Camera and Senato admission | 08 |
| [ ] | [R26-10](#r26-10-special-territories) | Special elections and seat totals | 09 |
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

Discuss whether separate bonus-only ballots need to be generated despite the
absence of calibration data. Assess Luca's working hypothesis: generated list
votes already include direct list votes plus the lawful redistribution of
bonus-only votes. It remains a proposal until this decision is recorded.

Use [the options and bias checks](politics-reform-2026-modelling.md#bonus-only-votes-r26-02).
Specify the generation-to-scrutiny boundary so an already attributed vote is
never redistributed twice. Preserve the separately testable legal attribution
function for explicit inputs even if production generation does not use it.

Acceptance: record the selected semantics, evidence, limits of any bias claim,
and criteria to apply in R26-19. A deferred decision is not completion.

### R26-03: Preference modelling

Discuss whether and how to simulate preferences without calibration data. Use
[the dedicated decision protocol](politics-reform-2026-modelling.md#preferences-r26-03)
to distinguish synthetic preferences, supplied preferences, and a limited mode
without candidate-level predictions. Define effects on multiple elections,
candidate exhaustion, and seat recovery before promising exact final results.

Acceptance: record the selected model or explicit supported limitation, required
inputs, sensitivity checks, and output semantics. Keep missing and zero distinct.

### R26-04: Camera-Senato vote modelling

Discuss retaining the shared generated territorial vote realization versus
modelling correlated chamber differences. Use
[the dedicated decision protocol](politics-reform-2026-modelling.md#camera-senato-votes-r26-04).
Assess the effect of the joint bonus condition rather than assuming independent
chamber draws or identical votes are neutral defaults.

Acceptance: record the selected dependence structure, any parameters and their
status as assumptions, reproducibility rules, and joint-bonus sensitivity checks.

### R26-05: Scrutiny contracts

Define reform-specific contracts for per-chamber votes, coalition links, list
coverage, candidate identities and affiliations, ordinary/bonus slates,
preferences, special contests, and seat endowments. Distinguish raw direct and
bonus-only inputs from already attributed list totals through explicit variants.
Reserve modelling metadata and supported-output status without choosing models.

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

Implement `allocateByQuotaAndRemainders()`, explicit tie resolvers, and
`truncateToSixDecimals()` with the precise numeric policy of each provision.
Support truncated integer quotas and non-truncated quotas where prescribed;
do not assume the existing Hare-Niemeyer helper is interchangeable. Avoid hidden
global RNG state and arbitrary floating-point tolerances that change legal ties.

Tests: independently worked allocations where truncation matters, successive
tie criteria, deterministic injected draws, zero seats, no eligible subjects,
zero quotas, and impossible allocations with explicit outcomes. Preserve the
legacy helpers' behavior. Exercise I03's final seeded-draw fallback after all
express legal criteria and require a warning in every affected simulation.

### R26-07: Territories and seat endowments

Prepare a versioned catalog of constituencies, colleges, and special territories;
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
Keep any generator changes limited to the reform's required data; the general
data-preparation migration remains deferred. Regenerate dependent defaults and
catalogs through scripts, never by editing generated files.

Tests: geographic coverage, unambiguous joins, chamber/territory seat totals,
70/35 bonus totals, and consistent alternative college capacities.

### R26-08: Electoral figures and rankings

Translate Camera art. 77 and Senato art. 16 into functions including
`distributeBonusOnlyVotes()`, `aggregateElectoralFigures()`, and
`rankPreferenceCandidates()`. Test the attribution rule with explicit raw inputs
regardless of the future production model. Accept already attributed totals
through a path that performs no second allocation.

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
preferences in legal fixtures even if their statistical model is still deferred.

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
votes, candidates, preferences if any, chamber differences if any, and legal
draws. Preserve approved mean-mode shares, abstention, correspondences, and
local overrides. Adapt list coverage, candidate templates, seven-slot lists,
bonus slates, affiliations, and compatible multiple candidacies.

Add explicit version conversions and validation for scenario/snapshot changes;
preserve old algorithm scenarios and prevent silent model switches. Persist
model identifiers, assumptions, seeds, and data version so runs can be reproduced.
Reuse existing historical/static inputs only at their verified resolution.

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
