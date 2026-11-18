# Politics reform 2026: modelling decisions

Read the section corresponding to R26-02, R26-03, or R26-04 in
[`politics-reform-2026-plan.md`](politics-reform-2026-plan.md). R26-02, R26-03 and
R26-04 are approved. Their R26-19 implementation, fitted preference parameters,
source reconciliation and assumed chamber profiles are documented in the
[generation guide](politics-reform-2026-generation.md). Application integration
and acceptance remain R26-20/21. No calibrated model of bonus-only ballot
composition or future national chamber-error covariance is established here.
Constructed data can test mechanics and sensitivity;
it cannot establish the real distribution of unobserved voting behavior.

For each decision, present options and evidence to Luca, then record the chosen
semantics, rejected alternatives and rationale, supported outputs, assumptions,
verification criteria, and conditions for revisiting it in
[`migration-history.md`](migration-history.md). Update active directives after
the choice. Approval of one decision does not settle the other choices.
Any experiment used for a decision must be reproducible and distinguish a
modelling assumption from a measured electoral fact.

## Bonus-only votes (R26-02)

### Approved semantics

**Approved by Luca on 17 October 2026:** generate final attributed list totals;
retain explicit components for supplied scenarios and legal tests. The
[decision record](migration-history.md#2026-10-17-r26-02-approved-final-attributed-vote-model)
contains the alternatives, rationale, arithmetic evidence and source-data limit.

- Interpret generated list totals, scenario shares and local overrides as
  support after lawful attribution: direct votes plus attributed bonus-only
  votes. Those ballots are already included in valid-vote totals. Their count
  is unknown, not zero; do not add ballots or redistribute these totals again.
- Preserve two explicit input variants: final attributed totals with provenance,
  or direct list totals plus explicit bonus-only/candidate-only counts. Only
  the component variant invokes the pure legal attribution functions, once,
  before admission thresholds. Do not generate random bonus-only counts by
  default or infer a ballot decomposition from a final total.
- Apply the final-total convention separately to special uninominal territories:
  attributed list totals include candidate-only attribution at uninominal level.
  For generated inputs, each special candidate's total is the sum of its linked
  lists' final totals. Preserve separately supplied candidate totals for explicit
  scenarios and validate consistency after attribution. These are candidate-only
  votes, not ordinary bonus-only ballots.
- Persist the model identifier `final-attributed-list-totals-v1`, input
  representation and source/conversion provenance in reproducible results and
  exports. Describe ballot composition as unspecified; do not report predicted
  bonus-only participation or infer preference counts. R26-03/04 remain separate.
- Reconcile historical direct-list/candidate-only categories before calibrating
  the reform's final-total generator. Audit each source, use its historical
  attribution rules at their legal geographic level, and document any finer
  geographic reconstruction assumption. Do not merely relabel a raw list field
  as a final total. Keep the legacy algorithm/data path unchanged.

Revisit this choice if calibrated bonus-only data become available, ballot
composition becomes a requested output, or validation exposes a material
limitation. Supplied components remain supported without assuming a calibrated
random composition model.

### Equivalence and bias protocol

For one ordinary plurinominal college and coalition, let `D[l]` be direct list
votes, `B` bonus-only votes, and `A(D, B)[l]` their legal attribution, including
integer remainders. Final list votes are `T[l] = D[l] + A(D, B)[l]`.

1. Apply R26-01's exact geographic and rounding rules in R26-08: ordinary
   attribution at plurinominal college, special attribution at uninominal college.
   Never substitute attribution at municipal or national level without proving
   equivalence. Aggregation and integer remainder allocation may not commute.
2. Use `T[l] = D[l] * (1 + B/sum(D))` before integer rounding where `sum(D) > 0`,
   and test the actual integer operation separately. In an ideal real-valued
   construction `D[l] = (1 - beta) * T[l]` and `B = beta * sum(T)`, the final
   proportions are preserved for `0 <= beta < 1`. Rounding can break exact
   reconstruction, and this identity says nothing about unknown behavior.
3. Feed identical final `T` to the direct-total path and to the downstream part
   of the explicit path. All subsequent figures, admissions, bonus decisions,
   seats, and supported candidate results must agree with identical candidate/
   preference inputs and the same downstream legal draws. Isolate attribution
   draws so their consumption cannot shift later draws. Attribution-stage traces
   may differ between representations.
   This proves representation equivalence, not empirical absence of bias.
4. Construct integer `D/B` scenarios across list sizes, coalition structures,
   local concentration, ballot-only shares, and vote volumes. Include zero
   direct support, small lists, special territories where relevant, and cases
   around 3%, 10%, 20%, 42%, and both caps. A no-support rule must come from the
   normative specification; do not silently distribute equally.
5. Compare explicit attribution against the approved final-total model under
   matched, stated counterfactual assumptions and comparable vote totals.
   Separate representation/rounding error from a changed distribution of final
   votes. Final `T` alone does not uniquely identify historical `D/B` components.
6. Check systematic signed differences by list size, coalition, and territory,
   as well as admission rates, bonus frequencies, expected seats, and cap
   activation. Define equivalence margins and a replication/seed budget before
   examining results. Use paired runs where meaningful, quantify Monte Carlo
   uncertainty, and rotate subject identities to detect tie-order effects.
7. Keep sensitivity to unobserved behavioral correlations separate from rounding
   checks. A correlation between bonus-only behavior and list/local strength is
   a different hypothesis, not a software bias automatically excluded by the
   proportional identity. Use exact worked boundary cases as well as random runs;
   rare threshold effects must not disappear in overall averages.

R26-08/19 implementation acceptance: no double attribution or unexplained
directional implementation effect; an explicit bound or measured sensitivity
for material rounding/model effects; and a qualified statement of which
hypotheses were tested. Do not claim unconditional absence of systematic bias
without data. If material bias remains,
revise the model or record the restriction before production integration.
Require exact downstream representation equivalence (zero discrepancies);
the less-than-one-vote bound per list/college concerns integer attribution versus
its ideal proportional value for fixed `D/B`, not arbitrary integer
reconstruction from a target `T` or a bound on seat changes. Full scrutiny
equivalence and conditional sensitivity checks are recorded under R26-19; they
do not identify unobserved ballot behavior.

## Preferences (R26-03)

### Approved model and input contract

**Approved by Luca on 17 October 2026:** fit a synthetic model informed by
historical preference patterns, with supplied valid preference counts taking
precedence. The [decision record](migration-history.md#2026-10-17-r26-03-approved-historical-pattern-preference-model)
contains the source audit, alternatives, rationale and transfer limits. Model
family selection is complete. R26-19 implements fitting, generation and downstream
validation using independently supplied legal fixtures; see the generation guide
for the exact evidence and transfer limits.

1. Separate effective preference intensity (marks per list vote), concentration
   among candidates and territorial variation. Fit historical patterns with
   partial pooling toward a shared baseline when list/geographic observations
   are sparse. Adjust for list length and the reform's capolista exclusion;
   do not transplant a regional participation rate or list profile unchanged.
2. Give each candidate a shared underlying popularity component across their
   candidatures, with local variation. For new candidates without individual
   evidence, use exchangeable synthetic strengths informed by the historical
   distribution. Do not transfer an old candidate's measured strength to a new
   name or redraw the same person's underlying strength independently by college.
3. Generate aggregate counts of **valid preference combinations**: zero to
   three distinct eligible candidates, no capolista, with the applicable gender
   constraints. For six eligible ordinary candidates there are at most 42 subsets
   before legal filtering; sample category counts rather than every voter.
   Derive candidate marginal counts from those categories. Retain a reproducible
   realizability trace; independent marginal draws/bounds alone do not prove
   valid ballots. This construction generates valid inputs, not invalid-ballot
   adjudication. Apply the legal ranking and presentation-order tie rule later.
4. For the central profile, use a maximum-entropy distribution over valid
   combinations constrained by the selected intensity and candidate-strength
   profile. This is an explicit modelling convention: aggregate candidate totals
   do not identify the zero/one/two/three-preference mix or pairing behaviour.
   Test alternative multiplicity/pairing assumptions, especially the third
   preference absent from the regional source.
5. Preserve R26-02. With final attributed totals, model effective preference
   intensity without reconstructing bonus-only counts; the zero-preference
   category remains unspecified composition. With explicit direct/bonus-only
   components, only direct list votes supply the preference budget. In either
   representation, percentages for legal ranking/multiple-election decisions
   use the statutory denominator, not the calibration intensity denominator.

Unknown supplied preferences remain missing, never silently all zero. Valid
supplied counts bypass synthetic generation for their declared scope; preserve
whether each resolved input was supplied or generated. A genuinely incomplete
input leaves its dependent identities/recovery unresolved rather than claiming
complete scrutiny.

### Calibration and validation requirements

The local 2024 Emilia-Romagna cache covers the region's municipalities and
sections, but a single election is not national calibration. Additional municipal
and European sources were located and sampled during the decision review;
their availability does not imply completed cleaning or calibration. Use the
[dated source/evidence register](migration-history.md#preference-source-audit-and-evidence)
to start R26-19, then:

- Reconcile source versions/totals, join verified candidate rosters and gender,
  and preserve geographic coverage, election/rule version and fingerprints.
  Do not infer gender from names or treat absent geographic data as zero.
  Distinguish multiple candidatures from people and zero counts from missing rows.
- Separate measured source patterns, estimated parameters and national transfer
  assumptions. Expand geographic/election coverage; use partial pooling and
  wider sensitivity where evidence is sparse. Regional two-preference ballots,
  different candidates/campaigns, list lengths and protected capolista effects
  require explicit adaptation. Marginals do not identify ballot combinations,
  even in a source allowing three preferences.
- Validate withheld provinces and, as sources expand, withheld elections/regions.
  Account for shared candidates and clustered section data when splitting and
  estimating uncertainty; row counts are not independent sample sizes. Check
  preference intensity, concentration, territorial persistence, zeros and ties.
- Compare central, concentrated/dispersed, weak/strong local-persistence and
  alternative multiplicity/pairing profiles in paired runs with votes and
  candidatures held fixed. Predeclare profile parameters, diagnostics, acceptance
  criteria and seed/replication budget; quantify Monte Carlo uncertainty within
  profiles separately from uncertainty about the model.
- Test valid subset counts, integer/nonnegative marginal counts, direct-vote
  budgets, capolista exclusion, gender constraints and statutory preference ties.
  Propagate preferences through multiple elections, bonus precedence, exhaustion
  and recovery; check which aggregate outputs change, not only candidate names.
  Test supplied-input precedence and reproducibility with isolated preference
  and legal-draw streams. Keep supplied law fixtures independent of calibration.

### Supported outputs and acceptance

Persist `preferences-empirical-synthetic-v1`, fitted-parameter/profile and source
versions, assumptions, seed and supplied/generated provenance. Report vote-based
seat entitlements separately from final filled seats, recoveries and candidate
outcomes conditional on preferences. Synthetic candidate probabilities are
conditional scenario results, not calibrated forecasts for real names. Show
sensitivity across profiles; do not label that spread a calibrated confidence
interval. Do not bypass candidate capacities to report complete final results.

R26-19/20 acceptance requires a versioned fitted profile with source reconciliation,
validation and sensitivity evidence; valid preference generation or supplied
inputs; and complete propagation/reporting of candidate-dependent effects. This
decision alone does not assert fitted parameters or passing application tests;
use the separate implementation evidence. Revisit
the model when broader data, actual reform-election results, material validation
failures or new candidate information change its evidential basis. Keep any UI
parameters tied to an explained modelling meaning.

## Camera-Senato votes (R26-04)

### Approved model

**Approved by Luca on 17 October 2026:** shared political shocks plus historical
chamber contrasts and correlated chamber-specific residuals, anchored to a common
scenario share when supplied. Explicit per-chamber shares take precedence over
model-derived references. The [decision record](migration-history.md#2026-10-17-r26-04-approved-paired-chamber-vote-model)
contains alternatives, the official-source audit and limits of calibration.

- Fit paired historical contrasts at common geographic units, with shared
  national and territorial movement plus chamber-specific national, regional
  and local components. Pool sparse estimates and strongly regularize persistent
  gaps; separate measured geography, turnout and list coverage from behaviour.
- Allocate uncertainty between shared and chamber-specific components while
  preserving each chamber's intended marginal uncertainty. Do not simply append
  Senate noise to a Camera draw and thereby inflate variance without evidence.
  Estimate or explicitly assume a valid joint covariance, then verify means,
  variances and dependence after normalization and integer vote generation.
- Use 2022 as the primary paired baseline reviewed here: both chambers used the
  18+ electorate. Older Senate results include the former 25+ threshold; retain
  them as supplementary evidence/sensitivity unless an explicit demographic
  model justifies comparison. Population ratios alone cannot recover unobserved
  age-specific political preferences. Do not attribute a current difference to
  an obsolete voting-age rule.
- Acquire and prepare paired Senate history within R26-19. Align identifiers,
  dates, electoral geography, elector/turnout bases, list correspondences and
  R26-02 attribution. Treat special-territory inputs separately, including their
  contribution to domestic bonus figures; foreign voting stays outside scope.
- Account for transfer limits: 2022 chamber differences include old uninominal
  candidate and ballot effects, whereas the reform changes those contests.
  Municipal pairs from one election do not identify repeated national forecast
  errors. A high cross-municipality share correlation is not a calibrated
  future-shock correlation. Use a central profile dominated by shared movement
  with weaker/stronger dependence and gap sensitivity profiles; numerical
  covariance/residual scales must be justified or labelled assumptions before
  production integration. Do not count two chambers on one date as independent
  elections when estimating temporal volatility.

### Common anchors and per-chamber precedence

Apply this resolution per list and at the specified **national or local scope**:

| Scenario input at that scope | Camera reference | Senato reference |
| --- | --- | --- |
| No supplied share | Historical/model baseline | Historical/model baseline |
| Common share only | Derived by the paired model from the common anchor | Derived by the paired model from the same anchor |
| Common plus Camera-specific share | Supplied Camera reference | Model-derived from the common anchor |
| Common plus Senato-specific share | Model-derived from the common anchor | Supplied Senato reference |
| Both chamber-specific shares, with or without a common share | Supplied Camera reference | Supplied Senato reference |

A single supplied chamber reference without a common share overrides that
chamber only; the other keeps its historical/model baseline. Explicit references
replace modelled reference adjustments at their scope, not all stochastic
variation. A common share is a starting anchor for distinct chamber references,
not an instruction to copy identical votes into both chambers.

Resolve geographic scope using the established specificity rules: apply broader
territories before more specific province/municipality overrides, so the most
specific value wins. **Within the same scope**, per-chamber input wins over a
model-derived reference from a common input, which wins over the default model
baseline. Do not silently reinterpret a shared override as Camera-only.

Preserve current mean-mode semantics: supplied shares are current valid-vote
references with their parameter dates and election-day uncertainty active;
they are not fixed simulation outputs. Retain correlated shocks even when both
references are supplied. Preserve partial overrides, valid-vote/elector
conversion, abstention handling and normalization/warnings. Validate list
coverage and incompatible inputs rather than creating votes for an absent list.
Trace supplied common anchors, supplied/derived chamber references, resolved
scope and normalization. Version new scenario fields explicitly; preserve
legacy algorithm interpretation and do not silently switch saved algorithms.

### Validation, outputs and acceptance

- Reconcile paired sources and R26-02 attributed totals before fitting; separate
  national/local contrasts, denominators, coverage and special territories.
  Validate withheld territories and, when comparable elections become available,
  temporal transfer. Sparse evidence must remain visible in fitted profiles.
- Test the entire precedence table, a single chamber-only input, partial list
  overrides and national/region/province/municipality interactions. Common-only
  input must allow chamber differences; one explicit reference must not overwrite
  the model-derived other chamber. Both explicit references suppress modelled
  reference shifts while mean-mode uncertainty remains active. Verify reference
  centring and resulting marginal distributions after normalization/rounding.
- Predeclare fit diagnostics, sensitivity profiles and seed/replication budgets.
  Compare joint first-place/42% frequencies, bonus probability, seats and caps
  under paired runs; quantify Monte Carlo uncertainty separately from uncertainty
  about model assumptions. Include identical-vote and independent-draw benchmarks
  without treating either as the production default or asserting universal bias.
- Use constructed legal inputs with linked/unlinked winners, different winners,
  one chamber below/exactly at 42%, incomplete joint input and single-chamber
  renewal. Generate only the renewed chamber in the last case; do not synthesize
  the other chamber or award a joint bonus. Keep fixtures independent of fitting.
- Preserve paired simulation IDs and isolated streams for shared shocks,
  chamber components, candidates/preferences and legal draws. Test reproducible
  replay, stream isolation and unchanged legacy algorithm behavior.

Persist `paired-chamber-votes-v1`, parameter/profile and source versions, age/
geography/coverage assumptions, seeds, and supplied/common/model-derived reference
provenance. Report dependence sensitivity and joint-bonus limitations; do not
present assumed correlation as measured certainty. Acceptance in R26-19/20
requires fitted or explicitly assumed, versioned parameters with reconciliation,
override/marginal tests and joint-bonus sensitivity evidence. The decision alone
does not establish numerical correlation values; R26-19 labels its
implemented profile correlations as assumptions. Revisit when
new comparable paired elections, changed ballots, validation failures or better
demographic information change the evidence.
