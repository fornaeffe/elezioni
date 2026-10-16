# Politics reform 2026: modelling decisions

Read the section corresponding to R26-02, R26-03, or R26-04 in
[`politics-reform-2026-plan.md`](politics-reform-2026-plan.md). These are pending
decisions, not implemented models. No calibrated generator for these choices is
established here; audit available source data in the relevant decision step.
Constructed data can test mechanics and sensitivity;
it cannot establish the real distribution of unobserved voting behavior.

For each decision, present options and evidence to Luca, then record the chosen
semantics, rejected alternatives and rationale, supported outputs, assumptions,
verification criteria, and conditions for revisiting it in
[`migration-history.md`](migration-history.md). Update active directives after
the choice. Approval to write this documentation does not settle the choices.
Any experiment used for a decision must be reproducible and distinguish a
modelling assumption from a measured electoral fact.

## Bonus-only votes (R26-02)

### Question and options

There are no data here to estimate how many ballots name only the bonus slate,
or how that behavior varies by coalition, locality, or chamber. Decide whether
to generate these ballots separately or to generate final attributed list totals.

Luca's working proposal is to interpret every generated list total as direct
list votes plus its share of bonus-only votes after legal redistribution. This
is a candidate baseline, not a decision to use zero bonus-only ballots.

Evaluate at least these paths:

| Path | Generator output | Scrutiny treatment | Main limitation |
| --- | --- | --- | --- |
| Already attributed totals | Final list totals | Do not redistribute again | Ballot composition is not identifiable |
| Explicit ballot components | Direct list totals and bonus-only total for each coalition/college | Apply legal attribution once | Needs unsupported parameters or user inputs |
| Supplied explicit components | Validated external scenario components | Apply legal attribution once | Depends on input availability; not a calibrated random model |

Keep the pure raw-ballot attribution function testable whatever production path
is selected. Do not add bonus-only votes on top of a generated final total.
Special uninominal candidate-only votes are a separate legal input; decide
whether the same final-total convention is applicable there instead of assuming it.

### Equivalence and bias protocol

For one ordinary plurinominal college and coalition, let `D[l]` be direct list
votes, `B` bonus-only votes, and `A(D, B)[l]` their legal attribution, including
integer remainders. Final list votes are `T[l] = D[l] + A(D, B)[l]`.

1. Establish the exact legal geographic level and rounding rule in R26-01/08.
   Never substitute attribution at municipal or national level without proving
   equivalence. Aggregation and integer remainder allocation may not commute.
2. Prove the real-valued proportional identity where `sum(D) > 0`, then test the
   actual integer operation separately. For example, in an ideal real-valued
   construction `D[l] = (1 - beta) * T[l]` and `B = beta * sum(T)`, the final
   proportions are preserved for `0 <= beta < 1`. Rounding can break exact
   reconstruction, and this identity says nothing about unknown behavior.
3. Feed identical final `T` to the direct-total path and to the downstream part
   of the explicit path. All subsequent figures, admissions, bonus decisions,
   seats, and supported candidate results must agree under the same legal draws.
   This proves representation equivalence, not empirical absence of bias.
4. Construct integer `D/B` scenarios across list sizes, coalition structures,
   local concentration, ballot-only shares, and vote volumes. Include zero
   direct support, small lists, special territories where relevant, and cases
   around 3%, 10%, 20%, 42%, and both caps. A no-support rule must come from the
   normative specification; do not silently distribute equally.
5. Compare explicit attribution against the proposed final-total model under
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

Acceptance: no double attribution or unexplained directional implementation
effect; an explicit bound or measured sensitivity for material rounding/model
effects; and a qualified statement of which hypotheses were tested. Do not claim
unconditional absence of systematic bias without data. If material bias remains,
revise the proposal or record the restriction before production integration.

## Preferences (R26-03)

### Question and options

No preference data or calibrated preference-generation model is available for
this task. The legal ranking functions can still be implemented against supplied
counts. Decide separately whether the web simulation should generate counts and
what candidate-level conclusions it can support.

| Option | Required choice | Consequence |
| --- | --- | --- |
| Synthetic preference generation | Distribution, concentration, territorial/candidate effects, ballot constraints | Candidate results depend on stated assumptions |
| Supplied preferences | Input resolution and validation | Exact ranking only for supplied scenarios |
| No random preference model for now | Explicitly limited outputs | Do not present unsupported candidate election probabilities |

Specify treatment of capolista, up to three preferences, gender constraints, and
ties in presentation order. If counts are generated in aggregate, establish
their realizability under valid ballots or use a construction that guarantees
it. Marginal count bounds alone do not establish ballot validity.

Do not represent unknown preferences as all zero: that would impose an actual
all-tied ranking and systematically favor list order. An explicit synthetic
equal/random/order-based ranking is also a modelling choice to discuss.

### Impact checks and acceptance

- Compare supplied rankings with concentrated, dispersed, and geographically
  varying synthetic examples. Check capolista priority and exact preference ties.
- Propagate alternative rankings through multiple elections, bonus precedence,
  exhaustion, and recovery. Distinguish seats assigned before these operations
  from final filled seats and candidate identities. Establish which aggregates
  are invariant and which can change; do not assume only names are affected.
- If candidate predictions are deferred, define which aggregate seat outputs
  remain justified and how unsupported candidate/recovery results are represented.
  Do not bypass candidate capacities to claim complete legal scrutiny.
- If a synthetic model is selected, define its parameters, reproducible stream,
  sensitivity range, and visible assumptions. Avoid adding uncalibrated UI
  controls without explaining their meaning.

Acceptance: an explicit output contract, model/input requirements, and a test
protocol covering these effects. A restricted mode may be a staged deliverable;
it does not close the plan's full candidate-level acceptance by itself.

## Camera-Senato votes (R26-04)

### Current boundary and options

The current `web/src/lib/politics/vote-generation.ts` generates one set of
territorial list draws and aggregates it into Camera and Senato uninominal
geographies. It does not estimate a separate chamber voting-behavior component.
Check downstream coverage/normalization before claiming final chamber totals
are identical. The reform's joint bonus makes this dependence consequential.

Inspect whether previous Senate election results are already available in the
source bundles before treating calibration data as absent. If R26-04 elects to
use them, audit the electorate-age rule for each election, align age/turnout
bases and geography, and separate list coverage from voting-behavior differences.
Otherwise leave this data-backed extension in the root plan's post-migration
roadmap; do not infer that choosing chamber residuals alone implements it.

| Option | Assumption to discuss | Required evidence |
| --- | --- | --- |
| Shared territorial votes | No modeled chamber-specific behavior | State the joint-bonus limitation |
| Shared component plus chamber residuals | Magnitude and correlation of residual differences | Sensitivity and valid marginal distributions |
| Chamber-specific supplied inputs/overrides | Which differences users supply and at what resolution | Validation and preserved scenario semantics |

Absence of calibration data is not evidence for independent chamber draws.
Do not introduce an obsolete voter-age distinction as an explanation for a
residual. Separate actual electorate/geography/list-coverage differences from
assumed differences in voting behavior.

### Impact checks and acceptance

- Separate common national/local shocks from any chamber-specific residuals;
  define their covariance, normalization, and interaction with abstention,
  overrides, and list/coalition coverage. Do not choose residual parameters here.
- Check nonnegative votes, intended marginal means/uncertainty, and preservation
  of approved global/local share semantics. Keep geographic aggregation effects
  distinct from a behavioral swing. Specify what a scenario share means for
  each chamber if the model changes.
- Use constructed outcomes with linked winners, different winners, only one
  chamber at 42%, and one chamber not renewed. The scrutiny tests must cover
  these branches even if the selected generator cannot reach all of them.
- Compare joint first-place and joint-42% frequencies, bonus probability, seats,
  and cap effects across a stated dependence/swing sensitivity range. Quantify
  uncertainty and avoid asserting a universal direction of bias without evidence.
- Define reproducible paired simulation IDs and isolated random streams. If a
  shared model is chosen, say explicitly that unmodeled chamber differences
  remain a limitation, not a measured absence of differences.

Acceptance: Luca's recorded choice, model/input and export semantics, justified
or explicitly assumed parameters, and the protocol to verify R26-19. A decision
to defer differentiation must still document how joint-bonus results should be
interpreted and when to revisit that assumption.
