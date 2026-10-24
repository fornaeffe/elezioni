# Drift-model testing protocol

This is the execution record for the [agreed model](vote-generation-drift-model.md).
It does not change production generation. No additional polling data are acquired.

## Protocol fixed before inspecting predictive results

- Primary panel: non-valid electors, PD, M5S, Lega and the changing complement
  of all other political lists. Observations retain exact groups rather than
  guessed splits. This panel does not establish calibration of individual lists
  inside other. Derived poll views are deduplicated.
- All compatible downloaded historical observations precede each reference
  cutoff. Poll publication controls availability; fieldwork midpoint is the
  explicit measurement-time approximation. Conditional questionnaire scenarios
  without a matching baseline assumption are audited, not blended.
- Sources: the eight pinned Camera 2008/2013/2018/2022 and European
  2009/2014/2019/2024 archives and 38 compatible downloaded survey waves
  (41 questions, three conditional alternatives excluded before fitting).
  Later waves remain unavailable to earlier cutoffs. The existing importer
  reconstructs valid candidate-only attribution in Camera 2018/2022 before
  computing non-valid = electors minus attributed valid list votes. Valle
  d'Aosta lies outside the common panel; European fuori-sede ballots without
  resident-municipality attribution stay separately audited. Unmatchable or
  inconsistent municipal records are excluded locally, with retained national
  totals and matched-vote mass reported. Unmapped political votes stay political.
- Development targets: Camera 2013/2018 and European 2014/2019, at 730, 365,
  90 and 15 days. Later Camera 2022 and European 2024 are confirmation targets;
  do not use their scores to select variants or retune priors.
- Run the agreed hierarchy on development folds, retaining the common-volatility
  model and actual current-generation mechanics. Run both regional and local
  mean-reversion branches separately. Evaluate selected models on confirmation
  folds with their choices frozen. Every fit may use observations available
  before that fit's cutoff, including earlier confirmation-era elections when
  legitimately available; the variant choice remains frozen.
- Screen with 128 complete geographic draws; final comparisons use 512.
  Cities and parties are correlated observations, not extra national elections.
- After freezing the 128-draw staged selection, rerun the four drift finalists
  and current benchmark on development folds with 512 draws as a Monte Carlo
  refinement, without revising selection from confirmation scores. Rare-tail
  screening counts at 128 draws are descriptive and need this qualification.

## Scores and summaries

1. **Plausibility:** count non-finite/negative/noninteger outputs, elector
   conservation failures and inconsistent national/municipal aggregation.
   Separately record named-party national shares above 80% of valid votes,
   non-valid above 90% of electors, zero-valid draws and municipal concentration
   above 95% where at least 500 valid votes are generated. These stress flags
   are permissive diagnostics, not a complete definition of plausible politics.
2. **Uncertainty:** empirical central 50/80/90/95% predictive coverage; 90%
   width; interval score and weighted interval score (WIS). For a central
   interval `[L,U]` with exclusion probability `alpha`, its score is
   `U-L + 2/alpha * (max(0,L-y) + max(0,y-U))`.
   WIS averages the four interval scores with weights `alpha/2` and absolute
   median error with weight `0.5`, dividing by `4.5`. Lower is better: it
   penalizes both unnecessarily wide intervals and missed observations.
3. **Expected-value accuracy:** MAE and RMSE of predictive means, with signed
   errors retained per outcome. A mean signed error over a whole simplex is
   mechanically zero and does not establish unbiased individual forecasts.

Also retain the national-vector energy score, estimated from independent
chain halves as `E||X-y|| - 0.5 E||X-X'||`, to assess joint predictions.
Quantiles use linear empirical interpolation. Score political shares among
valid votes and among electors, plus non-valid shares among electors, at
national/regional/municipal levels. Keep equal-municipality and elector/valid-
vote-weighted summaries. Main reported municipal scores use equal municipality
weights; elections/horizons receive equal fold weight. Missing municipal
matches are audited, not treated as zero. National scores use the common
source territory; future electorate counts are never supplied to fitting.

## Feasible inference and its approximations

The research solver is native Node/TypeScript. It uses multinomial likelihood
derivatives in iterated Gaussian/Laplace working messages. Independent municipal
Gaussian Markov chains are eliminated analytically, retaining their conditional
dependence on national, election-kind and optional regional states.

For aggregate polls, linearize around the joint state mode. Include the full
cross-time conditional geographic covariance, shared survey-period errors and
sample-dependent measurement covariance. This is a Gauss–Newton/Laplace
approximation, not exact Bayesian inference. Geographic field conditioning must
not be replaced by post-draw national reconciliation.

Local process scales use count-uncertainty-corrected temporal log-odds moments
and empirical-Bayes pooling, rather than a separate unconstrained estimate in
every municipality. Common national movement is removed using within-transition
municipal contrasts. Regularized list and municipality effects are fitted on
earlier observations only. Regional variance is estimated from between-region
versus within-region transition dispersion, preserving the marginal budget.
Local reversion is profiled against persistent initial geography.

National scales and shared polling-error scales are sampled from their
marginalized Gaussian/Laplace likelihood, with state draws conditional on each
parameter draw. Local scale uncertainty is estimated from temporal/spatial
blocks and pooled municipality information and propagated to future innovations.
Reference-state covariance conditional on fitted local scales is an empirical-
Bayes approximation; inspect local-scale sensitivity for the final selection.
Local outcome multipliers, regional allocation and the profiled reversion rate
are point estimates. Their full joint posterior, and uncertainty in the fixed
persistent geographic centre, are not integrated. Consequently this study
compares these practical fitted generators; it does not prove that a fully
Bayesian implementation of each richer variant would rank identically.

Declared priors/conventions: time in years; national scale 0.35 and local scale
0.25 per square-root year as prior/initial scales; log-national-scale prior SD
0.7; shared poll-error scale prior 0.12 with log SD 0.6; regularized outcome
log-scale effects SD 0.35; kind-effect contrast SD 0.5. Initial observed score
contrasts have weak SD 3; unobserved emerging-party geographic contrasts use
SD 0.5 and a 1% initial share convention, not invented historical votes.
Initial national contrast SD is 1.5. Exact choices and source/code hashes
belong in every result artifact.

The local moment estimator uses positive variance floors (common SD 0.02,
outcome SD 0.01) and municipality shrinkage equivalent to 12 prior degrees
of freedom. Its shared scale confidence mixture is based on region-by-time
blocks, with a minimum of three blocks. These are estimation conventions,
not claims of fully Bayesian local-scale inference.

Survey-period groups are deterministic calendar half-years by ballot kind.
The effective sample convention comes from the existing polling preparation:
design effect 2, maximum effective decided sample 600, unknown decided fraction
0.5. Printed percentages get a 0.1 percentage-point rounding allowance.
Different questions from one survey are not independent evidence.

## Numerical gates

Check matrix/conditioning identities, multinomial derivatives and observed-zero
behavior, unequal valid-vote weighting, temporal leakage, Gaussian covariance
sampling, parameter-chain diagnostics and small-problem posterior comparisons.
Record Laplace mode changes, iteration limits and PSD repairs. A numerically
failed forecast is an explicit failure, not a skipped fold or silently accepted
model. Distinguish numerical approximation diagnostics from predictive calibration.
Mode convergence requires both municipal count-state and global national/
polling/kind/regional state changes below 0.002, with a 20-iteration budget.
The reduced posterior check compares a seven-dimensional joint Laplace
approximation against elliptical slice sampling, including an observed zero;
it supports the approximation choice but does not establish accuracy of the
full high-dimensional empirical-Bayes fit.

Promotion rule (fixed before predictive scoring): compare numerical/invalid
failures first, then excessive national tails (named political party above 80%
of valid votes, or non-valid above 90% of electors; diagnostic allowance 0.5%,
material comparison tolerance 1 percentage point). Next minimize the mean
shortfall below 85% coverage for nominal 90% intervals across national,
regional and municipal political-valid and non-valid summaries. Shortfalls
within 1 percentage point are treated as tied. Then compare mean WIS across
those same six summaries; differences within 2% retain the simpler incumbent.
Predictive-mean errors are reported as the third substantive priority.
This practical screening rule does not establish a statistically significant
winner from only four development elections.

Hyperparameter sampling uses two chains, at least 128 retained draws each,
256 burn-in steps and thinning of at least four; extend automatically up to
1024 retained draws per chain if split R-hat exceeds 1.1 or ESS is below 30.
Failures remain visible. Scale bounds: national [0.02, 3], shared polling
[0.015, 0.7], outcome multipliers [0.1, 10]. Regional allocation is regularized
and bounded to [0.01, 0.85]. OU rate profiles [0, 0.1, 0.25, 0.5, 1, 2]
per year with a half-normal 0.5 penalty, conditional on moment-estimated scales.

Restricted sensitivities: six political outcomes plus non-valid at 15 days
(all-common and selected model); Camera-only history versus mixed-kind history
for Camera 2018/2022 at all horizons; selected local scale multiplied by 0.85
and 1.15 on the two confirmation elections at 15 days. These do not select a
new model. Confirmation comparisons use 512 draws; sensitivities use 128.

## Reproduce or inspect

From `web/`:

```sh
npm run data:forecast:drift:check
npm run data:forecast:drift:validate
npm run data:forecast:drift:compare
```

The comparison runner performs staged screening, confirmation and the planned
restricted sensitivities. The additional 512-draw development refinement in
the report is reproduced by these commands (they may run concurrently):

```sh
npm run data:forecast:drift:generate -- --only=development --draws=512 --model=literal.pooled.common.common.independent.brownian
npm run data:forecast:drift:generate -- --only=development --draws=512 --model=aggregate.adjusted.common.common.independent.brownian
npm run data:forecast:drift:generate -- --only=development --draws=512 --model=aggregate.adjusted.common.common.regional.brownian
npm run data:forecast:drift:generate -- --only=development --draws=512 --model=aggregate.adjusted.common.common.independent.ou
npx tsx --tsconfig tsconfig.json scripts/data-preparation/drift-study-cli.ts current --only=development --draws=512
npx tsx --tsconfig tsconfig.json scripts/data-preparation/drift-study-cli.ts current --only=all --panel=six --horizon=15 --draws=128
```

For the matched 128-draw sensitivity baselines, also generate the selected
mixed-history Camera 2022 cases and European 2024 fifteen-day case:

```sh
npm run data:forecast:drift:generate -- --target=camera-2022 --draws=128 --model=aggregate.adjusted.common.common.independent.brownian
npm run data:forecast:drift:generate -- --target=european-2024 --horizon=15 --draws=128 --model=aggregate.adjusted.common.common.independent.brownian
```

The comparison is resumable: completed cases with the same source/code/policy
hash are reused. It runs up to six independent Node processes; set the
`DRIFT_WORKERS` environment variable to reduce concurrency. Keep the source
fixed during a run. Native TypeScript preparation and the existing cached
official archives are required; no Python/R inference service is used.

For one inspectable case:

```sh
npm run data:forecast:drift:generate -- --target=camera-2018 --horizon=15 --draws=128 --model=literal.pooled.common.common.independent.brownian
```

Variant IDs specify polling link, election-kind effects, national scale,
local scale, regional component and time law, in that order. Case JSON retains
source/code hashes, cutoff, included election/poll IDs, fitted scales, mode and
chain diagnostics, all metrics, national intervals and geographic matching.
`development-stages.json` records promotions; `frozen-selection.json` precedes
confirmation generation; `comparison.json` includes confirmation and restricted
sensitivities. Generated artifacts stay in the ignored derived directory.

The current comparator calls the actual production municipal generator with
its present national/region/local sigmas and poll-reference rules, adapted
to the identical research outcome panel and as-of electorate inputs. Its
elector-fraction/logit normalization and fixed-zero semantics are retained;
it is not a reimplementation of the new softmax model. Grouped surveys are
expanded only for that comparator using prior historical within-group shares.

## Results

See the separate [measured comparison and recommendation](vote-generation-drift-results.md).
It retains the stage choices, higher-draw development refinement, historical
confirmation scores, sensitivity checks and numerical limitations. Keep
method and reproduction details here; update measured findings in that report.

## Runtime boundary

These fits belong to data preparation. Runtime adoption would export a compact
reference-state posterior and fitted innovation parameters for seeded browser
generation, instead of refitting historical states in every browser. It still
needs full-list split uncertainty, dated electorate/geography inputs, filing
masks, aggregate-to-latent user overrides and the agreed paired-chamber marginal
variance construction. The existing correspondence editor stays available.
These backtests do not validate Senate dependence, seat distributions or a
separate decomposition of non-valid voting into abstention and invalid ballots.
