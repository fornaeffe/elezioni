# Vote-generation research methods

This guide owns the reproducible protocol and mathematical definitions for the
four-category municipal study. For findings, recommendation, six-category
sensitivity and production work, use the
[research report](vote-generation-research.md). These commands do not activate
a runtime model or change scenario defaults.

For the current proposal, accepted modelling requirements and staged future
comparisons, go directly to the
[revised drift model](vote-generation-drift-model.md). The completed endpoint
experiments below retain their original definitions and results.

## Reproduce and locate

Run from `web/` after the pinned election ZIPs are available in
`data/sources/poll-history/`:

```powershell
npx tsx --tsconfig tsconfig.json scripts/data-preparation/generation-history-cli.ts check
npx tsx --tsconfig tsconfig.json scripts/data-preparation/generation-study-cli.ts check
npx tsx --tsconfig tsconfig.json scripts/data-preparation/generation-study-cli.ts generate
```

The default is 256 deterministic draws per model/fold. Optional study filters:
`--draws=256`, `--target=camera-2022`, `--horizon=15`,
`--regime=camera-only` or `--regime=mixed`. Filters produce partial reports;
they do not change training eligibility. `generate` writes immutable reports
under `data/derived/generation/`. Input/code hashes, policy, draw count and
metadata identify a run; completed compatible folds can be reused. RNG seeds
use the policy version, target, horizon and draw, independently of code hashes.

| Task | File under `web/scripts/data-preparation/` |
| --- | --- |
| Official municipal normalization, attribution and matching | `generation-history.ts` |
| Cutoffs, fitted distributions and conditional spatial draws | `generation-models.ts` |
| Actual existing-generator adapter | `generation-baseline.ts` |
| Quantiles, scalar scores and joint energy score | `generation-metrics.ts` |
| Sources, common domain, comparison and report assembly | `generation-study-cli.ts` |

## Evidence and geographic scope

The pinned Interior Ministry archives cover Camera 2008/2013/2018/2022 and
European 2009/2014/2019/2024. Manifest/member hashes and complete source-list
totals remain auditable. The common comparison domain excludes overseas and
**Valle d'Aosta for both kinds**: its Camera candidate ballot has no comparable
list decomposition. European fuori-sede ballots lack resident-municipality
attribution and are excluded. National totals are recomputed on this domain;
the poll frame remains an approximation to it.

The panel is PD, M5S, Lega and all remaining lists. The first three use reviewed
source labels; **other is a changing complement**, not a party lineage. Source
municipal results retain full list tables, enabling separate extension work.
See [poll calibration](polls-calibration.md) for terminal-poll sources and
official-result semantics.

Camera 2018/2022 candidate-only ballots are attributed at the historical
uninominal college, then reconstructed municipally with conserved margins.
This local decomposition is an estimate. The 2018 `CANDIDATO NON PRESENTE`
placeholder retains its direct list votes. Impossible source denominators and
ambiguous unprovincialized homonyms remain in national source totals and audits
but cannot become local probabilities. Missing or unmatched places are not
zero-filled. Matching uses region/name, reviewed spelling aliases and explicit
homonym handling; it does not reconstruct all mergers or boundary transfers.
Every fold reports matched municipal target-vote mass and baseline/target
electorates. Forecasts use baseline electors, never target electors.

The existing 16 terminal surveys cover six elections. An additional 17 reviewed
Demos observations provide earlier national Camera-intention tables, with
publication/fieldwork dates, sample metadata, primary links and evidence hashes
in `data/sources/polls/history/generation-horizon-*.json`. They are a sparse
one-house selection. Camera intentions are deliberately eligible to predict
European shares. Changes in polling-house composition between early and terminal
horizons confound a pure horizon comparison. These are frozen historical data
available now, not reconstructed contemporary download vintages.

## Temporal folds and central references

For target date `D` and horizon `h` in **730, 365, 90, 15 days**, forecast cutoff
is `C = D - h`. Dates use first voting days where specified in the existing
election manifest, including 8 June 2024. The two regimes are:

- **Camera-only:** Camera historical outcomes, with Camera targets.
- **Mixed:** chronological Camera and European outcomes, with both kinds as
  targets; compare its Camera subset against Camera-only on the same targets.

Only results dated **before C** enter history. The geographic baseline is its
latest eligible election. For every training outcome `e`, rebuild the forecast
at `date(e)-h`; its inputs must satisfy that earlier cutoff. Only training
outcomes themselves before the current cutoff enter the fit. Target/future
outcomes never set model margins, covariances, profiles or point estimates.

Poll publication and fieldwork end must both be at/before C. Eligible reviewed
observations are at most 180 days old; retain those within 35 days of the newest
eligible publication. Target-specific terminal evidence takes precedence and
cannot be reused for another election. Early observations use weights
`min(sampleSize,2000) * 2^(-publicationAgeDays/30)`; terminal aggregates retain
the existing effective-sample/recency-weighted polling point. No eligible polls
means a historical national-share fallback. The valid-ballot-rate anchor uses
the latest same-kind result, or the latest result if none exists.

Each completed election contributes **one national error vector**. Houses,
parties, horizons and municipalities are not extra independent elections.
National fits include earlier no-poll fallback forecasts: they estimate errors
of the stated procedure with varying information availability, not exclusively
poll-informed error. Excluding those events or conditioning on evidence quality
is a sensitivity, not an unnoticed refit after inspecting rankings.

## Coordinates and fitted budgets

For positive shares `p=(PD,M5S,Lega,other)`, let `z=H log(p)`, where row `i`
of the three-row orthonormal Helmert basis, for `i=1,2,3`, has its first `i`
entries `1/sqrt(i(i+1))`, next entry `-i/sqrt(i(i+1))`, and remaining entries
zero. Inversion is `softmax(H' z)`. The four-dimensional state is
`x=(z1,z2,z3,logit(validVotes/electors))`.

Before transformation, observed category fractions receive the explicit
floor `p*_j=max(p_j,0.005)/sum_k max(p_k,0.005)`. This includes tiny positive
categories and absent parties; the final fraction need not equal 0.5%. It is
a modelling proxy, not evidence of historical support or a legal filing rule.
The study does not test full-list structural-zero feasibility.

With `n` earlier training events at the same horizon, use uncentered endpoint
errors `e_b=x(actual_b)-x(forecast_b)` and:

```text
v_h = 0.2^2 + 0.35^2 * h/365.25
Psi = diag(v_h,v_h,v_h,0.2^2) + sum_b e_b e_b'
C_N = Psi/(n+1)
df  = n+3
```

Zero error location retains the shared forecast centre; persistent errors enter
uncertainty. These are total endpoint budgets: do not add the old national
movement or poll measurement variance to the candidates. The constants are
explicit priors, not imported foreign calibration or a separately identified
Italian temporal diffusion rate.

For a completed transition of length `T_b` years, subtract its national state
change from every matched municipal state change. Its regional residual `r_br`
is the valid-vote-weighted mean; municipal residual `l_bm` subtracts that mean.
Each election has equal total weight within each covariance budget:

```text
C_R = [0.12^2 I + sum_b sum_r r_br r_br'/(R_b T_b)]/(n+1)
C_L = [0.18^2 I + sum_b sum_m l_bm l_bm'/(M_b T_b)]/(n+1)
```

`R_b,M_b` count available regional/municipal residuals. Transition lengths and
forecast age `A` have a 0.25-year lower bound. Gaussian spatial shocks have
covariances `A*C_R` and `A*C_L`; all four coordinates are joint, so share/valid
rate correlations are retained. Outcome weights occur only inside completed
training events.

## Models and draw reconciliation

**Joint Gaussian:** draw one national `N(0,C_N)` error. Add independent regional
and municipal four-vectors to latest historical municipal states, using the
fitted spatial budgets.

**Joint Student-t:** multiply each Gaussian vector by
`sqrt((df-2)/chiSquare(df))`. One scale is shared by all four national coordinates;
each region and municipality has its own common within-vector scale. This gives
the same covariance as the Gaussian comparator and multivariate Student-t
scale `C*(df-2)/df`. The national construction corresponds to integrating an
inverse-Wishart covariance with four-dimensional prior degrees of freedom six.
The scatter fit remains sensitive to large training errors; this is not a robust
Student-t likelihood fit.

**Hybrid spatial transition bootstrap:** keep Gaussian national errors. Pool
historical municipal profiles after removing each event's national state and
adding the latest national state, weighted by an eight-year half-life. Select
one completed transition and one common random sign for the whole country.
Regional/local shocks combine its matched residual field, scaled by
`sqrt(A/T_b)`, with `0.35` times the corresponding Gaussian shock. A missing field
component contributes no empirical residual but retains regularizing noise;
without a block the fallback is the full Gaussian shock. This preserves observed
spatial co-movement, with sparse/discrete empirical support and a symmetry
assumption. It is not a national endpoint bootstrap.

For all candidates, inverse-transform the national share vector and national
valid rate once. Shift municipal valid-rate logits until their elector-weighted
aggregate equals the sampled rate. **Then** IPF-reconcile local shares using
`forecastElectors * sampledLocalValidRate` weights to the sampled national share
margins; continuous tolerance is `1e-8`. Round local valid totals, then use
largest remainders for list counts. The unallocated electorate is the complement.
Row counts conserve forecast electors; integer national margins have a measured
rounding discrepancy. No target result supplies these margins.

**Current mechanics comparator:** call the unchanged
`generatePairedMunicipalVotes()` for Camera with the central chamber profile.
First reconcile the same central national shares and valid-rate anchor. Retain
independent per-list national/regional/local elector-logit noise, with scales
`0.02/0.01/0.01` per square-root day, followed by elector-fraction normalization
and existing integer allocation. National list dates refresh with polling;
geographic/valid-rate dates remain historical. Measurement sigmas come from the
actual `estimatePollReference()` v1 procedure, using collapsed real terminal
questions or reviewed early questions. Its 60-day evidence limit can exclude an
older study-selected poll while retaining prior-derived sigma. The estimator's
historical-blend centre is intentionally replaced by the common polling centre:
this compares generation mechanics rather than reproducing unchanged defaults.

## Revised drift proposal — 24 October 2026

The corrected formulation now lives in the focused
[drift-model document](vote-generation-drift-model.md). It distinguishes latent
multinomial log scores from observed counts, identifies score coordinates,
specifies independent process increments and uncertain initial states, and
records the accepted polling requirements and comparison hierarchy.

The proposed electorate-wide outcome set adds a combined non-valid category;
polling links condition on political valid votes. This permits observed zeros
through a multinomial count layer without treating undecided respondents or
unmapped political lists as non-valid electors. No new backtests or runtime
integration are claimed.
## Metrics in the requested priority order

1. **Plausibility:** count nonfinite/out-of-range shares, noninteger/negative
   counts and row-conservation failures; report continuous/rounded national
   margin error. Also report draw frequency of any national category above 80%,
   and municipality-category concentration above 95% with at least 500 generated
   valid votes. Compare observed target frequencies. These are fixed diagnostics,
   not hard truncations. Concentrated **other** combines many lists and can be
   legitimate. Local eligibility depends on simulated valid totals. Passing
   these checks cannot establish full-party political plausibility.
2. **Coverage and informative uncertainty:** pointwise central 50/80/90/95%
   coverage; 90% width and interval score; WIS; national energy score. A wide
   interval cannot improve scores merely by covering everything. The reported
   all-four-covered frequency uses four marginal 90% intervals; it is **not** a
   separately calibrated simultaneous 90% set.
3. **Expected-value accuracy:** MAE, RMSE and signed bias of the Monte Carlo
   predictive mean. Inverse-ILR of the latent centre is not generally the expected
   share. Report national, regional and matched-municipal results, plus municipal
   voter-weighted results; score valid rates separately. Future vote/elector
   weights are evaluation weights only.

For central interval `[l,u]`, miscoverage `alpha`, and observation `y`:

```text
IS_alpha = (u-l) + (2/alpha)*[(l-y)_+ + (y-u)_+]
WIS = [0.5*|median-y| + sum_alpha (alpha/2)*IS_alpha]/4.5
alpha in {0.5,0.2,0.1,0.05}
Energy = E||X-y|| - 0.5*E||X-X'||
```

Quantiles use linear interpolation at sample index `(draws-1)*p` (type 7);
endpoints count as covered. Energy uses independent paired halves of the draws,
avoiding quadratic work; an unmatched odd draw is unused. Lower losses are
better. Weights normalize over variables; RMSE aggregates squared errors before
the square root. Summary tables weight elections equally, rather than letting
large elections supply extra national validation units. Sparse elections and
finite Monte Carlo tails limit fine rankings; geographic dependence still needs
a dedicated variogram/seat-level assessment.

## Interpretation and next-stage boundary

In the completed experiments, the fifth state component is **valid
votes/electors**, not turnout. The current
[drift proposal](vote-generation-drift-model.md) instead includes a combined
non-valid outcome in the electorate simplex; it need not separately generate
abstention and invalid ballots unless those distinct outputs are required.
Future electorates, geography, filing support and editable source-specific
correspondences remain necessary. Neither the changing other category nor its
covariance can be copied independently to each future list. The six-category
sensitivity belongs to the research report; it does not by itself validate
arbitrary full-list scenarios, Senate contrasts, municipal-candidate elections,
regional elections or seat probabilities.

## Statistical references

- [Egozcue et al., Isometric Logratio Transformations](https://doi.org/10.1023/A:1023818214614):
  orthonormal coordinates for compositional data.
- [Murphy, Conjugate Bayesian Analysis of the Gaussian Distribution](https://www.cs.ubc.ca/~murphyk/Papers/bayesGauss.pdf):
  covariance mixtures and Student-t predictive calculations.
- [Gneiting and Raftery, Strictly Proper Scoring Rules, Prediction, and Estimation](https://doi.org/10.1198/016214506000001437):
  interval and joint distribution scoring.
- [Heidemanns, Gelman and Morris, Dynamic Bayesian Forecasting](https://hdsr.mitpress.mit.edu/pub/nw1dzd02/release/2):
  correlated national/local forecasts, temporal information and nonsampling error.

The references motivate statistical structure. Their US-specific estimates and
hyperparameters are not used as Italian calibration parameters.
