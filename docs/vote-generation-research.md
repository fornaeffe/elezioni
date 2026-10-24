# National and municipal vote-generation comparison

**Status, 24 October 2026:** Luca has not selected any of these models for
adoption. The active proposal and desired research plan are the
[revised latent-drift model](vote-generation-drift-model.md). This report retains
the completed experiments and their original conclusions as historical evidence.

This report owns the model-selection findings and proposed runtime redesign.
The [methods](vote-generation-methods.md) own equations, metric definitions,
source caveats and implementation pointers. All code added for this study runs
offline in Node/TypeScript; the production generator remains unchanged.

**23 October recommendation, not selected for adoption:** take the **joint
Gaussian endpoint plus conditional Gaussian geography and post-draw
reconciliation** forward as the primary redesign
candidate. Keep Student-t as a close sensitivity. Mixed Camera/European history
is useful, particularly for municipal geography; the experiment does not support
excluding European evidence in advance.

**No tested model satisfies all three priorities across all horizons.** The
Gaussian family gives the strongest coverage and slightly better local means
near elections, but its current long-horizon fit produces broader, more extreme
draws and worse proper scores than the current generator. It should not become
an operational year-ahead forecast unchanged. This qualification follows the
first-priority plausibility requirement, rather than selecting on coverage alone.

## Decision criteria

Use Luca's priority order:

1. Reject invalid elections: negative/nonintegral votes, broken electorate
   accounting, invalid shares or failure to preserve the sampled national
   margins. Separately inspect extreme national/municipal concentration. These
   thresholds flag stress cases; they do not define political impossibility,
   especially when a category groups several parties.
2. Inspect 50/80/90/95% predictive coverage nationally, regionally and in
   municipalities. Report interval width, 90% interval score and weighted
   interval score (WIS), so very broad intervals do not win on coverage alone.
   Joint energy score checks the national vector. Four marginal 90% intervals
   covering together do not constitute a separately calibrated simultaneous
   90% region.
3. Score the predictive **mean**, not the latent log-ratio centre: MAE, RMSE and
   signed error. Report both equal-municipality and valid-vote-weighted scores.

An election is the independent validation unit. Municipalities, parties and
repeated forecast horizons do not multiply the number of independent elections.
The comparison is exploratory model selection, not a second untouched test set
proving that the selected family will retain its ranking.

## Evidence and experiments

Eight pinned official archives provide Camera 2008/2013/2018/2022 and European
2009/2014/2019/2024 municipal results. Forecast targets are the six 2013–2024
elections, with origins 730, 365, 90 and 15 days before the first voting day.
Only polls published with fieldwork complete before the cutoff and election
results already available before it may enter prediction or fitting. Each
calibration election is itself replayed from its own earlier cutoff.

The four-category main panel is PD/M5S/Lega/changing other. It compares 12
Camera-only forecasts and 24 mixed-history forecasts. Camera-only and mixed
histories are assessed on the same Camera targets; European targets provide
additional transfer diagnostics. All eligible baseline municipalities are
generated in every draw, with municipal scores restricted to audited matched
geography. Forecasts use historical electorate counts, never future electors.
The common domestic domain excludes Valle d'Aosta and ballots without a
resident-municipality attribution. Full raw source totals remain auditable.

Earlier polling adds 17 primary [Demos observations](https://www.demos.it/atlante_politico.php)
to the existing 16 reviewed terminal survey questions. The early archive covers
18 cutoff selections, with some observations up to 97 publication-days old at
the cutoff. Their actual fieldwork horizons are recorded. Early questions ask
about Camera: using them for European targets is an explicit cross-kind test.
Changing polling-house composition between early Demos-only and multi-house
terminal forecasts limits claims about horizon alone causing improvement.

The new six-category sensitivity separates FdI and FI/PdL from other for Camera.
It uses only seven terminal questions that explicitly observe all six categories:
one in 2013, three each in 2018/2022. Unreported FdI is never replaced by zero.
2018 and 2022 are its two chronological scored holdouts; 2013 is a prior audit.
This sensitivity retains the earlier archive's 15–16-day terminal cutoffs,
rather than asserting identical observation origins across the two panels.

## Models

| Model | National endpoint | Conditional municipal geography |
| --- | --- | --- |
| Current | Actual existing elector-logit generator and poll-v1 measurement sigmas | Existing national/region/local Gaussian shocks and normalization |
| Joint Gaussian | Regularized joint log-ratio/valid-ballot endpoint errors fitted on earlier elections | Joint Gaussian region and municipality innovations around the latest historical profile; reconcile after the draw |
| Joint Student-t | Same covariance as Gaussian, with one shared predictive tail scale | Student-t spatial innovations; reconcile after the draw |
| Spatial transition bootstrap | Same Gaussian national endpoint | Recency-pooled historical spatial profiles and signed whole-transition residual fields, plus regularization; reconcile after the draw |

This tests two different geographic constructions as well as Gaussian versus
Student-t distribution shape. The current benchmark receives the same national
point and initial valid-ballot-rate anchor as the alternatives; it compares the
generation mechanism rather than also changing the point estimator. Its
measurement sigmas come from the existing poll-v1 estimator. Earlier stale
polls that fail that estimator's 60-day window leave its historical-prior
uncertainty. The current runtime's ordinary default means are therefore not
being replayed unchanged.

The new models fit total endpoint errors at each tested horizon. They do not
multiply the earlier 15-day covariance by square-root time, and do not add the
old national movement/measurement noise again. Prior magnitudes, smoothing and
profile-pooling choices are explicit conventions, fixed before the comparison;
they are not claimed as precisely estimated Italian political constants.

## Results: near-election comparison

All numbers below come from 256 draws per model/forecast. Coverage is pointwise
for nominal 90% intervals. Widths, mean MAE and WIS use percentage points; lower
MAE/WIS is better. Elections receive equal weight, then categories receive equal
weight. Municipal columns here give municipalities equal weight; voter-weighted
versions, all interval levels, RMSE, bias and per-election intervals are in JSON.

Mixed-history forecasts at 15 days, all six targets including the difficult
initial 2013 case:

| Model | National coverage | National width | National mean MAE | National WIS | Municipal coverage | Municipal mean MAE | Municipal WIS |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Current | 58.3% | 7.60 | 3.42 | 1.65 | 85.93% | 5.05 | 2.57 |
| Joint Gaussian | 91.7% | 12.83 | 3.42 | 1.48 | 91.21% | 4.97 | 2.66 |
| Joint Student-t | 91.7% | 12.31 | 3.43 | 1.51 | 90.68% | 5.01 | 2.64 |
| Spatial transition bootstrap | 91.7% | 12.83 | 3.42 | 1.48 | 82.16% | 6.02 | 3.01 |

The national improvement is mainly calibration, not a new central poll estimate.
Gaussian regional coverage rises from 83.33% to 88.38%. Local intervals widen
from 20.69 to 25.72 pp on average; their WIS slightly worsens even as coverage
and predictive-mean MAE improve. Student-t gives slightly narrower intervals
and marginally lower local WIS, with somewhat worse local coverage and mean MAE.
The evidence does not establish a decisive Gaussian/Student-t separation.

Restricting the Camera near-election comparison to the two learned holdouts,
2018 and 2022, Gaussian national coverage is 8/8 categories versus 4/8 for the
current generator; municipal coverage is 93.30% versus 88.07%, mean MAE 5.44
versus 5.68 pp. Gaussian national mean MAE is 3.50 versus 3.74 pp. These are two
independent elections, not eight independent national observations.

The spatial bootstrap inherits the Gaussian national forecast but often
misallocates it locally. Its pooled profiles and tiny transition library do not
outperform the simpler latest-profile hierarchy. Do not choose it as the default.

## Results: one and two years before voting

The same mixed-history six targets, with no change of parameters after inspecting
the held-out scores:

| Days before vote | Model | National coverage | National width | National mean MAE | National WIS | Municipal coverage | Municipal mean MAE |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 730 | Current | 70.8% | 25.18 | 9.57 | 5.00 | 79.38% | 10.65 |
| 730 | Gaussian | 87.5% | 41.10 | 9.94 | 5.20 | 86.27% | 11.21 |
| 730 | Student-t | 79.2% | 37.40 | 9.95 | 5.37 | 83.93% | 11.16 |
| 365 | Current | 66.7% | 19.44 | 7.06 | 3.57 | 79.01% | 7.97 |
| 365 | Gaussian | 87.5% | 31.52 | 7.34 | 3.91 | 89.81% | 8.34 |
| 365 | Student-t | 87.5% | 30.35 | 7.35 | 3.93 | 88.69% | 8.35 |
| 90 | Current | 70.8% | 12.96 | 4.26 | 2.05 | 86.22% | 5.81 |
| 90 | Gaussian | 91.7% | 17.60 | 4.24 | 2.11 | 90.79% | 5.75 |

Precision improves toward election day, but no model delivers dependable
long-horizon calibration and means in this small panel. In particular, the
new models' wider bands are not a uniform gain: year-/two-year WIS and mean
errors are worse than current. The initial 2013 M5S surge is an important failure,
including forecasts with only prior covariance information. It is not removed
from these stress tables.
Prior-only main fits are all four Camera-only 2013 origins, plus mixed-history
Camera 2013 and European 2014 at 730 days. They remain labelled by empty training
IDs in the full report.

## Plausibility and accounting

All 36 four-category forecasts × four models × 256 full municipal draws pass
the tested hard numerical/integer/electorate checks. New models preserve their
sampled continuous national margins to below 1e-8; maximum observed integer
national discrepancy is 0.00567 pp (0.00310 pp in mixed-history forecasts).
These results verify accounting, not complete
legal filing support or realistic party geography.

At 15 days, none of the models produces a national category above 80%. At 730
days, this stress rate is **0% current, 3.71% Gaussian, 3.19% Student-t**. Across
all mixed horizons, municipal concentration above 95% occurs in 0.236% of
eligible current draws, 0.442% Gaussian and 0.587% Student-t; actual matched
outcomes average 0.265%. Eligibility requires at least 500 valid votes and is
draw-dependent. These are coarse four-category diagnostics: other can group
many parties, and real local strongholds exist. They do not warrant mechanically
clipping distributions at those thresholds.

The tested Gaussian fit therefore fails a claim of uniformly better
plausibility. Before runtime adoption, examine extreme **individual-list**
geography and estimate conditional bounds/donor uncertainty from training data,
then backtest any bounded distribution as a new candidate. Do not call an
untested truncation calibrated. Long-horizon national opinion drift needs denser
multi-house historical waves and stronger chronological validation; a terminal
poll error floor alone is insufficient.

## Camera-only versus Camera plus European results

The Gaussian model on identical Camera target elections, including the initial
2013 stress case:

| Days | Training/history | National coverage | National mean MAE | Municipal coverage | Municipal mean MAE |
| --- | --- | ---: | ---: | ---: | ---: |
| 730 | Camera only | 83.3% | 9.04 | 83.57% | 11.16 |
| 730 | Camera + European | 83.3% | 8.95 | 83.43% | 10.65 |
| 365 | Camera only | 83.3% | 7.09 | 85.04% | 9.09 |
| 365 | Camera + European | 83.3% | 7.14 | 87.50% | 8.57 |
| 90 | Camera only | 83.3% | 4.25 | 91.95% | 6.76 |
| 90 | Camera + European | 91.7% | 4.32 | 92.21% | 6.16 |
| 15 | Camera only | 91.7% | 3.66 | 92.47% | 5.93 |
| 15 | Camera + European | 91.7% | 3.74 | 91.90% | 5.38 |

Mixed history improves local expected-value error at every tested horizon.
National mean error is not consistently improved. Preserve cross-kind vote-share
information and test kind-dependent adjustments rather than assuming either
complete interchangeability or complete incompatibility. The current experiment
uses the latest same-kind national valid-ballot rate as its rate anchor, even
when another kind supplies the latest spatial vote-share profile.

## Six-category Camera sensitivity

Here other is split to expose FdI and FI/PdL. These scores compare models within
this six-category panel; their averages cannot be directly compared with the
four-category averages, because category weights and dimension differ.
Only Camera 2018/2022 contribute to the scored aggregate.

| Model | National coverage | National mean MAE | National WIS | Municipal coverage | Municipal mean MAE | Municipal WIS |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Current | 75.0% | 2.730 | 1.169 | 90.05% | 5.173 | 2.684 |
| Joint Gaussian | 100.0% | 2.417 | 1.098 | 91.97% | 5.029 | 2.690 |
| Joint Student-t | 100.0% | 2.419 | 1.101 | 91.13% | 5.039 | 2.679 |

FdI municipal coverage improves from 84.82% to 94.32% Gaussian / 93.45% Student-t;
mean MAE improves from 5.62 to 5.10 / 5.04 pp. FI/PdL municipal coverage changes
from 97.14% to 94.57% / 93.96%; mean MAE from 3.12 to 3.06 / 3.08 pp.
Gaussian municipal width grows from 22.37 to 25.00 pp. National width grows from
6.86 to 12.07 pp; the single 90% interval score actually favors current, whereas
national WIS favors Gaussian. This again shows a coverage/sharpness tradeoff.

All six-category hard checks pass and no national category exceeds 80%. Local
concentration stress increases from 0.034% current to 0.089% Gaussian / 0.116%
Student-t. The new candidates' national **valid-ballot-rate** coverage is only
1/2 elections, versus 2/2 current. Vote-share improvements do not establish a
validated turnout/ballot-validity model. Full European polling splits and every
minor/new list remain outside this sensitivity.

## Production design to take forward

The selected family should use a joint national endpoint, conditional spatial
allocation, and reconciliation **after** both valid-ballot weights and geography
are sampled. Keep the scrutiny algorithms independent of these statistical
choices. Adoption requires the following bounded implementation steps:

1. Generalize compositional dimensions and model stable political families plus
   conditional within-family splits. A single other-category error cannot be
   independently attached to every list. Preserve fitted-artifact versions and
   reproducible RNG namespaces; distinguish historical sampling zeros from
   legal filing zeros.
2. Retain the editable correspondence editor. Version each default by source
   election and forecast date. Exact stable-party renames are safe defaults;
   PdL→FI/FdI/NCD, left/green→AVS and Azione-IV splits are labelled donor
   conventions. Centre editable split weights on information known at the
   cutoff, never on held-out target municipal results. Sample split uncertainty
   jointly, conserving source mass. New lists need an explicit donor mixture or
   a broad geographic prior and additional uncertainty.
3. Separate electorate, turnout and ballot validity. This study's fifth
   coordinate is **valid votes/electors**, not turnout. Use dated geography and
   demographics available at forecast time, with explicit merger/crosswalk
   reconstruction and electorate uncertainty.
4. Apply national, regional and municipal overrides under the existing
   specificity rules, with explicit feasibility checks. Sample geographic
   residuals conditional on compatible margins and filed support. Use controlled
   integer rounding with documented row/column tolerances.
5. Fit and validate Camera/Senato contrasts separately from common political
   error; older 25+ Senate electorates are not directly interchangeable with
   current 18+ electorates. Recheck full-list geography, dependence and seats,
   then run worker/browser acceptance before activating a default.

The four-/six-category experiments do not validate every current list, Senate,
seat probabilities, or municipal-election candidate/runoff behavior. A
recommendation here selects a statistical family and composition architecture;
it does not close those production acceptance gates.

## Replay

From `web/`, a clean clone first downloads the already-pinned official archives:

```powershell
npm run data:polls:calibration:download
npm run data:forecast:history:check
npm run data:forecast:check
npm run data:forecast:generate
npm run data:forecast:six:check
npm run data:forecast:six:generate
```

No R caches or generated legacy snapshots are required for the research path.
Reports and per-fold caches are immutable ignored artifacts under
`data/derived/generation/`. The small authoritative poll JSON, source manifest
and research code are tracked. See [methods](vote-generation-methods.md) for
filtered replay commands and every implementation entry point.

The complete four-category report is
`data/derived/generation/municipal-generation-v1-c332a0b4b6d2/report-14125e1b.json`;
the six-category report is
`data/derived/generation/camera-six-category-v1-cbed83d338e3.json`.
The latter contains exact reviewed correspondence defaults, individual-party
scores and prior-only 2013 output. Municipal matching retains 99.03–99.95% of
target valid votes in main forecasts; the six-category scored pairs retain
99.16% and 99.55%. These are audited partial historical panels, not a claim that
every administrative change has already been reconstructed.
