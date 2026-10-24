# Weighted-poll uncertainty: research and integration proposal

Use this guide for uncertainty-model choice and geographic compatibility. The
[calibration guide](polls-calibration.md) owns source selection and the earlier
benchmarks. This study adds research commands; it does not activate a model.

For the later complete municipal-generation experiments and longer-horizon
polling, use the [generation comparison](vote-generation-research.md). Its
conditional geographic tests supersede this study's constructed-fixture audit
as empirical evidence; the terminal-only results below keep their original scope.

## Recommendation

Keep weighted polling as the central estimate. The first candidate for runtime
integration is a **joint Student-t endpoint-error distribution in log-ratio
coordinates**, with a covariance prior and sensitivity checks. Keep the
regularized Gaussian version as a comparator: the present data do not establish
a decisive winner between them. Use election-block resampling as a sensitivity
check, not a dependable tail estimator. Exact conformal bounds are currently
uninformative at 90%.

The key change is to model the error of a **whole election share vector**,
including dependence between parties and error common to pollsters. Adding more
polls does not eliminate that common error. This motivation is supported by
[Shirani-Mehr et al., Disentangling Bias and Variance in Election Polls](https://sites.stat.columbia.edu/gelman/research/published/polling-errors.pdf).
Their US estimates are not imported as Italian calibration parameters.

## Data and replay

Use the same 16 reviewed survey questions and six official election targets.
Five expanding chronological holdouts, of which two are Camera, remain the
validation units. Every candidate fits only earlier elections. The four
categories are PD, M5S, Lega and other lists, all positive in this selected panel.
Training horizons are restricted to the selected 15-16 days between cutoff and
first voting day. No new download, PDF, source pin or polling transcription is
required for this study.

From `web/`:

```powershell
npm run data:polls:uncertainty:check
npm run data:polls:uncertainty:generate
```

The generated `poll-uncertainty-v1-<hash>.json` report under
`data/derived/polls/` contains every fit, interval, score, prior sensitivity,
same-kind sensitivity and a constructed geographic audit. Entry points:

| Task | File |
| --- | --- |
| Fit/sample/score endpoint distributions | `web/scripts/data-preparation/poll-uncertainty.ts` |
| Load verified JSON and write immutable study | `web/scripts/data-preparation/poll-uncertainty-cli.ts` |
| Audit actual regional/local generation mechanics | `web/scripts/data-preparation/poll-geography-audit.ts` |
| Numerical and leakage regressions | `web/scripts/data-preparation/poll-uncertainty.test.ts` |

## Estimators tested

All three distributional candidates use three orthonormal log-ratio coordinates
for the four shares. Transforming back produces positive shares summing to one;
the covariance represents joint share movements rather than independent
party-specific errors. See
[Egozcue et al., Isometric Logratio Transformations](https://doi.org/10.1023/A:1023818214614).

Let `e = ilr(actual shares) - ilr(weighted polls)`. Retain zero error location:
the weighted polling point is not changed by a fitted bias correction. Use
uncentered error outer products, so persistent error is included in uncertainty
instead of subtracted as an estimated mean. These are **total endpoint errors**:
sampling, common nonresponse/turnout mismatch and blackout movement are not
separately identified. Do not add the old national movement or measurement
variance on top of these predictive distributions.

### Regularized joint Gaussian

Use an inverse-Wishart prior for the three-dimensional error covariance:
`nu0=5`, `Psi0=0.2^2 I`. Its mean covariance is `0.2^2 I`. After `n` earlier
elections, `Psi=Psi0+sum(e e')` and posterior mean covariance is `Psi/(n+1)`.
Sample a zero-centered Gaussian with that covariance around the polling point
in log-ratio space. The prior prevents an unsupported singular covariance when
only one or two earlier elections exist.

### Joint Student-t predictive distribution

Integrate out the covariance in that same fixed-zero-mean Gaussian/IW model.
The predictive distribution has `df=n+3` and scale `Psi/(n+3)`. Each sample uses
one common chi-square scale for the whole error vector, not separate tail draws
per party. Its covariance equals the Gaussian comparator's covariance; the
comparison isolates distribution shape. The conjugate predictive construction
follows the covariance-mixture calculations in
[Murphy, Conjugate Bayesian Analysis of the Gaussian Distribution](https://www.cs.ubc.ca/~murphyk/Papers/bayesGauss.pdf).

The prior magnitude is an explicit modeling convention, not a fitted Italian
fact. Prespecified sensitivities use prior standard deviations 0.1 and 0.3, and
a separate fit using earlier elections of the same kind only. None was selected
by tuning the held-out results. This is not a robust Student-t likelihood fit:
large training errors still affect the covariance scatter matrix.

### Symmetrized election-block residual bootstrap

Sample one earlier election's complete log-ratio error vector and a common
random sign. This keeps observed party dependence and leaves the log-ratio
centre at the polling estimate. Symmetry is an assumption. With `n` events there
are only `2n` possible error vectors: it cannot estimate unseen tails reliably.
Do not resample parties or pollsters as if they were independent elections.

### Election-block conformal diagnostic

Use each earlier event's maximum absolute share error as a simultaneous-box
score. The exact 90% quantile rank is `ceil(0.9*(n+1))`; with `n<=5` this exceeds
the available observations, giving full 0-100% bounds. At least nine calibration
events are needed even to obtain a finite rank by this construction; nine would
not establish adequate stability. Election/kind exchangeability is also
unverified. See [Angelopoulos and Bates](https://arxiv.org/abs/2107.07511).
This is a data-sufficiency check, not a proposed production interval.

## Held-out results

Nominal pointwise 90% coverage. Interval score and width are in percentage
points; lower interval score is better. Every row retains fixed polling-point
MAE 3.21 pp overall and 3.48 pp for Camera.

| Weighted-poll estimator | Overall coverage | Average width | Interval score | Camera coverage | Camera interval score |
| --- | ---: | ---: | ---: | ---: | ---: |
| Original national uncertainty | 20% | 3.75 | 33.13 | 0% | 36.51 |
| Previous additional-floor estimator | 65% | 8.49 | 14.85 | 62.5% | 12.39 |
| Regularized joint Gaussian | 100% | 13.29 | 13.29 | 100% | 12.87 |
| Joint Student-t, central prior | 100% | 12.72 | 12.72 | 100% | 12.47 |
| Election-block bootstrap | 85% | 10.68 | 15.06 | 87.5% | 12.21 |
| Conformal box diagnostic | 100% | 100.00 | 100.00 | 100% | 100.00 |

The 100% results are 20/20 category outcomes across five events, and 8/8 for
Camera, not evidence of guaranteed future coverage. Joint coverage of all four
categories is 5/5 for Gaussian/Student-t and 3/5 for bootstrap. Energy scores
(a proper score for the joint predictive distribution) are 4.83, 4.85 and 5.49
pp respectively: Gaussian and Student-t are close, and ordering is not uniform
across metrics. Do not select the best Camera score from just two events.

Student-t sensitivities give 90% overall coverage with the 0.1 prior and 100%
with 0.3; corresponding average widths are 11.17 and 14.83 pp. Same-kind-only
training gives 90% coverage and 12.79 pp interval score; its first European
forecast is prior-only because no earlier European poll/result pair exists.
All three Student-t sensitivities cover 8/8 Camera categories. This dependence
on assumptions must remain visible.

The fixed polling point is not necessarily the marginal mean or median after
inverse log-ratio transformation. The report therefore also scores those
summaries. Central Student-t predictive-mean MAE is 3.25 pp overall (3.45 Camera),
versus 3.21/3.48 for the retained point. Bootstrap median MAE is 4.30 pp overall,
illustrating instability from its very small discrete support.

## Regional/local pipeline compatibility

The existing runtime adds independent party logit shocks at national, regional
and municipal levels, perturbs turnout, then normalizes elector fractions.
Poll measurement uses per-list sigmas. National polling refreshes the national
date, while geographic uncertainty retains the old historical date. IPF aligns
the references **before** those draws, not their realized national totals.

Thus none of the proposed joint endpoint distributions is a drop-in replacement
for the current per-list measurement sigmas. Even the Gaussian needs correlated
vector draws and the correct valid-share transformation. Student-t also needs
one shared tail scale; bootstrap needs one event-vector draw. A conformal box
alone supplies no joint distribution to simulate.

The actual generation audit disables national/measurement noise on the
constructed 20-municipality fixture, with old geographic reference dates. Across
1,000 draws, regional effects alone induce 2.13 pp RMS national-share deviation,
local effects 2.15 pp, and both 2.90 pp (maximum 10.30 pp). With no effects the
deviation is zero. This is a mechanical counterexample, not an estimate for
Italian geography: one equal-sized municipality per region deliberately avoids
large-country aggregation. It proves geographic/turnout shocks can change a
previously specified national distribution. Merely weighted-centering logits
would not fully remove nonlinear normalization effects.

**Redesign the statistical composition layer, retaining geography, filing rules,
historical donors and legal scrutiny.** The proposed order is:

1. Draw a joint national valid-share endpoint vector once per simulation, with
   common political error shared across chambers and separately modeled chamber
   contrasts. A diagonal sigma map cannot encode the fitted covariance.
2. Draw turnout and conditional regional/local deviations around historical
   spatial profiles. National aggregate error is already in step 1; geographic
   residuals must describe allocation conditional on that target.
3. Reconcile each draw to its national valid-vote targets using that draw's
   turnout weights and legal filing support. Reuse/extract the existing IPF
   principle, but run it after geographic draws. Integer allocation must preserve
   row totals and national margins to rounding tolerance. Infeasible support
   remains explicit; use reviewed donor/uniform priors for missing geography.
4. Resolve user scope overrides explicitly before imposing compatible margins.
   Preserve scenario replay and chamber dependencies. Validate regional/local
   and seat coverage separately; national coverage cannot establish them.

These integration changes are proposed, not implemented. The current national
coefficient is not calibrated by the study, and old national noise must not be
retained as an additional endpoint draw.

## What additional data would help

| Data | Availability and purpose |
| --- | --- |
| Earlier poll waves in the same six campaigns | Government references/curated JSON can extend the archive. Waves at roughly 1, 2 and 3 months help separate temporal movement, house differences and sampling error. They do not add independent elections. |
| Municipal/regional election totals | Already present in the pinned official ZIPs. Harmonize geography and party definitions across Camera 2013/2018/2022 and European 2014/2019/2024, then study spatial changes after removing the national swing. Municipalities are conditional geographic observations, not thousands of independent national polling errors. |
| More independent same-kind poll/result pairs | Needed for stronger national validation. Additional older results alone are insufficient: matching polls and defensible party identities are required. Earlier pre-M5S elections cannot simply enter this positive four-category model. |
| Regional polling and comparable chamber data | Useful for local bias and chamber contrasts, but selected regional questions are not national polls. Older Senate 25+ data need an electorate adjustment before temporal pooling. |

The tested covariance is a near-election **total endpoint** budget. Do not
multiply it by square-root time to forecast a year ahead: shared poll error
does not vanish at zero horizon, and the selected cutoff panel cannot identify
a separate temporal diffusion rate. A time-series hierarchy becomes the next
candidate after obtaining within-campaign waves, not another fit claimed from
these six terminal observations. The different roles of measurement error,
movement and geographic correlation are illustrated in
[Heidemanns, Gelman and Morris](https://stat.columbia.edu/~gelman/research/published/Harvard_Data_Science_Review.pdf).

Only the four-category national total has been tested. Specific parties inside
other, including new lists, still need explicit conditional split uncertainty;
do not attach the learned other-category error independently to every member.
Longer horizons, regional/local residuals, full-generator and seat coverage
remain acceptance work before any model becomes active.
