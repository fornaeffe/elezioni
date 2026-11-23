## 2026-10-23 — Weighted-poll joint uncertainty study and geographic integration proposal

At Luca's request, researched uncertainty estimators appropriate to the six
available independent elections and retested weighted polling under each practical
candidate. Added a focused proposal in `docs/polls-uncertainty.md`, with primary
research references, equations, prior/horizon restrictions, source-extension
priorities and regional/local compatibility. Runtime policy and source JSON are
unchanged. The active plan now identifies conditional geographic reconciliation
as a dependency before integrating a calibrated joint national distribution.

Added deterministic offline `data:polls:uncertainty:check` / `:generate` commands.
They reuse the verified calibration cases and existing weighted-poll point
function, then fit only on earlier elections in five expanding holdouts. New
research modules implement joint log-ratio error fitting, simulation and scoring;
no third-party numerical dependency or new source download is required. Existing
calibration report remains exactly `poll-calibration-v2-d9116fb3de48` after
extracting its case loader; original comparison values are unchanged.

Compared regularized Gaussian errors, covariance-integrated Student-t prediction,
symmetrized whole-election residual resampling and exact-rank conformal boxes.
The first two share a zero-mean Gaussian/inverse-Wishart covariance model in
three orthonormal log-ratio coordinates, prior df=5, scale=0.2^2 I. Student-t
predictive df is n+3; Gaussian uses the same posterior mean covariance. Prior
0.1/0.3 and same-kind-only fits were prespecified sensitivities, not tuned on the
held-out outcomes. Keep complete election vectors and uncentered errors; no
partisan/house mean correction is fitted. Bootstrap symmetry is an assumption,
and only 2n residual vectors exist. Conformal rank ceil(0.9*(n+1)) exceeds n for
all folds, so its 90% bounds are 0-100%; exchangeability is also unverified.

All fits target total near-election endpoint error, including sampling, shared
polling errors, blackout movement and electorate mismatch. Adding the old
national movement/measurement variance again would double count error. The
selected 15-16 day horizons do not identify a separate time-series diffusion or
justify year-ahead extrapolation. Four-category dependence cannot be transferred
independently to each party inside other. The weighted point is retained, but
inverse log-ratio predictive means/medians can differ and are scored separately.

Pointwise nominal 90% results below, over five events/four categories (Camera:
two events/four categories). Width and interval scores are percentage points;
lower interval score is better. Fixed-point MAE stays 3.21 pp overall / 3.48 Camera.

| Weighted-poll uncertainty | Overall coverage | Average width | Interval score | Camera coverage | Camera interval score |
| --- | ---: | ---: | ---: | ---: | ---: |
| Original national uncertainty | 20% | 3.75 | 33.13 | 0% | 36.51 |
| Previous additional floor | 65% | 8.49 | 14.85 | 62.5% | 12.39 |
| Regularized joint Gaussian | 100% | 13.29 | 13.29 | 100% | 12.87 |
| Joint Student-t, central prior | 100% | 12.72 | 12.72 | 100% | 12.47 |
| Whole-election bootstrap | 85% | 10.68 | 15.06 | 87.5% | 12.21 |
| Exact conformal box | 100% | 100.00 | 100.00 | 100% | 100.00 |

Joint coverage of all categories is 5/5 events for Gaussian/Student-t, 3/5 for
bootstrap. Gaussian/Student-t energy scores are close (4.83/4.85 pp); the small
panel does not establish a decisive ranking. Student-t prior 0.1 and same-kind
fits give 90% overall coverage, prior 0.3 gives 100%; all cover 8/8 Camera
categories. Default Student-t predictive-mean MAE is 3.25/3.45 pp overall/Camera.
Bootstrap marginal-median MAE is 4.30 pp overall, exposing its discrete small-sample
instability. Twenty correlated category outcomes are not twenty independent
elections, and perfect historical coverage is not a future guarantee.

A separate constructed-fixture audit exercises the actual paired municipal
generator, with national/measurement noise disabled. Across 1,000 simulations,
regional-only effects move the national share by 2.13 pp RMS, local-only by
2.15 pp, and both by 2.90 pp (maximum 10.30 pp); all-zero effects give zero drift.
The equal-sized one-municipality-per-region fixture is a mechanical counterexample,
not an empirical estimate of Italian geographic error. Current IPF aligns
references before sampling, so later geographic/turnout shocks can alter national
margins. None of the new joint distributions is a drop-in diagonal sigma map.

Recommend Student-t as the first integration candidate, with Gaussian as a close
comparator and bootstrap as a sensitivity diagnostic. Rework statistical
composition: draw the national endpoint vector once, draw turnout and geographic
residuals conditional on it, then reconcile each realization to those national
valid-vote margins using legal filing support and realized turnout weights. Reuse
IPF, historical profiles/donors and legal scrutiny; preserve overrides and shared
chamber dependence. This redesign is proposed, not activated. Official municipal
archives already available can inform spatial residuals after harmonizing party
and geographic definitions; earlier within-campaign poll waves can inform time
and sampling components but do not increase independent-election counts.

Study version `poll-uncertainty-v1-b93db6c425fe` regenerated with identical bytes.
The JSON report retains all folds, covariance fits, intervals, prior sensitivities,
proper interval/energy scores, predictive summaries and geographic audit evidence.
Verification: all 965 unit tests in 70 files passed (12.24 s); the seven research
tests also passed after the final overlap guard check. Tests cover ILR geometry,
closed-form covariance/Cholesky identities, chronological leakage, prior/kind
isolation, unsupported horizons, conformal rank and actual geographic leakage.
Preparation/application type checks passed, zero Svelte errors/warnings. Build
passed (4.45 s SSR). Browser verification follows below. No active uncertainty,
default, legal algorithm, deployment or source-policy change was made.
The subsequent full root-base Chromium suite passed all 30 tests (50.1 s).
Git diff whitespace checks passed; chunk/plugin/color notices remained
informational. No commit or publication was performed.
