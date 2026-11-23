## 2026-10-23 — Original-uncertainty and error-floor benchmark comparison

Extended the historical comparison with four requested rows: previous same-kind
election and weighted polls alone, each with original uncertainty and with
original uncertainty plus a separately fitted error floor. Retained the existing
two point-only rows and all four blend/correction rows. The earlier comparison
table above now includes the four added rows, explicitly labelled as this later
extension. The focused calibration guide documents their timing and scope.

Original uncertainty uses the simulator's pre-poll national logit movement
component, sigma=0.02 per square-root day, inside the same compositional national
surrogate as the existing comparisons. Previous-election references accumulate
movement from the actual previous election date (read from the official source
ID), not merely from the polling cutoff. Weighted polls alone are treated as
references supplied at the cutoff and accumulate movement until election day.
No v1 poll measurement variance, pseudo-sample variance or shared-error term is
silently attached to these original-uncertainty rows. Regional/local shocks,
elector-share/turnout normalization and chamber coupling are outside this
surrogate; these are not claimed as exact full-generator coverage measurements.

Fit each benchmark floor from its own earlier-event errors, subtracting its own
original movement variance and pooling with three prior election equivalents.
Held-out outcomes never enter their prediction, intervals or fitted floors.
Structural zero/one references remain boundaries, matching original generator
support semantics. Their unbounded logit errors are excluded from floor fitting;
usable-event counts and zero fallback floors are explicit. In particular, a
historical-only M5S reference absent from the 2008/2009 ballots cannot be invented
by noise. These new rows are diagnostic; existing runtime acceptance checks and
active scenarios are unchanged.

Added results: MAE and interval scores below are in percentage points; coverage
is pointwise at a nominal 90%, over five holdout events/four categories (Camera:
two holdouts/four categories). Lower interval score is better.

| Benchmark | All MAE | All coverage | All interval score | Camera MAE | Camera coverage | Camera interval score |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Previous election + original national uncertainty | 14.26 | 65% | 98.76 | 11.55 | 75% | 56.00 |
| Weighted polls + original national uncertainty | 3.21 | 20% | 33.13 | 3.48 | 0% | 36.51 |
| Previous election + original uncertainty + fitted floor | 14.26 | 70% | 92.54 | 11.55 | 75% | 55.58 |
| Weighted polls + original uncertainty + fitted floor | 3.21 | 65% | 14.85 | 3.48 | 62.5% | 12.39 |

The historical reference's long uncertainty horizon gives broad intervals but
poor central accuracy and interval score; higher coverage alone is not better
forecast quality. Poll-only cutoff-to-election movement is insufficient for
empirical error in this panel. Its own fitted floor improves intervals while
preserving its central estimates, but does not validate 90% coverage. All six
previous score rows, original fitted bias/house/error parameters, acceptance
checks and actual production reference rows were compared with the preceding
report and are unchanged.

Comparison policy/report is now v2: `poll-calibration-v2-d9116fb3de48`. The
production reference receipt is `poll-reference-v1-3396faf5e852`, scenario suffix
`0786c2ae3948`; actual paired generation and composed scrutiny passed. New report
provenance records baseline dates, original movement variances, extra floors and
per-category training counts. Repeating generation preserves the immutable
version. No poll inputs or source-of-truth policy changed.

Verification on Windows/Node 24: nine focused calibration tests passed, followed
by all 958 unit tests in 69 files (12.40 s). Added tests assert reference dates,
original variance formula, unchanged benchmark means, floor provenance,
structural-zero behavior and held-out interval independence. Preparation and
application type checks passed with zero Svelte errors/warnings. Production
build passed (4.39 s SSR); production reference generation/scrutiny and original
six-row regression comparisons passed. Browser verification follows below.
The subsequent full root-base Chromium suite passed all 30 tests (46.2 s).
Git diff whitespace checks passed. Build chunk/plugin/color notices were
informational. No commit, deployment, default change or correction activation
was performed.
