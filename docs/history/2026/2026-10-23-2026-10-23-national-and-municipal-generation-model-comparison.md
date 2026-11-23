## 2026-10-23 — National and municipal generation-model comparison

At Luca's request, added a research-only replacement-model comparison, keeping
production generation, scenario editing, legal scrutiny and bundled defaults
unchanged. Findings and implementation boundaries are in
`docs/vote-generation-research.md`; equations, metrics, source restrictions and
code pointers are in `docs/vote-generation-methods.md`. The active plan records
the completed study separately from runtime implementation and acceptance.

Migrated the historical municipal input path needed for this study to offline
Node/TypeScript. It reads the eight existing pinned Ministry Camera/European
archives, reconstructs attributed candidate-only votes where necessary, retains
raw list tables and voters/electors/valid ballots, and emits versioned history
with source/mapping/geography audits. This is a partial preparation migration;
it does not retire legacy `dati/` or regenerate every production snapshot.
Historical matching conserves and reports source mass: main forecast pairs
retain 99.03–99.95% of target valid votes. Invalid municipal turnout denominators
remain in source national totals and are excluded from local fitting/scoring,
not silently corrected. The audit exposes Camera2018's 142-vote Lega/other
attribution discrepancy and 17,442 European2024 fuori-sede ballots lacking
resident-municipality attribution. Both election kinds use a common comparison
domain excluding Valle d'Aosta; no absent region receives invented zero shares.

Added 17 reviewed primary Demos observations and 18 early-cutoff selections in
separate authoritative JSON, with publication/fieldwork dates and optional
source PDF/image pins. The existing government archive is unchanged. Some
observations were up to 97 publication-days old at cutoff; all ask about Camera,
so their use for European targets is explicitly cross-kind. A separate six-party
input preserves seven complete Camera terminal questions and reviewed extra
early evidence; unreported FdI is never treated as observed zero.

The main experiment replays six targets at 730/365/90/15-day origins, comparing
12 Camera-only and 24 mixed-history forecasts. Every source, fitting outcome,
and fitting election's own predictor is filtered at its correct historical
cutoff. Forecast electors come from the historical baseline, never the held-out
target. The actual native generator is called with common central references
and its current poll-v1 measurement sigmas, then compared with regularized joint
Gaussian, equal-covariance predictive Student-t and a Gaussian-national/spatial
transition-bootstrap hybrid. New candidates draw the national endpoint once,
sample joint regional/local/valid-rate deviations, then reconcile each draw
under its sampled valid-vote weights before row-conserving integer rounding.
The bootstrap additionally pools historical profiles and uses signed transition
fields. Models are research modules, not runtime registry additions.

Metrics follow Luca's order: hard accounting and concentration stress;
50/80/90/95% coverage with widths, interval/WIS/energy scores; predictive-mean
MAE/RMSE/bias at national/regional/municipal levels, including voter-weighted
municipal versions. Coverage by all pointwise intervals is reported without
calling it a separately calibrated simultaneous confidence region. Elections
are the independent validation units; municipalities/horizons do not inflate N.
The study uses explicit priors, no fitted partisan bias correction, and does
not extrapolate the previous 15-day covariance by square-root time. Training
includes earlier no-poll fallbacks when that was the available information,
which is a conditional-calibration limitation.

Complete main artifact: `municipal-generation-v1-c332a0b4b6d2/report-14125e1b.json`
under `data/derived/generation/`. All 36 forecasts × four models × 256 full
municipal draws pass hard numerical/integer/electorate checks. Continuous
national margin error is below 1e-8; maximum observed rounded discrepancy is
0.00567 percentage points (0.00310 in mixed forecasts). Near voting (mixed, six targets), current/Gaussian/
Student national 90% coverage is 58.3/91.7/91.7%; municipal coverage is
85.93/91.21/90.68%, municipal predictive-mean MAE 5.05/4.97/5.01 pp. Gaussian
national WIS improves from 1.65 to 1.48 pp, but municipal WIS worsens slightly
from 2.57 to 2.66 as intervals broaden. The spatial bootstrap has inferior
municipal coverage/means and is not recommended.

Long-horizon rejection is material: at 730 days, Gaussian coverage is better
but national MAE/WIS are 9.94/5.20 pp versus current 9.57/5.00, with national
category concentration above 80% in 3.71% of Gaussian draws versus none current.
Student-t does not resolve that tradeoff. These coarse thresholds include the
changing other group; they are stress diagnostics, not automatic impossibility
rules. Initial 2013/new-party failures remain in the tables. No tested candidate
is accepted as an unchanged operational year-/two-year forecasting model.

On the same Camera targets, adding European history improves Gaussian municipal
mean MAE at every horizon (15 days: 5.93 to 5.38 pp), while national mean gains
are not uniform. Recommend retaining cross-kind vote-share information and
testing adjustments empirically. Recommend the joint Gaussian conditional
architecture as the primary replacement candidate, with Student-t sensitivity;
this is a family choice, not a claim that all long-horizon requirements passed.

Six-category sensitivity artifact:
`data/derived/generation/camera-six-category-v1-cbed83d338e3.json`. It separately
models PD/M5S/Lega/FdI/FI-PdL/other, stores the prior-only 2013 audit, and scores
only Camera 2018/2022 at original terminal cutoffs. At 256 draws, current/Gaussian/
Student national coverage is 75/100/100%, national mean MAE 2.730/2.417/2.419 pp;
municipal coverage 90.05/91.97/91.13%, municipal mean MAE 5.173/5.029/5.039 pp.
FdI local coverage rises 84.82→94.32% Gaussian and mean MAE 5.62→5.10 pp.
Municipal WIS is essentially tied. Both candidates' national valid-ballot-rate
coverage is only 1/2 events, despite vote-share improvements. All hard checks
pass; no national category exceeds 80%. These scores are not directly comparable
with four-category averages and do not establish all-list or Senate calibration.

Production follow-up preserves editable source-election correspondence matrices,
reviewed exact-name defaults and explicit uncertain donors/splits, while
generalizing composition dimensions, separating electors/turnout/invalid ballots,
reconstructing dated geography, enforcing filed support and compatible overrides,
and calibrating paired-chamber, regional/local dependence and seats. No clipped
distribution or default correction is activated without its own backtest.

Verification on Windows, Node 24.20.0/npm 11.19.0: `npm run check:data` passed;
`npm run check` found zero errors/warnings; all 990 tests in 74 files passed
(12.60 s), including 25 new research tests. `npm run build` passed (4.57 s SSR),
then the sequential root-base Chromium suite passed all 30 tests (27.6 s), no
skips/retries reported. Research arithmetic/replay/cutoff tests cover sampled
margin conservation, common Student scaling, unavailable outcome mutations,
source reconciliation, six-dimensional geometry and integer accounting.
The main run used three independent processes over disjoint fold sets; typical
native 256-draw generation plus scoring was about 29 s per forecast, Gaussian 5–7 s.
Full cached replay verifies immutable report bytes. Existing chunk-size/plugin
and color-environment notices are informational. No commit, deployment,
production-data write or runtime-model activation was performed.
