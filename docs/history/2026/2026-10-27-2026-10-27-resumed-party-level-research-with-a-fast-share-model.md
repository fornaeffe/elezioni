## 2026-10-27 — Resumed party-level research with a fast share model

Luca authorized model simplification for practical performance while preserving
plausible municipal list distributions, election/poll information and
historical/joint/new-list scenario support. The focused
[fast research report](../../../research/vote-generation/docs/vote-generation-fast-research.md) now owns the model,
mathematics, commands, artifact map and acceptance limits. Earlier Stan results
remain historical evidence; production generation/editor/defaults are unchanged.

- Tried exact Gaussian marginalization of the former joint-state model. The
  two-city case passes independent density/gradient and sampling checks in
  0.72 s. A richer four-city case exceeds a 120 s profiling limit; fixed-scale
  profiling takes 115.30 s but fails R-hat/ESS. A national-only grouped
  Dirichlet-multinomial experiment also exceeds the short limit. No failed or
  timed-out chain is accepted; the waived final-Stan 30-minute gate remains waived.
- Implemented a bounded Gaussian share-information reference target, exact
  native truncated-normal conditional sampling, four-chain diagnostics,
  mean-preserving Beta-stick forecasts and empirical geographic filtering.
  This deliberately replaces the former likelihood and joint municipality-state
  posterior; original grouped observations/counts are never replaced by inferred
  component counts. Country-margin probability projection before integer draws
  is another explicit model change, now documented.
- Added a headless scenario prototype for indivisible historical units, joints,
  new units with mandatory references, reference uncertainty and volatility
  overrides. It preserves choice-set substitution and exact list/elector
  aggregation. Added a portable compressed research posterior/profile contract;
  browser worker/editor integration, filing and paired-chamber validation remain open.
- Frozen development selects common local volatility, separate regularized kind
  contexts and conservative election discrepancy. Corrected 512-draw replays
  cover 16 development and eight confirmation forecasts. Confirmation averages
  86.0% national and 91.3% municipal valid-share coverage at nominal 90%.
  Only two confirmation elections exist, with party-specific errors still
  material; this does not establish unbiased means or full-list calibration.
- The first confirmation attempt found a loader that discarded already pinned,
  reviewed FDI/FI answers in 13 early survey views, plus a generic optimizer
  failure at a feasible mode. Restored the same-survey answers with input/date/
  core-share checks and replaced the solver with an independently verified convex
  active-set implementation. Statistical settings were not retuned from
  confirmation; both phases were replayed. These corrected replays are not
  described as untouched holdouts. No source archive was changed.
- Wider `all`-catalog preparation exposes unsupported future-only residuals and
  now rejects them explicitly rather than repeating an unresolved merge.
  They require reference information or an observed residual; no historical
  votes/splits are invented. The existing provisional membership review remains open.

Computability gate: 128-unit timings projected 4.98 s fitting / 20.21 s generation
at complete compatible scale. The full benchmark, on the i7-14700KF under WSL
Ubuntu with single-thread BLAS/OMP, measures **5.18 s total fitting**, **10.72 s
for 512 draws across 7,821 future municipalities**, **25.82 s including scores/
posterior writes**, and **1.90 GiB** peak Python resident memory. It processes
55,605 training municipality/election cells and the 8,194-identity geography
union; Valle d'Aosta/unmatchable source rows remain outside this research domain.
The 30-minute approximate-model gate passes comfortably. Full-domain municipal
90% coverage is 92.9%, mean absolute error 2.89 pp. All integer elector totals
are conserved; probability-margin residual is below $10^{-9}$.

Verification: `npm run check:data` passed; full `npm test` passed **80 files /
1,075 tests**. Eleven Python functional tests passed. Independent four-fixture
quadrature bounds mean error at 0.151 pp and 90% endpoint error at 0.356 pp;
the 120,000-draw martingale check bounds mean error at 0.064 pp. All 24 corrected
real reference fits pass R-hat/ESS/tail-ESS gates. Repeated 3.67-MiB exports are
byte-identical and pass dimension/probability/common-scale checks; modified
prepared input is rejected before export. `git diff --check` passed.

Final generated evidence is indexed in the fast report. The production
comparator, broader source/list calibration, actual upcoming-election input
refresh, browser parity and legal-geography/chamber integration still gate any
adoption. No production bundle, source election, R/legal code, commit or public
deployment was changed.
