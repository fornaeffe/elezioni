## 2026-10-24 — Electorate-wide latent-drift research and staged comparisons

Implemented the authorized research path with combined non-valid voting as
outcome 0. The native TypeScript fitter uses observed multinomial counts,
masked absent filings, centered Gaussian innovations, municipal Gaussian-chain
elimination and iterated Laplace working likelihoods. It keeps sample-dependent
poll uncertainty, shared half-year errors, fieldwork measurement dates and
publication availability in the observation layer. Aggregate polls retain
cross-time geographic covariance; future national totals come from municipal
counts without sampled-endpoint reconciliation.

Used the eight pinned Camera/European archives and already downloaded
compatible polls only. Camera 2018/2022 candidate-only attribution is reconstructed
before computing non-valid counts. The input inventory has 38 compatible waves
from 41 questions; three conditional alternatives are audited separately.
No additional polls were downloaded. New research commands, pure numerical
and leakage/accounting tests, protocol and measured report are separate from
production generation and legal scrutiny. The correspondence editor and its
editable defaults remain unchanged.

Completed eight drift variants through the agreed hierarchy on four development
elections and four horizons. The conditional separable-local comparison was not
triggered; both regional innovations and local mean reversion were tested.
Froze aggregate polling + regularized election-kind effects + common national
and local Brownian scales before Camera 2022/European 2024 confirmation.
Retained the actual current generator and all-common literal baseline.
Additional 512-draw development refinement, six-group/current comparisons,
Camera-only training and local-scale perturbations bring the run to 299 cases
and 84,352 country draws. Screening is `latent-drift-v1-367ca6e3b6a9`;
512-draw finalists are `latent-drift-v1-53927eca1222`.

In confirmation, selected/current national political coverage is 84.38%/37.50%
and MAE 5.38/10.46 percentage points; municipal coverage is 96.75%/66.84%.
Regional innovations improve coverage further but have higher pooled stress-tail
frequency. Their worst-case tail ordering reverses under higher Monte Carlo
resolution, so the selected baseline is not an established unique winner.
Participation intervals remain broad; the six-group national sensitivity is
not calibrated to its nominal 90% level. Keep runtime adoption and full-list,
electorate, override, filing, paired-chamber and seat validation open. Exact
metrics, matched-mass audits, stage decisions and limitations are in
`docs/vote-generation-drift-results.md`; priors, inference approximations and
replay commands are in `docs/vote-generation-drift-testing.md`.

Verification: all 253 fitted cases pass declared mode/chain/covariance gates;
all 299 forecasts preserve valid integer/elector accounting. The reduced
seven-dimensional Bayesian reference includes an observed zero; mean differences
from Laplace are at most 0.53 percentage points in its two examples. Preparation
and Svelte checks pass; all 1,002 unit tests pass; static build and then all
30 browser tests pass. No production model/default activation, commit, push or
publication was performed.
