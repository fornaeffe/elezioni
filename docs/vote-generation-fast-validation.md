# Fast vote generation: frozen validation protocol

FV-01 revision **1**, frozen 28 October 2026 **before the TypeScript port or new
production outputs**. The scope is FV-02 numerical acceptance through FV-08 reform
adoption. Record results, failures and any protocol revision in migration history;
do not tune these rules after seeing a failed result. The
[model](vote-generation-fast-model.md), [scenario](vote-generation-fast-contracts.md)
and [references](vote-generation-fast-references.md) define the targets.

## Fixture/source/seed budget

The [FV-01 evidence manifest](../research/vote-generation/fv01/evidence-manifest.json)
pins the original inputs, selected summaries and failures, source/code hashes,
and the compact independent fixture definitions. Verification needs no fit or
external numerical environment. Research reproduction keeps its recorded seed
20261027 and original source revision; new ports use seeds
`fv02-numeric-0/1/2`, `fv04-reference-0/1/2` and `fv08-production-0..999`.
Keep four independent fit chains; run seed and fit seed are separate.

| Stage | Cases and minimum budget |
| --- | --- |
| FV-02 reduced fit | Existing interior, near-zero, joint-with-distinguishing-information and coupled-context matrices; three seeds, four chains each; 1,000 burn sweeps, thin 2 and 2,000 retained draws/chain, increasing deterministically to 4,000 then 8,000 if diagnostics fail |
| FV-02 profiles | Hand-computed observed-zero, missing history, pooled joint feature, regional fallback, context mismatch, reference-date propagation and common-versus-filter scale fixtures |
| FV-04 transitions | 120,000 two-year forecast draws for the recorded martingale fixture; additionally zero horizon, exact zeros/ones, zero scales, reordered IDs and component-specific controls |
| FV-04 interventions | At least 50,000 independent reference draws for single/multiple soft and complementary-full references, plus exact/grouped/new/common/chamber/local/turnout examples; compare to independent integration |
| Research-domain replay | Frozen selected variant, 16 development + 8 corrected-confirmation cases at 730/365/90/15 days, 512 forecast draws/case, the same systematic 128-unit geography; then the full 7,821-future/8,194-profile compatible benchmark |
| FV-08 production | Complete new default and all physical reform cells, at least 1,000 paired simulations through generation, scrutiny, storage and exports; single-chamber and dependence sensitivity separately |

Input envelopes to probe before setting browser support: 28/64/128 fitted
political components, 0/128/512 selected poll questions and the complete bundled
history. More than 128 components or 512 questions is initially outside the
planned v1 envelope; validate/revise limits before acceptance, never silently
discard observations. The actual default must fit inside the accepted envelope.
If a browser target cannot fit the maximum envelope, either improve the engine
or record an explicit bounded support decision before defaults are selected.

## Numerical gates independent of chain convergence

All calculations use Float64 and declared canonical ordering. Compare
covariance/precision/mode objectives with a separately constructed dense reduced
reference: relative error <= `1e-9` and absolute error <= `1e-11` for scaled
well-conditioned fixtures. Check feasibility/simplex residual <= `1e-12`, and
KKT stationarity/complementarity <= `1e-8` after explicitly recording scaling.
Near-singular/tail fixtures must report conditioning and the approved arithmetic
regularization; no new statistical ridge or discarded correlation is permitted.

The actual research fixture harness has absolute mean/endpoint ceilings
0.005/0.008 in fraction units; the report's 0.151/0.356 pp are **observed errors**,
not those harness thresholds. For the port freeze both requirements:

- Each mean error <= `min(0.005, 0.001510236 + 4 * combined mean MCSE)`.
- Each 5%/95% endpoint error <=
  `min(0.008, 0.003551512 + 4 * combined quantile MCSE)`.
- Independent quadrature mean refinement <= `1e-6`; refine integration until
  the endpoint reference is stable within `1e-5`. Estimate quantile MCSE by
  predeclared chain/block resampling, not a normal approximation to the mean.

Require these across coordinates and the three frozen seeds. Record the exact
target/reference integration and Monte Carlo uncertainty; do not compare raw
draw bytes between Python and TypeScript RNGs. Within the new engine, repeat
identical inputs/seeds byte for byte, including Node/browser and serial/parallel
simulation scheduling. Verify entity-addressed stream isolation when a list is
renamed, display rows reorder or another simulation finishes first.

Convergence gates remain rank-normalized split/folded R-hat <= 1.05, bulk ESS >=
400 and tail ESS >= 200 for every free coordinate. Known constrained constants
are marked as such with null diagnostics and proof of the constraint; they do
not turn an all-stuck unconstrained chain into a passing fit. Reject non-finite
state, broken simplexes, diagnostic failure after the maximum budget and missing
chain diagnostics. Never replace posterior draws with modes/Hessian normals.
Retained portable-state subsampling must pass the same moment/endpoint checks.

Reference-family calibration must meet the separate `1e-6` share mean/SD and
`1e-7` integration-refinement contract. Sample-based checks must also agree
within four MCSE; tests cover multiple simultaneous SDs and impossible moments.
Keep turnout fixed in list-reference examples, and separately test non-valid
intervention and availability-removal behavior. The frozen rational/worked
fixtures distinguish these cases without relying on the future engine itself.

## Forecast/geographic/count invariants

Require exact identity at zero transition horizon/all-zero transition scales,
and exact structural zeros/ones where specified. The independent martingale
fixture mean error must be < `0.002` and within four mean MCSE; record the old
0.0006342 observed result without presenting it as a guaranteed tolerance.
The >80% named-component rate is <= 0.005 for this particular fixture only.
Do not turn that small stress screen into a universal political cap.

Every physical cell conserves electors exactly with integer nonnegative counts;
municipal/region/country sums and joint-list sums must agree exactly. No double
Camera-TAA ballot count. Projection's maximum expected-elector margin residual
is <= `1e-9` within 120 iterations, with explicit unsupported-target/capacity/
empty-choice-set errors and no target repair. Chunked and unchunked global
projection must agree within `1e-10`; independent per-chunk raking is invalid.
Observed source zero, missing history, new support and unfiled support have
different tests. Local profile filtering and future common local scales must
match their distinct formulas. Check electorate crosswalk conservation and
special-territory support separately from the old excluded research domain.

## Predictive and seat-distribution acceptance

Preserve the original research eligibility/cutoffs first. Report valid and
elector-share results separately, by observable group and election/horizon,
nationally/municipally/regionally. Report within-pooled-component splits only
where independently observed. Weight/denominator definitions remain the original
equal-group/equal-case, equally weighted municipal units for replay; add
elector-weighted summaries as additional labelled metrics.

For port parity, require coverage loss <= 3 percentage points, WIS increase <=
10%, and mean-MAE increase <= 0.25 pp national / 0.50 pp municipal relative to
the pinned selected candidate, allowing four **simulation** MCSE. Recompute the
comparator with matching frozen information and seeds/draw budgets when its
Monte Carlo error is unavailable; do not infer MCSE from the number of correlated
municipalities or treat the horizons as independent elections. Predeclared
resampling holds truth/case information fixed and resamples predictive draws in
chain blocks, with 1,000 replicates. Report election-cluster sensitivity separately
from simulation error. Corrected confirmation remains corrected, not untouched.

Expanded defaults/Senate/special domains have no equivalent calibrated research
baseline. Report plausibility first, interval coverage/width second, then MAE,
signed group errors, WIS and sparse-event tails against the current installed
generator on matched historical information. A deterioration beyond the above
margins or an unexplained concentration/coverage defect blocks default selection
pending diagnosis and a recorded decision; do not claim universal 90% coverage
from six elections or optimize settings on confirmation. Preserve new-party,
long-horizon, sparse-history and age/geographic limitations explicitly.

The 1,000-paired-run production slice must produce no invalid generated legal
input, no lost elector/vote counts and no unreported technical failure. Compare
admission, first-place/42%, bonus/caps, seat distributions, filled/unfilled seats
and candidate outcomes. Keep non-computable runs in requested denominators;
report the change in that rate and its interval. An increase > 1 percentage
point beyond four simulation MCSE is a default-selection blocker needing review.
Fixed-input legal regressions must remain identical; do not change scrutiny,
reroll failures or drop inconvenient runs to pass generator acceptance.

## Performance, browser and release gates

Before full-country work, measure the reduced fit/profile case and extrapolate
dimensions/time/workspace. Run full cases only after a plausible bounded path;
failure is an engineering result, not permission to weaken diagnostics. The old
Stan 30-minute research gate is not a blocker for this selected model.

Retain the production plan's initial targets: <= 10 MiB compressed default
generation assets, <= 25 MiB refit data, complete fit <= 30 seconds documented
desktop / <= 120 seconds at 4x CPU throttle, cancellation feedback <= 1 second,
fit workspace <= 256 MiB and aggregate active compute/data memory <= 512 MiB
(persisted result storage excluded). Report transfer/decode/fit/generation,
temporary copies, posterior retention and simultaneous worker memory separately.
Do not infer browser time/RAM from Python. Run five warm repetitions and one
cold run per accepted envelope; retain median/max and actual machine/browser/
version/worker count. Freeze/revise hardware targets after FV-02/03 measurement
and before FV-08 output inspection, with the reason recorded in history.

Required production checks: data integrity, `check`, `check:data`, numerical and
meaningful unit tests, build, production generation checks, and browser edit/
refit/cancel/retry/run/storage/plot/export/import workflows at `/` and `/elezioni`.
Include direct loads/refreshes, compressed and HTTP-decoded assets, stale reply
rejection, missing/corrupt assets, default zero-fit start and constrained-memory
single-worker execution. Exercise Chromium, Firefox and WebKit targets and record
what actually passed. Prepare from a cold-cache Linux clean clone and repeat
offline/warm with identical pinned assets. No required release skip, debug data
fallback or publication before the final whole-migration hosting gate.
