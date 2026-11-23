## 2026-10-29 — FV-02 browser-safe reference fitting and profile estimation

Completed FV-02's reduced numerical/browser gate. The
[engine guide](../../../web/src/lib/politics/reform-2026/vote-share/README.md) maps the
pure Float64 TypeScript modules, typed inputs, entry points and reproduction
commands. The engine implements the selected `kind-common-conservative`
national Camera/European fit, separate non-valid fit and empirical municipal
profiles. It returns retained posterior chains and diagnostics, not a mode-only
replacement. Marked FV-02 complete and kept the unaccepted maximum-envelope
latency explicit in FV-08. Production source preparation, Senate adaptation,
forecasts/interventions, default installation and application refit orchestration
remain later steps.

Ported original disjoint grouped observations, movement/error scales, Brownian
and shared-survey covariance, three GLS construction updates, training-only
election discrepancy and constrained active-set modes. True boundary modes
remain zero. The sampler uses independent addressed chains, exact conditional
pair transfers, stable truncated-normal rejection, four-chain rank-normalized
split/folded R-hat and bulk/tail ESS. Accepted fits retain the frozen
1,000-burn/thin-2 budget and deterministic 2,000/4,000/8,000 draw escalation;
diagnostic failures throw. Known constrained constants are distinguished from
stuck free coordinates. The fitted raw election-discrepancy scale is separate
from FV-04's selected 1.5 forecast multiplier.

Small covariance systems use equilibrated Cholesky solves; larger systems apply
the same matrix through Brownian age prefix/suffix sums, shared-group sums and
block-preconditioned CG. Every solve checks its actual relative residual against
`1e-11`; no extra ridge, covariance deletion or observation thinning was added.
Factoring grouped incidence as `H(S - r*u)` reduced work per record from O(KG)
to O(K + G²). On this host the 28-component/128-poll construction fell from
about 5.86 to 1.46 seconds; 64/128 fell from about 29.85 to 4.87 seconds.
Dense selection for smaller systems also avoids unnecessary iterative solves.
Guards bound dimensions and estimated numerical workspace rather than claiming
to measure browser process memory.

The profile estimator makes two sparse city passes and returns target-ordered
buffers, labelled pooled/missing-history fallbacks, component filtering scales
and separate common future scales. Targets contain IDs, regions and electorates,
not future vote counts. Historical-only cities can inform regional initialization;
observed zeros remain source zeros. Tests cover grouped-only and unmatchable
features, missing/new cities, region/context fallback, cutoff/date propagation
and raw-count validation.

Added an independent pinned NumPy 2.3.4/SciPy 1.16.3 oracle fixture and optional
regenerator. Matrix construction, mode/scales/participation, profiles and
diagnostics are checked against the unchanged separate Python implementation;
adaptive quadrature/CDF root finding independently supplies means and 5%/95%
endpoints for the four frozen posterior targets. Normal TypeScript/browser
validation requires no Python, C++ or Stan. Oracle generation used an isolated
Python environment because the installed interpreter lacked NumPy; research
sources and bytecode were not modified.

All four posterior targets passed under the three frozen seeds, with four
2,000-draw chains each. Maximum mean error was **0.14816 pp** and maximum endpoint
error **0.54804 pp**. These pass both the frozen absolute ceilings and
observed-baseline-plus-four-block-MCSE limits; the latter endpoint error is not
below the old observed 0.356 pp alone. MCSE uses 200 chain-stratified block
resamples of length 50, distinct from later predictive bootstrap budgets.
Maximum R-hat was 1.000972, minimum bulk ESS 7,431 and minimum tail ESS 6,975.
Independent quadrature mean/endpoint refinement was below `2e-13`.

The test-only worker runs without a production route or asset. Initial native
trigonometric normal generation differed between Node and Chromium by one ULP,
so it was replaced with the distribution-equivalent polar method; arithmetic
half-angle FFT roots similarly remove native trigonometric diagnostic rounding.
Identical inputs, solvers and seeds now produce identical raw sample SHA-256
hashes and diagnostics in both runtimes, including all three seeds per target.
Dense versus matrix-free comparisons use numerical tolerances, since their
operation order differs. The reduced full reference fit took 134 ms in Node and
about 209 ms in Chromium 148.0.7778.96; browser profile construction took 1.4 ms.

Recorded the [benchmark snapshot](../../../test/fixtures/benchmarks/vote_share_numerics.json)
with LF-normalized engine/oracle hashes, environment, numerical errors,
diagnostics, measurements and qualifications. Detailed reports remain ignored
under `data/derived/fv02-numerics/`. The Node scaling command probes 28/64/128
components by 0/128/512 poll questions without accepting its short sampler pilots
as posterior fits. On Node v24.20.0 / Windows 10.0.26200 / i7-14700KF:

- The synthetic 128-unit, 28-component, four-event profile panel took a cold
  56.8 ms and five warm runs with median/max 53.5/54.9 ms. Proportional
  8,194-unit extrapolation at that fixed history is 3.42 s / 9.93 MiB estimated
  workspace. Scaling time by the real panel's 55,605 historical rows instead
  gives 5.81 s; neither estimate executes the real municipal fit or includes
  source decoding/browser overhead.
- After those reduced measurements, an additional national-only fit used the
  existing admissible 28-component panel at cutoff 2024-05-24: seven historical
  elections and 33 polls. It passed at 2,000 retained draws per chain in 14.43 s,
  political R-hat 1.00425 / minimum bulk ESS 1,975 / minimum tail ESS 3,492.
  The numerical audit reports 604,736 workspace bytes and retained chains use
  3,840,000 bytes. The snapshot pins the exact prepared input hash. Reading
  municipal metadata only supplied extrapolation dimensions; no future counts,
  full municipal fitting or forecast were used.
- The largest synthetic 128-component/512-poll construction took **59.13 s**
  for 4,092 contrasts, with estimated workspace 23.22 MiB and all actual solve
  residuals below `1e-11`. A short sampler pilot projects about 67.35 s total
  for a 2,000-draw fit; that full posterior was not executed/accepted. Node peak
  RSS was about 207 MiB including the harness/input, not a browser peak measure.

**The maximum envelope has not met the 30-second desktop target.** Improve its
solve latency or explicitly approve measured support limits before production
defaults; reduced FV-02 success does not accept the entire envelope. Full real
geography, production source admissibility, Senate, decoded/retained/aggregate
browser memory, four-times-throttled latency, cancellation and other browser
engines remain FV-03/04/06/08 work. No input limits or numerical gates were
silently weakened to obtain these results.

Verification passed: application check (zero errors/warnings), preparation
typecheck, full unit suite **82 files / 1,120 tests**, including 24 new numerical
tests, and focused numerical reruns after final residual/audit guards. Static
builds and **six worker browser tests at each of `/` and `/elezioni`** passed;
the root build was restored. Existing large-chunk build advisories remain.
FV-01's checker rehashed all 1,433 original inventory files and checked 61
preserved evidence records; its numerical model extraction remains unchanged.
Final checks covered documentation links, benchmark source hashes and diff
whitespace. No production generator, scenario/default, source observation,
legal behavior, research location or original artifact changed. No research
cleanup, commit or publication was performed.
