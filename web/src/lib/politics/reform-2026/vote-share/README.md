# Fast-share engine

This folder implements the bounded reference fit, empirical profile estimator,
forecast adapter and opt-in reform candidate editor/worker integration. The
installed legacy generator remains the ordinary application default while
FV-08 acceptance is open.
Original compact data is implemented in [FV-03](../../../../../../docs/vote-generation-fast-data.md).
For forecast/intervention/physical-cell/Senate work read only the
[FV-04 generation guide](../../../../../../docs/vote-generation-fast-generation.md).
For fitting worker/client scheduling, revisions and buffer ownership, read the
[FV-06 fitting guide](../../../../../../docs/vote-generation-fast-fitting.md).
For the editor and application integration, use the FV-07/08 migration guides.
For default prefitting, immutable state, candidate loading and release commands,
read the [FV-05 release guide](../../../../../../docs/vote-generation-fast-release.md).

| Task | Entry point |
| --- | --- |
| Model equations and fixed conventions | [Maintained contract](../../../../../../docs/vote-generation-fast-model.md) |
| Filing-aware replacement candidates and geographic covariance | [Feasible model](../../../../../../docs/vote-generation-feasible-model.md), [research and results](../../../../../../docs/vote-generation-feasible-research.md) |
| Prepared numerical inputs/typed outputs | [types.ts](types.ts), [scenario/data boundary](../../../../../../docs/vote-generation-fast-contracts.md) |
| Lossless state serialization and compatible candidate loading | [state-codec.ts](state-codec.ts), [release.ts](release.ts) |
| Browser fit lifecycle, worker protocol and resource reservations | [fit-client.ts](fit-client.ts), [fit-types.ts](fit-types.ts), [fit-resources.ts](fit-resources.ts) |
| Country scales, grouped covariance, three GLS updates and participation | [information.ts](information.ts) |
| Constrained mode and four-chain bounded Gaussian sampling | [gaussian.ts](gaussian.ts) |
| Entity-addressed RNG and truncated tails | [random.ts](random.ts) |
| Rank/split/folded R-hat, FFT bulk/tail ESS | [diagnostics.ts](diagnostics.ts) |
| Regional shrinkage, sparse histories and profile filtering | [profiles.ts](profiles.ts) |
| Independent reduced tests | [numerics.test.ts](numerics.test.ts), [frozen protocol](../../../../../../docs/vote-generation-fast-validation.md) |
| Recorded numerical/browser/scaling evidence | [Benchmark snapshot](../../../../../../test/fixtures/benchmarks/vote_share_numerics.json) and migration history |

## Calling and ownership

Use the exports from [index.ts](index.ts) in either Node or a browser worker:

```ts
const state = fitReference(preparedNationalSource, fitSeed);
const profiles = fitProfiles(preparedProfileSource);
// Lower-level covariance/mode checks may use buildPoliticalTarget,
// buildParticipationTarget, gaussianMode and sampleGaussian.
```

Inputs retain original disjoint grouped counts and effective poll samples.
Callers must supply cutoff-eligible, source-reviewed, **admissible** components.
The numerical kernel does not adjudicate party identity or historical splits:
diagnostics passing is not permission to use an unidentified scenario. Source
availability beyond the numerical date/end/publication checks and complete
source-membership editing remain preparation responsibilities.

The country wrapper currently fits Camera/European contexts. Senate is not a
renamed European context. The profile function takes target IDs, regions and
electorates separately from historical observations and never needs future vote
counts. Historical-only identities can influence regional initialization; outputs
contain the requested targets, including explicit missing-history fallbacks.
Observed zeros remain source zeros.

Arrays are owned Float64 buffers in declared coordinate order, plus explicit
fallback masks. Callers own returned arrays; inputs are not mutated. Fits return
joint-context political and non-valid chains separately. Failed diagnostics,
nonpositive factors, unavailable workspace and unconverged solves throw instead
of returning an accepted replacement state. Browser cancellation/revision/state
installation belongs to FV-06, not this synchronous kernel.

## Numerical boundaries

- Constrained active-set modes retain true zeros. Only sampler initialization
  mixes the mode with independent interior starting states.
- Four independent chains use 1,000 burn sweeps and thinning 2; accepted fitting
  tries 2,000, 4,000 then 8,000 retained draws per chain. R-hat <= 1.05, bulk ESS
  >= 400 and tail ESS >= 200 remain separate from posterior accuracy gates.
- RNG state wraps at 32 bits, with strictly interior 52-bit uniforms. Polar normal
  generation avoids native trigonometric rounding differences; FFT half-angle
  roots use arithmetic/sqrt for reproducible diagnostics. Independent quadrature
  and SciPy diagnostic/tail fixtures check the distributions.
- Covariance includes Brownian minimum-age dependence, common survey groups and
  original sampling/rounding blocks. The only statistical arithmetic floor is
  the specified `1e-16` diagonal. No extra ridge or deleted correlations repair
  a failing fit.
- Small/dense cases use equilibrated Cholesky solves. Larger cases apply the
  same covariance matrix without storing its square, using Brownian prefix/suffix
  sums and grouped incidence factors. Block-preconditioned CG checks the actual
  relative residual <= `1e-11`; callers cannot weaken that gate.
- Incidence multiplication costs O(K + G²) per record rather than O(KG).
  Mode/participation targets stay small. No explicit covariance inverse is formed.
- The initial implementation bounds 128 components, 512 polls, 64 historical
  national/profile events and a 256 MiB conservative numerical-workspace estimate.
  Dense selection also respects the requested workspace budget. These checks do
  not measure total browser process/decoded-input/result memory.
- Profile construction uses two city passes and sparse row references rather
  than retaining a full feature list for every city/component/history cell.
  Component filtering scales and common future scales are returned separately.
  `electionDiscrepancy` is the fitted raw scale; the selected 1.5 forecast
  multiplier belongs to FV-04.

## Reduced acceptance and reproduction

From `web/`:

```powershell
npx vitest run src/lib/politics/reform-2026/vote-share/numerics.test.ts
npm run data:reform:shares:numerics
npx playwright test tests/e2e/fast-share-numerics.spec.ts --workers=1
```

The numerical command first checks four independently integrated targets under
the three frozen seeds, then profiles a synthetic 128-unit panel and probes the
28/64/128-component by 0/128/512-poll grid. It writes reports only under ignored
`data/derived/fv02-numerics/`. The short sampler scaling pilots are **not**
accepted posterior fits. Expect the stress grid to take substantially longer
than the small unit suite. The test worker is built by the harness; it creates
no user route or production asset.

Normal validation needs no Python/C++/Stan. Optional independent oracle
regeneration uses
`web/tests/fixtures/fast-share-oracle.py`, NumPy 2.3.4 and SciPy 1.16.3 in an
isolated environment, with `PYTHONDONTWRITEBYTECODE=1`. Pass a JSON serialization
of `reducedShareInput()` and the generated-reference output path. That script
uses the separate Python reference for construction/profiles/diagnostics and
independent adaptive integration plus CDF root finding for posterior means and
5%/95% endpoints. The checked reference JSON records input and source hashes;
no Python dependency enters production preparation or the browser.

Numerical Monte Carlo comparisons use the frozen limits and 200 chain-stratified
block resamples (50 retained draws per block); the 1,000 predictive bootstrap
budget in FV-08 is separate. Byte comparisons use identical inputs, solver and
seed; dense versus matrix-free solves may differ by harmless arithmetic roundoff.
Node and Chromium parity is tested at both root and project base. Other browser
engines and production-sized memory/retention remain FV-08 checks.

## Remaining resource gate

The reduced numerical/browser gate passes; **the entire maximum input envelope
has not passed production latency acceptance**. The 128-component/512-poll
synthetic case exceeds the initial 30-second desktop target. Before enabling
that envelope, improve covariance solving or explicitly approve measured support
limits under FV-03/06/08. Do not silently truncate polls or weaken accuracy.
Extrapolated full-country profile time/workspace is a planning estimate, not an
executed full-country fit or proof that every production source/list is supported.
The CLI's 8,194-unit estimate holds the synthetic four-event history fixed;
the benchmark snapshot separately records the real panel's history-cell count.
