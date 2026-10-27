# Fast vote-share research model

## Objective and authority

On **27 October 2026**, Luca resumed research and authorized simplifying or
replacing the earlier probability model to obtain practical performance. The
unchanged goals are plausible municipal list-vote distributions, adequate
probability around actual results, useful predictive means, historical and poll
information, and browser scenarios containing historical units, joint lists and
new lists with supplied reference shares. Unsupported retrospective splits may
be forbidden. This remains research: production generation, legal scrutiny and
the existing scenario editor are unchanged.

The [previous drift formulation](vote-generation-drift-model.md) and
[restricted-model report](vote-generation-party-drift-restricted.md) describe
earlier candidates. Their exact grouped multinomial likelihood, Gaussian score
prior and joint national/municipal posterior are **not** claims about this
alternative. The new observation model is a Gaussian approximation to grouped
share information; geography is an empirical-Bayes predictive module.

## Why change the implementation and model

The resumed experiments first retained the previous probability model. Exact
Gaussian marginalization removes the shared national field from the electoral
prior, then reconstructs it conditionally for polls. Independent density and
gradient checks pass. The small real two-city case samples in 0.72 seconds, but
a richer four-city case exceeds a 120-second engineering profiling limit.
Holding innovation scales fixed completes in 115.30 seconds but fails
convergence: maximum R-hat 1.084, minimum ESS 37. Those measurements do not
establish larger-case computability. The timeout is a profiling choice, not reinstatement of
the waived 30-minute Stan gate.

A national-only Dirichlet-multinomial candidate removes municipal latent states
but also exceeds that short profiling limit in the richer test. These failures
and sources remain reproducible; neither is accepted as a posterior.
These profiling runs precede the early-poll loader repair described below;
they characterize the captured inputs, not every possible Stan formulation.

The selected direction reduces inference to **reference-date probabilities**.
The full richer case needs 56 political and four participation coordinates,
rather than thousands of historical latent coordinates. Its native sampler
takes fractions of a second. Large municipal data affect preparation of
geographic features and generation, not the size of the national state vector.

## National reference estimation

Use political component shares among valid voters, $q_k$, and combined
non-valid elector share, $a_k$, with separate Camera/European reference contexts.
The country elector probabilities are $(a_k,(1-a_k)q_k)$.
Separating these parameters does not reinterpret poll nonresponse as turnout.
The pooled-kind alternative equates the reference contexts; the regularized-kind
alternative estimates both, with a proper penalty on their difference.

For a source question with reported political shares $r_g$, offered-set incidence
row $u$ and group incidence rows $S_g$, the estimating equation is

$$
(S_g-r_g u)q_k=0.
$$

Project the answer equations into independent orthonormal contrasts. Retain
each original source group: no inferred component counts become observations.
Historical counts are aggregated exactly before computing observable grouped
proportions. Their original denominators determine sampling variance. Printed
zeros in source counts stay zero; small variance regularization does not change
the observed proportions. The current poll loader retains its explicit
printed-zero restriction until that source path is extended.

The residual covariance includes reported/effective sample uncertainty,
rounding, shared survey-period error and shared historical movement. Movement
at historical ages $t_i,t_j$ shares the Brownian covariance factor
$\min(t_i,t_j)$; survey-period errors are shared within the declared half-year
and election-kind groups. Availability normalization is linearized at fitted
reference shares. Three feasible generalized-least-squares updates set those
covariances using training information only.

Movement scales use original separately observed historical contrasts with
proper pooling toward finite defaults. They use transitions/elections as
information units, not municipalities as independent national elections.
Historical scale bounds prevent a formerly larger party's current small mean
from artificially eliminating uncertainty about its decline. A group bound
sets a covariance convention only; it does not assign that group's votes to
its members. Kind regularization is also scaled on those historical bounds.

Conditional on the fitted covariance policy, the reference posterior is

$$
p(q\mid\text{information},\widehat\theta)
\propto \exp\{-\tfrac12 q^\top Qq+b^\top q\}
\prod_k\mathbf 1\{q_k\ge0,\;\mathbf1^\top q_k=1\}.
$$

This is a proper, bounded, log-concave target. The native C++ coordinate sampler
transfers probability between pairs within a simplex, plus joint directions
across kinds. Each update samples its exact truncated-normal conditional.
It checks four chains using rank-normalized split/folded R-hat, bulk ESS and
tail ESS. Gates are R-hat <= 1.05, bulk ESS >= 400 and tail ESS >= 200;
retained draws extend from 2,000 to 4,000/8,000 per chain if needed.
Diagnostics follow the [authors' rank-normalization and ESS definitions](https://avehtari.github.io/rhat_ess/rhat_ess.html).
Truncated-normal updates use reflected rejection and exponential-tail proposals
as in [Robert's simulation method](https://citeseerx.ist.psu.edu/document?doi=bb1edc1cf04a42dc4f2945e440e37ff531976594&repid=rep1&type=pdf).

This is exact sampling of the **conditional bounded Gaussian target**. It is
not exact inference for the original count model, and does not integrate all
uncertainty in covariance estimation. That limitation requires predictive
backtesting rather than acceptance from sampler convergence alone.

## Forecast uncertainty and predictive means

Future political and participation movement use quarterly Beta-stick steps.
Each stick has conditional mean equal to its reference fraction. Independent
random permutations per predictive draw remove a fixed component ordering.
Consequently

$$E[q_{t+\Delta}\mid q_t]=q_t.$$

This avoids an automatic Jensen shift in expected shares from a zero-centered
logit random walk. It does not guarantee that an estimated reference mean
equals an unobserved future result. Minority uncertainty scales with the share;
small components do not disappear merely because their Beta shape was forced
toward zero. Innovation-scale uncertainty is propagated as a finite confidence
mixture under the declared empirical-Bayes policy.

An additional election-realization discrepancy is calibrated only from earlier
elections and their latest compatible pre-election poll. Both are coarsened to
common observable groups before comparing; no internal shares are fabricated.
Sampling variance, already modeled poll error and elapsed movement are removed
from the contrast discrepancy estimate, which is pooled toward a finite default.
The developmental menu compares that estimate and a conservative 1.5 multiplier.
The discrepancy step also preserves conditional means. It is election
uncertainty, not a new poll-error draw added to generated ballots.

## Municipal predictive module

Observed separate components provide municipal relative-support features;
combined non-valid counts provide participation features. Small units shrink
toward their region. Scalar Gaussian feature filtering accounts for count noise,
elapsed time and missing observations, propagating uncertainty to the cutoff.
Local innovation scales are estimated from within-place temporal changes after
national movement, with sampling-noise correction and pooling. A common scale
and a partially pooled component-scale alternative are compared.

Where no separate geographic observation exists, the most recent observed joint
group supplies an explicitly pooled profile, with additional uncertainty. This
is a prediction convention, not measured component votes or identified local
splits. Future-only municipalities retain regional/national fallback uncertainty;
missing histories never become zero counts.

Generate shared regional and local predictive variation under a split variance
budget. For each country predictive draw, project the municipal probability
table to its country margins using elector weights, respecting structural
zeros. **This conditional probability projection is a model change** from the
earlier prohibition on endpoint reconciliation. It happens before integer count
generation; realized national counts are always sums of municipal counts.
Draw one multinomial per municipality including non-valid voting. Integer,
elector and list aggregation conservation are explicit invariants.

The municipal module is not a fully joint Bayesian historical-state posterior.
Its profile and residual approximations must pass the municipal holdouts.

## Scenario contract and browser boundary

`fast_scenario.py` supplies a headless prototype:

| Input | Behavior |
| --- | --- |
| Historical fitted atom | Reuses its historical/poll reference distribution and pooled estimated volatility/profile |
| Joint list of fitted atoms | Aggregates the same generated component counts; grouping alone changes no component draw |
| New list/unit | Requires a valid-voter reference share; receives no invented historical counts |
| Reference override | Intervenes on latent support at the prepared reference date; it is not a fictitious poll or a newly conditioned historical posterior |
| Uncertainty override | National/local list multipliers default to 1; a new unit may specify its national scale, with fitted common defaults |
| Unknown retrospective child | Rejects; an indivisible historical atom cannot be silently split |

Reference shares use fractions; their SD uses percentage points. The initial
research contract supports any number of disjoint exact references and one
uncertain reference, with explicit rejection of overlapping constraints.
New geography defaults to a neutral national profile, profile SD 0.45 and local
innovation SD 0.25; these may be specified explicitly. Reference uncertainty,
future movement, election discrepancy and integer sampling remain separate.

Removing an available component retains the approved choice-set substitution:
renormalize every remaining outcome, including non-valid support. A date change
requires a matching prepared reference fit. A reference is exact on latent
reference support when its SD is zero; later movement and election-realization
uncertainty still exist unless separately overridden.

The portable `party-share-research-v1` compressed JSON contract contains the
country reference draws, fitted scales, geographic features/uncertainties,
source fingerprints and component identities. It is suitable input for a future
browser worker and editor implementation. **Neither browser fitting/generation
nor editor integration has been implemented or accepted here.** The prototype's
scenario lists are nationally offered; integration with actual geographic
filing restrictions remains required. Camera/European research does not
constitute fitted Senate or paired-chamber validation.

## Numerical and predictive evidence

Independent quadrature passes four fixtures: interior probabilities, a near-zero
component, grouped support with distinguishing information and coupled kinds.
Maximum probability-mean error is 0.151 pp; maximum 90% endpoint error is
0.356 pp. Quadrature refinement changes means by less than $2\times10^{-14}$.
The complete fixture check takes about 0.5 seconds with a cached native kernel.
At 120,000 future draws, the two-year martingale check differs from its input
means by at most 0.064 pp and produces no named-component >80% outcome in that
fixture. Conservation and probability-projection checks pass.

Eleven Python unit tests cover source/held-out separation, new-list references and SD
units, missing-history uncertainty, incompatible references, choice-set
substitution, an independently checked active-set optimizer and exact joint-list
count aggregation. TypeScript tests also check source-view consistency and
explicit rejection of unsupported future residuals.

The first 15-day development screen exposed national undercoverage (67.5% at
nominal 90%) despite approximately 86.8% municipal coverage. Historical variance
bounds and training-only election discrepancy address that problem explicitly;
the failed screen is retained, not presented as a successful forecast model.

The four-variant, 16-case development screen covers Camera 2013/2018 and
European 2014/2019 at 730/365/90/15 days. Selection is frozen in
`data/derived/party-drift/fast-development-frozen-20261027.json` before 512-draw
refinement and confirmation. The selected candidate has separate regularized
kind contexts, a common local innovation scale and the 1.5 discrepancy multiplier.

The first confirmation attempt exposed two implementation faults. The loader
discarded already reviewed FDI/FI answers from 13 early survey views, although
their original PDFs and six-category restoration were already pinned. It now
replaces each coarse view with that same survey's reviewed answers, validates
unchanged core shares/dates/input pins and never duplicates the survey. Four
older views without those answers remain coarse. An active-set quadratic
optimizer also replaces a generic solver that failed at one feasible mode;
independent convex-optimization tests verify the same objective. Neither repair
changes the frozen statistical settings. Both development and confirmation were
replayed after repair. **These are corrected confirmation replays after defects
were seen, not a pristine untouched holdout.** Failed artifacts remain available.

At 512 predictive draws and a systematic 128-unit geographic sample, all 24
reference fits pass the four-chain diagnostics:

| Corrected replay | Cases | National valid-share 90% coverage | Municipal valid-share 90% coverage |
| --- | ---: | ---: | ---: |
| Development, four earlier elections | 16 | 90.1% | 90.2% |
| Confirmation, Camera 2022 and European 2024 | 8 | 86.0% | 91.3% |

Coverage is averaged over observed outcome groups within each forecast and then
over forecasts; municipal units are equally weighted. It is not individual-list
coverage inside pooled outcomes. Confirmation national coverage ranges from
72.7% to 100% across cutoffs. The horizons share elections and information;
they are not eight independent election confirmations.

| Confirmation horizon | National mean MAE | Municipal mean MAE | National 90% width | Municipal 90% width |
| --- | ---: | ---: | ---: | ---: |
| 730 days | 3.95 pp | 5.50 pp | 17.66 pp | 27.14 pp |
| 365 days | 2.82 pp | 4.42 pp | 14.70 pp | 21.98 pp |
| 90 days | 2.72 pp | 4.26 pp | 13.35 pp | 21.16 pp |
| 15 days | 1.67 pp | 3.23 pp | 7.26 pp | 16.10 pp |

These are equal-group valid-share errors, averaged over the two elections.
The combined national/municipal weighted interval score is 0.0190 on fraction
units (development 0.0316); interval width remains a material calibration cost.
Party-specific errors remain material: M5S is overestimated by 5.27 pp in the
2024 15-day forecast; the 2022 pooled research residual is underestimated by
8.52 pp. Summing signed errors across a simplex would mechanically cancel, so
it cannot establish unbiasedness. Conditional mean-preserving dynamics pass
their mathematical fixture, but empirical absence of systematic party bias is
**not established**. No >80% national component outcome appears in the 512-draw
confirmation runs; the maximum development frequency is 1/512. This is a narrow
stress screen, not a guarantee against every implausible municipal outcome.

### Complete compatible municipal benchmark

Before full processing, linear extrapolation from 128 profile units estimated
about **4.98 seconds for fitting** and 20.21 seconds for 512-draw generation over
the 8,194-unit historical/future union. This cleared the 30-minute gate. The
complete European-2024, 15-day case then used all 8,194 profile identities and
**7,821 usable future municipalities**, 28 political atoms, seven national
training elections and cutoff-available polls. The domain excludes Valle d'Aosta
and source rows that cannot be matched; it is not every legal Italian ballot.

Measured on the i7-14700KF machine under WSL Ubuntu, with one BLAS/OMP thread:

| Work | Seconds |
| --- | ---: |
| Country reference fitting | 0.44 |
| Geographic fitting | 4.74 |
| **Total fitting** | **5.18** |
| 512 complete municipal count draws | 10.72 |
| Complete benchmark including scores/posterior writes | 25.82 |

Peak Python resident memory is 1.90 GiB. Municipal 90% valid-share coverage is
92.9%, mean absolute error 2.89 pp; country figures are 90.9% and 1.44 pp.
All municipal draws conserve electors exactly. Probability projection needs at
most nine iterations, with maximum country-margin residual below $10^{-9}$.
There are no zero-valid-vote draws; four municipality/draw cells exceed 95%
single-component support with at least 500 valid votes, across 4,004,352 total
municipality/draw cells. Non-valid support exceeds 90% in 0.26% of cells.
This checks full-geography computability of the accepted panel, not broader
component-catalog calibration. The `all`-catalog preparation fails structural
admissibility: retaining all historical identities leaves future-only residual
support without any cutoff observation. Supply a reference for new support or
retain an explicitly observed residual; do not invent a historical split.

The compressed research export contains 1,024 reference draws and the complete
compatible geographic profiles in **3.67 MiB**. Repeated exports are byte
identical; probability/dimension checks, the selected common local scale and
rejection of modified prepared input pass. Generation and editor integration still require
implementation and parity checks. Only four developmental and two later
confirmation elections exist. Full-list coverage inside pooled research
outcomes, membership review, paired chambers, scrutiny/seat results,
missing-geography handling and a production comparator still gate adoption.
The replay uses pinned 2008–2024 information. It is not an upcoming-election
forecast with polls refreshed to October 2026.

## Reproduction and file map

Final local evidence is under `data/derived/party-drift/` (generated and ignored):

| Artifact | Evidence |
| --- | --- |
| `fast-fixtures-final-20261027/summary.json` | Quadrature, martingale, projection and count gates |
| `fast-development-final-20261027/summary.json`, `cases.json` | Corrected 16-case development replay |
| `fast-confirmation-final-20261027/summary.json`, `cases.json` | Corrected eight-case confirmation replay |
| `modular-041250483558/manifest.json`, `benchmark.json` | Full compatible municipal inputs, diagnostics, timing and scores |
| `fast-research-20261027.json.gz`, `fast-export-check-20261027.json` | Portable research bundle and integrity/replay checks |

Use `party-drift-modular-export-cli.ts` to prepare immutable case inputs from
the already pinned sources. `development all 128` prepares the 16 development
folds with a 128-unit geographic sample; a numeric election ID/horizon prepares
one case. The name retains the national-only intermediate experiment; the
fast runner consumes its `profiles.json`, not the rejected Stan posterior.

| File under `web/scripts/data-preparation/stan/` | Purpose |
| --- | --- |
| `fast_party_model.py` | Covariance estimation, bounded reference target, diagnostics and mean-preserving forecast steps |
| `simplex_gibbs.cpp` | Small dependency-free native sampler; compiled once into an external cache |
| `modular_geography.py` | Empirical profile filtering, geographic probability projection, multinomial counts and scores |
| `fast_scenario.py` | Historical/joint/new-unit scenario prototype and reference/uncertainty overrides |
| `run_fast.py` | One-case fit and forecast |
| `study_fast.py` | Development/refinement/confirmation orchestration with frozen choice |
| `benchmark_fast.py` | Complete fitting, generation, memory and scoring benchmark |
| `check_fast_fixtures.py`, `test_fast_model.py` | Independent mathematical and functional gates |
| `export_fast_bundle.py` | Portable research JSON posterior; no production installation |

The fast candidate requires Python, NumPy, SciPy and a C++20 compiler, not Stan.
Use the isolated environment already provisioned for research; native binaries
and all generated data remain outside tracked source directories. Set
`OPENBLAS_NUM_THREADS=1` and `OMP_NUM_THREADS=1` for these small matrices to avoid
thread-pool overhead. From `web/`, for example:

```powershell
npx tsx --tsconfig tsconfig.json scripts/data-preparation/party-drift-modular-export-cli.ts development all 128
```

Run `check_fast_fixtures.py --cache <external-cache> --output <derived-summary>`
and `python -m unittest test_fast_model.py` from the Python script directory.
`run_fast.py <case-directory> --cache <external-cache>` fits one prepared case.
`study_fast.py <index-file> --cache <external-cache> --output <derived-directory>`
runs development; add `--selected kind-common-conservative --draws 512` for the
frozen refinement/confirmation candidate. `run_fast.py` defaults to the baseline
discrepancy multiplier; use `benchmark_fast.py` for the selected conservative
candidate. Prepare the full case with
`npm run data:forecast:share:prepare -- european-2024 15 10000`, then run
`benchmark_fast.py <case-directory> --cache <external-cache> --draws 512` and
`export_fast_bundle.py <case-directory> --output <derived.json.gz>`.
Generated scores/posteriors stay under
`data/derived/party-drift/`. The source manifest pins inputs and algorithm files;
changed code requires re-exporting inputs. No source archive, production default,
commit or publication is implied by these commands.
