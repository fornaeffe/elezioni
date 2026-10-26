# Party-level drift research: stage-0 stop

Historical experiment. The [restricted-scenario follow-up](vote-generation-party-drift-restricted.md)
records Luca's later identifiability restriction, alternative estimators,
authorized offline Stan fallback and admissible research panels. The results
below remain unchanged evidence about the original fixtures.

**26 October 2026: stop before full-data fitting and stages 1–7.** The reduced
accuracy check rejects a single joint Laplace approximation for inseparable
components. Direct expansion of the exact reference sampler also exceeds Luca's
30-minute fitting budget. This rejects these computational approaches, not the
[approved probabilistic model](vote-generation-drift-model.md).
Production generation, source data and defaults are unchanged.

## Implemented research boundary

All new code is under `web/scripts/data-preparation/` and runs offline in Node.

| File | Responsibility |
| --- | --- |
| `party-drift-contract.ts` | Versioned count-research contract; partition, birth, cutoff, source uniqueness and conservation checks; Other complements; semantic fitting fingerprint |
| `party-drift-likelihood.ts` | Original grouped multinomial log likelihood, gradient and observed Hessian; availability; valid-vote-weighted aggregate polling operator; positive-answer poll measurement covariance; reference root diagnostic |
| `party-drift-prior.ts` | Conditional-scale neutral prior calibration, shared national/local Gaussian reference coordinates and normalized backward/forward innovation construction |
| `party-drift-fit.ts` | Exact-derivative, multiple-start reduced Laplace fit and exact-likelihood elliptical slice reference sampler; split-chain diagnostics |
| `party-drift-quadrature.ts` | Independent deterministic integration of one inseparable-list posterior |
| `party-drift-research-cli.ts` | Prespecified reduced fixtures, saved diagnostics, original-data census and reduced kernel timing; no countrywide fit command |
| `party-drift.test.ts` | Algebra, derivatives, data validation, covariance/window invariance, prior symmetry, reference inversion and diagnostic tests |

This is an initial research contract and accuracy gate, **not the complete new
scenario contract or browser fitter**. The reduced dense solver rejects dimensions
above 192. It is not offered as a countrywide architecture. No old local
log-ratio moment estimator, inferred component counts or fixed-outcome fit is
used to estimate the new model.

The reference root solve tests the aggregate operator; it does not implement
posterior conditioning on a hard constraint. Positive-answer poll covariance is
an observation primitive, not a fitted grouped-poll posterior. Printed zeros
explicitly reject the unimplemented interval-likelihood path. Genuine new-party
initialization and reference constraints explicitly reject use in this gate.
Complete scenario/poll/future-ballot defaults, hard/soft reference posterior
conditioning, joint volatility hyperparameter inference, prior sensitivity,
historical-trajectory posterior checks and browser execution remain open.

## Reduced posterior experiment

Fixtures were specified in code before scoring, independently of held-out
election results. Each municipality has 100 electors. Components A and B are
separate latent identities; a joint ballot observes their sum once. The
two-municipality fixture shares national coordinates rather than independently
splitting each municipality.

At the reference date, national contrast SD is 1.5 and local contrast SD is 0.5,
conditional on those scales. These are finite research conventions, not
party-level fitted or accepted default hyperparameters. The centered Gaussian
construction is exchangeable across political identities. A deterministic
32,768-draw calibration gives prior expected non-valid share 0.5; independent
prior-predictive draws check participation and political symmetry. Reference
date equals observation date here: temporal and hyperparameter inference have
not been validated by this experiment.

Each fixture uses nine optimization starts and four independent exact-likelihood
elliptical slice chains, each with 6,000 burn-in iterations and 12,000 retained
draws, thinning two. Laplace has 48,000 draws. Gates fixed in code are:

- Converged positive-definite mode, maximum gradient below $10^{-6}$.
- Reference split R-hat at most 1.05 and ESS at least 400 for component
  probabilities, the aggregate split and a latent component log ratio.
- Absolute probability-mean difference at most 2.5 percentage points;
  approximate/exact central 90% interval-width ratio between 0.65 and 1.5.

| Fixture | Observed elector counts | Largest mean difference, pp | Largest width ratio | Accuracy |
| --- | --- | ---: | ---: | --- |
| Separate components | 0/A/B/C = 30/35/25/10 | 0.29 | 1.03 | Pass |
| Inseparable balanced list | 0/(A+B)/C = 30/40/30 | 6.27 | 1.81 | Fail |
| Inseparable small list | 30/5/65 | 1.70 | 1.66 | Fail |
| Inseparable dominant list | 15/80/5 | 11.53 | 1.55 | Fail |
| Observed-zero joint list | 30/0/70 | 0.70 | 1.56 | Fail |
| Shared national split, two municipalities | 30/40/30 and 20/50/30 | 8.48 | 2.47 | Fail |

The maximum reference R-hat is 1.0073 and minimum ESS is 684. All 54 starts
reach the gradient tolerance; 53 have positive-definite curvature and one
symmetric saddle is excluded. The issue persists with a well-converged mode. The balanced fixture's
nine starts find the same mode. Its exact posterior mean joint-list elector
share is 39.98%, while the Laplace mean is 52.38%; non-valid means are 30.35%
versus 24.07% (quadrature versus Laplace).

Independent integration checks the balanced fixture in aggregate logits and
the internal split logit, evaluating the **original component Gaussian prior**.
The transformation has a constant Jacobian, so it introduces no alternative
prior or pseudo-observation. Coarse/fine grids use 697,068/5,447,334 evaluations;
mean differences between resolutions are below $3\times10^{-13}$ and boundary
weight below $3\times10^{-13}$. The largest mean difference from sampling is
0.075 percentage points. This supports the sampling reference independently of
the optimizer and sampler implementations.

Joint-list evidence constrains a sum of exponentials. The posterior can have a
curved ridge: changing the internal split requires compensating changes in the
aggregate score. A Gaussian at one mode misses that dependence and assigns
probability to inflated aggregate support. The dominant-list fixture also has
competing modes. More iterations, faster sparse factorization or simply
averaging symmetric modes does not resolve the balanced fixture's error.
Tightening priors until this disappears would change substantive assumptions;
it was not used as an automatic workaround.

## Computability and gross timing estimate

Measured on this machine: Intel Core i7-14700KF, 28 logical processors,
approximately 64 GiB RAM, Windows, Node 24.20.0. Reduced validation, including
quadrature, took **7.29 seconds**. This measures a small conditional posterior,
not a browser run or a scalable fit.

The census reads the eight pinned Camera/European archives: 63,426 usable
municipality/election observations and 8,194 distinct municipal identities,
excluding Valle d'Aosta and inconsistent/unmatchable rows by the existing
importer's policy. There are 146 distinct original source labels; that is
**not** a count of identified stable parties. No new polls were acquired and
no historical membership identities were guessed for fitting.

Timing uses systematic municipal midpoints across each event. Each source list
stays atomic except that the first list is represented by an explicitly
hypothetical two-component membership for the grouped-kernel probe. Its original
count remains intact. One warm-up is followed by six timed repetitions.

| Reduced observations | Median grouped likelihood, ms | Median likelihood + derivatives, ms |
| ---: | ---: | ---: |
| 256 | 0.333 | 1.926 |
| 1,024 | 1.333 | 7.311 |
| 4,096 | 5.220 | 25.253 |

Linear extrapolation gives approximately **0.081 seconds per countrywide
count-likelihood scan**, or **0.391 seconds with derivatives**. One hundred
derivative scans would take about 39 seconds. That number omits latent-state
evaluation, sparse linear algebra, polls, scale inference and posterior
integration and must not be reported as fitting time.

For direct whole-vector reference sampling with the same four-chain protocol:

- Even one likelihood proposal per iteration requires 120,004 scans: about
  **2.7 hours serial for the count kernel alone**.
- Actual reduced runs use 617,967–809,848 evaluations. Keeping that proposal
  count gives approximately **14–18 hours serial** for the count kernel.
- Ideal four-chain parallelism would reduce those estimates to about
  40 minutes and 3.5–4.5 hours respectively, before other costs. This parallel
  execution was not implemented or measured; countrywide mixing can be worse.

Thus direct expansion of this reference sampler exceeds the **30-minute stop
threshold**. No full-data fit was launched. A future sparse or hybrid inference
method has no defensible total fit-time estimate yet; the accuracy failure
prevents proceeding to that benchmark. These estimates do not establish that
every computational realization of the approved model is infeasible.

## Decision needed and proposed next step

Keep the approved membership, count and prior model. Change the inference
strategy before repeating the staged comparisons.

Recommended: investigate aggregate/split coordinates with the original Gaussian
prior and required density transformation, explicitly integrate weakly
identified shared split directions by quadrature or conditional sampling, and
use sparse Gaussian approximations only where reduced checks support them.
Historical memberships change over time, so a single static joint-list split
coordinate system is insufficient: preserve every original likelihood and its
shared temporal/national dependence. First repeat these fixtures, then temporal,
poll, hard/soft reference and joint-scale checks; only then benchmark progressively
larger geographic samples and reapply the 30-minute gate before full fitting.

An alternative is an offline reference fit using a mature sparse automatic-
differentiation/sampling engine. It would require a new measured budget and
would not establish browser fitting acceptance. Sparse Laplace acceleration
alone does not fix the posterior error measured here. Stronger split priors or
collapsing requested component identities would revise the statistical model
and need an explicit decision, rather than an implementation workaround.

No choice between these approaches has been applied. Stages 1–7, chronological
development/confirmation comparisons, prior sensitivity, production integration,
paired chambers and seat/browser acceptance remain unperformed.

## Reproduce and inspect

From `web/`:

```sh
npm run check:data
npm run test -- scripts/data-preparation/party-drift.test.ts
npm run data:forecast:party:validate
npm run data:forecast:party:profile
```

The validation command **intentionally exits nonzero on the measured accuracy
failure**, after saving its report. It is separate from the unit suite, whose
passing tests check implementation identities rather than approving Laplace.
`profile` is an inventory and reduced timing probe; it never fits the full data.

Measured artifacts (ignored derived data, reproducible with the commands):

- `data/derived/drift/party-drift-stage0-8ceb6b4a44a5/validate-2026-10-26T08-40-29-387Z.json`
- `data/derived/drift/party-drift-stage0-8ceb6b4a44a5/profile-2026-10-26T08-40-42-875Z.json`

Artifacts pin code/source hashes, policies, seeds, optimizer and chain
diagnostics, quadrature, sample sizes and timings. Earlier attempt directories
are retained separately. The original fixed-outcome experiment remains in its
[historical report](vote-generation-drift-results.md).

Method references:
[elliptical slice sampling](https://proceedings.mlr.press/v9/murray10a.html),
[sparse Laplace and automatic differentiation](https://arxiv.org/abs/1509.00660).
