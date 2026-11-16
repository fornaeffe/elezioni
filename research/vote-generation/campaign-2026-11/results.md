# Chamber-aware model campaign — 16 November 2026

**M02 can be improved substantially without adding parameters.** Its posterior
row coupling does not produce the intended political dependence. Full common
coupling already greatly reduces the problem within the existing kernel; a
single common political field provides a further simplification and saves work.
The evidence supports developing that construction, with explicit assumptions
about future chamber contrasts. It does not yet justify a production-model switch.

On the requested marginal criteria, **M01 remains the strongest baseline for
avoiding large errors and approaching nominal coverage**. M05/M06 improve
territorial MAE/WIS but generate more errors exceeding 30 percentage points.
M07 offers no consistent improvement. M01 also overgenerates chamber differences,
so switching to unchanged M01 would leave the reported joint problem unresolved.

| Need | Read |
| --- | --- |
| Every model, fold and geographic level; timing/RSS tables | [Measured tables](tables.md) |
| Exact numbers, source/code identities, diagnostics and report hashes | [Curated receipt](results.json) |
| Data boundary, metrics, folds and commands | [Protocol](protocol.md) |
| M02 simplification and M05/M06/M07 equations | [Candidate definitions](models.md) |

## Scope of the completed comparison

Executed **40 cases and 11,776 joint/single-chamber draws**, with no invalid,
negative, noninteger or elector-nonconserving output rows. The four primary folds
are Camera 2018/2022 at 15/365-day horizons with mixed Camera/European history,
256 draws each. Two Camera-only-history sensitivities use 256 draws; the paired
2022 comparison uses 512; reconstruction ablations use 256. Six additional
Camera score rows are explicitly inherited from M02 for M02-shared: their seeded
Camera outputs are identical, not independently rerun benchmarks.

The matched municipal panel comprises five named ticket families—FdI, FI/PdL,
Lega, M5S, PD—and changing Other. The paired domain contains **7,543 municipalities**
and 99.97% of target valid votes within the eligible domain. Valle d'Aosta and
Trentino-Alto Adige are excluded consistently because of their special ballots.
Both chambers are evaluated on the same reconstructed college/constituency
footprints, separately for Camera and Senate geography. National means this
matched domestic domain. These six-family aggregate results do not supersede
earlier full-list or municipality-level studies with different estimands.

## Diagnosis of the dependence fault

Current M02 selects separate empirical posterior rows using correlated uniforms.
MCMC row order is not a multivariate vote-share quantile ordering. On the
**actual released 30-component fitted state**, 8,192 paired selections at latent
rho=0.85 have index correlation about 0.84, but FdI posterior-value correlation
is essentially zero. At rho=0.98 the posterior-value correlation is still
essentially zero. Sorting whole rows by FdI support preserves the empirical
joint posterior and changes FdI coupling to about 0.85/0.98 respectively.
This dependence on arbitrary row order establishes a construction problem;
raising the existing correlation slightly cannot repair it.

The actual released-scenario diagnostic also finds a mean largest **component**
gap of 7.48 pp in the reference field and 8.12 pp after future movement. A
zero-calendar-horizon run still gives 7.65 pp. These component diagnostics are
separate from the six-family historical scores: they show that excessive
separation is already present before ordinary future movement, rather than
being explained only by a long forecast horizon. They are not full-list accuracy
scores or estimates of a legal bonus error rate.

Reference coupling is not the only issue. Separate chamber profile/movement
paths leave residual variation after synchronizing posterior rows. The
reconstruction ablations below isolate that practical consequence.

## Marginal forecast comparison

Equal averages over the four primary folds, on Camera college footprints:

| Model | Error30 % | Coverage90 % | MAE pp | WIS pp |
| --- | ---: | ---: | ---: | ---: |
| M01 2.0.0 | 0.71 | 88.98 | 4.80 | 2.52 |
| M02 2.0.0 | 0.96 | 92.53 | 4.82 | 2.40 |
| M02-shared | 0.96 | 92.53 | 4.82 | 2.40 |
| M05 — Joint Gaussian | 1.78 | 96.88 | 4.40 | 2.27 |
| M06 — Joint Student-t | 1.73 | 96.01 | 4.43 | 2.25 |
| M07 — Spatial transition bootstrap | 2.00 | 94.36 | 4.78 | 2.39 |

The constituency ranking is similar. Nationally M01 has observed Error30 0.00%,
Coverage90 87.50%, MAE 3.71 pp and WIS 1.71 pp; M02 has 0.07%, 91.67%,
4.12 pp and 1.99 pp. M05/M06 lower national MAE to about 3.4 pp, with Error30
0.65–0.73%. A displayed zero is an observed Monte Carlo result, not a zero-risk law.

Horizon matters: at 15 days M05/M06 often improve several scores, but the
one-year 2018 college Error30 is 3.12% for M05 versus 0.62% for M02. Favoring
WIS alone would overlook this tradeoff. M06's small advantage in averaged WIS
does not establish a decisive advantage over Gaussian. M07 has the same
national endpoint budget as M05; spatial resampling does not fix national bias.
The Camera-only results are retained separately and are not extra independent
election replications. Regime comparisons also change the latest baseline and
matching domain, so they are comparisons of complete procedures, not a causal
isolation of European-history effects.

## Proposed chamber criterion and results

Use **WIS and Coverage90 of the signed Senate-minus-Camera share difference** as
the primary joint criterion. Add the predictive/observed RMS difference ratio,
the observed RMS percentile among joint draws, and largest absolute gaps as
diagnostics. WIS penalizes both excessive width and collapsed uncertainty;
matching the overall difference magnitude alone can conceal wrong local signs.
The [protocol](protocol.md#scores-and-comparison-order) defines the equations.

Only 2022 supplies a bundled common-18+ pair. The out-of-time comparison
therefore uses a deliberately **age-confounded 2018 contrast sensitivity**:
Senate was 25+, Camera 18+. No 2022 Senate outcome trains this comparison.

| Model | National RMS ratio | National Δ WIS pp | National Δ Coverage90 % |
| --- | ---: | ---: | ---: |
| M01 | 13.60 | 0.32 | 100.00 |
| M02 | 62.34 | 1.49 | 100.00 |
| M02-shared | 3.41 | 0.22 | 50.00 |
| M05 | 3.41 | 0.25 | 33.33 |
| M06 | 3.40 | 0.25 | 33.33 |
| M07 | 3.52 | 0.25 | 33.33 |

The observed national RMS difference is **0.116 pp**. Current M02 generates
7.244 pp on average. At college level its ratio is 9.99; at constituency level
11.46. Shared M02 reduces these territorial ratios to 0.88/0.99, but their
signed-difference coverage is only 30.56%/32.05%. The inherited 2018 contrast
does not transfer reliably. Age eligibility changed, so this failure cannot be
attributed entirely to the stochastic model or treated as evidence about future
same-age contrast drift.

The following **in-sample 2022-contrast reconstruction** tests whether the model
can represent the observed relationship when the correct contrast is supplied.
It is not a held-out prediction or independent model-selection success.

| Construction | National RMS ratio | College Δ Coverage90 % | Constituency Δ Coverage90 % |
| --- | ---: | ---: | ---: |
| Current M02 | 62.09 | 100.00 | 100.00 |
| Same posterior row, other coupling unchanged | 18.75 | 100.00 | 100.00 |
| M02 with full coupling, rho=1 | 1.59 | 95.49 | 95.51 |
| One common M02 field | 1.00 | 92.01 | 90.38 |

Thus **a simpler M02 is feasible**. Full coupling uses a parameter value accepted
by the existing kernel and retains its marginal laws. The common-field candidate
removes the duplicate Senate latent path, preserves Camera exactly, then applies
the paired contrast and an independent Senate count draw. It introduces no new
fitted parameter. Its new Senate construction changes the model law and would
need a new MAJOR version. A fully shared profile is not currently a bundled UI
choice; this campaign does not install one.

## Computational requirements

On Windows, Node 24.20.0, i7-14700KF, 28 logical processors and 63.8 GiB RAM,
the isolated 512-draw paired cases take **207 ms/draw for M01, 837 for M02,
734 for shared M02, 609 for M05, 616 for M06 and 611 for M07**. Shared M02 saves
about 12% versus current M02 in this implementation. Fitting/preparation takes
0.49–1.41 seconds on the six-family panel. M01's integer apportionment and the
other models' multinomial count layers are part of these actual measured laws.

Peak process RSS is 513–609 MiB, including source loading, fitting, allocation
and scoring buffers; this is not isolated model workspace. The shared construction
reduces duplicated latent work, while whole-process peaks remain broadly similar.
No large memory advantage for any candidate is established by this one timing case.
The [resource table](tables.md#isolated-resources--512-paired-draws) includes all
values and fit-plus-1,000-generation extrapolations. These are Node generation
measurements on six families, excluding scrutiny, browser overhead, worker
replication and full-list scenario complexity. They do not certify the browser
60-minute requirement. Parallel accuracy-case timings are labelled and excluded
from the speed ranking.

## Decision and remaining acceptance

Keep the full-coupling configuration as the smallest correction to investigate,
and the common-field M02 as the cleaner model-law candidate. Retain M01 as the
main marginal/tail-risk and speed benchmark; retain M05/M06 as calibration and
point-accuracy alternatives. There is no evidence here to prioritize M07.

Before adoption, validate the chosen coupling on the full component/list panel,
different physical filings, joint groups, new components and explicit chamber
references. Preserve intentionally different chamber references. State a future
contrast assumption and assess its sensitivity: one same-age election cannot
identify temporal contrast innovations or certify future joint-bonus probabilities.
Then perform the browser/worker/scrutiny resource gate. Production model laws,
defaults and protected model drafts remain unchanged.

Verification passed: research TypeScript check; five native numerical/parity/
leakage tests; production preparation TypeScript check; the active-to-research
import-boundary test; and the existing archive checks (212 archived files,
61 retained evidence files). Full 512-draw Camera score parity between M02 and
M02-shared is also checked during receipt curation. Frozen historical evidence
was preserved. No browser, full-list or legal-result acceptance is claimed.
