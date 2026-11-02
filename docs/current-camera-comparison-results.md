# M01/M02 1.0.0 Camera-only comparison (historical)

This experiment evaluates the then-current M01/M02 1.0.0 probability kernels using the
Camera-only preparation exception Luca approved on 2 November 2026. Production
model contracts, sources, defaults, routing and legal scrutiny are unchanged.
The [previous comparison](vote-generation-legacy-comparison-results.md) is frozen;
none of its older models was rerun.

## Scope and reading the results

Historical accuracy uses the same 2013/2018/2022 Camera targets, 730/365/90/15-day
cutoffs, reduced municipal panels, full 2018/2022 short-horizon domains and
512 draws per case as the previous comparison. M01 uses the latest eligible
Camera baseline without a Senate prerequisite; M02 fits Camera/European history
and uses only its Camera coordinates/profiles. No Senate age-25+ observations
enter either historical calculation. Source availability cutoffs remain binding.

The tests retain the frozen cutoff-specific admissible research component bases,
including `research-other`, conditional target electorates and filing sets.
They are **not** the current 34-component future default. M01's nonlinear shocks
and M02's component pairing depend on this basis; scores are not an invariant
marginalization of a finer default model. Comparisons with older recorded
procedures describe these scenario/input procedures, not isolated causal effects
of changing the transition law.

Municipalities, current physical colleges, constituencies and the evaluated-area
aggregate use the previous observable-group contracts, denominators and equal
weight per group/unit/case. Joint-ticket truth is never split into invented
component counts. VdA and unmatched target-domain exclusions remain those of the
original historical study. Missing fine crosswalks retain municipal/area scores
and are omitted from college/constituency scores.

Error30 is the draw frequency of absolute error above 30 percentage points;
Coverage90 checks the central 90% interval. Mean MAE measures the predictive
mean, while WIS/CRPS assess the distribution. Near-truth frequencies, coverage at
50/80/90/95%, interval widths, 15-point errors, extreme-80% frequencies,
elector-denominator scores and per-group signed bias are retained in the
[compact receipt](../test/fixtures/benchmarks/current_camera_comparison.json)
and `test/fixtures/benchmarks/current_camera_details.json.gz`. Whole-draw Monte
Carlo intervals measure simulation error on these fixed cases, not uncertainty
about generalization to an unseen election.

<!-- BEGIN GENERATED CURRENT CAMERA COMPARISON -->
## Full-domain historical results

15-day cutoff; 512 draws per model. Error30 and coverage are percentages; MAE is in percentage points. Lower Error30/MAE/WIS is better.

| Election | Scope | Model | Error30 % | Coverage90 % | Mean MAE pp | WIS |
| --- | --- | --- | ---: | ---: | ---: | ---: |
| 2018 | municipality | M01 | 0.78 | 49.48 | 5.91 | 0.0383 |
| 2018 | municipality | M02 | 2.36 | 89.17 | 5.50 | 0.0290 |
| 2018 | college | M01 | 0.07 | 44.55 | 5.17 | 0.0336 |
| 2018 | college | M02 | 0.39 | 80.45 | 5.10 | 0.0258 |
| 2018 | constituency | M01 | 0.03 | 48.15 | 5.05 | 0.0327 |
| 2018 | constituency | M02 | 0.22 | 79.63 | 5.02 | 0.0259 |
| 2018 | evaluated area | M01 | 0.00 | 66.67 | 3.12 | 0.0125 |
| 2018 | evaluated area | M02 | 0.00 | 100.00 | 3.39 | 0.0134 |
| 2022 | municipality | M01 | 0.44 | 57.95 | 3.80 | 0.0260 |
| 2022 | municipality | M02 | 1.28 | 83.75 | 3.95 | 0.0204 |
| 2022 | college | M01 | 0.17 | 59.34 | 3.28 | 0.0237 |
| 2022 | college | M02 | 0.33 | 75.58 | 3.39 | 0.0185 |
| 2022 | constituency | M01 | 0.00 | 61.36 | 3.10 | 0.0218 |
| 2022 | constituency | M02 | 0.07 | 75.87 | 3.23 | 0.0178 |
| 2022 | evaluated area | M01 | 0.00 | 75.00 | 2.30 | 0.0151 |
| 2022 | evaluated area | M02 | 0.00 | 66.67 | 2.55 | 0.0162 |

## Reduced panels across all four cutoffs

Equal weight per case. Conditional component bases can differ across cutoffs, as in the previous comparison.

| Election | Model | Municipal Error30 % | Municipal coverage90 % | Municipal MAE pp | College Error30 % | College coverage90 % |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| 2013 | M01 | 1.92 | 61.46 | 7.58 | 1.07 | 60.70 |
| 2013 | M02 | 5.97 | 92.84 | 7.45 | 4.12 | 89.32 |
| 2018 | M01 | 1.72 | 67.58 | 6.16 | 1.13 | 64.87 |
| 2018 | M02 | 3.00 | 85.70 | 6.04 | 2.49 | 82.95 |
| 2022 | M01 | 1.25 | 59.37 | 4.77 | 0.97 | 58.67 |
| 2022 | M02 | 5.52 | 78.45 | 7.25 | 5.38 | 74.49 |

## Whole-draw Monte Carlo uncertainty

Full-domain college Error30: M01 minus M02, in percentage points. Independent-stream normal intervals describe simulation error conditional on these fixed cases.

| Election | Difference pp | Monte Carlo SE pp | Normal 95% interval pp |
| --- | ---: | ---: | --- |
| 2018 | -0.32 | 0.03 | -0.38 to -0.26 |
| 2022 | -0.16 | 0.01 | -0.18 to -0.14 |

## Current future Camera ballot in Chromium

1,000 attempted simulations per model; Chromium 148.0.7778.96; 4 workers. All attempted draws, including errors, count toward wall time.

| Model | Wall min | Max worker load s | Max preparation s | Generation worker-min | Scrutiny worker-min | Exceptions | Non-computable | Complete 392-seat results / returned |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| M01 | 9.77 | 0.36 | 1.50 | 35.58 | 3.08 | 0 | 0 | 1000 / 1000 |
| M02 | 6.45 | 0.40 | 0.65 | 22.50 | 2.98 | 0 | 0 | 1000 / 1000 |

Phase sums accumulate time across workers and are not additive phases of campaign wall time. These Camera-only timings have a smaller legal workload than the old paired 588-seat runs. They are not an isolated speed ranking.

<!-- END GENERATED CURRENT CAMERA COMPARISON -->

## Interpretation

In the full 2018 domain, M01 has fewer extreme generated errors, but its 49.48%
municipal coverage for nominal 90% intervals shows severe undercoverage. M02's
89.17% municipal coverage is much closer to nominal, with lower mean MAE
(5.50 versus 5.91 points) and WIS. Its college coverage is lower, at 80.45%.
In full 2022, M01 has slightly lower municipal/college mean MAE (3.80/3.28
versus M02's 3.95/3.39 points), but much lower coverage (57.95%/59.34% versus
83.75%/75.58%) and worse WIS. Numerical convergence does not establish
predictive calibration, and neither tail errors nor mean errors alone summarize
these differences.

The [frozen older selected candidate](vote-generation-legacy-comparison-results.md#full-domain-historical-results)
has lower municipal Error30 and mean MAE than current M02 in both full domains:
2018 Error30 2.03% versus 2.36%, MAE 5.02 versus 5.50 points; 2022 Error30
1.11% versus 1.28%, MAE 3.35 versus 3.95 points. Those old numbers are reused,
not rerun. Original offered-set treatment, reference construction and forecast
semantics differ, so these point comparisons do not isolate the effect of one
law change. They also do not support claiming that the corrections improved
historical predictive accuracy on these panels.

## Input preparation and limitations

- Original national offered-set strata are reconstructed from pinned municipal
  counts before M02 fitting. Their elector and non-valid totals must match the
  frozen aggregate export exactly. M02 profiles use the corrected original-offered
  policy. Poll groups keep their dated offered complements; neither historical
  joint groups nor M01's compositions become M02 component observations.
- M01 applies the reviewed original-list joint weights from
  `data/legacy-comparison-policy.json`, and that policy's explicitly declared
  equal-weight fallback for other joint/pooled groups. These are assumed
  compositions, including inside dated coarsened poll groups. This does not
  empirically identify the members of an unnamed residual.
- Filed M01 components absent from its baseline get explicit Camera references
  from cutoff-eligible named polls, using the declared joint composition where
  needed. These fixed means override the corresponding poll measurement draws,
  as scenario references do in M01. In 2022, the Impegno Civico residual's zero
  anchor comes from the already reviewed Centro Democratico 100% composition;
  it is an explicit scenario assumption, not a missing observation treated as zero.
  Full compositions, reference sources and missing-history audits are retained
  under `data/derived/current-camera-comparison/*-inputs.json`.
- M01 national/regional borrowing uses the full eligible original Camera domain.
  M02 reduced profiles use the original reduced research panel; full profiles use
  the full domain. The procedures therefore share scoring, not every input or
  source-construction assumption.
- Electorates are apportioned to current physical census parts before count
  generation. Municipal profile/shock streams are shared across splits. Filing
  conditioning happens after unrestricted reference calibration and future
  movement; no national margins or historical turnout are restored afterward.
  Unmatched fine geography has an explicitly labelled municipal-only cell.
- Independent model stream namespaces are used. Draw IDs are fixed; no failures
  are replaced. The browser workload uses the current 24-list default with only
  Camera renewed and Senate-specific references removed. Its required total is
  392 seats; it does not validate paired 588-seat behavior. Current prefitted state
  is loaded, so browser preparation is not a new fitting benchmark.

The source-pinned receipts retain failures, code hashes, input identities,
diagnostics and raw-report hashes. Detailed evidence is under
`data/derived/current-camera-comparison/`. These are research results, not a
production-default selection, public deployment or general calibration claim.

## Reproduce and verify

Checkout `8dcb830` to run the original commands and source/code bindings below.
Current M01/M02 2.0.0 restore different laws; the historical entry points reject
them. Kernel snapshots remain in `web/scripts/acceptance/archive/models-v1/` for
the adapter regression tests. This report and its receipts remain frozen.


Run from `web/`, with the pinned source archives, frozen historical profile
folders and current generated release present. Missing required data fails.

```powershell
npm test -- scripts/acceptance/current-camera-models.test.ts
npm run check:data
node --max-old-space-size=8192 --import tsx scripts/acceptance/current-camera-comparison.ts pilot
node --max-old-space-size=8192 --import tsx scripts/acceptance/current-camera-comparison.ts reduced
node --max-old-space-size=8192 --import tsx scripts/acceptance/current-camera-comparison.ts full
node --import tsx scripts/acceptance/current-camera-future.ts
node --import tsx scripts/acceptance/current-camera-report.ts
```

`CAMERA_DRAWS`, `CAMERA_TARGET`, `CAMERA_HORIZON`, `CAMERA_MODELS` and
`CAMERA_REPORT` isolate historical pilots. `CAMERA_BROWSER_DRAWS`,
`CAMERA_BROWSER_WORKERS` and `CAMERA_BROWSER_MODELS` select browser pilots.
Curated evidence requires all 28 historical rows at 512 draws and both browser
models at 1,000 attempts. Pilots never replace those workloads. No command above
executes an older model.

Adapter checks cover the held-out-count boundary, original offered-set strata,
unrestricted reference calibration, deterministic draws, electorate conservation
and final filing zeros. All seven checks and preparation TypeScript checks passed.
Curated evidence verification requires exact case sets, source/code bindings,
accepted fit diagnostics and conserved original counts. No legal algorithm or
production component changed.
