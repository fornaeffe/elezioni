# Historical drift-model comparison — 24 October 2026

The recommended research baseline is the **combined non-valid outcome +
aggregate polling link + regularized Camera/European effects + common national
and local Brownian volatilities**. It improves political-share coverage and
mean accuracy against the current generator in this experiment. It does not
yet justify changing production defaults: participation intervals are very
broad, extreme tails remain, and full-list/chamber/seat calibration is open.

The regional variant is a serious alternative: its coverage is better and its
mean errors are slightly smaller, but its pooled extreme-draw frequency is
higher. The 128-draw screening decision against it is not a robust estimate of
its worst-case tail: 512-draw refinement reverses that particular worst-case
ordering. Keep this trade-off visible rather than treating the selected
baseline as an established unique winner.

Use the [testing protocol](vote-generation-drift-testing.md) for priors,
estimation approximations, source handling, promotion rules and reproduction.
The [mathematical specification](vote-generation-drift-model.md) remains the
model contract. No production generator, scenario editor or scrutiny rule is
changed by this research.

## Results

Screening `latent-drift-v1-367ca6e3b6a9`; 512-draw finalists `latent-drift-v1-53927eca1222`.
Selection frozen at `2026-10-24T14:08:13.400Z` before confirmation scoring.

Finalist tables use 512 countrywide draws per case; screening and sensitivities are explicitly labelled as 128-draw comparisons. Coverage is the proportion covered by central 90% predictive intervals; width, WIS and MAE are percentage points. Municipal summaries give equal weight to municipalities. Rates are descriptive: the independent events are four development elections and two confirmation elections, not the municipalities or four horizons.

### Staged development decisions

Unmentioned terms use the aggregate link, kind effects, common scales and independent local Brownian drift. Exact CLI variant IDs are in the protocol and case artifacts.

| Stage | Variant | Coverage shortfall | WIS | MAE | Worst extreme rate % | Decision |
| --- | --- | --- | --- | --- | --- | --- |
| 1-poll-link | Literal / pooled / common | 5.88 | 5.51 | 11.34 | 1.56 |  |
| 1-poll-link | Aggregate / pooled / common | 3.40 | 5.11 | 11.00 | 2.34 | selected |
| 2-election-kind | Aggregate / pooled / common | 3.40 | 5.11 | 11.00 | 2.34 |  |
| 2-election-kind | Aggregate / kind / common | 2.43 | 4.18 | 7.95 | 2.34 | selected |
| 3-national-scale | Aggregate / kind / common | 2.43 | 4.18 | 7.95 | 2.34 | selected |
| 3-national-scale | + outcome-specific national scale | 4.22 | 3.90 | 7.19 | 1.56 |  |
| 4-local-scale | Aggregate / kind / common | 2.43 | 4.18 | 7.95 | 2.34 | selected |
| 4-local-scale | + outcome-specific local scale | 2.24 | 4.14 | 7.88 | 2.34 |  |
| 4-local-scale | + municipality-specific local scale | 2.74 | 4.19 | 8.07 | 2.34 |  |
| 6-national-recheck | Aggregate / kind / common | 2.43 | 4.18 | 7.95 | 2.34 | selected |
| 6-national-recheck | + outcome-specific national scale | 4.22 | 3.90 | 7.19 | 1.56 |  |
| 7-regional-and-reversion | Aggregate / kind / common | 2.43 | 4.18 | 7.95 | 2.34 | selected |
| 7-regional-and-reversion | + regional innovations | 0.69 | 4.19 | 7.20 | 3.91 |  |
| 7-regional-and-reversion | + local mean reversion | 2.36 | 4.17 | 8.01 | 2.34 |  |

Stage 5 was not triggered because the two local effects did not each improve on the common baseline. Stages 3 and 6 are the same cached comparison when the local structure remains common. Screening used 128 draws; final tables refine Monte Carlo estimates without changing the frozen choice.

### Development finalists

National political shares among valid votes.

| Model | Coverage % | 90% width | WIS | MAE |
| --- | --- | --- | --- | --- |
| Current | 51.56 | 17.74 | 6.10 | 9.91 |
| Literal / pooled / common | 75.00 | 27.41 | 4.51 | 8.38 |
| Aggregate + kind / common (selected) | 82.81 | 24.42 | 3.34 | 6.82 |
| + regional innovations | 87.50 | 26.74 | 3.34 | 6.69 |
| + local mean reversion | 82.81 | 24.66 | 3.33 | 6.82 |


Municipal political shares among valid votes.

| Model | Coverage % | 90% width | WIS | MAE |
| --- | --- | --- | --- | --- |
| Current | 67.34 | 26.85 | 6.23 | 10.55 |
| Literal / pooled / common | 85.05 | 39.99 | 4.93 | 9.03 |
| Aggregate + kind / common (selected) | 90.59 | 39.45 | 4.21 | 7.80 |
| + regional innovations | 90.87 | 40.44 | 4.23 | 7.75 |
| + local mean reversion | 91.22 | 38.19 | 4.08 | 7.74 |


National non-valid share among electors.

| Model | Coverage % | 90% width | WIS | MAE |
| --- | --- | --- | --- | --- |
| Current | 100.00 | 27.41 | 2.24 | 4.24 |
| Literal / pooled / common | 100.00 | 49.69 | 5.56 | 13.41 |
| Aggregate + kind / common (selected) | 100.00 | 53.06 | 3.98 | 7.30 |
| + regional innovations | 100.00 | 57.31 | 4.06 | 6.02 |
| + local mean reversion | 100.00 | 53.84 | 4.04 | 7.39 |


Regional political shares among valid votes.

| Model | Coverage % | 90% width | WIS | MAE |
| --- | --- | --- | --- | --- |
| Current | 61.68 | 22.35 | 6.22 | 10.26 |
| Literal / pooled / common | 68.17 | 27.41 | 4.97 | 8.89 |
| Aggregate + kind / common (selected) | 73.36 | 24.97 | 4.07 | 7.60 |
| + regional innovations | 83.14 | 33.09 | 4.02 | 7.49 |
| + local mean reversion | 73.36 | 25.06 | 4.04 | 7.63 |


### Confirmation: Camera 2022 and European 2024

National political shares among valid votes.

| Model | Coverage % | 90% width | WIS | MAE |
| --- | --- | --- | --- | --- |
| Current | 37.50 | 18.03 | 6.43 | 10.46 |
| Literal / pooled / common | 90.62 | 25.68 | 2.78 | 6.33 |
| Aggregate + kind / common (selected) | 84.38 | 22.21 | 2.52 | 5.38 |
| + regional innovations | 90.62 | 24.23 | 2.48 | 5.23 |
| + local mean reversion | 84.38 | 22.24 | 2.51 | 5.31 |


Municipal political shares among valid votes.

| Model | Coverage % | 90% width | WIS | MAE |
| --- | --- | --- | --- | --- |
| Current | 66.84 | 28.51 | 6.18 | 11.94 |
| Literal / pooled / common | 95.98 | 38.55 | 3.58 | 7.69 |
| Aggregate + kind / common (selected) | 96.75 | 36.70 | 3.42 | 7.31 |
| + regional innovations | 97.24 | 37.58 | 3.40 | 7.16 |
| + local mean reversion | 95.14 | 34.18 | 3.39 | 7.42 |


National non-valid share among electors.

| Model | Coverage % | 90% width | WIS | MAE |
| --- | --- | --- | --- | --- |
| Current | 100.00 | 27.82 | 3.54 | 9.65 |
| Literal / pooled / common | 100.00 | 54.70 | 4.76 | 8.50 |
| Aggregate + kind / common (selected) | 100.00 | 49.68 | 3.69 | 5.80 |
| + regional innovations | 100.00 | 53.76 | 4.04 | 6.30 |
| + local mean reversion | 100.00 | 50.30 | 3.71 | 5.60 |


Regional political shares among valid votes.

| Model | Coverage % | 90% width | WIS | MAE |
| --- | --- | --- | --- | --- |
| Current | 62.01 | 24.28 | 5.58 | 10.56 |
| Literal / pooled / common | 86.18 | 26.36 | 3.07 | 6.55 |
| Aggregate + kind / common (selected) | 80.10 | 23.20 | 2.83 | 5.83 |
| + regional innovations | 97.37 | 31.48 | 2.82 | 5.81 |
| + local mean reversion | 80.59 | 22.94 | 2.74 | 5.65 |


### Confirmation by horizon

| Days | Model | National coverage % | National width | National MAE | Municipal coverage % | Municipal MAE | Non-valid width | Non-valid MAE |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 730 | Current | 50.00 | 29.03 | 12.34 | 74.25 | 13.72 | 33.08 | 11.15 |
| 730 | Selected | 87.50 | 35.39 | 8.95 | 97.53 | 10.86 | 57.50 | 8.88 |
| 365 | Current | 37.50 | 22.01 | 12.85 | 57.59 | 14.30 | 28.25 | 9.23 |
| 365 | Selected | 87.50 | 27.89 | 5.20 | 97.59 | 7.12 | 48.53 | 5.14 |
| 90 | Current | 25.00 | 13.43 | 12.35 | 50.22 | 13.73 | 25.04 | 9.23 |
| 90 | Selected | 87.50 | 17.58 | 4.27 | 96.60 | 6.23 | 47.36 | 3.95 |
| 15 | Current | 37.50 | 7.63 | 4.32 | 85.29 | 5.99 | 24.91 | 9.00 |
| 15 | Selected | 75.00 | 7.98 | 3.09 | 95.25 | 5.01 | 45.36 | 5.23 |


### Fifteen-day national forecasts from the selected model

| Election | Outcome / denominator | Observed % | Mean % | 90% lower % | 90% upper % | Covered |
| --- | --- | --- | --- | --- | --- | --- |
| camera-2022 | PD | 19.07 | 21.72 | 17.36 | 26.27 | True |
| camera-2022 | M5S | 15.43 | 13.34 | 10.69 | 16.20 | True |
| camera-2022 | Lega | 8.77 | 13.06 | 10.26 | 16.27 | False |
| camera-2022 | Other | 56.72 | 51.89 | 46.51 | 57.81 | True |
| camera-2022 | Non-valid / electors | 38.95 | 31.60 | 10.16 | 56.97 | True |
| european-2024 | PD | 24.09 | 21.83 | 17.44 | 26.47 | True |
| european-2024 | M5S | 10.00 | 15.42 | 12.05 | 18.84 | False |
| european-2024 | Lega | 9.00 | 8.80 | 6.72 | 11.12 | True |
| european-2024 | Other | 56.91 | 53.94 | 47.99 | 59.86 | True |
| european-2024 | Non-valid / electors | 52.96 | 49.84 | 26.89 | 70.78 | True |


### Numerical and plausibility audit

The complete run contains **299 forecast cases and 84,352 complete country
draws**: eight drift variants on development folds, current-generator
benchmarks, 512-draw finalists and restricted sensitivities. All 253 fitted
cases converge, maximum split R-hat is 1.095, minimum reported ESS is 30.09,
maximum final mode change is 0.00185, and maximum geographic covariance jitter
is 1e-12. No case is silently dropped. The table below covers the 120 final
development/confirmation cases; the full audit also includes screening and
sensitivities.

| Model | Invalid draws/counts | Numerical failures | Mean stress rate % | Worst stress rate % | Min matched valid votes % |
| --- | --- | --- | --- | --- | --- |
| Current | 0 | 0 | 0.000 | 0.000 | 99.03 |
| Literal / pooled / common | 0 | 0 | 0.244 | 1.562 | 99.03 |
| Aggregate + kind / common (selected) | 0 | 0 | 0.155 | 1.758 | 99.03 |
| + regional innovations | 0 | 0 | 0.317 | 1.367 | 99.03 |
| + local mean reversion | 0 | 0 | 0.220 | 1.758 | 99.03 |


The stress rate is the larger of two flags: a named party above 80% of valid votes, and non-valid above 90% of electors. These permissive flags do not exhaust substantive plausibility. Broad residual Other is excluded from the named-party flag. National scoring retains the entire common source territory; municipality scoring uses auditable matched identities.

## Restricted sensitivities

All sensitivity comparisons below use 128 draws, including matched-size mixed-history and unperturbed baselines. They do not revise the frozen variant choice. Values use the same units as the main tables.

### Six political groups plus non-valid voting

PD, M5S, Lega, FdI, FI/PdL and Other; all six forecast elections at 15 days. Historical grouped questions remain grouped in the likelihood. This is a sensitivity to a wider panel, not evidence that coarse-panel WIS can be compared numerically with six-group WIS.

| Variant | National coverage % | National width | National MAE | Municipal coverage % | Municipal MAE | Non-valid width | Non-valid MAE |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Current | 33.33 | 6.10 | 4.34 | 69.70 | 5.67 | 22.72 | 5.74 |
| Literal / pooled / common | 72.22 | 11.84 | 3.91 | 87.66 | 5.32 | 43.84 | 12.75 |
| Aggregate + kind / common | 72.22 | 8.05 | 2.84 | 92.27 | 4.36 | 46.58 | 5.90 |


### Camera-only versus mixed Camera/European history

The same selected structure, Camera 2018 and 2022, four horizons each. Camera-only filters both election counts and ballot-specific European polling observations. Forecast geography/electorate inputs follow each history inventory, with matched-mass audits retained.

| Variant | National coverage % | National width | National MAE | Municipal coverage % | Municipal MAE | Non-valid width | Non-valid MAE |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Mixed history | 78.12 | 23.22 | 5.81 | 93.62 | 7.79 | 53.42 | 7.82 |
| Camera-only history | 87.50 | 25.56 | 6.04 | 88.46 | 8.67 | 62.23 | 8.70 |


### Local-scale perturbation

Multiply the selected fitted local innovation scale by 0.85 or 1.15, refit the state and global scales, and forecast Camera 2022/European 2024 at 15 days. This checks sensitivity to conditional empirical-Bayes local covariance; it does not integrate its full posterior.

| Variant | National coverage % | National width | National MAE | Municipal coverage % | Municipal MAE | Non-valid width | Non-valid MAE |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 0.85 × local scale | 75.00 | 7.68 | 3.02 | 92.74 | 4.98 | 46.03 | 4.34 |
| 1 × local scale | 75.00 | 7.69 | 3.10 | 94.72 | 5.09 | 45.90 | 4.54 |
| 1.15 × local scale | 75.00 | 7.61 | 3.19 | 96.08 | 5.22 | 44.96 | 4.66 |


## Interpretation and remaining limits

- **Plausibility comes first.** Softmax and multinomial counts solve support
  and accounting, but do not eliminate extreme latent-process tails. Over the
  24 development/confirmation references, the selected model's mean stress
  rate is 0.155%, versus 0.317% regionally and zero observed for the current
  generator. The selected model's worst rate is 1.758%, versus 1.367%
  regionally: the worst-case ordering reverses at 512 draws. The screening
  decision is therefore sensitive to Monte Carlo tail counts. Regional
  improvement is real in these coverage tables; a unique plausibility winner
  among drift variants is not established.
- **Precision improves close to polling day.** The selected model's national
  political interval width falls from 35.39 points at two years to 7.98 at
  15 days, and its MAE from 8.95 to 3.09. Fifteen-day coverage is still 6/8
  political outcomes: Lega 2022 and M5S 2024 lie below their intervals.
  Municipal coverage near 97% against nominal 90% also indicates conservative
  marginal intervals, rather than exact calibration.
- **Participation is the main unresolved problem.** The selected non-valid
  intervals average 49.68 points wide in confirmation, and 45.36 even at 15
  days. Party-intention polls supply no direct participation observation.
  The current generator has narrower participation intervals and better
  participation WIS in both development and confirmation. Its participation
  MAE is better in development, while the drift candidate's is better in
  confirmation. Keeping non-valid voting in the simplex is coherent; a shared
  political/participation diffusion scale is not thereby validated.
- **Evidence is limited.** These are chronological holdouts for this variant
  selection, not a prospective test on new elections; previous research
  already examined these historical results. Four development and two
  confirmation elections cannot establish long-run calibration. Three named
  parties plus Other, and the wider sensitivity below, do not validate all
  individual lists. The Laplace/empirical-Bayes qualifications in the protocol
  remain material, especially for richer local variants.
- **Cross-kind history is useful, with a trade-off.** In the restricted
  Camera comparison, mixed history improves political MAE nationally
  (6.04 to 5.81 points) and municipally (8.67 to 7.79), and improves non-valid
  MAE. Camera-only has higher national coverage; mixed history has higher
  municipal coverage. This supports modeling kind effects rather than
  assuming either complete interchangeability or complete non-comparability.
  The six-group near-election panel still achieves only 72.22% national
  coverage, so additional party detail is not yet calibrated.
- **Local-scale uncertainty does not explain the late national misses.**
  All three local-scale settings retain 75% national coverage at 15 days;
  municipal coverage ranges from 92.74% to 96.08%. This is a sensitivity
  result, not permission to tune on the confirmation elections.

The model recommendation is a **research starting point**, with the regional
branch retained as a strong coverage alternative. It is not an instruction to
replace defaults. A focused subsequent comparison could give non-valid voting
its own national volatility while retaining a common political volatility;
that intermediate variant was not tested here. Full-list split uncertainty,
filing support, dated electorates, overrides, paired chambers and seat
distributions still require runtime implementation and validation.

## Artifacts and verification

The protocol's commands reproduce the experiment without acquiring new polls.
Primary inputs are eight pinned official election archives and the compatible
downloaded waves; each case records the actual as-of election/poll inventory.
Full cases live under `data/derived/drift/<version>/`.

- `latent-drift-v1-367ca6e3b6a9`: screening, frozen selection,
  `comparison.json`, matched 128-draw baselines, `supplemental-summary.json`
  and `complete-audit.json`.
- `latent-drift-v1-53927eca1222`: all 512-draw finalists and
  `final-summary.json`.
- `latent-drift-v1-332c7730d812`: six-group comparisons including current.
- `latent-drift-v1-5f9e284c584f`: Camera-only history.
- `latent-drift-v1-6d271dc99b8e` / `latent-drift-v1-774466388e5e`:
  local scales 0.85 / 1.15.

The reduced seven-dimensional Bayesian check, including an observed zero,
finds maximum predictive probability mean differences of 0.17 and 0.53 points
in its two examples; 90% width ratios are 0.978–1.038. This supports the
Laplace approximation on that problem, not an exact-posterior guarantee for
the full fit. Preparation and Svelte type checks pass, all **1,002 unit tests**
pass, and the static build followed by **30 browser tests** passes. No runtime
model/default activation, commit or publication is performed.


