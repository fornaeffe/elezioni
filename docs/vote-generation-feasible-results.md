# Filing-aware research results, 30 October 2026

Use [the model](vote-generation-feasible-model.md) for equations and
[the execution guide](vote-generation-feasible-research.md) for reproduction.
This is iterative model development, with an opt-in candidate application.
It does not activate the ordinary reform default or certify predictive calibration.

## Default choice set

The authored default in `data/fast-share-future-scenario.json` contains Luca's
24 lists and two coalitions in both chambers. Ora! and DSP file in precisely the
14 requested Camera constituencies and throughout the nine corresponding Senate
regions. The other ordinary/TAA/VdA filings and five minority declarations match
the requested scenario. Those declarations describe the scenario, not legally
certified future candidatures or finalized real electoral agreements.

Historical correspondences preserve original reported groups. PD/Articolo Uno,
AVS's constituent identities, FI/PdL, Lega and the recognized smaller formations
retain their historical atoms. The existing distinct poll atoms represent
Più Uno, Progetto Civico, PLD and Futuro Nazionale. Spazio Pubblico belongs to the
future joint ticket without an invented separate historical vote series. Exact
TEAM K/Die Freiheitlichen labels are linked as whole identities. No broad VdA
alliance is reclassified as observed Union Valdôtaine votes.

The fit has **28 observed-supported coordinates**, including historical-rest;
the future scenario has **34 components**. This does not mean every component
has recent separate polling support or an independently well-estimated split.
Six components use explicit reference support: Ora!, Süd-Tiroler Freiheit,
Team K, Union Valdôtaine and the two new VdA tickets. Grouped observations remain
grouped; Gaussian diagnostics do not prove empirical identification.

The editable assumptions include Ora! at 0.5% nationally (0.5 percentage-point
SD); STF at 10% in Bolzano (6 pp SD), with a weak 0.3% new-filing reference in
Trento (0.3 pp SD). The Bolzano reference is informed by the official 2023
provincial 10.9% result, not treated as an observed political-election count.
Team K borrows province-specific 2022 Senate list shares, with labelled weak
positive support where it was not observed. VdA references use explicitly
labelled whole-alliance proxies and uncertainty; they do not identify internal
party shares. Special-region turnout is anchored separately. Municipal 2022
18+ electorates stay fixed conditionally in both future chambers.

Historical-rest is available to the fit/editor but is unassigned to the requested
future ballots. Its support is conditionally redistributed over offered choices,
rather than silently mapped into a named future party or counted as non-valid.

Identity/reference evidence is linked in the authored scenario. Examples include
[the Camera's IV/Casa Riformista record](https://www.camera.it/leg19/217?idlegislatura=19&shadow_gruppi_parlamentari=4291),
[PSI's Avanti project](https://www.partitosocialista.it/avanti-per-litalia-e-il-nuovo-progetto-dei-socialisti-e-sara-sulla-tessera-2026/),
[the official 2022 parliamentary election summary](https://www.parlamento.it/leg/19/Elettorale/riepilogo.htm)
and [Bolzano's 2023 provincial results](https://news.provinzia.bz.it/it/news/elezionialtoadige23-i-risultati-provvisori).

## Comparison design and selection

Each case uses only cutoff-eligible past elections and polls, then forecasts a
subsequent political election. Development is Camera 2013/2018 at 730, 365, 90
and 15 days; later assessment is Camera 2022 at the same horizons. Each candidate
uses 512 predictive draws with shared fitted/posterior seeds. These panels and
the 2022 results were used in earlier research: **2022 is reused validation, not
an untouched holdout**. Subsequent experiments are dated iterative development.

The initial seven candidates compare global Beta, filing-stratum Beta and uniform
bounded pairs, including scale sensitivities. The country-calibrated reference
experiment worsened development mean error and interval scores, so it was not
selected. Mass-weighted matching addresses variance disappearing when major
parties are paired with numerous tiny components. The last experiment reallocates
existing profile variance to a shared regional term; municipal marginals remain
unchanged in distribution. Each round's candidates were declared before its runs.

The selected compromise is mass-weighted pairs, scale **1.5**, regional profile
variance share **0.25**. On the eight development cases, changing rho from zero
to 0.25 raises college 90% interval coverage from 85.58% to 87.11%, while municipal
error-over-30-pp probability stays approximately 4.2%. Rho 0.5 raises college
coverage only to 87.54%, with larger college tails and slightly worse WIS and
near-observation probability. Selection was frozen before the new regional
2022 validation runs. Uniform pairs have lighter tails but inadequate aggregate
coverage; stratum Beta remains the alternative with more of the earlier law.

The global-Beta comparator above is a preceding fast-model kernel, not the
installed R-derived production generator. A separate
[legacy comparison assessment](vote-generation-legacy-comparison-plan.md)
defines how to add that reference without changing its probability law.

## Full-geography short-horizon results

These runs evaluate 7,883 municipalities in 2018 and 7,830 in 2022, reconstructing
52 current Camera physical colleges and 27 constituencies. Nine 2022 municipalities
cannot be uniquely crosswalked to colleges. The original retrospective domain
excludes VdA; these are full retained historical domains, not every historical
ballot or a validation of all future territorial assumptions. Known future filing
and electorates are conditional inputs. Historical vote outcomes enter scoring.
College shares use labelled census population fractions for split municipalities.

Scores give equal weight to observable groups within a unit, then equal weight
to units. Joint lists are coarsened to common observable groups; no internal
component truth is invented. MAE is in percentage points. Tail is the probability
that a draw differs from the actual share by more than 30 pp, averaged over units
and observable groups. These are descriptive diagnostics, not universal caps.

| Election/model | Municipal tail | Municipal 90% coverage | Municipal MAE | College tail | College 90% coverage | College MAE |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 2018 global Beta | 2.430% | 90.65% | 4.88 | 0.240% | 84.62% | 4.14 |
| 2018 stratum Beta | 2.381% | 90.36% | 4.94 | 0.254% | 83.97% | 4.19 |
| 2018 uniform pairs | 1.999% | 89.54% | 5.02 | 0.123% | 77.24% | 4.27 |
| 2018 selected | 2.030% | 89.60% | 5.02 | 0.267% | 84.29% | 4.26 |
| 2022 global Beta | 1.384% | 93.59% | 3.31 | 0.182% | 79.78% | 2.73 |
| 2022 stratum Beta | 1.266% | 92.98% | 3.37 | 0.133% | 74.91% | 2.77 |
| 2022 uniform pairs | 1.068% | 92.48% | 3.35 | 0.112% | 67.66% | 2.73 |
| 2022 selected | 1.114% | 92.39% | 3.35 | 0.162% | 80.11% | 2.75 |

The selected model reduces municipal extreme-error tails against global Beta,
keeps college coverage close to that comparator and removes unsupported global
future-margin requests by construction. It does **not dominate every score**:
the 2018 college tail is slightly larger, and mean errors remain slightly worse.
In full 2022, college WIS improves from 0.01384 to 0.01355; probability within the
declared near-truth band declines from 54.36% to 50.88% as shared uncertainty
widens. Sampled-country coverage is 75% for both. Exact point likelihood is not
estimated from 512 discrete predictive draws; interval coverage, proper WIS/CRPS
and neighbourhood probabilities assess support around actual results.

The remaining 80–84% college coverage for nominal 90% intervals is material
undercoverage. At long horizons, the selected reduced 2022 case has only 75.68%
college coverage at 730 days. Mean error also grows: Lega is overestimated by
about 14.2 pp and FdI underestimated by 14.1 pp in the average sampled municipality
at that horizon. At 15 days, sampled residual support is still underestimated by
6.34 pp. Compositional signed errors sum to zero, so their overall average is
not evidence of absence of bias. New parties, structural realignments and regional
minorities need specific evidence and sensitivity review.

The retrospective accuracy panel is Camera-only. The future Senate uses the
existing adapter from the 2022 common-18+ paired election; the bundle has no
earlier comparable common-18+ Senate political-election pair for a strictly
prospective adapter test. Successful paired generation and legal scrutiny do
not establish Senate predictive calibration.

## Evidence and computation

The compact [comparison report](../test/fixtures/benchmarks/vote_share_feasible_research.json)
and compressed group/Monte Carlo details retain every comparison and selection.
There are 134 successful candidate/case runs, with 512 draws each; these are
repeated variants of three political elections, not 134 independent elections.
Tail-rate simulation errors aggregate whole joint draws before estimating MCSE
and paired intervals, preserving geographic dependence. They condition on the
fixed cases and do not estimate uncertainty about future elections. WIS, coverage
and mean-error differences remain point estimates. Elector-denominator scores
are retained separately from political valid-vote shares.

In the full domains, selected-minus-global-Beta municipal tail-rate differences
are -0.400 percentage points in 2018 (conditional simulation 95% interval
-0.496 to -0.304) and -0.270 pp in 2022 (-0.304 to -0.237). The much smaller
college tail differences have intervals containing zero in both elections;
their signs should not be interpreted as established improvements or regressions.

The candidate release verifies 7,904 future municipalities, 15,810 physical cells,
24 lists per chamber, all 588 modelled domestic seats filled in the smoke test, and zero startup
fits. Generation assets are about 9.00 MB compressed; advanced-refit history is
about 4.51 MB. This is a local static candidate build, not a public deployment.

The actual Chromium **148.0.7778.96** run on the **i7-14700KF** completed loading,
a real national/profile/paired refit and **512 paired generations/legal scrutinies
in 1,583.0 seconds (26.38 minutes)**. All 512 requested runs succeeded, with all
588 modelled domestic seats filled in each and no non-computable stages. The
refit took 6.75 seconds; generation took 1,413.28 seconds and scrutiny 160.34
seconds. The default reference date was deliberately changed from 19 to 20 October
to force the refit. There was no reroll or omission of failed requests.

The [browser report and full ledger](../test/fixtures/benchmarks/vote_share_feasible_browser.json)
retain the exact catalog, implementation, request and timings. This is a measured
single-worker default computation, not an extrapolation or acceptance of every
possible edited scenario/draw budget. The unchanged memory policy permits one
generation consumer for this larger packet; unsafe parallel reservations are
rejected. Other regression checks ran briefly during the measurement. Browser
storage/plots/export at the full campaign size, maximum-envelope resource
acceptance and ordinary default activation remain the separate FV-08 gates.

All 1,232 unit tests, seven Chromium release/refit/worker checks and 16 functional
Chromium/Firefox workspace checks across `/` and `/elezioni` pass. The latter
validate the real exported payload before replay. They exposed and fixed a
fractional worker-memory estimate that violated the existing result importer;
the producer now rounds the reported MiB value up. Corrected single-run Chromium
page/worker samples peaked at 420.85 MiB root and 444.69 MiB project-base. An
earlier 589.64 MiB failed check is retained separately; these samples do not
establish an unsampled bound or full-campaign memory/export acceptance. The
ordinary root build was restored with candidate dispatch left opt-in.
