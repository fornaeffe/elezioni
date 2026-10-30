# Installed legacy model comparison

The two tested legacy procedures use the unchanged installed R-derived vote
generator. “Without polls” means **without general poll refresh**: both variants
still receive the requested explicit positive poll references for new lists.
This differs from the installed historical-only default and from the fast-model
predecessor previously called `legacy-global-beta-v1`.

Use [the execution guide](vote-generation-legacy-comparison-plan.md) for commands
and [the selected candidate's results](vote-generation-feasible-results.md) for
the preceding experiment. The production default is unchanged.

## Interpretation and shared test design

Historical accuracy compares the same subsequent Camera elections, observable
groups, municipalities, conditional target electorates, filing and geographic
crosswalks as the selected candidate. There are 12 reduced cases per procedure
and full 2018/2022 short-horizon cases, with 512 draws per case. Reference dates
only constrain available inputs; unchanged effective inputs reuse the same
seeded draws. The four 2018 no-refresh cutoffs have identical effective inputs.

Historical forecasts use the latest eligible previous Camera result, not the
2022 production snapshot retroactively. A missing/invalid municipality baseline
uses its region's eligible original counts, rescaled to the conditional target
electorate. Original Camera lists remain distinct before the membership matrix.
Unknown identities do not inherit votes through the research residual. The
candidate's compact **geography**, rather than its fitted profiles, supplies the
municipal census crosswalk to current physical colleges and constituencies.
The full accuracy domains retain the preceding study's exclusions, including
VdA and the unmatched 2022 municipalities; they are not a claim of complete
all-territory historical validation. Future paired tests include special regions.
The full municipal domains contain 7,883 observations in 2018 and 7,830 in 2022.
Respectively 100 and nine lack a unique current-college crosswalk; they remain
in municipal/evaluated-area scores and are omitted from college/constituency
scores. Both comparisons cover 52 physical colleges and 27 constituencies.
The labelled missing/invalid-history regional fallbacks cover 94 and 40
municipalities, respectively.

Scores give each observable group equal weight within a unit, each unit equal
weight within a case, and each case equal weight in panel summaries. They cover
municipalities, colleges, constituencies and the evaluated-area aggregate.
No internal truth is fabricated by splitting reported joint tickets. Encoding
a predicted observable group at one temporary coordinate changes neither its
score nor its subsequent coarsened area score; an independent invariance test
checks this.

Error30 is the generated frequency of an absolute error exceeding 30 percentage
points. Coverage90 measures truth falling inside the central 90% interval.
MAE measures the error of the predictive mean; WIS and CRPS assess the whole
distribution. Near-truth probability is a finite-band frequency, not a density
or joint likelihood. A draw above 80% can be plausible for a genuine local
majority, so that tail statistic needs context. The elector-denominator scores
also include combined non-valid support.

Monte Carlo tail comparisons aggregate whole joint draws before estimating
sampling error. Model streams are different, so comparison intervals use
independent-stream standard errors, not paired differences. They quantify
simulation error conditional on these reused cases. They do not establish
calibration for an unseen election. WIS, coverage and mean errors remain point
comparisons. Across-list signed errors cancel compositionally; inspect the
per-group signed bias retained in the compressed evidence.

<!-- BEGIN GENERATED COMPARISON -->
### Full-domain historical results

At the 15-day source cutoff; 512 draws. Error30 and coverage are percentages; MAE is in percentage points. Lower error/MAE/WIS is better.

| Election | Scope | Procedure | Error30 % | Coverage90 % | Mean MAE pp | WIS |
| --- | --- | --- | ---: | ---: | ---: | ---: |
| 2018 | municipality | Selected candidate | 2.03 | 89.60 | 5.02 | 0.0282 |
| 2018 | municipality | Legacy, no general refresh | 6.91 | 82.61 | 8.28 | 0.0436 |
| 2018 | municipality | Legacy + poll refresh | 2.50 | 81.87 | 5.87 | 0.0335 |
| 2018 | college | Selected candidate | 0.27 | 84.29 | 4.26 | 0.0238 |
| 2018 | college | Legacy, no general refresh | 4.38 | 83.01 | 7.54 | 0.0395 |
| 2018 | college | Legacy + poll refresh | 0.85 | 77.88 | 5.06 | 0.0297 |
| 2022 | municipality | Selected candidate | 1.11 | 92.39 | 3.35 | 0.0168 |
| 2022 | municipality | Legacy, no general refresh | 4.93 | 82.38 | 5.71 | 0.0322 |
| 2022 | municipality | Legacy + poll refresh | 0.78 | 86.36 | 3.23 | 0.0168 |
| 2022 | college | Selected candidate | 0.16 | 80.11 | 2.75 | 0.0135 |
| 2022 | college | Legacy, no general refresh | 2.04 | 86.96 | 4.93 | 0.0273 |
| 2022 | college | Legacy + poll refresh | 0.22 | 90.71 | 2.56 | 0.0129 |

### Reduced panels across all four cutoffs

Equal weight per cutoff, including reused identical no-refresh forecasts.

| Election | Procedure | Municipal Error30 % | Municipal coverage90 % | Municipal MAE pp | College Error30 % | College coverage90 % |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| 2013 | Selected candidate | 5.90 | 90.09 | 6.95 | 4.08 | 87.30 |
| 2013 | Legacy, no general refresh | 5.66 | 75.97 | 7.91 | 4.24 | 74.17 |
| 2013 | Legacy + poll refresh | 4.34 | 75.62 | 8.32 | 2.98 | 72.34 |
| 2018 | Selected candidate | 2.52 | 88.72 | 5.45 | 1.88 | 86.93 |
| 2018 | Legacy, no general refresh | 6.24 | 83.60 | 8.18 | 5.55 | 81.82 |
| 2018 | Legacy + poll refresh | 2.84 | 84.45 | 5.91 | 2.17 | 81.72 |
| 2022 | Selected candidate | 2.13 | 85.48 | 4.76 | 1.76 | 82.38 |
| 2022 | Legacy, no general refresh | 5.23 | 83.05 | 6.21 | 4.37 | 84.96 |
| 2022 | Legacy + poll refresh | 2.29 | 82.23 | 4.77 | 1.77 | 84.34 |

### Whole-draw simulation error for full-domain college tails

Legacy minus selected Error30, in percentage points. Intervals describe Monte Carlo error only.

| Election | Legacy procedure | Difference pp | Monte Carlo SE pp | Normal 95% interval pp |
| --- | --- | ---: | ---: | --- |
| 2018 | Legacy, no general refresh | 4.11 | 0.21 | 3.69 to 4.53 |
| 2018 | Legacy + poll refresh | 0.58 | 0.04 | 0.51 to 0.66 |
| 2022 | Legacy, no general refresh | 1.87 | 0.09 | 1.69 to 2.06 |
| 2022 | Legacy + poll refresh | 0.06 | 0.01 | 0.03 to 0.09 |

### Requested future ballot in Chromium

1,000 attempted paired simulations per procedure, including all failed attempts.

| Legacy procedure | Browser | Total min | Load s | Preparation s | Generation min | Scrutiny min | Exceptions | Non-computable returned results | Filled 588-seat results / returned results |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| Legacy, no general refresh | 148.0.7778.96 | 19.49 | 0.16 | 0.94 | 14.14 | 5.29 | 2 | 0 | 998 / 998 |
| Legacy + poll refresh | 148.0.7778.96 | 21.30 | 0.17 | 1.34 | 15.72 | 5.22 | 18 | 1 | 981 / 982 |

Failed draw IDs: no-polls: 131, 580; polls: 23, 59, 125, 239, 285, 417, 495, 516, 570, 617, 637, 692, 726, 785, 825, 832, 875, 935.

Elapsed time includes failed attempts. The generation/scrutiny sub-timers include completed attempts only; they must not be used to hide time spent in exceptions.

Non-computable draw IDs: no-polls: none; polls: 135.

<!-- END GENERATED COMPARISON -->

The poll-informed legacy procedure is a meaningful comparator. The selected
candidate improves the full 2018 municipal and college tails, mean errors and
distribution scores. In full 2022, the poll-informed legacy procedure instead
has lower municipal Error30 and mean error, and higher college coverage with
lower college mean error/WIS. The selected candidate retains the lower college
Error30. There is no uniform winner across both elections and priority metrics.

This does not justify silently changing the selected production candidate. The
procedures also differ in polling construction, historical inputs, reference
uncertainty and non-valid support semantics. A common-reference comparison would
be needed to attribute these differences specifically to the transition laws.
The present experiment answers the requested ordinary-user procedure comparison.

## Inputs an ordinary scenario user supplied

The matrix uses recognized identities from the reviewed membership table,
including PD, FI/PdL, Lega, M5S, SEL/SI, PRC and the identified joint tickets.
Deterministic illustrative weights include Azione/IV 56/44, Noi Moderati/UDC
75/25, and LeU's Articolo Uno/SI/Possibile 40/50/10. The JSON policy records
all overrides, equal-weight fallbacks and identity citations. These are declared
user assumptions, not estimated internal historical vote counts.

The Senate TAA source uses named joint candidate groups. The centre-right group
is split FdI/FI/Lega/NM/UDC 60/11/27/1.5/0.5; the PD/+Europa/AVS group is split
65/12/23. The broader Campo Base ticket receives the named-party proxy split
recorded in the future-input JSON, using known Camera TAA relative strengths.
These are recognizable joint tickets, not unidentified residuals. FN also gets
an explicit 7.2% TAA point, using the national poll as a labelled territorial
proxy, because the production poll refresh excludes TAA. Input preparation
checks that every offered special-region list has positive aggregate support.

Legacy factors normalize over every destination of a historical row. A single
recognized destination therefore receives that whole row. A fractional factor
cannot discard an unidentified remainder. Unmapped rows and mapped votes where
the future list is unfiled become combined non-valid support. Ordinary municipal
mapped historical valid fractions are retained; no observed held-out turnout
is supplied. Special-region turnout uses the same explicit authored assumptions
as the future candidate scenario. Turnout and residual handling consequently
remain a substantive difference between procedures.

Before reference refresh, mapping/filing removes 3.89%, 4.35% and 3.81% of the
rescaled historical valid mass in the 2013, 2018 and 2022 scenario domains,
respectively. Those amounts become non-valid support rather than a named
residual list. They are model projections on conditional electorates, not
observed changes in turnout.

For new lists, use the newest explicitly named, eligible observation found in
the reviewed archive plus the recorded basic source searches. Unnamed Other is
not a measured new-party share. A list lacking an eligible named poll and any
recognized historical link stays unsupported; every such case is listed in the
input audit. This includes future tickets that did not yet exist at early
historical cutoffs. The search is not an exhaustive census of every poll ever
published; sparse early archives remain a limitation.

For the future scenario, the new-list points include FN at 7.2%, Progetto
Civico/PSI at 1.3% and Ora at 1.2%, before any required joint normalization.
The first two follow the source-eligible reviewed October records. Ora uses the
separately identified [Youtrend/Sky observation published 8 October](https://tg24.sky.it/politica/2026/10/08/sondaggio-nuova-legge-elettorale),
with fieldwork ending 6 October. SWG's joint LibDem/Ora answer must not be read
as separately observed 0.6% shares. The Azione/PLD reference combines the latest
named estimates, explicitly as a mixed-wave scenario assumption.

The poll variant refreshes established parties from the latest reviewed
publication; unreported lists retain the historical relative remainder. When
mixed-wave inputs exceed 100%, their named points are normalized while retaining
the historical unreported mass; the future input audit records the 100.4% raw
sum and adjustment. It uses the existing legacy poll-reference path and its
measurement term, not a new fitted movement model. Manual points have no
transferred candidate reference SD. The legacy single reference date refreshes
all supplied manual points together; the audit retains their individual source
dates so this approximation is visible.
Manual overrides refresh both national and territorial shock ages. The poll
path refreshes national ages while retaining historical territorial ages.
The complete poll reference vector also dates baseline-derived remainder rows
at that common date, although their shares were not separately measured; their
extra measurement sigma is zero. Equal point means therefore do not imply equal
predictive uncertainty across the two procedures.

This is a simple latest-publication reference construction through the existing
engine interface. It does not run the separate production polling estimator's
14-day decay, one-wave-per-house pool and historical pseudo-sample. Consequently
the label describes the legacy vote generator **with these poll inputs**, not a
claim that every current polling-UI setting was replayed verbatim.
Measurement errors use an assumed effective-sample proxy and the existing 0.08
logit error floor. Deterministic split weights do not propagate a fitted
decomposition uncertainty.

Special-region references retain the candidate scenario's explicitly authored
VdA alliance proxies and Bolzano/Trento priors as point assumptions. No whole
historical VdA alliance is recast as identified Union Valdôtaine votes. With four
offered VdA lists, the three fixed ticket anchors leave the fourth share as the
FN remainder; it is not FN's measured national support in VdA. The exact TEAM K
Senate label links to the future Team K identity. It is not evidence of a
separately observed Camera list. These assumptions remain editable limitations.

## Operational findings and implementation checks

The browser experiment uses the actual 24-list scenario in both chambers,
production preparation, candidature/preference generation and legal scrutiny.
There is no future observed truth against which to score this ballot. Loading
and reference preparation are reported separately; legacy has no fitting step
equivalent to the selected model's refit. Browser workloads were run concurrently
with other research computations on this machine. Elapsed times describe that
measured workload, not an isolated speed ranking.

At least one tested legacy forecast reaches a shared scrutiny failure:
`Invalid recovery seat or retained election`. The poll-informed simulation 23
also reproduces in Node after successful generated-input validation; the stack
enters `recovery.ts`'s preflight from scrutiny. This is an operational failure
on the generated legal path, not proof that the municipal probability vector
is invalid. The benchmark retains failing IDs and does not change scrutiny or
retune forecasts to avoid the case. The final browser table records all attempts.

Inspection localized the original mismatch: the TAA allocator considered all
locally filed members of an admitted coalition under the interpretation then
adopted for C93-septies(2)(b). The recovery eligibility adapter filters by national
list admission or the separately recorded local exceptions. In draw 23, it therefore
omits `europa-piu-uno-cd` despite that list already holding a filled Camera TAA
proportional seat. Recovery preflight rejects the seat because no eligibility
row accompanies it. An isolated diagnostic build exposed this rejected row by
changing only the exception message; the production files and benchmark code
were not changed during that diagnosis.

On 30 October, Luca reported external legal review and authorized the corrected
interpretation: C93-septies(2)(b) implicitly restricts the internal coalition quota
and recipients to admitted lists. The implementation now retains all coalition
votes at subject level but excludes unadmitted members from the internal division;
the recovery filter remains unchanged. This supersedes the earlier proposal to
widen recovery eligibility. See the [current N11 contract](politics-reform-2026-specification.md#inputs-votes-and-admission).

All 20 original exceptions now complete in a separate Node replay: no-refresh
131 and 580; poll-informed 23, 59, 125, 239, 285, 417, 495, 516, 570, 617, 637,
692, 726, 785, 825, 832, 875 and 935. Each yields 588 filled seats (392 Camera,
196 Senate) with 588 distinct candidates. In draw 23 the centrosinistra TAA seat
now goes to PD rather than nationally excluded `europa-piu-uno-cd`. The frozen
variant input hashes are unchanged; draw 23's generated-input hash matches the
original diagnostic. The receipt is
`test/fixtures/benchmarks/taa_admission_replay.json`, with code/input hashes and
per-draw seat/eligibility checks. Original benchmark receipts and browser timings
remain historical evidence; this targeted replay is not a fresh 1,000-draw browser
acceptance run.

The original poll-informed draw 135 followed a different failure path: it returned a non-computable
Senate college allocation, `N21/impossible-residual`, affecting region `19`
(Sicilia), after successful regional/list allocation. That result contains 399
filled seats rather than the required 588. The downstream proclamation/recovery
stages consequently remain non-computable. This is retained as a failed
attempt, not silently replaced or counted as a complete simulation. It required a
separate territorial-allocation investigation; no statutory rule or fallback
was changed to force completion during this benchmark. The corrected TAA replay
also checked draw 135 as a separate control: the same Senate failure and 399 filled
seats remained at that point.

The [30 October Sicilia investigation](senate-sicilia-n21-review.md) reproduces
the exact generated input and identifies a single-pass initial remainder limit:
Sicilia 1 needs four remainder seats with three eligible lists. An isolated
extension of Camera's I14 policy completes all 588 seats. Luca subsequently
approved the Senate extension, now implemented as I14-v2 for N21 and N22 internal
lists. Its implementation verification is recorded in that investigation;
original receipts retain their historical meaning.

The separate [paired Node replay](../test/fixtures/benchmarks/senate_i14_replay.json)
now completes both frozen 1,000-attempt workloads with 588 filled seats and
distinct candidates in every attempt. Draw 135 is the only allocation change
relative to the post-TAA/pre-I14-extension baseline. Seven other objects differ
only in I14's version labels; 1,992 are byte-identical. This is functional
verification and does not replace the original browser timing measurements.

Research-adapter verification covers cutoff availability, deterministic joint
weights, unmapped support, exact elector conservation, unchanged-input identity,
no held-out-count influence, regional missing-history fallback, TAA exclusion
from ordinary poll refresh and observable-group scoring invariance. The focused
suite includes the existing production generation tests. TypeScript preparation
checks pass. During the original comparison no production generator, legal
algorithm or default was changed; the later TAA legal correction is recorded above.

Compact evidence is retained in
`test/fixtures/benchmarks/vote_share_legacy_comparison.json` and
`vote_share_legacy_details.json.gz`; regenerable full scores and legal input
diagnostics remain under `data/derived/legacy-comparison/`.
