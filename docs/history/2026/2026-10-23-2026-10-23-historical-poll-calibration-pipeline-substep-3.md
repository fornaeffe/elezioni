## 2026-10-23 — Historical poll calibration pipeline (substep 3)

Implemented matched historical inputs, official-result import, partially pooled
candidate fitting and chronological validation. The pipeline substep is complete;
the parent forecasting task and forecast acceptance remain open. Current v1
parameters remain assumptions: no unsupported correction was enabled. Operations
and statistical boundaries are in [polls-calibration.md](../../polls-calibration.md).

Selected 16 original government-submitted PDFs: SWG/Demos/Tecnè for Camera
2013/2018/2022; SWG/Demos for European 2014; all three for European 2019;
SWG/Tecnè for European 2024. The retained historical selection includes reviewed
question/page IDs, dates, methods, sample bases, translations and checksums.
PDFs total 275,342 bytes; the complete historical source/target set is 380,434
bytes (371.5 KiB), below its enforced 1 MiB budget. Together with current polls,
tracked selected inputs total 543,667 bytes. Ordinary preparation needs no
network, PDF parser, Python, R or ignored caches.

Archive fieldwork filters also return local and later-published surveys.
Excluded Demos 11857 (post-election Veneto autonomy), 12931 (Veneto vote),
SWG 1350 (Bari), later Demos 2019 topic releases, and Tecnè 1331 (generic
political-party table without an established European ballot). Correct national
Demos 2022 source is 11839. PDF publication dates control eligibility, not archive
insertion dates. SWG 2018/Tecnè 2019 range midpoints are labelled estimates;
Demos decision certainty is not a vote-table nonresponse fraction. Complete PDF
text was inspected; representative attachment/range tables were also rendered
and visually reviewed. Metadata N/methods were checked against the sources.

Downloaded eight pinned Interior Ministry archives: Camera 2008/2013/2018/2022
and European 2009/2014/2019/2024. Earlier same-kind elections supply baselines.
Large originals remain ignored under `data/sources/poll-history/`; tracked
`results.json` retains full list totals, denominators, member/source hashes and
the comparable panel. Node `verify-results` reproduced it exactly from all eight
originals; download/cache verification passed. `freeze-results` is an explicit
reviewed input refresh, never part of normal check/generate. Candidate-preference
members repeat list votes and are excluded. Camera excludes Valle d'Aosta and
overseas; European totals include domestic Valle d'Aosta and separate 2024
out-of-town voters, excluding overseas. Adult poll frames approximate these scopes.

Camera 2018 uses attributed national totals. Camera 2022 reuses the existing
original-college candidate-only attribution: restore 1,027,135 candidate-only
votes, yielding 28,096,790 valid list votes. An initial test used an incorrect
remembered M5S total; independent source candidature summation confirmed
4,336,660, and the expectation was corrected. Pinned open-data snapshots are
not asserted to equal every later/live proclamation release. Baseline snapshots
are current frozen historical results, not reconstructed download vintages.

The comparable partition is PD, M5S, Lega and all other lists. Each selected poll
observes the three parties separately; other is a changing exact complement,
not a transferable party identity. Missing FdI detail is not zero or inferred
from future results. European translations retain this panel and complete PDFs;
Camera translations retain fuller tables. Empty 2008/2009 M5S result mappings
explicitly mean ballot absence. Selection is purposive, not a complete poll census.

Five expanding chronological holdouts run from European 2014 through 2024,
including two Camera holdouts. Each fold trains only on earlier elections;
election scores have equal weight regardless of house count. Reuse production
v1 weighting/prior code. Center house deviations within elections, then shrink
house and aggregate bias toward zero with three prior election equivalents.
Fit separate error floors for corrected/uncorrected means, subtracting existing
measurement/prior variance and assumed future movement before adding a
nonnegative floor. Blackout movement, shared response bias and electorate
mismatch cannot be identified separately from these endpoint errors.

Held-out MAE in percentage points and pointwise nominal 90% interval coverage,
with four equally weighted panel categories per election:

| Candidate | All-event MAE | Coverage | Camera MAE | Camera coverage |
| --- | ---: | ---: | ---: | ---: |
| Previous same-kind election | 14.26 | — | 11.55 | — |
| Weighted polls alone | 3.21 | — | 3.48 | — |
| V1 historical/poll blend | 4.36 | 35% | 4.09 | 25% |
| Pooled bias/house correction | 4.70 | 45% | 4.07 | 50% |
| Additional error floor only | 4.36 | 70% | 4.09 | 87.5% |
| Bias plus error floor | 4.70 | 55% | 4.07 | 62.5% |
| Previous election + original national uncertainty | 14.26 | 65% | 11.55 | 75% |
| Weighted polls + original national uncertainty | 3.21 | 20% | 3.48 | 0% |
| Previous election + original uncertainty + fitted floor | 14.26 | 70% | 11.55 | 75% |
| Weighted polls + original uncertainty + fitted floor | 3.21 | 65% | 3.48 | 62.5% |

The last four rows were added in the later comparison extension, report
`poll-calibration-v2-d9116fb3de48`; the six original rows are unchanged. Original
uncertainty here means the simulator's national component (0.02 logit units per
square-root day), measured from the previous election date for historical
references and from the cutoff for weighted polls. This is the same national
surrogate boundary as the existing rows, not full municipal-generator coverage.
Additional floors are fitted separately for each benchmark using earlier events
only. See the extension entry below for structural-zero handling and interval
scores.

The uncertainty-only interval score improves from 42.76 to 22.59 percentage
points overall, and 36.21 to 16.96 for Camera. Its Camera metric check passes;
bias/combined candidates fail improvement requirements. Intervals use 4,000
deterministic joint logistic-normal draws per event/candidate: a compositional
national valid-share surrogate, not municipal/turnout/chamber generator or
simultaneous coverage. There are six independent events, not independent
party-survey trials. The current v1 intervals under-cover this panel.

Report `poll-calibration-v1-854b5ebc5454` replays deterministically. Acceptance
requires three Camera holdouts, improved interval score, >=80% empirical Camera
coverage and full-generator coverage validation; mean corrections also need
>=5% Camera MAE improvement, while uncertainty-only must preserve MAE. Only two
Camera holdouts and no full-generator validation are available, so corrections
remain withheld. Reference preparation writes the calibration report/receipt and
embeds rejection reasons while retaining v1 shares, proxies and computability.
The production generation/composed scrutiny passed for reference
`poll-reference-v1-f09e53578bd2`, scenario suffix `373d6c1df654`. Expand same-kind
evidence, revisit prior strength and validate full-generator intervals before
forecast acceptance. No calibrated forecast or default activation is claimed.

Verification on Windows/Node 24: 954 unit tests in 69 files passed, including seven
calibration tests for chronological leakage, whole-event pooling, source totals,
offline source-only copies, corruption rejection and receipt behavior.
Preparation/application checks passed with zero Svelte errors/warnings. Build
passed, followed sequentially by all 30 root-base Chromium tests (47.8 s),
including poll-v3 import/edit/reload/export. Official-result replay, calibration
generation and the production reference vertical slice passed. Chunk/plugin/color
notices were informational. A legacy non-UTF8 dash in the preceding history
heading was normalized while appending this entry. No commit, publication,
legal scrutiny change, default activation or persistent user-data change occurred.
