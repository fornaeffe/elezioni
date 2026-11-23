## 2026-10-20: R26-21 production acceptance and diagnostic frequencies

Implemented the [local acceptance procedure](../../politics-reform-2026-acceptance.md).
At Luca's direction, R retirement is already decided: removed the active R/browser
comparison and 10x stop rule. Historical R measurements remain dated evidence.
The municipal migration, remaining preparation importers and final hosting/publication
gate remain separate and open.

- Added `npm run acceptance:reform`: requires the legacy politics, regional and
  reform production bundles, records their sizes/SHA-256, checks reform digests
  and its production vertical slice, then runs `check:data`, `check`, the full
  unit suite, root build/e2e and `/elezioni` build/e2e sequentially. Successful
  acceptance restores the root build. Missing bundles, unit/browser skips,
  TODOs, expected failures or retries cannot pass. Ordinary development skips
  remain explicit. Report-validator tests cover those rejection paths.
- Added direct-load, refresh and navigation browser checks. Existing browser
  suites exercise both politics algorithms, regional simulation, worker/data
  loading, candidate output, parallel execution, interruption, detail queries
  and archive replay. The constructed production-worker check also queries
  diagnostic frequencies across 51 simulation IDs without production fixtures.
- Added a streaming post-run `diagnostics` query over the saved presentation
  warning ledger. Errors, warnings and informational notes have separate groups;
  each exact code/message reason retains occurrence counts, distinct affected
  simulations, shared-context occurrences, requested/processed denominators and
  both frequencies. Duplicate notices within a run do not inflate its frequency.
  Distinct reasons can overlap. Session-wide notes have no affected simulation.
  Infrastructure failures remain manifest errors and fail benchmark completion;
  they are not converted into legal non-computability. Query/deletion takes
  place after the measured execution interval. No legal/model logic changed.
- Extended the production browser benchmark with those diagnostic groups,
  complete executed scenario/default identity, architecture and an optional
  report destination, preserving the earlier nine-case worker matrix.

Preparation and application verification:

- Regenerated both legacy production snapshots using the existing R exporters
  and regenerated politics defaults, municipality/candidate catalogs and regional
  defaults. The inherited `LC_ALL=C.UTF-8` is unsupported by Windows R and its
  first export corrupted accented college labels. Repeated with
  `LC_ALL=English_United States.utf8`; generated tracked defaults/catalogs matched
  their previous contents. No legacy cache was modified.
- Ran `data:download`, `data:generate`, `data:reform:download` and
  `data:reform:generate -- --publish`. All pinned cached originals verified.
  Regeneration preserved snapshot `r26-generation-v1-c5c4be9fafb5`, model
  `r26-models-v1-f71a72020aaa`, defaults `r26-defaults-v1-f204e831aad3` and census
  table `r26-seats-census-2021-i08-v1-9672c20529b2`. Compressed reform runtime:
  **992,943 bytes**, decoded JSON **13,870,007 bytes**. Held-out preference fit
  passed; no generated defaults were edited by hand.
- `data:reform:validate` completed its calculations but refused to replace an
  existing immutable validation-v2 report. Added an optional `--output` directory
  and reran into `data/derived/r26-21-acceptance/`, preserving the previous file.
  The new run passed **7 preference profiles × 100 complete legal fixtures** and
  **7 chamber profiles/benchmarks × 1000 replicates**, with conserved entitlements
  and no non-computable conditioning fixtures. Comparing the two reports found
  17 differing scalar summaries, all under conditional `candidateChanges`;
  other summaries matched. The earlier report is not asserted to reproduce the
  current implementation. The new report is separate evidence; no model, legal
  behavior or acceptance threshold was changed to match the older counts.
- `npm run acceptance:reform`: **PASS**, **856 unit tests**, **21 root browser
  tests** and **21 `/elezioni` browser tests**, with **zero skips**. Preparation
  and application type checks had zero diagnostics. Static builds and browser
  suites ran sequentially. Production reform simulation 1 completed with
  **41 stages and 588 filled domestic seats**. Detailed JSON reports and input
  digests are under `data/derived/r26-21-acceptance/`; the
  [compact acceptance record](../../../test/fixtures/benchmarks/reform_application_acceptance.json)
  preserves the passed checks, artifact identities and scope in the repository.

Source refresh on **20 October 2026**:

- Reopened the [controlling GU act](https://www.gazzettaufficiale.it/eli/id/2026/10/09/26G00201/sg),
  its operative article and the [Camera legislative record](https://www.camera.it/leg19/126?idDocumento=2822-B&leg=19).
  The correction labels had no linked correction; targeted official-source
  searches for `26G00201`, errata/rettifica and reform seat decrees found no
  subsequent applicable correction or decree table. This is a dated search,
  not proof that future corrections/decrees cannot appear.
- The law's [publication warning](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticoloDefault/originario?atto.codiceRedazionale=26G00201&atto.dataPubblicazioneGazzetta=2026-10-09&atto.tipoProvvedimento=LEGGE)
  still announces republication with notes on **23 October 2026**; entry into
  force is **24 October 2026**. Both dates are later than this acceptance.
  Retained the current legal transcription and explicitly provisional I08
  census-based allocation. Recheck that republication and any new decree before
  a later release. This acceptance does not claim official-decree support has
  been exercised, whole-chamber foreign-seat support, supplementary elections,
  nationally calibrated individual-candidate forecasts or universal computability.

Production performance on Windows x64 / Node 24.20.0 / Chromium 148.0.7778.96 /
Intel i7-14700KF / 63.79 GiB physical RAM, algorithm `politiche-reform-2026-v1`,
the regenerated versions above and seed `r26-default-v1`:

| Requested | Elapsed, 4 workers | Generation total | Scrutiny total | Presentation total | Storage/coordinator total | Complete | Non-computable |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 10 | 4.873 s | 5.275 s | 2.137 s | 0.042 s | 1.663 s | 8 | 2 |
| 100 | 22.970 s | 49.219 s | 21.064 s | 0.452 s | 6.429 s | 68 | 32 |
| 1000 | 197.351 s | 482.152 s | 199.361 s | 4.116 s | 47.237 s | 698 | 302 |

- Ran `benchmark:reform` with `REFORM_BENCHMARK_WORKERS=4` and
  `REFORM_BENCHMARK_REPORT=../test/fixtures/benchmarks/browser_reform_acceptance.json`:
  **all three sizes passed**, about 3.9 minutes including setup/teardown. This
  is a new four-worker acceptance sweep, not a repeat of the previous full
  nine-case matrix, which remains preserved in its original report. Browser
  regressions still prove complete normalized archive equivalence at 1/2/4
  workers. No R performance comparison or stop rule was applied.
- Controlled browser resource hints were 8 logical processors / 16 GiB,
  maximum 5 workers, selected/actual 4. Each case started a fresh coordinator
  and pool, including production loading/verification/preparation in elapsed time.
  Worker phase totals overlap and are neither additive elapsed time nor measured
  process CPU time. The largest periodic unforced combined worker JavaScript
  heap observation was **844.70 MiB**; the largest individual compute-worker
  observation was **255.81 MiB**. These are sampled maxima, not absolute peaks,
  retained-after-GC heaps or browser process RAM. The existing conservative
  1536 MiB/worker memory budget was retained.
- The [new benchmark report](../../../test/fixtures/benchmarks/browser_reform_acceptance.json)
  records every exact reason separately at **10, 100 and 1000** runs, alongside
  the executed scenario, phase timings and memory samples. At 1000 requested
  and processed runs, there were **1390 error occurrences in 302 simulations**
  and **2613 warning occurrences in 914 simulations**, including **two shared
  warning contexts**; informational context had **three occurrences**. There
  were **468 bonuses**. No run was rerolled or removed from the denominator.

Errors by exact stage/reason at 1000 requested runs (each listed occurrence is
in a distinct simulation for that row; downstream rows and chambers can overlap):

| Code/stage | Chamber | Reason | Occurrences / affected runs | Frequency / requested |
| --- | --- | --- | ---: | ---: |
| R26_N18_NON_COMPUTABLE | Camera | impossible-residual | 7 | 0.7% |
| R26_N18_NON_COMPUTABLE | Camera | no-eligible-donor-remainder | 151 | 15.1% |
| R26_N18_NON_COMPUTABLE | Camera | no-eligible-recipient-remainder | 1 | 0.1% |
| R26_N21_NON_COMPUTABLE | Senato | no-eligible-donor-remainder | 59 | 5.9% |
| R26_N22_NON_COMPUTABLE | Senato | alternative-allocation-path | 57 | 5.7% |
| R26_N23_NON_COMPUTABLE | Camera | ordinary-allocation-non-computable | 159 | 15.9% |
| R26_N23_NON_COMPUTABLE | Senato | ordinary-allocation-non-computable | 59 | 5.9% |
| R26_N24_NON_COMPUTABLE | Camera | ordinary-allocation-non-computable | 159 | 15.9% |
| R26_N24_NON_COMPUTABLE | Senato | ordinary-allocation-non-computable | 59 | 5.9% |
| R26_N25_NON_COMPUTABLE | Camera | prerequisite-non-computable | 159 | 15.9% |
| R26_N26_NON_COMPUTABLE | Camera | prerequisite-non-computable | 159 | 15.9% |
| R26_N27_NON_COMPUTABLE | Senato | prerequisite-non-computable | 59 | 5.9% |
| R26_N28_NON_COMPUTABLE | Joint | stage-prerequisite-non-computable | 216 | 21.6% |
| R26_RUN_ERROR | Generation/run | Supply a validated seven-slot slate and its college figures | 86 | 8.6% |

Warnings, independently counted:

- `I03_DRAW`: **690 occurrences**, **55 affected runs / 5.5%**.
- `I07_CROSS_REGION_RECOVERY`: **93 occurrences** across **42 distinct exact
  route/disposition reasons**. Every individual reason's occurrences, affected
  runs and frequency are recorded separately in the linked JSON; same-list,
  same-college and coalition recovery, retained versus released, and source/
  destination territories are not merged. Frequencies range from **0.1% to 0.5%**
  per exact reason. Their union must not be inferred by summing them.
- `I08_PROVISIONAL_SEATS`: **1828 occurrences**, **914 affected runs / 91.4%**.
  Both chambers can issue the notice in one run. Generation failures do not
  reach scrutiny and therefore do not emit it.
- `R26_CONDITIONAL_CANDIDATES` and `R26_SEAT_TABLE_3` (Rome boundary-continuity
  assumption): **one shared context occurrence each**, no simulation frequency.
  Informational seat/geography/model notes are recorded separately, not counted
  as warnings. These known model/legal boundaries remain visible in acceptance.

Final safeguards and completion:

- Temporarily moved only the three generated production bundles aside, with
  checked workspace paths and `finally` restoration. The reform suite passed
  **626 tests**, with exactly **one explicit production-bundle skip**. This
  demonstrates that constructed law/model fixtures do not require those files.
  The production acceptance command then failed at its required-data preflight,
  before any suite could turn missing data into an accepted skip. Restored all
  three bundles and reran the full no-skip application gate successfully.
- Diagnostic frequencies require a native per-simulation ledger. Imported
  archives may contain older all-run aggregation with only a first simulation
  ID; the query explicitly rejects them rather than inventing affected-run
  counts. Browser coverage verifies this rejection after archive import.
- R26-21 is complete as **local application/performance acceptance with
  provisional I08 census tables and conditional preference/chamber models**.
  The non-computable branches above are retained limitations, not successful
  allocations. The final hosting/publication, municipal and remaining
  data-preparation steps remain open. No commit, push or deployment was performed.
