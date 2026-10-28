# Reform production acceptance (R26-21)

Use this procedure for local application acceptance. The [application guide](politics-reform-2026-application.md)
defines worker/storage contracts; the [generation guide](politics-reform-2026-generation.md)
defines model evidence and inputs. Measured outcomes belong in [history](migration-history.md).
This does not enable deployment or complete the municipal/data-preparation migration.

## Prepare required inputs

From the repository root, regenerate the existing legacy bridge artifacts and
their defaults/catalogs. These exporters still require R and `dati/dati.RData`;
replacing those dependencies belongs to the remaining preparation migration.
Use an installed `Rscript` (its absolute path is also valid):

```powershell
Rscript scripts/export_politics_static_snapshot.R
node scripts/export_politics_scenario_defaults.mjs
node scripts/export_politics_municipality_catalog.mjs
node scripts/export_politics_candidate_slots.mjs
Rscript scripts/export_regional_er_static_snapshot.R
node scripts/export_regional_er_scenario_defaults.mjs
```

On Windows use a valid UTF-8 R locale. If the environment supplies an unsupported
`LC_ALL=C.UTF-8`, set `$env:LC_ALL='English_United States.utf8'` before exporting;
otherwise accented geography labels can be corrupted during RDS conversion.

Then from `web/`, acquire pinned sources and regenerate reform production inputs:

```powershell
npm run data:download
npm run data:generate
npm run data:reform:download
npm run data:reform:generate -- --publish
npm run data:reform:validate -- --output ../data/derived/r26-21-acceptance
npm run acceptance:reform
```

`--publish` writes local runtime bundles/defaults/catalogs. Validation's optional
`--output` directory preserves older immutable evidence instead of overwriting it.
The acceptance command requires all three production bundles before running
the checksum/vertical-slice check, `check:data`, `check`, full unit suite, root
build/e2e suite, and `/elezioni` build/e2e suite, sequentially. It restores a root
build after success. Unit/browser skips, TODOs, expected failures and retries
cannot pass this gate. Ordinary development tests retain explicit artifact skips.
Stop other preview servers before acceptance. Reports and input SHA-256 values
are in `data/derived/r26-21-acceptance/`; preserve them before another run.

On hosts with many cores, set `$env:ACCEPTANCE_TEST_WORKERS='1'` (or another
integer from 1 to 64) to bound simultaneous Vitest and Playwright test processes.
This leaves the production compute-worker matrix and acceptance assertions
unchanged; it does not enable retries or skips. Without this setting, both test
runners keep their defaults.

## Performance and diagnostic frequencies

From `web/`, run `npm run benchmark:reform` for the production 1/2/4-worker
matrix at 10/100/1000 requested simulations. Run without another build or
CPU-heavy task. `REFORM_BENCHMARK_WORKERS` and `REFORM_BENCHMARK_COUNTS` select
focused subsets; record the subset used. The report is
`test/fixtures/benchmarks/browser_reform_workers.json`.
Set `REFORM_BENCHMARK_REPORT` to a different output file to preserve an earlier
matrix when recording a new acceptance or focused measurement.

Each case records algorithm/data/model versions, seed, environment, worker policy,
elapsed time, accumulated generation/scrutiny/presentation/storage durations and
sampled worker heaps. Overlapping phase times are not additive elapsed time.
Periodic unforced heap maxima are not absolute peaks or process RAM. R retirement
is decided; no R comparison or 10x stop rule applies. Historical timings remain
dated evidence.

The result-service `diagnostics` query streams the saved **Warnings and errors**
ledger after execution. It requires a native per-simulation ledger; imported
archives are rejected because older aggregations retain only the first simulation
ID and cannot establish distinct-run frequencies. Reports separate `errors`, `warnings` and `information`.
Each exact `(code, message)` reason records occurrence count, distinct affected
simulations, context occurrences, and frequencies per requested/processed run.
The additional `codes` summaries merge reasons while counting each simulation
once per code; distinct-run counts must not be obtained by summing reason counts.
Repeated notices within one simulation count once toward its frequency, while
retaining all occurrences. Different reasons can overlap in the same run, so
their frequencies need not sum to the total failure rate. Context notices have
no simulation ID and contribute no simulation frequency. No message normalization
merges distinct reasons. Session execution/storage failures remain in manifest
warnings and fail benchmark completion; they are not legal per-run failures.
The post-run query/delete interval is outside measured execution latency.

## Acceptance scope and source refresh

Acceptance covers domestic allocation and candidate proclamations, conditional
on supplied/generated preferences and the chosen chamber profile. Synthetic
candidate identities are not individual forecasts; sensitivity profiles are not
calibrated uncertainty intervals. Non-computable runs remain in the original
requested denominator, with no reroll. Acceptance does not assert every draw is
computable or extend the scope to foreign seats or later supplementary elections.

Recheck [the controlling publication and correction links](https://www.gazzettaufficiale.it/eli/id/2026/10/09/26G00201/sg),
the [legislative record](https://www.camera.it/leg19/126?idDocumento=2822-B)
and official election-specific seat decrees before a new acceptance. Record the
date, findings and any source changes in history. I08 census tables support
explicitly provisional acceptance only; an official decree requires a new
verified table version. Recheck the announced 23 October 2026 republication when
available; it is later than this acceptance date.
