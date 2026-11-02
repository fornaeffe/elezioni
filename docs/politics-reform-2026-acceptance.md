# Reform production acceptance (R26-21)

Use this procedure for local application acceptance. The [application guide](politics-reform-2026-application.md)
defines worker/storage contracts; the [generation guide](politics-reform-2026-generation.md)
defines model evidence and inputs. Measured outcomes belong in [history](migration-history.md).
This does not enable deployment or retire the retained R workflows and caches.

## Prepare required inputs

From `web/`, use the [complete Node production pipeline](data-preparation.md),
including previous-law politics, Emilia-Romagna and all reform assets/defaults:

```powershell
npm ci
npm run data:prepare -- --download
npm run data:reform:validate -- --output ../data/derived/r26-21-acceptance
npm run acceptance:reform
```

Preparation writes local runtime bundles/defaults/catalogs without R or legacy
caches. Validation's optional
`--output` directory preserves older immutable evidence instead of overwriting it.
The acceptance command requires all three production bundles before running
`data:check` once (including reform checksums/scrutiny, workspaces/defaults and
share-release checks), both type checks and the complete unit suite.

| Build | Required browser coverage |
| --- | --- |
| `/elezioni` | Full general Chromium suite; four Chromium editor tests; full M01/M02 Chromium and Firefox workflows, including three paired draws with two compute workers, persisted results, indexed details and complete archive roundtrip |
| `/` | Direct routes/refresh/navigation, landing selector, previous-law lifecycle/import/cancellation, regional execution; one paired draw for each model in Chromium and Firefox |

Each path is built once, sequentially. Root is built last and remains available
for local preview. Numerical/statistical/editor behavior runs comprehensively at
the project path; root checks focus on URL-sensitive integration. All required
unit/browser reports reject skips, TODOs, expected failures and retries, including
the dedicated editor report. Ordinary development tests retain explicit artifact
skips. Missing production inputs remain fatal.
Stop other preview servers before acceptance. Reports and input SHA-256 values
are in `data/derived/r26-21-acceptance/`; preserve them before another run.

On hosts with many cores, set `$env:ACCEPTANCE_TEST_WORKERS='1'` (or another
integer from 1 to 64) to bound simultaneous Vitest and Playwright test processes.
This leaves the production compute-worker matrix and acceptance assertions
unchanged; it does not enable retries or skips. Without this setting, both test
runners keep their defaults.

## Performance and diagnostic frequencies

`test:results` and `benchmark:reform` are retired: they used the unsupported
pre-unified scenario/worker contract. Their old reports remain historical evidence.
Use the current unified workflow for production throughput, detail queries,
streamed JSON.gz import/re-export integrity and resource sampling:

```powershell
$env:FV08_FULL = '1'
$env:BASE_PATH = '/elezioni'
$env:FV08_CAMPAIGN_WORKERS = '1'
npm run test:reform:shares:unified -- --project=chromium
```

This separate campaign requests 1,000 paired runs per model and retains the
60-minute execution bound per model. `FV08_WORKERS` can select a supported compute
pool for a focused campaign; record the selected count. Do not run another build
or CPU-heavy task concurrently. Reset `FV08_FULL` and `BASE_PATH` after the campaign.
Use `test:reform:shares:resources` separately for repeated refits and dimension/
throttling probes; see [unified verification](vote-generation-unified-workflow.md#verification-commands).
These expensive campaigns are not part of routine application acceptance.

The current `reform-production-worker.spec.ts` retains the deterministic 1/2/4
worker comparison and 50-run chunk-boundary regressions against constructed
inputs. This is functional concurrency coverage, not a full-source throughput
matrix. Campaign receipts under `data/derived/fv08-unified/browser/` record
model/implementation/data identities, worker count, execution/phase timing,
sampled memory, legal seat totals and full archive-content digests. Default
functional runs omit unified memory instrumentation; `FV08_FULL=1` retains it.
Samples are not absolute peaks or process RAM guarantees. Overlapping worker
phase times are not additive elapsed time. Historical 10/100/1000 worker-matrix
timings must not be presented as current-model acceptance.

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
