# Politics algorithms, worker and results (R26-20)

Use this guide for application integration. For preparation, schema versions,
model fitting, constructed inputs and explicit legacy conversion, use the
[generation guide](politics-reform-2026-generation.md). For legal logic use the
[core reading map](../web/src/lib/politics/reform-2026/README.md).

## Application behavior

- `/politics` defaults to **Legge precedente (R parity)**. Selecting **Riforma
  2026** opens a separate scenario/workspace with its own local storage. Existing
  legacy plots and tables retain their visibility. This selector does not compare
  algorithms or convert scenarios.
- Reform controls edit dates, seed, model profiles and national common/per-chamber
  shares. The complete scenario editor/import exposes coalition/filing choices,
  geographic references, candidates, supplied votes/preferences and conversion
  provenance. Shares in files are fractions; share inputs display percentages.
- Missing production data fails explicitly. The legacy worker no longer falls
  back to debug snapshots. Reform data must match the pinned compressed/decoded
  digests, schema and scenario snapshot/model versions.
- Reform execution uses consistent global simulation IDs. Each ID generates
  its requested chamber(s) before the common scrutiny entry point decides the bonus.
  Presentation rows are saved and released after each run. Non-computable runs
  remain in the requested denominator; no reroll, legal-endowment rescaling or
  successful-run renormalization occurs.
- Reform has no arbitrary run cap. Its new viewer mounts one selected table,
  at most 50 detail rows, one selected simulation/chamber chart and at most ten
  warnings at once. Worker messages contain a compact manifest or the requested
  page/run, never the full detail collection. Stored/exported detail grows on
  disk with requested runs; general performance acceptance belongs to R26-21.
- Legacy politics retains its 1,000-run limit because its unchanged simultaneous
  scatter plots mount an SVG point per subject/run. Requests above the limit
  fail explicitly rather than silently truncating. Removing this restriction
  needs a rendering change that preserves the existing visible charts. Regional
  aggregate charts have no analogous run cap.

## Result and export contracts

`result-presentation.ts` owns reform aggregation and chart derivation. Tables
separate vote entitlements, cap effects, territorial targets, seat status,
candidate identities, original entitlement and actual election locations.
Coalition bonus ownership stays distinct from ordinary list seats and candidate
affiliations. Candidate conclusions retain missing/supplied/generated preference
conditions. Model/source/profile versions, conversion provenance, resolved
common/chamber/geographic references, census/decree notes and applied
interpretations remain inspectable. Profile sensitivity is conditional on the
selected profile pair, not calibrated uncertainty or a cross-law comparison.

Structured exports use the generic schema-v2 columnar module and include the
**executed** scenario, replay seed, algorithm/model/data identities and complete
worker presentation result. Editing a draft does not alter exported results.
Imports validate reform algorithm/seed/data agreement; old politics exports
without an algorithm ID explicitly resolve to `politiche-r-parity-v1`. Both
workspaces route imported results to their matching law. Reform scenarios are
never interpreted by the old scenario parser. CSV remains table-oriented.
Generated municipal inputs and composed legal stage objects are released after
each run; inspect the legal core directly when full internal stages are needed.

## Local result storage and large archives

`core/result-store.ts` stores per-run presentation batches in IndexedDB, using
compressed chunks targeted at 256 KiB or 256 rows. A larger individual record
occupies its own chunk; records above 16 MiB fail explicitly. Shared model,
source and reference metadata is stored once with occurrence-count vectors.
Each compute worker retains one generated/scrutinized run and bounded encoding
buffers, not the presentation details of every requested run.
`ReformWorkspace.svelte` queries selected pages and simulation ledgers through
`core/result-service-client.ts`;
`workers/result-service.ts` owns execution, storage and query orchestration.

Publishing a manifest commits a complete run. Readers ignore chunks beyond its
published counters, including partially written runs after quota/write failure.
Cancellation stops new dispatch and finishes the bounded prefix already assigned
to compute workers. Cancellation, storage failure or worker loss leaves an
explicitly interrupted session with its saved-run count
and the original requested denominator; it never retries legal draws. Reloading
the same tab restores its locally recorded sessions. The owner token and session
list live in session storage; this is local recovery, not cross-device storage.
Use **Elimina risultato locale** to delete only the selected session and its
temporary exports. Browser storage eviction can still remove local data.
The displayed compressed-chunk size excludes metadata, indexes and export files.

Structured JSON.gz and CSV exports stream from stored rows to temporary files
in the origin-private file system (OPFS). A disk-backed `File` crosses the worker
boundary; no full-result clone, whole-file JSON string or giant byte buffer is
created. Files remain until explicit session deletion so downloads stay valid.
Large exports require OPFS support; an unsupported browser reports an error and
retains the local result. `core/result-export.ts` keeps the existing schema-v2
columnar format and strict scenario/algorithm/seed/data checks. Imports of both
plain JSON and JSON.gz parse rows incrementally through `core/json-stream.ts`
and build a new staging session. Invalid imports delete only that staging
session. Legacy result imports keep their existing route and semantics.

## Parallel execution

`ReformWorkspace.svelte` offers **Processi di calcolo** from 1 to the estimated
maximum, initially selecting that maximum. A chosen value is remembered locally
after execution and clamped to the current maximum on reload. This is an
execution preference, outside the scenario, model and RNG identity. Runs shorter
than the chosen pool size start only as many compute workers as simulations.

`core/reform-worker-capacity.ts` owns the versioned resource policy. Budget
**1,536 MiB per compute worker** and **512 MiB for the coordinator**. Use half
the reported device RAM, reserving the other half for the system and other tabs:

```text
cpuMaximum = max(1, floor(hardwareConcurrency) - 1)
memoryMaximum = max(1, floor((deviceMemoryGiB * 1024 / 2 - 512) / 1536))
maximum = min(cpuMaximum, memoryMaximum)
```

Missing/invalid hints use two logical CPUs and 2 GiB, giving one compute worker.
The one-worker minimum permits execution; it is not a guarantee that a very
small device can run the model. Both browser hints are approximate rather than
free-memory measurements. `deviceMemory` has limited browser availability and
may be privacy-coarsened; do not treat an absent hint as abundant RAM. See
[device memory](https://developer.mozilla.org/en-US/docs/Web/API/Navigator/deviceMemory)
and [logical processors](https://developer.mozilla.org/en-US/docs/Web/API/Navigator/hardwareConcurrency).
The UI explains the estimate. The coordinator independently validates the chosen
integer against the same policy; it never silently reduces an excessive request.

The initial 1.5 GiB budget is deliberately above retained-heap measurements.
A Windows/Node 24.20.0 production-data probe of simulations 1–20, sampling after
generation, scrutiny, presentation and whole-batch encoding without forced GC,
observed 929,540,912 bytes of used heap and 1,147,371,520 bytes of process RSS.
It retained both decoded and prepared snapshots and included Node/loader costs.
These stage samples are neither absolute peaks nor browser process measurements.
The budget includes transient work and one pending presentation run; browser
sampling in the reform benchmark provides separate evidence. Revisit the budget
when data/model shape or observed transient memory changes, updating its version.

`workers/reform-compute.ts` executes one complete simulation, using the unchanged
generator and common scrutiny registry. Each ID keeps jointly renewed Camera and
Senato together; single-chamber renewals remain single.
Simulation 1 emits shared catalogue/context notes once; all tasks emit the same
per-run rows, warnings and aggregate counters as serial execution. The presenter
keeps legal/result aggregation out of worker orchestration.

`workers/reform-pool.ts` permits one outstanding simulation per compute worker.
Results may finish out of order, but the single result-service coordinator drains
and commits them in ascending ID order. A slot is reused only after its durable
commit, so a slow run cannot create an unbounded queue. Compute workers retain
their own verified/prepared data and one pending run. Pull requests transfer one
columnar byte buffer at a time, targeted at 256 KiB/256 rows, with the existing
16 MiB individual-record guard. The coordinator performs compression and writes
through the existing chunk store; UI messages remain compact manifests/pages.

Cancellation stops dispatch and finishes the already assigned contiguous prefix.
Compute initialization, native worker, encoding and messaging failures interrupt
the session with `RESULT_EXECUTION_ERROR`; storage failures use
`RESULT_STORAGE_ERROR`. Unpublished chunks stay invisible. Legal/model failures
keep their per-run representation and requested denominator. Neither failure
class triggers a retry, seed change or reroll. Pools terminate on completion,
cancellation or failure; descendants also terminate when their coordinator closes.

Archives preserve optional execution metadata: chosen/actual/maximum worker
counts, memory-estimate version and per-worker budget, accumulated generation/
scrutiny/presentation phase durations, and coordinator storage time. Phase
durations use `performance.now()` within each worker, can overlap and must not
be summed as end-to-end elapsed time or interpreted as measured process CPU time.
Older archives remain valid.
Normalized legal/result content is invariant across worker counts; timestamps,
timings and resource metadata are deliberately execution-specific.

Run `npm run benchmark:reform` for the production 1/2/4-worker × 10/100/1000-run
matrix. Required production data must exist; this check never skips. Optional
`REFORM_BENCHMARK_WORKERS` and `REFORM_BENCHMARK_COUNTS` are comma-separated
subsets for focused runs. The test uses explicitly recorded controlled resource
hints so the four-worker case can be exercised independently of browser privacy
limits. It records accumulated phase timings, outcome/row-count invariance and
periodic unforced Chromium worker-heap samples in
`test/fixtures/benchmarks/browser_reform_workers.json`. Samples are not absolute
peaks or process RAM; this benchmark does not replace R26-21 legal/application
acceptance or the old-law R comparison. Run it without another build or CPU-heavy
check in parallel.

## URLs, loading and verification

Set `BASE_PATH=/elezioni` for a project-base static build (omit it for root).
Routes obtain SvelteKit's base and pass it to workers; workers must not import
`$app/paths`, which pulls unsupported client modules into their Vite bundle.
`core/runtime-data.ts` resolves data URLs and decodes verified gzip JSON. When
HTTP `Content-Encoding: gzip` makes Fetch decode the wire representation, verify
the decoded digest directly. For a raw gzip asset verify both digests. Neither
path accepts unchecked JSON or a debug fallback.

Focused checks: `core/runtime-data.test.ts`, `reform-2026/result-presentation.test.ts`,
`core/result-export.test.ts`, `core/json-stream.test.ts`,
`politics/simulation-scrutiny.test.ts` and the `reform-workspace`,
`reform-production-worker` and `result-store` browser specs. The latter compiles
the actual production worker against a test-only catalog and constructed bundle,
covering chunk-boundary IDs without large production artifacts. Production UI
tests skip explicitly when their generated bundle is absent; release acceptance
must require it. The browser regression suite accepts `BASE_PATH` for root and
project-base runs. Run builds and browser tests sequentially.

Run `npm run test:results` separately for the long 1000-run Chromium acceptance
check with required production data. It measures post-GC UI/worker heaps,
queries details, exports the complete archive, imports it and compares the
re-exported content hash. Its isolated temporary browser profile is removed at
the end. Reports and downloaded archives are written under `web/test-results/`;
keep any evidence needed before another Playwright run clears that directory.
Heap samples are retained JavaScript heap measurements, not peak process memory
or a full R26-21 performance benchmark. Do not run another build during this test.
