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
  both chambers before the common scrutiny entry point decides the joint bonus.
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
The worker retains one generated/scrutinized run and bounded encoding buffers,
not the presentation details of every requested run. `ReformWorkspace.svelte`
queries selected pages and simulation ledgers through `core/result-service-client.ts`;
`workers/result-service.ts` owns execution, storage and query orchestration.

Publishing a manifest commits a complete run. Readers ignore chunks beyond its
published counters, including partially written runs after quota/write failure.
Cancellation finishes the current durable run. Cancellation, storage failure or
worker loss leaves an explicitly interrupted session with its saved-run count
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
