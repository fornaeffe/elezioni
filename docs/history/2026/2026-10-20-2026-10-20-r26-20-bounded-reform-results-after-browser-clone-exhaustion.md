## 2026-10-20: R26-20 bounded reform results after browser clone exhaustion

The reported 1000-run `DedicatedWorkerGlobalScope.postMessage` failure came from
cloning the complete accumulated presentation result. Pagination bounded the DOM
but did not bound worker retention or structured-clone cost. A production-default
50-run diagnostic contained 112,553 presentation rows and about 121.8 MiB of row
JSON, including 77.5 MiB of candidate-recovery traces. Export/import also created
whole-result copies and whole-file strings. The failure did not justify changing
legal calculations, discarding traces or imposing a reform run-count cap.

- Added IndexedDB result sessions and gzip row chunks, targeted at 256 KiB or
  256 rows with an explicit 16 MiB individual-record guard. The reform worker
  persists and releases each run; compact manifests and selected page/run queries
  replace the full-result worker message. Shared context is stored once with
  occurrence-count vectors. No existing plot/table visibility was reduced.
- Run manifests act as commit markers. Partially written chunks remain invisible
  after quota/write failure. Cancellation and a closed worker preserve published
  runs as interrupted sessions, retaining the original denominator and global
  simulation IDs. Recovery never rerolls, rescales endowments or changes a legal
  failure into a successful run. The existing seven-slot-slate validation failure
  in production-default simulation 19 remains an explicit non-computable run.
- Added same-tab session restoration and owner-scoped deletion. JSON.gz/CSV
  exports stream stored rows into OPFS files and send a disk-backed File to the
  UI; downloads retain their temporary source until session deletion. OPFS is
  required for this large-file export path. Compressed-chunk bytes shown by the
  UI exclude metadata, indexes and temporary export files.
- Plain/gzip schema-v2 imports parse incrementally into isolated staging
  sessions. Existing identity validation and legacy routing remain in force.
  Invalid imports clean only their staging session; arbitrary table-field order,
  split UTF-8, escaping and column widths are covered. The synchronous generic
  export API rejects storage references to prevent accidental full-result copies.
- Initial per-run metadata lookups were unnecessarily slow. A calculation-scoped
  dictionary of distinct shared keys plus persisted count vectors removes those
  repeated IndexedDB lookups without retaining simulation details. Scrutiny and
  generation algorithms, RNG streams and source/generated data were unchanged.
- Added `npm run test:results` as a separate long Chromium check using actual
  production bundles, detailed queries, full gzip export/import/re-export and
  post-GC UI/worker heap samples. Its temporary persistent profile is isolated
  from ordinary browser tests. R26-21 remains open: these samples do not measure
  peak process memory or replace its generation/scrutiny/presentation benchmarks.

Verification on Windows x64 / Node 24.20.0 / Chromium 148.0.7778.96, with all
required existing production artifacts available:

- `npm run check:data` and `npm run check`: zero diagnostics.
- `npm run test`: **835 tests passed in 55 files**, no skips, **7.95 s**.
  Browser storage tests additionally cover real IndexedDB, cross-chunk pages,
  shared metadata counts, quota isolation, malformed staging cleanup and
  out-of-order JSON fields. Constructed production-worker tests cover seeded
  replay, IDs 1/50/51, cancellation and cold recovery without large artifacts.
- Processed and saved **1000/1000** production-default runs through the UI in
  **714.344 s** of worker time (11 min 54 s). Outcomes were **698 complete,
  302 non-computable**, with 468 bonuses and the unchanged requested denominator
  of 1000. These existing legal/model execution paths remain visible; this work
  does not establish that every requested scrutiny succeeds.
- Post-GC calculation samples at 100 through 900 runs showed **4.70–4.85 MiB**
  retained UI JavaScript heap and **47.88–76.91 MiB** retained worker heap, with
  no sustained increase. After archive roundtrip the sample was **6.32 MiB UI /
  0.94 MiB worker**. These are retained-heap samples, not peak browser/process RAM.
- Full export contained **2,144,543,961 uncompressed bytes**, compressed to
  **83,637,510 bytes**. Export took about **53 s**. Import/re-export preserved
  every byte after export-timestamp normalization; SHA-256:
  `f7537fe10b347988b98cd18f22f477f39e3fb3cfb9483092e1a0bc438ecc353d`.
  The complete archive passed another import/export cycle and detail queries.
  No browser page errors occurred.
- The first long test used `innerText` as an expected `textContent` assertion,
  causing a formatting-only failure after successful calculation/export/import.
  Corrected it to wait for changed rows and compare the same text representation
  by hash. Verification resumed from the saved archive without rerunning draws;
  both complete hashes matched. A stale `imported` status made one import timer
  invalid; its duration is omitted, and the test now waits for a new session ID.
  The [small evidence report](../../../test/fixtures/benchmarks/browser_reform_results_1000.json)
  records versions, seed, counts, samples, hashes and these measurement limits.
- Full root Chromium regression: **15 passed**, no skips, **22.8 s**. Project-base
  `/elezioni/` regression: **5 passed**, no skips, **15.9 s**, covering both
  politics laws, the completed regional workflow and navigation. Static builds
  and browser suites ran sequentially. Restored the default root build afterward.
- Svelte MCP reported no component issues; asynchronous worker queries account
  for its effect suggestions. Type checks are clean. Visually inspected the
  workspace, including the local-session controls and unchanged chart/table scope.
- Final repeated-import/session-change regression: **2 focused workspace tests
  passed, 16.0 s**, after correcting the test selector's accessible-name match.
- `git diff --check` passed. R26-21 and the whole-migration publication gate remain
  open. No commit, source/data regeneration or deployment was performed.
