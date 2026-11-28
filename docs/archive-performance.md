# Archive transfer performance

Use this guide for profiling or changing result archive import/export. Format,
validation and staging rules remain in [common politics files](common-politics-scenario/files.md).
Measured outcomes belong in [migration history](history/README.md).

| Responsibility | Entry point under `web/` |
| --- | --- |
| Scalar row framing; general strict JSON reader | `src/lib/core/json-stream.ts` |
| UTF-8 output buffering | `src/lib/core/export-stream.ts` |
| Bounded IndexedDB prefetch | `src/lib/core/result-store.ts`: `storedRows` |
| Operation-owned decompression pool and fallback | `src/lib/core/archive-chunk-decoder.ts`, `src/lib/workers/archive-codec.worker.ts` |
| Transfer stages, request IDs and throttling | `src/lib/workers/result-service.ts`, `src/lib/core/result-service-client.ts` |
| Progress text and mounted status | `src/lib/core/archive-progress.ts`, `src/lib/politics/CommonPoliticsWorkspace.svelte` |
| Repeatable browser measurement | `scripts/acceptance/archive-performance.ts`, `tests/fixtures/archive-performance.worker.ts` |

## Bounds and ordering

Archive rows contain scalar cells. The reader frames a row within decoded input
chunks and parses it natively, retaining the 4096-cell limit, finite numbers,
strict UTF-8, JSON syntax and bounded records. Object fields still use the
duplicate-key-rejecting reader. Nested cells are rejected before native parsing.
Oversized rows fail explicitly; archives never become whole resident results.

Output coalesces strings to a 64 Ki-character target, plus one source record,
before one UTF-8 encoder and one gzip stream. CSV uses the same buffering.
Archive transfers and full result scans fetch at most 16 chunks per IndexedDB
transaction, reduced using the stored maximum raw chunk size to target 4 MiB.
Full result scans include stored diagnostics, filtered simulation-ID exports,
reform statistics and election probabilities. One permitted oversized record
can exceed the target. Ordinary table pages request one chunk. Unpublished
chunk sequences remain invisible.

For common archives with at least 2 MiB of stored compressed chunks and four
logical processors, an operation lazily starts two decompression workers. At
most one prefetch batch is outstanding. Replies retain chunk order; compressed
inputs remain available for serial retry after worker, transfer or ten-second
timeout failures. Workers close on success, failure or generator cancellation.
The pool receives chunk bytes only. Storage writes, membership checks and
publication remain ordered. The archive schema, checkpoint binding and electoral
algorithms are unchanged. Small archives and limited browsers use serial codecs.

Progress is keyed to the service request and throttled to ten updates/second
within a phase. Reading reports compressed input bytes and parsed rows;
membership verification and export report rows against known totals. Percentages
describe their phase. Checkpoint validation and final file closure are explicit
phases without an invented percentage. Completion follows durable saving or
OPFS closure and export registration.

## Measuring

Run sequentially from `web/` with installed Chrome. These commands launch an
isolated temporary browser context; they do not read or modify existing sessions.

```powershell
$env:ARCHIVE_BENCH_LABEL = 'baseline'
$env:ARCHIVE_BENCH_BASELINE = 'd21b119777b1ef4cc05f9110003f67b56debb591'
$env:ARCHIVE_BENCH_ROWS = '1000000'
npm run benchmark:archives
Remove-Item Env:ARCHIVE_BENCH_BASELINE
$env:ARCHIVE_BENCH_LABEL = 'current'
npm run benchmark:archives
```

Optional `ARCHIVE_BENCH_CODEC_WORKERS=0`, `2` or `4` builds an in-memory variant
for codec comparison without changing production code. Default is two.
The baseline revision supplies tracked source; generated untracked assets use
the checkout's current bytes. Receipts record the resolved revision.

Each run performs three import/export/reimport/reexport trials with a constructed
M01 archive and real workers, IndexedDB, membership validation, gzip and OPFS.
It checks SHA-256 of every decoded byte, normalizing only export time, and row
counts. Receipts go to `data/derived/archive-performance/<label>-<rows>.json`.
Use medians, keep workload/browser constant and avoid concurrent builds or tests.
Windows receipts sample renderer-wide private/working-set memory, including
nested workers, constructed input and verification buffers; these are sampled
whole-process observations, not hard bounds or production-only memory.

This benchmark isolates transfer performance. It does not replace a national
M02 checkpoint roundtrip or the [common-politics acceptance gate](common-politics-scenario/acceptance.md).
