## 2026-10-22 — Reform message priority and affected simulations

Implemented the approved three-level message presentation: general conditions
first, a severity/code frequency summary second, and affected simulation IDs
only after selecting a summary row. Global warnings remain expanded; information
notes use compact details. Execution failures and interrupted saved/requested
coverage have their own labels. Unknown scope never becomes an all-simulation
claim merely because no simulation ID was recorded. Code titles are Italian;
original wording, severity, references and context remain available.

The tested presentation reducer counts distinct simulation unions per code and
severity. Totals across codes are unions, not summed frequencies; occurrence
counts remain additive. Requested simulations remain the primary denominator,
with saved-run percentages secondary during interruption. Reason and chamber
filters use explicit saved evidence; they neither merge causes heuristically
nor change scrutiny or rerun simulations. Summary, general, ID and original
context pages contain at most 50 entries. ID CSV export streams the complete
filtered sequence, independent of the current page. IDs can select their result
chart or reveal the matching original messages.

The old warning deduplication discarded differing chamber/seat/draw context and
appended occurrence text to messages. New full-context deduplication preserves
those differences and stores counts separately. `Diagnostic events v1` carries
scope, membership, original reason and source context into schema-v2 archives.
Optional `completedSimulations` preserves zero-run interruption coverage; import
validates it against any saved run ledger. Reading global/execution messages
from canonical chunks avoids the manifest's 20-information-note preview limit.

Older native run-indexed ledgers still establish frequencies and IDs, with an
explicit unavailable-context explanation and disabled chamber filter. Older
imported first-ID aggregates expose occurrences and original texts while marking
exact frequencies/IDs unavailable. New versioned imports retain exact diagnostic
membership even with rows before column labels; the existing diagnostic API now
recognizes this evidence. Its earlier blanket-import-rejection browser assertion
was updated to require equality with native frequencies for new archives and
continued rejection of legacy aggregates. No algorithm, legal interpretation or
model choice changed. The focused reading/code map is in
[`politics-reform-2026-messages.md`](../../politics-reform-2026-messages.md).

Queries stream committed rows and retain per-code counters and bounded pages,
without an all-ID/all-reason collection or new diagnostic cache. Owner checks
and deletion waits cover message queries and exports. Temporary export files
are registered atomically so simultaneous ID/full-result exports cannot lose
each other's cleanup entries.

Verification: `npm run acceptance:reform` passed with 925 unit tests and 29 browser
tests at each of `/` and `/elezioni`, zero skips/retries. Production checksums,
the vertical slice, preparation/application types (zero errors/warnings), static
builds and root-build restoration passed. The new browser fixture exercises
1,201 affected simulations, overlapping severities/codes, exact context, complete
ID CSV, interruption including zero saved runs, notes beyond the preview limit,
mobile layout and old-archive limitations. The Svelte checker reported no issues;
its general effect suggestions were reviewed: these effects synchronize external
worker queries with cleanup guards. Concurrent export registration also passed
the IndexedDB browser check. Evidence is under ignored
`data/derived/r26-21-acceptance/messages-acceptance.log` and `report.json`.

The separate `npm run test:results` check passed on 1,000 fresh production
simulations, including the bounded frequency/ID UI, a complete CSV containing
1,000 unique ascending affected IDs, chart/detail regressions and unchanged
message frequencies after import. Full gzip import/re-export retained the same
decoded-content hash across 2,970,611,188 bytes (128,674,527 compressed bytes).
Computation took 116.126 s, initial export 81.394 s and import 98.496 s on this
Windows/Chromium checkout. Sampled retained JS heap peaked at 11.16 MiB in the
UI and 20.57 MiB in the coordinator, below the existing 32/256 MiB limits; these
are retained heaps, not peak process RAM. There were no page errors. The report
and archive are retained under
`data/derived/r26-21-acceptance/messages-1000.json[.gz]`; the decoded-content
SHA-256 is `2b20cfa643d247957a3d695ba3a13919c7a5d1a364a9944eddb219c14af69981`.

Final boundary review added an empty-table guard so zero-run interruption does
not issue an invalid table-page request. Explicit saved coverage of zero proves
an empty per-simulation ledger even before the first table exists; it does not
classify unprocessed simulations as message-free. The regression checks this
case alongside the zero-count gzip export.

The final acceptance replay passed with the same 925 unit tests and 29 browser
tests at each base, including the zero-run guard. The root build was restored
and `git diff --check` passed. Build chunk-size/plugin-timing notices remain
informational. No commit, deployment or publication was performed.
