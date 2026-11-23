## 2026-10-22 — Reform result tables and plots alignment

Implemented the approved alignment at `MIGRATION_PLAN.md:209`. Reform results
now offer paired mean-elected/valid-vote bars and summary tables, percentage
boxplots, chamber/list and chamber/supporting-subject scatter panels, national
list spinograms, and a searchable college spinogram with final-election and
original-entitlement measures. The pre-2026 adapter uses the extracted shared
renderer and colour helpers; its existing visible charts remain available.
Broader routing/election-kind generalization remains a separate plan item.

New presentation rows retain final personal affiliation and compact national
and college observations before generated inputs/stage objects are released.
List counts include every domestic seat category by final affiliation; coalition
counts preserve legal ownership and never invent member-list bonus quotas.
Independent special winners have an explicit unaffiliated bucket. Local counts
use actual election locations, including bonus recoveries, while original
entitlements remain a separately labelled measure. No legal algorithm, model,
RNG stream, generated snapshot/default or production data was changed.

Per-chamber unknowns remain null. Complete means require full requested
coverage; confirmed elected totals divided by requested simulations are labelled
lower bounds otherwise. Boxplots describe available percentages, scatter panels
report missing pairs, and spinograms retain grey unknown-outcome cells plus
missing-percentage counts. Preferences and candidate outcomes remain conditional.
Calculations and quantile/bin boundaries preserve precision; labels alone round.

The worker builds disposable on-disk series/value indexes and supplies at most
256 observations per request. Canvas draws every available scatter/outlier point
without one DOM node per observation. Panels mount when opened and query near
the viewport; local series indexes are built only when selected. Exact type-7
quantiles use indexed ranks. Canonical summaries need only subject counters,
so cache quota exhaustion cannot interrupt an otherwise complete election.
Owner checks, cache invalidation, failed-build cleanup and deletion after active
queries protect canonical sessions and prevent surviving derived samples.

Schema-v2 JSON.gz/CSV stays generic and streaming. Canonical presentation tables
include capabilities, observations and list summaries; derivative caches never
alter archive content. Older reform archives use only saved evidence, explicitly
limit non-reconstructible affiliations/local percentages, and never rerun draws.
The operational definitions, compatibility limits and scoped code/check map live
in [`politics-reform-2026-results.md`](../../politics-reform-2026-results.md).

Verification: `npm run acceptance:reform` passed with 915 unit tests and 28 browser
tests at each of `/` and `/elezioni`, zero skips/retries. Production checksums
and the 41-stage/588-filled-seat vertical slice, preparation/application types
(zero errors/warnings), static builds and root-build restoration passed.
The new 1,201-observation browser fixture verifies exact disk quantiles, unknown
outcomes, bounded pages, every chart family, enlargement, local measure/search,
old archives, ownership, cache-quota isolation, rebuild and scoped cleanup.
An asynchronous import/index needs an explicit readiness wait in concurrent
browser suites; exact select lookup uses the combobox role because nested option
text affects `getByLabel`. A precision assertion uses the original arithmetic
value rather than rounding the stored observation to a decimal literal.

The separate `npm run test:results` production check passed: 1,000 stored
simulations, all 1,000 observations drawn in the checked scatter panel, boxplot
and spinogram/enlargement queries, paginated legal detail, and complete streamed
gzip import/re-export with the same content hash. Computation took 114.892 s,
the exercised chart queries 11.830 s, initial export 79.475 s and import 76.728 s
on this Windows/Chromium checkout. The archive contains 2,969,183,961 decoded
bytes / 128,631,873 compressed bytes. Sampled post-GC retained heap peaked at
11.03 MiB in the UI and 20.47 MiB in the coordinator worker, below the existing
32/256 MiB acceptance limits. These are retained JS heaps, not peak process RAM.
There were no page errors. The report and compressed archive are retained under
`data/derived/r26-21-acceptance/results-alignment-1000.json[.gz]`; the archive's
decoded-content SHA-256 is
`2ce0f5db46b706b808eeae935465c652dfce6b45abeeb1f259f0d8977c3c560c`.

The college search/select is constrained to its container for long list/college
labels on mobile; a 390-pixel browser assertion checks its right edge and the
statistics container's overflow. The final acceptance replay passed with the
same 915 unit tests and 28 browser tests at each base, now including these mobile
assertions. The root build was restored and `git diff --check` passed. Build
chunk-size/plugin-timing notices remain informational. No commit, deployment or
publication was performed.
