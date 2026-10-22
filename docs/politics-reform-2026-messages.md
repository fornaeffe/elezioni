# Reform result messages

Use this guide for message presentation, frequency counting and saved-message
compatibility. For charts use the [results guide](politics-reform-2026-results.md);
for execution/recovery use the [application guide](politics-reform-2026-application.md).

## Reading order and controls

1. **Informazioni e avvisi generali** shows conditions applicable to every
   simulation once. Errors and warnings precede compact informational details.
   Codes, law references and original wording remain accessible. Execution
   failures are labelled separately, with saved/requested coverage when
   interrupted. A message without a simulation ID has no inferred global scope.
2. **Messaggi delle simulazioni** groups by severity and diagnostic code.
   The main count is distinct affected simulations / requested simulations,
   followed by its percentage. Occurrences count repeated events separately.
   Rows sort by error, warning, information, then affected frequency. Severity,
   chamber and original-text search filters apply when submitted. Severity
   totals are ID unions across codes, rather than sums of overlapping rows.
3. Selecting a frequency opens **Simulazioni interessate**. Ascending unique
   IDs have pages of at most 50, an exact ID search and an original-reason text
   filter. Chamber filtering is inherited from the summary. Each ID can reveal
   its original matching messages and structured context, or select its result
   chart. The CSV button streams every filtered ID, including other pages.

All three lists and original-context pages are bounded to 50 entries. A message
appearing in every processed simulation remains a simulation message, rather
than becoming a global condition. For interruption the primary denominator
remains the original request; a secondary percentage reports saved simulations.
Unprocessed simulations are never considered message-free. Filters do not
reclassify severity, infer causality, merge reasons or rerun scrutiny. Original
reason fields distinguish contexts only when explicitly supplied by the source.

## Storage and archive contract

New results append `Diagnostic events v1`, with explicit scope, simulation ID,
code, severity, original text, occurrences, chamber, reason, law/TODO reference,
interpretation, seat and draw context. Identical full-context events within a
run may share an occurrence count; different contexts remain separate. No
occurrence suffix is added to the original message text. The existing warning
ledger remains for compatible diagnostic exports and consumers.

The schema-v2 version is unchanged. Optional result `completedSimulations`
preserves saved coverage even when interruption precedes the first run table;
explicit zero coverage proves an empty simulation ledger. When present the count
must agree with any run table. Old archives without the field
remain readable and are not given new header fields on re-export.
Optional warning fields `scope`, `occurrences` and `reason` survive import/export.
The versioned event table
proves membership after import, including JSON with rows before column labels.
Old native sessions can establish frequencies from their run-indexed warning
ledger; their lost chamber/seat/draw context cannot be recovered. Missing structured
context disables the chamber filter with an explicit explanation. Old imported
aggregates containing only a first simulation ID expose occurrence counts and
explicitly unavailable frequencies/IDs, with saved original texts still accessible.
No membership is fabricated. Missing scope is classified globally only for
the established reform condition codes;
other messages with no ID are labelled as having undocumented scope.

Queries read only durably published chunks, including raw execution messages
outside the manifest's short information preview. Summaries retain counters
per severity/code, never all distinct messages or simulation IDs. ID/context
queries scan the ledger with bounded pages; complete ID exports stream to OPFS.
There is no additional diagnostic cache or database migration. Queries validate
simulation ordering and coverage before presenting exact memberships. Ownership
checks and deletion waits cover message queries and exports. Concurrent exports
register their temporary files atomically for eventual session cleanup.

## Task-oriented code and checks

| Task | Entry points |
|---|---|
| Scope, titles, distinct counting, filters | `web/src/lib/core/diagnostic-messages.ts` |
| Bounded archived queries and ID iteration | `web/src/lib/core/stored-diagnostics.ts` |
| Event creation and context preservation | `web/src/lib/politics/reform-2026/result-presentation.ts` |
| Three-level message UI | `web/src/lib/results/DiagnosticMessages.svelte` |
| Requests, exports, ownership, cleanup | `web/src/lib/workers/result-service.ts`, `core/result-store.ts`, `core/result-export.ts` |

Unit checks: `core/diagnostic-messages.test.ts`, `core/stored-diagnostics.test.ts`,
`reform-2026/result-presentation.test.ts`, `workers/reform-compute.test.ts`.
Browser check: `reform-messages.spec.ts` covers 1,201 affected IDs, overlapping
severities/codes, preserved contexts, interruption, global notes beyond the
preview limit, complete CSV, mobile layout and old aggregate archives.
`result-store.spec.ts` also checks concurrent export registration.
Run `npm run acceptance:reform` for production validation, types, full unit
coverage and browser regressions at `/` and `/elezioni`.
