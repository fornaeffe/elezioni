## 2026-10-30 — Display fixed reform seat endowments

Added two fixed endowment tables to the reform results before the existing
elected-and-percentages summary: college seat counts with and without the bonus,
and bonus-slate seat counts by constituency. They are built from the validated
seat-table snapshot once in the shared presentation context, persisted with the
result, and read independently of simulation statistics. College rows include
all college types; bonus-slate rows include only ordinary constituencies with a
positive bonus endowment. Older archives without these tables continue to load
without showing an empty endowment section. Marked the corresponding result UI
item complete in the migration plan.

Verification: `npm run check` reports zero errors/warnings and `npm run build`
passes. No tests were added or run, as requested. The build retains its existing
chunk-size and plugin-timing advisories.
