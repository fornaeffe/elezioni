## 2026-10-20: Compact 1000-run diagnostics CSV by code

Exported [the compact CSV](../../../test/fixtures/benchmarks/browser_reform_1000_diagnostics_by_code.csv)
with `severity,code,occurrences,simulations`: **10 error codes and 5 warning
codes**, merging all reasons of each code. Occurrences are additive; affected
simulations require a distinct union. The original reason-only report lacked
the IDs needed to obtain those unions safely.

Added tested code-level counters to the existing streaming diagnostic query and
replayed the same production scenario/seed at 1000 runs with four workers.
The replay passed and matched the original overview and every reason's counts
exactly. Enriched the original report with `codes` summaries and explicit replay
provenance, preserving its original timings. The replay's own elapsed time was
217.130 s, recorded separately under `data/derived/r26-21-acceptance/`.
For example, N23 and N24 each have **218 occurrences in 216 distinct simulations**;
I07 cross-region recovery has **93 occurrences in 55 simulations**. Session-wide
context warnings have one occurrence and zero per-run simulation IDs. No legal
logic, source data or draws changed. Focused counter tests and the production
browser replay passed.
