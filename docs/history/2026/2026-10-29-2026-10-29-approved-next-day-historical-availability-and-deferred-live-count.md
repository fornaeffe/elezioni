## 2026-10-29 — Approved next-day historical availability and deferred live-count forecasting

Luca approved replacing FV-03's arbitrary 30-day fallback with availability
**by the end of the first civil day after the final polling day**, only where
availability is unknown. Versioned the preparation policy as
`fast-share-production-sources-v2` / `retrospective-next-day-end-v1`. Added
explicit final-polling-day overrides for Camera 2008/2013 and a separate
`knownDates` table with required citations. Known dates take precedence, including
known same-final-day availability; currently that table is empty rather than
inventing publication evidence. UTC civil-date arithmetic preserves the existing
inclusive daily cutoff and the actual observation dates. Examples now qualify
on 2008-04-15, 2013-02-26 and 2022-09-26 respectively.

Regenerated the compact source and verified its CLI integrity/conservation checks.
The new source ID is
`fast-share-source-v1-95d505ed1f7ea79c99cd0bf91a86dbc6a2d54c1f24fd9b37bec40a57e3c72511`.
All ten observation chunks, national counts, election dates, poll records and
geography remain unchanged; only availability/policy/provenance metadata changes.
The prior [FV-03 benchmark](../../../test/fixtures/benchmarks/vote_share_sources.json)
remains historical evidence for its original policy and measurements. The
[separate policy-check record](../../../test/fixtures/benchmarks/vote_share_availability.json)
pins current sources/metadata, all ten new availability dates and the comparison.

All 24 frozen six-election/horizon cases retain identical eligible election IDs.
Compiling all eight C/E elections at reference 2026-10-19 with an explicit reduced
two-component basis and conditional target inputs preserves national/profile
numerical keys while changing provenance. This is an eligibility/input comparison,
not a new fitted-posterior or predictive run. Source changes are generated offline
under `data/derived/fv03-next-day-output/`; no app defaults or production dispatch
are installed.

Added tests for multi-day contests, leap/year boundaries, known-date precedence,
invalid evidence/date overrides, inclusive eligibility on the availability day,
rejection on the preceding day and unchanged observation time. Both focused test
files pass **16 tests** and preparation typechecking passes. Source preparation
and the full ten-event data checker pass; documentation links, source evidence
hashes and diff whitespace were checked. No model equations or sampler changed.

At Luca's request, appended a task at the end of the post-migration list for
short-term forecasting of the **current election's final scrutiny result** from
incomplete incoming returns. It requires timestamped counting snapshots,
reporting/denominator information, uneven-reporting and revision treatment, and
chronological final-result/uncertainty validation. It is explicitly separate from
forecasting subsequent elections and from this retrospective availability rule.
No partial-return ingestion or live-count model was implemented now.
