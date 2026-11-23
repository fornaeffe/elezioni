## 2026-10-23 — Poll-informed references (substep 2)

Implemented explicit poll-informed scenario preparation, leaving historical bias
calibration open. `data:polls:references` consumes verified production history and
the selected archive, writes immutable report/scenario JSON under ignored
`data/derived/polls/`, and validates one paired generation/full scrutiny before
writing. Defaults and existing saved scenarios remain unchanged. The operation
and exact assumed policy are documented in `polls-preparation.md`.

`poll-reference-v1` uses compatible questions available at the reference date,
one recent wave per pollster, generic ballots by default and explicitly selected
conditional alternatives. It combines recency/effective-N weights with a weak
historical prior. All weights, exclusions, inferred decided samples, reconciled
mass/proxies and source versions survive in the report. No house-effect or
historical-accuracy correction is asserted. The numerical policy is an assumption
awaiting substep 3, not calibrated confidence coverage.

Opt-in scenario v3 stores date, rows, reference-date uncertainty and territorial
donors. Versions 1/2 keep existing interpretation and streams. A presentation
upgrade initially forced every draft to v2; it now retains v3, with an import,
edit, reload and export browser regression. Generated vote/chamber provenance
includes polling source/reference versions and assumptions. The generator applies
poll targets only in ordinary geography before explicit national/local user
references, preserves historical turnout/special inputs, and records donor or
uniform-filed-support estimates for newly supported parties. Unmapped history
with observed raw valid votes uses those totals as a labelled turnout proxy.
Reference-date error has shared chamber draws and compositional normalization;
future movement stays in the existing independent named streams. National
polling refreshes the national date while territorial uncertainty retains its
historical date. User-supplied
references remove polling measurement error only in their own scope/chamber.

The 23 October preparation selected Eumetra 14358/q4, Piepoli 14364/q1, Only
Numbers 14362/q1 and SWG 14346/q1. Conditional SWG/LAB21 questions were excluded
by default. The produced reference is `poll-reference-v1-2a6c7e0ef629`; the scenario
fingerprint suffix is `475ad8234360`. Its production paired generation and composed
scrutiny were computable. Repeating preparation reproduced the immutable output.

Verification: preparation and application type checks passed (zero Svelte
errors/warnings); 947 unit tests in 68 files passed, including eight new tests
for selection/as-of dates, conditional replacement, round trips, geographic
fallbacks, override precedence, zero-horizon measurement variance, shared-chamber
errors and deterministic replay. Production build passed. The existing 29
root-base Chromium tests passed (40.2 s); the additional focused v3 browser
compatibility test passed (8.0 s). Chunk/plugin/color notices remain informational.
No new UI control, default activation, historical fitting, commit or deployment
was performed.
