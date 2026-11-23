## 2026-10-31 — Constituency spinogram and simpler territorial controls

Implemented the approved constituency extension of the college spinogram.
The shared section offers College / Constituency, ordinary territory dropdowns,
chamber and entity selection; constituency mode supports lists or coalitions /
standalone lists. Both axes always describe that entity and geography. Removed
territory search and the advanced alternative X-axis UI, worker request and
pairing helper. Closing the section retains selector state while unmounting
the plot. Opening it or changing its controls requests the chosen series
immediately, independently of viewport position. The national plot remains separate.

Constituency elected counts default to ordinary + bonus, with an ordinary-only
option. Categories follow the original seat, including bonus recoveries into
ordinary slates; counts follow the final election territory. List affiliation
and coalition seat ownership remain distinct. Initial bonus winners without a
college are included. Original entitlements remain ordinary-only and disable
the category selector. Last-position measurement is disabled for constituencies,
resets to elected counts when changing level, and is also rejected by the worker.
VdA and TAA retain existing special views, including the TAA college selector.

New runs export `Reform constituency series v1`, with raw constituency vote totals,
valid-vote denominators, final ordinary/bonus counts, completeness and ordinary
entitlements. Shares never average college percentages. Lazy disk-backed indexes
and exact filtered quantiles remain bounded. Older archives can reconstruct the
view from saved coverage, raw college totals, endowments, national completeness,
seat locations and candidate affiliations; absent capabilities are unavailable,
missing observations remain unknown and no scrutiny is rerun. No legal algorithm,
model, RNG stream, generated defaults or production input was changed.

Initial browser checks required updating removed search selectors and checking the native
option's disabled attribute; a synthetic 1,201-run fixture was corrected to keep
its percentages within 0–100. Captured acceptance logs now live outside Playwright's
output directory after an initial cleanup delay. A reproducible loading stall
showed that an opened territorial panel could remain below the viewport after a
measure change, leaving its visibility observer untriggered. Territorial queries
now start on open/change; no longer assertion timeout is needed. All eight focused
statistics browser tests pass with two workers, including retained selectors,
archived reconstruction, offscreen queries and the formerly stalled measure.

Final verification: `npm run acceptance:reform` passed with
`ACCEPTANCE_TEST_WORKERS=2`: all 1,264 unit tests and 51 browser tests at each
of `/` and `/elezioni`, zero skips or retries. Production bundle checksums and
the 41-stage / 588-filled-seat scrutiny fixture, preparation/application types,
static builds and final root-build restoration passed. Svelte MCP reported no
component issues; mobile rendering was inspected. `git diff --check` passed.
Scale assertions use the 1,201-observation constructed archive; the separate
1,000-run production results benchmark was not rerun. The results guide and
migration checklist are updated. Changes remain uncommitted; no deployment.
