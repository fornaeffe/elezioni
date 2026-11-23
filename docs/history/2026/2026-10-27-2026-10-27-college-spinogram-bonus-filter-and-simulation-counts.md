## 2026-10-27 — College spinogram bonus filter and simulation counts

Added the single-college spinogram selector for all simulations (default),
joint bonus awarded, and joint bonus not awarded. The selection persists across
chamber, college, list and elected/entitled measure changes. Filtering reads the
archived `Reform runs.Bonus` decision independently of the selected list's
filled bonus seats; missing/non-computable decisions enter neither subset.

The result worker recomputes exact type-7 deciles, bin frequencies and coverage
for the selected subset. Value-index scans use bounded 256-observation pages
and retain only the quantile ranks required, plus selected simulation IDs.
Derived caches and canonical exports keep their existing formats. The panel
reports plotted simulations (known percentages, including unknown outcomes)
and selected simulations separately; no-data subsets show zero and no plot.
Updated the active plan and the focused results guide.

Verification:

- `npm run check`: zero errors/warnings; Svelte MCP review found no issues
  (retained the existing `bind:this`/IntersectionObserver lifecycle).
- `npm test -- result-presentation.test.ts result-series.test.ts`: 11 tests
  passed across three files.
- `npm run test:e2e -- reform-statistics.spec.ts --workers=1 --reporter=line`:
  all five browser tests passed, including exact filtered ranks/deciles on
  1,201 archived simulations, both measures, missing X/Y, unknown bonus
  decisions, old archives with no decision, empty subsets, selector persistence
  and unchanged canonical exports. The first run exposed an overly strict
  test label lookup; using the control's accessible combobox name resolved it.
- The browser suite built the static app successfully; existing chunk-size and
  plugin-timing advisories remain. Inspected the 390-pixel mobile screenshot:
  the selector and counts wrap within the panel. `git diff --check` passed.
