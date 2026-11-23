## 2026-10-26 Remaining reform UI refinements

Completed the remaining **Refine UI** checklist (the requested lines 329–357):

- Added a chamber/list election-probability table with one row per filed college
  and seven positions: capolista followed by the first six preference ranks.
  New executions export compact per-run position outcomes in
  `Reform college election positions v1`. The presentation helper reuses the
  existing preference-ranking function and reads final election locations,
  including recoveries into the selected college/list and excluding elections
  elsewhere. It records confirmed wins, unknown outcomes and absent positions
  separately. Worker queries stream these rows with counters bounded by the
  selected list's colleges; duplicate/unordered/invalid observations fail
  explicitly. The UI uses all requested simulations as denominator, labels
  incomplete frequencies as lower bounds, and explains that ranks can represent
  different people across simulations. Older archives remain readable and show
  an explicit unavailable state; opening them never reruns scrutiny.
- Added lists/coalitions-or-autonomous-lists selectors to mean bars and vote
  percentage boxplots. Both reuse the existing national statistical series,
  including coalition seat ownership and final personal list affiliation.
  Added chamber-selectable bonus frequency bars with adjacent percentages,
  derived from the actual joint award decision and first-subject evidence,
  independently of seats subsequently filled. No-award simulations remain in
  the denominator; unknown award decisions remain explicit.
- Split local spinogram selection into chamber, college and list. Colleges are
  deduplicated across lists; changing college preserves the visible list when
  filed there and falls back to an available list otherwise. The existing
  actual-election/original-entitlement measure selector is retained.
- Moved application state, worker progress, scenario-preview activity and
  import/export activity into a compact collapsible fixed corner overlay.
  Interaction errors, scenario validation and preview notifications use
  dismissible fixed toasts. Dismissing validation never enables an invalid
  execution; changing the draft can show its current validation again.
  Result information, warnings, execution failures and stale-result notices
  remain in the results section. Dependency-removal review remains an explicit
  in-place decision with its existing actions.
- Put the single-simulation seat chart and its controls into a collapsible
  section; stored chart queries wait until it opens. Inspecting a diagnostic
  opens that section before scrolling/focusing the simulation control.
  Expanded the summary-table explanation to distinguish requested/known
  observation counts, complete means and category-specific minimum means.

Verification:

- `npm run check`: zero errors and warnings. The official Svelte autofixer found
  no issues in all seven changed/new components; generic effect suggestions
  were reviewed and retained for asynchronous worker-read lifecycle handling.
- 29 targeted unit tests passed across seven files: election probability,
  result-series/presentation, compute/pool and result-export coverage. Position
  tests distinguish preference rank, recovery location, election elsewhere,
  unknown outcomes, absent ranks and invalid/duplicate observations.
- All 14 browser tests in `reform-statistics.spec.ts`, `reform-editor.spec.ts`
  and `reform-workspace.spec.ts` passed at both `/` and `/elezioni`, without
  retries or skips. They cover coalition/chamber selectors, exact constructed
  probability and bonus frequencies, list preservation/fallback, collapsed
  chart and overlay, validation dismissal, archive compatibility, production
  worker runs and scenario/result roundtrip/replay. Repeated the four statistics
  browser tests after final validation changes, adding invalid-file toast
  dismissal and a failed rerun whose error stays in results.
- Static builds passed during browser checks, retaining the existing large-chunk
  advisory. Visually reviewed desktop and 390px mobile chart/table screenshots;
  tables scroll within their containers and controls remain within the viewport.
  Updated the separate long-running production acceptance test to open the
  single-simulation section; that acceptance was not rerun.

No legal scrutiny or statistical-generation algorithm, generated default,
source dataset, deployment or existing archive was changed. New runs include
additional exportable presentation evidence. No commit or publication was made.
