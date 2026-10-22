# Reform results: tables and plots

Use this guide for the results alignment at `MIGRATION_PLAN.md:209`. For
execution, local session recovery and archive transport use the
[application guide](politics-reform-2026-application.md). For legal allocation
and recovery use the [core reading map](../web/src/lib/politics/reform-2026/README.md).

## Views and counting definitions

- Paired bars show mean elected candidates and mean percentages on valid votes
  with one shared list order and separate scales. The expandable summary table
  includes coverage and ordinary, bonus, special-uninominal and proportional-TAA
  components. Its pages contain at most 50 rows.
- Boxplots show available simulated list percentages by renewed chamber. They
  use R type-7 quartiles, Tukey 1.5-IQR whiskers, mean markers and all outliers.
- Scatter panels show each chamber/list and chamber/supporting subject,
  including standalone lists. Every available pair is drawn; no point sampling
  or 1,000-run cap is introduced.
- National list spinograms use decile-based percentage bins, collapsing equal
  boundaries. Width represents the frequency of available percentages; cell
  height represents outcomes in that bin. A grey `?` cell represents unknown
  election counts. Missing percentages remain separately reported against the
  requested count. Clicking a spinogram opens its enlarged view; Escape closes it.
- The searchable college spinogram defaults to final winners at their actual
  election college. A separate measure shows original seat entitlements.
  Initial bonus winners without a college remain national; recovered bonus
  winners with an election college appear locally with their bonus category.
  The selector covers filed ordinary and TAA proportional slates, including
  zero-seat outcomes, rather than inventing plurinominal observations for
  special uninominal contests.

National list counts use final personal affiliation after multiple-election
resolution and recovery. Coalition/subject counts use the legal supporting
subject; they do not divide coalition bonus quotas among member lists. A special
winner without a declared affiliation belongs to the explicit unaffiliated
summary bucket and retains the supporting subject in coalition views. This
bucket has no fabricated list vote percentage. All views cover the initial
domestic scope (392 Camera / 196 Senato entitlements), excluding Estero.

Vote percentages use the applicable valid-vote denominator, including excluded
lists and special-territory ballots nationally. TAA proportional geography is a
view of existing ballots, never an extra national ballot contribution. Local
percentages use local ballots. Calculations and bin boundaries retain their
precision; only display labels are rounded.

## Unknowns and conditions

Known zero, no declared affiliation, unresolved preferences and unavailable data
remain distinct. Each chamber has its own availability: a failure in Camera
does not erase a known Senate outcome. A complete elected count requires a
terminal chamber ledger with no unresolved seats; definitively unfilled seats
are not elected candidates. Otherwise confirmed winners provide a lower bound.

Means labelled complete require the relevant value in every requested
simulation. The paired bars use `>=`/`≥` for confirmed elected totals divided by
the original requested count when the complete mean is unavailable. Vote means
remain unavailable when coverage is incomplete. Boxplots describe available
percentages and label their observation count; scatter/spinogram panels report
known pairs, missing outcomes and missing percentages against requested runs.
There is no successful-run renormalization of probabilities, reroll or implicit
zero for a failed/interrupted simulation. Candidate conclusions retain their
supplied/generated/missing-preference conditions.

## Archives and local storage

New archives retain final affiliation in `Candidate outcomes (conditional)` and
add `Reform statistical series v1`, `Reform college series v1` and
`Reform list summary`. A versioned presentation-capability record appears in the
existing metadata table. The schema-v2 envelope and generic streamed JSON.gz/CSV
export remain unchanged. Generated summaries use the same observation contract
as the charts. Editing the scenario draft does not relabel existing results.

Older archives remain readable without a new execution. National vote plots and
supporting-subject counts use available saved evidence. Ordinary/recovered list
locations can establish affiliation; original coalition bonus/special ownership
cannot establish personal affiliation. Such counts are explicitly reported as
not reconstructible, and list means retain their limitations. College vote
percentages absent from old archives cannot be reconstructed. Opening an archive
does not rerun generation or scrutiny, fetch replacement data, or rewrite it.

The worker builds a separate, disposable IndexedDB database,
`elezioni-statistics-v1`. National samples are indexed by series/simulation and
series/percentage/simulation. College samples are indexed only when selected.
Fixed-size pages (at most 256 observations) feed canvas scatter/outlier rendering;
exact quantiles read ranks from the on-disk value index. No array of every run
is retained in UI memory. Statistical panels mount when opened and load when
near the viewport; closing them discards their point-rendering components.

Caches are keyed by the committed source-table counters and presentation cache
version, excluded from exports and rebuilt after import. A cache quota failure
does not invalidate a completed canonical result. Deleting a result waits for
its active statistical queries and removes its derived samples. Owner checks
apply to every statistical service request. Interrupted results expose only the
durably published prefix and preserve the requested denominator.

## Task-oriented code and checks

| Task | Entry points |
|---|---|
| Final affiliation, per-run national/local observations | `web/src/lib/politics/reform-2026/result-series.ts`, `result-presentation.ts` |
| Archived evidence, compatibility, statistical queries | `web/src/lib/politics/reform-2026/stored-statistics.ts` |
| Rebuildable sample/value index and bounded pages | `web/src/lib/core/statistics-cache.ts` |
| Shared statistical/table/chart contracts and renderers | `web/src/lib/results/` |
| Reform layout, coverage and college selection | `ReformResultCharts.svelte`, `ReformSeriesPanel.svelte` |
| Worker request/owner/cleanup orchestration | `web/src/lib/workers/result-service.ts` |

Focused unit checks: `result-series.test.ts`, `result-presentation.test.ts`,
`politics/result-charts.test.ts`, `core/result-export.test.ts`. Browser checks:
`reform-statistics.spec.ts` (1,201 constructed observations, exact disk ranks,
unknowns, quota isolation, cleanup, every chart family and old archives),
`reform-workspace.spec.ts`, `result-store.spec.ts` and legacy/regional regressions.
Run `npm run acceptance:reform` for required production inputs and root/project-base
regressions. Run `npm run test:results` separately for the 1,000-run production
chart/detail/archive/memory check; do not run another build during it.
