# Reform results: tables and plots

Use this guide for the results alignment at `MIGRATION_PLAN.md:209`. For
execution, local session recovery and archive transport use the
[application guide](politics-reform-2026-application.md). For legal allocation
and recovery use the [core reading map](../web/src/lib/politics/reform-2026/README.md).
For global conditions, message frequencies and affected IDs use the
[messages guide](politics-reform-2026-messages.md).

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
- The territorial spinogram has ordinary dropdowns for chamber, College /
  Constituency, geography and entity. Both axes always use the selected entity
  and geography; search and alternative X-axis controls are absent. One plot
  shares the main section with its controls and has no inner toggle. Closing
  the section unmounts the plot while retaining its controls and selections.
  College mode defaults to final winners at their actual election college,
  including recovered bonus seats. Another measure shows original entitlements.
  The last-position measure reports 0 for nobody, 1 for capolista, and 2–7 for
  preference ranks. Skipped positions retain their ordering; elections elsewhere
  alone do not advance it. Unknown later outcomes can leave the exact position
  unknown. Entries without slate evidence, including Other, show unavailability.
  The college selector covers filed ordinary and TAA proportional slates,
  including zero-seat outcomes, rather than inventing plurinominal observations
  for special uninominal contests.
- Constituency mode covers ordinary territories in either chamber, with lists
  or coalitions/standalone lists. VdA and TAA keep their existing special views;
  TAA proportional slates remain available in College mode. Last elected position
  is disabled: constituent slates have independent rankings. Switching from that
  measure selects elected counts. The worker rejects constituency positions too.
  Elected counts offer **ordinary + bonus** (default) or **ordinary only**.
  Category follows the original seat, including bonus seats recovered into
  ordinary college slates. Winners count at their final election constituency,
  including initial bonus winners with no college. List counts use final personal
  affiliation; coalition counts use original seat ownership. The entitlement
  measure counts only original ordinary seats and disables the category selector;
  no member-list bonus entitlement is inferred. Shares divide constituency raw
  vote totals by constituency valid votes, never average college percentages.
- Both territorial modes offer all simulations (default), joint bonus awards,
  or joint non-awards, using `Reform runs.Bonus` independently of the selected
  entity and seat categories. Unknown/missing decisions belong to neither subset.
  Bin boundaries and frequencies are recalculated for the selected subset.
  Coverage distinguishes plotted percentages (including grey unknown outcomes),
  selected runs, missing percentages and known pairs. An empty subset shows zero
  and no plot. Ordinary-only counts remain unknown when the final count is unknown;
  confirmed category counts are not promoted to exact outcomes.
- The election-probability table orders each slate by capolista followed by
  descending preferences within each simulation, using the existing filed-order
  tie break. Position $k$ counts a success when that position **or any later
  position** has a confirmed final winner from the selected college/list.
  This is a literal OR; no causal link between elections is required. A later
  winner counts even when an earlier candidate was unavailable. Elections
  elsewhere alone do not count. For complete evidence, if $L$ is the last
  locally elected position, each cell reports the frequency of $L \ge k$.
  The denominator remains all requested simulations. Confirmed successes survive
  other unknown outcomes; a failure requires every existing position from $k$
  onward to be known zero. Incomplete coverage is labelled as a lower bound,
  absent positions as `—`, and wholly unknown outcomes as `n.d.`. These are
  per-run rank statistics, not fixed-person election probabilities.

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
known pairs, missing outcomes and missing percentages against requested runs
(or the selected bonus subset for territorial spinograms).
There is no successful-run renormalization of probabilities, reroll or implicit
zero for a failed/interrupted simulation. Candidate conclusions retain their
supplied/generated/missing-preference conditions.

## Archives and local storage

New archives retain final affiliation in `Candidate outcomes (conditional)` and
add `Reform statistical series v1`, `Reform college series v1`,
`Reform constituency series v1` and `Reform list summary`. A versioned
presentation-capability record appears in the existing metadata table. The schema-v2 envelope and generic streamed JSON.gz/CSV
export remain unchanged. Generated summaries use the same observation contract
as the charts. Editing the scenario draft does not relabel existing results.

`Reform college election positions v1` retains direct per-position final-election
evidence. Both the cumulative probability table and last-position measure derive
their values from it without rewriting that table or rerunning scrutiny. Existing
archives containing it support the new definitions. Archives without it explicitly
report unavailable position data. The last-position query streams this evidence
and pairs it with its own college vote-share series by simulation ID; a missing row
is an unknown outcome, not zero. It uses the same bonus subsets, exact X-axis
quantiles and missing-value coverage as the other college measures.

Older archives remain readable without a new execution. National vote plots and
supporting-subject counts use available saved evidence. Ordinary/recovered list
locations can establish affiliation; original coalition bonus/special ownership
cannot establish personal affiliation. Such counts are explicitly reported as
not reconstructible, and list means retain their limitations. College vote
percentages absent from old archives cannot be reconstructed. Opening an archive
does not rerun generation or scrutiny, fetch replacement data, or rewrite it.

Constituency observations are indexed lazily from the compact native table.
Archives predating that table can reconstruct them only from saved ordinary
college endowments, scenario coverage, raw college votes/valid totals, national
completeness, final seat locations and candidate affiliations. Each college's
valid-vote denominator is counted once. Missing coverage or expected vote rows
remain unknown; documented non-contestation contributes zero. Recovery destinations
and original categories remain distinct. Compatibility reads retain at most one
simulation from each evidence stream and never rewrite the canonical archive.
Archives lacking those capabilities show constituency data as unavailable.

The worker builds a separate, disposable IndexedDB database,
`elezioni-statistics-v1`. National samples are indexed by series/simulation and
series/percentage/simulation. College and constituency samples are indexed only
when selected.
Fixed-size pages (at most 256 observations) feed canvas scatter/outlier rendering;
exact quantiles read ranks from the on-disk value index. No array of every run
is retained in UI memory. Territorial panels request the chosen series when opened or changed, even below
the viewport; closing them unmounts the plot and preserves selector state. Other
statistical panels mount when opened and query near the viewport.

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
| Archived evidence, compatibility, statistical queries | `web/src/lib/politics/reform-2026/stored-statistics.ts`, `stored-constituencies.ts` |
| Ranked-position OR outcomes, last elected position and probability table | `election-probability.ts`, `stored-probabilities.ts`, `ElectionProbabilityTable.svelte` |
| Rebuildable sample/value index and bounded pages | `web/src/lib/core/statistics-cache.ts` |
| Shared statistical/table/chart contracts and renderers | `web/src/lib/results/` |
| Reform layout, coverage and college selection | `ReformResultCharts.svelte`, `ReformTerritorialSpinogram.svelte`, `ReformSeriesPanel.svelte` |
| Worker request/owner/cleanup orchestration | `web/src/lib/workers/result-service.ts` |

Focused unit checks: `result-series.test.ts`, `stored-constituencies.test.ts`, `result-presentation.test.ts`,
`politics/result-charts.test.ts`, `core/result-export.test.ts`. Browser checks:
`reform-statistics.spec.ts` (1,201 constructed observations, exact disk ranks,
unknowns, quota isolation, cleanup, every chart family and old archives),
`reform-workspace.spec.ts`, `result-store.spec.ts` and legacy/regional regressions.
Run `npm run acceptance:reform` for required production inputs and root/project-base
regressions. Run `npm run test:results` separately for the 1,000-run production
chart/detail/archive/memory check; do not run another build during it.
