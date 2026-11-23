## 2026-10-26 Reform result chart selectors

Completed the first **Refine UI** item: reform results now display one chamber
at a time for mean elected/valid-vote bars and simulated list-percentage
boxplots, with independent Camera/Senato controls. Scatterplots share one
section for lists and coalitions/autonomous lists. Both that section and
national spinograms offer grouping, chamber and entity selectors and mount
only the selected series through `ReformSeriesChart.svelte`.

Selections resolve to an available series after changing chamber/grouping;
missing combinations show an explicit empty state. Keyed remounts replace
series statistics, streamed scatter points and boxplot outliers when the
selection changes, preventing stale plots. Existing lazy loading, complete
point streaming, unknown-outcome semantics, spinogram enlargement, summary
tables, local-college controls and old-archive reading are retained. The
remaining UI checklist items stay pending.

Verification:

- `npm run check`: zero errors and warnings; the official Svelte autofixer
  also reported no issues in either changed/new component.
- `npm test -- result-presentation.test.ts statistics chart-builders`:
  seven tests passed across two matching files.
- `npm run test:e2e -- reform-statistics.spec.ts --workers=1 --reporter=line`:
  all three tests passed at `/`; repeated at `/elezioni` with retries disabled,
  also three passes. The synthetic multi-series archive exercises both
  chambers, both groupings and two entities per combination, checking one
  rendered panel and the selected series' distinct known-observation count.
  Existing tests still cover all 1,201 observations, sparse archives and local
  entitlement charts. Adjusted the latter test to scroll its lazy panel into
  view after the added controls moved it below the viewport.
- Visually inspected desktop and 390px mobile screenshots; mobile controls
  and charts remain within their container. Updated the separate production
  result-acceptance test's section locators; that long-running acceptance was
  not rerun for this presentation-only change.

Static builds passed as part of both browser runs, retaining the existing
large-chunk advisory. No scrutiny, generation or archived-result data changed.

### Follow-up: remove the nested national-spinogram toggle

The selected plot in **Spinogrammi degli eletti** now renders directly inside
the outer collapsible section. The shared chart renderer supports an optional
non-collapsible wrapper, enabled only for the national series selector; other
chart sections retain their existing behavior. The plot, coverage notes and
click-to-enlarge dialog are preserved. Added an assertion to the existing
browser check that this section contains no nested `details` element.

Verification: `npm run check` reported zero errors/warnings. All three
`reform-statistics.spec.ts` browser tests passed, including plot visibility,
selector changes and opening/closing the enlargement dialog; the static build
also passed. The Svelte autofixer found no issues in the three affected
components (its existing `bind:this` refactoring suggestion was outside scope).
