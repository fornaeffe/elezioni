## Politics result component ownership (2026-11-16)

Introduced `results/ResultSection.svelte` as the single owner of result
disclosures, summary styles, focus appearance and open state, including a
compact message-detail variant. Split general diagnostics, simulation summaries
and affected-simulation inspection; the latter now sits inside its message
disclosure, retaining its controls across collapse/reopen and resetting when
closed or another group is selected. Diagnostic content styles are shared in
a stylesheet bounded by their wrapper.

`PoliticsSessionResults` now only composes sections and forwards revisioned
inspection requests. Stored-table selection/paging and single-simulation seats
own their controls, loaders, request guards and independent errors. Repeating
an inspection synchronizes both views without coupling subsequent edits.
Renamed the cross-law statistics composition to `PoliticsResultStatistics`;
means, boxplots and summary pagination own their respective state. Pure boxplot
assembly moved to a TypeScript presentation helper. Adjacent section wrappers
retain lazy national-chart/probability mounting, while territorial selectors
remain mounted and own their plot activation. Preserved default open states,
the keyed session reset, eager boxplot statistics and committed-input loading.
See the [component task map](../../web-workspaces.md#politics-result-components).

Verification from `web/` on Windows, root base path:

- `npm run check`: zero errors and warnings. The official Svelte autofixer
  reported no issues in the extracted/changed components; generic effect
  suggestions were reviewed against guarded worker requests and independent
  navigation/control state.
- Focused Vitest: `result-presentation.test.ts` and `result-series.test.ts` under
  `src/lib/politics/reform-2026/`, plus `src/lib/core/diagnostic-messages.test.ts`:
  14/14 passed.
- `npx playwright test reform-messages.spec.ts reform-statistics.spec.ts
  --project=chromium --workers=2 --reporter=line`: 9/9 passed, including uniform
  computed summary styles, independent controls, repeated inspection, nested
  drill-down visibility/reset, sparse archives and retained territorial choices.
- `npx playwright test common-politics-lifecycle.spec.ts --grep
  'common lifecycle .*/M01:' --project=chromium --workers=2 --reporter=line`:
  2/2 passed for Rosatellum/reform, including mean selectors, law-specific
  sections, stored-table selection, durable execution, reload and archive reuse.
- Playwright's production builds and `git diff --check` passed. Vite retained
  its large-chunk warning. No project-base acceptance matrix was run.

The expanded diagnostics test initially timed out on an exact wrapped-select
label lookup; selecting by accessible combobox role/name resolved the test
failure. No new test files or package dependencies were added.
