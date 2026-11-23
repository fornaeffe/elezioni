## 2026-10-27 — Independent college spinogram share source

Added normally collapsed advanced X-axis controls to the college spinogram.
An opt-in checkbox enables a searchable college and list selector within the
current chamber. The main controls continue to select Y-axis elected counts or
original entitlements. The active sources remain labelled above the plot when
advanced controls are collapsed; disabling the option restores matching axes.
The plot now renders directly in the main college section, with no nested
collapsible chart. Its enlarged view retains the independent X-source label.

The result service accepts an optional `xSeries`. Statistics use that series for
share distributions/deciles and merge both ordered series by simulation ID in
bounded pages. A missing outcome row remains unknown, a known zero remains zero,
and an absent/null share remains a missing percentage. Both measures and all
bonus subsets use the same pairing. Cache and canonical archive formats remain
unchanged. Updated the plan and focused results guide.

Verification:

- `npm run check`: zero errors/warnings; Svelte MCP found no issues, retaining
  the existing observer lifecycle suggestion.
- `npm test -- result-presentation.test.ts result-series.test.ts result-charts.test.ts`:
  14 tests passed in four files.
- All six `reform-statistics.spec.ts` browser tests passed across the suite run
  and a targeted rerun. The first run passed five tests; its disk-test assertion
  incorrectly expected an odd seat count in the even-simulation subset. Fixed
  that expectation and reran the disk test against the same static build.
  The 1,201-run fixture checks distinct X/Y sources, unequal/missing simulation
  rows, missing shares/outcomes, both measures, all bonus subsets, and unchanged
  exports. UI checks cover default collapse, independent selection, collapse
  persistence, source labels, enlarged view, matching-axis reset, list/chamber
  changes and absence of an inner chart toggle.
- The static build passed with existing chunk-size/plugin-timing advisories.
  Inspected the 390-pixel screenshot of the advanced controls, source labels and
  plot; the controls fit within the panel. `git diff --check` passed.
