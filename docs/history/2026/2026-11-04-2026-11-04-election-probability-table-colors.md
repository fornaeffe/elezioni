## 2026-11-04 — Election probability table colors

Probability cells now blend from white at 0% to the selected list color at
100%. List colors with less than 3:1 contrast against white are darkened to
that threshold before calculating the scale. Cell text selects black or white
for at least 4.5:1 contrast. Added helper tests for endpoints, faint-color
darkening, and text contrast; the table's probability labels and calculations
are unchanged. Verification: all 5 election-probability tests pass,
`npm run check` reports zero errors and warnings, and `git diff --check` passes.
