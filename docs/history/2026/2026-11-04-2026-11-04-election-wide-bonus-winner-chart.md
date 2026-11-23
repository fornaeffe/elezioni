## 2026-11-04 — Election-wide bonus winner chart

Removed the chamber selector and its state/filter from the bonus-winner chart.
The chart now groups awarded simulations by the linked Camera/Senato subject
pair, using the election-level status in `Reform runs` and matching the two
`Bonus eligibility` rows by simulation. Each awarded simulation contributes
once; equal display names appear once, while distinct names are joined. The
requested-simulation denominator and known-decision lower-bound behavior are
preserved. Missing or duplicate pair evidence is reported as an error rather
than silently counted.

Verification: focused stored bonus-pair tests passed (2 tests), including equal
names, distinct names, and duplicate-chamber rejection; `npm run check` passed
with zero Svelte/type errors and warnings; `git diff --check` passed. No issues
or workarounds.

Follow-up: E2E archives with no `Bonus` field exposed that a bonus-statistics
error could abort construction of unrelated vote boxplots. Isolated that request
to the bonus section and surfaced its error there; chart loading now continues.
All eight tests in `reform-statistics.spec.ts` pass, including the two affected
chart-family cases, and `npm run check` remains clean.
