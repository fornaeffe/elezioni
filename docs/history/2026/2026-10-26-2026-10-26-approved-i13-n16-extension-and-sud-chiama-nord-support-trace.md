## 2026-10-26 — Approved I13 N16 extension and Sud chiama Nord support trace

Luca approved extending I13 to N16 and explicitly rejected passing positive-seat
zero-vote targets automatically to a sole college. Implemented the N16 donor
extension in `territorial-matrix.ts`, preserving C83(1)(h)'s surplus-list order,
same-area recipient preference and cross-area fallback. Only the currently
ordered surplus list supplies integer donors after its strict donors exhaust.
Retained existing decimal/remainder states, seeded I03 ties and per-removal
`I13_INTEGER_SEAT_REMOVAL` warnings/transfer metadata. Separated the existing
college donor scope from the new N16 scope so I16 recipient reuse is not enabled
in N16 accidentally. N15/N17 subject donor policies remain strict.

Replayed all 19 archived failures against the approved extension. All eleven
former N16 failures now complete, each with 588 seats, 588 distinct elected
candidates and one N16 I13 integer-removal warning. All 1,254 national list vote
counts remain identical to the archive. The other eight retain exactly the same
Camera territorial communications and N18 `zero-index-quota` blockers as before.
Evidence is in `data/derived/r26-manual-tests/r26-risultati-2-after-i13-n16.json`
and `i13-n16-verification.json`; the original archive and pre-change replay were
preserved. This was a replay of the 19 affected IDs, not a new 1,000-run archive
or browser performance acceptance.

Reconstructed the zero Molise votes from source through municipal generation:

1. The scenario maps future `lista-3` only to historical `list-847ac39a103a9017`
   (`SUD CHIAMA NORD`), with factor one in both chambers. It supplies a common
   national reference of 1.4%, no geographic share overrides and no poll proxy.
   The Camera/Senato Molise colleges both include the future filed list.
2. Both pinned ministerial 2022 ZIPs have 1,768 Molise rows over 136 municipalities,
   but no Sud chiama Nord row. Basilicata also has no row for that list. Verified
   both ZIP SHA-256 values against the manifest/snapshot source versions.
   Therefore this absence originates in the historical source, not the importer.
   It is historical absence, not an observed positive-candidature zero to impose
   on future support.
3. Historical projection initializes future municipal support to zero where no
   mapped row exists. National reference reconciliation seeds available zero
   cells with `1e-12`; fitting national totals multiplicatively mostly rescales
   existing support elsewhere. The common reference resolves to Camera
   1.313881112% and Senate 1.491678265%. Molise's prepared Camera valid shares
   remain only `1.703282e-12`–`2.984495e-12`, corresponding to about `2.464847e-7`
   votes before future drift. The numerical seed is not a meaningful territorial
   prior for a newly filed area.
4. In simulation 311, the generator's logit floor and actual paired shocks produce
   normalized Camera Molise weights equivalent to about 0.000471070 votes in
   total, at most 0.000107148 in one municipality. Largest-remainder integer
   apportionment awards zero in all 136 municipalities. Municipal and college
   totals are both zero in both chambers for all eight inspected zero-quota IDs;
   college aggregation does not discard positive municipal votes.

The defect is the absence of a meaningful territorial-support estimate when a
future filed list has no historical local candidature/support. Filing only
defines eligibility, while a national anchor does not identify a local profile.
Luca requested diagnosis rather than a new support rule: no generator, scenario,
filing, source or bundle changes were made in this follow-up. Documented the
limitation in the generation guide and replaced LG06's proposed sole-college
bypass with the explicit rejection and upstream-correction requirement. Kept the
plan open for a selected local estimate/reference policy; the unrelated drift
research remains paused. Source and numerical traces/scripts are retained as
`scn-source-audit.json` and `scn-molise-trace.json` in the manual-test directory.

Verification: added seven regressions for N16 fallback ordering, same/cross-area
routes, preserved evidence, I03 replay, unchanged I16 scope, transfer export and
the retained sole-college zero-vote blocker. All 1,044 tests in 78 files pass
without skips; `npm run check` reports zero errors/warnings; `git diff --check`
passes. No browser acceptance, commit or publication was performed.
