## 2026-10-28 — Complete the legislator note's legal references and examples

Replaced the requested comments in `politics-reform-2026-nota-legislatore.md`
with a statutory tie-reference catalogue and seven worked simulation examples:
163 (Camera constituency integer donor), 2 (Camera college integer donor),
28 (Senate college integer donor), 354/312 (Camera/Senate exhausted recipient
decimals), 22 (unspecified correction tie), and 19 (Camera territorial-capacity
conflict). Each example traces upstream entitlements, initial allocation,
intervening corrections, the uncovered state and any provisional simulator
treatment. Corrected the cap-group correction reference from C83-bis(1-bis)(b)
to (c), and the constituency terminology/heading typo in section 4.

Rechecked GU's 23 October annotated republication, articles 1 and 2, and the
parliamentary comparison for unchanged surrounding provisions. Linked these
sources and identified the archive, seed and provisional seat-table scope.
Kept two `esempio non trovato` comments for empirical LG02 cap-group donor
exhaustion: none appears in the designated archive or the other local archived
simulation warning sets inspected. Constructed legal-core fixtures were not
presented as observed simulation examples.

Verification used the current `r26-risultati (2).json.gz`, exported at
`2026-10-26T23:33:07.368Z` (27 October, 00:33 in Italy), SHA-256
`00a085ca4b2fbaa03a9a4d79a4798a8482d52781b4c93378a864011f68dd23f5`.
Stream-read the relevant archive tables and all header warnings; regenerated
the seven selected runs from their archived scenario and pinned generation
snapshot. All 462 national list vote totals and all 565 exported transfers
match exactly. Independently checked the examples' integer quotas, initial
seats, correction chains, exhausted decimals, exact tie and group totals;
checked constituency names against the generation geography. All seven runs
complete with 588 seat records under the existing provisional policies.
Evidence and replay/verification scripts are retained in the ignored
`data/derived/r26-manual-tests/note-20261028-*` artifacts.

Documentation checks confirm only the two requested missing-example comments
remain, Markdown math delimiters are balanced, and `git diff --check` passes.
No scrutiny/runtime changes, archive writes, frequency-table recalculation,
Word regeneration or migration-step completion are included.
