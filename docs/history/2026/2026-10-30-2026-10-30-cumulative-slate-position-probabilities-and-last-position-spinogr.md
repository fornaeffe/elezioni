## 2026-10-30 Cumulative slate-position probabilities and last-position spinogram

Completed the two ranked-position result items originally at
`MIGRATION_PLAN.md:399` and `:406`. Each probability cell now counts election
in that position OR any later position in the same college/list. Luca approved
the literal OR without causality, final election locations including recoveries,
and an unavailable measurement for entries without candidate-slate evidence.
An election elsewhere alone never counts.

The shared `slatePositionOutcomes` helper derives both cumulative threshold
outcomes and the exact last locally elected position from the existing direct
`Reform college election positions v1` evidence. It retains confirmed threshold
successes when later outcomes are unknown, while an unknown position above the
last confirmed winner makes the exact last position unknown. Earlier unknowns
below a confirmed last winner do not obscure it. Complete no-winner evidence
gives zero; absent ranks remain distinct. Table percentages keep all requested
simulations as denominator and retain lower-bound/unknown labels.

The college spinogram adds “Ultima posizione eletta nella lista”: 0 for nobody,
1 for capolista, 2–7 for the first through sixth preference ranks, including
skipped positions. Its worker query streams archived evidence and joins the
chosen share stream by simulation ID. It preserves bonus filtering, independent
X-axis shares, exact deciles, missing-data coverage and enlarged chart labels.
Missing observations stay unknown. Missing position tables or missing selected
slate evidence show an explicit unavailable state. Canonical tables, archive
formats, legal scrutiny and derived sample-cache schemas are unchanged; opening
an archive never reruns generation or scrutiny.

Added exhaustive checks of all 128 complete position patterns, partial-evidence
cases, final/recovered locations and probability denominators. Browser worker
checks cover 1,201 observations, independent X streams, both bonus subsets,
missing observations, unavailable slates, invalid national-measure requests and
unchanged archive exports. UI checks cover cumulative percentages, the new
measure, filter/axis changes, enlargement and archives without position data.

Verification: `npm test` passed **107 files / 1,259 tests**; the final focused
unit run passed **four files / 15 tests**. `npm run check` passed with zero
errors/warnings, and `npm run check:data` passed. Static builds and all seven
`reform-statistics.spec.ts` browser cases passed at both `/` and `/elezioni`
(the root run's six passing cases plus the corrected new UI case in a targeted
run; all seven passed together at the project base). Svelte autofixer found no
issues in the three changed components; existing asynchronous-effect and DOM-ref
suggestions were reviewed without expanding this task into lifecycle refactoring.
`git diff --check` passed. Full production acceptance and the separate production
performance/archive benchmark were not run.

An initial TypeScript narrowing diagnostic was resolved by explicit winner/unknown
flags. Initial browser assertions incorrectly expected position 7 in the even-only
subset (whose maximum is 6), used a label locator instead of the existing control's
accessible role, and waited for a saved-result selector before the first import.
The tests now use the correct subset expectation and wait for result-ID changes
only when an existing result is present. No deadlines were increased.

Updated the results guide and marked both items complete. The subsequent
college/constituency selector remains pending. Changes are uncommitted.
