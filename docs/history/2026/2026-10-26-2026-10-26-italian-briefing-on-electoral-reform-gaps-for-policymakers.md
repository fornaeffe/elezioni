## 2026-10-26 Italian briefing on electoral reform gaps for policymakers

Created `docs/politics-reform-2026-nota-legislatore.md` and its six-page Word
edition, linked from the legal-gap register. The briefing covers all six
registered allocation gaps, unspecified terminal ties and Senate cross-region
recovery. It states the problems, full statutory references, worked examples
and decisions requiring clarification, without implementation identifiers,
project notation or presenting simulation policies as statutory rules.
Distinguished the two possible readings of ordinary list donor decimals from
express remainder restrictions, and the regional-recovery ambiguity from a
claim of constitutional invalidity.

Rechecked the official 23 October annotated republication, articles 1 and 2,
and Constitutional Court order 86/2020. The order establishes the Pavanelli
background but dismissed the conflict as inadmissible; it did not settle the
constitutional merits of moving a Senate seat between regions.

Read the user-designated `data/derived/r26-manual-tests/r26-risultati (3).json.gz`
without changing it. Archive SHA-256:
`39e33b69d6588cdbc5a2219286b0be13cb0772fcdf013efb1cb28411ded42bc2`;
exported at `2026-10-26T18:19:02.865Z`. All 1,000 runs are complete, after the
existing interpretive policies. Deduplicated warning events by simulation ID
and checked the counts against the diagnostic-event table. Classified integer
removals by legal reference/context so constituency-list corrections are not
mistaken for college-list corrections. Recorded frequencies:

| Situation | Distinct simulations | Events |
| --- | ---: | ---: |
| LG01 college list integer removals | 328 | 446 |
| LG02 cap-group integer removals | 0 | 0 |
| LG03 constituency/college capacity conflict | 7 | 14 |
| LG04 reused list recipient decimals | 16 | 17 |
| LG05 constituency internal-list integer removals | 11 | 11 |
| LG06 preventive zero-vote recipient exclusions | 60 | 60 |
| Unspecified terminal ties | 8 | 8 |
| Senate cross-region recovery | 1 | 1 |

LG06 exclusions are preventive eligibility checks, not 60 observed zero-quota
failures; the briefing explicitly marks the latter incidence as undetermined.
The seven LG03 simulations involve fourteen constituency occurrences. For
LG01, 323 simulations affect Camera and 12 Senate, with overlap; chamber
counts must not be added as distinct elections. The recovered Senate seat in
simulation 772 moves from Toscana to Emilia-Romagna for the same list; its
candidate recovery trace was checked. Simulation 163's internal-list integer
removal was also corroborated against the transfer table. Frequencies remain
conditional on this scenario, synthetic candidatures/preferences and provisional
census-derived seat tables; they are not forecasts or a strict-law failure rate.

Evidence is retained under the ignored manual-test directory in
`policymaker-frequency-audit.json`, `policymaker-frequency-verification.json`
and `policymaker-document-verification.json`. Independently verified the
briefing's quota arithmetic, five official-source hyperlinks, absence of
internal identifiers and nine-row summary table. Rendered and visually
inspected all six final pages. The packaged renderer initially failed because
Windows has no bundled LibreOffice; Word COM was also unavailable. Used a
task-isolated LibreOffice Portable 26.2.4 package from the official publisher
with a valid Authenticode signature, retaining the packaged renderer and
bundled Python/Poppler. No installed desktop LibreOffice was used. Removed the
default template's title border and rerendered before final visual review.

This is a standalone documentation task, so no migration checklist completion
was claimed. No scrutiny logic, scenarios, historical inputs, archived results,
release/deployment state or production data changed. No commit or publication
was performed.
