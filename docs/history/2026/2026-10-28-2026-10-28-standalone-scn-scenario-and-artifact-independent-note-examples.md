## 2026-10-28 — Standalone SCN scenario and artifact-independent note examples

Executed a new series of 1,000 paired runs from the current
`r26-risultati (2).json.gz` scenario, making SCN standalone in both chambers.
Luca chose an equal-weight mean across Abruzzo, Calabria, Emilia-Romagna and
Puglia, using the available chamber per region and the two-chamber mean within
Calabria; Sicily is excluded. Pinned attributed 2022 counts give
**0.18986691282818832%**. Explicitly supplied the same fraction in every missing
regional branch with filed SCN support: 14 Camera and 15 Senate values across
16 regions. Preserved all other scenario properties, including the 1.4%
national reference, seed, filings, dates, candidates policy and bonus declarations.
Unfiled VdA/TAA remain unchanged; source ZIP hashes and the exact scenario delta
were independently verified.

Saved the executed scenario, calculation, full schema-v2 result and audit under
`data/derived/r26-manual-tests/scn-autonoma-20261028/`; its README is the artifact
map. Eight Node compute processes reused the existing generator, scrutiny,
presenter and statistics helpers. IDs 1–1,000 were executed once each, with
Camera/Senato together, in about 396 seconds including initial export. All runs
complete with 588 filled, distinct candidates each; 641 award the joint bonus.
The final result gzip is 279,165,138 bytes, SHA-256
`3f3b2f2afa54b90c39f9572a6702dcf5728c92dc0c2bf00322b6058f8a0a8f06`.
Production `importResultStream` read the entire archive successfully, verifying
scenario/benchmark agreement, every table count, 1,000 complete runs and all
588,000 elected-seat records. This is import validation, not browser UI acceptance.

The standalone runner uses `.mts`: `.ts` outside `web/` otherwise selects CJS
and rejects top-level await. Kept Node worker measurements in the sidecar run
report; reserialized the saved tables without rerunning simulations to omit
optional browser resource-estimate fields from the canonical export.

No new constituency donor exhaustion, cap-group donor exhaustion or Camera
capacity mismatch occurred. All eleven old constituency integer removals involve
SCN; all seven old capacity-conflict transfers likewise involve SCN. Replaced
examples 1.a and 4 with `esempio non trovato`; retained and updated those comments
for 1.d and 1.e. Preserved Luca's removal of simulator workarounds from the note.

Replaced the zero-vote tie example with old archive `(1)`, run 332, Senate VdA:
two top candidates each receive 10,930 positive votes and share the synthetic
birth date 1977-01-01, defeating the express S21(2) age criterion. Rechecked the
official provision and matched the replay's 47 national list totals and 73
transfers to the archived rows. The note explicitly labels synthetic birth dates.
Old positive-decimal ties 556/798 and new ties 570/621/843 only change correction
order in the checked cases: reversing each draw leaves its entire 588-seat
ledger unchanged. They were not used as necessary outcome-deciding examples.

Regional overrides do not fill intra-region historical holes: Camera colleges
080002 and 190203 still have zero SCN votes in every new run. This concrete
generation limitation is recorded in the research README and verification report;
no selected note example relies on it. The note explicitly retains the original
frequency table as original-cohort counts, including its SCN cases. No frequency
recalculation, runtime changes, Word regeneration or migration-step completion
is claimed. Scenario/evidence assertions and `git diff --check` pass.
