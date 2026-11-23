## 2026-10-20: Territorial eligibility review and provisional I12

Investigated the benchmark's `R26_RUN_ERROR` with message `Supply a validated
seven-slot slate and its college figures`. Replay of simulation 19 showed
successful generation/validation, followed by an N20 cap transfer to a
special-only Valle d'Aosta group with zero votes and no ordinary slate in
Lombardia. N23 then threw for its missing college slate. The old report's
"Generation/run" category did not establish a generation failure.

Official-source findings and decision:

- Reopened the promulgated GU amendment articles 1 and 2, including
  C18-bis(2-bis), C31(4-bis), S1(4-ter), S9(2), S11(3-bis), the territorial
  allocation rules and S16-bis(1-ter)(b). Filing/ballot/proclamation structure
  supports territorial presence; special-only groups need not file elsewhere.
  Found no decisive official instruction expressly authorizing or excluding
  an absent-list recipient in the precise cap-remainder case. Apply Luca's
  explicitly authorized **provisional I12-v1**. Exact source links, evidence
  versus inference, and the dated search limits are in the
  [I12 source review](../../politics-reform-2026-specification.md#territorial-eligibility-and-zero-remainders-i12).
- Targeted official GU/Camera/Senato/Interior Ministry/judicial searches did
  not establish either proposed rule that zero votes produce no remainder,
  or that a zero remainder cannot receive a remainder seat. Applied **neither**
  change; no provisional numerical exclusion was authorized.

Implementation:

- Added one filed-slate coverage helper. A list needs an ordinary filed slate
  in the Camera constituency/Senate region; a coalition needs at least one
  filed member, and internal list splits use each member's own coverage.
  National admission, special votes and national bonus eligibility are retained.
- Camera N15/N16/N17 matrices carry explicit cell eligibility. Absent cells
  cannot receive initial or corrective seats; their absence is distinct from
  a filed zero-vote cell. Senate N19 supplies explicit regional subject
  eligibility; N20 filters both unused-remainder and regional-vote recipients,
  and N21 restricts coalition-list remainder eligibility. Keep zero targets
  in complete communications. A region with no permissible recipient produces
  `no-eligible-recipient` before any unpaired seat change.
- Composed results retain `I12-v1`; chambers with admitted lists absent from
  ordinary territories emit `I12_TERRITORIAL_ELIGIBILITY`. Candidate exhaustion,
  I07/I10/I11 recovery, special allocation and partial college coverage inside
  an otherwise filed territory keep their existing semantics.
- Added ten constructed regressions, including an admitted special-only Senate
  group through full 588-seat scrutiny, coalition/standalone Camera corrections,
  no-recipient failure and both filed-zero-vote and positive-vote/zero-remainder
  recipients. Two initial failures concerned an old bundled-seat fixture's
  stale slate college IDs and an exact-object expectation missing the new
  eligibility field; corrected those fixtures. A new synthetic coalition fixture
  initially had only integer-seat donors and correctly blocked; revised its
  inputs to exercise remainder transfers. Type checking also caught two
  readonly fixture assignments, replaced with immutable copies.

Verification:

- `npm run check:data`: pass. `npm run check`: zero errors/warnings.
- `npm test`: **866 tests, 61 files, zero skips**, including the legacy politics
  and regional suites. Production bundles were present.
- `npm run build`: pass. `npm run test:e2e`: **21 passed at root**; repeat with
  `BASE_PATH=/elezioni`: **21 passed**. Build and browser suites ran sequentially;
  no deployment was performed. Color-environment/plugin timing notices did
  not affect successful exits.
- Direct replay of recorded benchmark simulation **19**, unchanged scenario,
  data and seed: **complete, 588 seats, zero non-computable stages**, no ordinary
  entitlement for the special-only group, and the explicit I12 warning.
- Replayed **all 1000 recorded simulation IDs** through generation and composed
  scrutiny in four isolated Node workers, using the recorded scenario,
  `r26-default-v1` seed and digest-verified production bundle. Outcome:
  **823 complete, 0 partial, 177 structured non-computable, 0 exceptions**
  (hence no `R26_RUN_ERROR`, versus 86 in the historical browser report).
  All 1000 runs remained in the denominator. Other legal-stage blockers remain;
  their revised counts are separate evidence, not conclusions about their legal
  cause. There were 1000 explicit I12 warning occurrences. Reproducible runner
  and exact summary are under `data/derived/r26-i12-review-20261020/`
  (`replay.mjs`, `replay-summary.json`), including baseline-report and runner
  digests. This is a deterministic legal-output replay, not a browser benchmark
  or a new latency/memory acceptance measurement.

The historical browser benchmark report remains unchanged. Revised legal output
must not be described as reproducing its old frequencies or timing measurements.
