## 2026-10-21: implement LG04 recipient reuse and unlimited I15 passes

Luca confirmed LG04 and authorized reusing the greatest used recipient decimals
after unused decimals exhaust, without a limit on reuses, including removal of
I15's one-pass limit. Implemented **I16-v1** for Camera N18 and Senate N21/N22
ordinary/internal cap-list correction and updated mismatched Camera cap-group
correction to **I15-v2**. Each descending used-decimal pass visits an eligible
cell once, then restarts if deficits remain; targets are rechecked after every
seat. Unused recipients retain priority even over greater used decimals.
Absent, consumed and undefined decimals remain ineligible.

No conflict was found with allocation or recovery contracts. Each addition
reduces the finite total deficit, and additions stop at each target. A list
receiving correction seats therefore cannot become a donor later in that
matrix. Retain exact decimal values and `used -> used` state without adding
a multiplicity counter or changing the initial integer/provisional evidence.
I13 donor removal, I14 initial passes, balanced group correction and all
constituency/regional/recovery algorithms retain their prior scopes.

Each reused list seat emits `I16_REUSED_RECIPIENT_REMAINDER` with severity
`warning`, list/college/context, pass and seat counts; composed metadata records
I16-v1. I15 retains its per-constituency warning, now describing unlimited
descending passes and recording I15-v2. A transfer combining I13/I14 donor
fallback with I16 reuse preserves its primary donor `interpretationId` and
adds optional `interpretationIds` containing both policies. Located transfer
exports retain that field; existing JSON/CSV presentation needs no special
case or input/result schema-version change. Updated the specification, legal
gap register, scoped README and both active plans.

Verification:

- `npm run check`: zero errors/warnings; `npm run check:data`: passed.
- `npm test`: **62 files / 901 passed, no skips**. Includes unlimited reuse in
  independent and paired N18/N21/N22 corrections, unused-before-used priority,
  repeated descending passes across colleges, immediate target exclusion,
  genuinely empty recipient pools, I13/I16 compound transfers, recovery states,
  seeded I03 permutation/JSON replay and I15 reuse beyond one pass.
- Production regressions for **42, 49, 80, 242, 598, 792** all complete.
  Simulation 242 verifies I13/I16 annotations, final used recipient recovery
  evidence, warning/stage presentation and JSON export/import preservation.
- `npm run build`: passed; plugin-timing notices were informational.
  Prebuilt `ACCEPTANCE_PREBUILT=1 npm run test:e2e -- --reporter=line`:
  **21 Chromium tests passed**. These are functional checks, not performance
  measurements; the direct comparison also ran during part of verification.
- Replayed **all 1,000 saved acceptance IDs** against the changed implementation
  and an isolated pre-I16 module graph. Compressed/uncompressed production
  SHA-256 checks passed; snapshot `r26-generation-v1-c5c4be9fafb5`, model
  `r26-models-v1-f71a72020aaa`, seed `r26-default-v1`, saved acceptance report
  SHA-256 `bf12f43b8019326203cfb46d6366aa35145d8e0fb37c781cad5f2d0ff4b19373`.
  Results improve from **994 complete / 6 non-computable** to **1,000 complete /
  0 non-computable**, with zero partials or exceptions. Full serialized results
  for the other **994 IDs are identical**. Every complete result has 588 seats
  and 588 distinct elected candidates. All changed runs retain byte-equivalent
  upstream national/regional/list communications and satisfy every final matrix
  and territorial list target. I16 emits **seven warnings in six simulations**:
  simulation 49 also encounters and resolves a second minority-list exhaustion
  in Lazio after resolving Lombardia. This is a direct Node correctness replay,
  not a new browser frequency/performance benchmark.
- The existing constructed LG03 full input still completes with 588 distinct
  elected candidates and both recoveries `complete / all-filled`, now recording
  I15-v2. Full deterministic replay and JSON roundtrip assertions pass. The
  constructed group tests separately demonstrate the newly unlimited I15 reuse.

Reproducible baseline/comparison, changed inputs/results and per-run summaries:
`data/derived/r26-i16-review-20261021/` (`replay.mjs`, baseline source copies,
`replay-summary.json`, `verify-constructed.mjs`, `constructed-summary.json`).
Previous investigation artifacts and historical benchmarks remain unchanged.
Closed the listed error investigations in `MIGRATION_PLAN.md`; the publication
gate remains separate. No commit, deployment or publication was performed.
