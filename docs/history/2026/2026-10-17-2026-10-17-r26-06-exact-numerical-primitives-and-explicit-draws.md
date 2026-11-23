## 2026-10-17 R26-06 exact numerical primitives and explicit draws

Implemented the [numerical execution boundary](../../../web/src/lib/politics/reform-2026/README.md#numerical-execution-r26-06)
and marked R26-06 complete. R26-07 territories and seat endowments is next.
The implementation is isolated under the reform directory and exported through
its public index; no reform algorithm is registered or activated.

### Delivered boundary

- Nonnegative rational arithmetic uses bigint intermediates and reduced decimal
  string fractions at JSON/worker boundaries. Comparisons, integer/decimal parts,
  ratios and threshold comparisons remain exact beyond JS's safe number range.
  Unsafe number inputs and undefined/negative fraction operations are rejected.
  `truncateToSixDecimals()` computes `floor(x * 10^6) / 10^6` exactly.
- `allocateByQuotaAndRemainders()` separates integer quotas (truncate the quota
  first) from exact quotas used by TAA/attribution and normalized active indices.
  Callers provide exact figures, explicit secondary criteria at the prescribed
  geographic scope, terminal draw rules and optional residual-seat exclusions.
  Results expose raw/applied quotas, the exact initial-seat total, integer seats,
  vote remainders, quotient decimal parts and single-use remainder states.
- I04 outcomes are explicit: zero seats completes without division; no subjects,
  zero positive-seat vote total, zero integer quota, initial overallocation and
  impossible residual allocation return coded non-computable results with no
  assignments. Feasibility is checked before drawing; overallocation totals are
  checked exactly before conversion to safe seat counts. Malformed primitive
  arguments throw separately from these legal-domain failures.
- `resolveTie()` applies successive exact ascending/descending criteria, tracing
  surviving alternatives. Only a final unresolved tie consumes the injected
  resolver. Canonical ID ordering makes draws independent of source-array order;
  list presentation order, electoral figures, dates and percentages remain
  explicit caller-selected criteria. Injected trace/request inconsistencies are
  rejected instead of recording an ineligible or misidentified outcome.
- `createSeededDrawContext()` owns a private RNG seeded by the supplied seed and
  stream ID, with integer rejection sampling. No default/global RNG is added.
  Every I03 fallback returns a simulation-specific `warning` linked to its draw;
  express statutory draws retain their distinct rule and do not emit I03 warnings.
  The README requires downstream composition to preserve both returned arrays
  and the owning stage's applied interpretation versions.

### Verification and limits

- Added **49 numerical/tie tests**. Independently worked anchors cover ordinary
  `(10,3,1), 6 -> (5,1,0)` versus exact-quota `(4,1,1)`, and the arithmetic for
  N04 `(60,30,10), 7 -> (4,2,1)`. Tests inspect quotas and remainder values/state,
  scope-specific secondary criteria, list-order/age/percentage priorities,
  injected draws, six permutations, seeded stream isolation, per-simulation I03
  warnings and explicit failure paths. A constructed I05 arithmetic fixture
  changes a seat after truncating both indices; it does not execute the cap stage.
  Safe-number boundary and larger exact-intermediate fixtures round-trip through
  JSON. Small exhaustive exact-quota cases verify conservation and single use.
- On Windows, Node **v24.20.0**, npm **11.19.0**, from `web/`: `npm run check`
  reported **zero errors/warnings**; `npm run test` passed **26 files / 293 tests**
  in 4.37 s, including existing politics/regional and legacy allocation regressions.
  `npm run build` produced the static site successfully (Vite plugin timing
  advisory only). Subsequently `npm run test:e2e -- --reporter=line` passed
  **all 7 Chromium tests** in 13.3 s, including both worker workflows and result
  import/restore. Build and browser tests ran sequentially; Playwright also ran
  its configured build before starting preview. Browser output included existing
  color-environment notices and the Vite timing advisory, with no failed tests.
- All **113 local file/section links** across the root plan, reform documents,
  history and core README resolved. `git diff --check` and a separate whitespace
  scan covering all 17 reform TypeScript files, including untracked additions,
  passed.
- Statutory stage attribution remains R26-08/10, target corrections/caps
  R26-12 through R26-15, and candidate recovery R26-17. The generic six-decimal and tie
  helpers do not infer which provisional policies a caller must record. Undefined
  percentages and inactive zero-target groups must be guarded by the owning
  stage; no undefined value is converted to zero. Simulation-level warning
  propagation will be exercised when actual stages are composed in R26-18.
- Legacy helpers, scrutiny, seeds, registry, scenarios, generated data, workers
  and UI were not changed. No statistical fitting or performance benchmark was
  run; this step adds constructed numerical execution, not reform application
  acceptance. The existing legal specification/policies were implemented without
  introducing a new interpretation or changing the statutory transcription.
