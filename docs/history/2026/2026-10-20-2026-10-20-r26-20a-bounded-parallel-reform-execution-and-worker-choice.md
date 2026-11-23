## 2026-10-20: R26-20a bounded parallel reform execution and worker choice

Completed the approved pre-R26-21 parallelization, including Luca's requested
resource-based maximum/default and explicit user choice. Operational contracts,
the capacity formula and benchmark command are in the
[application guide](../../politics-reform-2026-application.md).

- Added a compute task for one complete global simulation ID, keeping joint
  chambers together and single renewals single. Generation, legal algorithms,
  RNG namespaces and model/source bundles are unchanged. Each compute worker
  loads and verifies its own production bundle and prepares its references once.
- Added a bounded pool with one outstanding simulation per worker. A single
  result-service coordinator drains and durably commits ascending IDs, reusing a
  slot only after commit. It receives pull-driven transferable columnar buffers
  targeted at 256 KiB/256 rows, retaining the existing 16 MiB record guard.
  Shared context is emitted once by simulation 1; presentation helpers aggregate
  per-run counters. There is no full-result message or growing reorder queue.
- Cancellation finishes the already assigned contiguous prefix and stops new
  dispatch. Compute/native-worker/transport failures interrupt the session;
  quota/write failures preserve the last published manifest and hide incomplete
  chunks. Neither path rerolls or converts infrastructure failure into a legal
  non-computable run. Pools close on completion/failure; a browser test also
  verifies that closing their coordinator leaves no compute descendants.
- Added **Processi di calcolo**, from 1 to the estimated maximum, initially
  selecting that maximum. The preference is saved separately from the scenario
  and clamped to current capacity on reload. Actual pool size also respects the
  requested simulation count. UI and coordinator share strict count validation.
  Archives preserve optional chosen/actual/maximum counts, memory-estimate
  identity and accumulated phase/storage durations; older archives remain valid.

Memory estimation and scope:

- A production-data Node 24.20.0 probe of IDs 1–20 sampled after generation,
  scrutiny, presentation and encoding, without forced GC, while retaining raw
  and prepared snapshots. Largest observed used heap: **929,540,912 bytes**;
  process RSS: **1,147,371,520 bytes**. This includes Node/loader costs and is a
  stage-boundary observation, not an absolute peak or browser measurement.
- Adopted versioned **1,536 MiB per compute worker**, plus **512 MiB for the
  coordinator**, using half of reported device memory and reserving one reported
  logical processor. Missing/invalid hints conservatively permit one worker.
  Reported memory is approximate total RAM, not available RAM. The UI explains
  the estimate and permits choosing fewer workers; there is no fixed four-worker
  cap. Native Chromium hints on this machine give a maximum/default of **10**,
  exercised by the production default-pool browser test.
- The production matrix's largest sampled compute-worker heap was **265.33 MiB**;
  its largest combined coordinator/compute heap sample was about **859.59 MiB**
  with four workers. Sampling was periodic and unforced; these values are not
  absolute peaks, retained-after-GC measurements or process RAM. Retained-heap
  samples alone were not used to reduce the conservative resource budget.

Production performance verification on Windows x64 / Node 24.20.0 /
Chromium 148.0.7778.96 / Intel i7-14700KF / 63.79 GiB physical RAM, using
`politiche-reform-2026-v1`, snapshot `r26-generation-v1-c5c4be9fafb5`, model
`r26-models-v1-f71a72020aaa` and seed `r26-default-v1`:

| Requested simulations | 1 worker | 2 workers | 4 workers |
| ---: | ---: | ---: | ---: |
| 10 | 16.228 s | 14.439 s | 12.690 s |
| 100 | 110.948 s | 41.310 s | 24.262 s |
| 1000 | 1033.859 s | 380.101 s | 206.564 s |

- `npm run benchmark:reform`: **all nine matrix cases passed**, about 30.7 min
  in the test. Controlled benchmark hints (8 logical processors / 16 GiB) allowed
  1/2/4 workers under a recorded maximum of 5, independently of native privacy
  limits. Each run included new pool setup and production load/verification.
  The single ordered sweep included CDP sampling; these are machine-specific
  comparisons, not portable speedup guarantees or comparisons against another
  instrumentation setup. Four workers were about **5.0 times** faster than the
  matching one-worker 1000-run case.
- Every worker count preserved the same outcome, table-row and warning counts.
  At 1000 requested runs: **698 complete, 302 non-computable, 468 bonuses**.
  Known legal/model failures, including the generation failure at ID 19, remain
  visible with the original denominator. Complete normalized archive content
  (all tables, draws, warning rows and shared metadata counts) also matched in
  constructed browser runs with 1/2/4 workers.
- The [benchmark report](../../../test/fixtures/benchmarks/browser_reform_workers.json)
  records all cases, phase durations and heap samples. Phase durations use
  `performance.now()` inside workers, may overlap and are not process CPU time
  or additive end-to-end latency. Storage includes decoding, compression and
  durable writes; presentation timing covers row derivation.

Other verification:

- `npm run check:data` and `npm run check`: **zero diagnostics**.
- `npm run test`: **852 tests in 58 files passed**, no skips, 9.22 s in the final
  run. Added capacity/fallback validation, bounded out-of-order completion,
  cancellation, failure/identity handling, chunk transport/presentation parity
  and execution-metadata roundtrip checks.
- `npm run data:reform:check`: verified pinned checksums; production simulation 1
  complete, **41 stages / 588 filled seats**, no missing production data.
- Root and `/elezioni/` static builds and browser suites ran sequentially.
  Final root regression: **20 passed**, no skips, 58.2 s, restoring the default
  root build after project-base verification.
  Project-base regression: **11 passed**, no skips, 44.1 s, including paired and
  default-sized production pools, recovery, native worker crashes, quota failure,
  archive equivalence, legacy selector/navigation and the regional workflow.
- Svelte MCP reported no component issues. Its suggestions concern the existing
  asynchronous page/chart/warning effects. Visually inspected the new control,
  memory explanation and existing results in the project-base screenshot.
- During development, fixed a missing required timestamp in a new test, gave the
  new selector an explicit accessible name, and replaced a Vite-only defaults
  import in the benchmark with reading the catalog-selected JSON file. These
  were test/accessibility setup issues, not changes to legal/model behavior.
- Final `git diff --check`: passed.

R26-20a is complete. R26-21 legal/application acceptance and the whole-migration
publication gate remain open. No commit, source/data regeneration or deployment
was performed.
