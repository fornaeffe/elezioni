# Diagnostic retry fixture and prefetch memory assessment

The [diagnostic retry regression](../../../web/tests/e2e/reform-messages.spec.ts) now uses 60 completed simulations out of 75 requested. It still crosses the 50-ID boundary, exercises two pages of general messages and summary groups, and controls worker replies for retry and ordering checks. The original 1,201-run diagnostics and disk-statistics cases retain their large-archive coverage.

Both diagnostics tests passed in focused Chromium checks. In the one-worker retry traces, the presentation fixture seeded in 0.32 seconds with 60 runs versus 2.35 seconds with 1,201 runs. The retry test itself took 2.6 versus 17.5 seconds before teardown. These are separate focused runs.

## Browser memory

In Chrome 158 on Windows, a fresh national reform/M02 preparation and one simulation with one compute worker passed. The result-read comparisons used the same saved 1,000-run national reform/M01 archive, rebuilt the statistics cache for each variant, and exercised diagnostics, statistics, and election probabilities. The overlap comparison started a new one-worker M02 simulation and selected the saved archive while it ran. Both variants completed their queries and simulations.

| Workflow | Current private / working set (MiB) | Bounded prefetch private / working set (MiB) |
| --- | ---: | ---: |
| Fresh M02 preparation and one simulation | 819 / 921 | — |
| Browse the 1,000-run archive | 681 / 772 | 716 / 810 |
| Run one simulation while browsing the archive | 1,019 / 1,103 | 876 / 982 |

Each result is a sampled peak across Chrome renderer processes, including application workers. In the tested workflow, prefetch raised the read-only peak and the existing workflow still reached a higher peak during overlapping compute and result reads. The comparison used temporary worker builds and supports the selective prefetch change as a subsequent step.

Evidence is in `data/derived/memory-prefetch-assessment/`, including the one-worker `*.summary.json`, `result-read-memory.json`, `overlap-memory.json`, and the probe script.

## Implementation and verification

Enabled the existing [bounded prefetch](../../archive-performance.md#bounds-and-ordering) for the four full-scan call sites profiled above. These serve stored diagnostics and filtered simulation-ID exports, reform statistics, and election probabilities. Ordinary table pages still use the single-chunk path. Playwright concurrency remains a manual choice.

`npm run check` reported zero errors and warnings. All 27 focused unit tests and five Chromium tests passed. The browser coverage included exact and legacy diagnostics, the full filtered ID CSV, the 1,201-run disk-statistics case, and election-probability selections. The browser log is `data/derived/memory-prefetch-assessment/applied-prefetch-playwright.log`.
