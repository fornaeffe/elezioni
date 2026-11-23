## 2026-11-02 — Production acceptance pruning and stale entry-point retirement

Applied the approved acceptance audit without changing generation or legal
scrutiny behavior. The application gate now runs `data:check` once: it already
includes the reform checksum/vertical slice, workspace/default and share-release
checks formerly repeated by the runner. It builds `/elezioni` once for complete
behavioral coverage, then `/` once for focused URL integration; building root
last also removes the separate restore build.

The required browser matrix is now:

- `/elezioni`: all 47 general Chromium checks; four Chromium scenario-editor
  checks; ten unified Chromium/Firefox checks, including both current models,
  three paired simulations with two compute workers, model/UI capacity controls,
  durable results and archive replay.
- `/`: seven route/refresh/navigation, workspace/import/cancellation and regional
  checks; both models run one paired simulation in each of Chromium and Firefox
  (four additional checks). Full numerical, statistical and editor behavior is
  exercised at the project path rather than repeated at root.

Every required browser suite now uses the same first-attempt/no-skips/no-expected-
failures report validation, including the previously unvalidated editor suite.
Unified functional checks omit heap/process instrumentation; the separate
`FV08_FULL=1` campaigns retain resource sampling and their 60-minute per-model
execution bound. No smaller draw count replaces those full campaigns.

Retired `test:results` and `benchmark:reform`, deleting their specs and the
now-unused results Playwright configuration. They used unsupported old defaults
or UI/worker assumptions. Existing reports remain historical evidence. The current
unified workflow now additionally queries indexed details for the final run,
compares those details after import, and streams/hash-compares every decompressed
byte of the imported/re-exported archive except the export timestamp. This
preserves useful detail/archive checks from the retired entry points. Existing
constructed-input 1/2/4-worker invariance, chunk-boundary, crash/quota/cancellation,
large statistical-query and legal regression checks remain. Full-source throughput
matrices and memory campaigns stay separate from routine acceptance.

Captured ten exact historical legal inputs (simulations 1, 4, 165, 31, 42, 80,
598, 792, 49 and 242), their traces and presentation context in
`test/fixtures/reform-legal-inputs-v1.json.gz` (2,691,396 bytes). Its adjacent
manifest pins compressed/uncompressed and per-input SHA-256, source catalog and
seed. `legal-regressions.test.ts` preserves the I13/I14/I16, seat/candidate,
remainder, stage, warning, presentation and obsolete-export rejection assertions
without regenerating these cases or depending on current production data.
The small production-bundle/reference integration tests remain. The one-time
capture script under `web/scripts/fixtures/` refuses overwrites; legal changes
require a reviewed new fixture version, not a refreshed failing seed.

Verification on Windows, Node v24.20.0:

- Focused frozen-legal/production tests: two files, five tests passed.
- `npm run check:data`: passed before acceptance; repeated by the actual gate.
- From `web/`, `$env:ACCEPTANCE_TEST_WORKERS='2'; npm run acceptance:reform`:
  passed on the first complete run, with **1,219 unit tests and 72 browser
  checks**, no skips/retries/expected failures. Both type checks passed;
  Svelte reported zero errors and warnings. The final root build is available.
- Recorded subprocess time: **551.8 seconds (9m11.8s)** versus **767.2 seconds
  (12m47.2s)** in the preceding saved run with the same worker bound, platform,
  Node version and identical three production-input SHA-256 values. This is a
  **215.4-second / 28.1% reduction**, despite adding full archive re-export
  comparisons. It is one local before/after comparison of the revised workload,
  not a repeated isolated performance benchmark. The production integration file took 7.15s and the frozen legal file 3.51s;
  their combined wall time is not the sum because the test processes overlap.

Raw evidence: `data/derived/r26-21-acceptance/`,
`data/derived/acceptance-pruning-run.log` and unified browser receipts under
`data/derived/fv08-unified/browser/`. Earlier acceptance evidence was preserved
under `data/derived/acceptance-before-pruning-20261102/`. Updated the acceptance,
application, results, data-preparation and unified verification instructions;
archived FV-08 curators are no longer presented as current active commands.
Local documentation-link and `git diff --check` verification passed.
The existing staged plan changes were preserved; only this task's checkbox was
updated in the working tree. No new 1,000-run predictive/resource campaign,
manual acceptance, legal correction, R retirement, commit, push or deployment
was performed.
