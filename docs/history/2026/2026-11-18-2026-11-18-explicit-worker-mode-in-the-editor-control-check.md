## 2026-11-18 — Explicit worker mode in the editor control check

- The next Luca-run development gate passed data validation, both type checks
  and all 1,382 units, then reached the worker-control browser check after twelve
  passing browser cases. Its trace showed a ready editor with automatic workers
  selected and no failed requests or browser errors. This is separate from the
  earlier `ERR_NO_BUFFER_SPACE` startup failure.
- Moving this check away from `openCommon()` had removed its implicit fixed-two-
  worker setup. The authored default uses automatic workers, so the numeric input
  was correctly absent. The test now selects explicit worker mode, asserts that
  the input is visible, enters 26, waits for readiness and retains the memory
  warning/value assertions. Application behavior and timeout budgets are unchanged.
- Luca authorized retrying only this case. It passed in Chromium at `/elezioni`
  in **2.9 seconds**, with one worker, zero retries and no skips, against the
  existing static build. Complete output and structured evidence are in
  `data/derived/worker-control-fix/browser.log` and `browser.json`.
  `git diff --check` passed. No other test, build or preparation ran. The full
  suite remains for Luca, and no startup recovery or buffer-error workaround
  was implemented. No commit, push, merge or publication occurred.
