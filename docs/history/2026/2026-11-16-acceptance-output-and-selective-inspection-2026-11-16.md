## Acceptance output and selective inspection (2026-11-16)

- The production acceptance driver now saves each subprocess's complete combined
  stdout/stderr under `ACCEPTANCE_OUTPUT/logs/`, recording its log path, elapsed
  time, exit code/signal and status in `report.json`. Console output contains
  starts, minute heartbeats, completion/count summaries and bounded failure
  excerpts. Structured unit/browser reports and the strict no-skip/no-retry
  validators remain unchanged. Validation failures after exit zero still fail.
- National resource campaigns use a compact Playwright reporter plus a full
  JSON reporter. Complete stdout/stderr and test errors remain in `.log` and
  `.playwright.json` files beside the original receipt. New `.summary.json`
  sidecars expose actual legal outcome counts, approved/unexpected failure
  counts, infrastructure/page errors, model version/workers, timings, sampled
  memory and archive verification with evidence paths. Unknown values remain
  `null`; approved failed elections remain failed membership. No model, legal,
  seed, simulation or archive acceptance criterion changed.
- Campaign console progress is one line per minute and phase change across
  preparation, execution and archive work. Actual UI counters are retained when
  available; missing counters are not invented. Five-second memory sampling and
  thirty-second execution receipt saves remain intact. Shared full-panel checks
  now print a short result and file path instead of duplicating their receipt.
- Added [acceptance reporting](../../acceptance-reporting.md) as the focused guide to
  file locations and selective PowerShell inspection. Stable agent instructions
  now require starting with summaries, selecting failed checks/fields/IDs and
  bounded source ranges, and avoiding repeated empty polling and unchanged
  report dumps. Full evidence remains on disk.
- Verification on Windows / Node 24.20.0: both `npm run check:data` and
  `npm run check` passed (zero Svelte diagnostics); the complete two-worker unit
  suite passed **1,351 tests**, no pending/TODO/failures, in 63.7 seconds. The
  focused output/legal/report tests passed **10/10**, including large-output
  retention, bounded excerpts, spawn/exit failures and genuine progress counts.
  The static build passed; its full log and verification receipts are under
  ignored `data/derived/acceptance-output-verification/`.
- Tiny Playwright tests, without browser fixtures or electoral simulations,
  exercised successful and deliberately failing reporter runs: 20,403/30,552
  characters remained in full logs while console output was 527/4,180 characters.
  A synthetic npm shim exercised the real gate driver: success completed all 12
  subprocess stages; exit 9 and an exit-zero skipped-unit report both failed the
  gate with linked evidence. These are reporting tests, not production acceptance.
  Replaying all four existing 1,000-ID receipts through the formatters preserved
  outcome/error counts, actual workers and archive verification: summaries were
  about 1.3 KB and final console lines 440–448 characters. Original receipts
  and archives were read only.
  Resource configuration was inspected with `--list` only; no new resource
  campaign or full application browser matrix ran. An initial build-verification
  wrapper guessed the wrong local npm CLI path; invoking installed npm directly
  resolved that verification-only failure.
