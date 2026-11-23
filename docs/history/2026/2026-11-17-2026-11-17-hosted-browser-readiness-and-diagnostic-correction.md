## 2026-11-17 — Hosted browser readiness and diagnostic correction

- Luca downloaded the `af5325764261f119a2add1a73c73c4688bfed1ac` hosted run
  into `data/github-artifacts/`. Its cold preparation passed in **350 seconds**;
  data/type checks and **1,373 units** passed. The first project-base browser
  suite ran **73 checks in 1,193 seconds: 49 passed, 24 timeout failures**, with
  no skips/retries. `browserChecks=0` meant no complete browser suite had passed
  acceptance. All four national law/model lifecycle cases passed. Available
  system RAM stayed above **9.2 GiB** on the four-CPU runner; these reports do
  not establish CPU contention as the sole cause of every stalled DOM action.
- Identified contradictory budgets (30-second tests containing 120-second
  preparation waits), editor readiness that checked only `dati verificati`,
  synthetic storage seeding competing with live initialization, and a trace
  policy (`on-first-retry`) that recorded nothing with zero retries. The M02
  residual case was still progressing at `covariance-solves · 130/180` when its
  old two-minute preparation assertion expired.
- Acceptance now records independent unit/browser worker limits. Hosting uses
  **two unit workers and one browser context**; application compute workers
  remain governed by their existing scenario/capacity contracts. Explicit new
  environment variables override the legacy shared-worker fallback. General
  browser tests default to one worker in CI and zero retries; failures retain
  traces/screenshots. Each suite/base has its own output directory, preventing
  later commands from deleting earlier evidence.
- Before the full first-base browser matrix, a **four-check fail-fast diagnostic
  preflight** covers controlled status/error UI, list creation, M02 residual
  fitting/execution and sparse result presentation. It uses one context, zero
  retries and `--max-failures=1`; all four checks must pass. Diagnostic counts
  live separately in `report.json.diagnostics` and never inflate accepted full
  matrix counts. Full project/root, editor and unified acceptance remain required.
- Shared setup now waits for actual model readiness; editor setup also requires
  verified assets. Startup waits remain bounded at **two minutes**, refitting
  at **five minutes**, ordinary politics UI tests at **six minutes**, and the
  multi-phase residual M02 case at **fifteen minutes**. General DOM actions have
  a **30-second** cap. Controlled status tests hold data acquisition explicitly;
  presentation seeding leaves the simulator for the neutral home route before
  writing IndexedDB/localStorage, then reenters the viewer and awaits readiness.
  No expected political output, legal failure, sample membership, default,
  production data pin, law or model implementation was changed.
- Added focused worker-policy validation and updated operational instructions
  in `docs/hosting.md` and `docs/common-politics-scenario/acceptance.md`. Existing
  downloaded artifacts and Luca's pre-existing `.gitignore` edit are preserved.
  Initial-trial evidence is under `data/derived/hosted-browser-readiness/`;
  final evidence is under `data/derived/hosted-browser-readiness-final/`.
  Begin with `report.json` and its linked `application/report.json`.
  Hosted Linux cold/warm confirmation still requires the committed/pushed
  correction; local timings are not runner proof.
- The initial local trial exposed a race in the first readiness helper: hidden
  status details could match an older ready entry, followed by a non-retried
  read of the still-changing visible status. It now polls only the active
  status toggle's accessible label. Residual-list transitions explicitly await
  preparation after reload and law changes before opening controls. Initial
  failed contexts/traces remain preserved; the final correction is rerun in
  `data/derived/hosted-browser-readiness-final/`. All browser commands now fail
  fast with line-plus-JSON logging; no partial matrix is accepted as complete.
- Final Windows / Node 24.20.0 application acceptance passed in **7,789.1
  seconds**: complete production-data validation, both type checks, all
  **1,380 unit tests**, all **168 full browser checks**, and **four separate
  diagnostic checks**, with zero skips or retries. General/editor/unified
  counts remain **73/3/8 at each base**. Project-base suites took
  **2,679.2 / 407.9 / 847.7 seconds**; root suites took
  **2,362.8 / 380.0 / 663.7 seconds**. The diagnostic preflight took
  **332.8 seconds**. All previously failing editor/presentation areas and the
  M02 residual lifecycle are covered by these complete matrices.
- The final project-base Pages build and all **six staged-site browser checks**
  passed; the latter took **248.7 seconds**, with zero skips or retries.
  The staged payload contains **68 files / 112,870,855 bytes**, with a
  **20,862,491-byte** gzip estimate. These are local staging measurements,
  not public-CDN transfer measurements. Final logs, JSON reports, archives and
  transfer receipts remain under `data/derived/hosted-browser-readiness-final/`.
- Production implementation, source pins, authored defaults and frozen fixtures
  are unchanged. `git diff --check` passed. No additional logs from Luca were
  needed. Commit/push the correction on `migrazione`, then require the new
  revision's hosted cold **and** warm passes before merge. No repository-variable
  adjustment is needed. The final hosting/publication task remains open;
  no commit, push, merge or publication was performed here.
