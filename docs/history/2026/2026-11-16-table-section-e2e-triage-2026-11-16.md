## Table-section E2E triage (2026-11-16)

Inspected the saved 73-test HTML report: 64 passed and nine failed with 14 actual
Playwright workers. Five failures were stale UI assumptions after moving stored
tables inside the collapsed `Tabelle` result section: four lifecycle cases
looked for the removed `.result-tables` wrapper, and diagnostic inspection tried
to edit the hidden simulation filter. Added the idempotent `openResultTables`
fixture, selected the section by its exact accessible region name, and opened
it before interaction and after reload. Preserved existing data assertions and
the staged application changes.

The other four failures found their territorial chart panels but still saw
`caricamento…` after the default five-second assertion timeout. They were not
missing targets. The territorial view already mounts its selected panel with
`eager`, which requests statistics immediately rather than waiting for viewport
intersection. Those four tests passed unchanged both in the affected-file run
and the subsequent full run. Resource contention from the original 14-worker
run is a plausible explanation, not a proven backend defect.

Verification from `web/`, Windows, root base, Chromium, no retries:

- `npx playwright test common-politics-lifecycle.spec.ts reform-messages.spec.ts
  reform-statistics.spec.ts --project=chromium --workers=2 --reporter=line`:
  17/17 passed (1.2 minutes).
- `npx playwright test --project=chromium --workers=4 --reporter=line`:
  72/73 passed (3.0 minutes). All nine originally failing cases passed. The
  separate editor test `invalid imports and missing pins...` timed out waiting
  for `Dati esatti non disponibili` while model preparation and file loading
  were still active.
- The unchanged editor case passed alone with
  `npx playwright test common-politics-editor.spec.ts --grep
  'invalid imports and missing pins' --project=chromium --workers=1
  --reporter=line` (18.4 seconds). This does not establish a fully passing
  combined suite.
- Production builds and `git diff --check` passed; Vite retained its large-chunk
  warning. No application code, worker configuration, assertion timeouts or
  expected result values were changed by this triage.

Proposed follow-up: bound default E2E parallelism (initially four workers), wait
for model readiness after edits where a test assumes an idle workspace, and
give asynchronous import/statistics completion assertions explicit operation
budgets instead of relying on the five-second default. Preserve the error,
draft-integrity and numerical assertions. These changes remain proposals;
the current evidence does not demonstrate an election-result regression.
