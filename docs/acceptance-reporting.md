# Acceptance output and selective inspection

Use this guide when running or reviewing acceptance. The
[common-politics acceptance guide](common-politics-scenario/acceptance.md) owns
the criteria and campaign commands. Reporting does not change those criteria.

## Production gate

`npm run acceptance:reform`, from `web/`, writes under `ACCEPTANCE_OUTPUT`
(default `../data/derived/r26-21-acceptance`):

| File | Purpose |
| --- | --- |
| `report.json` | Start here: overall status, input identities, stage exit codes/timings/log paths and validated test counts/report paths. |
| `logs/*.log` | Complete combined stdout/stderr for each subprocess, including builds and preparation. |
| `unit.json`, `browser-*.json`, `editor-*.json`, `unified-*.json` | Full structured test results, including failures, skips and retries. |

The console prints stage start/completion, a heartbeat every minute, validated
counts and the final report path. A failed command prints only the final 3,500
log bytes; its complete output remains saved. A validation failure after exit
zero still fails the gate and points to the full test report. Failed stages
remain in `report.json` with their actual exit code or signal.

## National resource campaigns

`npm run benchmark:common-politics` (also `benchmark:politics`) writes under
`CP_RESOURCE_OUTPUT` (default `../data/derived/common-politics-resources`). Each
`MODEL-LAW-SIMULATIONS-WORKERS` filename stem has these files:

| Suffix | Purpose |
| --- | --- |
| `.summary.json` | Start here: outcome counts, legal acceptance and approved/unexpected failure counts, infrastructure/page-error counts, times, sampled memory, archive verification and evidence paths. |
| `.json` | Original full receipt: exact IDs/reasons, membership, identities and all memory/status samples. |
| `.log` | Full reporter stdout/stderr, including server output and complete test error messages. |
| `.playwright.json` | Structured test results and complete assertion failures. |
| `.archive.json.gz`, `.archive.json.gz.roundtrip.gz` | Original archive and verified import/re-export evidence. |

Console progress is one line per minute and on phase changes during preparation,
execution and archive verification, retaining real counters when the UI
provides them. Missing counters are not inferred. Archive verification has an
explicit start message; the final summary includes its outcome and timing.
Five-second memory sampling and thirty-second execution receipt saves are
unchanged. Summary counts never reclassify approved failed elections as complete.
Missing evidence is `null`, not zero. Full diagnostics and failed membership
remain in the original receipt. Pre-existing receipts need not have sidecars.

Failures show a bounded console excerpt and retain full details in the log and
Playwright JSON. A forced process termination may prevent final reports; inspect
the partial log and last durable receipt rather than assuming success.

## Read only the evidence needed

From `web/`, select gate fields in PowerShell:

```powershell
$gate = Get-Content -Raw -LiteralPath '../data/derived/r26-21-acceptance/report.json' | ConvertFrom-Json
$gate | Select-Object status
$gate.checks | Select-Object name, status, passed, skipped, exitCode, elapsedMs, log, report
```

For a campaign, read its small `*.summary.json` first. Only if legal details are
needed, parse the linked full receipt and print selected fields:

```powershell
$summary = Get-Content -Raw -LiteralPath $summaryPath | ConvertFrom-Json
$summary
$receipt = Get-Content -Raw -LiteralPath $summary.receipt | ConvertFrom-Json
$receipt.legalAcceptance
$receipt.stored.legalErrors | Where-Object simulationId -eq 242
```

Use `Get-Content -Tail 40 -LiteralPath $logPath` for a first failure inspection,
then search the full log for the relevant stage/error. For browser JSON, inspect
failing test entries and their errors rather than printing its entire suite tree.
Keep IDs and diagnostics in their evidence files; include only the cases needed
to explain the finding in the conversation. During long runs, follow heartbeats
and phase changes instead of repeatedly polling empty output or unchanged files.
