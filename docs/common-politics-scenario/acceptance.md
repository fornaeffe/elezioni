# Common politics acceptance (CP-08)

Use this guide for the release gate, national campaigns and retirement boundary.
The [editor](editor.md), [lifecycle](lifecycle.md) and [files](files.md) guides own
their respective contracts. Record results and blockers in [history](../migration-history.md).

Current model adoption and its receipts are recorded in
[November model acceptance](../model-acceptance-2026-11.md). The recorded CP-08
receipts below retain their original M01/M02 2.0.0 identities.

## Production gate

From `web/`, with Node 24 and pinned sources available:

```powershell
npm run data:prepare
$env:ACCEPTANCE_TEST_WORKERS = '2'
$env:ACCEPTANCE_OUTPUT = '../data/derived/cp08-acceptance'
npm run acceptance:reform
Remove-Item Env:ACCEPTANCE_TEST_WORKERS, Env:ACCEPTANCE_OUTPUT
```

Add `-- --download` to preparation when acquiring missing originals. The gate
requires every production bundle, runs `data:check`, preparation/application type
checks and all unit tests, then builds and checks `/elezioni` and `/` sequentially.
Each base runs the full Chromium matrix, focused preparation/checkpoint controls,
and real national execution/export/import in Chromium and Firefox for
Rosatellum/M01, Rosatellum/M02, reform/M01 and reform/M02. Functional national tests use three
draws at the project path and one at root. Root is built last. Reports reject
skips, TODOs, expected failures, retries and missing data. Stop other preview
servers and keep builds/preparation sequential.

Lifecycle tests cover all pairs, immutable executed scenarios, stored queries,
reload, exact checkpoint reuse, archive roundtrips, one/two-worker row equivalence,
cancellation, quota retention, native compute failure, routing and original
denominators. Editor tests cover switching, inactive values, seeds, shared drafts,
JSON/gzip files, keyboard and mobile forms. Regional execution, persistence,
residual-category controls and navigation remain required.

Synthetic storage/chart fixtures can contain more than 1,000 saved rows to test
pagination and denominators. They do not generate electoral simulations or
establish throughput above the campaign limit. Old transport regressions use
test-only entry points; common compute/lifecycle suites test production directly.

## National performance campaigns

Run the pairs **sequentially**, without another build, fit or suite running
concurrently. Each campaign includes automatic browser preparation, generation,
both chambers' scrutiny and durable results, with authored seeds. Never reroll
a legal failure or change seeds to obtain acceptance.

```powershell
$env:CP_RESOURCE_OUTPUT = '../data/derived/cp08-resources'
$env:CP_RESOURCE_SIMULATIONS = '1000'
$env:CP_RESOURCE_LAW = 'politiche-r-parity-v1'
$env:CP_RESOURCE_MODEL = 'M01'
npm run benchmark:common-politics
$env:CP_RESOURCE_LAW = 'politiche-reform-2026-v1'
npm run benchmark:common-politics
$env:CP_RESOURCE_MODEL = 'M02'
npm run benchmark:common-politics
Remove-Item Env:CP_RESOURCE_OUTPUT, Env:CP_RESOURCE_SIMULATIONS, Env:CP_RESOURCE_LAW, Env:CP_RESOURCE_MODEL
```

The harness uses installed Chrome on Windows, one test process and no retries.
`CP_RESOURCE_WORKERS` requests an explicit count; otherwise the saved automatic
policy applies. Campaign sizes outside 1–1,000 fail. Receipts record exact model,
law, adapter, implementation, dependencies, seeds, requested/effective workers,
preparation/execution/total time, complete/partial/failed membership,
infrastructure errors, storage bounds and sampled renderer private/working-set
bytes. Memory includes workers/native allocations; samples are not guaranteed
maxima or JS heap measurements. Each fitting-plus-execution campaign must finish
in under 60 minutes. Afterwards, export/import/re-export verifies every decoded
archive byte except the export timestamp and preserves execution/membership.
Receipts report this additional timing and sampled memory separately. Exact
legal error reasons are retained before the isolated browser context closes.

A completed session is insufficient: every requested ID needs its actual legal
outcome, with no infrastructure failure, page error or crash. The legal gate is
the same political output, including expected errors, for the same votes. Per the
15 November clarification, the retained Rosatellum Camera error
`Non ho a chi dare il seggio eccedente` is acceptable when recorded as
`ROSATELLUM_EXECUTION_ERROR` / `scrutiny-failed`; preserve failed membership,
diagnostics and original denominators. The harness rejects unexplained failures,
changed errors and mismatched membership. Never reroll errors to obtain success.
Fixed-input legal regressions, including the frozen Camera failure, must pass
unchanged. Correcting that legal path remains a separate task.

## Comparison and retirement

D19 comparisons use the [CP-01 corpus](baselines.md). Fixed-input legal/checksum
evidence stays unchanged. M01/M02 retain their model laws and versions; assembly,
RNG consumption and presentation identities can differ from old draws. M00-to-M01
distribution equivalence is neither required nor claimed. Rosatellum/M01 has no
earlier executable pair to reproduce. Explain differences without retaining a
duplicate application architecture.

Installed routes use only `CommonPoliticsWorkspace.svelte`. Superseded politics
workspace/editor trees, M00 dispatch, old draft writers and the M00 default
compiler are retired. `tests/fixtures/retired-politics/` and
`retired-result-service.ts` preserve isolated old scenario/table/transport
regressions and never enter the application dependency graph. Shared legal,
numerical, regional and table-oriented export services remain independent.

Close CP-08 and its parent merger only after the full production gate and all
three campaigns meet these criteria. A receipt made before the clarification can
be assessed against the amended policy without regenerating its draws; retain
the original receipt and record that assessment explicitly. Public deployment
and legal/model corrections stay separate.

## Recorded acceptance

CP-08 and the workspace merger were accepted on 15 November 2026. See the
[final history entry](../migration-history.md#cp-08-final-acceptance-2026-11-15)
for outcomes, timings and qualifications. The [machine-readable evidence](acceptance-2026-11-15.json)
pins the original local receipts, exact execution/data identities, worker counts,
memory samples' peaks and full archive digests. Large receipts and archives remain
under ignored `data/derived/cp08-*` paths.
