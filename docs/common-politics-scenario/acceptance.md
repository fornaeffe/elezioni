# Common politics acceptance (CP-08)

Use this guide for development/release test selection and national campaigns.
[Hosting](../hosting.md) owns hosted cold/warm reproduction and publication;
[acceptance reporting](../acceptance-reporting.md) owns evidence inspection.
Historical CP-08 receipts below retain their original model identities; current
adoption is recorded in [November model acceptance](../model-acceptance-2026-11.md).

## Choose a profile

Luca launches test sequences. Agents may inspect results and run compact checks
when authorized, but must not start long campaigns themselves.

| Command from `web/` | Intended use | Required browser membership |
| --- | --- | --- |
| `npm run acceptance:dev` | Routine work and automatic push checks | 66 general fixture/UI checks + 1 controlled editor cancellation check at `/elezioni` |
| `npm run acceptance:reform` | Release application acceptance | 70 general checks + 1 editor cancellation + 2 production archive workflows at `/elezioni`; 1 root routing/worker smoke = 74 |
| `npm run acceptance:extended` | Optional manual broader diagnosis | 70 general + 1 editor + all 8 production archive combinations at each base = 158 |

These are selected-case contracts, not newly measured outcomes. Every profile
requires production bundles, `data:check`, both type checks and all unit tests.
Release also requires the real national previous-law execution and three
residual-list lifecycles tagged `@production`; development excludes those four.
The four constructed law/model lifecycle cases remain required in every profile,
including real worker execution, checkpoint reuse, archive round trips, worker
row equivalence, cancellation, quota/native failures and exact run membership.

The representative production archive workflows are Firefox Rosatellum/M02
(first) and Chromium reform/M02, with three draws each at `/elezioni` to exercise
two compute workers. The extended profile additionally runs M01 and both laws
in both browsers, with one draw per workflow at root. Six real staged-site
worker/route/export checks remain separately required by the cold hosting gate.

`policy.ts` records profile names and exact browser counts. Reports reject wrong
counts, skips, TODOs, expected failures, retries and missing data. All browser
commands fail fast, save line+JSON logs, and retain failure traces per suite/base.
There is no repeated diagnostic preflight. A smaller profile never authorizes a
partial report from a larger profile. The report identifies `policy` and `profile`.

## Local commands

With Node 24 and dependencies installed, prepare missing/stale assets first:

```powershell
npm run data:prepare -- --download
$env:ACCEPTANCE_UNIT_WORKERS = '2'
$env:ACCEPTANCE_BROWSER_WORKERS = '1'
$env:ACCEPTANCE_OUTPUT = '../data/derived/acceptance-development-review'
npm run acceptance:dev
```

For release confirmation after development succeeds, choose a separate output:

```powershell
$env:ACCEPTANCE_OUTPUT = '../data/derived/acceptance-release-review'
npm run acceptance:reform
Remove-Item Env:ACCEPTANCE_UNIT_WORKERS, Env:ACCEPTANCE_BROWSER_WORKERS, Env:ACCEPTANCE_OUTPUT
```

Without `ACCEPTANCE_OUTPUT`, each profile writes to its own
`data/derived/acceptance-{development,release,extended}` directory. Preserve prior
important evidence before reusing an output directory. Keep builds and browser
commands sequential. Development leaves a project-base build; release/extended
build root last. Run an ordinary `npm run build` for the usual local root build.

`npm run test:e2e` selects the development general suite only;
`npm run test:e2e:all` selects all 70 general cases. Dedicated editor, root and
production workflows have their own configurations and are included by the
profile runner. `ACCEPTANCE_FULL_MATRIX=1` enables all eight combinations for a
standalone unified-suite invocation; the profile runner sets it explicitly.

Unit/browser worker limits are independent integers 1–64. The legacy
`ACCEPTANCE_TEST_WORKERS` is a fallback for either unspecified limit. Defaults
remain two unit workers and one CI browser context (two locally); hosted jobs
explicitly use 2/1. Application compute pools retain their normal contracts.

## Fixture and readiness boundaries

Editor-only specs retain real pinned asset loading, import validation, forms,
serialization and persistence, while `editor-preparation.ts` supplies controlled
worker preparation responses. It rejects election execution/checkpoint creation;
it cannot provide production fitting evidence. Result-service requests still
reach the real worker. Presentation fixtures use the same boundary while seeding
real result storage from the neutral home route. Controlled cancellation tests
UI draft preservation; real worker cancellation remains in the constructed
lifecycle suite. Worker-count/warning controls use this UI boundary too.

Removed repetitive browser checks: three of the four legacy v1–v4 rejection
cases (one representative retained), the alignment/placeholder-only case, and
the separate production checkpoint/cosmetic editor case whose core behavior is
covered by constructed lifecycle and file/execution units. All retired versions
remain enumerated in fast parsing tests. One mobile/keyboard editor check remains.

Real initial fitting and refitting both have a five-minute readiness cap;
production workflows allow ten minutes overall (the multi-refit residual M02
case retains fifteen). Controlled UI readiness has a 20-second cap. The active
status toggle determines readiness; detailed progress is included on timeout.
These are maximum waits, not added sleeps. Legal output, deterministic seeds,
explicit zeros, conservation and failed/partial membership expectations remain.

Synthetic storage/chart fixtures can contain more than 1,000 saved rows for
pagination/denominator checks; they do not establish electoral throughput.

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
