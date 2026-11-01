# Unified reform generation workflow

Canonical current specifications: [M01 1.0.0](models/M01.md) and [M02 1.0.0](models/M02.md). Model identity/version, numerical policies, data IDs and implementation hashes are separate.

The reform web app uses the current schema-5 workflow only. M02 (`bounded-mass-pair-field-v2`) and M01
share scenario identities, original data, physical geography, legal assembly,
workers and result/export storage. Their statistical laws remain distinct.
The [correspondence audit](correspondence-policy-audit.md#adopted-corrections)
is the adopted policy contract; its initial measurements describe the frozen
pre-correction implementation. Public deployment and FV-09 cleanup stay separate.

| Task | Entry point under `web/` |
| --- | --- |
| Model dispatch / shared cell generation | `src/lib/politics/reform-2026/vote-share/unified-generation.ts`, `generation.ts` |
| Offered-set pooling, zero treatment and reference coverage | `vote-share/component-policy.ts` under the same source directory |
| M01 original-count compilation / dated poll groups / shocks | `vote-share/legacy-adapter.ts`, `legacy-observations.ts`, `legacy-components.ts` |
| M02 original partitions / identifiable groups and contrasts | `vote-share/source-compiler.ts`, `identification.ts`, `information.ts`, `profiles.ts` |
| Component reference controls / scope and chamber resolution | `vote-share/FastReferencesEditor.svelte`, `reference-plans.ts`, `reference-groups.ts`, `interventions.ts` |
| Final attributed list counts and legal assembly | `src/lib/politics/reform-2026/assembly.ts` |
| Authored default compositions and missing anchors | `scripts/data-preparation/unified-share-default.ts`, `unsupported-component-references.ts` |
| Current production checks / historical comparator | `scripts/acceptance/component-policy-current.ts`, `correspondence-policy-audit.ts` |

## Numerical and observation contract

Use policy `component-prefiling-conditioning-v2`, numerical version
`fast-share-float64-v2`, and M02 count version
`fast-share-prefiling-physical-cells-v2`. Implementation hashes and semantic keys
bind checkpoints, assets and runs; an old checkpoint is not silently upgraded.
Historical comparison implementations live under `scripts/acceptance/archive/`
and must not enter browser/worker bundles. The browser rejects older reform
scenarios/archives rather than routing them to an old generator.

Structural membership stays independent of M01 composition weights. M01
requires explicit nonnegative, provenance-bearing joint compositions, including
zero proportions. Default equal compositions are explicit authored assumptions,
never runtime missing-composition fallbacks. Future list membership is binary:
joining tickets sums component counts and does not attenuate support.

Original historical counts and offered sets survive compilation. M02
national fitting retains heterogeneous historical ballot strata and grouped poll
observations. Bounded orthogonal Gaussian elimination handles ill-conditioned strata without
adding a statistical ridge; its sufficient statistics are independently compared
with the original covariance calculation. QR orthogonality and covariance-solve
residuals are separate diagnostics. The existing matrix-free path and its residual
gate remain available when a rectangular QR factor exceeds the workspace budget. Identical observation
columns are fitted as group totals, with reference-backed component contrasts.
Other overlapping unidentified contrasts retain their original operators and
explicit reference requirements. M02 profile calculations retain their
smoothing, regional shrinkage, filter and variance conventions, comparing actual
historical choice sets rather than recording missing choices as zeros.

M01 keeps the latest eligible common-18+ Camera/Senato baseline, decomposes
original counts without early rounding, and pools regional/national support using
the audit's conditional-frequency objective. Local observed proportions determine
local relative support; missing alternatives borrow donor mass. Identifiable
local zeros receive the zero-only half-vote logit treatment using the original
electorate; positive fractional counts stay unchanged. There is no equal-list
fallback or arbitrary donor replacement for a required current reference.

## Reference, movement and filing order

1. Build unrestricted reference-date component support. Political scenario
   references target components, including components in joint tickets. Validate
   identity, scope, denominator, compatible distributions and required coverage.
2. Apply the model-specific reference uncertainty: M02 uses its calibrated
   joint reference family; M01 uses fixed scenario-reference means. Poll
   measurement error is separate: one original group realization is shared by
   its fixed-composition components, geography, ticket labels and reused chambers.
   Complete questions retain their total; partial named surveys remain partial.
3. Advance support with the M02 joint field or independent M01 component
   innovations, retaining within-component chamber dependence. M01's prepared
   geographic estimates are fixed, without extra estimation-error draws.
4. In each physical cell, remove unavailable components and deterministically
   condition the complete vector, including non-valid support. Empty remaining
   support is an error. Do not restore valid-vote totals or pre-filing margins.
5. Generate electorate-conserving counts in those cells, sum components to the
   filed lists, then aggregate to colleges. Camera TAA's proportional view is
   derived once from its physical special ballots. No municipal vote relocation,
   greedy capacity allocation or pre-filing reference-capacity solver is used.

Scoped references retain specific-over-general and explicit-chamber-over-common
precedence. Unrestricted reference calibration is permitted; final filed shares
need not reproduce references. `Models, references and conditions` exports retain
reference-date, election-date-before-filing and filed probability support, their
dates/denominators, retained mass, amplification and fallback summaries. Realized
integer list vote shares remain separate result tables.

## Defaults and verification

Luca approved explicit default anchors for IdV, Futuro Nazionale, PLD/LibDem,
+Uno, Progetto Civico and PSI/civici on 1 November 2026. Preparation obtains their
reference-date means and sample SDs from 256 deterministic reference-only draws
of the M02 **before adding these anchors**. The policy is
`M02-reference-moments-v1` in `data/fast-share-default-policy.json`.
The scenario state records the source fit/profile IDs, seed, draw count and
estimates. These are model estimates, not new observations or calibrated
forecasts. SDs apply only to M02 generation; M01 uses fixed means.
Existing scoped overrides retain precedence. Regenerate through the release
pipeline; never edit generated defaults manually.

From `web/`, use `npm run data:reform:shares:release`, `npm run check:data`,
`npm run check`, `npm test`, and the unified browser/storage/export checks.
`npx tsx --tsconfig tsconfig.node.json scripts/acceptance/component-policy-current.ts`
checks the installed current assets and reports invalid default reference inputs.
The original audit reader uses the terminal-only comparator and does not rewrite
its frozen benchmark receipt. Full model campaigns, resource observations and
actual check outcomes belong in migration history, not assumed acceptance.
After the application, full-campaign and resource suites pass, run
`npx tsx --tsconfig tsconfig.node.json scripts/acceptance/component-policy-curate.ts`
to produce the [current acceptance snapshot](../test/fixtures/benchmarks/correspondence_corrections_acceptance.json).
It rejects missing/failed reports and campaign implementation identities that do
not match the installed catalog. Earlier audit and FV receipts remain historical.

## Worker ownership and resource policy

Both models use complete paired simulations, stable IDs, ordered bounded commits,
cancellation and the shared result pipeline. Numerical workspace remains capped
at 256 MiB. The previous 512 MiB aggregate ceiling remains waived. The following
resource planning policy is retained; older measurements are provenance for its
conservative estimates, not fresh measurements of corrected algorithms.

For $P$ declared logical processors, the selectable ceiling is
$\max(1,P-2)$ when $P\ge4$, otherwise $\max(1,P-1)$. Two slots cover the main
interface and coordinator; this is a CPU-based upper bound on potentially useful
parallelism, not a measured speedup guarantee. RAM does not hide selectable CPU
options. The recommendation is the largest allowed count whose estimated
coordinator plus workers fits half the declared RAM and the browser-heap
allowance below, with a minimum of one.
Missing RAM reporting defaults to one. A saved explicit user choice is retained.

Candidate accounting reserves 128 MiB coordinator scratch and 320 MiB per-worker
scratch, adding owned state/packet copies. The M01 adapter reserves
384 MiB per-worker scratch plus actual
packet copies, calibrated conservatively against the nine-worker national
1,000-run process peak (about 4.3 GiB including the coordinator). The earlier
1,536 MiB estimate belonged to the larger original-workspace representation.
Warn when the chosen count projects to all declared RAM or more; the warning
does not prevent the authorized choice. Report estimated memory, declared RAM,
recommendation and CPU ceiling; none measures free RAM.

Device RAM does not represent allocatable browser heap. V8 documents a shared
[4 GiB pointer-compression cage](https://github.com/v8/v8/blob/main/docs/heap/pointer-compression.md)
for isolates in a process. A 26-worker campaign stalled during startup even
after splitting initialization messages; Luca independently reported an
out-of-memory failure at 26 and successful trials at 10. Both 10-worker browser
probes passed. Planning therefore also allows 3 GiB of browser heap, reserving
512 MiB for the coordinator/interface and at least 256 MiB per worker (or the
larger owned-packet estimate). On this 28-processor/64-GiB device, the default
is 10 workers for both models while 1–26 remain selectable. Above that allowance
the editor warns about browser exhaustion independently of the whole-RAM
warning. These are conservative estimates, not measured hard browser limits.

Only one owned packet crosses the page/coordinator boundary. The coordinator
initializes compute workers separately; pool width must not multiply the size
of one serialized startup message. A nonresponsive initialization fails after
60 seconds and preserves the interrupted requested denominator; it is not
automatically retried. Whole-renderer Windows process measurements
include nested compute workers; CDP enumeration alone misses some nested
workers and its heap samples are explicitly labelled partial.

The ordinary build installs the unified workspace. `REFORM_FAST_CANDIDATE=0`
is a compatibility-test override. v1-v3 imports route to their original installed
workflow; v4 remains candidate-only and v5 supports the selector. No statistical
conversion is performed. JSON.gz retains the exact executed scenario and
self-contained session; CSV remains table-oriented.

## Verification commands

From `web/`, Node 24:

```powershell
npm run data:reform:shares:release
npm run check:data
npm run check
npm test -- --maxWorkers=4
npm run test:reform:shares:release
npm run test:reform:shares:ui -- --project=chromium --project=firefox
npm run test:reform:shares:unified
npm run test:reform:shares:resources
$env:FV08_FULL = '1'
$env:BASE_PATH = '/elezioni'
npm run build
$env:FV08_CAMPAIGN_WORKERS = '2'
$env:PLAYWRIGHT_JSON_OUTPUT_NAME = '../data/derived/fv08-unified/campaign-tests.json'
npm run test:reform:shares:unified -- --reporter=json
```

After every gate passes, retain the compact reviewed snapshot with
`npx tsx --tsconfig tsconfig.node.json scripts/acceptance/unified-share-curate.ts`.
The curation command rejects missing/stale campaigns and incomplete browser,
preparation or resource evidence; it does not create a passing result by itself.

Required raw reports (relative to `data/derived/fv08-unified/`):

| Gate | Report |
| --- | --- |
| Complete application acceptance | `../r26-21-acceptance/report.json`, from `npm run acceptance:reform` |
| Dedicated release checks at both paths | `release-tests-root.json`, `release-tests-nested.json` |
| Dedicated Chromium/Firefox editor checks at both paths | `ui-tests-root.json`, `ui-tests-nested.json` |
| Resource suite | `resource-tests.json` plus its `resources/` measurements |
| Full project-path campaigns | `campaign-tests.json` plus `browser/*-nested-1000.json.gz` and receipts |
| Source-only cold Linux/offline warm | `../fv05-acceptance/isolated-acceptance.json`, from `npm run acceptance:reform:shares:candidate` on Linux |

For each dedicated suite, set `PLAYWRIGHT_JSON_OUTPUT_NAME` to its table path
(prefixed `../data/derived/fv08-unified/` when running from `web/`) and pass
`--reporter=json`. Build each matching `BASE_PATH` first; reset `FV08_FULL=0`
for functional suites. Resource measurements use the ordinary root build.
The final full campaign runs the models concurrently in two isolated Chromium
instances, with each application using its own recommended compute-worker pool.
Its timings include that contention. A frozen copy of the project build can be
served independently through the test-only `ACCEPTANCE_BUILD_ROOT` and
`ACCEPTANCE_SERVER_PORT` overrides; never replace a build during its campaign.

The final command requests 1,000 paired runs **per model** through the actual
browser editor, shared workers, IndexedDB, charts, streaming JSON.gz export and
import. Its ledger checks 588 filled seats and distinct candidates, including
392 Camera and 196 Senate seats, in every requested run. Receipts retain model,
worker count, timings, memory samples, admission/42% summaries and seat totals.
Repeat functional checks at `/elezioni`; use the release command's `--base` and
`BASE_PATH` as described in the existing release guide. Required assets never skip.

The resource suite records six actual default national/profile/paired refits,
then six 128-coordinate/512-poll numerical/profile probes over 7,904 synthetic
municipalities. Its slower-device probe constrains the private Windows benchmark
renderer processes to approximately one quarter of one logical CPU using a
[Windows job-object CPU hard cap](https://learn.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-jobobject_cpu_rate_control_information).
The cap includes the numerical worker thread. Chromium's worker target rejects
`Emulation.setCPUThrottlingRate`; throttling the page alone is not accepted as
worker evidence. The synthetic dimension probe does not make unsupported
political splits admissible or establish predictive coverage.

Chromium and Firefox are the current acceptance targets. Luca explicitly
deferred the previously reported Windows WebKit export capability issue until
it recurs in use; this does not claim Safari export acceptance. Public deployment
and research archival remain separate tasks.
