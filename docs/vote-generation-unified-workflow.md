# Unified M01/M02 workflow

Current probability laws are [M01 2.0.0](models/M01.md) and
[M02 2.0.0](models/M02.md), with M02 default. Both restore the implementation
immediately before `4ee5a91`, while retaining canonical names and exact-version
checks. Model law, numerical policy, source/scenario IDs and implementation hashes
are separate. Read only the appropriate model definition for its equations.

## Task map

| Task | Entry point under `web/` |
| --- | --- |
| Public model identities/version checks | `src/lib/politics/reform-2026/vote-share/model-registry.ts`, `scenario.ts` |
| Select/dispatch models | `vote-share/GenerationModelEditor.svelte`, `unified-generation.ts` under the same source directory |
| M01 original paired counts, compositions and named polls | `vote-share/legacy-adapter.ts`, `correspondences.ts`, `legacy-polls.ts` |
| M01 list shocks and municipal-to-college allocation | parent `paired-vote-generation.ts`, `generation.ts` |
| M02 grouped fitting/geography/physical fields | `vote-share/source-compiler.ts`, `information.ts`, `profiles.ts`, `senate-adapter.ts`, `generation.ts` |
| Reference editing and scope resolution | `vote-share/FastReferencesEditor.svelte`, `reference-plans.ts`, `interventions.ts` |
| Shared candidature/preference/legal assembly | parent `assembly.ts`; scrutiny is independent |
| Authored defaults and release | `scripts/data-preparation/unified-share-default.ts`, `fast-share-release.ts` |
| Current model smoke check | `scripts/acceptance/component-policy-current.ts` (writes only `data/derived/model-rollback/`) |
| Frozen 1.0.0 historical kernels | `scripts/acceptance/archive/models-v1/README.md` |

## Model boundary and restored semantics

- M01 projects original historical compositions onto future lists, keeps historical
  valid-vote fractions, refreshes named list polls and applies list-level shocks.
  Its missing-mapping fallback is regional/equal-list; it generates municipal
  integer counts and reconstructs colleges afterwards. Municipal filing unions,
  per-list poll projection and greedy capacity allocation retain their documented
  limits. No component-pooling or half-vote correction from 1.0.0 remains active.
- M02 fits coarsened national groups, uses exact matching national baselines for
  municipal profiles, and constructs the reference field on physical filed sets.
  The default locally normalizes the field without compulsory national raking.
  Explicit list/component references precede bounded mass-pair movement; counts
  are multinomial in physical cells, with no extra late filing conditioning.
  Unidentified fitted bases are rejected rather than split using references.
- Political references preserve already constructed turnout in both models;
  non-valid references are separate. M01 accepts list references and singleton
  component references, ignoring reference SDs. M02 supports disjoint list/component
  references and calibrated reference uncertainty. Their meanings are model-specific.
- Both use the same original sources, geographic/legal scenario controls, seeded
  worker orchestration, result tables and exports. Shared inputs do not imply
  identical probability laws. No scrutiny rule is changed by the rollback.

The [correspondence audit](../research/vote-generation/docs/correspondence-policy-audit.md) and
[Camera comparison](../research/vote-generation/docs/current-camera-comparison-results.md) retain the superseded
1.0.0 decisions/results. Reproduce those campaigns at their recorded commits;
current command entry points reject 1.0.0 rather than relabelling 2.0.0 output.
The 1.0.0 correction curator is likewise historical. Existing benchmark receipts
are not overwritten or counted as fresh validation of the restored versions.

## Defaults and verification

Regenerate defaults and every fitted/profile/runtime/catalog asset through
`npm run data:reform:shares:release`; do not hand-edit snapshots. The policy restores
its pre-correction references and omits the six 1.0.0 model-derived anchors.
M01/M02 selectors, checkpoints and result provenance require exact version 2.0.0.
Saved 1.0.0 scenarios do not execute implicitly under the restored laws.

## Worker ownership and resource policy

Both models use complete paired simulations, stable IDs, ordered bounded commits,
cancellation and the shared result pipeline. Numerical workspace remains capped
at 256 MiB. The previous 512 MiB aggregate ceiling remains waived. The following
resource planning policy is retained; older measurements are provenance for its
conservative estimates, not fresh measurements of the restored 2.0.0 algorithms.

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
is a compatibility-test override. The ordinary browser accepts only current v5
scenarios with exact M01/M02 2.0.0 identities. Older formats or model versions
are rejected; no statistical conversion is performed. JSON.gz retains the exact executed scenario and
self-contained session; CSV remains table-oriented.

## Verification commands

From `web/`, Node 24:

```powershell
npm run data:reform:shares:release
node --import tsx scripts/acceptance/model-rollback-parity.ts
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
