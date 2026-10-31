# Unified reform generation workflow (FV-08)

This is the active scenario/editor/execution contract for reform scenario **v5**.
The selected bounded mass-pair field is the default. The alternative is the
**latest-publication legacy poll procedure** from the reviewed comparison,
with the approved per-chamber new-list classification and choice-set/turnout
policy below. The older pooled
poll estimator is not this alternative. Public deployment remains at the final
migration gate. FV-08 acceptance is complete; see the
[accepted measurements](vote-generation-fast-acceptance.md) and
[curated snapshot](../test/fixtures/benchmarks/vote_share_unified_acceptance.json).

| Task | Entry point |
| --- | --- |
| Edit shared scenarios or select a model | `vote-share/FastScenarioEditor.svelte`, `GenerationModelEditor.svelte` |
| Edit memberships or legacy composition | `FastEvidenceEditor.svelte`, `SourceCompositionEditor.svelte`, `correspondences.ts` |
| Compile original data for legacy | `legacy-adapter.ts`, `legacy-polls.ts` |
| Dispatch either model | `unified-generation.ts`, `workers/reform-compute.ts` |
| Change worker recommendations/warnings | `core/reform-worker-capacity.ts`, `vote-share/fit-resources.ts` |
| Generate defaults | `scripts/data-preparation/unified-share-default.ts`, existing release command |
| Verify complete browser runs and archives | `playwright.unified-share.config.ts`, `tests/e2e/unified-share.spec.ts` |
| Measure full refits and maximum numerical dimensions | `playwright.unified-resources.config.ts`, `tests/e2e/unified-resources.spec.ts` |
| Review measurements and outcome | [Migration history](migration-history.md) |

Source paths above are relative to `web/src/lib/politics/reform-2026/`, except
`core/` and `workers/` (relative to `web/src/lib/`) and `scripts/`, `tests/` and
the Playwright configuration (relative to `web/`).

## Membership and assumed composition

Keep the [maintained numerical notation](vote-generation-fast-model.md):
$S_i$ is the binary $G_i\times K$ membership matrix used by the selected model,
and $u_i=\mathbf1^\top S_i$ is its offered set. **Do not reuse $B_i$:** that symbol
already denotes a sampling/rounding covariance in the numerical contract.

Store structural members independently from legacy nonnegative relative weights.
For the legacy calculation normalize each nonempty source row into $W_i$:

$$
(W_i)_{gj}=\frac{w_{igj}}{\sum_k w_{igk}},\qquad
C_{i,f}=W_iS_f^\top.
$$

$S_f$ is binary future membership, separately for the applicable chamber and
physical choice set. Never normalize its rows: a future joint list receives the
sum of its components. Membership remains present when its assumed legacy
fraction is zero. Columns cannot occur in competing groups on the **same**
ballot/question; different territorial ballots may have different tickets.
Original grouped counts remain indivisible. Candidate pooling, unresolved-basis
rejection and reference-supported fallback components are retained.
Legacy poll points are projected independently onto Camera and Senate future
memberships; a union of both tickets would duplicate or misattribute components.
The latest-publication calculation retains the compared wave selection,
normalization and measurement rules. A list lacking mapped history in the
current renewed chamber uses the new-list exception even if the other chamber
has history. Luca explicitly approved this per-chamber classification on
31 October; the original comparison classified lists across renewed chambers.
Duplicate component assignments within a poll question and poll proxies for
unmeasured identities are rejected, including in partial named observations.

Scenario `generation` stores the selected `model`, `compositions` keyed by
original choice ID (poll rows use `poll:<question ID>:<label>`), and the explicit
partial named `supplementalPolls` used in the comparison. Composition rows retain
relative weights (the storage field is named `fractions`) and provenance;
normalization occurs at the model boundary, so editing 60:40 preserves that
entered ratio. Reviewed named opaque TAA groups have separately
labelled **legacy-only proxy** decompositions; those proxies never identify a
candidate fitted split or replace the candidate's opaque observations.

Default numerical weights come from `data/legacy-comparison-policy.json` and
`data/legacy-comparison-future-polls.json`. These are now active preparation
inputs and must survive FV-09 cleanup. An explicit equal-allocation action is
available. Partial named observations are editable legacy inputs; they are not
silently promoted to complete candidate polls. Full poll editing remains shared.

Missing original observations/denominators and unavailable identities derive
from source masks, source dates and the represented choice set. Other is its
dated offered complement, constructed before candidate pooling. No extra
user-authored missingness matrix is required. Date/status exclusions are
automatic; explicit deselection records a user exclusion. Missing is never zero,
and an unnamed poll Other is not a separately measured new-party point.

## Effective inputs and turnout policy

Both engines receive the same source/geography/electorate and future ballot.
The candidate uses its eligible historical/poll estimation inputs. Legacy uses
the latest available same-date **common-18+ Camera/Senato pair** and the reviewed
latest-publication poll refresh, including its mixed-wave/new-list exceptions,
measurement floor and historical unreported remainder. With current bundled
sources that pair is 2022. Unsupported earlier dates fail explicitly.
Missing valid municipal baselines use labelled original regional proportions,
rescaled to the common conditional target electorate.

For v5 legacy, `preserve-historical-turnout-v1` applies the source-row fractions
to filed destinations, then conditions the resulting municipality's political
composition on its available choice set. Incomplete destinations and unfiled
support **do not automatically become non-valid support**. The baseline valid
fraction is preserved. If no political destination remains, use regional mapped
support over filed lists, or an explicitly labelled equal filed-list fallback.
No filed choice is an error. Original v1-v3 scenarios retain their old policy.

This aligns the separation of turnout and political composition with the
selected field. The statistical generators remain different. In particular,
legacy still reconstructs physical college counts from municipal draws, whereas
the candidate conditions its probability field at physical cells. The scheduled
post-FV-08 audit must assess incomplete mappings, ceased/new components,
unfiled support, overlapping filing domains and submunicipal capacity, reporting
turnout and list/college effects before any further policy change.
This includes national poll allocation when one component belongs to different
future tickets on geographically separate ballots: legacy retains national
list-target reconciliation, rather than acquiring the candidate's physical-cell
probability field. Shared scenario representation does not remove that legacy
approximation or establish calibrated local predictions for those cases.

Political point references, legal lists/filing, coalitions, candidates,
preferences and bonus links remain shared. Reference SDs, list uncertainty
multipliers, fallback-scale controls and partially specified within-list
component interventions remain **candidate-only**; switching engines preserves
them. Legacy uses whole-list point references and equivalent single-component
lists; it acquires no artificial SD support. Poll reference dates, territorial
shock ages and the legacy common-date approximation retain their documented
meaning. The shared explicit FN TAA point is the reviewed 7.2% scenario proxy,
not a national poll relabelled a territorial measurement.

## State, workers and compatibility

The model selector changes generation independently of the legal algorithm.
Legacy preparation does no candidate fitting. Candidate evidence fingerprints
survive legacy edits/checkpoints, so returning to the candidate requires any
actually stale estimation. Each accepted run freezes scenario, engine identity,
compiled legacy inputs or fitted candidate state and result provenance.
Sessions/results embed the compiled legacy state for ordinary offline replay;
editing evidence still requires the pinned original source chunks.

Both engines use complete paired simulations per worker, stable simulation IDs,
ordered bounded commits and the existing cancellation/result-storage pipeline.
The former candidate **512 MiB aggregate ceiling is waived**. Numerical fitting
retains its separate 256 MiB workspace/solver safeguards.

For $P$ declared logical processors, the selectable ceiling is
$\max(1,P-2)$ when $P\ge4$, otherwise $\max(1,P-1)$. Two slots cover the main
interface and coordinator; this is a CPU-based upper bound on potentially useful
parallelism, not a measured speedup guarantee. RAM does not hide selectable CPU
options. The recommendation is the largest allowed count whose estimated
coordinator plus workers fits half the declared RAM and the browser-heap
allowance below, with a minimum of one.
Missing RAM reporting defaults to one. A saved explicit user choice is retained.

Candidate accounting reserves 128 MiB coordinator scratch and 320 MiB per-worker
scratch, adding owned state/packet copies. The compact legacy adapter reserves
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
