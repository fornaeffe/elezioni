# Common politics lifecycle and results (CP-07)

Use this guide for task ownership, fit reuse, session persistence and durable
execution. Use [editor.md](editor.md) for forms, [execution.md](execution.md) for
model/law adapters and [files.md](files.md) for strict portable formats.

## Entry points

Both `/politics` and `/politics/editor` mount the same workspace. Apply the
configured project base to navigation, assets and workers. The regional route
keeps its independent lifecycle and formats. Retired workspace implementations
remain only for CP-08 cleanup and isolated regression evidence.

| Responsibility | File under `web/src/lib/` |
| --- | --- |
| Single draft, revision guards, execution lock, independent viewed session | `politics/workspace-controller.ts` |
| Mount/disposal, editor execution port and session actions | `politics/CommonPoliticsWorkspace.svelte` |
| Worker requests, progress, failure/disposal handling | `politics/execution/client.ts`, `workers/politics-workspace.worker.ts` |
| Consumed-stage reuse, numeric-state validation and legal assembly | `politics/execution/prepare.ts` |
| One-entry durable M02 numeric cache | `politics/execution/preparation-store.ts` |
| Resource validation and requested/effective workers | `politics/execution/resources.ts` |
| Ordered durable run commits and interruption recovery | `politics/execution/stored-run.ts`; existing `workers/reform-pool.ts`, `core/result-store.ts` |
| Checkpoint construction and exact restore | `politics/execution/checkpoint.ts`, `politics/files/checkpoint.ts` |
| Staged archive storage and bounded row-membership checks | `politics/files/stored-archive.ts` |
| Pages, messages, disk statistics and streaming exports | `workers/result-service.ts`, `core/result-export.ts`; `politics/PoliticsSessionResults.svelte` |

## Ownership and reuse

The controller validates and detaches each draft replacement, writes only
`elezioni:politics:common-draft:v1`, and retains the in-memory edit on storage
failure. It ignores former politics draft keys. Preparation starts automatically
at initialization and after edits. Each reply belongs to a task and draft
revision; late progress/results cannot replace newer state. Execution locks
editing before waiting for preparation. Cancellation is explicit and preserves
the ordered assigned prefix. Navigation asks the coordinator to close nested
workers, with a bounded termination fallback.

Numeric steppers yield worker tasks regularly for cancellation and throttle UI
progress to at most ten messages per second. Statistical caches hold one M01
preparation, one M02 national fit and one M02 profile/paired state. National and
profile dependencies remain separate: changing the fitting seed refits the
national stage while retaining compatible profiles. Names/colors, inactive
settings, candidates and simulation seeds do not refit M02. Generation and legal
keys still change for their consumed inputs.

`elezioni-politics-preparation-v1` stores one replaceable, verified new-format
M02 checkpoint plus its consumed-stage keys. It restores numeric caches after
reload without loading its scenario into the draft. Cache corruption, unavailable
storage or quota failure falls back to ordinary visible preparation, with a
status note. Explicit checkpoint imports remain bound to the entire current
scenario and exact implementation/dependencies; failed imports retain the
previous compatible preparation. No action randomizes seeds: each visible seed
button edits only its own field.

## Sessions and bounded resources

`elezioni:politics:common-sessions:v1` stores the owner, selected session ID and
compact session index; canonical rows remain in the existing IndexedDB chunk
store. Browsing, importing and law/model switching keep the draft independent.
Only **Carica lo scenario di questa sessione** validates and replaces it. Reload
restores exactly the selected session, reports unavailable storage, and never
substitutes another session. Stale results keep their immutable executed identity.

All three pairs use the ordered pull-driven pool: one pending simulation per
worker, bounded row chunks, and a manifest commit only after the entire simulation
is stored. Stable simulation IDs and RNG streams do not depend on pool width.
Failures retain complete/partial/failed membership and the original requested
denominator. Quota failures recover the last durable manifest and leave orphaned
uncommitted chunks invisible. Later deletion removes that session's chunks.

Worker planning accounts for actual packet buffers plus adapter scratch and
keeps requested and effective counts separate. CPU is the selectable ceiling;
RAM/heap budgets determine automatic recommendations. Rosatellum conservatively
uses the previous 1536 MiB scrutiny scratch budget plus packet copies; CP-08
must measure the common national adapter's actual footprint. This estimate is
not a measurement of available RAM. Simulation counts are positive safe integers,
with a 64 MiB compact membership budget (64 estimated bytes per requested run),
rather than the former Rosatellum 1,000-run ceiling. This bound does not establish
performance above the approved 1,000-run acceptance size.

Stored tables/charts use tested presentation helpers and the existing disk
statistics service. Rosatellum lists retain the prior plurinominal-seat meaning;
coalition/support series also include uninominal winners. The UI labels this
distinction and hides reform-only bonus/candidate views for Rosatellum.
Simulation diagnostics use the versioned event ledger for exact membership.
JSON.gz exports stream the common archive and its executed scenario/checkpoint;
CSV remains table-oriented. Imports stage rows under a fresh local ID, validate
EOF, identities and row/diagnostic membership, then select the session without
editing the draft. Corrupt imports delete only staging data.

## Focused verification

Run npm commands from `web/`; build and browser tests must be sequential:

```text
npm run check
npm test
npm run build
npx playwright test tests/e2e/common-politics-lifecycle.spec.ts tests/e2e/common-politics-editor.spec.ts tests/e2e/common-politics-worker.spec.ts tests/e2e/common-politics-files.spec.ts --workers=2
```

`workspace-controller.test.ts` covers session actions and delayed replies;
`execution/lifecycle.test.ts` covers consumed-stage reuse, exact checkpoint restore
and resource bounds; `execution/result-presentation.test.ts` checks Rosatellum seat
meanings and ticket labels. The browser lifecycle suite uses constructed exact assets,
three simulations per deterministic comparison and ten requested simulations for
explicit interruption. It tests real workers, one/two-worker row equivalence,
queries, archive roundtrips, reload, portable checkpoints, execution locking and
injected quota failure. The editor suite additionally uses production bundles.
Repeat focused checks under `BASE_PATH=/elezioni` after the corresponding build.
CP-08 retains production acceptance, regional smoke cases, national campaigns,
measured performance/memory, cleanup and publication gates.
Before the full browser gate, CP-08 must retarget the old-workspace suites
(`app`, `workspace-uniformity`, `politics-default`, `politics-file-isolation`,
`result-import`, `other-list`, `reform-workspace`) to current mounted controls and
formats. They still contain transitional workspace assumptions; the focused
common suites above are the CP-07 runtime evidence, not a full release-gate claim.
