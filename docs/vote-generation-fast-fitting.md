# Browser fitting infrastructure (FV-06)

FV-06 supplies the fitting worker, typed client and state/ownership boundary for
the **2026 reform only**. It does not install the scenario v4 editor or replace
application defaults. FV-07 owns UI/parser integration; FV-08 owns production
predictive, resource-envelope and activation acceptance.

| Task | Read |
| --- | --- |
| Wire the editor to load, validate, fit, cancel or restore | Client contract below; [fit-client.ts](../web/src/lib/politics/reform-2026/vote-share/fit-client.ts) |
| Change request/reply or snapshot metadata | [fit-types.ts](../web/src/lib/politics/reform-2026/vote-share/fit-types.ts) |
| Change evidence compilation or reuse | [fit-service.ts](../web/src/lib/politics/reform-2026/vote-share/fit-service.ts), [source compiler](../web/src/lib/politics/reform-2026/vote-share/source-compiler.ts) |
| Change scheduling or cancellation | [reform-fit.worker.ts](../web/src/lib/workers/reform-fit.worker.ts), [execution.ts](../web/src/lib/politics/reform-2026/vote-share/execution.ts) |
| Plan memory or hand buffers to generation workers | Ownership section below; [fit-resources.ts](../web/src/lib/politics/reform-2026/vote-share/fit-resources.ts) |
| Prepare a default / review its political basis | [FV-05 release guide](vote-generation-fast-release.md) |
| Review verification evidence | [Snapshot](../test/fixtures/benchmarks/vote_share_fit.json), [history](migration-history.md#2026-10-29--fv-06-browser-refitting-and-revision-bound-state) |

## Client contract

Create `ReformFitClient(configuration, { onChange })` on the browser side.
Configuration contains the base path, pinned candidate pointer and independently
supplied build implementation IDs for generation, fitting and profiles. Compute
these with preparation's transitive `implementationKey` entries (`generation.ts`,
`information.ts`, `senate-adapter.ts`); do not infer compatibility merely by
copying whatever identities an arbitrary catalog declares. FV-07 must supply the
prepared identities with its app entry metadata. The worker rejects mismatches.

The default factory uses Vite's emitted module-worker URL. The base path is
explicit and applies to every data fetch. No server fitting endpoint,
SharedArrayBuffer, cross-origin-isolation header or persistent cache is required.
An injected worker factory is available for lifecycle tests.

| Operation | Behavior |
| --- | --- |
| `load()` | Verify/load the precomputed candidate, validate generation compatibility and install it without fitting or fetching original refit history |
| `copyDraft()` | Return an owned scenario copy, including IDs rebound after successful refitting |
| `setDraft(scenario)` | Increment the revision, cancel outstanding work and mark the draft stale; do not estimate automatically |
| `validate()` | Determine compatibility/reuse; future-only edits validate without original-data loading. A compatible result becomes ready; an evidence edit returns a refit plan |
| `fit()` | Compile exact semantic inputs and perform only required national/profile/paired work; install the complete passing result atomically |
| `cancel()` | Reject the active promise with `AbortError`, send cancellation and immediately terminate the worker, including uninterruptible batches |
| `restoreAccepted()` | Restore the last accepted scenario/fit and make that revision ready; it discards current draft edits, so retain the existing UI confirmation convention when wiring it |
| `reserveGenerationCopies(count, hints, reservedBytes)` | Reserve capacity and return owned packet copies with transfer lists and idempotent lease release functions |
| `dispose()` | Terminate outstanding fitting and drop private accepted/draft state; generation consumers remain owned by their caller |

Observe `state` for phase, draft/accepted revisions, progress, refit plan and
errors. Phases are empty/loading/validating/stale/fitting/ready/cancelled/error/
disposed. A failed or cancelled task preserves accepted buffers and keeps the
draft editable. Only `canRun` authorizes a new generation handoff. An old fit
cannot run an edited stale draft. Restore or a passing matching replacement is
required; there is no silent default substitution.

Every worker request/reply carries request ID, scenario revision and fingerprint.
The client checks all three, build identities, scenario binding and state IDs
before installation, including after asynchronous fingerprint checks. Late
progress/results from terminated workers are ignored. Rapid edits during
fingerprinting cannot start a stale worker. The worker never receives ownership
of the authoritative accepted buffers.

## Evidence and numerical reuse

The worker compiles original counts and keeps numerical/provenance keys distinct.
A cheap evidence fingerprint avoids fetching history for future grouping,
references, filing, election-date, simulation-seed or presentation-only changes.
When evidence differs, exact compiled keys determine which estimates to reuse:

| Edit | Required numerical work |
| --- | --- |
| Poll selection, effective sample, shares, dates or error group | National reference fit; reuse unchanged Camera profile and Senate adapter |
| Fit seed | National fit; reuse profiles |
| Historical selection/memberships/basis | Recompute affected national/profile inputs; rebuild the adapter when its inputs change |
| Paired-Senate-only correspondence | Rebuild the adapter; reuse unchanged C/E fit and Camera profile |
| Reference date, even one day with unchanged selected records | Refit national and date-dependent profile/adapter state |
| Citation/provenance only | Reuse numerical state; update provenance identity |

`scenario.polls.added` contains complete normalized `SourcePoll` records owned by
the scenario. A matching bundled ID overrides that poll for this scenario;
a new ID adds a poll. `polls.selected` controls inclusion/removal. Originals are
never overwritten. Records require dates, citation/method, original answers,
offered groups, normalized positive shares, explicit sample/decided/design-effect
calculation and the supported effective-sample/rounding policy. Duplicate survey
evidence, invalid samples, unavailable dates, overlapping choices, unsupported
conditional questions and unsupported zero-share treatment fail. Poll forms and
incomplete edit drafts belong to FV-07, not the fitting protocol.

The fitted basis remains explicit in `components` and `componentMap`. Refitting
does **not** automatically turn FV-05's 25 reference-supported components into
individually estimated parties. Its default pooling, historical-group boundaries
and fallback assumptions are unchanged. A different basis must pass the source
identifiability, numerical and physical-support checks; its production suitability
still requires review. Geography and conditional electorates remain pinned to
the loaded runtime package in this client.

Node and browser use the same numerical steppers. Synchronous wrappers consume
them immediately; the worker yields to the event loop after roughly eight
milliseconds of scheduled work. Samplers yield every 16 sweeps, profile passes
every 128 cities/256 history rows, and compilation every 256 rows. GLS solve
columns are separate steps. Factorization, one solve, diagnostics, some event
coarsening and Senate adaptation remain synchronous batches; termination is the
cancellation fallback. RNG ordering, three GLS updates, diagnostics/retention,
residual gates and all model equations are unchanged.

## Ownership and memory

The client privately owns the last accepted fitted/profile arrays. A task uses
structured copies in one ephemeral fitting worker. Its successful result transfers
the completed buffers back; the client terminates the worker on result, failure
or cancellation. Decoded histories and compilation temporaries are not retained
in a persistent worker or cache. Fit once, then copy compact generation packets
to consumers; no consumer performs parameter estimation.

Each generation lease contains `{ packet, transfer, release }`. Transfer only
that disposable copy. Close/terminate its consumer **before** calling `release()`;
always release in `finally`. Existing consumer packets remain frozen when the
editor draft changes. Active leases block new fitting/validation tasks until
consumers are closed and released, preventing an unbudgeted fitting/compute overlap.
Caller-supplied results-memory reservations must include its other active buffers.

Fit planning defaults to **512 MiB aggregate**, **256 MiB numerical workspace**
and a **64 MiB results-pipeline reservation**. It accounts for accepted/task/output
copies, original/pooled histories, decoded chunks and maximum retained diagnostic
draws, and rejects over-budget work before historical decoding. Larger dense
systems select the existing matrix-free solver under the same residual gate.
These are conservative allocation estimates, not measured browser heap/free RAM.
Unmeasured or larger scenarios may be rejected and remain FV-08 resource gates.

Generation capacity retains the existing conservative scrutiny/results budgets
(1,536 MiB per worker and 512 MiB coordinator) and adds owned fit/packet copies and
explicit reservations. It leaves half reported RAM available to the OS/other tabs
and bounds concurrency by CPU. It returns zero when no safe slot fits; it does
not force a nominal single worker. Missing device-memory reporting uses the
existing 2 GiB hint, which can yield zero with these budgets. FV-08 must measure
and revise supported limits/browser behavior before production activation.

## Verification

From `web/`, after candidate preparation:

```powershell
npm test -- --maxWorkers=4
npm run check:data
npm run check
npm run test:reform:shares:release
```

The browser command covers FV-05 loading and FV-06 fitting against actual static
candidate data, using the real Vite-emitted fitting worker. For a project path,
prepare with `--base /elezioni`, set `BASE_PATH=/elezioni` and repeat. It checks
manual polling, history/date changes, failed-load retry, cancellation/restore,
Node/browser fit and generated-input parity, and serial/parallel scrutiny parity.
Unit tests exercise stale replies, rapid revisions, cancellation before startup,
worker/message errors, budgets and detached consumer copies. Development browser
tests skip clearly when no candidate exists; that is not production acceptance.

The snapshot retains exact pre-FV-06 reduced/default arithmetic hashes and final
worker measurements. This is current-candidate Chromium engineering acceptance;
full-envelope latency, throttling, browser heap, all target browsers, production
predictive/1,000-run seats and editor/result/export integration remain FV-07/08.
