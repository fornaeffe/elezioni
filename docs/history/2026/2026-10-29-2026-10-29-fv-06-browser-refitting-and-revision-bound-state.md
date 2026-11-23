## 2026-10-29 — FV-06: browser refitting and revision-bound state

Completed the [browser fitting infrastructure](../../vote-generation-fast-fitting.md)
for the 2026 reform. Added `reform-fit.worker.ts`, a typed ephemeral-worker client,
scenario/evidence fingerprints, refit plans, atomic accepted-state installation,
memory planning and owned generation handoffs. The
[FV-06 snapshot](../../../test/fixtures/benchmarks/vote_share_fit.json) records code hashes,
pre-step parity and browser outcomes. The existing editor/parser/dispatch remain
inactive for this model; FV-07/08 retain integration and activation.

The worker loads the prefitted default without original-history requests or
parameter estimation. Future-only edits validate against reusable accepted state.
Evidence edits compile exact national/profile/paired keys before selecting work:
manual polls refit the national reference, historical/date changes refit affected
profiles, and paired-only changes can retain the C/E fit and Camera profile.
A complete scenario-owned poll record can add or override a bundled ID; selection
controls removal. Availability, effective samples, supported observation policies,
duplicate surveys and admissible grouped bases remain enforced. Original counts,
source metadata and FV-05's explicit political basis/fallback policy are unchanged.
In particular, refitting does not automatically estimate its 25 reference-supported
components separately.

Introduced shared arithmetic steppers for GLS solve columns, Gaussian sampling
and profile passes. The existing synchronous APIs drive the same steppers without
awaiting. The browser yields between numeric batches while preserving iteration,
RNG and draw order. Dense solves/factorization, diagnostics and some compilation/
adapter work remain synchronous; the client sends cancellation and terminates
immediately as the fallback. Four-chain diagnostics, retention escalation,
residual tolerances, model equations and the numerical/model IDs are unchanged.

Each task binds request ID, scenario revision and fingerprint. Async fingerprint
completion, queued progress and late replies cannot install a stale result.
Errors/cancellation preserve the previous accepted buffers and leave the draft
editable; stale drafts cannot create new generation packets. Restore explicitly
returns to an accepted compatible revision. Workers are recreated for subsequent
requests, so decoded history is not retained after task completion/cancellation.

The client keeps authoritative accepted buffers private. Each generation consumer
receives a reserved copy with an owned transfer list; transferring it does not
detach accepted state. Consumer leases are released after worker termination;
active consumers block estimation to avoid unbudgeted fitting/compute overlap.
Capacity accounting adds fit/packet copies to the existing conservative scrutiny/
results budgets and can report zero safe slots. Missing memory hints are not
permission to force a nominal worker. The fitting task defaults to a 512 MiB
aggregate plan and 256 MiB numerical workspace, including a 64 MiB results reserve.
These are documented estimates, not measured browser heap or production-envelope
acceptance; FV-08 must measure/refine supported limits and browser behavior.

Code review and integration tests also corrected object-key-order-sensitive
comparison of source chunk descriptors to canonical comparison. Offline and
interactive serialization now share explicit joint-chain/profile shape helpers,
preserving the portable schema for subsequent export/import integration.

Verification:

- Independently replayed the pre-FV-06 reduced kernels from repository `HEAD` and
  froze exact fit/profile hashes. Cooperative and synchronous execution match
  those hashes. Regenerated default fit, Camera profile and Senate adapter also
  match their pre-step numerical payloads exactly. Implementation/content IDs
  change because scheduling code changed; generated-input provenance therefore
  carries new fitted/profile IDs. The parity audit distinguishes those metadata
  updates from vote/preference/candidate/legal fields.
- The full unit suite passes **95 files / 1,192 tests** with four test workers,
  including new lifecycle, semantic invalidation, source immutability, budget,
  cancellation, stale/mismatched reply and transferred-copy tests.
- Chromium **148.0.7778.96** passes **seven browser checks at each of `/` and
  `/elezioni`**, against actual static candidate files and the real Vite-emitted
  fitting worker. Tests cover zero-fit startup, manual polling with Node-identical
  fitted IDs/generated input and retained profiles, date/membership refits,
  required-source failure followed by successful retry, cancellation/restore,
  and identical serial/parallel generated inputs and scrutiny results for the
  same simulation IDs. Test consumers use an explicit 8 GiB planning hint;
  this is not a browser RAM measurement or a maximum-concurrency acceptance.
- The project-path run measured about **4.56 seconds** for a manual-poll fit,
  **6.63 seconds** for a one-day reference-date refit and **6.85 seconds** for
  historical membership refitting. Its largest measured numeric batch was about
  **169 ms**. Cancellation feedback was below one millisecond. The current
  candidate's fitted/profile numeric buffers total **4,619,232 bytes**; its
  conservative aggregate refit estimate is about **390 MiB**. Phase measurements
  exclude main-thread fingerprint/startup and are not full browser-heap evidence.
- `check:data`, `check` (zero errors/warnings), root/project static builds and
  candidate asset verification pass. FV-01 still verifies 1,433 original inventory
  entries and 61 retained evidence files. The root static build was restored;
  existing chunk-size/plugin-timing advisories remain.

FV-07 must wire forms, parser, loading states, build-pinned implementation IDs
and this lifecycle into the editor. FV-08 still owns expanded-default review,
predictive/1,000-run seat acceptance, input-envelope latency/throttling, measured
aggregate memory, supported browsers and activation. FV-09 archival and public
publication remain separate. No commit or deployment was performed.
