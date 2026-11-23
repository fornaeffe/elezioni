## FV-08 unified scenario and generation workflow — 31 October 2026

Luca approved one reform workflow offering the selected bounded mass-pair field
by default and the exact compared latest-publication legacy poll procedure as
an alternative. Scenario v5 shares original source chunks, conditional target
electorates, geography, legal lists, filing and candidate settings. The legacy
adapter selects the latest eligible same-date common-18+ Camera/Senato pair
(currently 2022); original regional proportions provide labelled missing-history
fallbacks. It does no candidate fitting. Frozen legacy inputs travel with saved
sessions/results, while candidate evidence fingerprints survive legacy edits
and require genuinely stale fitting when switching back. v1-v3 archives retain
their original generator/policy; v4 remains candidate-only.

The correspondence contract keeps binary source/future membership $S_i$ separate
from nonnegative assumed source composition $W_i$; the legacy destination matrix
is $C_{i,f}=W_iS_f^\top$. $B_i$ retains its existing covariance meaning. Column
exclusivity applies within actual ballots/questions, preserving distinct special
territorial tickets. Defaults import the reviewed comparison weights and partial
named points; equal allocation is an explicit action, and typed ratios remain
unchanged until model-boundary normalization. Opaque TAA legacy proxies are
labelled separately and never identify a candidate fitted split. Candidate
pooling, unresolved fitted-split rejection and reference-supported fallback
components remain. Missingness/exclusion derives from original masks, dates,
choice sets and explicit deselection rather than a user-authored missing matrix.

The new legacy policy conditions mapped political composition on filed choices
while preserving the historical valid fraction. Incomplete mappings/unfiled
support do not silently become nonvalid votes. Where no political destination
remains, use labelled mapped regional support or an equal filed-list fallback.
Original archive policy is untouched. A separate post-FV-08 audit in the plan
will quantify incomplete destinations, ceased/new components, unfiled support
and overlapping/submunicipal filing capacity before further policy changes.
Legacy retains whole-list point references and equivalent single-component
points; SDs, uncertainty/fallback scales and unsupported partial within-list
component interventions remain candidate-only and persist when inactive.

Worker selection is CPU-based (reserve two logical processors at four or more,
otherwise one; minimum one worker). Memory estimates determine recommendations
and warnings, not which CPU options are selectable. The 512 MiB aggregate cap
is waived; numerical workspace remains 256 MiB. Recommendations aim at at most
half declared RAM, using 320 MiB candidate / 384 MiB compact-legacy scratch plus owned
packet copies and coordinator reserves; unknown RAM recommends one.

Two 26-worker attempts failed: the first renderer disappeared during preparation;
a subsequent single-packet handoff attempt remained at prepare 0/1000 until its
60-minute calculation / 70-minute test timeout. The first trace is retained under
`data/derived/fv08-unified/failed-full-startup-26/`; the second error was inspected
before the restart but its trace was not retained. Luca independently reported
a selected-model out-of-memory failure at 26 and successful trials of both
models at 10 (manual draw count unspecified). Bulk IPC was therefore not proved
the sole cause. Startup now sends one owned packet to the coordinator and
initializes compute workers separately. Memory planning additionally uses a
conservative 3 GiB browser-heap allowance, reserving 512 MiB for interface/
coordinator and at least 256 MiB per worker; V8's shared pointer-compression cage
can constrain a renderer independently of physical RAM. This yields a 10-worker
default on this 28-processor/64-GiB machine, preserving all 1-26 choices and
adding a separate browser-memory warning. Both ten-draw / ten-worker browser
probes passed, including streaming export/import (26.507 s selected; 32.734 s
legacy). The original CDP sampler omitted nested compute workers: its samples
are now explicitly partial, with separate Windows renderer working-set/private
measurements including nested workers and transient copies.

Before the restart, source-only cold Linux preparation and offline/warm identical
assets passed, including missing/corrupt-cache failure and pointer preservation.
The current-source Linux warm report retains the same catalog. Six actual full
default refits took 8.41-8.51 s, without startup fits; six synthetic maximum
128-coordinate / 512-poll / 7,904-municipality numerical/profile probes took
26.34-26.68 s. A real Windows job-object quarter-core cap on the private benchmark
renderers, including the numerical worker, took 112.27 s. Chromium rejects CPU
emulation on worker targets, so a page-only throttle was not used as evidence.
All resource probes pass the 30/120-second and 256 MiB workspace gates. Raw
reports are under `data/derived/fv08-unified/resources/`; synthetic dimensions
do not establish identifiable political splits or predictive calibration.

The active workflow and authorized protocol amendments are documented in
[vote-generation-unified-workflow.md](../../vote-generation-unified-workflow.md) and
[vote-generation-fast-validation.md](../../vote-generation-fast-validation.md). Earlier
global-Beta capacity failures and statistical comparison limitations are retained.
Luca deferred Windows WebKit export work; Chromium/Firefox acceptance does not
claim Safari export capability. No commit, publication or deployment is implied.


Final integration review corrected legacy poll projection to use each chamber's
future membership matrix independently. A chamber union could misattribute a
component when Camera and Senate tickets differ. New fixtures swap the two
chamber compositions and check the derived point references, reject duplicate
within-question memberships and invented poll identities, and normalize extreme
finite composition ratios safely. The compared symmetric-ticket poll procedure
still matches its original implementation exactly. Nonresponsive worker
initialization now fails explicitly after 60 seconds without automatic retries.
Only explicit selector changes save worker preferences; running an automatic
recommendation does not turn it into a permanent user preference.

The pre-correction full browser campaigns passed calculation, storage, plots,
streaming export, import and scenario restoration: selected 1,000/1,000 in
443.779 s with 10 workers; legacy 1,000/1,000 in 313.806 s with its then-automatic
9-worker recommendation. Each run filled all 588 seats with 588 distinct
candidates. Native whole-renderer private-memory samples peaked at about
4.0 GiB selected and 4.3 GiB legacy. The compact legacy adapter's scratch
estimate is now 384 MiB plus actual packet copies, replacing the old larger
workspace's 1,536 MiB estimate; both final recommendations are 10 on this device.
These earlier receipts/archives remain under
`data/derived/fv08-unified/pre-chamber-correction/`. The final fingerprinted
release is rerun with the same frozen seeds, not substituted into those receipts.

Fresh final-code cold Linux preparation passed in 331.398 s and produced exactly
the Windows catalog `fast-share-catalog-v1-4e39f79a06472053c91ca3b0d97e22e74ce3666e1fc66ab3989c0601b9b6680e`.
Offline warm preparation and the missing-refit/corrupt-cache/pointer-preservation
checks also passed (`data/derived/fv05-acceptance/isolated-acceptance.json`,
checkout `clean-4YDTs8`). Final application/resource/browser measurements follow
below once their current-code gates complete.


### Final FV-08 acceptance and approved classification

FV-08 is complete. The installed reform workflow selects the bounded mass-pair
field by default and retains the reviewed latest-publication legacy procedure
with the approved per-chamber new-list classification. The final source matches
`implementation-v1-9c6267ec47223aacfe9d40ed76c2830dca98fa956a2be0e9c1dd024978cdea33`
and catalog `fast-share-catalog-v1-4e39f79a06472053c91ca3b0d97e22e74ce3666e1fc66ab3989c0601b9b6680e`.

The final complete application acceptance passed all **1,272 unit tests and
122 browser checks** (51 retained plus 10 unified checks at each of `/` and
`/elezioni`), with no skipped tests or retries. Required production assets,
checksums, preparation/application types, the legal vertical slice and restored
root static build passed. The dedicated suites add **30 passing checks**: seven
release/refit/worker checks and eight Chromium/Firefox editor checks per base.
They cover original-source availability, Node/browser input parity, no-fit
startup, explicit refitting, cancellation feedback, retry, source errors, mobile
keyboard forms, strict drafts, new supported lists, filing, plots and archives.
Svelte MCP found no component issues; remaining effect suggestions refer to
intentional asynchronous table/chart queries.

The first final application attempt exposed a stale worker preference when
switching model: the previous 26-worker selection remained visible while legacy
preparation ran. Resource estimates now use the current draft model, and a model
switch immediately restores its own explicit preference or recommendation. Both
browser regressions and the full application rerun pass. The failed root report
is preserved under `data/derived/fv08-unified/failed-worker-preference-switch/`.
A separate dedicated Firefox export exceeded its generic 15-second action wait
while both full workloads were calculating. The entire root editor suite passed
without that heavy calculation load; the measured 9.4-MB Firefox archive took
13.332 s (Chromium 2.910 s). The browser harness now gives asynchronous file
generation an explicit bounded 60-second wait. Product export logic is unchanged;
the contended failure/trace is retained under `contended-ui-root/`. All nested
checks also passed. No calculation, cancellation, refit or workspace threshold
was relaxed.

The final full campaigns ran concurrently in two isolated Chromium 148 instances
against a frozen `/elezioni` build, with **10 compute workers per model**. The
browser reported 28 processors and 32 GiB RAM on this physically 64-GiB machine.
Selected calculation took **638.092 s** and legacy **380.602 s**, including
contention. Both produced **1,000/1,000 complete simulations**, each with 588
filled seats, 588 distinct elected candidates, 392 Camera and 196 Senate seats
and zero unfilled seats. Both archives passed streaming export/import, recorded
ledger and list-seat-total checks, scenario restoration and zero page errors.
The first attempt to parallelize was stopped after discovering that Playwright
still serialized tests within the file; explicit file parallelism was enabled
without changing frozen simulation seeds. Its progress log is retained under
`interrupted-parallel-harness/`. Final reports have no retries.

Whole-renderer sampled private-memory peaks were **4,285,210,624 bytes selected**
and **5,148,151,808 bytes legacy** (about 3.99/4.79 GiB); these are sampled process
footprints including nested workers/native allocations, not free-RAM measurements.
The partial CDP samples are separately qualified. Legacy's compact-adapter
estimate uses 384 MiB scratch plus owned packet copies; the 3-GiB browser-heap
planning allowance yields a 10-worker recommendation while preserving all
1-26 selectable CPU options and independent browser/device-memory warnings.

Final resource acceptance passed two browser tests. Six complete default refits
took **8.534-8.726 s** with zero startup fitting; six maximum-dimension probes
took **26.953-27.422 s**, and the actual quarter-core job-object probe took
**110.778 s**. Numerical workspace was **46,383,008 bytes**, below 256 MiB.
The final source-only cold Linux run took 331.398 s; after synchronizing current
task files, offline/warm preparation took **128.463 s**, all four caches reused,
the identical catalog selected and zero Svelte diagnostics. Source integrity,
missing-refit and corrupt-cache failure, and pointer preservation passed.

Final parity review found that the per-chamber new-list classification changes
the legacy Senate result slightly relative to the earlier cross-chamber rule:
one net Senate seat shifts from AVS to PD across 1,000 runs (0.001 mean seats),
with maximum national mean vote-share difference about 0.00043 percentage points.
Selected draw/seat/vote summaries are identical to the earlier campaign. Luca
explicitly accepted the new classification and requested no additional acceptance
round; the accepted implementation is the already-tested fingerprint above.
The latest-publication wave selection, normalization and measurement rules remain
those of the reviewed comparison. These conditional differences do not establish
predictive calibration.

Curated evidence is retained in
`test/fixtures/benchmarks/vote_share_unified_acceptance.json`, including source/
implementation IDs, archive/ledger hashes, complete denominators, vote/seat/
admission/bonus/cap summaries and qualified timing/memory measurements. Raw
reports/archives are ignored under `data/derived/fv08-unified/`. Earlier global-
Beta failures and their statistical limitations remain historical evidence.
The post-FV-08 correspondence-policy audit, FV-09 cleanup and public deployment
remain open. Changes are uncommitted; no publication/deployment was performed.
Automatic approval review rejected optional removal of empty temporary folders
under `web/data/derived/fv08-unified/`; they remain empty and untracked.
