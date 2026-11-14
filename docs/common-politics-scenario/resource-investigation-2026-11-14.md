# M02 memory investigation after CP-07

Investigation at `2f05136`, compared with the CP-01 input revision `a700d19`.
Scope: reform/M02 startup, worker ownership, automatic sizing and stored runs.
Runtime changes below are proposals; none was applied. CP-08 remains open.
This is the historical investigation. The subsequently approved implementation
is described in [execution](execution.md#workers-and-workspace-integration) and
[lifecycle](lifecycle.md#sessions-and-bounded-resources); its verification is
recorded in [migration history](../migration-history.md).

## Finding

CP-03/07 introduced avoidable preparation and retained-state overhead. In a
short comparison the current common UI peaked about 8% above the pinned pre-CP
M02 source. That supports a moderate increase in memory pressure, but
does **not** establish a growing per-simulation memory leak or the exact cause
of the crash. Both versions already have a large renderer footprint. The
automatic worker recommendation's old calibration has not been revalidated for
the current model and common path, so added overhead can consume its headroom.

Luca reported a tab crash in Chrome `157.0.8103.51` around simulation 250 of a
1,000-run reform/M02 request with automatic workers. This investigation did not
reproduce that crash or complete a 1,000-run campaign.

## Measurements and limits

All browser probes used separate temporary profiles and the installed national
M02 default, both chambers, the unchanged fitting/simulation seeds and matching
historical source version. No user browser profile or saved session was changed.
Probes ran sequentially. Sampled private memory sums the test browser's renderer
processes through CDP process IDs and Windows `PrivateMemorySize64`, including
nested workers; it is **not** a JavaScript heap measurement or a hard peak.

| Path | Browser | Runs / workers | Stored-run elapsed | Sampled renderer private peak |
| --- | --- | --- | --- | --- |
| Pinned pre-CP M02 generation + retained execution services | Playwright Chromium 148.0.7778.96 | 60 / 10 | 83.11 s | 5486 MiB (5.36 GiB) |
| Retained fast execution services + CP-03 compatibility wrapper | Playwright Chromium 148.0.7778.96 | 60 / 10 | 80.86 s | 5844 MiB (5.71 GiB) |
| CP-07 production UI | Playwright Chromium 148.0.7778.96 | 60 / auto = 10 | 75.84 s | 5923 MiB (5.78 GiB) |
| CP-07 production UI | Installed Chrome 157.0.8103.51 | 60 / fixed 4 | 121.24 s | 3354 MiB (3.28 GiB) |

All four completed 60 simulations. Stored-run elapsed includes compute-worker
initialization and storage, but excludes the page's initial model preparation.
See the [compact receipt](resource-investigation-2026-11-14.json) for samples,
execution identities and counters.

The first two rows invoke the retained real worker and storage services through
a small harness, rather than mounting the former editor or checking out an
entire old application. The first replaces the generation module with its exact
`a700d19` source through a diagnostic Vite load override (Git blob
`8df3998bee92d0a19bf97f225ead2de2d760046c`); the second keeps the CP-03 wrapper.
The compute/pool implementation is unchanged by CP except for the common-worker
entry branch. The installed default source and simulation seed match the common
run. This is execution-path evidence, **not** a controlled historical UI benchmark.
The roughly 8% increase over the pinned source is consistent with the ownership
overhead below; the roughly 1.3% difference from the compatibility wrapper alone
would conceal that part of the extraction. Single sampled runs cannot attribute
the exact difference or establish a CP throughput slowdown. Four-versus-ten
worker speed or memory ratios cannot be inferred across browser versions.

Disk-reader startup probes also confirmed that the already-fitted common worker
still compiles history. It took roughly 5–7 seconds versus roughly 1–2 seconds
to load/prepare the retained generation packet in these short Node probes.
These are diagnostic timings, not browser throughput acceptance.

## Changes that matter

### Historical compilation in each compute worker — CP-03, mounted by CP-07

`workers/politics-compute.ts:preparePoliticsCompute` calls the full
`execution/prepare.ts:preparePoliticsExecution`. Even with CP-07's supplied
`numericStates`, this loads/checks/parses every required JSON dependency, builds
domain inputs, decodes original election columns, calls `compileSource` and
`compilePairedHistory`, and hashes their full inputs. Only the numeric fitting
and profile-estimation steps are bypassed by reuse.

The former `workers/reform-compute.ts:prepareFastCompute` receives prepared
`FastVoteAssets` and legal context directly. It does not compile historical
observations during worker initialization. The new route therefore adds CPU,
temporary objects and allocation pressure multiplied by pool width. It is
startup work, not work repeated on every simulation.

The packet's estimated retained size is 36.91 MiB over the preview server's HTTP
transport, versus 29.04 MiB for the retained fast packet. The common packet
includes 8.12 MiB of pinned JSON bytes and 13.61 MiB of decoded history buffers.
The server advertises gzip encoding, so the browser caches decoded history
columns; counting only their 3.51 MiB on-disk compressed size is misleading.
These object-accounting estimates are distinct from measured renderer memory.

### Duplicate numeric ownership — CP-03/07

`prepareM02Votes` clones its input assets. The common preparation also returns
the original `numericStates`, and every common compute worker retains that full
preparation object even though it never exports checkpoints. The originals
include 11.22 MiB of owned numeric buffers (13.23 MiB estimated including objects)
in this default, alongside the generator's cloned arrays. The pre-CP generator
cloned its input assets once without retaining those originals for later use.
CP-03's retained compatibility wrapper also captures the original `assets` in
its run closure for `m02AssemblySource`, alongside the extracted generator's
clones. Thus the retained path with the current wrapper is not a sufficient
control for the extraction's ownership changes; the pinned-source comparison
above is needed. Precomputing that metadata would avoid its numeric retention.

The coordinator additionally retains its own prepared generator, reusable
numeric cache and raw asset/chunk cache. These caches have bounded entries, but
their footprint is not captured by treating the serialized packet alone as the
whole coordinator cost. Retaining an inactive M01 preparation after switching
can increase that baseline further; it is not required to explain a fresh M02 run.

### Worker sizing inherited an old heap allowance — CP-07 integration

`execution/resources.ts:politicsCapacity` copies the older `generationCapacity`
policy: 320 MiB M02 scratch plus packet copies for the RAM estimate, but only
`max(256 MiB, packetBytes)` per worker in the separate 3 GiB heap plan, with
512 MiB reserved for the coordinator/interface. For this small-enough packet,
that separate calculation still permits ten workers. The current HTTP packet
raises the RAM estimate from about 379 to 394 MiB per worker without changing
the ten-worker heap recommendation. Neither value measures available memory.

This is not a newly removed worker cap. Both paths permit the same CPU ceiling
and recommend ten on the observed 28-processor device. The old calibration
already carries risk: model restoration at `cd69edd` preceded CP-01, and its
history explicitly records no new 1,000-draw resource campaign. The modest common
overhead can consume remaining headroom, but does not by itself prove the cause
of Luca's exact crash. Renderer private memory and the planning heap allowance
are different metrics and must not be equated.

### Expensive manifests — shared pre-existing issue

`runStoredPolitics` embeds a roughly 10.97 MiB JSON checkpoint in the stored
record, and `saveStoredResult` serializes the whole record at every simulation
commit. `getStoredResult` also retrieves it for otherwise small queries. The
former path similarly embeds a roughly 11.57 MiB portable fit file. This is
avoidable repeated copying/storage work, but it is **not** a new CP memory leak.

### No accumulating simulation queue found

`runOrderedReformPool` still allows one outstanding simulation per worker,
dispatches a replacement only after durable ordered commit, and releases its
pending slot. `encodeReformRun` empties each consumed table. The coordinator's
presentation object accumulates counts, not all legal outcomes. The new session
membership array grows with completed IDs, but hundreds of tiny entries do not
explain a multi-GiB crash. No growing cache indexed by simulation was found in
the extracted M02 generation path.

CP-01 added evidence; CP-02 added the domain contract; CP-04/05 changed defaults
and file/draft handling; CP-06 added the editor and its asset/tree baseline.
The immediately preceding name/territory/color/bonus-chart changes affect
presentation, not the compute loop. The scrutiny implementation has no CP
algorithm change; M02's source move and common adapter preserve the tested
ballot/legal outcome behavior.

## Proposed fixes, in order

1. **Reduce and recalibrate automatic parallelism.** Start conservatively (four
   workers is a practical temporary setting supported by the Chrome 157 probe),
   then measure cold startup, warmed execution, pending result batches, editor
   and coordinator ownership on the supported browsers. Budget startup and
   steady-state peaks separately. Preserve explicit user choices with clear
   resource warnings and record actual workers. A 60-run pass is not a guarantee
   that 1,000 runs will complete.
2. **Prepare once and send a compact compute packet.** Keep exact asset/source
   validation and historical compilation in the coordinator; hand off normalized
   generation settings, numeric assets, legal settings/context and validated
   dependency/build keys. Workers must still verify the relevant identities and
   numeric parents. They should neither receive raw history/catalogue bytes nor
   recompile history. Return a compute-only handle without checkpoint/cache
   state, and avoid cloning an already owned immutable numeric state again.
   This changes ownership/preparation, not probability laws, RNG or scrutiny.
3. **Separate immutable checkpoint data from mutable result manifests.** Store
   the checkpoint once under the session identity and reference it from compact
   commits. Fetch it only for explicit checkpoint/archive operations. Keep the
   existing atomic-prefix, quota-recovery and archive-binding guarantees. This
   improves both paths rather than correcting a CP-specific leak.

Before accepting the fixes, compare deterministic generated ballots/legal
outcomes and stored rows at different worker counts, then run the agreed
sequential 1,000-simulation campaigns with renderer-wide sampling, including
Chrome 157. Check for a growing post-GC live set as well as native/process peaks;
the current samples cannot identify the allocation that caused the reported
simulation-250 crash.

## Verification

The investigation used the four short browser probes above and focused CP-01
replay, common execution/lifecycle and ordered-pool tests. No generated default,
production bundle, frozen legal/checksum fixture, probability model or scrutiny
implementation was edited. The exact focused test outcome is recorded in
[migration history](../migration-history.md).
