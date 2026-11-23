## 2026-10-29 — FV-07 candidate browser workspace and fitted-session replay

Implemented the strict-v4 editor in the existing reform workspace, behind
`REFORM_FAST_CANDIDATE=1`. Vite supplies independently fingerprinted generation,
fitting and profile implementation identities. The installed v1-v3 editor is
retained as `InstalledReformWorkspace.svelte`; the normal build still selects it.
The [workspace guide](../../vote-generation-fast-workspace.md) is the task entry point;
the [verification snapshot](../../../test/fixtures/benchmarks/vote_share_workspace.json)
records source hashes and measured results.

The small default scenario/catalog loads first. Heavy fitted/runtime data loads
on request, and the source catalog loads when advanced editing is opened.
Future-only edits validate without original history or numerical fitting. Poll,
membership, historical baseline, fit-seed and reference-date changes require an
explicit cancellable refit. Forms cover list/component composition, new support
with labelled fallback assumptions, references and uncertainties, separate
turnout inputs, filing with filtered batch actions, coalitions, bonus links and
the shared candidate editor/preview. Complete strict JSON retains supplied
votes/preferences, pooling, protected-minority settings and detailed fallbacks.
Pending JSON blocks execution. Other keeps its fixed identity/color and legal
exclusions. List removal confirms dependent changes; supplied fixed votes prevent
automatic deletion. The FV-05 basis remains 10 fitted and 25 reference-supported
components, visibly distinguished in the editor.

Added portable fitted sessions: lossless encoded fit/profile/runtime states,
their hashes and semantic keys, the accepted scenario, and a separately editable
draft. An evidence-stale saved draft remains stale after loading. Custom refits
can be restored without fitting again; original count chunks stay server-side
and are fetched only for subsequent refitting. Schema-3 result archives include
the exact executed v4 scenario and fitted checkpoint. Streaming JSON.gz/CSV,
bounded tables, result diagnostics and charts use the existing result service.
Run packets are copied under capacity reservations and independently checked in
compute workers; an editor change during execution cannot alter that run's
stored result or export. Result restoration never silently replaces a separate
draft. Lightweight valid drafts use their own localStorage namespace; persistent
model-cache management and background prefetch/notifications remain deferred.

Verification on Node 24.20.0 / Windows / headless Chromium 148.0.7778.96:

- `check:data` and `check` pass, with zero Svelte errors/warnings; **96 unit
  files / 1,199 tests** pass. The Svelte MCP reports no component issues. Its
  generic effect suggestions were reviewed: revision-checked asynchronous table
  and chart reads intentionally remain effects rather than synchronous derived
  values.
- **4 UI + 7 existing numerical/worker browser checks at each of `/` and
  `/elezioni`** pass. Coverage includes demand loading, edited-before-load state,
  fixed-turnout references, manual original-set/Other poll entry and optional
  question subsamples, cancellation/retry, custom-state replay, frozen results,
  streamed exports/imports, metadata retry, mobile viewport/keyboard editing,
  new-list filing and invalid/legacy-file handling. The original numerical checks
  retain Node/browser parity, history/date invalidation, serial/parallel legal
  equivalence and explicit missing-asset failure.
- Both candidate releases verify **7,904 municipalities, 15,810 physical cells,
  588 filled seats**, matching semantic keys and zero startup fits. The UI's
  one-run edited-reference examples took approximately **8.5 seconds**, including
  result storage; they fetched no original history and produced no page errors.
  These are functional examples, not FV-08 latency or measured-heap acceptance.
- Restored the **standard root static build** and passed **12 installed editor
  and workspace browser regressions**, including v1/v3 import, candidate preview,
  confirmation/reset, paired execution and result replay.
- FV-01 `freeze.mjs --check-sources` still verifies **61 retained evidence files
  and all 1,433 original inventory entries**. Research was not moved or deleted.

Browser integration exposed and resolved a Svelte-proxy clone failure at the
candidate-pointer boundary, stale custom-fit parent IDs after default reload,
and the static test server's missing extensionless HTML resolution. An initial
synthetic Other poll incorrectly named pooled fitting coordinates instead of
original offered atoms: the compiler correctly rejected the uninformative merged
partition. The form/test now preserve original offered-set semantics and prevent
overlapping named answers. Poll editing preserves its observation date and
question subsample and rejects implicit Senate-to-Camera conversion.

The candidate UI gives an actionable standard-build message for older generator
files. FV-08 must accept their routing in the finally activated application,
along with predictive/full-list/geographic/paired/seat and complete resource
acceptance. FV-06's conservative capacity policy can still disable simulation on
an unsupported device; a mobile viewport does not establish handheld resource
support. Existing build chunk-size/plugin-timing advisories remain. FV-09 research
archival and the final publication gate are still separate. No commit, deployment
or production-default activation was performed.
