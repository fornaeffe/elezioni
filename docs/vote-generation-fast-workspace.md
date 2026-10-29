# Fast vote-share workspace (FV-07)

This is the browser editor for the **2026 reform only**. FV-08 still owns the
full production envelope, predictive acceptance and default activation. FV-09
owns research archival. Neither step is implied by this UI implementation.

| Task | Read / change |
| --- | --- |
| Prepare the precomputed default | [Release guide](vote-generation-fast-release.md) |
| Start or verify the candidate workspace | Commands below |
| Edit forms and their state transitions | `web/src/lib/politics/reform-2026/vote-share/FastReformWorkspace.svelte`, `FastScenarioEditor.svelte`, `FastReferencesEditor.svelte`, `FastEvidenceEditor.svelte` |
| Change pure list/reference/poll operations | `vote-share/editor-model.ts` |
| Change v4 scenario validation | `vote-share/scenario.ts` |
| Change fitting, cancellation or memory ownership | [Fitting guide](vote-generation-fast-fitting.md) |
| Change saved fitted sessions | `vote-share/checkpoint.ts`, contracts below |
| Change compute or result import/export | `web/src/lib/workers/reform-compute.ts`, `result-service.ts`; `web/src/lib/core/result-export.ts` |
| Review recorded verification | [FV-07 snapshot](../test/fixtures/benchmarks/vote_share_workspace.json), [history](migration-history.md#2026-10-29--fv-07-candidate-browser-workspace-and-fitted-session-replay) |

Paths beginning `vote-share/` above are relative to
`web/src/lib/politics/reform-2026/`.

## Build and run

From `web/`, in PowerShell:

```powershell
$env:REFORM_FAST_CANDIDATE = '1'
npm run data:reform:shares:release -- --report ../data/derived/fv07-acceptance/release-root.json
$env:ACCEPTANCE_PREBUILT = '1'
npm run test:reform:shares:ui
npm run test:reform:shares:release
```

For project hosting, prepare/build with `--base /elezioni`, set
`$env:BASE_PATH = '/elezioni'` for both browser suites and repeat them. They serve
adapter-static's actual `build/`, including module workers and HTTP-decoded gzip.
Required candidate assets are a failure in the FV-07 UI suite, never a skip.

For interactive development, set `REFORM_FAST_CANDIDATE=1` and run `npm run dev`.
Open `/politics` and select the existing **Riforma 2026** scrutiny algorithm.
There is no statistical-model selector or additional route. Vite independently
computes generation/fitting/profile implementation identities from the code.

Remove `REFORM_FAST_CANDIDATE` and rebuild to return to installed v3 defaults:

```powershell
Remove-Item Env:REFORM_FAST_CANDIDATE -ErrorAction SilentlyContinue
Remove-Item Env:BASE_PATH -ErrorAction SilentlyContinue
npm run build
```

No publication, deployment workflow or production-default switch is enabled.

## Browser behavior

1. Open the reform workspace. Only the candidate pointer, catalog and small
   default scenario are fetched. The scenario remains editable before heavy data
   is loaded. **Carica modello predefinito** loads and verifies the fitted,
   geographic and runtime state; it performs no parameter estimation.
2. Arrange whole political components into lists, including joint lists; choose
   chamber presence, filing, coalitions, colors, uncertainty multipliers and
   references. The normal candidate editor and preview use the same legal
   assembly and scrutiny contracts. Supplied votes/preferences, protected
   minority settings, pooling maps and fallback details remain available in the
   complete v4 JSON editor. Pending JSON must be applied or discarded before a run.
3. A list-reference edit changes valid-vote composition while keeping turnout
   fixed. Non-valid/abstention references are separate. A blank reference inherits
   the model, whereas zero is explicit; uncertainty is in percentage points.
   Future-only edits are validated without downloading original history or fitting.
4. Open **Dati avanzati** to demand-load the source catalog. Polls and historical
   choices are paginated. Poll entry records fieldwork, publication, sample,
   decided fraction, design effect, shared error group and explicit offered
   component sets. Unsupported observations and cutoff reasons remain visible.
   The observation date and optional question subsample are retained when editing
   a poll; Senate polls cannot silently become Camera observations.
   Historical totals stay indivisible; opaque observations cannot be split.
5. Poll selection/entry, historical membership/basis, paired baseline, fit seed
   or reference-date changes require **Ricalcola parametri**. Only the necessary
   estimates are recomputed. Errors and cancellation preserve the draft and last
   accepted state. Retry explicitly; there is no silent substitution of defaults.
6. Run simulations with the accepted revision. Each run freezes its scenario,
   fitted checkpoint and owned worker copies. Edits during calculation belong to
   a new draft. Results, charts and exports retain the executed state; restoring
   that state is an explicit action. Table pages contain at most 50 rows.

New lists are not automatically filed. Whole components must not compete in
overlapping lists in the same physical college. Composition edits may briefly
leave an invalid draft; validation reports this and execution remains blocked.
Deleting a list asks for confirmation before removing its dependent candidate,
filing and reference entries. Supplied fixed votes block automatic list deletion.
Changing coalition membership clears bonus links for explicit reconfirmation.
**Altre liste** requires explicit component membership and keeps its reserved
identity, grey color, and exclusion from candidacies and seat allocation.

The candidate still uses the FV-05 basis: **10 fitted coordinates and 25
reference-supported components**. The UI labels those support types separately;
the latter retain explicit fallback assumptions. FV-07 does not expand the
identified fitting basis or accept the full production refit envelope.

## Saved state and compatibility

| File / storage | Contract |
| --- | --- |
| Downloaded scenario | `politics-reform-session`, schema 1: editable strict-v4 draft plus the last accepted scenario, fitted/profile/runtime states, catalog, hashes and semantic keys |
| Results JSON.gz | Result-export schema 3: executed v4 scenario, matching self-contained fitted session and columnar result tables; streamed through the existing disk-backed export boundary |
| Results CSV | Existing table-oriented export; used for spreadsheet review, not fitted-state replay |
| Lightweight local draft | Separate localStorage namespace; scenario only. A custom fitted state is preserved by downloading its session, not by an implicit persistent model cache |
| Local result history | Existing owner-scoped IndexedDB result storage and session-local result index; reading results does not replace an independently saved draft |

Session binary data is lossless gzip encoded in bounded base64 chunks, with a
64 MiB compressed-state ceiling. Original historical count chunks are not
embedded: refitting an imported session still requires the corresponding hashed
source artifacts from the server. Ordinary replay requires only the embedded
states. Loading verifies integrity, dimensions, semantic keys and current build
identities before installation. Unknown v4 fields are rejected. The candidate
workspace explicitly rejects earlier scenario/result versions with instructions
to use the standard build, which retains their installed workflow. No automatic
statistical conversion is attempted. FV-08 must accept archive routing in the
final activated application before switching its defaults.

A saved stale draft deliberately differs from the accepted scenario inside its
session. Loading it preserves both and leaves execution blocked until validation
or refitting succeeds. A result archive requires an exact scenario/checkpoint
match. Read-only result import verifies the archived model internally; restoring
it for a new run additionally requires the current app's implementation IDs.

## Verification and remaining gates

`editor-session.test.ts` checks exact replay, corrupt/missing/incompatible states,
stale evidence drafts, reference inheritance/zero, whole-component operations and
a newly fitted manual poll. `fast-share-ui.spec.ts` covers demand loading,
editable-before-load state, fixed turnout references, frozen runs, streaming
JSON.gz/CSV, scenario/result replay, manual poll refitting/cancellation and a
mobile viewport with keyboard editing and metadata retry. Existing FV-05/FV-06
browser tests remain independent numerical/worker checks.

Background prefetch/ready notifications and persistent model-cache management
are deferred as planned. Worker capacity retains FV-06's conservative memory
budget and can be **zero** on an unsupported device; the run button then explains
why it is disabled. A mobile viewport check does not accept actual handheld RAM
or latency. FV-08 must still measure and accept the complete supported resource
envelope, predictive behavior and browser coverage before activating defaults.
