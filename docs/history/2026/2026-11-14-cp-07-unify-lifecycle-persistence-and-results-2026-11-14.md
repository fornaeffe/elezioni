## CP-07 — Unify lifecycle, persistence and results (2026-11-14)

Connected both politics routes to `CommonPoliticsWorkspace` and one detached
`PoliticsWorkspaceController`. The shared draft persists through law/model
switches and reload; storage failures retain the in-memory edit. Viewed sessions
remain independent: edits/switching preserve their executed identity and stale
indicator, browsing/importing never replaces the draft, and explicit scenario
loading validates before replacement. A missing selected session is reported
without substituting another. The session index uses a new common key and leaves
former politics keys untouched.

The coordinator automatically loads exact dependencies and prepares/reuses the
selected model. Separate bounded national-fit and profile caches avoid M02 refits
for presentation, inactive, candidature or simulation-seed edits; fitting-seed
changes retain compatible profiles. One M01 preparation is retained in memory.
A one-entry IndexedDB cache stores verified new-format M02 states for reuse after
reload, independently of the live draft. Explicit checkpoints retain the stricter
whole-scenario/dependency/build binding. Worker packets carry validated M02
numeric parents and states instead of repeating sampling in every compute worker.
The actual namespaced implementation key is kept for execution; portable envelopes
store its raw SHA-256 and numeric metadata binds both representations.

Execution locks edits before awaiting preparation, exposes explicit cancellation,
records requested/effective workers and disposes nested workers on navigation.
Each seed button remains an ordinary edit of only its own visible field; loading,
preparation and running preserve both seeds. Progress is throttled, and numerical
steppers yield worker tasks so cancellation/new revisions can be handled during
long fits. The recommendation for 1,000 runs requires ten complete outcomes and
never changes the count or starts a run.

All three pairs now use the existing ordered bounded pool, chunk store, lazy
queries, disk statistics and OPFS streaming export service. Manifest commits
publish only whole simulations; cancellation drains assigned IDs and quota
failure recovers the durable prefix, preserving outcomes and the original
requested denominator. Rosatellum's 1,000-run ceiling is removed; positive safe
counts, CPU worker limits and a 64 MiB compact membership estimate remain bounded.
Rosatellum uses the conservative prior 1536 MiB scrutiny scratch budget plus
actual packet copies; national footprint calibration remains CP-08 evidence,
not a claim about currently free RAM or measured performance above 1,000 runs.

Rosatellum presentation adapts the existing plurinominal list and total-support
seat meanings to the tested disk-series contract, resolves typed ticket names
and colors, preserves explicit zero, and supplies an exact diagnostic-event
ledger. Reform-only bonus/position views are hidden for Rosatellum. Common JSON.gz
archives stream their immutable scenario, provenance, membership, rows,
diagnostics and portable M02 checkpoint; CSV stays table-oriented. Imports stage
under a fresh local ID, validate EOF/identities/checkpoints and row/diagnostic
membership, then select without editing the draft. Generic synchronous retired
export helpers reject common scenarios rather than emit an old envelope.

Retained independent regional parsing/storage. Common and regional file metadata
now hand off to the appropriate route from scenario inputs, with delayed-reply
guards; regional result uploads keep their gzip contract and reject old politics
formats. Updated the scoped editor/file/execution/workspace guides and added the
[lifecycle entry-point guide](../../common-politics-scenario/lifecycle.md).

Verification from `web/`:

- `npm run check`: zero errors and warnings; `npm run check:data` passed.
- `npm test -- --maxWorkers=4 --reporter=dot`: 114 files / 1,315 tests passed.
  A concurrent unrestricted run hit the existing 5-second production local-
  override test timeout; the complete suite passed with four test workers,
  without changing that test's timeout or its expected output.
- Static builds passed for `/` and `/elezioni`. The common editor, lifecycle,
  files, worker and regional-smoke suites passed 20/20 at each base with two
  browser test workers. Final focused lifecycle/file/compute checks were repeated
  after the synchronous-export guard, using matching freshly built assets.
- Lifecycle browser cases compare actual stored observations and diagnostic rows
  for all three pairs with one and two compute workers, three simulations each;
  exercise JSON.gz archive export/import, queries, selected-session reload, stale
  results, explicit scenario loading, exact checkpoint restore and cache reuse.
  Cancellation requests ten runs and verifies retained contiguous membership.
  Injected quota failure retains exactly simulation 1 of 3 through reload with
  `RESULT_STORAGE_ERROR` and the original denominator. Cross-route imports and
  corrupt-file retention also pass.
- Svelte MCP found no issues in changed common components. The regional component
  retained only existing advisory suggestions for local nonreactive Maps,
  attachments and its prior busy callback; its change is confined to file routing.
  `git diff --check` passed. Authored defaults, generated pins/data and frozen
  legal fixtures/checksum manifests were not edited.

Browser checks initially exposed excessive progress traffic and promise-only
worker stepping that delayed cancellation; throttling plus regular task yields
fixed this and restored the production editor suite. Streamed membership checks
were also corrected to validate nullable global diagnostic rows by their scope,
while retaining strict membership for simulation rows.

Marked CP-07 complete. CP-08, the parent merger and publication remain open:
retarget superseded workspace-specific browser suites before the full production
acceptance gate, retire old mounted-flow helpers, measure national resources and
run the three sequential 1,000-simulation campaigns. No campaign above 1,000 was
run; the 1,001-count check validates configuration only. Existing legal failures
and their frozen evidence remain unchanged. No commit, deployment or manual user
acceptance is claimed.
