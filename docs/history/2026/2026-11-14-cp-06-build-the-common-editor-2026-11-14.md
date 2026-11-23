## CP-06 — Build the common editor (2026-11-14)

Implemented the capability-driven common editor under
`web/src/lib/politics/editor/`; use the [component map](../../common-politics-scenario/editor.md)
for task-specific entry points. The review route `/politics/editor` is linked
from `/politics` and uses the new shared draft key. Kept the existing simulator
available during this step: replacing its execution/session owner belongs to
CP-07, rather than disabling working simulation while delivering the editor.

The editor contains shared subject presentation/affiliation/minority controls,
per-law tri-state filings and lazy candidate trees, shared candidate identities,
future Camera/Senato membership locks, source/option membership and M01
composition controls, M02 whole-component pooling, fallback/component references,
the visible basic non-valid reference and advanced national/chamber/local trees,
read-only electorate data, candidate-generation settings and separate seed/worker
controls. Deferred D14 declarations remain stored. Ordinary reform slots use
the existing seven-candidate legal contract; no scrutiny or generation policy
was changed.

Law switching restores its saved generator; model edits affect only that law.
Detached validated edits reject conflicts atomically. Filings retain inactive
pins; pin removal retains identities and other positions. Bonus controls use
the shared resolver, retain inactive reciprocal declarations with reasons,
filter eligible counterparts, disclose replacements and clean up on deletion.
SD controls preserve explicit values, resolve/persist defaults, distinguish zero
from absence and remain hidden but stored under M01. Model-family feasibility
and complete legal validation remain authoritative at the execution boundary.

Automatic editor loading verifies pinned assets and selected model parameters.
Scenario imports stage decoding, checksum verification and contextual validation
before replacement; newer edits/imports or disposal defeat delayed replies.
JSON/gzip downloads preserve inactive inputs. Storage errors retain the visible
in-memory draft and report that it was not saved. Former draft keys and regional
routing remain independent. `RunControls` exposes the optional CP-07 execution,
fit, checkpoint and cancellation port; the review host explicitly disables those
actions. Loading editor data is not claimed as a statistical fit or a result run.

Verification on Windows x64 / Node 24.20.0, from `web/`:

- `npm run check`: zero errors/warnings. Svelte MCP checked all 15 changed/new
  Svelte files; corrected components were checked again with no remaining issues
  or suggestions.
- `npm test -- --maxWorkers=4`: **111 files, 1,303 tests passed**, no skips,
  28.80 s. Eight new pure-editor tests cover scope identity, default/explicit SDs,
  inactive/model-specific declarations, per-law selections, JSON/gzip roundtrips,
  atomic membership conflicts and candidate option reasons.
- Static builds passed at `/` (Vite 5.51 s) and `/elezioni` (5.01 s), with the
  existing plugin-timing/chunk-size notices. Sequential prebuilt Playwright runs
  passed **14/14 at `/`** (15.0 s) and **14/14 at `/elezioni`** (14.5 s), no
  skips/retries. Nine editor cases cover selections/reload/both file encodings,
  default/reset/import failures, non-valid/local references, new lists/locks,
  candidate identities and inactive sparse pins, bonus link transitions,
  pooling/source/fallback/electorate forms and keyboard/mobile layout. Five
  retained cases cover startup/reset, former draft/file isolation, navigation
  and an actual Emilia-Romagna worker simulation.
- Inspected the mobile render at 390 px, with visible keyboard focus and no
  horizontal overflow. Expensive selectors/trees remain absent until expanded.
  `git diff --check` passed; no generated defaults, pins, data or frozen fixtures
  changed.

Browser verification caught a proxied pending-link object passed to
`structuredClone`; immutable `$state.raw` fixed it. A scope-key check also
removed JSON property-order dependence for imported local references. Test
selectors now use accessible combobox roles and the native disabled property
for options; a mixed parent checkbox test explicitly changes a checked leaf
because native `uncheck()` is a no-op when a mixed checkbox is not checked.

Marked CP-06 complete. CP-07/08, the parent merger, execution/checkpoint/session
integration, the final three 1,000-run campaigns and publication remain open.
No commit, deployment or manual user acceptance is claimed.
