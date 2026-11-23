## 2026-11-02 — R and temporary bridge retirement

Retired the R implementation, RStudio project/session files, Quarto workflows
and rendered examples/caches, Excel scenarios, legacy `dati/`, R exporters and
benchmark/check commands. Removed the two temporary Node bridge exporters and
the one-time R/TypeScript Other checker. Kept the four Node default/catalog
compilers in `scripts/`: the production pipeline still invokes them.

The three local `web/static/data/v1/politics-*-debug*.json`/debug-scrutiny
bridges were removed. `npm run build` now invokes Vite directly; its former
post-build debug-file filter is unnecessary. Added an asset-boundary regression
that rejects debug files in `static/data/v1`. Renamed snapshot test descriptions
to distinguish frozen historical regression from Node production contracts.
No legal algorithm, model, generated default or fixture expectation changed.

Frozen JSON/gzip references remain under `test/fixtures/`, including the
checksum-pinned R outputs and the independent Camera simulation-584 failure.
They are regression evidence consumed directly by TypeScript, not temporary
runtime/preparation bridges. Historical benchmark receipts and research evidence
remain. `test/fixtures/README.md` documents this boundary and versioning rules.

The pre-retirement revision is
`9bcbe403342c2570c83f547f7ae70adddec9961f`. It retains the R sources and Excel
workbooks cited by `data/workspace-defaults.json`; those provenance paths/hashes
remain historical identities. Municipal simulation is still unimplemented in
TypeScript, and no municipal golden fixtures or acceptance are claimed. The
future municipal step now explicitly requires an isolated historical checkout
for reference/fixture capture before translation. This removal does not resolve
the known previous-law Camera flipper failure or any deferred legal issue.

Automatic approval review rejected the initial recursive deletion command
(`blocked by policy`). Used a reversible move instead: 39 explicit paths,
including ignored local caches, were archived outside the repository at
`C:\Users\forna\Repos\elezioni-retired-r-20261102`. All resolved source and
destination paths were checked against their intended roots before moving.
The archive is a local recovery aid, not a build/test/preparation dependency.
An ignored-file audit also found eight old clean-checkout copies under
`data/derived/` and 42 third-party R sample/test scripts in the local Python
analysis environment. Archived those copies/scripts under the same root;
Python runtime files and independent analysis results remain in place.
Independent analysis, legal investigations under `_output/`, research archives
and authoritative `data/` sources were preserved.

Rewrote the Italian user README for supported browser workflows, Node-only setup,
checks, export/import and remaining limits. Updated agent guidance, preparation,
workspace, Other and acceptance documentation, retired obsolete R ignore rules,
and labelled the old legal implementation reference as historical. Earlier
migration-history entries remain an account of their original state.

Layout decision: retain `web/`. The root still owns `data/`, `docs/`,
`test/fixtures/`, research and independent investigations. Flattening would
require coordinated changes to preparation roots, fixture URLs, replay bindings
and documented commands, with little benefit to this split. A root invocation
can use `npm --prefix web run <script>` without introducing another package or
workspace wrapper. No folder relocation was performed.

Verification after removal (Windows, Node v24.20.0, npm, two test workers):

- From `web/`, `$env:ACCEPTANCE_TEST_WORKERS='2'; npm run acceptance:reform`:
  passed on the first complete run. Complete Node production-data verification,
  preparation and application types passed; Svelte reported zero errors/warnings.
- **1,220 unit tests and 72 browser checks** passed, without skips or retries.
  Sequential Vite builds and browser checks covered `/elezioni` and `/`,
  Chromium/Firefox M01/M02 workflows, storage and archive round trips. Total
  recorded subprocess time was 602.4 seconds; this is functional acceptance,
  not a new 1,000-run predictive or resource campaign.
- Frozen compressed/uncompressed fixture hashes passed. The three runtime
  debug bridges are absent from both `web/static/data/v1` and `web/build/data/v1`.
- An ignored/hidden-file scan found no remaining `.R`, `.r`, `.RData`,
  `.Rhistory`, `.Rproj`, `.qmd`, `.rds` or `.rda` files outside `.git/`,
  including installed dependencies and old preparation directories.
- Changed-document local file links and `git diff --check` passed. The final
  root build remains available. Evidence: `data/derived/r-retirement-acceptance.log`
  and `data/derived/r26-21-acceptance/`.

Marked the retirement step complete. No municipal acceptance, legal correction,
commit, push, workflow activation or public deployment was performed.
