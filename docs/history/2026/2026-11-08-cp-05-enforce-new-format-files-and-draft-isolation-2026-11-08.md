## CP-05 — Enforce new-format files and draft isolation (2026-11-08)

Implemented the [common file/draft API](../../common-politics-scenario/files.md) under
`web/src/lib/politics/files/`, with strict inferred schemas in
`politics/scenario/envelopes.ts` and generic result-export entry points reexported
from `core/result-export.ts`. Scenario, result and archive versions remain
independent. Results preserve the complete detached executed scenario, both saved
generator choices and inactive settings, actual execution provenance and stage
keys, worker benchmark, session identity/outcomes, diagnostics and columnar cells.
Parsing checks exact supported versions/pairs, dependency and seed identities,
coverage, simulation membership, unique columns and row widths. No scrutiny,
generation policy, model version or frozen legal expectation changed.

Archive import stages rows/diagnostics through a separate sink and commits only
after EOF and complete identity/checkpoint validation. Late corruption aborts
staging instead of replacing current results. Metadata inspection cancels after
routing headers without parsing result details; it is explicitly not a complete
validity check. JSON/gzip readers reject duplicate keys, trailing data and invalid
UTF-8. Row decoding stops at the 4,096-cell bound. Export streams external row
sources, diagnostics and portable-state chunks without constructing a whole
archive string.

The new checkpoint wrapper binds the full common-scenario fingerprint, exact
active dependencies, model, fitting seed, numerical policy, preparation key and
implementation checksum. It retains the existing v1 fit/profile numeric codecs,
verifies hashes, dimensions, sampler/diagnostic policy, dates and metadata parents,
and rejects former scenario/catalog/runtime states. Portable bytes are bounded
at 64 MiB and base64 transport strings at 64 KiB; final review replaced a single
potentially oversized fitted-asset token with individually streamed chunks. The
execution owner still checks the restored component/geography basis against its
loaded exact assets before reuse; durable reuse integration belongs to CP-07.

`readPoliticsDraft`/`writePoliticsDraft` access only
`elezioni:politics:common-draft:v1` and validate before storage writes. Both old
politics draft readers/writers were removed from the transitional browser
editors; their old keys stay unused and untouched. Old external politics files
and scenario-bearing checkpoint restores fail before changing draft/results or
routing to another editor. Independent regional routing, formats and persistence
retain their existing behavior.

The projected editors cannot represent the complete common scenario without the
conversion excluded by D12. Their edits therefore remain memory-only until
CP-06/07 wire the new draft/file/session APIs. Valid common files are readable
through the new APIs; the transitional upload guard preserves the current screen
and explains that loading requires the new editor. Existing internal projected
export/read helpers remain until CP-08 retirement. Retained result-presentation
browser evidence now seeds isolated test storage directly rather than requiring
old files to pass public upload validation. Historical fixtures/checksum manifests
remain separate, unchanged evidence.

Verification on Windows x64 / Node 24.20.0, from `web/`:

- Final complete `data:prepare` passed in 148.90 s; read-only `data:check` passed
  in 23.71 s. Both verified all 20 artifacts with receipt SHA-256
  `035e2ce040d797204b2702793f3fdd4f555f2b8265260f1b4f9747618c96899e`.
  Release checks filled all 588 domestic seats with zero default-start fits.
- `check:data` passed; `check` reported zero errors/warnings. Svelte MCP's initial
  component pass found no issues (existing lifecycle/local-map suggestions were
  reviewed without unrelated refactoring). A final remote repeat failed at the
  Svelte MCP HTTP transport; current local Svelte/TypeScript checks still passed.
- `npm test -- --maxWorkers=4`: 110 files, **1,295 tests passed**, no skips,
  35.02 s. The final checkpoint streaming refinement also passed its focused
  eight-test rerun, including JSON/gzip roundtrips, exact bindings, bounded chunks,
  late-corruption rollback, metadata inspection and draft-key isolation.
- Final static builds passed at `/` (5.10 s) and `/elezioni` (8.00 s), with the
  existing plugin-timing/chunk-size notices. Sequential prebuilt Playwright
  acceptance passed **53/53 at `/`** (42.7 s) and **53/53 at `/elezioni`** (46.3 s),
  no retries/skips. This includes actual-browser-worker gzip archive codecs,
  commit/abort behavior, old-file rejection with current draft/results preserved,
  both ignored old draft keys, common-file transitional isolation, regional
  navigation, all three common compute pairs and retained presentation evidence.
- Documentation file links, frozen-evidence diff audit and `git diff --check`
  passed. Local acceptance logs remain under
  `data/derived/cp05-acceptance-20261108/`, outside the tracked tree.

The first unrestricted unit run overlapped type checking and hit two existing
five-second computation timeouts; the bounded-worker full rerun passed. Browser
updates initially used an obsolete coalition value/error-region label; current
labels corrected those test selectors. A preparation attempt caught a stale
implementation pin after source edits during hash capture; stable-code
regeneration and the matching final read-only check passed. No pin, default or
regression output was edited by hand.

Marked CP-05 complete. CP-06–08, the parent merger, durable common-session/editor
integration, final 1,000-run/resource acceptance and publication remain open.
No commit, deployment or manual user acceptance is claimed.
