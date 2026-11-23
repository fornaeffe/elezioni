## 2026-11-02 — Shared web workspaces and politics defaults

Implemented the approved workspace-uniformity scope in the first two remaining
migration directives. The remaining clean-clone Node data-preparation migration
stays separate. The [workspace guide](../../web-workspaces.md) provides the focused
routing/defaults/data/UI task map and regeneration/check commands.

### Workspace and data boundaries

Added a typed registry for previous-law politics, reform politics with M01/M02,
and Emilia-Romagna regional elections. It owns supported combinations, routes,
capabilities and run limits; the worker validates the same combinations. Municipal
simulation remains unregistered/unavailable. Existing routes and the previous-law
politics default selection remain intact. Route components now compose adapters;
legacy execution moved out of the dispatcher into a lazy runner.

Shared the navigation shell, run controls, severity-separated messages and table
renderer. Legacy adapters share owned worker cancellation/teardown and draft
persistence, retaining their existing storage keys. Reform keeps its specialized
fit/checkpoint/pool/storage ownership. Memory and stored tables share a page-access
contract; presentation aggregation remains in the existing TypeScript helpers and
exports remain in `core/result-export.ts`. Imports inspect scenario metadata,
including cross-route one-shot handoffs, without parsing whole large result
archives. Unknown/mismatched combinations fail explicitly.

Reset confirms replacement of the draft and preserves the executed scenario,
results and their interpretation warnings. Stale results can restore their
original scenario. Workers terminate on cancellation/navigation; revision guards
reject late replies. Execution errors now retain real elapsed time and, for legal
run failures, simulation/chamber/reason context. No partial forecast tables are
manufactured after a fatal legacy error.

Generated exact schema/byte/SHA-256 descriptors for the existing R politics and ER
snapshots. UI/worker loading and required-data acceptance consume these contracts;
missing/corrupt data has no debug fallback. Added `data:workspaces:generate` and
`data:workspaces:check`; the existing politics default exporter invokes the
compiler, and `acceptance:reform` includes its check. These are runtime/default
contracts around existing assets, not replacements for the remaining R importers.
No legacy `dati/` consumer was removed.

### Shared authored default and compatibility

Both active politics paths use the established authored
`data/fast-share-future-scenario.json`: 24 lists, the two requested political
coalitions, autonomous lists, geographic filing and minority declarations.
The older-law compiler consumes promoted membership rules, reviewed comparison
weights and named poll references. Unspecified internal historical splits retain
explicit equal-member assumptions and unmapped-to-abstention behavior. Historical
lists retain projected means; national overrides are initially active for named
references without compatible historical membership. Camera/common special and
provincial reference means from the accepted reform default are explicitly
borrowed into the older shared field. Assumptions/provenance survive in
`defaultSource.notes`; reference SDs do not acquire new legacy semantics.

Autonomous lists keep `coalition: null` in the editable scenario. Singleton
candidate tickets are compiled only at the previous-law boundary, with rename,
removal, validation and candidate-template support. They do not appear as extra
political coalitions. Final visual review caught and corrected an initial leak
of these adapter tickets into the coalition editor.

Scenario schema 9 adds optional per-chamber constituency filing and minority
regions. Older scenarios without these fields retain their prior interpretation.
New coverage filters candidate slots and cross-list candidate reuse and conditions
shared latent votes per chamber while preserving each unit's non-valid fraction.
Typed arrays replace expanded per-simulation object arrays for explicit-filing
scenarios, with a 256 MiB numerical-workspace bound; chunks remain 50. Historical
scenarios use the untouched generation path. Local-only lists with zero national
historical support use an explicit candidate-weight fallback; old joins retain
their R behavior. No R or TypeScript scrutiny implementation was edited.

### Verification

Environment: Windows, Node 24.20.0, Chromium 148.0.7778.96. Commands below ran from
`web/`; builds and browser suites ran sequentially.

- `npm run check:data` and `npm run check`: passed; **zero Svelte errors/warnings**.
  Svelte MCP checks found no remaining component issues. Existing local Map
  builders are nonreactive calculations; effects retained for storage, external
  busy-state synchronization and asynchronous result-page loading are intentional.
- `npm test -- --maxWorkers=2 --reporter=json --outputFile=../data/derived/workspace-refactor/unit.json`:
  **102 files / 1,212 tests passed, zero skips**. Historical snapshot tests now
  explicitly use the frozen snapshot default rather than assuming a new UI
  default must equal the old workbook. New coverage tests include normal-draw
  equivalence with unrestricted filing, valid-mass preservation, owned copies,
  autonomous tickets, both chamber assemblies, rejected corrupt assets and the
  preserved Camera failure described below.
- `npm run data:workspaces:check`: passed. Reform cached release preparation,
  staging/build and `data:reform:shares:release:check` passed, including the staged
  production candidate. Source identity stayed
  `fast-share-source-v1-95d505ed1f7ea79c99cd0bf91a86dbc6a2d54c1f24fd9b37bec40a57e3c72511`;
  the regenerated local catalog is
  `fast-share-catalog-v1-2d9327e717551b20c4c0a3b801f0c2d5ee7f7d5695daea6455cea9cf7efcdd45`.
- `npm run build`, then prebuilt `npm run test:e2e -- --workers=2 --reporter=json`:
  **47 Chromium checks passed at `/` (50.0 s)**. Repeated build and suite with
  `BASE_PATH=/elezioni`: **47 passed (49.0 s)**. Both had zero skips/retries/flaky
  results. Reports are `data/derived/workspace-refactor/browser-root.json` and
  `browser-project-base.json`. Covered direct loads, refreshes, navigation, both
  politics defaults, ER, archive/import compatibility, cancellation, reset/stale
  results, independent tickets, required-data errors and bounded stored results.
  Mounted 390×844 viewport checks and screenshots for all three workspaces
  confirmed document width 390 with no horizontal overflow.
- Before/after seeded generation of the frozen historical scenario, projection
  date `2026-11-02`, seed `workspace-parity-v1`, stayed byte-identical:
  SHA-256 `5485bc46c8d1bc648f4e48113f2e2d5da8abed3cee7ef4047bd4f1d9107d5dde`.
- Updated the constructed production-worker harness to serve all emitted lazy
  dependency chunks at both bases. Updated a stale FV-04 reference fixture to
  reference its fitted list basis under the current contract, retaining its
  Node/browser equality assertions. An existing Node type-overload issue in the
  research-boundary test was fixed by explicitly converting directory entries to
  strings. Import-error browser assertions now use the separate error section.

### Preserved Camera failure and benchmark qualification

`POLITICS_BENCHMARK_OUTPUT=../data/derived/workspace-refactor/politics-benchmark.json`
with `npm run benchmark:politics` successfully completed **10 runs in 4.637 s** and
**100 runs in 12.948 s**. The **1,000-run request failed**, reproducibly, at global
simulation **584 / Camera**, seed `politiche-2027`, chunk start 551, local run 34:
`Non ho a chi dare il seggio eccedente`.

This is the preserved exception at `scrutiny.ts`'s Camera national/constituency
flipper, also present in `R/politiche/scrutinio.R`. Input counts are finite,
nonnegative and match declared filing. At the failure, Futuro Nazionale retains
one excess seat; the remaining deficit subject is Die Freiheitlichen. Its eligible
constituency row already received a remainder seat, so the unchanged recipient
filter returns no row. This records program behavior, not an approved legal
interpretation or correction. Replayed the exact input independently and froze
the failure in the generated-production test. Raw replay/trace evidence is under
ignored `data/derived/workspace-refactor/legacy-failure*.json`. The Camera input
remained byte-identical after the autonomous-ticket presentation correction:
SHA-256 `fad5b52c83fff7e5d5983c1473277ad38bd404f585d61e6c6ae5097f79759c28`.

The benchmark now fails immediately on an execution error and writes its passing
prefix plus failed-request diagnostic, rather than waiting seven minutes for an
absent result table or losing successful timings. The existing tracked benchmark
receipt was not overwritten. Added a separate open legal-review/publication
blocker in the active plan. No seat-transfer fallback, reroll, skipped simulation,
changed denominator or accepted 1,000-run result is claimed. M01/M02 probability
laws, broader predictive acceptance and their earlier resource campaigns remain
outside this workspace refactor.

The two workspace/default directives are complete; Node importers/clean-clone
bundles, bridge retirement, the newly recorded legal blocker and the final
hosting/publication gate remain open. No commit, push, deployment, new Linux
runner measurement or full cold-source preparation acceptance occurred.
