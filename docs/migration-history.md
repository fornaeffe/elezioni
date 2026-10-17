# Migration History

Record through: 2026-10-17

This file preserves the detailed migration record: completed implementation,
decisions and rationale, verification results, encountered problems,
workarounds, risks, and dated checkpoints. It is historical context, not the
active task list. See [`../MIGRATION_PLAN.md`](../MIGRATION_PLAN.md) for the
current directive-only plan and [`../AGENTS.md`](../AGENTS.md) for stable agent
instructions.

## Purpose

The sections before the checkpoint log capture the consolidated state through
2026-06-06. The dated checkpoints retain the implementation
sequence and the evidence available at each point in time.

## Initial R Baseline And Constraints

Measured on 2026-06-01 with R 4.5.1 at
`C:\Program Files\R\R-4.5.1\bin\Rscript.exe` and 28 detected cores:

- `simula_comunali("Bologna", ..., 1000)`: about 3.0 seconds elapsed.
- Emilia-Romagna regional workflow, 1000 simulations: about 6.8 seconds.
- `simula_politiche(..., 10)`: about 7.1 seconds.
- `simula_politiche(..., 100)`: about 20.8 seconds.

The 100-simulation politics phases measured about 0.4 seconds and 47 MB to load
filtered cached data, 0.1 seconds and 3 MB for `calcola_collegi`, 3.2 seconds
and 8 MB for `calcola_parametri_input`, 3.3 seconds and 33 MB for
`genera_candidati`, 2.4 seconds and 25 MB for `genera_voti_politiche`, and 9.3
seconds with a 37 MB output for `esegui_scrutini_politiche`.

The ten-simulation debug scrutiny fixture round-tripped exactly against the R
output, including the warning-producing simulation 4. At this point
`R/politiche/simula.R` stated that politics simulation required at least 16 GB
of RAM; this motivated the browser requirement to avoid large row-object
materialization and to use typed arrays, chunking, or worker-side columnar data.

## Initial Target And Performance Gate

Target architecture: a SvelteKit static TypeScript app under `web/`, with
simulation and scrutiny running in browser workers. The first migrated workflow
is `politiche`, because it has the largest legal and performance risk.

The browser path remains preferred as long as the optimized politics workflow is
not 10x or more slower than the same full R baseline on the same machine. If a
future browser benchmark reaches that stop threshold after one optimization
pass, pause and present Python fallback options before continuing.

## Consolidated State Snapshot (2026-06-05)

| Area | State | Evidence or reference at the time |
| --- | --- | --- |
| Migration tracking | Done | Root agent guidance and migration plan were present. |
| R safety fixes | Done | `candidati_pluri_sim_` renamed to `candidati_pluri_sim`; debug scrutiny still matches. |
| Politics golden fixtures | Done for current politics slice | `scripts/export_politics_golden.R`, `test/fixtures/politiche/debug_scrutinio.json`. |
| R and browser benchmarks | Done for current politics slice | `scripts/benchmark_r_workflows.R`, `web/tests/benchmarks/politics-worker.spec.ts`, benchmark JSON under `test/fixtures/benchmarks/`. |
| SvelteKit app scaffold | Done | `web/` with strict TypeScript, static adapter, Vitest, Playwright. |
| Shared TypeScript core | Usable, still growing | Worker API types, seeded RNG, allocation primitives, scenario types, severity-aware warning/result contracts, politics result presentation, result export/import helpers, R-style politics result charts. |
| Politics scrutiny | R-parity direct fixture passes | `web/src/lib/politics/scrutiny.ts`; registry id `politiche-r-parity-v1`. Split only when boundaries are clearer. |
| Politics generation pipeline | Current browser path working | Candidate generation, vote generation, vote preparation, direct-scrutiny adaptation, worker chunking. |
| Production static politics snapshot | Bridge done, richer raw data added | `scripts/export_politics_static_snapshot.R` writes schema v4 `web/static/data/v1/politics-static.json`, including raw historical municipal list votes, a compact municipality catalog for UI lookup, and named politics college metadata for candidate-slot selection. |
| Politics scenario defaults | Bridge done | `scripts/export_politics_scenario_defaults.mjs` writes `web/src/lib/scenario/politics-defaults.generated.ts`. |
| Scenario editor | Core workflow plus compact advanced controls working | List/coalition/share editor, mean-mode global share overrides, separate abstention input, local percentage override editor, historical correspondence editor, JSON save/load, localStorage, reset, validation. |
| Rich correspondence parameter builder | Done and wired | `web/src/lib/politics/parameter-preparation.ts` rebuilds politics `liste`, `liste_elezioni`, and `comuni_liste` from raw historical votes and correspondences. `scenario-projection.ts` uses it when snapshot raw votes are present. |
| R correspondence audit | Done | Current R output is coherent for the politics workbook because all historical list keys are mapped, but `calcola_parametri_input.R` drops unmatched rows instead of automatically sending them to `astensione`. |
| Generated data storage | Done | Large generated JSON snapshots/fixtures are ignored and untracked; regenerate locally from scripts. Small generated TypeScript remains tracked. |
| Politics browser vertical slice | Stabilized | `web/src/lib/politics/vertical-slice.test.ts` guards snapshot -> scenario -> projection -> generation -> scrutiny. |
| Performance gate | Passed | Chromium worker on the R-exported production static snapshot is below fresh full R politics baselines for 10, 100, and 1000 simulations. |
| Emilia-Romagna regional workflow | Browser slice migrated | Static snapshot, default scenario, vote generation, scrutiny, worker, UI, parity tests, e2e, and benchmark are in place. |
| Municipal workflow | Not started | Migrate after the politics and Emilia-Romagna browser workflows are stable enough. |
| Data-preparation migration | Deferred | Reassess typed Python vs Node/TypeScript after simulator workflows are migrated. |

## Superseded Plan Snapshot (2026-06-05)

This was the ordered implementation plan at the time. It is retained because
its completion notes and sequencing rationale are part of the record; use the
root plan for current work.

1. Keep the politics browser vertical slice green while scenario semantics grow.
   Every change to defaults, projection, generation, worker behavior, or the
   scrutiny registry should preserve or deliberately update
   `web/src/lib/politics/vertical-slice.test.ts`.
2. Implement richer correspondence semantics in staged slices:
   raw historical municipal list votes are in the static politics snapshot, and
   the `calcola_parametri_input()` correspondence/parameter math now exists in
   a tested TypeScript module with unmapped original-list votes explicitly
   falling back to `astensione`.
3. Extend scenario/UI semantics for historical correspondences. Done for this
   slice: the advanced editor now exposes grouped historical correspondence
   rows with destination, factor, split, delete, and reset controls; the old
   source-model reuse concept has been removed from the UI and scenario
   helpers; projection treats every active scenario list as simulatable and
   generates candidate slots from the legal college grid independently of
   historical vote correspondences.
4. Keep the current global mean-mode share override semantics stable while the
   scenario model grows. Done for this slice: list shares are valid-vote
   percentages, abstention is a separate advanced elector-share input,
   projection converts to internal elector fractions, non-overridden list
   fractions are normalized from source data, all-overridden shares are
   normalized with a warning when needed, and overridden list model dates are
   set to the projection current date. Fixed modes are explicitly deferred.
5. Build the UI for local percentage overrides only after the data contract has
   been exercised on production data. Done for this slice: scenario JSON
   carries municipality/province/region list valid-vote share overrides,
   validation guards malformed or impossible partial local totals, projection
   keeps local abstention fixed from the base municipality model, calibrates
   province/region targets across covered municipalities while preserving
   historical geographic variation, applies more specific overrides after
   broader ones with overlap warnings, recomputes municipal `DELTA`/`DATA` rows
   before vote generation, and the advanced scenario UI exposes searchable
   municipality/province/region list/share editing.
6. Preserve architecture hooks for future fixed behavior without exposing it:
   a globally fixed mode should bypass random vote generation and produce a
   single deterministic vote distribution/scrutiny output; an optional
   per-percentage fixed mode may later remove uncertainty for selected global
   shares or local deltas. Both require a separate design pass.
7. Build the rich candidate-template editor only after the slot-selection UX is
   clear. Done for this slice: scenario JSON carries uninominal and
   plurinominal candidate templates, validation guards malformed or duplicate
   slots, projection applies matching templates to the worker candidate source,
   unmatched templates warn while generated candidates still fill every
   unspecified place, and the advanced scenario UI exposes a slot-by-slot
   candidate editor backed by a compact generated named-college catalog.
8. Continue improving politics result presentation. Done for this slice:
   `web/src/lib/politics/result-presentation.ts` builds tested user-facing
   summary tables for election overview, plurinominal seats by list, valid-vote
   share distributions by list, and uninominal winners by support. JSON/CSV
   result export helpers and UI buttons are also implemented. Worker messages
   now carry optional severity, and the UI separates informational run notes
   from real warnings/errors. R-style web charts now mirror the current
   `presentazione_risultati.R` politics report slice: summary bars, simulated
   valid-vote boxplots, list and coalition seat-vote scatter plots, list
   spinograms, and a selectable plurinominal-college spinogram using
   `NUMERO_MAX`. Spinograms use darker colors for higher seat counts,
   contrast-aware labels, x-axis valid-vote percentage break labels, and
   click-to-enlarge overlays. Keep diagnostic tables such as
   `Generated pipeline runs` collapsed by default. Bundled result exports use
   compact columnar result tables, are downloaded as `.json.gz`, and can be
   re-imported to restore both the generating scenario and displayed results
   without rerunning the worker.
9. Refactor politics scrutiny only when it lowers risk. The likely target is
   stage-focused modules behind the existing scrutiny algorithm registry, but
   do not split during active parity discovery just for size alone.
10. Add a second politics scrutiny algorithm only for a concrete law-review or
   comparison need. Expose UI selection/comparison only after at least two real
   same-election-kind algorithms exist.
11. Repeat the politics browser performance gate after major scenario, data,
   generation, or scrutiny changes.
12. Done 2026-06-06: Emilia-Romagna regional workflow migrated with R golden
    fixture export, static snapshot export, scenario defaults, shared vote
    generation, R-parity scrutiny, worker dispatch, `/emilia-romagna` UI,
    landing selector, e2e coverage, and regional browser/R benchmarks.
13. Migrate the municipal workflow: first add R golden fixtures and benchmarks,
    preserve current behavior, and keep known runoff/councilor-candidate
    business TODOs explicit for later law review.
14. Generalize the web app across election kinds: routing, snapshot selection,
    scenario defaults, worker dispatch, result components, validation, and
    shared UI patterns.
15. Migrate data preparation after simulator workflows are migrated. Reassess
    typed Python versus Node/TypeScript then; keep it a periodic,
    election-kind-agnostic devops/GitHub Actions pipeline producing static
    previous-election bundles.
16. Retire temporary bridge artifacts and update user/developer documentation
    once migrated workflows no longer depend on R-exported intermediary
    snapshots.

## Completed Foundations Snapshot (2026-06-05)

- Created durable migration notes in `AGENTS.md` and this living plan.
- Measured local R baselines and created repeatable benchmark scripts.
- Renamed the R politics output key to `candidati_pluri_sim` and verified the
  debug scrutiny fixture still matches.
- Exported politics golden-master fixtures and direct scrutiny trace tables.
- Scaffolded the SvelteKit static app with strict TypeScript, Vitest,
  Playwright, static adapter, worker API types, seeded RNG, and allocation
  primitives.
- Ported politics scrutiny enough for the direct debug fixture to match R final
  outputs across Camera and Senato.
- Added the politics scrutiny algorithm registry with `politiche-r-parity-v1`
  as the current default.
- Ported the politics candidate generation, vote generation, vote preparation,
  and direct-scrutiny input adaptation path with focused fixture tests.
- Built the generated politics worker path with chunked execution and progress.
- Exported the R-produced production static snapshot bridge at
  `web/static/data/v1/politics-static.json`.
- Upgraded the production static snapshot bridge to schema v4 with
  `data.comuni_liste_elezioni`, the raw historical municipal list votes needed
  to rebuild correspondence-based parameters in TypeScript, plus
  `data.municipalities`, a compact municipality/province/region catalog for UI
  lookup, and named uninominal/plurinominal college metadata for candidate-slot
  lookup.
- Added `web/src/lib/politics/parameter-preparation.ts`, the TypeScript port of
  the deterministic `calcola_parametri_input()` parameter math from raw
  historical municipal votes and list correspondences. Synthetic tests cover
  factor normalization, one-to-many splits, many-to-one aggregation, and
  unmapped original-list votes becoming `astensione`; gated parity tests rebuild
  the R-exported production default parameters when local generated data exists.
- Wired the parameter builder into `scenario-projection.ts` and the worker when
  raw historical votes are present. Scenario lists now receive
  historical/static/synthetic parameters directly, without a source-model reuse
  layer.
- Stopped tracking large generated JSON snapshots and bridge fixtures in Git.
  They remain local/generated artifacts and snapshot-dependent tests skip
  clearly when they are absent.
- Generated the default web politics scenario from that static snapshot,
  including bundled list correspondences.
- Built the basic scenario editor: metadata, list/coalition/share editing,
  validation, reset, JSON import/export, localStorage autosave, and plain
  worker-safe scenario snapshots.
- Added scenario schema-v4 abstention fields and an advanced UI input for
  abstention as a percentage of electors. Projection now treats list shares as
  valid-vote percentages and converts them to elector fractions using the active
  abstention fraction.
- Added scenario schema-v5 local share overrides and projection hooks; the same
  scenario array now supports municipality/province/region list valid-vote
  share overrides.
- Added scenario schema-v6 candidate templates and projection hooks for
  uninominal/plurinominal candidate slots. Matching templates pin
  `CANDIDATO_ID`/`DATA_NASCITA` before candidate generation; unspecified slots
  remain generated.
- Added `web/src/lib/scenario/politics-candidate-slots.generated.ts`, a compact
  generated catalog of named uninominal and plurinominal candidate slots.
- Added `web/src/lib/politics/result-presentation.ts`, a tested politics
  presentation boundary that turns scrutiny runs into primary result tables:
  election overview, plurinominal seats by list, valid-vote share distribution
  by list, and uninominal winners by supporting coalition/list.
- Added `web/src/lib/core/result-export.ts` and result-panel export actions.
  Users can download a compressed JSON payload containing scenario plus worker
  result, or a sectioned CSV containing all result tables.
- Added strict bundled result JSON import. The Results panel can load app
  exports containing both scenario and result, and the existing scenario upload
  accepts the same bundled file while preserving result provenance. Result JSON
  export schema v2 is the only supported result schema and stores table rows
  columnarly to reduce repeated keys; result files are imported/exported through
  gzip-only `.json.gz` in the UI.
- Added optional severity to `ScrutinyWarning` and split result-panel messages
  into informational run notes versus warnings/errors. Production static
  snapshot and scenario-projection metadata now render as notes instead of
  warning-looking messages.
- Added `web/src/lib/politics/result-charts.ts` and
  `web/src/lib/politics/PoliticsResultCharts.svelte`. The result panel now has
  tested chart extraction/rendering for average seats, mean vote shares,
  simulated valid-vote boxplots, list/coalition seat-vote scatter plots, list
  spinograms, and a selectable plurinominal-college spinogram.
- Removed the provisional global `fixed` UI/projection behavior. The active
  scenario share mode is mean-only; older serialized `fixed` values normalize
  to `mean` until fixed semantics get their own design pass.
- Added a grouped historical correspondence editor in the scenario advanced UI.
  It edits destination lists, split factors, deletion, and reset-to-default for
  bundled historical election/list rows.
- Added scenario projection from the web-native scenario into the generated
  worker source, including explicit share overrides, proportional recalculation
  for non-overridden active lists, historical/static/synthetic parameter source
  reporting, and candidate slot generation from legal college grids.
- Completed the current global mean-mode share override semantics: overridden
  valid-vote shares are converted to elector fractions, all-overridden totals
  that are not 100% are normalized with a warning, and overridden list
  parameter `DATA` values are set to the worker/projection current date so
  temporal drift starts from today.
- Added typed local share override hooks and the advanced local override UI.
  `Scenario.localShareOverrides` stores municipality/province/region list
  valid-vote share overrides. Projection expands aggregate province/region rows
  only in memory, weights covered municipalities by expected valid votes,
  calibrates shares to the requested aggregate target while preserving
  historical geographic variation, keeps local abstention fixed, and
  recomputes local `DELTA` plus `DATA` before the existing vote generator
  runs. The UI searches municipalities, provinces, and regions by name/code and
  edits grouped location/list percentages.
- Reworked the web page into a single-column scenario-then-results layout.
  Primary user-facing summaries appear before diagnostics, with generated
  pipeline details collapsed by default. The worker now delegates politics
  result summarization to `result-presentation.ts` instead of keeping
  presentation math in the orchestration layer.
- Added the production politics vertical-slice guard.
- Passed the current browser performance gate for 10, 100, and 1000 politics
  simulations.

## Decision Record Through 2026-06-05

- SvelteKit/browser remains the primary migration target because it gives the
  best usability and distribution story.
- Architecture may change freely, but business/legal behavior must match R
  unless Luca explicitly approves a correction.
- Scrutiny code is the highest-risk migration surface. Keep law comments close
  to translated logic, golden-master tests nearby, and `TODO(law-review)` on
  suspicious parity-preserving behavior.
- The web UI is the preferred long-term scenario editor. Excel scenarios do not
  need to remain a first-class migrated input.
- Scenario defaults come first from bundled defaults for election kind and
  territory; if absent, use the most voted lists from the last same-kind
  election in the same territory; if past coalitions are unavailable, each list
  defaults to its own coalition.
- Users should be able to specify only some global list shares. Unspecified
  list shares should be recalculated from previous results and list
  correspondences, preserving the current R model's intent.
- Rich correspondence semantics are now defined: original historical votes are
  projected into future lists per municipality/election; many originals can
  aggregate into one future list; one original can split across future lists by
  normalized factors; original abstention and unmapped original-list votes count
  as `astensione`.
- UI list percentages are percentages of valid votes, excluding abstention.
  Internal pipeline fractions are elector fractions and must include
  `astensione` to sum to 1. Abstention should be an advanced separate input,
  not a normal scenario list.
- User global share overrides in mean mode represent a current known
  `P_{l,t-1}` for that list, with `data_{t-1}` set to today.
- Fixed percentage behavior is deferred. A future globally fixed mode should
  bypass random vote generation completely, treat all percentages as fixed,
  and produce one deterministic vote distribution plus one scrutiny output; the
  municipal distribution method must reflect historical local distribution
  while guaranteeing the requested global percentages and is still to be
  chosen. A future optional per-percentage fixed mode may remove uncertainty
  for selected global shares or local deltas, but only after its interaction
  with local variation is explicitly designed.
- Location-specific percentages are first-class for municipalities, provinces,
  and regions. Province/region rows stay compact in scenario JSON and are
  expanded only during projection; region overrides apply first, then province,
  then municipality, so more specific rows win. Candidate-template editing and
  rich correspondence matrices remain advanced features to build only when the
  underlying data contract is stable.
- Scrutiny algorithms must remain modular and swappable. Keep the registry hook
  even while there is only one registered politics algorithm.
- Results UI should prioritize election outputs and keep diagnostic pipeline
  tables behind an explicit detail/debug affordance.
- Result charts should stay downstream of tested result-table/presentation
  boundaries. Avoid putting computation-specific aggregation in Svelte
  components; derive chart-friendly structures in small TypeScript helpers.
- Result exports should preserve both the scenario that produced the run and
  the worker result. CSV exports are table-oriented for spreadsheet use; JSON
  exports are the durable structured format for reproducing or reviewing runs.
- Worker messages should use severity deliberately: `info` for run metadata and
  benign implementation notes, `warning` for scenario/projection/result issues
  that may affect interpretation, and `error` for failed execution paths.
- Data preparation should be migrated last. It should remain a periodic
  devops/GitHub Actions workflow and stay agnostic about election kind. Typed
  Python was retained as an option because `pydantic` or dataclasses with
  `mypy`/`pyright`, plus `polars`, `pyarrow`, and optionally `numpy`, offer a
  strong validation and columnar-performance path. The trade-off is losing the
  install-free browser advantage unless the pipeline is paired with a server or
  packaged desktop workflow, so the Python-versus-Node decision was deferred.

## Verification Snapshot

Latest full web verification on 2026-06-05:

- `cd web; npx vitest run src/lib/scenario/politics.test.ts src/lib/politics/scenario-projection.test.ts src/lib/politics/parameter-preparation.test.ts`: passed, 31 tests.
- `cd web; npx vitest run src/lib/politics/result-presentation.test.ts`: passed, 3 tests.
- `cd web; npx vitest run src/lib/core/result-export.test.ts`: passed, 2 tests.
- `cd web; npx vitest run src/lib/politics/result-charts.test.ts`: passed, 3 tests.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 156 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test, including the
  advanced abstention, manual correspondence controls, result export buttons,
  politics result charts, and the spinogram enlarged-overlay interaction. The
  smoke test also checks that normal static-snapshot metadata is shown as an
  informational note, with no warning block for the default edited scenario.

Verification note: do not run `npm run build` and `npm run test:e2e` in
parallel. Both commands touch SvelteKit build/prerender output and can produce a
transient hashed-asset 404 even when each command passes alone.

Current performance gate:

| Workflow | Simulations | Elapsed | Fixture |
| --- | ---: | ---: | --- |
| R full politics workflow | 10 | 6.20 s | `test/fixtures/benchmarks/r_baseline_politics_10_compare.json` |
| R full politics workflow | 100 | 19.58 s | `test/fixtures/benchmarks/r_baseline_politics_100_compare.json` |
| R full politics workflow | 1000 | 151.96 s | `test/fixtures/benchmarks/r_baseline_politics_1000_compare.json` |
| Chromium politics worker, R-exported static bridge | 10 | 1.357 s | `test/fixtures/benchmarks/browser_politics_worker.json` |
| Chromium politics worker, R-exported static bridge | 100 | 13.194 s | `test/fixtures/benchmarks/browser_politics_worker.json` |
| Chromium politics worker, R-exported static bridge | 1000 | 123.229 s | `test/fixtures/benchmarks/browser_politics_worker.json` |

Gate result: pass. The 1000-simulation browser run is about 0.81x the fresh
full R workflow elapsed time, far below the 10x stop threshold. Repeat the gate
after major scenario/schema/snapshot/generation/scrutiny changes.

## Caveats And Risks Snapshot (2026-06-05)

- `web/src/lib/politics/scrutiny.ts` is large. Keep it stable while parity is
  still being discovered; split it into stage-focused modules only when the
  tested boundaries are clear.
- Several politics `sorteggio` paths preserve R stable ordering where law
  comments may imply random draws. These are marked `TODO(law-review)` where
  found and should be reviewed with Luca before business correction.
- The R candidate-only vote attribution appears inconsistent with its law
  comment: the comment says highest remainders, while current R effectively
  sorts `RESTO` ascending because `order()` receives six `decreasing` values
  for five keys and ignores the final `TRUE`. TypeScript preserves R behavior
  for parity.
- The R subentro block has parity-sensitive behaviors that look accidental:
  although `subentro(livello = "pluri", coal = TRUE)` and
  `subentro(livello = "circ", coal = TRUE)` describe same-coalition searches,
  the non-national plurinominal path calls `cerca_accettori(i)` without those
  arguments. Later `merge()` calls implicitly include the retained
  `CIFRA_PERCENTUALE` column as a join key, so ripescati uninominal candidates
  with `NA` can fail to receive `ELETTI`. TypeScript preserves both for parity.
- The production static snapshot is still an R-exported bridge, not the final
  migrated data-preparation pipeline. The snapshot JSON is ignored by Git
  because it is large and generated. Regenerate it with
  `Rscript scripts/export_politics_static_snapshot.R`, then regenerate
  dependent tracked defaults with
  `node scripts/export_politics_scenario_defaults.mjs` and
  `node scripts/export_politics_municipality_catalog.mjs` when the default
  scenario or municipality metadata changes.
- Bundled list correspondences in the generated default are durable metadata for
  defaults and the current advanced editor. Editing a bundled row marks that row
  manual; reset restores the generated default rows for that historical source.
- `web/src/lib/politics/parameter-preparation.ts` is wired into projection and
  the worker when raw historical votes are available. The editor now exposes real historical
  split/merge factors, and projection treats candidate slots as independent
  legal grid rows for every active scenario list.
- `abstentionOverride` currently makes the global `astensione` parameter fixed
  by setting its `SIGMA_GLOBAL` to zero. Local share overrides keep each
  municipality's base abstention fraction fixed; there is not yet a separate
  local abstention override schema or UI.
- Fixed percentage modes are deliberately not active. The scenario schema keeps
  the `globalShareMode` slot as `mean` for future extensibility, and legacy
  serialized `fixed` values normalize back to `mean`.
- `calcola_parametri_input.R` currently relies on exhaustive correspondence
  rows. Its data.table join uses `nomatch = NULL`, so unmatched original-list
  votes would be dropped from the calculated denominator instead of becoming
  `astensione`. The current politics workbook is exhaustive, so this does not
  affect the present default scenario, but the web implementation must not copy
  this fragility.
- New/unmatched scenario lists are ignored by the current static-snapshot
  worker path unless they reuse one current source-model list through a declared
  correspondence. The worker reports this as a warning.
- Placeholder uninominal candidates may be generated when a matched scenario
  list uses a coalition absent from the current candidate template. This keeps
  the browser workflow runnable but needs final scenario/data semantics.
- The generated worker path currently runs in 50-simulation chunks and is
  capped at 1000 simulations. Revisit after columnar packaging or if larger UI
  runs are needed.
- `npm audit` reports 3 low-severity findings through SvelteKit's transitive
  `cookie` dependency. The suggested automatic fix is a semver-major downgrade
  to obsolete SvelteKit packages, so it has not been applied.

## 2026-06-01 Checkpoint 2

Completed after the initial scaffold commit:

- Added Svelte 5 runes-mode guidance to `AGENTS.md`.
- Fixed `scripts/export_politics_golden.R` so simulations serialize as arrays
  and warning/message strings do not split into characters.
- Regenerated `test/fixtures/politiche/debug_scrutinio.json`.
- Added typed politics fixture/scrutiny types under `web/src/lib/politics`.
- Ported the first politics scrutiny stage: uninominal candidate election.
- Added golden parity tests for uninominal election across all 10 Camera and 10
  Senato fixture simulations.
- Refactored the scenario page to Svelte 5 runes style with `$state` and
  `$derived`.
- Fixed worker payload cloning by deriving a plain scenario snapshot before
  `postMessage()`.

Verification:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 27 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 3

Completed in the next scrutiny-port pass:

- Extended `scripts/export_politics_golden.R` to export schema v2 fixtures with
  early R trace tables for each Camera/Senato debug simulation.
- Regenerated `test/fixtures/politiche/debug_scrutinio.json`.
- Added TypeScript trace types for the early politics scrutiny stages.
- Ported the early politics scrutiny pipeline:
  - uninominal candidate election;
  - candidate-only vote attribution to linked lists;
  - uninominal list figures;
  - plurinominal list figures and percentages;
  - circumscription list figures;
  - uninominal candidate percentages;
  - circumscription totals.
- Added golden parity tests for all early trace tables across all 10 Camera and
  10 Senato debug simulations.
- Preserved the current R remainder ordering for parity and added a
  `TODO(law-review)` because the law comment says remainders should be ordered
  descending while the current R call sorts them ascending.

Verification:

- `Rscript scripts/export_politics_golden.R`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 47 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 4

Completed in the national-threshold scrutiny pass:

- Extended `scripts/export_politics_golden.R` to export schema v3 fixtures with
  national list figures, national/circumscription coalition figures, and
  threshold/admission flags.
- Regenerated `test/fixtures/politiche/debug_scrutinio.json`.
- Added TypeScript trace types for:
  - `liste_naz_soglie`;
  - `liste_circ_soglie`;
  - `coal_naz_soglie`;
  - `coal_circ_cifre`;
  - `totale_naz`.
- Added `runPoliticsScrutinyTrace()` with ramo/list metadata context.
- Ported national list/coalition figures and 1%/3%/10% threshold flags against
  the R trace.
- Added golden parity tests for the threshold trace across all 10 Camera and 10
  Senato debug simulations.

Verification:

- `Rscript scripts/export_politics_golden.R`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 67 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 5

Completed in the Camera national-riparto pass:

- Extended `scripts/export_politics_golden.R` to export schema v4 fixtures with
  nested `camera_riparto` traces.
- Regenerated `test/fixtures/politiche/debug_scrutinio.json`.
- Added TypeScript trace types for:
  - Camera national proportional riparto;
  - internal coalition/list riparto;
  - Camera list-to-riparto-subject mapping.
- Added the required `totali_pluri` and `totale_seggi` fields to
  `PoliticsScrutinyContext`.
- Ported Camera national proportional seat allocation and internal coalition
  allocation to `runPoliticsScrutinyTrace()`.
- Preserved R stable-order behavior for `sorteggio` tie cases and marked both
  national and internal-coalition riparto paths with `TODO(law-review)`.
- Extended golden parity assertions for all 10 Camera and 10 Senato debug
  simulations. Senato intentionally receives empty/null `camera_riparto` traces
  at this stage.

Verification so far:

- `Rscript scripts/export_politics_golden.R`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 67 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 6

Completed in the subject-level circumscription riparto pass:

- Extended `scripts/export_politics_golden.R` to export schema v5 fixtures with
  nested `circ_riparto` traces.
- Regenerated `test/fixtures/politiche/debug_scrutinio.json`.
- Added TypeScript trace types for:
  - circumscription totals with seats and quotients;
  - list-to-riparto-subject mappings;
  - subject-level circumscription allocation rows;
  - Camera national reconciliation counters after circumscription allocation.
- Ported the shared subject-level circumscription allocation:
  - Camera: decimal allocation plus flipper reconciliation back to national
    subject seats;
  - Senato: regional remainder allocation.
- The debug fixture exercises Camera flipper moves, so the reconciliation loop is
  covered by golden parity tests.
- Added `TODO(law-review)` for the Senato equal-remainder `sorteggio` path,
  preserving R stable ordering for parity.

Verification so far:

- `Rscript scripts/export_politics_golden.R`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 67 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 7

Completed in the internal list-in-coalition circumscription riparto pass:

- Extended `scripts/export_politics_golden.R` to export schema v6 fixtures with
  nested `internal_circ_riparto` traces.
- Regenerated `test/fixtures/politiche/debug_scrutinio.json`.
- Added TypeScript trace types for:
  - admitted circumscription lists;
  - internal subject/list quotients;
  - list-level circumscription allocation rows;
  - Camera list-level national reconciliation counters.
- Ported internal list-in-coalition circumscription allocation:
  - Camera: decimal allocation plus second flipper reconciliation back to
    national list seats;
  - Senato: regional remainder allocation inside each subject.
- The debug fixture exercises Camera list-level flipper moves, so the second
  reconciliation loop is covered by golden parity tests.
- Preserved the R behavior that decrements the recipient list's
  `SEGGI_ECCEDENTI_CONTATORE` during the Camera list-level flipper. This is
  marked `TODO(law-review)` in TypeScript because it appears counterintuitive.
- Added `TODO(law-review)` for the Senato internal equal-remainder `sorteggio`
  path, preserving R stable ordering for parity.

Verification so far:

- `Rscript scripts/export_politics_golden.R`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 67 tests.

## 2026-06-01 Checkpoint 8

Completed in the plurinominal riparto pass:

- Extended `scripts/export_politics_golden.R` to export schema v7 fixtures with
  nested `pluri_riparto` traces.
- Regenerated `test/fixtures/politiche/debug_scrutinio.json`.
- Added TypeScript trace types for:
  - admitted plurinominal list rows;
  - plurinominal college totals, quotients, and remaining seats;
  - circumscription list reconciliation counters;
  - final pre-subentro plurinominal seat rows.
- Ported Camera and Senato plurinominal seat allocation before candidate
  availability/subentro handling.
- Preserved R stable ordering for equal decimal remainders/equal figures where
  the law comments mention `sorteggio`; marked the path `TODO(law-review)`.
- The debug fixture exercises the plurinominal reconciliation flipper: across
  the 10 simulations it has 430 moved Camera rows and 162 moved Senato rows.
- Noted the architecture goal that scrutiny algorithms should remain swappable
  behind a stable interface. `scrutiny.ts` is large, but splitting is deferred
  until the golden-tested stage boundaries are stable enough to make the
  refactor safer.

Verification so far:

- `Rscript scripts/export_politics_golden.R`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 67 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 9

Completed in the final direct-scrutiny output pass:

- Added `runPoliticsScrutiny()` as the public TypeScript function that builds
  final politics scrutiny output from the already-tested trace stages.
- Ported candidate availability, candidate exhaustion/subentro passes,
  pluricandidature resolution, and final
  `liste_pluri`/`candidati_uni`/`candidati_pluri` projection.
- Added golden parity tests for final direct scrutiny output across all 10
  Camera and 10 Senato debug simulations.
- Preserved two R behaviors that look accidental but affect parity:
  - non-national same-coalition subentro calls do not pass `coal`/`livello` to
    `cerca_accettori()`;
  - after the first pluricandidature merge, `CIFRA_PERCENTUALE` remains on
    `candidati_pluri` and becomes an implicit join key for later merges, so
    late ripescati with `NA` may fail to receive `ELETTI`.
- Both behaviors are marked `TODO(law-review)` in TypeScript and noted in
  `AGENTS.md`.

Verification so far:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 87 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 10

Completed in the worker direct-scrutiny bridge pass:

- Added `scripts/export_politics_worker_snapshot.mjs`.
- Generated `web/static/data/v1/politics-debug-scrutiny.json`, a compact
  browser snapshot derived from the golden fixture with direct scrutiny inputs
  and contexts only.
- Updated `web/static/data/v1/metadata.json` to document the bridge snapshot.
- Wired the worker to load the compact snapshot and run `runPoliticsScrutiny()`
  for Camera and Senato debug simulations.
- The worker now returns completed result tables for direct scrutiny runs and
  average plurinominal list seats.
- The worker deliberately warns with `POLITICS_SCENARIO_GENERATOR_PENDING`
  because scenario-to-vote generation is not ported yet.
- Updated the Playwright smoke test to assert the real worker scrutiny path.

Verification so far:

- `node scripts/export_politics_worker_snapshot.mjs`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 87 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 11

Completed in the generated-input adapter pass:

- Added `scripts/export_politics_adapter_fixture.R`.
- Exported `test/fixtures/politiche/generated_adapter.json`, an 8.1 MB fixture
  containing the R-style generated vote/candidate tables consumed by
  `esegui_scrutini_politiche()`.
- Added generated politics source table types and `web/src/lib/politics/adapter.ts`.
- Ported the deterministic adapter boundary from generated R-style tables to:
  - `PoliticsScrutinyContext`;
  - per-simulation `PoliticsScrutinyInput`.
- Preserved the R minority-list rule used before scrutiny: for Camera,
  `REG_COD` is derived from `CIRC_COD` by removing the last two digits; for
  Senato, `REG_COD` is `CIRC_COD`.
- Added exact adapter parity tests against the existing direct R golden fixture
  for Camera and Senato.

Verification so far:

- `Rscript scripts/export_politics_adapter_fixture.R`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 92 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 12

Completed in the vote-preparation pass:

- Added `scripts/export_politics_vote_preparation_fixture.R`.
- Exported `test/fixtures/politiche/vote_preparation.json`, an 8.7 MB fixture
  for the deterministic `prepara_dts()` boundary.
- Added TypeScript source/output types for raw uninominal list votes, generated
  uninominal candidates, and prepared politics vote tables.
- Added `web/src/lib/politics/vote-preparation.ts`, porting:
  - `astensione` filtering;
  - uninominal college joins;
  - list-to-coalition joins;
  - uninominal candidate joins by simulation, college, and coalition;
  - candidate-vote aggregation;
  - filtering to plurinominal list/circumscription validity.
- Preserved R/data.table order-sensitive behavior by keeping grouped candidate
  vote totals and valid plurinominal rows in first-seen order.
- Added exact parity tests against the R-produced fixture for Camera and Senato.

Verification so far:

- `Rscript scripts/export_politics_vote_preparation_fixture.R`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 95 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 13

Completed in the generic vote-generation pass:

- Added `scripts/export_vote_generation_fixture.R`.
- Exported `test/fixtures/core/vote_generation.json`, a small synthetic trace
  for the generic `genera_voti()` logic.
- Added `createNormalSampler()` to `web/src/lib/core/rng.ts`, using the existing
  seeded uniform RNG and Box-Muller transform for reproducible browser normal
  draws.
- Added `web/src/lib/core/vote-generation.ts`, porting:
  - temporal distance calculation;
  - global list-level normal draws;
  - local delta normal draws;
  - R/data.table-style expansion and join order;
  - logit-to-probability normalization by simulation/locality;
  - R-compatible half-to-even rounding for vote counts.
- Added fixture parity tests that inject R-produced normal draw values, so the
  TypeScript generator matches R formula and row order while production browser
  runs remain seeded by TypeScript.

Verification so far:

- `Rscript scripts/export_vote_generation_fixture.R`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 98 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 14

Completed in the politics vote-generation orchestration pass:

- Added `scripts/export_politics_vote_generation_fixture.R`.
- Exported `test/fixtures/politiche/vote_generation.json`, a compact synthetic
  trace for `genera_voti_politiche()` orchestration.
- Added politics vote-generation source and fixture types.
- Added `web/src/lib/politics/vote-generation.ts`, porting:
  - municipal list-parameter joins to political base data;
  - unit-level vote generation through the generic core generator;
  - Camera aggregation by `CU20_COD`;
  - Senato aggregation by `SU20_COD`;
  - handoff to `preparePoliticsVoteTables()` for R-compatible prepared vote
    tables.
- The synthetic fixture includes three municipalities, shared Camera/Senato
  colleges, `astensione`, and a list valid in one plurinominal college but not
  another, so aggregation and filtering paths are covered.
- Added parity tests that inject R-produced normal draw values and reproducible
  TypeScript-seed tests for browser runs.

Verification so far:

- `Rscript scripts/export_politics_vote_generation_fixture.R`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 101 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 15

Completed in the politics candidate-generation pass:

- Added `scripts/export_politics_candidate_generation_fixture.R`.
- Exported `test/fixtures/politiche/candidate_generation.json`, a compact
  synthetic trace for `genera_candidati()`.
- Added candidate-generation source, output, sample-draw, and fixture types.
- Added `web/src/lib/politics/candidate-generation.ts`, porting:
  - validation for `frazioni_pluricandidature`;
  - generated uninominal candidate IDs;
  - random selection of uninominal candidates that may also appear in
    plurinominal lists;
  - coalition/list normalized percentage assignment;
  - plurinominal candidate filling with Hare-Niemeyer fraction allocation;
  - repeated plurinominal candidate reuse for pluricandidature;
  - default generated-candidate birthdate cleanup.
- The fixture includes fixed and generated uninominal candidates, fixed and
  missing plurinominal slots, minority-list rows, uninominal-to-plurinominal
  reuse, and repeated-candidate pluricandidature paths.
- Added parity tests that replay R-recorded `sample()` outputs and verify every
  sample population, sample size, replacement flag, and context.
- Captured the R default generated-candidate date from the fixture because
  `as.POSIXct("2000-01-01")` depends on the local timezone.

Verification so far:

- `Rscript scripts/export_politics_candidate_generation_fixture.R`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 104 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 16

Completed in the composed politics pipeline pass:

- Added `scripts/export_politics_pipeline_fixture.R`.
- Exported `test/fixtures/politiche/pipeline.json`, a compact synthetic trace
  that combines candidate generation, vote generation, `prepara_dts()` parity
  behavior, and direct-scrutiny input adaptation.
- Added `PoliticsPipelineSource` and related fixture/source types.
- Added `web/src/lib/politics/pipeline.ts`, which builds a
  `PoliticsDirectScrutinySnapshot` from:
  - candidate generation;
  - politics vote generation;
  - generated-table adapter context;
  - per-simulation direct scrutiny input splitting.
- Added parity tests that replay both R `sample()` draws and R normal draws,
  then compare the complete direct-scrutiny snapshot against R.
- Preserved the R edge where candidate generation can propagate an `NA`
  candidate into plurinominal slots when list assignment samples an empty
  right-join row. This is noted in `AGENTS.md` as a business-review caveat.

Verification so far:

- `Rscript scripts/export_politics_pipeline_fixture.R`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 107 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 17

Completed in the real-source pipeline smoke pass:

- Added `scripts/export_politics_pipeline_source.R`.
- Exported `test/fixtures/politiche/pipeline_source_debug.json`, a compact
  politics generation-source snapshot derived from `dati/debug_scrutinio.RData`.
- The fixture includes real debug-source lists, municipality/list parameters,
  base college data, college maps, and fixed candidate tables for Camera and
  Senato. It defaults to one simulation and can be overridden by tests or future
  worker code.
- Added `web/src/lib/politics/pipeline-source.test.ts`.
- The new test builds a `PoliticsDirectScrutinySnapshot` from the real debug
  source with seeded browser random draws, then runs `runPoliticsScrutiny()` for
  Camera and Senato.
- This was a smoke/equivalence-boundary test, not the browser performance gate.
  At this checkpoint, the worker still ran the compact direct-scrutiny snapshot
  bridge; Checkpoint 18 replaces that UI worker path with generated pipeline
  execution.

Verification:

- `Rscript scripts/export_politics_pipeline_source.R`: passed.
- `cd web; npx vitest run src/lib/politics/pipeline-source.test.ts src/lib/politics/pipeline.test.ts`: passed, 5 tests.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 109 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 18

Completed in the generated worker integration pass:

- Exported `web/static/data/v1/politics-pipeline-source-debug.json` from the
  real debug generation-source fixture.
- Updated `web/static/data/v1/metadata.json` to describe both the old direct
  scrutiny bridge and the new generated pipeline source bridge.
- Rewired `web/src/lib/workers/simulation.worker.ts` to:
  - load the generated pipeline source snapshot;
  - apply the request election date, seed, and simulation count;
  - project scenario list shares by exact list-name matches;
  - build a generated `PoliticsDirectScrutinySnapshot`;
  - run Camera/Senato scrutiny on generated simulations.
- Updated the scenario editor defaults to the real source list and coalition
  names so edits map cleanly into the worker projection.
- Updated the Playwright smoke test to assert the generated worker path.
- Kept a conservative 100-simulation worker cap pending chunked execution and
  browser performance benchmarking.

Verification:

- `Rscript scripts/export_politics_pipeline_source.R web/static/data/v1/politics-pipeline-source-debug.json`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 109 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-01 Checkpoint 19

Completed in the first browser performance-gate pass:

- Added `web/playwright.benchmark.config.ts`.
- Added `web/tests/benchmarks/politics-worker.spec.ts`.
- Added `cd web; npm run benchmark:politics`, separate from the normal
  Playwright smoke suite.
- Added a stable `data-testid="elapsed-ms"` hook to the result elapsed-time UI.
- The benchmark drives the built static app in Chromium, runs the generated
  politics worker path for 10 and 100 simulations, asserts the generated
  Camera/Senato run rows, and writes
  `test/fixtures/benchmarks/browser_politics_worker.json`.
- Re-ran fresh same-machine R politics baselines for comparison:
  - `test/fixtures/benchmarks/r_baseline_politics_10_compare.json`
  - `test/fixtures/benchmarks/r_baseline_politics_100_compare.json`
- Current gate result: pass for the debug-source generated worker path.
  Chromium ran 10 simulations in 1.248 s and 100 in 14.025 s, while fresh full
  R politics baselines were 6.20 s and 19.58 s. This is far below the 10x stop
  threshold, so there is no reason to switch to Python at this stage.

Verification:

- `Rscript scripts/benchmark_r_workflows.R --politics-sims=10 --municipal-sims=1 --regional-sims=1 --output=test/fixtures/benchmarks/r_baseline_politics_10_compare.json`: passed.
- `Rscript scripts/benchmark_r_workflows.R --politics-sims=100 --municipal-sims=1 --regional-sims=1 --output=test/fixtures/benchmarks/r_baseline_politics_100_compare.json`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 109 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
- `cd web; npm run benchmark:politics`: passed.

## 2026-06-02 Checkpoint 20

Completed in the chunked worker and 1000-simulation benchmark pass:

- Raised the generated politics worker cap from 100 to 1000 simulations.
- Added 50-simulation chunking inside `web/src/lib/workers/simulation.worker.ts`.
- The worker now builds and scrutinizes one chunk at a time, remaps local chunk
  simulations back to global simulation IDs, and keeps compact per-run summaries
  instead of retaining every full scrutiny output until the end.
- Updated `web/tests/benchmarks/politics-worker.spec.ts` to benchmark 10, 100,
  and 1000 simulations.
- Regenerated `test/fixtures/benchmarks/browser_politics_worker.json`.
- Added fresh 1000-simulation full R politics baseline at
  `test/fixtures/benchmarks/r_baseline_politics_1000_compare.json`.
- Current gate result: pass for the debug-source generated worker path.
  Chromium ran 1000 simulations in 122.608 s, while the fresh full R politics
  baseline was 151.96 s. This remains far below the 10x stop threshold.

Verification:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 109 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
- `cd web; npm run benchmark:politics`: passed.
- `Rscript scripts/benchmark_r_workflows.R --politics-sims=1000 --municipal-sims=1 --regional-sims=1 --output=test/fixtures/benchmarks/r_baseline_politics_1000_compare.json`: passed.

## 2026-06-02 Checkpoint 21

Completed in the production-shaped static snapshot bridge pass:

- Added `scripts/export_politics_static_snapshot.mjs`.
- Generated `web/static/data/v1/politics-static-debug.json` from the existing
  debug pipeline-source bridge.
- Added static snapshot and scenario snapshot TypeScript types:
  - reusable data: `base_dati`, Camera/Senato uninominal colleges, and
    Camera/Senato plurinominal colleges;
  - default scenario: election date, list model parameters, municipality/list
    model parameters, candidate-generation settings, and candidate templates.
- Added `web/src/lib/politics/static-snapshot.ts` with
  `buildPoliticsPipelineSourceFromSnapshot()`, keeping the existing internal
  compute shape stable.
- Added `web/src/lib/politics/static-snapshot.test.ts`, proving the split
  static snapshot reconstructs the previous `PoliticsPipelineSource` exactly.
- Rewired `web/src/lib/workers/simulation.worker.ts` to load
  `politics-static-debug.json` instead of the older pipeline-source bridge.
- Updated data metadata, browser smoke, and benchmark expectations to use
  `POLITICS_DEBUG_STATIC_SNAPSHOT`.
- Regenerated `test/fixtures/benchmarks/browser_politics_worker.json`; the
  1000-simulation Chromium run is 131.616 s against the fresh full R baseline
  of 151.96 s, still comfortably below the 10x stop threshold.

Verification:

- `node scripts/export_politics_static_snapshot.mjs`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 111 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
- `cd web; npm run benchmark:politics`: passed.

## 2026-06-02 Checkpoint 22

Completed in the first web-native scenario editor pass:

- Added `web/src/lib/scenario/politics.ts` as the typed politics scenario
  helper module.
- Added default scenario construction, cloning, normalization, validation,
  schema-v1 JSON serialization/parsing, and the versioned localStorage key.
- Added `web/src/lib/scenario/politics.test.ts` with coverage for valid
  defaults, JSON round-trips, malformed/partial scenarios, duplicate list
  names, and unknown coalitions.
- Reworked `web/src/routes/+page.svelte` around one `scenarioDraft` state
  object instead of separate list/coalition arrays.
- The page now supports:
  - editing scenario name and election date;
  - adding/removing/renaming coalitions;
  - adding/removing lists and editing names, coalitions, colors, and global
    shares;
  - validation before worker execution;
  - reset to defaults;
  - JSON download/upload;
  - automatic localStorage persistence.
- The localStorage writer is side-effect-only and uses `$derived` snapshots; it
  does not update state inside `$effect`.
- Added unique default names for newly created lists/coalitions so repeated add
  actions do not immediately create duplicate-name validation errors.
- This is still the basic scenario editor slice. Partial override semantics,
  past-to-future correspondences, location overrides, fixed/mean modes, and
  candidate templates remain deferred until the next scenario model/data
  packaging pass.

Verification:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 115 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
- Built-app desktop/mobile layout overflow check with Playwright: passed.

## 2026-06-02 Checkpoint 23

Completed in the scenario-to-worker projection pass:

- Extended `ScenarioList` with `shareOverride`, making partial global share
  overrides first-class in the scenario model.
- Bumped the serialized politics scenario contract to schema v2 while keeping
  old schema-v1 JSON compatible by defaulting missing override flags to
  `false`.
- Updated the UI so editing a list percentage automatically marks that list's
  share as used, and added a compact per-list checkbox to enable/disable the
  override.
- Added `web/src/lib/politics/scenario-projection.ts`, a tested projection
  boundary between the web-native scenario and the generated politics worker
  source.
- The projection now:
  - removes static-snapshot source lists not present in the scenario;
  - ignores and warns about scenario lists missing from the current snapshot;
  - applies explicit global share overrides;
  - recalculates non-overridden matched lists proportionally from source data;
  - projects matched list coalitions into the generated source;
  - creates placeholder generated uninominal candidates for newly named matched
    coalitions, with a warning.
- Rewired `web/src/lib/workers/simulation.worker.ts` to use the projection
  helper and return a `Scenario projection` result table.
- Updated the Playwright smoke test to edit `Partito Democratico` to 30%,
  verify the override checkbox activates, run the worker, and assert the worker
  output includes that projected scenario row.

Verification:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 121 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
- Built-app desktop/mobile layout overflow check with Playwright: passed.

## 2026-06-02 Checkpoint 24

Completed in the production static snapshot bridge pass:

- Added `scripts/export_politics_static_snapshot.R`.
- The exporter runs the current R politics preparation path:
  - loads `dati/dati.RData` through `carica_dati()`;
  - filters to `camera 2018`, `europee 2019`, `camera 2022`, and
    `europee 2024`;
  - computes `dati_collegi`;
  - computes `parametri_input` from `scenari/politiche_2027.xlsx`;
  - loads candidate templates with `carica_candidati()`;
  - writes a split static snapshot with reusable data and `default_scenario`.
- Exported `web/static/data/v1/politics-static.json`, about 12.5 MB.
- Updated `web/static/data/v1/metadata.json` to list the production static
  bridge snapshot.
- Extended static snapshot TypeScript types for optional scenario metadata used
  by future defaults/correspondence work: coalitions, historical list results,
  and list correspondences.
- Rewired the worker to prefer `/data/v1/politics-static.json` and fall back to
  `/data/v1/politics-static-debug.json` if the production bridge is absent.
- Updated the browser smoke and benchmark expectations from
  `POLITICS_DEBUG_STATIC_SNAPSHOT` to `POLITICS_STATIC_SNAPSHOT`.
- Updated visible politics scenario default shares to match the production
  static snapshot's political-list percentages.
- Re-ran the browser performance gate on the production static bridge:
  - 10 simulations: 1.289 s;
  - 100 simulations: 12.283 s;
  - 1000 simulations: 125.006 s.
- Gate result: pass. The 1000-simulation browser run remains below the fresh
  full R politics baseline of 151.96 s and far below the 10x stop threshold.

Verification:

- `C:\Program Files\R\R-4.5.1\bin\Rscript.exe scripts/export_politics_static_snapshot.R`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 122 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
- `cd web; npm run benchmark:politics`: passed.

## 2026-06-02 Checkpoint 25

Completed in the first results-priority UI pass:

- Updated `web/src/routes/+page.svelte` so user-facing result tables are
  separated from diagnostic tables.
- Prioritized `Average plurinominal seats by list` ahead of `Scenario
  projection`, keeping the primary election summary visible first.
- Hid `Generated pipeline runs` by default behind a `Mostra dettagli` /
  `Nascondi dettagli` toggle.
- Reset the diagnostics toggle on scenario reset, scenario load, and new worker
  runs so repeated simulations start from the user-facing result view.
- Updated the Playwright smoke test to assert that diagnostics are hidden until
  the details toggle is opened.
- Updated the browser benchmark test to open diagnostics explicitly before
  counting generated Camera/Senato run rows.
- Regenerated `test/fixtures/benchmarks/browser_politics_worker.json`; the
  production static bridge benchmark remains below the R baseline and far below
  the 10x stop threshold.

Verification:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 122 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
- Built-app desktop/mobile layout overflow check with Playwright: passed.
- `cd web; npm run benchmark:politics`: passed, 10 simulations in 1.357 s,
  100 simulations in 13.194 s, and 1000 simulations in 123.229 s.

## 2026-06-03 Checkpoint 26

Completed in the scrutiny-algorithm registry pass:

- Added optional `scrutinyAlgorithmId` to `SimulationRequest` and
  `SimulationBenchmark`.
- Added `web/src/lib/politics/scrutiny-algorithms.ts`, registering
  `politiche-r-parity-v1` as the default politics scrutiny algorithm.
- Rewired the generated politics worker to resolve the algorithm through the
  registry instead of importing `runPoliticsScrutiny()` directly.
- Recorded the selected algorithm in benchmark metadata and added a fallback
  warning for unknown requested algorithm IDs.
- Added registry tests proving the default algorithm is registered, unknown IDs
  fall back explicitly, and the default registry path calls the same
  implementation as the direct R-parity scrutiny function.
- Kept the UI selector deferred: there is still only one real politics
  algorithm, so exposing a visible choice would add noise without user value.

Verification:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 125 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-04 Checkpoint 27

Completed in the politics browser vertical-slice stabilization pass:

- Added `web/src/lib/politics/vertical-slice.test.ts`.
- The new guard loads the production `web/static/data/v1/politics-static.json`
  snapshot and verifies that the UI default politics scenario:
  - validates successfully;
  - has the same active list names as the production snapshot;
  - uses coalition names matching the production snapshot;
  - keeps displayed list shares within a small rounding tolerance of the
    snapshot's normalized political-list shares.
- The same guard projects the default scenario into the production source,
  verifies projection emits no warnings, builds one generated direct-scrutiny
  snapshot, and runs the registered `politiche-r-parity-v1` algorithm for both
  Camera and Senato.
- This stabilizes the current politics browser slice before the scenario JSON
  contract grows. Future changes to defaults, correspondences, projection,
  generation, or algorithm registration should keep this test green or update
  it deliberately with the changed contract.

Verification:

- `cd web; npx vitest run src/lib/politics/vertical-slice.test.ts`: passed, 2 tests.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 127 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-04 Checkpoint 28

Completed in the politics scenario JSON contract maturation pass:

- Bumped the politics scenario contract to schema v3.
- Added typed scenario fields for:
  - `defaultSource`, recording bundled/default provenance such as election
    kind, territory, data version, and snapshot id;
  - `globalShareMode`, currently `mean` or `fixed`;
  - `listCorrespondences`, the future scenario-owned structure for
    past-to-future list mappings.
- Kept old saved scenario JSON compatible: v1/v2 payloads without the new
  fields normalize to bundled politics defaults, `globalShareMode = "mean"`,
  and no list correspondences.
- Added validation for default-source metadata and malformed/duplicated list
  correspondences. Correspondences may target active scenario lists or the
  special `astensione` destination used by the existing R/static snapshot
  bridge.
- Implemented the first safe `globalShareMode = "fixed"` behavior in
  `projectScenarioOntoPoliticsSource()`: active political list rows keep their
  projected percentages but get `SIGMA_GLOBAL = 0`, while default `mean` mode
  preserves existing stochastic behavior.
- Added tests for v3 round-trip compatibility, malformed correspondences, old
  schema compatibility, and mean/fixed projection behavior.
- Updated the planned future steps so the next work uses the v3 contract for
  improved defaults/projection instead of redoing the contract step.

Verification:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 130 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-04 Checkpoint 29

Completed in the v3 scenario projection/defaults pass:

- Extended `projectScenarioOntoPoliticsSource()` so scenario lists now resolve
  against the static source in this order:
  - homonymous source-model list name;
  - one-to-one declared correspondence from a current source-model list to a
    future scenario list.
- Propagated correspondence-based list renames through:
  - global list model rows;
  - municipal list parameter rows;
  - Camera/Senato plurinominal candidate templates;
  - scenario projection result rows.
- Added `sourceList` and `matchMode` to projection rows, and exposed them in
  the worker `Scenario projection` result table. Users can now distinguish
  homonymous matches, declared-correspondence matches, removed source rows, and
  unmatched scenario rows.
- Made unmatched/new list warnings more explicit: ignored scenario lists now
  report that no homonymous source list or usable declared correspondence was
  found.
- Added manual stale-correspondence warnings. Bundled correspondence metadata is
  allowed to remain quiet until it is used by richer default/projection logic.
- Kept multi-source correspondence aggregation deferred. The current safe
  bridge supports one current source-model list per future scenario list; split
  factors and multi-list merges still need explicit semantics for combining
  local deltas, variability, and candidate templates.

Verification:

- `cd web; npx vitest run src/lib/politics/scenario-projection.test.ts`: passed, 7 tests.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 132 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-04 Checkpoint 30

Completed in the bundled scenario defaults pass:

- Added `scripts/export_politics_scenario_defaults.mjs`.
- Generated `web/src/lib/scenario/politics-defaults.generated.ts` from the
  production `web/static/data/v1/politics-static.json` snapshot.
- Rewired `web/src/lib/scenario/politics.ts` so the default politics scenario is
  imported from the generated snapshot-derived module rather than maintained as
  a hand-written object.
- Preserved the editor's stable coalition order (`sinistra`, `centro`,
  `destra`, `PaP`) while deriving names, shares, and bundled correspondences
  from the static snapshot.
- The generated default now carries the 95 bundled `corrispondenza_liste`
  records from the R-exported production static scenario as typed schema-v3
  `listCorrespondences`.
- Updated scenario and vertical-slice tests so the generated default
  correspondence metadata is part of the expected contract, while partial and
  legacy saved JSON still normalize missing correspondences to an empty array.
- Kept rich correspondence UI and multi-source correspondence aggregation
  deferred. Bundled correspondences are metadata for defaults/future advanced
  editing; the current projection continues to use homonymous and safe
  one-to-one declared source-model matches.

Verification:

- `node scripts/export_politics_scenario_defaults.mjs`: passed.
- `cd web; npx vitest run src/lib/scenario/politics.test.ts src/lib/politics/vertical-slice.test.ts`: passed, 10 tests.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 132 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-04 Checkpoint 31

Completed in the first advanced scenario-control pass:

- Added an `Impostazioni avanzate` expander to the politics scenario panel.
- Added a segmented `Variabilita quote` control bound to
  `scenarioDraft.globalShareMode`.
- The control exposes the existing schema/projection behavior:
  - `Media` keeps stochastic global list variation;
  - `Fissa` uses the already-tested projection path that sets active political
    list `SIGMA_GLOBAL` values to zero.
- Kept the implementation event-driven and Svelte 5 runes friendly: no state is
  updated from `$effect`, and the worker still receives a plain cloned scenario
  snapshot.
- Updated the Playwright smoke test to open the advanced section, choose
  `Fissa`, and then run the worker path.
- Updated the active plan so the next scenario task is the compact one-to-one
  correspondence view/editor.

Verification:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
- `cd web; npm run test`: passed, 132 tests.
- `cd web; npm run build`: passed.

## 2026-06-04 Checkpoint 32

Completed in the compact correspondence editor pass:

- Added `createScenarioListCorrespondence()` and
  `defaultPoliticsSourceModelListNames` to `web/src/lib/scenario/politics.ts`.
- Added a compact manual list-correspondence editor inside the scenario
  advanced section.
- The editor supports the currently safe projection semantics only: one future
  scenario list can reuse one current source-model list. It does not implement
  multi-source aggregation or split-factor behavior.
- Kept bundled correspondence metadata quiet during validation, so removing or
  renaming a list does not produce validation errors for bundled mappings that
  are not part of the user's manual edits.
- Updated the Playwright smoke path to rename `+Europa`, add a manual
  correspondence back to the `+Europa` source model list, and assert the worker
  projection reports `declared-correspondence`.
- Updated the active plan so the next scenario work is deciding richer
  correspondence semantics or moving to candidate-template support.

Verification:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npx vitest run src/lib/scenario/politics.test.ts src/lib/politics/scenario-projection.test.ts`: passed, 17 tests.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
- `cd web; npm run test`: passed, 134 tests.
- `cd web; npm run build`: passed.

## 2026-06-04 Checkpoint 33

Completed in the rich-correspondence audit and first data-contract pass:

- Audited `R/calcolo_parametri_input.R` against the intended historical-list
  correspondence semantics and `_spiegazione_metodo.qmd`.
- Confirmed the current politics scenario output is substantially coherent
  because `scenari/politiche_2027.xlsx` maps all 92 filtered historical
  `(DATA, ELEZIONE, LISTA_ORIGINALE)` keys.
- Recorded the important fragility: the R implementation uses
  `nomatch = NULL` when joining historical votes to correspondences, so an
  incomplete correspondence sheet would drop unmatched original-list votes
  instead of sending them to `astensione`.
- Decided to implement richer semantics in staged slices rather than one large
  pass. The next implementation target is a tested TypeScript parameter
  builder that explicitly maps unmapped original-list votes to `astensione`.
- Upgraded `scripts/export_politics_static_snapshot.R` to write snapshot schema
  v2 with `data.comuni_liste_elezioni`, the raw historical municipal list-vote
  rows needed by the TypeScript parameter builder.
- Regenerated `web/static/data/v1/politics-static.json` and the generated
  default scenario module.
- Added a snapshot test guard that the production static snapshot is schema v2
  and carries the raw historical vote rows.
- Added the correspondence, valid-vote display, abstention, override
  normalization, fixed-mode, and local-delta requirements to `AGENTS.md`.

Verification:

- `C:\Program Files\R\R-4.5.1\bin\Rscript.exe scripts/export_politics_static_snapshot.R`: passed.
- `node scripts/export_politics_scenario_defaults.mjs`: passed.
- `cd web; npx vitest run src/lib/politics/static-snapshot.test.ts src/lib/politics/vertical-slice.test.ts`: passed, 5 tests.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 134 tests.
- `cd web; npm run build`: passed.

## 2026-06-04 Checkpoint 34

Completed in the generated-data repo hygiene pass:

- Stopped tracking large generated JSON snapshots and fixtures with
  `git rm --cached`, while keeping Luca's local copies on disk.
- Added explicit `.gitignore` entries for:
  - `web/static/data/v1/politics-static.json`;
  - `web/static/data/v1/politics-static-debug.json`;
  - `web/static/data/v1/politics-pipeline-source-debug.json`;
  - `web/static/data/v1/politics-debug-scrutiny.json`;
  - `test/fixtures/politiche/debug_scrutinio.json`;
  - `test/fixtures/politiche/generated_adapter.json`;
  - `test/fixtures/politiche/pipeline_source_debug.json`;
  - `test/fixtures/politiche/vote_preparation.json`.
- Kept small generated code and small synthetic fixtures tracked, including
  `web/src/lib/scenario/politics-defaults.generated.ts`,
  `test/fixtures/politiche/pipeline.json`,
  `test/fixtures/politiche/candidate_generation.json`, and
  `test/fixtures/politiche/vote_generation.json`.
- Added `web/src/lib/test/generated-fixtures.ts` so tests that depend on large
  generated JSON artifacts skip clearly when the local generated files are
  absent.
- Gated snapshot-dependent Vitest suites, the Playwright smoke test, and the
  politics worker benchmark on the presence of generated static snapshot data.
- Documented the policy in `AGENTS.md`: large generated data belongs outside
  normal Git, while scripts and small generated code/fixtures remain tracked.

Verification with local generated artifacts present:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 134 tests.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
- `cd web; npm run build`: passed.

## 2026-06-04 Checkpoint 35

Completed in the first rich-correspondence implementation slice:

- Added `web/src/lib/politics/parameter-preparation.ts`.
- Ported the deterministic `calcola_parametri_input()` parameter math from raw
  historical municipal votes to TypeScript:
  - historical original-list votes are projected into future-list rows;
  - multiple original lists can aggregate into one future list;
  - one original list can split into several future lists by normalized
    correspondence factors;
  - original `astensione` and unmapped original-list votes become
    `astensione`;
  - global list/election percentages, logits, and global drift sigmas are
    computed;
  - municipal deltas and local drift sigmas are computed.
- Added `web/src/lib/politics/parameter-preparation.test.ts` with synthetic
  split/merge/unmapped-to-abstention tests.
- Added a gated production parity test that rebuilds the R-exported default
  politics `liste`, `liste_elezioni`, and `comuni_liste` from
  `data.comuni_liste_elezioni` when local ignored `politics-static.json`
  exists.
- Kept the builder deliberately unwired from the worker for now. The current
  compact correspondence UI maps to the static source model, while this builder
  expects real historical election/list correspondences.

Verification with local generated artifacts present:

- `cd web; npx vitest run src/lib/politics/parameter-preparation.test.ts`: passed, 3 tests.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 137 tests.
- `cd web; npm run build`: passed.

## 2026-06-04 Checkpoint 36

Completed in the rich-parameter projection wiring slice:

- Wired `web/src/lib/politics/parameter-preparation.ts` into
  `web/src/lib/politics/scenario-projection.ts`.
- Updated projection so schema-v2 raw historical municipal votes rebuild
  `liste` and `comuni_liste` before vote generation.
- Preserved the current compact correspondence UI by treating
  `pastElection = "politics-static source model"` as source-model template
  reuse. Historical/bundled correspondences are retargeted through those active
  source-model mappings before the parameter builder runs.
- Restricted source-model list reuse to the explicit special election label, so
  real historical correspondences are no longer accidentally interpreted as
  candidate-template reuse instructions.
- Wired the worker and production vertical-slice test to pass
  `data.comuni_liste_elezioni` and `percentualiPartenza = "europee"` into
  projection.
- Added a scenario-projection integration test proving a renamed source-model
  list receives retargeted historical parameters.

Verification with local generated artifacts present:

- `cd web; npx vitest run src/lib/politics/scenario-projection.test.ts`: passed, 8 tests.
- `cd web; npx vitest run src/lib/politics/parameter-preparation.test.ts src/lib/politics/scenario-projection.test.ts src/lib/politics/vertical-slice.test.ts`: passed, 13 tests.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 138 tests.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
- `cd web; npm run build`: passed.

## 2026-06-04 Checkpoint 37

Completed in the valid-vote share and abstention slice:

- Extended the shared `Scenario` contract with schema-v4
  `abstentionShare` and `abstentionOverride` fields.
- Updated `scripts/export_politics_scenario_defaults.mjs` so generated default
  politics scenarios carry the bundled abstention percentage over electors.
- Regenerated `web/src/lib/scenario/politics-defaults.generated.ts`.
- Updated scenario normalization and validation so old JSON stays compatible
  and invalid abstention percentages are rejected.
- Added an advanced scenario UI control for abstention as a percentage of
  electors, with its own `Usa` checkbox. Editing the value automatically marks
  it as used.
- Updated projection so list shares remain valid-vote percentages in the UI but
  are converted to internal elector fractions using the active abstention
  fraction. Overridden abstention fixes the global `astensione` parameter by
  setting `SIGMA_GLOBAL = 0`.
- Added tests for schema-v4 round-tripping, abstention validation, projection
  conversion, and the Playwright smoke path.

Verification with local generated artifacts present:

- `cd web; npx vitest run src/lib/scenario/politics.test.ts src/lib/politics/scenario-projection.test.ts src/lib/politics/vertical-slice.test.ts`: passed, 22 tests.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 140 tests.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
- `cd web; npm run build`: passed.

## 2026-06-04 Checkpoint 38

Completed in the R-style politics result presentation slice:

- Extended `web/src/lib/politics/result-presentation.ts` so each scrutiny run
  also exposes chart-ready list seat-vote points, coalition seat-vote points,
  and plurinominal-college seat-vote points.
- Added internal plot-data result tables for list, coalition, and
  plurinominal-college charts. These are hidden from the main table stack but
  remain part of the worker result for charting and export reproducibility.
- Rebuilt `web/src/lib/politics/result-charts.ts` around the R presentation
  plots in `R/presentazione_risultati.R` and
  `R/politiche/presentazione_risultati/grafico_eletti_pluri.R`: summary bars,
  valid-vote boxplots, list/coalition seat-vote scatter plots, list spinograms,
  and a selectable plurinominal-college spinogram using `NUMERO_MAX`.
- Added `web/src/lib/politics/PoliticsResultCharts.svelte` to render the chart
  model downstream of tested TypeScript helpers.
- Rearranged `web/src/routes/+page.svelte` into a single-column layout with
  scenario controls above results. Result chart sections can still arrange
  chart panels side by side on large screens.
- Kept plot-data tables out of the visible primary and diagnostic table stacks.

Verification with local generated artifacts present:

- `cd web; npm run test -- src/lib/politics/result-presentation.test.ts src/lib/politics/result-charts.test.ts`: passed, 6 tests.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 156 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-05 Checkpoint 39

Completed in the spinogram refinement slice:

- Reversed spinogram color ordering so larger seat counts receive darker
  colors and lower counts receive lighter colors.
- Added contrast-aware spinogram cell labels so dark cells render white text.
- Added valid-vote percentage break labels on the spinogram x axis.
- Added click-to-enlarge spinogram overlays, including the selectable
  plurinominal-college chart.
- Extended the Playwright smoke test to open and close an enlarged spinogram.

Verification with local generated artifacts present:

- `cd web; npm run test -- src/lib/politics/result-charts.test.ts`: passed, 3 tests.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 156 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## 2026-06-05 Checkpoint 40

Completed in the historical correspondence editor slice:

- Replaced the compact source-model correspondence editor with grouped
  historical correspondence controls in the advanced scenario panel.
- Added scenario helpers for historical correspondence grouping, split/delete,
  reset-to-default, and list rename/removal cascades through correspondences,
  local share overrides, and plurinominal candidate templates.
- Removed the old `politics-static source model` projection concept from the
  web scenario path.
- Updated projection so every active scenario list receives vote parameters
  from historical correspondences when raw history is available, and candidate
  slots are generated independently from the legal uninominal/plurinominal
  college grids.
- Updated the worker projection table and Playwright smoke path to report
  historical/static/synthetic parameter sources instead of source-model
  matches.

Verification with local generated artifacts present:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 162 tests.
- `cd web; npm run test:e2e`: passed, 4 Playwright tests.

## 2026-06-05 Checkpoint 41

Completed in the advanced candidate-generation settings slice:

- Extended the shared politics `Scenario` contract to schema v7 with
  `candidateGeneration.uninominalToPlurinominalShare` and friendly
  exact-candidacy-count shares for plurinominal pluricandidatures.
- Added scenario conversion helpers between exact candidate-count shares and the
  R-compatible `frazioni_pluricandidature` slot-layer vector.
- Updated generated politics defaults so the bundled scenario derives the
  friendly candidate-generation settings from the static snapshot.
- Wired scenario projection to override `frazione_uni_in_pluri` and
  `frazioni_pluricandidature` before worker candidate generation.
- Added advanced UI controls for uninominal-to-plurinominal reuse and the
  five-value exact-candidacy distribution.
- Tightened candidate-generation validation to require five finite,
  non-negative, non-increasing internal fractions summing to 1.

Verification with local generated artifacts present:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 166 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 4 Playwright tests.

## 2026-06-05 Checkpoint 44

Completed in the rich candidate-template editor slice:

- Upgraded the production static snapshot bridge to schema v4 by preserving
  `CIRC_DEN`, `PLURI_DEN`, `UNI_DEN`, and plurinominal `MAX_CANDIDATI` from
  the R college builder.
- Added `scripts/export_politics_candidate_slots.mjs` and
  `web/src/lib/scenario/politics-candidate-slots.generated.ts`, a compact
  named-college catalog for candidate-slot selection in the UI.
- Added scenario helpers for candidate-template create/upsert/update/remove
  behavior and coalition rename/removal cascades through uninominal templates.
- Added the advanced `Candidati` editor: slot-by-slot uninominal and
  plurinominal candidate pins, searchable named college slots, candidate
  number/minority controls for plurinominal rows, editable grouped rows, and
  duplicate-slot validation through the existing scenario contract.
- Extended the Playwright smoke path to add one Camera uninominal candidate
  and one Camera plurinominal candidate before running the worker.

Verification with local generated artifacts present:

- `C:\Program Files\R\R-4.5.1\bin\Rscript.exe scripts/export_politics_static_snapshot.R`: passed.
- `node scripts/export_politics_scenario_defaults.mjs`: passed.
- `node scripts/export_politics_candidate_slots.mjs`: passed.
- `node scripts/export_politics_municipality_catalog.mjs`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 175 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 4 Playwright tests.

## 2026-06-05 Checkpoint 43

Completed in the province/region local override slice:

- Extended local share override scopes from municipalities to municipalities,
  provinces, and regions while keeping the same
  `Scenario.localShareOverrides` JSON array shape.
- Updated scenario normalization, validation, grouping, upsert, update, and
  removal helpers so rows are keyed by `(scope, locationCode, list)` and old
  municipality-only JSON remains compatible.
- Passed the static municipality catalog into scenario projection and used it
  to resolve province/region coverage. If catalog metadata is missing or a
  location code cannot be matched, aggregate local rows warn and are ignored.
- Added aggregate projection calibration for province/region overrides:
  municipalities are weighted by expected valid votes, local abstention stays
  fixed, requested valid-vote shares are matched with multiplicative
  adjustments, and adjusted municipal `DELTA`/`DATA` rows are written only in
  the projected worker source.
- Applied scope precedence in projection: region first, then province, then
  municipality. More specific overrides win, and overlap warnings explain when
  a broader aggregate may no longer match exactly.
- Extended the advanced `Quote locali` UI with an area selector, searchable
  municipality/province/region options, and grouped override rows that show the
  covered municipality count for aggregate locations.

Verification with local generated artifacts present:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 171 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 4 Playwright tests.

## 2026-06-05 Checkpoint 42

Completed in the candidate-generation defaults refinement:

- Updated the advanced scenario UI so the "exactly 1 candidacy" share is
  disabled and recalculated from the editable exactly 2-5 candidacy shares.
- Clamped editable pluricandidature shares in the UI so normal edits keep the
  stored five-value distribution summing to 1.
- Updated `scripts/export_politics_static_snapshot.R` to use the current
  `politiche_2027.qmd` defaults: `frazione_uni_in_pluri = 0.35` and
  `frazioni_pluricandidature = c(0.8, 0.1, 0.05, 0.03, 0.02)`.
- Regenerated `web/static/data/v1/politics-static.json` locally and then
  regenerated `web/src/lib/scenario/politics-defaults.generated.ts`; the
  friendly default exact-candidacy distribution is now
  `[0.875, 0.0625, 0.025, 0.0125, 0.025]`.

Verification with local generated artifacts present:

- `C:\Program Files\R\R-4.5.1\bin\Rscript.exe scripts/export_politics_static_snapshot.R`: passed.
- `node scripts/export_politics_scenario_defaults.mjs`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 166 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 4 Playwright tests.

## 2026-06-06 Checkpoint 45

Completed in the Emilia-Romagna regional workflow migration:

- Added `scripts/export_regional_er_static_snapshot.R`,
  `scripts/export_regional_er_golden.R`, and
  `scripts/export_regional_er_scenario_defaults.mjs`.
- Exported local generated regional artifacts:
  `web/static/data/v1/regional-er-static.json` and
  `test/fixtures/regionali-er/debug_scrutiny.json`. Both follow the politics
  storage policy and remain ignored; the small generated default scenario
  `web/src/lib/scenario/regional-er-defaults.generated.ts` is tracked.
- Updated `web/static/data/v1/metadata.json` and `.gitignore` for the regional
  snapshot and golden fixture.
- Split the app routes: `/` is now the election selector, the existing politics
  editor moved to `/politics`, and `/emilia-romagna` hosts the regional editor.
- Added regional scenario helpers and UI for metadata, coalitions, lists,
  abstention, historical correspondences, local overrides, validation,
  import/export, charts, and result tables. Candidate controls are intentionally
  absent because Emilia-Romagna regional multiple candidacy is forbidden in this
  workflow slice.
- Added `web/src/lib/regional-er/` modules for static snapshot loading,
  scenario projection, shared vote generation, pipeline adaptation, scrutiny,
  result presentation, result charts, and chart rendering.
- Ported `scrutinio_regionali_ER` to TypeScript with law comments near the
  translated logic for provincial population seats, 3%/5% thresholds,
  provincial quotient allocation, regional residual recovery, majority
  bonus/guarantee, runner-up reservation, and province-level placement with
  candidate caps.
- Preserved the current R regional starting-point behavior: the default path
  uses `scenari/BO_insieme.xlsx` and does not force
  `percentuali_partenza = "europee"`, despite the older `.qmd` prose.
- Extended the simulation worker with separate politics and regional dispatch.
  The regional path requires `/data/v1/regional-er-static.json`, runs in
  50-simulation chunks, and is capped at 1000 simulations.
- Added golden parity tests, a regional production vertical slice test, a
  regional e2e smoke test, a landing-page selector e2e, and a regional browser
  benchmark.

Benchmark snapshot with local generated artifacts present:

| Workflow | Simulations | Elapsed | Source |
| --- | ---: | ---: | --- |
| R Emilia-Romagna regional workflow | 1000 | 7.78 s | `test/fixtures/benchmarks/r_baseline_regional_er_1000_compare.json` |
| Chromium regional worker, R-exported static bridge | 10 | 0.211 s | `test/fixtures/benchmarks/browser_regional_er_worker.json` |
| Chromium regional worker, R-exported static bridge | 100 | 0.557 s | `test/fixtures/benchmarks/browser_regional_er_worker.json` |
| Chromium regional worker, R-exported static bridge | 1000 | 6.405 s | `test/fixtures/benchmarks/browser_regional_er_worker.json` |

Verification with local generated artifacts present:

- `C:\Program Files\R\R-4.5.1\bin\Rscript.exe scripts/export_regional_er_static_snapshot.R`: passed.
- `C:\Program Files\R\R-4.5.1\bin\Rscript.exe scripts/export_regional_er_golden.R`: passed.
- `node scripts/export_regional_er_scenario_defaults.mjs`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 187 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 6 Playwright tests.
- `cd web; npm run benchmark:regional-er`: passed.
- `C:\Program Files\R\R-4.5.1\bin\Rscript.exe scripts/benchmark_r_workflows.R --politics-sims=1 --municipal-sims=1 --regional-sims=1000 --output=test/fixtures/benchmarks/r_baseline_regional_er_1000_compare.json`: passed.

## 2026-10-16 Politics reform implementation documentation

Completed documentation only, following Luca's approved implementation proposal
and three additional requests for modelling discussions:

- Added [`politics-reform-2026-plan.md`](politics-reform-2026-plan.md) with
  21 pending steps, dependencies, legal function boundaries, test cases, and
  acceptance criteria. The steps retain the original 18-stage scope and add
  three explicit modelling decisions before dependent production generation.
- Added [`politics-reform-2026-modelling.md`](politics-reform-2026-modelling.md)
  to isolate the decisions on bonus-only ballots, preferences, and Camera-Senato
  differences. No statistical model was selected or implemented.
- Preserved Luca's proposal to interpret generated list votes as final totals
  including redistributed bonus-only votes. Required checks distinguish exact
  representation equivalence, integer/geographic attribution effects, and
  sensitivity to unobserved behavior; they cannot establish empirical absence
  of bias without calibration data. Double redistribution is explicitly excluded.
- Documented missing-preference alternatives and their effects on candidate
  identities, multiple elections, exhaustion, and final filled seats. Documented
  chamber-dependence alternatives and their effect on the joint bonus condition.
- Linked the focused plan and decisions from `MIGRATION_PLAN.md` and the reform
  transcription. Retained stable guidance in `AGENTS.md` and kept narrative here.
- Kept all reform steps unchecked. Normative concordance, modelling decisions,
  legal scrutiny, generators, worker, and UI remain future work. The quoted legal
  transcription was not changed.

Verification:

- `git diff --check`: passed; Git emitted only LF-to-CRLF conversion notices.
- A read-only Python check validated 37 local Markdown file/section links in
  the active migration plan and three reform documents, all 21 ordered step
  headings and matching unchecked entries, and exact preservation of the legal
  transcription from its original introductory paragraph onward against `HEAD`.
- Changes are limited to Markdown documentation. Application tests, builds,
  benchmarks, and normative source reconciliation were not run for this task.

## 2026-10-16 R26-01: normative specification and provisional decisions

Completed the specification and updated it to Luca's requested scope:

- The [transcription](politics-reform-2026.md) is the current consolidated
  operative source for all and only statutory provisions translated within the
  domestic initial-scrutiny scope. Promulgated law and official corrections
  control conflicts with other official sources. Provisional interpretations
  remain separate in the [specification](politics-reform-2026-specification.md).
- Included constitutional elective totals, foreign-seat endowments, population
  apportionment and electorate/candidate ages; majority age; single-chamber
  candidatures; applicable territorial/list coverage; ordinary and special
  vote attribution, allocations, proclamations and initial recovery. The
  promulgated Bolzano U04-U06 mapping contains 116 municipalities.
- Kept filing/signature/symbol procedures, individual ballot adjudication,
  foreign scrutiny and later supplementary elections outside the transcription's
  operational scope. Large unchanged geography/census tables remain sourced
  input catalogs rather than duplicated algorithmic text.
- The specification contains 29 norm-to-function/test boundaries and nine
  interpretation/data policies. Test IDs are obligations for later coding,
  not application tests executed during this documentation task.
- Legal-source review uses Law 9 October 2026, no. 180 (26G00201, GU 235),
  the A.C. 2822-B dossier and parliamentary erratum, and the Presidency of the
  Republic's constitutional text. The reform enters into force on 24 October.
  The dated source register records the local/official comparison PDF hashes.

Luca's provisional decisions, recorded on 16 October 2026 and revisable when
further legal evidence arises:

- I03: after all express legal criteria, resolve an otherwise unspecified tie
  with an injected seeded draw and report a warning in every affected simulation.
  Preserve tied alternatives, draw context and outcome; do not override express
  preference-list order or age criteria.
- I05: apply symmetry, truncating both majority and minority Camera cap indices
  at six decimals before normalization.
- I06: resolve equal Senate cap-removal ranks by warned seeded draw; trace
  used/unused state and never reuse a remainder for another award/removal.
- I07: add pending R26-01a before R26-02 to research any relevant past Senate
  recovery case, establish its primary-source outcome and legal applicability,
  and record the supported provisional recovery reading and tests. No such
  precedent is asserted or researched as completed in this documentation update.
- I08: until the election decree exists, generate provisional seat tables from
  the latest legally declared population census, applying constitutional/special
  constraints and the documented interim population quota/remainder method.
  The identified baseline is DPR 20 January 2023's legal population at
  31 December 2021; recheck the applicable latest source at R26-07 generation.
  Label/version census-based tables and replace them with verified decree
  tables when available, preserving replay of prior runs.

Plan changes:

- Only R26-01 is complete. R26-01a is the next step and R26-02 depends on it;
  no modelling decision, legal implementation, generator, worker or UI step
  was implemented here. Relevant downstream directives now carry the provisional
  policies, warnings and seat-table provenance.
- After completion of all web simulator/data-preparation migration work, plan
  foreign-seat simulation, historical Senate-result modelling if not already
  delivered through R26-04 (accounting for the voter-age change), and
  away-from-home voter distribution. R26-04 first audits available source data;
  statistical choices remain pending.

Verification of the updated documents:

- Read-only checks passed for 56 local file/section links across the root plan
  and four reform documents; 22 ordered reform checklist entries with only
  R26-01 complete; R26-01a before and required by R26-02; all N01-N29 and I01-I09
  rows; and the provisional policy, warning and scope references.
- Checked 23 retained scrutiny article bodies for content consistency after
  source/typography normalization. Compared constitutional articles 56-58 and
  the added C18-bis(2-ter), S9(2-bis), S11(3-bis) excerpts directly with downloaded
  official text, normalizing publication typography only.
- Confirmed the Bolzano table reproduces the promulgated annex's 116 distinct
  municipality entries: U04 has 2, U05 has 59 and U06 has 55. The table preserves
  the source's bilingual names; geography joins still belong to R26-07.
- Exact rational checks passed for bonus-only attribution, integer versus
  non-truncated quotas, six-decimal truncation and the 314/154/220/113 seat
  identities. These are documentation checks, not implemented reform tests.
- `git diff --check`: passed. Application tests/build/browser checks were not
  run because all changes are Markdown; application behavior and generated
  data remain unchanged. Temporary source/review scripts stayed outside the
  repository. The Senate precedent search and census-table generation remain
  pending in their own steps.

## 2026-10-16 R26-01a: Senate recovery precedents and approved I07

Completed the requested primary-source research and, after Luca approved the
proposal, recorded the provisional [I07 protocol and worked fixture expectations](politics-reform-2026-specification.md#senate-initial-recovery-i07).
This completes the research decision step, not recovery implementation. It
supersedes the pending-I07 status in the preceding R26-01 entry. R26-02 is the
next discussion; all subsequent implementation steps remain open.

### Initial exhaustion: Sicily to Umbria, Pavanelli (2019)

The 2018 Senate election left a proportional M5S seat in Sicilia-02 unfilled
because the list's candidates were exhausted. Candidates already elected in
uninominal seats, including Nunzia Catalfo, could not fill that additional seat;
there were no remaining eligible regional candidates and M5S had no coalition
partner to receive it.

The Giunta delle elezioni e delle immunità parlamentari adopted its proposed
solution on 26 June 2019. The Assembly approved **Doc. XVI no. 2 on 31 July 2019**;
the Giunta identified Emma Pavanelli in Umbria and the President proclaimed her
that day. This was an initial-allocation deficiency resolved later during
verification of powers, rather than a seat vacated during the term.

Primary evidence:

- [Doc. XVI no. 2, majority report](https://www.senato.it/service/PDF/PDFServer/DF/346068.pdf),
  particularly pp. 7-9, and the [relator's explanation on 25 July 2019](https://www.senato.it/show-doc?id=1119720&idoggetto=0&leg=18&part=doc_dc-ressten_rs-gentit_ddddxn2rgeeissnars&tipodoc=Resaula):
  regional same-list recovery, then the then-existing unsuccessful-uninominal
  recovery, then regional coalition recovery; only after those failed, residual
  S19(2)/C86/C84(4) recovery for the same list in another region, selected by the
  greatest unused quotient remainder.
- [Official procedure and outcome](https://www.senato.it/legislature/18/leggi-e-documenti/attivita-non-legislative/procedure?documentoId=42318),
  [Assembly proceedings of 31 July 2019](https://www.senato.it/show-doc?id=1120170&leg=18&part=doc_dc&tipodoc=Resaula)
  and [Pavanelli's official profile](https://www.senato.it/leg/18/BGT/Schede/Attsen/00034677.htm)
  establish the actual transfer and proclamation in Umbria. Her later
  convalidation date is distinct from the 2019 proclamation.

This supplies a relevant parliamentary precedent for an initial unfilled seat
crossing a regional boundary as a residual remedy. The outcome was contested;
it is not a judicial determination that every such transfer is constitutional.

### Later vacancy: Veneto to Calabria, Minasi (2021)

Lega senator Paolo Saviane died on 20 August 2021. With Lega candidates exhausted
in Veneto, the Giunta on **11 November 2021** rejected the proposal to replace
him with the regional coalition partner's Bartolomeo Amidei (FdI), and adopted
the same-list cross-region solution of Clotilde Minasi (Lega) in Calabria.
The Assembly approved **Doc. XVI no. 7 on 2 December 2021**, and Minasi was
proclaimed that day.

Primary evidence:

- [Giunta decision of 11 November 2021](https://www.senato.it/show-doc?id=1317860&idoggetto=0&leg=18&part=doc_dc-sedetit_vdp&tipodoc=SommComm)
  records the rejected regional-coalition proposal and the adopted alternative.
- [Doc. XVI no. 7, majority report](https://www.senato.it/service/PDF/PDFServer/DF/365843.pdf),
  [official procedure and final outcome](https://www.senato.it/legislature/18/leggi-e-documenti/attivita-non-legislative/procedure?documentoId=46913)
  and [Assembly proceedings of 2 December 2021](https://www.senato.it/show-doc?id=1321518&leg=18&part=doc_dc-ressten_rs-gentit_ddddxn7rgeeipssvirv&tipodoc=Resaula)
  establish the legal argument, approval and proclamation.

This confirms that a same-list cross-region replacement actually occurred, but
in a **later vacancy**. Its priority over an available regional coalition
candidate must not replace the regional-first order for initial allocation.
Later vacancies remain outside the current simulation's scope.

### Constitutional proceedings and applicability limits

The Constitutional Court's [order 86/2020](https://www.cortecostituzionale.it/scheda-pronuncia/2020/86),
decided on 7 April and deposited on 7 May 2020, declared Gregorio De Falco's
conflict of powers concerning the Pavanelli procedure **inadmissible**. The
Court found no demonstrated substantial impairment of his individual
parliamentary powers; he could not assert the electoral offices' powers or a
generic interest in legality. It did not decide the regional-transfer issue
on its merits. The bounded research did not identify a merits judgment settling
the Minasi issue; that is a search limit, not a claim that none exists.

The historical decisions applied the pre-reform system. Their old C84(3)
unsuccessful-uninominal remedy is abolished by the reform and is excluded from
the approved protocol. Neither decision addresses the new bonus slates or
directly validates the S16-bis(1-quinquies) mechanism.

Some Senate pages/PDFs returned access challenges or HTTP 403 during direct
retrieval. Official indexed text supplied the relevant report/proceeding
passages; official procedure/profile records corroborated the outcomes. The
Court's decision text was accessible. These access limits do not amount to a
complete visual review of every linked Senate PDF.

### Approved decision and documentation changes

I07 is a provisional simulator interpretation, revisable on further legal
evidence, separate from the consolidated statutory transcription:

- Initial ordinary recovery, including releases after multiple elections,
  exhausts same-region list and coalition remedies under C84(2),(5), then uses
  S19(2)/C86(2)/C84(4) for the original list in another region. It does not
  activate C84(7)'s cross-region coalition remedy.
- Bonus shortages follow the separate express
  S16-bis(1-quinquies)/C83(1-quinquies)/C86(1) route: local recovery first, then
  cross-region recovery for the bonus-winning list or coalition, unused before
  used remainders. A coalition bonus is not split into arbitrary list shares.
  This is a provisional reading of the new cross-reference, rather than a
  result proved by the historical cases.
- Cross-region recovery requires an I07 warning and a reproducible trace,
  preserving original entitlement and actual election territories. Ordinary
  allocation is not recalculated to hide a movement; exhausted permitted
  routes leave an explicitly unfilled seat.

Updated the specification's N27 boundary, I07 register entry and six constructed
fixture expectations; carried the policy into R26-17 and result/export
requirements in R26-20. Marked only R26-01a newly complete and updated the root
plan's next-step directive. The legal transcription, modelling choices,
application code and generated inputs are unchanged.

Verification:

- Checked 66 local file/section links across the root plan, four reform
  documents and this history entry; all resolved. Confirmed 22 ordered reform
  checklist entries with only R26-01 and R26-01a complete, and R26-02 still
  dependent on R26-01a.
- Confirmed N01-N29 and I01-I09 remain present, all six I07 fixture boundaries
  are recorded, and active directives no longer describe I07 as pending.
  Only four Markdown files changed; the statutory transcription is unchanged.
- `git diff --check` passed. No application tests/build were run for this
  documentation-only change; the worked expectations are future test
  obligations, not implemented or executed tests.

## 2026-10-17 R26-02: approved final-attributed vote model

Luca approved generation of **final attributed list totals**, retaining explicit
direct-list and bonus-only/candidate-only components for supplied scenarios and
legal tests. Recorded the active [R26-02 contract and implementation protocol](politics-reform-2026-modelling.md#bonus-only-votes-r26-02)
and marked the decision step complete. R26-03 is next; preferences and chamber
dependence remain undecided. No generator, input conversion or legal scrutiny
function was implemented in this documentation update.

### Decision, alternatives and limits

Scenario shares and local overrides describe final list support, including
lawfully attributed bonus-only votes. Their ballot composition remains unknown;
no default random bonus-only count or implicit zero count is introduced. Explicit
components undergo attribution once before admission. The same convention
applies separately to special candidate-only votes at uninominal level; generated
special candidate totals equal the sum of their linked lists' final totals.
The model ID is `final-attributed-list-totals-v1`; future results/exports must
retain representation and source/conversion provenance.

Alternatives assessed:

| Alternative | Decision and rationale |
| --- | --- |
| Generate final attributed totals | Selected default: understandable forecast-share semantics without an unsupported intermediate ballot-composition model; fits the existing generator's list-total boundary. |
| Generate direct votes and random bonus-only counts | Not selected: frequency, geography, coalition effects and chamber correlation lack calibration. Adding assumptions can change the forecast distribution without evidence of improved accuracy. |
| Accept supplied explicit components | Retained for controlled scenarios and independently testable legal attribution; this does not require a calibrated random composition model or a new UI control in this step. |
| Use historical candidate-only behaviour as a bonus-only proxy | Not selected as default calibration: different ballot/election context. Historical evidence may inform explicitly labelled sensitivity hypotheses. |

The algebraic identity before integer rounding is
`T[i] = D[i] * (1 + B / sum(D))` for positive direct total. It preserves
within-coalition proportions at the attribution college. Thus identical final
totals support identical downstream scrutiny, conditional on identical other
legal inputs and downstream draws. This is representation equivalence, not
empirical evidence that the forecast is unbiased. Unknown correlations between
ballot behaviour and territorial/list strength remain uncalibrated.

Use the legal [N04/N10 boundaries](politics-reform-2026-specification.md#inputs-votes-and-admission):
ordinary attribution at plurinominal college, special attribution at uninominal
college, with integer additions/remainders and I03/I04 tie/zero-support policies.
Do not infer preferences from the unknown composition; R26-03 decides that model.
Revisit R26-02 if calibrated composition data become available, composition is
a requested output, or implementation checks reveal a material limitation.

### Reproducible arithmetic and source-data evidence

Read-only Python exact-integer/Fraction checks, repeated when recording approval:

- Enumerated list counts 2, 3 and 4; every direct-vote vector with entries in
  `0..6`, excluding the all-zero vector; and every bonus-only count in `0..16`.
  For each, assigned `floor(B*D[i]/sum(D))`, then one addition per remaining
  highest remainder `B*D[i] mod sum(D)`. Equal remainders were ordered by list
  index only for this arithmetic enumeration, not as a proposed legal tie rule.
- All **47,430** cases conserved `sum(D)+B`. The largest absolute list deviation
  from the ideal proportional total was **3/4 vote**; every deviation was less
  than one vote. This bound concerns fixed explicit `D/B` at one attribution
  college, not decomposition of an arbitrary target total or a bound on seats.
- Worked example: direct `(60,30,10)` plus seven bonus-only ballots gives
  `(64,32,11)`. Geography counterexample: two units each with direct `(1,2)`
  and one only ballot give `(2,6)` if attributed separately and then summed,
  versus `(3,5)` when the combined `(2,4)` and two only ballots are attributed
  once. Hence municipal/national substitution cannot be presumed equivalent.

Inspected the existing [vote generator](../web/src/lib/politics/vote-generation.ts)
and [historical loader](../R/caricamento_dati.R), which selects the 2022
`VOTILISTA` field. Audited the bundled
[2022 Camera CSV](../dati/camera-20220925/Camera_Italia_LivComune.csv), SHA-256
`9f4f49dd0dfde3d1ea32ccda9665d6d230959f97ce074418ef2553242aa675b3`:

- Read as UTF-8 with BOM handling and semicolon delimiter. Grouped by
  `CIRC-REG`, `COLLPLURI`, `COLLUNINOM`, `COMUNE`, `COGNOME`, `NOME`,
  `DATANASCITA`; summed list votes and counted the repeated candidate total once.
- All **70,539** groups had a consistent candidate total, never below the list
  sum; **42,740** had a positive difference. The combined excess was
  **1,027,135 votes**. This is historical candidate-only evidence, not an
  estimate of future bonus-only participation.
- Reform calibration must reconcile each source's vote categories using that
  election's attribution rules at their legal geographic level. Document any
  finer-resolution reconstruction assumption. Do not merely relabel the raw
  list field or alter the preserved legacy parity path. This conversion remains
  an R26-19 implementation obligation.

Updated R26-05 representation/provenance requirements, R26-08 equivalence checks,
R26-19 historical calibration requirements and R26-20 export semantics. The
specification now points to the approved statistical contract; the legal
transcription remains unchanged. Full seat/bonus equivalence is still a future
implementation check with exact zero-discrepancy acceptance for matched inputs
and isolated downstream draws. Any stochastic sensitivity experiment must state
hypotheses, margins and seed/replication budget before examining results.

Verification of this documentation update:

- All 73 local file/section links across the root plan, four reform documents
  and this history entry resolved. Confirmed 22 ordered steps with only
  R26-01, R26-01a and R26-02 complete, and all N01-N29/I01-I09 entries retained.
- R26-03/04 decision sections and the statutory transcription are unchanged.
  Exactly five Markdown files changed; no application/generated data changed.
- Repeated the arithmetic enumeration and CSV audit above; counts and source
  hash matched. These are decision-evidence checks, not reform application tests.
- `git diff --check` passed. Application tests/build/browser checks were not
  run for this documentation-only update; generator integration, historical
  conversion and full downstream scrutiny equivalence remain unimplemented.

## 2026-10-17 R26-03: approved historical-pattern preference model

Luca approved fitting a **synthetic preference model informed by historical
patterns**, with supplied valid counts taking precedence. Recorded the active
[model, calibration and output contract](politics-reform-2026-modelling.md#preferences-r26-03)
and marked the decision step complete. R26-04 remains the next discussion.
This approval selects the model family and validation obligations; no parameters
were fitted, generator implemented or source data changed.

### Decision and alternatives

The approved generator separates effective preference intensity, candidate
concentration and territorial variation. It uses partial pooling when evidence
is sparse, shared underlying strength across a person's candidatures, and local
variation. New candidates without individual evidence receive exchangeable
synthetic strengths informed by the historical distribution. Historical names
and their measured support are not automatically mapped onto new people.

Generate counts of valid combinations of zero to three distinct eligible
preferences, excluding capolista and applying gender constraints. The resulting
candidate counts have a constructive realizability trace. The central profile
uses maximum entropy over valid combinations subject to selected intensity and
candidate-strength constraints; this is an explicit modelling convention.
Aggregate source counts do not identify individual ballot combinations or the
zero/one/two/three-preference mix. Alternative multiplicity/pairing assumptions,
especially the regional source's unobserved third preference, require sensitivity
checks. Preference-list ties still use statutory presentation order.

The convention preserves R26-02: attributed totals support effective preference
intensity without reconstruction of bonus-only counts. The zero-preference
category is unspecified composition. When direct/bonus-only components are
explicit, only direct list votes supply the preference budget. Supplied valid
preference counts take precedence; missing counts are never silently zero.

| Alternative | Decision and rationale |
| --- | --- |
| Historically informed synthetic preferences | Selected: supports complete candidate-dependent scrutiny while learning concentration/territorial patterns and exposing transfer assumptions. |
| Supplied counts only | Retained as the preferred input when available; insufficient by itself for automated future-election generation. |
| Defer preferences and restrict outputs | Honest fallback for incomplete inputs, but not the chosen primary model; candidate resolution/recovery cannot be claimed complete. |
| Uniform preferences or independent random rankings | Useful diagnostic baselines; discard observed concentration and shared candidate strength. |
| Copy historical candidate profiles directly | Not selected: list lengths, candidate identities, campaigns, protected capolista and electoral systems differ. |

### Preference source audit and evidence

The assessment located preference data already downloaded locally. Repeated the
read-only cache checks when recording approval, using `readRDS()` with R 4.5.1;
the `.RData` suffix does not make these workspace images:

| Source | Audited coverage/content | SHA-256 |
| --- | --- | --- |
| [Raw ER 2024 preferences](<../dati/raw/regionali 2024Regionali_EmiliaRomagna_2024_Preferenze.csv.RData>) | 325,133 candidate-section rows; 4,529 distinct municipality/section keys, 330 municipalities, nine constituencies, 11 list labels and 547 candidate slots in 97 list-constituency pairs. No duplicate candidate-section keys were found in the assessment. | `0518457791af919962d448a3e0604cb49679e22421266db0083e5749ee739631` |
| [Matching raw scrutiny](<../dati/raw/regionali 2024Regionali_EmiliaRomagna_2024_Scrutini.csv.RData>) | 49,819 rows; summed list votes 1,494,375. Two blank-list zero-vote placeholders have no matching preference slate; do not interpret them as missing positive support. | `a4fbc2f7c1e3309a287e0ef7d567911670d45aef5d5f40b2c959dd2a02ec01d6` |
| [Prepared bundle](../dati/dati.RData), `preferenze` | 22,050 municipal candidate-preference rows, all ER regional 2024; lower resolution than the raw section cache. | `1d5b7fbde447bdacf54574ac638e8a004dcc637cf0c4d46ed6e332f5f2526fdf` |

Reproduce the concentration diagnostic by aggregating raw `PREFERENZE` over
`CIRCOSCRIZIONE, LISTA, NUM_CANDIDATO`, then dividing each list-constituency's
maximum candidate count by its preference total. Across the 97 pairs, the
unweighted minimum/median/maximum leading share is **18.3% / 38.6% / 81.3%**.
Lists have three to eleven candidates; these summaries are diagnostics, not
national parameters. The 547 entries identify candidatures, not 547 distinct
people or independent statistical observations.

The cache sums to **644,626 preference marks**, or **0.4313683 marks per list
vote**. That is not a measured fraction of voters expressing preferences: the
source allows two. The [regional metadata and voting rules](https://statistica.regione.emilia-romagna.it/dati/rappresentazioni-grafiche-dati/visualizzazione-elezioni-regionali-2024-emilia-romagna-risultati/visualizzazione-elezioni-regionali-2024-emilia-romagna-risultati-metadati)
confirm the section/municipality scope and double preference with different
genders. The raw candidate fields include list position and names, but no gender;
join verified rosters before gender-based calibration, rather than inferring it.

The later [regional publication, table 4.1, PDF page 74](https://statistica.regione.emilia-romagna.it/studi-analisi/2025/elezioni-regionali-2024-emilia-romagna-pubblicazioni/documenti-allegati/pubblicazione_elet_2024_web.pdf/@@download/file)
reports **644,089** marks, 537 fewer than the cache. Its reading notes distinguish
official electoral-office records from ministerial election-night data, and
table 4.1 specifically identifies its preference totals as ministerial. This
establishes a version/reconciliation requirement, not the cause of every
difference or a completed correction. Preserve source status and provenance.

Additional sources checked during the assessment, not added to the repository:

- [Bologna municipal 2021, PD candidate preferences](https://opendata.comune.bologna.it/explore/dataset/elezioni-amministrative-2021-preferenze-lista-partito-democratico-lepore-sindaco/):
  the public API returned 445 section records with candidate columns. Other
  municipal lists were located in the same catalogue.
- [Bologna European 2024 candidate preferences](https://opendata.comune.bologna.it/explore/dataset/elezioni-europee-2024-preferenze-ai-candidati-suddivisi-per-sezione/):
  the public API returned 73,870 records with section, list, candidate number/name
  and valid preference votes. European data can inform three-preference
  sensitivity, but do not resolve joint-ballot identification from marginals.
- [Bari European 2024, official Puglia catalogue](https://dati.puglia.it/ckan/dataset/elezioni-europee-2024):
  verified catalogue metadata/resources for candidate preferences and list votes
  by section. File contents were not validated in this assessment.

Some browser fetches failed while direct public-API requests succeeded. The
Ministry's European ZIP URL returned content that was not a ZIP; no national
preference extract was claimed or imported from it. Geographic expansion,
cleaning, source reconciliation and calibration remain R26-19 work. A complete
regional cache and large section row count do not establish national coverage
or an equally large independent sample.

### Downstream requirements and supported claims

Use model ID `preferences-empirical-synthetic-v1` with separately versioned fitted
parameters/profiles, sources, seeds and supplied/generated provenance. Before
production integration, reconcile totals/rosters, adjust list length and the
capolista exclusion, validate held-out provinces and later elections/regions,
and run paired sensitivity with votes/candidatures fixed. Account for repeated
candidates and clustered sections. Declare diagnostic/acceptance criteria and
seed budgets before inspecting stochastic results.

Propagate alternative profiles through multiple elections, bonus precedence,
candidate exhaustion and recovery. Separate vote-based seat entitlements from
final filled seats and candidate/recovery outcomes conditional on preferences.
Synthetic candidate probabilities are conditional scenario outputs; profile
sensitivity is not a calibrated confidence interval or a forecast for real names.
New data, actual reform results, candidate evidence or material validation
failures can justify revisiting the model.

Updated R26-05 contracts, R26-16 fixture independence, R26-19 fitting/generation,
R26-20 reporting and the specification's scope references. The selected model
does not settle Camera-Senato vote dependence, change legal ranking rules or
simulate invalid-ballot adjudication. R26-04 remains pending; all application
implementation and acceptance steps remain open.

Verification of this documentation update:

- All 76 local file/section links across the root plan, four reform documents
  and this entry resolved. Confirmed 22 ordered steps with only R26-01, R26-01a,
  R26-02 and R26-03 complete, and all N01-N29/I01-I09 entries retained.
- The R26-02 contract, pending R26-04 discussion section and statutory
  transcription are unchanged. Exactly five Markdown files changed.
- Read-only R checks reproduced the cache coverage, preference/list-vote sums
  and leading-share diagnostics. All three recorded source hashes remained
  unchanged. No fitting, stochastic model experiment or application test ran.
- `git diff --check` passed. Application build/browser checks were not run for
  this documentation-only update; calibration and implementation acceptance
  remain assigned to their later steps.

## 2026-10-17 R26-04: approved paired chamber vote model

Luca approved **shared political shocks plus historical chamber contrasts and
correlated residuals**, refined so a supplied common national/local share anchors
model-derived chamber references and explicit per-chamber shares take precedence.
Recorded the [model, input precedence and validation contract](politics-reform-2026-modelling.md#camera-senato-votes-r26-04)
and marked the decision step complete. R26-01/01a/02/03/04 are now complete as
documentary/decision steps; R26-05 contracts are next. No parameters were fitted,
generator implemented or production data imported.

### Decision and reference precedence

The model combines shared national/territorial movement with regularized
historical contrasts and chamber-specific national, regional and local variation.
Its joint covariance must preserve the intended marginal uncertainty; adding
unbudgeted Senate noise to an existing Camera draw is not the selected method.
Numerical residual scales and dependence parameters require fitting or explicitly
labelled assumptions, with weaker/stronger dependence and gap sensitivity.

A supplied common share is a shared starting anchor from which the model derives
distinct Camera and Senato references. An explicit reference replaces the
modelled reference adjustment for that chamber at that scope. When only one
chamber is explicitly supplied, the other still derives from the common anchor,
or from its historical/model baseline if no common input exists. Both explicit
references override both modelled adjustments. Correlated election-day
uncertainty remains active in mean mode; explicit reference values are not
fixed simulation results.

Across geographic scopes retain the established broader-to-more-specific
resolution. Within the same scope, per-chamber input wins over common-derived
references, which win over model defaults. This preserves local specificity,
partial-share rules, abstention handling, valid-vote/elector conversion and
normalization/warnings. Trace the common anchor, supplied/derived references,
scope and reconciliation rather than interpreting a shared input as Camera-only.

| Alternative | Decision and rationale |
| --- | --- |
| Identical territorial votes | Retain as a benchmark, not the production model: omits chamber behaviour and can couple joint-bonus events too tightly. |
| Independent chamber generation | Not selected: discards the common political environment. |
| Separate historical baselines with identical shocks | Not selected as the full model: treats the chamber gap's uncertainty as absent. |
| Shared shocks, historical contrasts, correlated residuals | Selected: represents common movement and chamber differences, with explicit transfer/dependence uncertainty. |
| Supplied chamber-specific inputs | Retained with precedence over model-derived references at the same scope. |

### Official source audit and exploratory comparison

The decision review found no Senate election history in the existing
`dati/dati.RData` bundle. Its historical elections were Camera 2018/2022,
European 2019/2024 and regional 2020/2024. The existing
[vote generator](../web/src/lib/politics/vote-generation.ts) draws one territorial
list table and aggregates it into each chamber's geography. Downstream list
coverage/filtering can still produce differences; this is not a claim that all
final legacy chamber figures are identical.

Official archives were downloaded on 17 October 2026 to the operating-system
temporary directory `elezioni-r26-04`, outside the repository. The Ministry's
[2022 catalogue entry](https://www.dati.gov.it/node/view-dataset/dataset?id=caca3eec-122a-4668-a75b-3a2426ce3ae6)
identifies municipal Camera and Senato result files. The historical download
endpoint redirected successfully to the DAIT archive; direct ZIP retrieval
worked even where browser page retrieval returned access errors.

| Archive | Bytes | SHA-256 |
| --- | --- | --- |
| [Camera 2022](https://dait.interno.gov.it/documenti/opendata/camera/camera-20220925.zip) | 1,216,091 | `dd8ede158ea3f7b0aae65024135a27e34ab9e959728df95f0e977cd56edf88ff` |
| [Senato 2022](https://dait.interno.gov.it/documenti/opendata/senato/senato-20220925.zip) | 1,181,512 | `ffaf0296cbb3702d1e0d13a9b7f88b6370f2ea2930b44d8b80a57c5fcc759c05` |
| [Senato 2018](https://dait.interno.gov.it/documenti/opendata/senato/senato-20180304.zip) | 1,314,885 | `ebee153d2546a99a1e14682a070f05f6fa6008f53ac13a658506e0a9a3d87920` |

The 2022 ordinary tables are `camera2022_Italia_LivComune.csv` (117,591 rows,
including Camera TAA rows) and `Senato_Italia_LivComune.csv` (117,510 rows).
They contain municipality, electoral colleges, electorate/turnout, direct list
votes and repeated candidate totals. Senate Valle d'Aosta/TAA results and turnout
tables are separate; both ZIPs also contain foreign results, which were not
incorporated into this domestic assessment. The 2018 archive contains municipal
and aggregate Senate results; availability was verified, not age-adjusted fitting.

Exploratory Python comparison, reproducible from the fingerprinted ZIPs:

- Read the two 2022 ordinary CSVs as UTF-8 with BOM handling and semicolon
  delimiter. Normalize Camera `CIRC-REG` to region by removing a trailing
  constituency number 1-4; exclude regions starting `TRENTINO` or `VALLE` in both
  ordinary tables. Match on region and municipality name, then sum `VOTILISTA`
  by that key and list label. All **7,548** resulting keys match across chambers.
  This name-based join is exploratory; verify ISTAT identities, ambiguities,
  geography and correspondence before production fitting.
- On that common ordinary footprint, summed raw direct list votes are
  **26,600,019 Camera** and **26,601,426 Senato**. These are raw-field diagnostics,
  not final attributed national figures or a completed calibration dataset.
- For illustration, FdI's raw share differs by about **-0.122 percentage points**
  (Senato minus Camera), while the unweighted municipality difference's
  nearest-index 10th/90th percentiles are about **-2.083/+1.941 points**. List coverage, vote
  volume, candidate effects and attribution must be reconciled before interpreting
  that spread as chamber behaviour or future residual variance.

The [Ministry's age-change explanation](https://www.interno.gov.it/it/notizie/18enni-chiamati-voto-prima-volta-eleggere-senato-repubblica)
identifies 2022's common 18+ electorate after constitutional law 1/2021. Its
[2018 election instructions](https://www.interno.gov.it/it/notizie/elezioni-2018-come-vota-corpo-elettorale-tessera-elettorale)
state the former Camera 18+/Senato 25+ bases. Therefore 2022 is the primary paired
baseline reviewed here; older differences confound electorate age and chamber
behaviour. Scaling by population alone cannot recover unobserved age-specific
voting choices.

One comparable election's municipality pairs do not supply repeated independent
national forecasting errors. Geographic share correlation must not be reported
as a calibrated future-shock correlation. The reform also changes the old
uninominal candidate/ballot context, limiting transfer of 2022 contrasts. No
numerical future correlation, mean gap or residual variance was estimated here.

### Downstream requirements and scope

Use `paired-chamber-votes-v1`, separately versioned parameter/profiles and sources,
age/geography/coverage assumptions, seeds and supplied/common/model-derived
reference provenance. R26-05 must reserve those contracts. R26-19 must acquire
and prepare paired Senate history, reconcile R26-02 attribution, fit or explicitly
label covariance/contrast assumptions, implement the precedence table and scope
rules, preserve marginal uncertainty and validate paired sensitivity/replay.
R26-20 must report reference provenance and joint-bonus dependence assumptions.

Required fixtures cover common-only, one/both explicit chamber references,
chamber-only input without a common anchor, partial lists, conflicting scopes,
mean-mode uncertainty, normalization/abstention and coverage. Keep constructed
legal fixtures for different/linked/unlinked winners, one chamber at/below 42%,
single-chamber renewal and incomplete joint inputs independent of model fitting.
Separate Monte Carlo uncertainty within profiles from dependence sensitivity;
do not claim a universal direction of bias or measured certainty for assumptions.

The root plan and specification now include paired Senate-history preparation in
the current R26-19 scope. Post-migration work reviews/extends historical coverage
instead of planning a duplicate model; approval of R26-04 is not delivery of that
data pipeline. Updated dependent contracts, generation and reporting directives.
R26-02/03 contracts and the statutory transcription remain unchanged; all
application implementation/acceptance steps remain open.

Verification of this documentation update:

- All 77 local file/section links across the root plan, four reform documents
  and this entry resolved. Confirmed 22 ordered steps with only R26-01, R26-01a
  and R26-02/03/04 complete; R26-05 depends on those approved decisions. All
  N01-N29 and I01-I09 entries remain present.
- Checked common-only, one/both chamber references and chamber-only input
  without a common anchor are documented, together with same-scope precedence,
  geographic specificity and continuing mean-mode uncertainty. No obsolete
  pending-R26-04 directive remains in the active documents.
- R26-02/03 contracts and the statutory transcription are unchanged. Exactly
  five Markdown files changed. Rechecked all three temporary source ZIPs'
  fingerprints and readable directory entries; no source data were imported
  into the repository or fitted in this update.
- `git diff --check` passed. Application tests/build/browser checks and
  stochastic calibration experiments were not run for this documentation-only
  update. The contract/precedence checks above do not claim runtime behavior.

## 2026-10-17 R26-05 scrutiny contracts and validation

Implemented the isolated [reform contract boundary](../web/src/lib/politics/reform-2026/README.md)
and marked R26-05 complete. R26-06 is next. The new schema version and reserved
algorithm ID identify a legal input envelope; this step does not register an
executable algorithm or change saved scenario/snapshot versions.

### Delivered boundary

- Readonly, JSON/worker-safe input/result contracts cover joint/single renewal,
  chamber-specific votes and references, reciprocal political links, coverage,
  global person identities, affiliations, ordinary/bonus slates, preferences,
  special contests and sourced seat endowments. The shared simulation ID pairs
  chamber inputs. Model metadata carries supplied/generated provenance,
  versions, common anchors, resolved scopes and chamber reference origins.
- A strict structural schema rejects missing/extra fields, mixed vote variants,
  invalid dates, non-finite/negative/fractional/unsafe counts and incompatible
  envelope versions. Semantic validators report coded issues and paths. Successful
  validation returns an owned clone with a branded readonly type; callers cannot
  invalidate it by editing their original payload. No coercion or legacy migration.
- Raw ordinary components retain bonus-only ballots; attributed totals have no
  such field. Special candidate-only components remain separate and reconcile to
  candidate totals, counted once for the valid-ballot/electorate bound. Every
  college and contesting list/candidate requires a row/count, including zeros.
  Zero-vote elections remain accepted for later explicit I04 stage failures.
- Candidature checks cover seven filed slots, alternating genders, election-day
  ages 25/40, rounded national bonus/special gender limits, same-badge and multiple
  candidacy limits, local ordinary/bonus prefix links, special incompatibilities,
  single-renewal exclusions and foreign/other-chamber declarations. Subsequent
  candidate unavailability keeps the original filed slate intact and is accepted
  for later recovery, rather than misreported as a malformed filing.
- Coverage validation separates Camera and Senate: Camera's ordinary-contesting
  lists need half-constituency coverage, including declared linked special
  candidatures, subject to the asserted minority exception; Senate may contest
  one region. Both need two-thirds college coverage per filed ordinary territory.
  Special-only lists do not acquire a nationwide filing requirement. TAA
  proportional slates use the imported seven-slot/alternating rules. Camera TAA
  and Senate special uninominal gender checks are distinct, under I09.
- Missing preferences retain a reason. Supplied adjudicated counts assert upstream
  ballot validity, with identities and aggregate bounds still checked. Generated
  counts require the approved valid-combination certificate: distinct eligible
  candidates, up to three marks, mixed genders for multiple marks, correct exact
  margins and complete ballot budget. Raw components use direct list votes as
  that budget; attributed totals preserve unknown composition. Validity assertion
  and constructive certificate remain explicit alternatives, not inferred zeros.
- Endowment validation checks 392/196 domestic seats, 70/35 bonuses, special
  totals, college kinds/capacities and territorial reductions. Sources distinguish
  constructed, I08 provisional-census and official-decree tables. Provisional
  sources carry census, geography, method and table versions.
- Result types reserve stage and overall partial/non-computable status, subject
  decisions/reasons, conditional conclusions, severity-bearing warnings, policy
  versions, exact fraction strings, injected draw requests/traces, separate seat
  categories, allocation transfers and candidate recovery state. Original
  entitlement and actual election location remain separate, including I07
  cross-region recovery and explicit unfilled or
  unresolved seats; I06 remainder consumption has before/after states.

### Verification and boundaries

- Added **57 reform contract tests**, using tracked, invented geography and
  complete domestic totals independently of generated artifacts. Cases cover
  valid joint/single inputs, missing versus zero preferences, later unavailable
  candidates, malformed references/slots/links, coverage and age constraints,
  incompatible candidatures, double attribution, special candidate-only totals,
  invalid/safe numeric domains and constructive preference validity. Metadata
  fixtures cover common-only, one/both explicit chamber references, no common
  anchor, differing scopes and duplicate scope rejection. A typed JSON/structured
  clone round-trip preserves conditional partial results, I03 warning/draw links,
  I06 consumed remainders, I07 destinations and exact integers beyond JS's safe
  number range. This is contract evidence, not execution of those legal stages.
- `npm run check`: zero errors/warnings. `npm run test`: **23 files, 244 tests
  passed**, including existing legacy regressions. `npm run build` produced the
  static site successfully; Vite emitted timing advisories only. Verified all
  **107 local file/section links** across the root plan, four reform documents,
  history and contract README. `git diff --check` and a separate whitespace scan
  of the new contract files passed.
- Actual official geography, Bolzano joins, complete real regional coverage,
  constitutional regional minima and census apportionment remain R26-07;
  constructed fixtures assert internal accounting, not an official election map.
  Numerical/draw execution remains R26-06, attribution R26-08, special scrutiny
  R26-10 and initial recovery R26-17. Output JSON validation and actual result
  production are not implemented by the type declarations.
- The accepted R26-02/03/04 models remain unfitted. R26-19 must implement scope/
  supplied-share precedence, preference construction and statistical generators;
  current metadata tests establish representability and reference consistency.
  Legacy scrutiny, random streams, scenario schema, algorithm registry, workers,
  UI and generated/default data were left unchanged. Browser/performance checks
  are deferred to integration; no new application path is active in this step.

## 2026-10-17 R26-06 exact numerical primitives and explicit draws

Implemented the [numerical execution boundary](../web/src/lib/politics/reform-2026/README.md#numerical-execution-r26-06)
and marked R26-06 complete. R26-07 territories and seat endowments is next.
The implementation is isolated under the reform directory and exported through
its public index; no reform algorithm is registered or activated.

### Delivered boundary

- Nonnegative rational arithmetic uses bigint intermediates and reduced decimal
  string fractions at JSON/worker boundaries. Comparisons, integer/decimal parts,
  ratios and threshold comparisons remain exact beyond JS's safe number range.
  Unsafe number inputs and undefined/negative fraction operations are rejected.
  `truncateToSixDecimals()` computes `floor(x * 10^6) / 10^6` exactly.
- `allocateByQuotaAndRemainders()` separates integer quotas (truncate the quota
  first) from exact quotas used by TAA/attribution and normalized active indices.
  Callers provide exact figures, explicit secondary criteria at the prescribed
  geographic scope, terminal draw rules and optional residual-seat exclusions.
  Results expose raw/applied quotas, the exact initial-seat total, integer seats,
  vote remainders, quotient decimal parts and single-use remainder states.
- I04 outcomes are explicit: zero seats completes without division; no subjects,
  zero positive-seat vote total, zero integer quota, initial overallocation and
  impossible residual allocation return coded non-computable results with no
  assignments. Feasibility is checked before drawing; overallocation totals are
  checked exactly before conversion to safe seat counts. Malformed primitive
  arguments throw separately from these legal-domain failures.
- `resolveTie()` applies successive exact ascending/descending criteria, tracing
  surviving alternatives. Only a final unresolved tie consumes the injected
  resolver. Canonical ID ordering makes draws independent of source-array order;
  list presentation order, electoral figures, dates and percentages remain
  explicit caller-selected criteria. Injected trace/request inconsistencies are
  rejected instead of recording an ineligible or misidentified outcome.
- `createSeededDrawContext()` owns a private RNG seeded by the supplied seed and
  stream ID, with integer rejection sampling. No default/global RNG is added.
  Every I03 fallback returns a simulation-specific `warning` linked to its draw;
  express statutory draws retain their distinct rule and do not emit I03 warnings.
  The README requires downstream composition to preserve both returned arrays
  and the owning stage's applied interpretation versions.

### Verification and limits

- Added **49 numerical/tie tests**. Independently worked anchors cover ordinary
  `(10,3,1), 6 -> (5,1,0)` versus exact-quota `(4,1,1)`, and the arithmetic for
  N04 `(60,30,10), 7 -> (4,2,1)`. Tests inspect quotas and remainder values/state,
  scope-specific secondary criteria, list-order/age/percentage priorities,
  injected draws, six permutations, seeded stream isolation, per-simulation I03
  warnings and explicit failure paths. A constructed I05 arithmetic fixture
  changes a seat after truncating both indices; it does not execute the cap stage.
  Safe-number boundary and larger exact-intermediate fixtures round-trip through
  JSON. Small exhaustive exact-quota cases verify conservation and single use.
- On Windows, Node **v24.20.0**, npm **11.19.0**, from `web/`: `npm run check`
  reported **zero errors/warnings**; `npm run test` passed **26 files / 293 tests**
  in 4.37 s, including existing politics/regional and legacy allocation regressions.
  `npm run build` produced the static site successfully (Vite plugin timing
  advisory only). Subsequently `npm run test:e2e -- --reporter=line` passed
  **all 7 Chromium tests** in 13.3 s, including both worker workflows and result
  import/restore. Build and browser tests ran sequentially; Playwright also ran
  its configured build before starting preview. Browser output included existing
  color-environment notices and the Vite timing advisory, with no failed tests.
- All **113 local file/section links** across the root plan, reform documents,
  history and core README resolved. `git diff --check` and a separate whitespace
  scan covering all 17 reform TypeScript files, including untracked additions,
  passed.
- Statutory stage attribution remains R26-08/10, target corrections/caps
  R26-12 through R26-15, and candidate recovery R26-17. The generic six-decimal and tie
  helpers do not infer which provisional policies a caller must record. Undefined
  percentages and inactive zero-target groups must be guarded by the owning
  stage; no undefined value is converted to zero. Simulation-level warning
  propagation will be exercised when actual stages are composed in R26-18.
- Legacy helpers, scrutiny, seeds, registry, scenarios, generated data, workers
  and UI were not changed. No statistical fitting or performance benchmark was
  run; this step adds constructed numerical execution, not reform application
  acceptance. The existing legal specification/policies were implemented without
  introducing a new interpretation or changing the statutory transcription.
