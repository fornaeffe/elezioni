# Migration History

Record through: 2026-10-22

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

## 2026-10-18 R26-07 geography, census preparation and seat endowments

Implemented R26-07 and its [reproducible preparation workflow](../data/README.md).
R26-08 is next. Luca approved migrating shared acquisition/geography/census now,
using `data/sources` and `data/derived`, retaining `dati` as legacy, checking the
usability of the original spreadsheets, and permitting explicit population
estimates when finer data are unavailable. Historical vote/preference importers
and model calibration remain later consumers, including R26-19.

### Delivered boundary and source decisions

- Added reusable Node/TypeScript source acquisition, checksum verification,
  cached/manual import and immutable artifact writing. The tracked manifest pins
  **12 downloads / 145,471,137 bytes**; raw and intermediate files are ignored.
  Generation is offline, independent of R caches and scenario workbooks. Census,
  geography, actual used-source hashes, method, seed, population assumptions and
  reconstruction availability determine replayable versions.
- Investigated all four definitive 2020 XLSX tables. Explicit continuation/header
  handling extracts **8,073 A1 memberships / 147 colleges**, **146 A2 relations /
  49 colleges**, **8,073 B1 memberships / 74 colleges**, and **67 B2 relations /
  26 colleges**. A2/B2 relations agree with the machine-readable DBF. The primary
  catalog uses the government's **2022-updated `UT_Collegi2020.dbf`**, avoiding
  presentation-format dependence, obsolete proposal membership and manual Lazio
  patches. It includes Misiliscemi and the two municipalities moved to
  Emilia-Romagna. No polygon/GIS processing or second runtime is required.
- Applied the reform's Bolzano U04/U05/U06 overlay. Independent parsing of
  statutory annexes 4 and 5 yields the same **116 municipal memberships**;
  U04 contains Bolzano and Laives. A documented alias reconciles Montagna's
  post-census rename. Both autonomous provinces retain three Senate contests.
- Rechecked [Istat's electoral-population reference](https://www.istat.it/notizia/popolazione-censuaria/):
  the pinned 2021 legal census covers **7,904 municipalities / 59,030,133 people**.
  No applicable reform allocation decree was verified, so generated endowments
  are explicitly **provisional I08-v1**, not official decree allocations.
- Used Istat's 2021 population for Roma's **155 urban zones**, totaling exactly
  **2,749,031**. Their code crosswalk assumes continuity with electoral geography;
  geometric boundary identity has not been independently verified. An Italian
  warning is retained in the artifact and legal input's additive optional
  `SeatTableSource.populationNotes` field, without changing existing schema-1
  constructed inputs. `seatTableMessages()` separates this warning from ordinary
  provisional-table information; R26-20 must render the messages at integration.
- Missing fine-geography population uses available observations, historical
  weights and, only without informative weights, equal weights. Reconstruction
  conserves the legal municipal total and records method, inputs, estimates and
  seeded ties. Missing submunicipal files produce a different immutable version,
  rather than making the application depend on their availability. Corrupt
  downloads still fail integrity checks. Unused change-history/section-population
  files are cached for future importers, without claiming them as consumed inputs.
- Added pure catalog/table validation, exact I08 allocations, constrained Senate
  minima, alternative college capacities, a validated chamber adapter and a small
  tracked offline bundle. Camera has 54 active colleges and Senato 33. Endowments
  reconcile to Camera **8 special + 384 ordinary**, or **8 + 314 + 70 bonus**;
  Senato **7 special + 189 ordinary**, or **7 + 154 + 35 bonus**. Camera TAA has
  four uninominal seats plus three proportional seats. Official tables can later
  replace capacities with distinct decree provenance and versions, preserving
  earlier artifacts. Incompatible statutory totals/capacities remain errors.

### Verification, problems and limits

- On Windows, Node **v24.20.0**, npm **11.19.0**, from `web/`:
  `npm run check` reported **zero errors/warnings**; `npm run check:data` passed;
  `npm run test` passed **29 files / 328 tests** in 5.52 s. The **35 new tests**
  cover acquisition, source parsing, real municipal coverage/Bolzano, missing-data
  estimation, minima, accounting, immutable versions, official replacement,
  deterministic I03 draws and an independent Alabama-paradox example
  (`29 -> [14,1,14]`, `24 -> [11,2,11]`). After a validator guard refinement,
  preparation typechecking and the affected **28 tests** passed again. A final
  zero-information reconstruction refinement passed its **7 population tests**
  and preparation typechecking; all-zero inconsistent observations use available
  historical weights before falling back to equal weights.
- Real-source preparation tests explicitly skip when their ignored archives are
  absent. Tracked table/core fixtures and synthetic format/I/O fixtures require
  no network or large downloaded artifacts. The missing-submunicipal-file test
  generates and validates a usable complete seat bundle with estimation notices.
- `npm run data:download -- --sources ../data/derived/clean-clone-sources`
  downloaded/verified all originals into an initially empty cache. Generation
  into a separate empty output directory reproduced the published JSON **byte
  for byte**. Table version: `r26-seats-census-2021-i08-v1-9672c20529b2`;
  compact JSON-content SHA-256:
  `02bc849b58bbc1178c3216e898e7a09d07c46238a8e8d2d486ba30bf11ab97a3`.
- A separate Python `fractions.Fraction` audit over normalized populations,
  without importing TypeScript allocation helpers, independently reproduced
  **244 territorial/bonus/college-capacity comparisons**. No production
  remainder-cutoff ties occurred. Python was used only for this independent
  verification, not added to the preparation workflow.
- `npm run build` produced the static site (13.43 s; Vite plugin timing advisory).
  Subsequently `npm run test:e2e -- --reporter=line` passed **all 7 Chromium
  tests** in 13.3 s, including existing politics/regional workers and result
  import/restore. Build and e2e ran sequentially; browser output also included
  existing color-environment notices.
- Workarounds: the geographic archive is approximately 98 MB despite the source
  page's 23 MB label; its DBF attributes require Windows-1252 although the `.cpg`
  says UTF-8. Istat's SDMX endpoint returns HTTP 500 (`languageTag1`) for Node's
  default wildcard language header; the manifest's explicit `Accept-Language: it`
  resolves it and reproduces the pinned CSV bytes.
- The four added preparation dependencies are pinned dev dependencies. The npm
  audit reports nine findings in the existing SvelteKit/Vite/Vitest toolchain
  and transitive dependencies; none names the added preparation packages.
  Unrelated dependency upgrades were not folded into this step.
- All **122 local document links/anchors** resolved. `git diff --check` and a
  whitespace scan including untracked additions passed.
- Legacy R logic/data, snapshots/defaults, registry, workers and UI behavior
  remain unchanged. No reform algorithm is activated, no statistical model is
  fitted and no application/performance acceptance is claimed. No dependent
  legacy defaults require regeneration from this separate geography artifact.

## 2026-10-18: R26-08 electoral figures and preference rankings

Implemented only R26-08 under `web/src/lib/politics/reform-2026/`; R26-09
admission is the next step. The [module contract](../web/src/lib/politics/reform-2026/README.md#electoral-figures-and-rankings-r26-08)
documents calls, scopes and remaining boundaries. Consulted the
[promulgated act](https://www.gazzettaufficiale.it/eli/id/2026/10/09/26G00201/sg)
and [official comparison](https://documenti.camera.it/leg19/dossier/Pdf/AC0469g.pdf)
against the scoped transcription/specification. This step does not change the
normative source version or claim the future annotated republication was checked.

### Delivered boundary

- `distributeBonusOnlyVotes()` implements N04 at the ordinary plurinominal
  college, using R26-06's exact quota/remainder primitive. The independent
  `(60,30,10), B=7` fixture gives quota `100/7`, initial additions `(4,2,0)`,
  quotient fractions `(1/5,1/10,7/10)` and final additions `(4,2,1)`.
  Already attributed totals bypass allocation and draw consumption. Mixed
  representations are rejected. Zero only-ballots and single linked lists
  avoid division; positive only-ballots with multiple zero-support lists return
  a non-computable outcome with no invented or partial totals. All groups are
  preflighted before draws; group order and tied alternatives are canonical.
- `aggregateElectoralFigures()` implements N06 over disjoint attributed ordinary
  and special uninominal college rows. It supplies list/coalition figures and
  exact shares at college, territory, region and national scope. National figures
  include VdA/TAA for admission, first place and bonus eligibility; ordinary
  allocation figures exclude both. All list ballots stay in denominators and
  coalition totals include sub-1% members. Special-only lists remain represented.
  Missing/extra colleges or incomplete/duplicate/non-contesting list rows fail
  explicitly rather than silently producing partial national figures.
- Camera TAA proportional figures are a view of its uninominal territorial sum,
  not an additional ballot partition. The envelope's TAA `ordinaryVotes` row
  is not supplied as an ordinary aggregation contribution; its slate continues
  to carry preferences. No second copy of those ballots enters national totals.
  Raw special candidate-only attribution remains N10/R26-10: callers must supply
  attributed special list totals or future N10 output, never add candidate totals
  to list figures. The functions require validated political/geographic references;
  they are legal-stage helpers, not a composed scrutiny entry point.
- `rankPreferenceCandidates()` implements N05: capolista excluded, preferences
  descending, filed order breaking ties without a draw. Missing preferences
  remain a separate outcome from explicit zeros. Filed unavailable candidates
  stay in this ranking for later proclamation/recovery stages to handle.
  `calculatePercentages()` uses all valid college votes (TAA constituency votes
  for its proportional slate), with exact fractions in percentage units. A zero
  denominator yields an explicit undefined percentage, while zero-vote counts
  themselves can still be aggregated. Unsafe scalar counts/sums throw; bigint
  protects intermediate percentage multiplication.
- Representation-equivalence tests for both chambers use matching candidate
  identities/preferences and final totals after a deliberately tied raw
  attribution. Separate attribution/downstream streams give exactly equal
  aggregates, rankings and downstream tie traces. This is an accounting and
  reproducibility check; it does not establish statistical absence of bias.
  I03 ties retain simulation-specific warnings and JSON-safe traces.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  Final `npm run check`: **zero errors/warnings**. `npm run test`:
  **32 files / 351 tests passed**, 4.50 s, including **23 new tests** in three
  files. Tests cover worked arithmetic, college-before-aggregation rounding,
  conservation across integer cases, zero support, direct-total bypass,
  multi-subject draw ordering/preflight, all aggregation scopes, special ballots,
  subthreshold contributions, preference ties/denominators, missing versus zero,
  exact arithmetic/overflow, owned outputs and downstream equivalence.
- `npm run build`: static build succeeded, 4.45 s reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  12.6 s, covering the existing politics/regional workers and result imports.
  Build and e2e ran sequentially. Existing plugin-timing and color-environment
  notices were non-blocking. An initial readonly assignment in a permutation
  test failed typechecking; replacing it with an owned object copy resolved it.
- `git diff --check` passed. No R code, persistent data, generated defaults,
  registry, workers or UI were changed. No reform application path, admission,
  special scrutiny, generator/calibration, full composition or performance
  acceptance is implemented by this step. Changes are left uncommitted.

## 2026-10-18: R26-09 admission and preliminary special winners

Implemented only R26-09 under `web/src/lib/politics/reform-2026/`;
R26-10 special territories is next. The [module contract](../web/src/lib/politics/reform-2026/README.md#admission-and-preliminary-special-winners-r26-09)
documents inputs, outputs and the dependency order. Consulted the
[promulgated act and its Camera/Senato amendment articles](https://www.gazzettaufficiale.it/atto/vediMenuHTML?atto.codiceRedazionale=26G00201&atto.dataPubblicazioneGazzetta=2026-10-09&tipoSerie=serie_generale&tipoVigenza=originario)
and [official comparison](https://documenti.camera.it/leg19/dossier/Pdf/AC0469g.pdf)
against C83(1)(e), S16-bis(1)(e), C93(2)-(3), C93-septies(1)(a)-(b),(2)
and S21(1)-(2) in the scoped transcription/specification. No normative source
version changed; this does not check the future annotated republication.

### Delivered boundary

- `determineCameraAdmission()` and `determineSenateAdmission()` implement N07/N08
  with distinct coalition, standalone-list and internal-member decisions, stable
  reason codes and legal references. Evidence retains exact national shares,
  qualifying regions, protected-minority shares and required/actual uninominal
  wins. Comparisons use bigint products of counts at inclusive 3%, 10% and 20%;
  display percentages do not decide access. Special ballots remain in national
  figures and excluded votes remain in every applicable denominator.
- A coalition needs both national 10% and a qualifying member. Qualifying
  members enter under (2-bis); (2-ter) adds exactly one highest-national-figure
  otherwise excluded list, including a list below 1% or with zero votes. It
  adds none when every member already qualifies. Equal figures use canonical,
  injected I03 draws with simulation/chamber-specific warnings. Coalition order
  and tied alternatives cannot change the seeded draw sequence.
- Failed-coalition lists are evaluated for independent access. Camera's 20%
  minority route requires protected recognition, autonomous-region status and
  exclusive presentation. The quarter-uninominal-wins alternative is reserved
  to unlinked minorities and cannot qualify a coalition or its (2-bis) members.
  The independent fixture with five uninominal colleges requires two wins, not
  one. A failed-coalition minority below national 3% remains excluded nationally;
  `taaLocalExceptions` records I01 local 20% access separately, preserving
  C93-septies(2)'s anomalous (2-bis) reference without extending nationwide access.
- Senate's regional 20% route applies to any list, ordinary or special region,
  independently or inside an admitted coalition, and is not restricted to a
  minority or to allocation in the qualifying region. Zero-valid-vote regions
  cannot supply a 20% pass. Zero national valid votes return a non-computable
  stage with no invented exclusions/admissions or admission draws.
- `determineSpecialWinners()` implements preliminary N09 before admission,
  independent of list access or special proportional allocation. It uses the
  adjudicated candidate totals already present in both vote representations:
  most votes, then younger Camera/older Senate candidate, then warned I03 on
  equal dates. Counts/references for all colleges are preflighted before any
  draws; college and alternative order are canonical. The ledger preserves
  candidate, college/territory/region, votes, political subject, all list links,
  separate affiliation and tie traces. Camera admission requires the same
  simulation's complete preliminary winners, never recomputes their outcome
  and does not emit their draws/warnings again. R26-10 must reuse this ledger.
- The functions require validated political/geographic/candidate references and
  N06 aggregates from the same simulation. Focused completeness guards do not
  replace envelope validation. Full validated Camera/Senato fixtures run through
  preliminary winners, attributed aggregation and admission without generators
  or TAA proportional allocation. Public exports and the module reading map are
  updated; the existing result envelope and algorithm registry are unchanged.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`:
  **34 files / 391 tests passed**, 4.49 s, including **40 new tests** in two
  files. Coverage includes below/at/above thresholds, coalition qualification,
  independent access, minority coverage, I01, rounded-up quarter wins, regional
  Senate access, sub-1%/zero-vote rescue, multiple canonical I03 draws,
  same-preliminary-winner reuse, candidate age precedence, raw/attributed winner
  equivalence, all-zero denominators, large exact counts, JSON-safe owned results,
  swapped chambers and missing/duplicate special contributions.
- An initial fixture-array inference error was fixed with explicit input-array
  types; subsequent typechecking passed. A final strengthening of the vote-over-age
  test passed all **10 special-winner tests** again (254 ms).
- `npm run build`: static build succeeded, 3.95 s reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  12.7 s, covering existing politics/regional workers and result imports.
  Build and e2e ran sequentially. Existing plugin-timing and color-environment
  notices were non-blocking.
- `git diff --check` and a whitespace scan including untracked additions passed.
  No R code, persistent data, generated defaults, registry, workers or UI changed.
  Special list-vote attribution and TAA proportional seats remain R26-10;
  joint bonus, later allocations/proclamations/recovery, composition, generation,
  integration and reform application/performance acceptance remain pending.
  Changes are left uncommitted.

## 2026-10-18: R26-10 special territories, proportional seats and cap ledger

R26-09 was already implemented at the start of this task; retained its admission
and preliminary-winner behavior and completed R26-10 only. The
[module boundary](../web/src/lib/politics/reform-2026/README.md#special-territories-r26-10)
documents stage order, inputs, outputs and remaining dependencies. Checked the
[promulgated Camera amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticoloDefault/originario?atto.codiceRedazionale=26G00201&atto.dataPubblicazioneGazzetta=2026-10-09&atto.tipoProvvedimento=LEGGE)
and [Senate amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
against the scoped C92-C93-octies and S20-S21 transcription and N09-N11/I01-I04
specification. No normative source version changed; the future annotated
republication remains an acceptance check.

### Delivered boundary and rationale

- `attributeSpecialVotes()` translates N10 separately from preliminary winner
  calculation. Candidate-only ballots are attributed at each uninominal college,
  using exact quota/remainders for each candidate's linked lists, before any
  territorial sum. Single-list and zero-only cases avoid division; positive
  multi-list only-votes without direct support return a named non-computable
  result. The entire college is preflighted before draws; candidate/list order is
  canonical. Equal residuals use warned I03 draws without an invented secondary
  vote criterion. Senate uses the approved I02 cross-reference reading.
- Attributed special inputs bypass redistribution. Candidate totals remain
  unchanged in both representations; their ballots enter national admission and
  bonus figures once, including VdA-only lists, and never enter ordinary allocation
  figures. Traces retain additions, exact quotas/remainders and draw/warning events.
- `allocateCameraTaaSeats()` implements N11 with exact subject and internal-list
  quotas, then local electoral figures and express statutory draws on equal
  remainders/figures. The independent `(10,3,1)`, six-seat fixture yields
  `(4,1,1)`, rather than the ordinary integer-quota `(5,1,0)`. Its capacity is the
  proportional college's own invariant endowment, with no second subtraction of
  uninominal seats. I01 local standalone exceptions are unioned/deduplicated with
  the national communication without changing nationwide admission.
- Clarified the N11 specification: C93-septies(2)(b) allocates inside an admitted
  coalition among its locally filed coalition lists. It lacks C83(1)(g)'s express
  limitation to national (2-bis)/(2-ter) members. A dedicated fixture keeps a list
  nationally excluded while awarding it local proportional seats. This is the
  special provision's distinct allocation route, not a change to N07 decisions.
- TAA provisional candidates follow capolista then preference rank, with filed
  order on equal counts and all-valid territorial percentage denominators.
  Missing preferences permit the head but leave later identities unresolved;
  explicit zero counts remain computable. Unavailable/exhausted candidates retain
  entitlements with explicit R26-16/17 recovery requirements. Whole-chamber
  multiple-election precedence and refilling are still deferred to those steps.
- Corrected TAA preference-budget validation to use the territorial special-list
  ballots, excluding raw candidate-only components. Previously the compatibility
  `ordinaryVotes` row could incorrectly reject preferences supported by actual
  territorial ballots. Schema version 1 still requires that row; it never supplies
  extra ballots or a separate preference budget. Tests cover both a valid count
  exceeding its duplicate view and rejection above the raw direct-list budget.
- `proclaimSpecialUninominalWinners()` consumes the same matching N09 winner
  communication and accepts no draw dependency. Excluded linked lists cannot
  remove a winner. Availability may block the identity but not the entitlement.
  Links and personal affiliation remain separate; coalition seats have no
  invented individual-list attribution. Nested N09 traces are retained for
  evidence, not emitted as another set of top-level events.
- `summarizeSpecialSeats()` counts known uninominal and TAA entitlements once per
  subject for later caps. Validated Camera/Senate fixtures produce 8/7 special
  seats. Missing preferences do not change known counts; missing/non-computable
  Camera proportional entitlement remains null. Ordinary/foreign seats and cap
  decisions are outside this helper. Public exports and reading maps are updated.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`:
  **37 files / 438 tests passed**, 4.54 s, including **47 new tests** in three
  files. The focused reform suite passed **15 files / 242 tests** (469 ms).
  Coverage includes college-before-aggregation rounding, integer conservation,
  multi-candidate preflight/draw ordering, raw/attributed downstream equivalence
  with isolated draw streams, local/national access separation, both exact quota
  stages, local tie criteria/statutory draws, provisional candidates, special
  restrictions, winner reuse, owned JSON-safe results and validated-envelope
  integration. Existing N07/N08 and younger-Camera/older-Senate tests still pass.
- An initial discriminated-union narrowing error was corrected before the final
  typecheck. One fixture intended to exclude a coalition list initially gave it
  the best national subthreshold figure; increasing the other subthreshold
  member's national figure fixed the independent admission expectation.
- `npm run build`: static build succeeded, 12.50 s reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  13.2 s, including existing politics/regional worker and result-import paths.
  Build and e2e ran sequentially. Existing plugin-timing/color notices were
  non-blocking. `git diff --check` and whitespace scans of tracked/untracked
  additions passed.
- No R code, persistent data, generated defaults, algorithm registry, worker or
  UI changed. Joint bonus, ordinary allocations/caps, final multiple-election
  precedence/recovery, generators/calibration and application integration remain
  R26-11 onward. Later supplementary elections remain outside initial scrutiny.
  The plan now continues with R26-11. Changes are left uncommitted.

## 2026-10-18: R26-11 joint governability bonus decision

Implemented the requested N12 stage in `reform-2026/bonus.ts`, exported through
the core index. The [module boundary](../web/src/lib/politics/reform-2026/README.md#joint-bonus-decision-r26-11)
documents the stage contract and its remaining dependencies. Rechecked the
applicable [Camera amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
and [Senate amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
against the scoped C83/S16-bis transcription and N12/I03/I04 specification.
No normative source version changed; the announced annotated republication and
later corrections remain acceptance checks. Operative excerpts and article
references are retained beside the translated logic.

### Delivered boundary and rationale

- `determineGovernabilityBonus()` consumes the declared renewal scope, both
  admission communications, N06 national all-valid figures and explicit bonus
  links. It selects first place only among admitted coalitions/standalone lists;
  coalition members are not ranked again. Counts include VdA/TAA votes once and
  retain excluded subjects' ballots in the threshold denominator.
- Exact integer cross-products implement inclusive 42% independently in both
  chambers, ignoring display percentages. Both selected first subjects must be
  explicitly reciprocally linked. Equal IDs/names imply no link; list/coalition
  kinds remain distinct. A lower linked subject never replaces the first.
  Successful output awards 70/35 to the paired subjects, preserving coalition
  ownership without inventing shares for member lists or candidate identities.
- Results distinguish awards, computed no-award reasons and non-computable
  blockers. Threshold and linkage failures can both be reported. Single renewal
  follows the statutory no-bonus path without ranking/drawing; absent data in a
  declared joint renewal never synthesizes a chamber or implies single renewal.
  Both chambers are preflighted before consuming any legal draw.
- Equal first figures use the existing injected I03 resolver, canonical subject
  identities and fixed Camera/Senato order. Each fallback emits a warning with
  simulation/chamber/policy/context metadata; traces retain the selected subject
  and paired bonus effect. Admission events are not emitted again. Computed
  results retain counts, denominators, exact percentages and threshold decisions
  as owned JSON-safe data. Pairing stage records to the same simulation remains
  the caller's responsibility, as N06/N07/N08 records lack independent run IDs.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`:
  **38 files / 468 tests passed**, 4.48 s, including **30 new bonus tests**.
  Cases cover exact/below/above 42%, differing chamber winners, reciprocal/missing
  links, single and incomplete joint renewals, blocked-stage preflight,
  special-only first subjects, coalition/list identity collisions, excluded
  subjects, large exact counts, deterministic/permutation-invariant warned draws,
  chosen-tie bonus effects, ownership/JSON transfer and validated-envelope
  composition through special attribution, figures and admission.
- Verification caught a test union-narrowing omission and two expectations that
  specified only one element of a two-chamber array; corrected the tests before
  the final passing checks. No scrutiny change was needed for those failures.
- `npm run build`: static build succeeded, 12.81 s reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  13.3 s, covering existing politics/regional worker and import workflows.
  Build and e2e ran sequentially; existing plugin-timing/color notices were
  non-blocking. `git diff --check` and new-file whitespace scans passed.
- No R code, generated data/defaults, registry, worker or UI changed. This is
  bonus entitlement only: ordinary allocations/caps, territorial bonus seats,
  candidate proclamations/recovery, generators and application composition remain
  their later R26 steps. Both plans now continue with R26-12. Changes are left
  uncommitted.

## 2026-10-18: R26-12 Camera national allocation and cap

Implemented N13/N14 in `reform-2026/camera-national.ts`, exported through the
core index. The [stage boundary](../web/src/lib/politics/reform-2026/README.md#camera-national-allocation-r26-12)
documents inputs, traces and subsequent dependencies. Rechecked the applicable
[Camera amendments, article 1(22)(g)-(n)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
against the scoped C83 transcription and N13/N14/I04/I08 specification. No
normative source version changed. Operative excerpts and article references
remain beside the translated logic; later corrections and annotated
republication remain acceptance checks.

### Delivered boundary and rationale

- `allocateCameraNationalSeats()` consumes the validated Camera input and the
  same run's N06/N07/N10/N12 communications. It isolates provisional subject
  allocation, the cap check, cap correction and internal coalition-list targets.
  No bonus allocates 384 ordinary seats; bonus paths first allocate 314 and
  retain 70 bonus seats under the winning subject's ownership.
- Ordinary list figures exclude VdA/TAA votes. At subject level, coalitions
  include every linked list's votes; internal allocation uses only admitted
  (2-bis)/(2-ter) members, including the single best excluded list. All quota
  allocations discard the quota fraction before assigning integer seats and
  residuals, using exact remainders, ordinary national figures and injected
  statutory draws. Zero-seat coalitions do not require division.
- The cap check includes linked special seat entitlements, even when candidate
  identities remain unresolved. Exactly 220 is unchanged; 221 triggers a fresh
  national allocation. For five special seats, majority ordinary entitlement is
  145, minority entitlement is 169 and the winner retains 220 domestic seats
  including the bonus. The majority quota is floored for R26-13 without
  reapportioning its directly fixed target. Estero has no allocation/cap input.
- Output preserves provisional and final targets separately, majority/minority
  quotas, coalition allocations, remainder consumption and seeded draw evidence.
  Bonus seats are never divided among member lists. Results are owned,
  JSON-safe data and preserve seat-table provenance. The R26-07 bundled table
  remains provisional census data; the 392 = 384 ordinary + 8 special ledger
  and 314 + 70 ordinary/bonus alternatives are checked before allocation.
  Incompatible endowments are rejected rather than rescaled.
- Incomplete admission/bonus or required special totals block before allocation
  draws; I04 arithmetic failures retain the failed stage and attempted traces
  without publishing final targets. Unknown bonus capacity remains null.
  Upstream draw/warning events are not re-emitted. The N12 simulation ID is
  checked; pairing N06/N07/N10 communications remains the caller's responsibility
  because those records lack independent run IDs.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`:
  **39 files / 506 tests passed**, 4.52 s, including **38 new national-allocation
  tests**. Coverage includes all three paths, 220/221, zero through eight linked
  special seats, 314-seat conservation, quota/remainder traces, coalition-member
  filtering and corrected minority coalitions, identity collisions, statutory
  draws/replay/permutation invariance, I04 failures, endowment checks, ownership
  and validated single/joint-envelope composition through preceding legal stages.
- Verification caught two fixture expectations that used an incorrect raw-quota
  fraction and a TypeScript mutable-fixture assertion mismatch. Corrected the
  expectations and supplied an explicitly typed input before cloning; the final
  typecheck and full suite passed.
- `npm run build`: static build succeeded, 3.95 s reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  12.1 s, including politics/regional worker and result-import regressions.
  Build and e2e ran sequentially; plugin-timing/color notices were non-blocking.
- No R code, generated data/defaults, registry, worker or UI changed. This step
  establishes national seat entitlements, not territorial allocation or candidate
  outcomes. R26-13 onward retain their implementation scope. Both plans now
  continue with R26-13; changes are left uncommitted.

## 2026-10-18: R26-13 Camera territorial allocation

Implemented N15-N18 in `reform-2026/camera-territorial.ts` with the scoped matrix
engine in `camera-territorial-matrix.ts`, exported through the core index. Use
the [stage boundary](../web/src/lib/politics/reform-2026/README.md#camera-territorial-allocation-r26-13)
for API order, matrix contracts, correction rules and blockers. Rechecked the
applicable [Camera amendments, article 1(22)(l)-(n) and (23)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
and unchanged C83(h) wording in the
[official consolidated comparison](https://documenti.camera.it/leg19/dossier/Pdf/AC0469g.pdf)
against the scoped transcription and N15-N18/I03/I04/I05 specification. No
normative source version changed. Applicable operative excerpts/references
remain beside the translated operations; the later republication/correction
and decree checks remain R26-21 acceptance work.

### Delivered boundary and rationale

- `allocateCameraConstituencies()` consumes final N13/N14 subject targets,
  ordinary constituency figures and the selected endowments. N15 uses integer
  local quotas, exact decimal parts, ordinary national figures and statutory
  draws. N17 instead uses the corrected majority/minority national divisors,
  truncates both sides before normalization, and records `I05-v1` explicitly.
  `allocateCameraCapConstituencies()` is also independently callable.
- `allocateCameraCoalitionListsInConstituencies()` uses only the national
  admitted internal-list targets. Initial N16 ties use constituency list votes;
  corrections import h's national criteria. Excluded linked votes still count
  in the coalition's subject figure, but not its internal denominator.
- `allocateCameraColleges()` uses only positive constituency list targets.
  The normal branch applies college integer quotas with constituency-vote
  residual ties. The cap branch first calculates majority/minority group
  quotients and both truncated college indices, reconciles groups within each
  college, then allocates each group's lists separately. Zero-target groups
  are inactive and do not evaluate an undefined quota/index.
- `allocateCameraTerritorialSeats()` composes these three communications,
  retaining each stage result. It does not proclaim candidates, distribute
  coalition bonus identities, execute recovery, or register a full algorithm.
  Missing preferences do not prevent computing ordinary seat entitlements.
- Matrices expose capacities, divisor/quotient arithmetic, raw/truncated indices,
  integer parts, decimal parts, reached-target exclusions, provisional/final
  seats, used/unused/consumed remainders and ordered movements. Integer parts
  are computed across the full matrix before reached-target residual exclusions,
  avoiding an arbitrary geographic ordering. Inputs are unmodified; results
  are separately owned and JSON-safe. Draws are canonical/injected; missing
  correction tie-breaks warn under I03, separately from statutory initial draws.
  Upstream events are not re-emitted by downstream stages.

### Correction accounting and explicit limits

- N15/N16/N17 process surplus subjects by excess seats, then ordinary national
  figure. They search all eligible same-constituency movements before falling
  back across constituencies. Donors use ascending used decimals; recipients
  use descending unused decimals. Same-constituency recipient ties use national
  figures; unspecified cross-constituency destination ties use I03. Movements
  consume donor remainders and use recipient remainders once.
- The independent worked fallback `(A,B,C)` fixture has capacities `(2,2,3)` and
  votes `(40,35,25)`, `(45,40,15)`, `(100,45,155)`. Initial allocations total
  `(3,2,2)` against targets `(2,3,2)`. Removing A's used `.8` in the first
  constituency and assigning B's unused `.45` in the third yields the required
  national targets and local totals `(1,2,4)`. Thus a statutory fallback need
  not preserve the original individual constituency capacities.
- If that corrected local communication cannot match its fixed college
  endowment, N18 returns `constituency-capacity-target-mismatch` and the affected
  territory, retaining complete N15/N16 results. No compensating seat movement
  or decree-table rescaling is invented. Any subsequent reconciliation requires
  reviewed legal support. The scoped coordinator must preserve the blocker;
  this step does not claim a composed full scrutiny for that exceptional case.
- N18 normal/list-within-group movements rank donor and recipient colleges
  independently and can change college totals while preserving constituency
  list entitlements and total seats. Its cap-group correction instead conserves
  each college capacity. These operations remain distinct. Undefined arithmetic,
  impossible residuals or no eligible correction progress return explicit
  non-computable results without final targets.

### Verification

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`:
  **41 files / 548 tests passed**. The **42 new tests** cover independently
  worked matrices, all three national paths, local versus national tie criteria,
  surplus ordering, same/cross-area movements, consumed remainders, no-progress
  failures, zero groups, I05 sixth/seventh-decimal boundaries and changed-seat
  cases, special exclusion, stage prerequisites, seeded replay/permutations,
  ownership and JSON round trips. Existing validated single/joint-envelope tests
  now continue through N18 with **384/314** ordinary entitlements respectively.
- Fixture development caught impossible expectations: four required recipient
  remainder seats cannot be supplied by three single-use remainder cells, and
  a cap-group donor without a remainder seat cannot cede an integer seat under
  the specified route. Corrected those hand-worked fixtures; separate tests
  retain the explicit no-progress behavior. Typechecking also caught an overly
  narrow mutable-fixture assertion; cloning an explicitly typed input fixed it.
- `npm run build`: static build succeeded, **3.95 s** reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  **12.6 s**, covering politics/regional workers and result import. Build and
  e2e ran sequentially. Plugin-timing/color notices were non-blocking.
- No R code, generated snapshots/defaults, registry, worker or UI changed.
  Senate allocation, proclamations, multiple-election precedence, recovery and
  application integration retain their subsequent steps. Both plans now continue
  with R26-14; changes are left uncommitted.

## 2026-10-19: R26-14 Senato regional allocation and cap

Implemented N19/N20 in `reform-2026/senate-regional.ts`, exported through the core
index. Use the [regional stage boundary](../web/src/lib/politics/reform-2026/README.md#senato-regional-allocation-r26-14)
for inputs, arithmetic, ordered cap movements, failure states and later-stage
limits. Rechecked S16-bis against the scoped transcription and the
[official Senate amendments, article 2(9)(f)-(h)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1).
No source version changed. Operative excerpts and references remain beside the
translated operations. Acceptance-time correction/decree checks remain R26-21.

### Delivered behavior and accounting

- `allocateSenateRegions()` consumes same-run N06/N08/N10/N12 communications.
  N19 allocates subjects independently in each ordinary region, using integer
  quotas, exact remainders, regional electoral figures and statutory draws.
  Every linked list contributes to its coalition's figure; coalition-to-list
  allocation remains N21. National totals are sums of regional allocations,
  never a national proportional allocation redistributed to regions.
- Endowments conserve 196 domestic seats: 189 ordinary plus 7 special, or
  154 ordinary plus 35 bonus plus 7 special. Regional alternative-capacity
  differences must match regional bonus seats. Incompatible tables are rejected
  without rescaling. Bundled census-table provenance remains provisional I08.
- `applySenateCap()` exposes the independently callable N20 boundary. Exactly
  113 is unchanged; 114 triggers correction. The majority ordinary target is
  `78 - linkedSpecialSeats`, retaining the 35 bonus seats. Special entitlements
  count once regardless of unresolved candidate identities; Estero is absent.
  No bonus means no cap check or correction.
- The ascending national removal ranking uses raw vote remainders that supplied
  majority seats. Regional quotient decimal parts would give a different order
  when regional quotas differ. Equal values share a rank until a required
  removal invokes I03, avoiding draws for irrelevant ordering below the cap.
- After the single-use remainder removals, percentage passes rank exact
  majority shares against all valid regional votes, including excluded lists.
  The first pass excludes remainder-phase regions and protects a sole seat.
  Later passes readmit remainder-phase regions and protect the current final
  seat. The six-sentence structure of S16-bis(1-ter)(a) matters: sentence six
  repeats only sentences three/four (percentage ranking/one per region), not
  sentence five's initial exclusions, and specifies current-last-seat protection.
  Final review temporarily carried the initial exclusions into every pass after
  miscounting those sentences; checking the numbered wording corrected that
  interpretation before final verification. Tests cover re-entry, an empty
  first pass, and sole used-remainder seats which can become zero before the
  percentage protection applies.
- Every removal has a corresponding assignment in the same region. Recipients
  use their largest unused remainder once, then regional votes after all such
  remainders are exhausted. Equal removal remainders, percentages, recipient
  remainders and fallback votes use warned seeded I03 draws. No unexpressed
  regional-vote secondary criterion is imported into remainder-correction ties.
- Results retain provisional regions and national totals separately from final
  targets, exact quotas/remainders, `I06-v1`, ordered movements, pass numbers,
  donor/recipient seat counts and remainder states before/after, tie resolutions,
  draws and warnings. Donated used remainders become consumed; recipient unused
  remainders become used. Each paired movement preserves its region's capacity.
- Incomplete prerequisites block before allocation draws. I04 arithmetic failures
  retain the affected region and attempted quota trace. No eligible recipient
  or no lawful removal progress retains provisional data and attempted movements
  without publishing final targets. Outputs are owned and JSON-safe; input
  permutations preserve replay. Upstream warnings/draws are not re-emitted.
  N12's simulation ID is checked; N06/N08/N10 pairing remains the caller's
  responsibility because those communications lack independent run IDs.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  `npm run check`: **zero errors/warnings**. Final `npm run test`:
  **42 files / 611 tests passed**, 6.05 s, including **63 new tests**.
  Coverage includes all three paths, every zero-to-seven linked special-seat
  count at 113/114, regional rather than national allocation/ties, raw-remainder
  ranking, repeated passes and exclusions, last-seat protection, recipient
  exhaustion/fallback, I03 warnings, explicit no-progress and arithmetic failures,
  zero-seat regions, endowment/prerequisite checks, coalition/list identity
  collisions, ownership, JSON and seeded permutation replay. Validated single
  and joint envelopes continue through N19/N20 with 189/154 ordinary seats;
  the joint majority has 71 ordinary + 7 special + 35 bonus = 113.
- Fixture verification caught an inconsistent national capacity, an incorrect
  expectation for a floored quota and mutable/union fixture typing errors.
  Corrected the fixtures; the final checks above passed.
- `npm run build`: static build succeeded, **5.33 s** reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  **15.7 s**, covering politics/regional workers and result import. Build and
  browser checks ran sequentially; plugin-timing/color notices were non-blocking.
- No R code, generated snapshots/defaults, registry, worker or UI changed.
  Internal Senate lists/colleges, proclamations, recovery and full application
  integration retain their later steps. Both plans now continue with R26-15;
  changes are left uncommitted.

## 2026-10-19: R26-15 Senato list and college allocation

Implemented N21/N22 in `reform-2026/senate-territorial.ts`, exported through the
core index. Use the [stage boundary](../web/src/lib/politics/reform-2026/README.md#senato-list-and-college-allocation-r26-15)
for inputs, arithmetic, traces, failures and remaining scope. Rechecked S17
against the scoped transcription and [official article 2(10)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1).
No source version or interpretation policy changed; acceptance-time source and
decree checks remain R26-21.

### Delivered behavior and rationale

- Regional coalition-list allocation uses corrected N19/N20 subject targets,
  only admitted 2-bis/2-ter members, integer quotas, exact remainders, regional
  list votes and statutory draws. Single-college regions take those targets
  directly. Ordinary list allocation never divides the coalition's 35 bonus
  seats among member lists.
- Multi-college ordinary allocation uses integer quotas, reached-target
  exclusion, independently ranked surplus/deficit movements and college votes
  for initial remainder ties. The corrected-cap path first allocates majority
  and minority groups with both indices truncated to six decimals; group ties
  also use college votes. Group corrections stay within each college, then
  S17(4) allocates the lists within each group. Region and regional list totals
  are conserved; independent list corrections may change college totals.
- Extracted the existing Camera matrix arithmetic to `territorial-matrix.ts`,
  with explicit chamber and caller-selected criteria. Camera criteria, context
  IDs and algorithms retain their behavior. Equivalent college correction and
  index formulas are shared; Senate regional and college tie figures remain
  distinct. Matrix traces now identify the chamber explicitly.
- Stage communications check source/run identity where available, final subject
  and admitted-list coverage, regional capacities and college/regional figure
  reconciliation. N06/N08 still lack run IDs, so their pairing is the caller's
  responsibility. Zero-seat stages/groups skip undefined divisions. Invalid
  communications throw; non-computable prerequisites, I04 failures and missing
  lawful correction progress retain traces without final targets. Statutory
  draws and warned I03 correction draws remain distinct, and upstream events
  are not emitted again by downstream stages.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**; commands from `web/`.
  Final `npm run check`: **zero errors/warnings**. `npm run test`:
  **43 files / 649 tests passed**, **6.15 s**, including **38 new tests**.
  Existing Camera matrix/territorial and old-law regressions passed.
- Worked fixtures cover integer versus exact coalition quotas, admitted-member
  denominators, single/multiple colleges, independent regions and shared-region
  territories, regional list targets, surplus/deficit corrections, reached-target
  exclusion, zero-capacity colleges/groups, six-decimal changed-seat examples,
  college-versus-regional tie figures, statutory/I03 draws, no-progress failures,
  source/run/figure checks, ownership, JSON and seeded permutation replay.
  Validated single/joint envelopes now continue through N22: 189/154 ordinary
  seats respectively, with the joint majority retaining 71 ordinary seats plus
  7 special and 35 bonus seats from N20.
- Initial fixture runs exposed missing contesting slates, shared source-object
  aliases, fixture admission below threshold, readonly fixture typing and an
  expectation omitting the separate I03 recipient draw. Corrected the fixtures;
  final checks above passed. The validated multi-college envelopes legitimately
  require seeded correction draws rather than the previous no-draw test context.
- `npm run build`: static build succeeded, **5.46 s** reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  **15.5 s**. Build and browser checks ran sequentially. Plugin-timing/color
  notices were non-blocking.
- No R code, generated snapshots/defaults, registry, worker or UI changed.
  Candidate proclamations, multiple-election precedence, recovery and application
  integration retain their later steps. Both plans now continue with R26-16;
  changes are left uncommitted.

## 2026-10-19: R26-16 proclamations and multiple elections

Implemented N23 in `reform-2026/proclamations.ts` and N24 in
`reform-2026/multiple-elections.ts`, exported through the legal-core index.
Use the [stage boundary](../web/src/lib/politics/reform-2026/README.md#proclamations-and-multiple-elections-r26-16)
for inputs, provisional batches, precedence decisions and the R26-17 handoff.
Rechecked Camera C83-bis/C84/C85/C93-octies and Senate S17/S17-bis/S20-bis
against the scoped transcription, [official Camera amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1),
[Senate amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
and the [Camera consolidated comparison](https://documenti.camera.it/leg19/dossier/Pdf/AC0469g.pdf)
for unchanged cross-references. No source version or interpretation policy
changed; acceptance-time source and decree checks remain R26-21.

### Delivered behavior and rationale

- N23 receives final ordinary list targets and the existing special/TAA and
  bonus communications. It elects the winning bonus slate in filed order,
  retaining personal affiliation separately from coalition entitlement, then
  ordinary/TAA capolista and preference-ranked candidates. Special winners are
  reused without new draws; TAA seat IDs are preserved without double counting.
- Missing preferences leave dependent identities unresolved while preserving
  known heads and all entitled seats. Explicit zeros use filed preference order.
  Unavailable candidates and exhaustion remain explicit recovery requirements;
  no replacement route is run implicitly. Stage/run/source checks and target,
  figure and seat-total checks reject mismatched communications. Filed
  special/bonus incompatibilities are rejected before proclaiming candidates.
- N24 applies special uninominal, bonus and capolista precedence in order.
  Repeated heads use the lowest exact list percentage; other repeated wins use
  the highest exact individual percentage. Only the surviving role's wins enter
  comparisons. Final equal percentages use canonical seeded I03 draws with
  warnings; undefined percentages leave competing seats unresolved without
  fabricating a zero or a winner.
- Each person retains at most one filled seat. Released seats retain their ID,
  category and original entitlement, with explicit links to the former candidate
  and retained seat. Decisions record eligible/considered seats, legal references
  and draw IDs. Outputs are owned, JSON-safe and replay under source permutations.
  A complete N23 batch is provisional; released/unresolved seats keep N24 partial.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**; commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`: **45 files /
  699 tests passed**, **4.64 s**, including **50 new tests**. Focused N23/N24
  verification also passed **50/50**, including the final fixture refinement.
- Worked fixtures cover both chambers, every precedence, mixed roles, list
  percentage versus individual percentage, exact I03 ties, undefined values,
  missing/zero preferences, unavailable candidates, exhaustion, preserved seat
  identities, incompatible input, communication checks, ownership and JSON replay.
  Validated joint envelopes continue through N23/N24 with **392/196** entitled
  seats and **70/35** retained bonus members. Mixed-affiliation coalition slates
  keep coalition `listId: null`; a real special/TAA double election retains the
  special seat. Deliberately oversized fixture colleges remain exhausted/partial,
  correctly preserving entitlements rather than claiming complete recovery.
- Initial verification caught fixture-only readonly typing and an incorrect
  Camera helper import name; these were corrected before the final checks.
- `npm run build`: static build succeeded, **5.12 s** reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  **12.6 s**. Build and browser checks ran sequentially. Plugin-timing/color
  notices were non-blocking; existing politics/regional workflows passed.
- R26-17 still owns ordered exhaustion/replacement routes and repeated
  proclamation/recovery with a progress condition. R26-18 composition/registry
  and later generators, workers and UI remain inactive. No R code or generated
  snapshots/defaults changed. Both plans now continue with R26-17; changes are
  left uncommitted.

## 2026-10-19: R26-17 initial exhaustion and replacements

Implemented N25/N26/N27 in `reform-2026/recovery.ts`, exported through the
legal-core index. Use the [recovery boundary](../web/src/lib/politics/reform-2026/README.md#initial-exhaustion-and-replacements-r26-17)
for its inputs, immutable remainder evidence, rounds and R26-18 handoff.
Rechecked Camera C84/C86/C83(1-quinquies) and Senate S17-bis/S19/S16-bis against
the scoped transcription, [official Camera amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1),
[Senate amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
and the [Camera consolidated comparison](https://documenti.camera.it/leg19/dossier/Pdf/AC0469g.pdf).
The approved I07 regional-first protocol remains unchanged; final official-source
and seat-decree checks remain R26-21.

### Delivered behavior and rationale

- Camera initial exhaustion follows all permitted C84 levels. Initial releases
  use C86(2)'s limited reference and cannot reach C84(7). Senate ordinary
  recovery exhausts the original regional list and coalition before residual
  same-list cross-region recovery. It never imports ordinary C84(7).
- Bonus shortages use the separate express C83/S16-bis route: local winning
  list/coalition, member-list unused then used remainder, best college list
  percentage, candidate ranking; only then other territories ranked by the
  bonus-winning subject. Coalition entitlement is preserved without invented
  member-list bonus quotas. Territorial admissions are explicit inputs, so an
  excluded filed list cannot become a coalition replacement.
- Final allocation decimals and states remain immutable priority evidence,
  separate from allocation-transfer traces. Already-consumed correction
  decimals are already-used, never unused. Recovery fills further eligible
  candidate slots without recalculating quotas or imposing an invented quota
  on the number of replacement identities a destination may supply.
- Proposal rounds apply N24 repeatedly. Each successful round adds an unelected
  identity; undefined comparisons freeze their seats and unchanged states stop.
  A released recovered seat searches at its actual election location while its
  original entitlement, category and ID survive. Missing preferences do not
  become zero and do not conceal independently proven candidate exhaustion.
  Missing applicable ranking evidence remains unresolved; exhausted routes
  remain unfilled. Lower already-used missing decimals do not block a known
  unused tier.
- Ordinary decimal ties use statutory C84(8) draws. Bonus ties retain I03.
  Extended N24 only for explicitly marked ordinary-recovery proposals into
  multiple bonus seats: no final comparison is supplied, so warned I03 chooses
  between those seats. Filed multiple bonus candidatures remain invalid.
  The specification records this I03 boundary. Every proposed I07 cross-region
  recovery warns, including subsequent releases; trace dispositions and draw
  references preserve the complete history.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**; commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`: **46 files /
  748 tests passed**, **4.53 s**, including **49 new recovery tests**.
  Focused recovery and proclamation verification passed **74/74**.
- Worked fixtures cover I07-A through I07-F, every route, used/unused ordering,
  coalition and bonus selection, all-used variants, statutory versus I03 draws,
  repeated subentro, fully exhausted candidates, undefined comparisons, missing
  preferences/quotients, admitted lists, ownership and source-permutation replay.
  A cross-region recovered seat released by N24 is replaced at its actual
  destination while preserving its original regional entitlement.
- Existing validated joint fixtures now continue through recovery with
  **392/196** entitlements, **70/35** bonus holders, unique final identities and
  unchanged special winners. Their deliberately oversized colleges correctly
  retain unfilled residue rather than claiming full scrutiny acceptance.
- Initial focused checks exposed source-order-dependent eligibility trace
  ordering; canonicalized that evidence. An equal-percentage fixture required
  corrected supplied counts for its intended no-draw comparison, and test
  mutations needed explicit union narrowing. Final focused/full checks passed.
- `npm run build`: static build succeeded, **3.85 s** reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  **12.6 s**. Build and browser checks ran sequentially. Plugin-timing/color
  notices were non-blocking. `git diff --check` passed.
- R26-18 still owns complete stage composition, admission/remainder adapters
  and registry activation. Later scenario/generator, worker and UI stages remain
  inactive. Future vacancies and supplementary elections remain outside scope.
  No R code, generated snapshots/defaults or persistent data changed. Both plans
  now continue with R26-18; changes are left uncommitted.

## 2026-10-19: GitHub Actions/Pages target and final publication gate

Luca selected GitHub Actions plus GitHub Pages on GitHub Free as the desired
hosting target for the whole migration, with reassessment and conditional
publication as its last step, before post-migration additions. Luca retains the
merge into `main`; the current checkout is named `migrazione` (the migration
branch), and no branch rename or merge was performed.

### Decision and targeted documentation changes

- `MIGRATION_PLAN.md` now sets the hosting target and ends with a gate covering
  current free-tier eligibility/limits, complete cold/warm-cache Linux runner
  measurements, production transfer sizes, all migrated election kinds and
  project-base browser checks. Passing feasibility leads to publication from
  `main` after Luca's merge and deployed smoke tests. A failed assessment keeps
  the task open and triggers a review of free alternatives.
- `AGENTS.md` retains only stable hosting/base-path and release-data rules.
  `data/README.md` owns the shared non-interactive Node preparation contract:
  clean-clone regeneration, versioned sources/models/bundles/defaults, optional
  caches, checksum failures and production-only payloads. Its existing
  `--publish` command publishes a local source artifact, not a website.
- R26-18 keeps the composed legal core browser-worker compatible. R26-19
  delivers its required historical import/calibration through shared Node
  commands; R26-20 handles configurable project-base URLs, versioned payloads
  and explicit missing-data failures. R26-21 includes preparation typechecking
  and production-data/base-path acceptance. Its application acceptance remains
  distinct from final all-election hosting acceptance and publication.
- Existing model/legal decisions, R26 ordering and completion markers remain
  unchanged. The final gate contains no fixed provider quotas that could become
  stale; recheck the official [Pages limits](https://docs.github.com/en/pages/getting-started-with-github-pages/github-pages-limits),
  [Actions limits](https://docs.github.com/en/actions/reference/limits),
  [runner resources](https://docs.github.com/en/actions/reference/runners/github-hosted-runners)
  and [billing allowances](https://docs.github.com/en/billing/concepts/product-billing/github-actions)
  when executing it.

### Assessment evidence and verification scope

- The preceding hosting assessment verified a public repository and measured
  145,471,137 bytes of pinned source downloads, cached geography/census generation
  in 1.67 s, and a successful static build in 4.71 s (28 files / 116,030,390 bytes).
  The politics snapshot was 75,921,415 bytes, or 4,034,519 bytes with gzip;
  Emilia-Romagna was 5,102,281 bytes, or 325,628 bytes with gzip. These were local
  Windows measurements, excluding cold downloads, dependency installation,
  future import/calibration and actual deployed content encoding. They do not
  establish free-runner or publication acceptance.
- Documentation-only verification: `git diff --check` passed; all **100 local
  links/anchors** in the five edited documents resolved. A structural check
  confirmed the publication gate is the last migration task before the
  post-migration section. Application tests were not rerun for these prose edits.
  No application, workflow, source manifest, generated data or legal/model
  implementation changed; no R26 step or publication was completed.

## 2026-10-19: R26-18 composed scrutiny and simulation registry

Implemented the requested composition/registry step. Operational state is in the
[reform plan](politics-reform-2026-plan.md#r26-18-composition-and-registry); use the
[core boundary](../web/src/lib/politics/reform-2026/README.md#composition-and-registry-r26-18)
for direct execution, request/result types, registry availability and limitations.

### Delivered behavior and rationale

- `runPoliticsReformScrutiny()` validates the full owned envelope before draws,
  derives all stage communications within one global simulation, and composes
  N01-N27 through N28. It reuses special winners, attributes raw votes once,
  aggregates distinct national/ordinary figures, decides the paired bonus, runs
  each allocation/cap path, proclaims candidates, resolves multiple elections,
  and performs terminal initial recovery. It accepts single-chamber renewals.
- Added the simulation-level `runPoliticsSimulation()` boundary and registered
  both real algorithms. The legacy adapter calls the unchanged scrutiny and
  checks supplied `SIM` tags. Reform requests must match their envelope's
  algorithm/simulation. Unknown IDs are rejected by the common resolver; reform
  is explicitly unavailable in production mode until R26-19/20. The existing
  legacy generator/worker resolver retains its reported unknown-ID fallback,
  but now rejects a known reform request instead of executing the old law.
- Default legal streams include seed, stream version, algorithm and simulation
  ID; callers can still inject draws. Results retain source/model/seat metadata,
  conditions, applied policy versions, admission/bonus reasons, detailed stage
  communications, exact arithmetic, located allocation movements and final
  original/actual seat locations. Owning stages contribute warnings/draws once.
  Terminal ledgers check domestic entitlement totals and unique candidates.
- Recovery adapters use admissions, including zero-seat lists and local TAA
  exceptions, rather than positive targets alone. They preserve final matrix
  decimal states, Senate cap transitions, regional coalition-list quotas and TAA
  quotas with source IDs. A consumed decimal stays in the already-used tier;
  recovery does not consume it again. Coalition affiliations remain separate
  from bonus entitlements. Located transfer summaries also preserve coalition
  membership rather than relabelling member lists as standalone subjects.
- Composition exposed the need for a unique TAA proportional communication.
  Added an endowment check for exactly one Camera TAA proportional college,
  preventing selection of an arbitrary view or duplicate proportional counting
  before any draw. Legal references remain next to this validation and the
  existing scrutiny operations.

### Verification, problems and limits

- Independently worked complete fixtures use six-seat ordinary colleges,
  disjoint seven-person slates and supplied preferences. Full domestic A/B/C
  entitlements are: no bonus Camera `179/149/64`, Senate `92/73/31`; bonus
  without cap Camera `217/122/53`, Senate `111/59/26`; corrected cap Camera
  `220/121/51`, Senate `113/64/19`. Tests explain their quota/remainder arithmetic,
  count 8/7 special seats, preserve 70/35 bonuses, and fill 392/196 unique people.
- Additional composed fixtures cover mixed-affiliation coalition bonuses,
  ordinary/bonus replacement, I07 cross-region ordinary and bonus recovery,
  missing preferences, fully unavailable candidates, single renewals, zero-vote
  and zero-direct-support failures, injected/seeded replay, simulation identity,
  owned JSON output, source remainder states and raw/final vote equivalence.
- Some source stages do not calculate college/coalition or single-college
  quotients. The adapter does not invent them or substitute cap group indices.
  A constructed shortage test proves the affected recovery ends explicitly
  `unresolved` with `missing-recovery-remainder`, retaining all 588 entitlements.
  This remains a candidate-output limitation requiring reviewed source-stage
  support before claiming unrestricted candidate-level acceptance. The existing
  focused R26-17 boundary can accept independently established communications.
- The initial legacy adapter test compared R keyed-table ordering directly to
  TypeScript emission order. Corrected the test to compare keyed tables, as the
  existing golden checks do, while also requiring exact equality with the direct
  TypeScript function. All ten paired old-law simulations, including simulation
  4, pass without changing legacy legal behavior. An initial exhaustion fixture
  used low counts that correctly failed I04 integer-quota allocation; replaced
  it with a valid full allocation and explicit post-filing unavailability.
- `npm run check:data` passed; `npm run check` passed with zero errors/warnings;
  `npm run test` passed **766 tests in 48 files**, without skips. The static build
  passed. Browser verification passed **8 Chromium tests**, including politics
  and regional regressions and a new bundled module-worker test of direct reform
  execution, transferable paired results, replay and unavailable/unknown IDs.
  The browser worker is test-only and does not activate production dispatch.
  Build/e2e ran sequentially; Vite plugin timing and terminal color notices were
  informational, with successful exits.
- R26-19 generation/model fitting and R26-20 scenario/production worker/UI/export
  integration remain inactive. No generated defaults, data bundles, legal
  interpretation policy, production deployment or saved-file migration changed.
  Both plans now continue with R26-19. Changes are left uncommitted.

## 2026-10-19: recovery evidence handoffs and provisional I10

Follow-up to R26-18, explicitly authorized by Luca: repair missing downstream
communications without new legal assumptions; research the inherited C84(5)
coalition phrase and the R implementation; use the proposed member-list maximum
provisionally if the evidence does not establish another interpretation. The
[interpretation register](politics-reform-2026-specification.md#interpretation-register)
contains the operational I10-v1 policy. The [core README](../web/src/lib/politics/reform-2026/README.md#composition-and-registry-r26-18)
describes execution and the remaining boundary.

### Source review and authorized decision

- Searched the exact phrase and C84(5) recovery terminology, including election
  instructions, candidate exhaustion, parliamentary material and interpretations.
  The [2017 Senate dossier 548 comparison](https://www.parlamento.it/show-doc?id=1046530&leg=17&part=dossier_dossier1-sezione_sezione11-h2_h21&tipodoc=DOSSIER)
  confirms that the wording predates the reform. The [Camera dossier of 26 June 2026](https://documenti.camera.it/leg19/dossier/testi/AC0469b.htm)
  discusses member-list decimals, but its summary does not conclusively resolve
  the exact college-selection operation. The [6 October 2021 Senate debate](https://www.parlamento.it/show-doc?id=1313906&leg=18&part=doc_dc-sedetit_vdp-genbl_rv-intervento_durnwalderautsvppattuv&tipodoc=SommComm)
  quotes C84(5) and debates coalition replacements; the cited intervention is not
  an authoritative algorithm specifying this maximum. No conclusive official
  ruling or operational interpretation was located. This is a scoped search
  finding, not proof that no interpretation exists.
- Inspected `R/politiche/scrutinio.R`: `cerca_accettori()` filters candidate-bearing
  list/college rows and sorts unused before used, then descending list decimal.
  `cerca_naz()` filters the requested lists and unused/used tier, then aggregates
  list decimals by constituency with `max`. It never constructs a new coalition
  quotient. This corroborates member-list maximum reasoning in a different
  territorial operation, not empirical proof of C84(5)'s college ordering.
- Crucially, `subentro()` calls the local `cerca_accettori(i)` without forwarding
  `coal`/`pluri`; this is the previously recorded legacy parity boundary. Thus
  real-election output agreement cannot establish this branch's interpretation
  without demonstrated coverage. No assertion was made that the disputed case
  actually occurred in a historical election. The R and legacy TypeScript paths
  remain unchanged, including this known forwarding behavior.
- With inconclusive exact evidence and no established contrary interpretation,
  applied Luca's authorized provisional I10-v1: rank candidate-bearing colleges
  by the largest unused decimal of any admitted member list filed there, then
  the largest already-used decimal. Preserve actual member-list source IDs and
  states; do not sum votes/decimals or invent coalition quotients. Allocation
  records of an exhausted member still contribute to that college's priority;
  eligible candidates in another member list can fill its seat. Then apply
  the normal member-list ordering inside the selected college. Equal college
  priorities use the statutory C84(8) draw. Emit an I10 warning on application,
  retain I10 in the recovery trace and composed policy metadata, and preserve
  independent I07 cross-region warnings and route ordering.

### Mechanical repairs, tests and remaining boundary

- A sole candidate-bearing alternative now bypasses a comparison that cannot
  affect the choice. No missing quotient or percentage is replaced by zero;
  existing evidence is retained and no draw is consumed. Multiple alternatives
  still require all applicable ranking evidence.
- The S17(3) single-college path now receives the existing regional list quotient
  and final state from N19 (standalone) or N21 (coalition member). New scoped IDs
  retain `sourceId` pointing to the original record; no allocation or vote ratio
  is recomputed. Tests include Senate cap states and coalition member records.
- The formerly blocked constructed coalition-shortage election now fills all
  588 domestic seats. Added tests for member maxima conflicting with artificial
  coalition decimals, exhausted member witnesses, unused versus used/consumed
  states, statutory ties and permutation replay, singleton comparisons, exact
  source aliases, warning/trace/policy retention, and direct browser-worker I10
  execution. Existing missing-evidence tests now supply multiple alternatives,
  because a single alternative no longer needs that comparison.
- One distinct problem remains, reproduced with a full valid envelope: an
  admitted list has zero regional entitlement, hence is excluded from the local
  college-allocation denominator. There is no original college/list quotient
  to communicate. Recovery can choose a sole destination (and the single-college
  regional evidence is carried), but comparison between two such destination
  colleges remains `unresolved: missing-recovery-remainder`. Establishing a
  divisor or alternative priority here requires an additional legal/modelling
  decision; this is not the now-resolved absence of a coalition quotient. The
  regression preserves this distinction and all seat entitlements. The same
  principle preserves nulls for zero-capacity allocation paths. Recorded the
  remaining comparison as a precondition for unrestricted candidate acceptance.
- Verification: `npm run check:data` and `npm run check` passed (zero diagnostics);
  `npm run test` passed **775 tests in 49 files**, with no skips; the static build
  passed; **9 Chromium browser tests** passed, including I10 recovery and policy
  warnings in a bundled module worker. Build/e2e ran sequentially. No production
  generation, UI selector, source data, generated defaults or deployment changed.
  Changes are left uncommitted.

## 2026-10-19: provisional I11 excludes zero territorial targets from recovery

Luca explicitly resolved the last edge case with the provisional reading that a
list with zero territorial seat entitlement has no college quotient and cannot
obtain a recovery seat. This supersedes the zero-entitlement limitation recorded
in the preceding I10 follow-up. The current rule is in the
[interpretation register](politics-reform-2026-specification.md#interpretation-register).

- Implemented `I11-v1` before every recovery candidate/priority selection,
  including a sole destination, coalition partners and bonus replacements.
  Admitted zero-target lists also cannot supply an I10 priority witness. Targets
  are the final ordinary list allocations after territorial/cap operations and
  before recovery: Camera constituency, Senate region, and Camera TAA
  proportional allocation. A zero allocation in one college does not exclude a
  list whose territorial target is positive.
- Added explicit nonnegative `ordinarySeats` to the internal recovery admission
  communications. The coordinator obtains these from final constituency/regional
  list and TAA targets, without inferring missing targets as zero. Standalone
  recovery calls must supply the same evidence; malformed/missing counts fail
  before execution. Existing initial ordinary entitlements must have a positive
  territorial list target.
- `InitialRecoveryResult.excludedLists` records excluded territory/list pairs
  with `zero-territorial-entitlement`; recovery and composed policy metadata
  record I11-v1. Final targets remain immutable throughout recovery. Zero-target
  Senate/TAA college aliases are not created, while original territorial
  allocation traces remain available.
- Both full-envelope regressions (one and two zero-target destination colleges)
  now exclude those candidates and terminate with explicit unfilled seats when
  permitted routes exhaust, preserving all 588 domestic entitlements. They no
  longer end in `missing-recovery-remainder` or fill the formerly permitted sole
  zero-target destination. This closes the identified zero-territorial-target
  ambiguity under the authorized provisional policy; genuinely missing evidence
  and undefined arithmetic remain separate failure states.
- Added focused cases for Camera/Senate singleton exclusions despite supplied
  quotients, continuing after an excluded coalition partner, coalition bonus
  replacement, missing/invalid target rejection and TAA zero-target evidence.
  Updated prior stage fixtures to communicate their real territorial totals.
- Verification: `npm run check:data` and `npm run check` passed, with zero
  diagnostics. All **784 unit tests in 49 files** and **9 Chromium browser tests**
  passed, including browser transfer of I11 metadata and exclusions. Playwright's
  static build passed before browser execution. Plans and current API
  documentation now describe the resolved boundary. Legacy law and later
  generator/UI integration remain untouched; changes are left uncommitted.

## 2026-10-19: R26-19 scenarios, historical preparation and generation

Implemented the approved R26-02/03/04 generation boundary after verifying the
existing R26-18 composition and registry. The production selector/worker remains
unavailable for the reform until R26-20. The
[generation guide](politics-reform-2026-generation.md) owns the task map, commands,
contracts and replay instructions; the separate
[protocol](politics-reform-2026-generation-protocol.md) owns diagnostic gates and
replication budgets.

### Preparation and evidence

- Added a pinned eight-source historical/model manifest and non-interactive
  `data:reform:download`, `data:reform:generate`, `data:reform:check` and
  `data:reform:validate` commands on the existing Node/TypeScript acquisition,
  checksumming, geography and immutable-output foundation. Preparation reads no
  legacy R cache and performs no fitting/downloads in the browser.
- The 2022 Camera/Senato domestic sources independently resolve to all **7,904
  municipalities** and **46,120,143 electors** in each chamber. The source list
  fields are direct votes. Old-law uninominal attribution adds **1,027,135**
  candidate-only Camera ballots to **27,069,655** direct ordinary list votes,
  giving **28,096,790**. Senate ordinary figures are **26,601,426 + 963,877 =
  27,565,303**. Separate special archives contribute **53,746 / 557,313** valid
  ballots. Municipal reconstruction conserves both each list's historical
  uninominal total and each municipality's candidate-only margin. Independent
  per-list rounding was replaced with controlled row/column reconciliation.
- Geography joins reject ambiguity/missing rows. Explicit aliases cover changed
  and bilingual names; original college membership resolves ambiguous same-region
  names. Reform submunicipal support uses labelled census-weight estimates.
  Special candidate ballot groups retain their own identities, without invented
  member-list decomposition. Source candidate links supply historical coalition
  metadata for defaults. Older Senate 2018 data are acquired and audited but
  excluded from the central fit because of the 25+ electorate.
- Reconciled ER 2024's **547 roster slots / 532 recorded people**, with verified
  gender and birth metadata. List numbers differ between releases, so joins use
  exact names to establish list identity, then verify filed slots. This also
  resolves the two Rimini spelling/name variants without inferring gender.
  The later regional publication has **644,089** marks against the ministerial
  cache's **644,626**, with identical **1,494,375** list votes. The later coherent
  preference/list release is used for fitting; the **-537** difference and the
  unidentified causes remain in the audit.
- Expanded preferences to all five European 2024 municipal workbooks, with
  recorded roster gender/birth fields and **15,588,458** marks. The flat XLSX
  reader iterates rows without a million-cell object graph, supports only the
  pinned source encoding and rejects formulas/unsupported cells. Complete
  roster and list-budget/footprint checks pass. The fit uses **93,659 positive
  ballot observations**, with **zero invalid-budget/roster exclusions**; known
  zero-ballot observations remain in source reconciliation rather than becoming
  invented intensity observations.
- Equal-election weighting, partial pooling, roster concentration and separate
  within-person local dispersion yield baseline effective intensity
  **0.5501554059**, log-strength sigma **1.5971061015** and local sigma
  **0.3954103197**. Twenty-nine province/region hold-outs exclude all training
  observations sharing the held-out candidate identities. Intensity MAE
  **0.1957805028** and pooled leading-share MAE **0.0592449574** pass the
  predeclared 0.25 transfer-diagnostic gates. Zero/tie and dispersion errors are
  also retained by fold. These are distributional transfer diagnostics, not
  calibration of future named-candidate forecasts.

### Runtime contracts and corrections

- Added strict versioned reform scenario/snapshot/model contracts, independent
  fitted-parameter/profile/default identities, typed persistence and explicit
  opt-in legacy conversion. The legacy parser rejects reform/unknown-algorithm
  envelopes and unsupported future versions instead of silently restoring old-law
  defaults. Existing legacy scenarios, inputs, generator and R-parity behavior
  remain available. Unreconciled manual historical mappings and gender-less old
  candidate templates require explicit resolution during conversion.
- Correspondences normalize splits per historical list, aggregate many-to-one
  mappings and send unmapped support to abstention. Common anchors and explicit
  chamber references follow the approved same-scope precedence, then broader to
  narrower geography. IPF preserves local abstention and filed-support zeros;
  incompatible targets fail explicitly. Dates retain election-day mean-mode
  uncertainty. References, scope, normalizations, source/model versions and seeds
  are transferable and reproducible.
- Vote shocks split a marginal variance budget among shared and chamber-specific
  components rather than adding Senate noise. Future daily scales and joint
  correlations are explicit assumptions; one 2022 paired election cannot identify
  future national forecast covariance. Weaker/central/stronger dependence, gap
  sensitivity and identical/independent benchmarks remain distinguishable.
- Candidate generation constructs seven alternating-gender positions, ordinary
  bonus prefixes, coalition bonus affiliations, special links and compatible
  same-list multiple candidacies, respecting filed templates. Generated identities
  are distinct across chambers; shared ordinary identities retain their underlying
  preference strength. Supplied preferences or explicit missing counts take
  precedence. Supplied component inputs use only direct votes for preferences;
  Camera TAA certificates partition the actual special ballot budget once.
- Synthetic preferences use maximum entropy over valid zero-to-three-name
  combinations and aggregate binomial/multinomial sampling, with constructive
  certificates. Capolista is excluded and multiple preferences contain both
  genders. The central persistence share is derived from the fitted variance
  components. Review corrected a confound in the initial implementation:
  persistence now partitions a fixed fitted total variance instead of also
  changing marginal concentration. A dedicated 1,000-seed variance check guards it.
- Entity-addressed streams isolate shared/chamber vote components, candidates,
  person/local preference strengths, combination counts and legal draws. R26-18's
  default attribution stream is now isolated from downstream legal draws too.
  Complete representation-equivalence checks cover 30/42/45/50/60 majority cases,
  special ballots and a tied coalition attribution with identical downstream
  results. This establishes tested arithmetic/representation equivalence, not
  empirical absence of all unobserved behavioral bias.

### Reproduction, limits and verification

- Accepted catalog versions: snapshot **`r26-generation-v1-c5c4be9fafb5`**, models
  **`r26-models-v1-f71a72020aaa`**, preference parameters
  **`r26-preferences-fit-v1-cf0024e3b8e3`**, paired parameters
  **`r26-paired-2022-assumptions-v1-31896d7dffaa`**, defaults
  **`r26-defaults-v1-f204e831aad3`**. Runtime size is **992,943 compressed bytes /
  13,870,007 uncompressed bytes**. The generated catalog records both hashes and
  the defaults fingerprint. Only the current runtime gzip is present in static
  assets; exploratory versions were removed from runtime/source publication paths.
- Acquired all geography/model sources into an initially empty
  `data/derived/r26-clean-sources` directory, then generated into a separate empty
  output directory with the same commands. This checks cache-miss reproduction
  locally; it is not a hosted Linux/Actions measurement or a fresh `npm ci` clone.
- The first artificial twenty-region preference-conditioning experiment hit
  the already documented N18 capacity blocker. Its 700 non-complete runs were
  rejected as evidence of complete legal propagation, without changing the law
  or rescaling endowments. The corrected conditioning uses the independently
  worked complete election fixture, and its TAA budget is the actual special
  ballots rather than the redundant proportional view. Detailed failed/final
  experiments remain separate in `data/derived/`.
- Browser execution initially exposed Node 24's JSON import-attribute requirement
  in the test runner; added `with { type: 'json' }` to the tracked seat import.
  No UI route or application algorithm selector was activated.
- N18 non-computability remains a legal/input boundary for some generated
  scenarios. R26-20 must report it and retain the requested-run denominator,
  without rerolling or silently dropping failures. Synthetic probabilities are
  conditional; pairing/triple behavior and future chamber covariance remain
  assumptions. Whole-application acceptance, performance and publication remain
  R26-21 and the final migration gate.

Final verification on Windows x64, Node **24.20.0**:

- Empty-cache acquisition/generation produced **byte-identical snapshot, model
  and default JSON** against normal preparation. The actual production check
  verifies gzip/uncompressed/default fingerprints and completes all **41 legal
  stage entries**, filling **588/588 domestic seats**, with no unfilled,
  unresolved or non-computable output for the default seed.
- `data:reform:validate` on the final model ran **100 paired full legal runs for
  each of seven preference profiles**. All **700** runs filled 588 seats, with
  zero entitlement changes and zero non-computable stages. Compared with central,
  mean changed candidate identities were **5.25 concentrated, 6.20 dispersed,
  178.71 weak persistence, 6.97 strong persistence, 4.43 no triples and 4.51 enhanced
  triples**. Mean recovery activity varied from 97.50 to 97.54 operations. These
  are conditional worked-fixture diagnostics, not real-candidate probabilities.
- Paired vote diagnostics used **1,000 seeds per profile/benchmark**. Measured
  normalized correlations were **0.4989 / 0.8497 / 0.9802** for assumed
  **0.50 / 0.85 / 0.98** dependence. Marginal variance/mean gates and integer
  conservation passed. In the constructed near-42% example, conditional joint-42
  frequencies were **0.283 / 0.317 / 0.322**, with Monte Carlo standard errors
  about **0.014–0.015**. Zero/doubled gap frequencies were **0.415 / 0.173**;
  identical/independent benchmarks were **0.502 / 0.254**. This is model sensitivity,
  not a measured national bonus forecast or a confidence interval. The final
  versioned detailed report is
  `data/derived/r26-models-v1-f71a72020aaa-validation-v2.json`.
- `npm run check:data` and `npm run check` passed with **zero diagnostics**.
  `npm run test` passed **815 tests in 52 files**, without skips, in **7.83 s**;
  this includes old-law golden, registry and production-data checks. The static
  build passed in about **4.06 s**. Build and e2e ran sequentially.
- Final Chromium suite passed **10/10**, including Node/browser-worker replay of
  generated scenarios, direct registry execution and I10/I11 recovery evidence.
  An earlier full browser run had a single landing-heading timeout, which passed
  in isolation and on subsequent full runs; no unrelated UI change was made.
- R26-19 is complete within these explicit model/legal limits. R26-20/21 remain
  open. No commit, deployment, hosted-runner acceptance or whole-migration
  completion is claimed.

## 2026-10-20 — N18 ordinary corrections with unequal local totals

Luca approved applying C83-bis(1)(b)(3) as independent subtraction of excess
list seats and assignment of missing list seats in the ordinary Camera branch
(no bonus or bonus without cap). This supersedes the blanket N18 local-capacity
blocker documented under R26-13. The controlling amendment, law 180/2026,
article 1(23), was rechecked against the
[promulgated text](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1).
The separate cap-group branch retains `constituency-capacity-target-mismatch`:
C83-bis(1-bis)(c)'s same-college transfers cannot reconcile unequal totals.

- Added an explicit `independent-colleges` matrix correction mode, restricted
  to Camera N18 list quotas. Initial integer quotas still use the supplied
  decree endowments. Remove excess seats by ascending used donor decimals,
  then assign missing seats by descending unused recipient decimals; preserve
  single-use remainder states and I03 draws for unspecified equal ranks.
  Other matrix modes retain their capacity-equality checks and paired behavior.
- Ordinary N18 now accepts different initial and communicated local totals,
  while checking preservation of national list entitlements. Corrections record
  genuine one-sided operations: removals have a null recipient and additions
  have a null donor, including in composed `SeatTransferTrace` results. Recovery
  continues to receive the exact final remainder values and states.
- Retained arithmetic, impossible-residual and no-eligible-donor/recipient
  blockers without publishing final college targets. The earlier `(2,2,3)`
  cross-constituency fixture still blocks in X with
  `no-eligible-donor-remainder`: its excess consists of integer seats.
  No integer-seat fallback, table rescaling or reroll was introduced.
- Added an independently worked N15–N18 regression with endowments `(4,2,4)`
  and corrected constituency totals `(3,3,4)`. Independent college removal and
  addition complete the allocation, preserving national list targets `(3,4,3)`.
  Separate tests cover the two-seat `60/40` example reaching targets `(2,1)`,
  removal without a recipient, both ordinary national paths, cap blocking,
  exhausted remainder pools, trace export, recovery evidence and seeded replay.
- Updated the operational plan, normative specification, legal-core README and
  generation limits to distinguish ordinary correction failures from the cap
  gap. An initial impossible-residual test fixture actually allowed residual
  allocation and reached a correction tie; replaced it with the independently
  checked exhausted-pool case. No production source or generated data changed.

Verification on Windows x64, Node **24.20.0**, npm **11.19.0**, from `web/`:

- Focused territorial suite: **55 tests passed** in two files.
- `npm run check` and `npm run check:data`: **zero diagnostics**.
- `npm run test`: **828 tests passed in 52 files**, no skips, **12.21 s**.
- `npm run build`: static build passed, **15.02 s** reported for the full Vite
  build; plugin-timing notices were informational. Build and e2e ran sequentially.
- `npm run test:e2e -- --reporter=line`: **10 Chromium tests passed, 19.9 s**,
  including reform generation and full scrutiny replay in browser workers.
- `git diff --check`: passed. No commit or publication performed.


## 2026-10-20: R26-20 worker, results and application integration

Implemented only R26-20. The common registry now exposes both algorithms to the
production worker and selector. Legacy politics remains the default; reform
scenarios have separate storage and are never inferred from an old saved file.
The [application guide](politics-reform-2026-application.md) is the focused entry
point for UI/worker/export work; legal and generation guides retain their scopes.

- Reform generation prepares references once and sends each globally numbered
  paired Camera/Senato simulation through the existing common scrutiny entry
  point, retaining 50-run chunk boundaries. Old-law generation keeps its random
  streams; both legacy chambers now share the same global envelope ID. Optional
  wrapper instrumentation preserves measured per-chamber diagnostic times
  without adding nondeterministic fields to replayable legal results.
- Added typed presentation tables for bonus eligibility/reasons, national and
  territorial entitlements, cap effects, special seats, candidate outcomes,
  entitlement/election territories, admission, draws, recovery and I11 exclusions.
  Conditions, model/profile/source/conversion provenance, geographic reference
  resolution, census/decree messages and applied interpretations remain
  inspectable. Repeated metadata/warnings are deduplicated with occurrence counts.
  Chart structures derive from the table contract and display political names.
- Complete, partial, domain-blocked and execution-failed runs remain in the
  requested denominator. N18 cap-capacity and ordinary no-progress failures
  produce explicit errors/stage rows; there is no reroll, legal rescaling or
  successful-run normalization. Candidate/profile conclusions are explicitly
  conditional; dependence-profile sensitivity is separate from calibrated
  uncertainty and does not add cross-law comparison UI.
- The new reform viewer renders one selected simulation/chamber plot, one
  selected detail table with at most 50 rows and at most ten warnings. Existing
  legacy/regional plot and table visibility is preserved. Reform has no arbitrary
  run cap. Legacy retains 1,000 because its simultaneous scatter plots create
  one SVG element per subject/run; exceeding it now fails instead of silently
  truncating. Regional aggregate charts have no equivalent structural cap.
  Exported/stored reform detail still grows with run count; R26-21 must measure
  large-run time and memory rather than infer performance acceptance here.
- Generic schema-v2 columnar exports preserve the executed scenario, worker
  result, algorithm/model/data identities and seed, including after draft edits.
  Reform imports check algorithm/seed/data agreement; old exports without an
  algorithm explicitly resolve to the legacy ID. Warning chamber/interpretation/
  seat/draw context survives roundtrip. Imports route to the appropriate law;
  full reform scenarios expose supplied input and explicit conversion provenance.
- `BASE_PATH` controls navigation, worker and data URLs. Framework base resolution
  stays in routes and crosses the worker request boundary: importing `$app/paths`
  inside the worker initially failed to bundle unsupported SvelteKit client
  dependencies. The shared runtime loader verifies pinned gzip/decoded hashes.
  Vite preview serves `.json.gz` as HTTP `Content-Encoding: gzip`, so Fetch has
  already decoded it; that path verifies the pinned decoded digest directly.
  Raw gzip assets verify both compressed and decoded digests. Missing production
  inputs fail; the old politics debug fallback was removed. Defaults/models
  follow the regenerated catalog rather than hard-coded immutable filenames.

Verification on Windows x64 / Node 24.20.0, from `web/`, with the existing required
production artifacts available (no source/data regeneration or publication):

- `npm run check:data` and `npm run check`: zero diagnostics.
- `npm run test`: **832 tests passed in 54 files**, no skips, **12.15 s**; subsequent
  focused presentation/export/registry checks also passed after final refinements.
- Root static builds passed; build and browser execution ran sequentially.
- Full Chromium regression: **13 passed**, no skips, **34.3 s**, covering old-law,
  regional, reform production UI/replay/imports, explicit missing input failure,
  and a constructed-bundle compilation of the actual production worker across
  simulation IDs 1/50/51. The constructed worker test needs no ignored artifacts.
- Project-base `/elezioni/` regression: **5 passed**, no skips; both politics laws,
  regional simulation, navigation and import/replay/export were exercised.
- Svelte MCP checked the modified components; the new component has no reported
  issues. Existing large pages retain advisory unkeyed-loop/style suggestions;
  no unrelated rendering refactor was performed. Visually inspected the new
  workspace screenshot; changed chart labels from internal IDs to political names.
- `git diff --check` passed. R26-20 is checked in both plans; R26-21 and the final
  whole-migration publication gate remain open. No commit or deployment performed.


## 2026-10-20: R26-20 bounded reform results after browser clone exhaustion

The reported 1000-run `DedicatedWorkerGlobalScope.postMessage` failure came from
cloning the complete accumulated presentation result. Pagination bounded the DOM
but did not bound worker retention or structured-clone cost. A production-default
50-run diagnostic contained 112,553 presentation rows and about 121.8 MiB of row
JSON, including 77.5 MiB of candidate-recovery traces. Export/import also created
whole-result copies and whole-file strings. The failure did not justify changing
legal calculations, discarding traces or imposing a reform run-count cap.

- Added IndexedDB result sessions and gzip row chunks, targeted at 256 KiB or
  256 rows with an explicit 16 MiB individual-record guard. The reform worker
  persists and releases each run; compact manifests and selected page/run queries
  replace the full-result worker message. Shared context is stored once with
  occurrence-count vectors. No existing plot/table visibility was reduced.
- Run manifests act as commit markers. Partially written chunks remain invisible
  after quota/write failure. Cancellation and a closed worker preserve published
  runs as interrupted sessions, retaining the original denominator and global
  simulation IDs. Recovery never rerolls, rescales endowments or changes a legal
  failure into a successful run. The existing seven-slot-slate validation failure
  in production-default simulation 19 remains an explicit non-computable run.
- Added same-tab session restoration and owner-scoped deletion. JSON.gz/CSV
  exports stream stored rows into OPFS files and send a disk-backed File to the
  UI; downloads retain their temporary source until session deletion. OPFS is
  required for this large-file export path. Compressed-chunk bytes shown by the
  UI exclude metadata, indexes and temporary export files.
- Plain/gzip schema-v2 imports parse incrementally into isolated staging
  sessions. Existing identity validation and legacy routing remain in force.
  Invalid imports clean only their staging session; arbitrary table-field order,
  split UTF-8, escaping and column widths are covered. The synchronous generic
  export API rejects storage references to prevent accidental full-result copies.
- Initial per-run metadata lookups were unnecessarily slow. A calculation-scoped
  dictionary of distinct shared keys plus persisted count vectors removes those
  repeated IndexedDB lookups without retaining simulation details. Scrutiny and
  generation algorithms, RNG streams and source/generated data were unchanged.
- Added `npm run test:results` as a separate long Chromium check using actual
  production bundles, detailed queries, full gzip export/import/re-export and
  post-GC UI/worker heap samples. Its temporary persistent profile is isolated
  from ordinary browser tests. R26-21 remains open: these samples do not measure
  peak process memory or replace its generation/scrutiny/presentation benchmarks.

Verification on Windows x64 / Node 24.20.0 / Chromium 148.0.7778.96, with all
required existing production artifacts available:

- `npm run check:data` and `npm run check`: zero diagnostics.
- `npm run test`: **835 tests passed in 55 files**, no skips, **7.95 s**.
  Browser storage tests additionally cover real IndexedDB, cross-chunk pages,
  shared metadata counts, quota isolation, malformed staging cleanup and
  out-of-order JSON fields. Constructed production-worker tests cover seeded
  replay, IDs 1/50/51, cancellation and cold recovery without large artifacts.
- Processed and saved **1000/1000** production-default runs through the UI in
  **714.344 s** of worker time (11 min 54 s). Outcomes were **698 complete,
  302 non-computable**, with 468 bonuses and the unchanged requested denominator
  of 1000. These existing legal/model execution paths remain visible; this work
  does not establish that every requested scrutiny succeeds.
- Post-GC calculation samples at 100 through 900 runs showed **4.70–4.85 MiB**
  retained UI JavaScript heap and **47.88–76.91 MiB** retained worker heap, with
  no sustained increase. After archive roundtrip the sample was **6.32 MiB UI /
  0.94 MiB worker**. These are retained-heap samples, not peak browser/process RAM.
- Full export contained **2,144,543,961 uncompressed bytes**, compressed to
  **83,637,510 bytes**. Export took about **53 s**. Import/re-export preserved
  every byte after export-timestamp normalization; SHA-256:
  `f7537fe10b347988b98cd18f22f477f39e3fb3cfb9483092e1a0bc438ecc353d`.
  The complete archive passed another import/export cycle and detail queries.
  No browser page errors occurred.
- The first long test used `innerText` as an expected `textContent` assertion,
  causing a formatting-only failure after successful calculation/export/import.
  Corrected it to wait for changed rows and compare the same text representation
  by hash. Verification resumed from the saved archive without rerunning draws;
  both complete hashes matched. A stale `imported` status made one import timer
  invalid; its duration is omitted, and the test now waits for a new session ID.
  The [small evidence report](../test/fixtures/benchmarks/browser_reform_results_1000.json)
  records versions, seed, counts, samples, hashes and these measurement limits.
- Full root Chromium regression: **15 passed**, no skips, **22.8 s**. Project-base
  `/elezioni/` regression: **5 passed**, no skips, **15.9 s**, covering both
  politics laws, the completed regional workflow and navigation. Static builds
  and browser suites ran sequentially. Restored the default root build afterward.
- Svelte MCP reported no component issues; asynchronous worker queries account
  for its effect suggestions. Type checks are clean. Visually inspected the
  workspace, including the local-session controls and unchanged chart/table scope.
- Final repeated-import/session-change regression: **2 focused workspace tests
  passed, 16.0 s**, after correcting the test selector's accessible-name match.
- `git diff --check` passed. R26-21 and the whole-migration publication gate remain
  open. No commit, source/data regeneration or deployment was performed.

## 2026-10-20: R26-20a bounded parallel reform execution and worker choice

Completed the approved pre-R26-21 parallelization, including Luca's requested
resource-based maximum/default and explicit user choice. Operational contracts,
the capacity formula and benchmark command are in the
[application guide](politics-reform-2026-application.md#parallel-execution).

- Added a compute task for one complete global simulation ID, keeping joint
  chambers together and single renewals single. Generation, legal algorithms,
  RNG namespaces and model/source bundles are unchanged. Each compute worker
  loads and verifies its own production bundle and prepares its references once.
- Added a bounded pool with one outstanding simulation per worker. A single
  result-service coordinator drains and durably commits ascending IDs, reusing a
  slot only after commit. It receives pull-driven transferable columnar buffers
  targeted at 256 KiB/256 rows, retaining the existing 16 MiB record guard.
  Shared context is emitted once by simulation 1; presentation helpers aggregate
  per-run counters. There is no full-result message or growing reorder queue.
- Cancellation finishes the already assigned contiguous prefix and stops new
  dispatch. Compute/native-worker/transport failures interrupt the session;
  quota/write failures preserve the last published manifest and hide incomplete
  chunks. Neither path rerolls or converts infrastructure failure into a legal
  non-computable run. Pools close on completion/failure; a browser test also
  verifies that closing their coordinator leaves no compute descendants.
- Added **Processi di calcolo**, from 1 to the estimated maximum, initially
  selecting that maximum. The preference is saved separately from the scenario
  and clamped to current capacity on reload. Actual pool size also respects the
  requested simulation count. UI and coordinator share strict count validation.
  Archives preserve optional chosen/actual/maximum counts, memory-estimate
  identity and accumulated phase/storage durations; older archives remain valid.

Memory estimation and scope:

- A production-data Node 24.20.0 probe of IDs 1–20 sampled after generation,
  scrutiny, presentation and encoding, without forced GC, while retaining raw
  and prepared snapshots. Largest observed used heap: **929,540,912 bytes**;
  process RSS: **1,147,371,520 bytes**. This includes Node/loader costs and is a
  stage-boundary observation, not an absolute peak or browser measurement.
- Adopted versioned **1,536 MiB per compute worker**, plus **512 MiB for the
  coordinator**, using half of reported device memory and reserving one reported
  logical processor. Missing/invalid hints conservatively permit one worker.
  Reported memory is approximate total RAM, not available RAM. The UI explains
  the estimate and permits choosing fewer workers; there is no fixed four-worker
  cap. Native Chromium hints on this machine give a maximum/default of **10**,
  exercised by the production default-pool browser test.
- The production matrix's largest sampled compute-worker heap was **265.33 MiB**;
  its largest combined coordinator/compute heap sample was about **859.59 MiB**
  with four workers. Sampling was periodic and unforced; these values are not
  absolute peaks, retained-after-GC measurements or process RAM. Retained-heap
  samples alone were not used to reduce the conservative resource budget.

Production performance verification on Windows x64 / Node 24.20.0 /
Chromium 148.0.7778.96 / Intel i7-14700KF / 63.79 GiB physical RAM, using
`politiche-reform-2026-v1`, snapshot `r26-generation-v1-c5c4be9fafb5`, model
`r26-models-v1-f71a72020aaa` and seed `r26-default-v1`:

| Requested simulations | 1 worker | 2 workers | 4 workers |
| ---: | ---: | ---: | ---: |
| 10 | 16.228 s | 14.439 s | 12.690 s |
| 100 | 110.948 s | 41.310 s | 24.262 s |
| 1000 | 1033.859 s | 380.101 s | 206.564 s |

- `npm run benchmark:reform`: **all nine matrix cases passed**, about 30.7 min
  in the test. Controlled benchmark hints (8 logical processors / 16 GiB) allowed
  1/2/4 workers under a recorded maximum of 5, independently of native privacy
  limits. Each run included new pool setup and production load/verification.
  The single ordered sweep included CDP sampling; these are machine-specific
  comparisons, not portable speedup guarantees or comparisons against another
  instrumentation setup. Four workers were about **5.0 times** faster than the
  matching one-worker 1000-run case.
- Every worker count preserved the same outcome, table-row and warning counts.
  At 1000 requested runs: **698 complete, 302 non-computable, 468 bonuses**.
  Known legal/model failures, including the generation failure at ID 19, remain
  visible with the original denominator. Complete normalized archive content
  (all tables, draws, warning rows and shared metadata counts) also matched in
  constructed browser runs with 1/2/4 workers.
- The [benchmark report](../test/fixtures/benchmarks/browser_reform_workers.json)
  records all cases, phase durations and heap samples. Phase durations use
  `performance.now()` inside workers, may overlap and are not process CPU time
  or additive end-to-end latency. Storage includes decoding, compression and
  durable writes; presentation timing covers row derivation.

Other verification:

- `npm run check:data` and `npm run check`: **zero diagnostics**.
- `npm run test`: **852 tests in 58 files passed**, no skips, 9.22 s in the final
  run. Added capacity/fallback validation, bounded out-of-order completion,
  cancellation, failure/identity handling, chunk transport/presentation parity
  and execution-metadata roundtrip checks.
- `npm run data:reform:check`: verified pinned checksums; production simulation 1
  complete, **41 stages / 588 filled seats**, no missing production data.
- Root and `/elezioni/` static builds and browser suites ran sequentially.
  Final root regression: **20 passed**, no skips, 58.2 s, restoring the default
  root build after project-base verification.
  Project-base regression: **11 passed**, no skips, 44.1 s, including paired and
  default-sized production pools, recovery, native worker crashes, quota failure,
  archive equivalence, legacy selector/navigation and the regional workflow.
- Svelte MCP reported no component issues. Its suggestions concern the existing
  asynchronous page/chart/warning effects. Visually inspected the new control,
  memory explanation and existing results in the project-base screenshot.
- During development, fixed a missing required timestamp in a new test, gave the
  new selector an explicit accessible name, and replaced a Vite-only defaults
  import in the benchmark with reading the catalog-selected JSON file. These
  were test/accessibility setup issues, not changes to legal/model behavior.
- Final `git diff --check`: passed.

R26-20a is complete. R26-21 legal/application acceptance and the whole-migration
publication gate remain open. No commit, source/data regeneration or deployment
was performed.

## 2026-10-20: R26-21 production acceptance and diagnostic frequencies

Implemented the [local acceptance procedure](politics-reform-2026-acceptance.md).
At Luca's direction, R retirement is already decided: removed the active R/browser
comparison and 10x stop rule. Historical R measurements remain dated evidence.
The municipal migration, remaining preparation importers and final hosting/publication
gate remain separate and open.

- Added `npm run acceptance:reform`: requires the legacy politics, regional and
  reform production bundles, records their sizes/SHA-256, checks reform digests
  and its production vertical slice, then runs `check:data`, `check`, the full
  unit suite, root build/e2e and `/elezioni` build/e2e sequentially. Successful
  acceptance restores the root build. Missing bundles, unit/browser skips,
  TODOs, expected failures or retries cannot pass. Ordinary development skips
  remain explicit. Report-validator tests cover those rejection paths.
- Added direct-load, refresh and navigation browser checks. Existing browser
  suites exercise both politics algorithms, regional simulation, worker/data
  loading, candidate output, parallel execution, interruption, detail queries
  and archive replay. The constructed production-worker check also queries
  diagnostic frequencies across 51 simulation IDs without production fixtures.
- Added a streaming post-run `diagnostics` query over the saved presentation
  warning ledger. Errors, warnings and informational notes have separate groups;
  each exact code/message reason retains occurrence counts, distinct affected
  simulations, shared-context occurrences, requested/processed denominators and
  both frequencies. Duplicate notices within a run do not inflate its frequency.
  Distinct reasons can overlap. Session-wide notes have no affected simulation.
  Infrastructure failures remain manifest errors and fail benchmark completion;
  they are not converted into legal non-computability. Query/deletion takes
  place after the measured execution interval. No legal/model logic changed.
- Extended the production browser benchmark with those diagnostic groups,
  complete executed scenario/default identity, architecture and an optional
  report destination, preserving the earlier nine-case worker matrix.

Preparation and application verification:

- Regenerated both legacy production snapshots using the existing R exporters
  and regenerated politics defaults, municipality/candidate catalogs and regional
  defaults. The inherited `LC_ALL=C.UTF-8` is unsupported by Windows R and its
  first export corrupted accented college labels. Repeated with
  `LC_ALL=English_United States.utf8`; generated tracked defaults/catalogs matched
  their previous contents. No legacy cache was modified.
- Ran `data:download`, `data:generate`, `data:reform:download` and
  `data:reform:generate -- --publish`. All pinned cached originals verified.
  Regeneration preserved snapshot `r26-generation-v1-c5c4be9fafb5`, model
  `r26-models-v1-f71a72020aaa`, defaults `r26-defaults-v1-f204e831aad3` and census
  table `r26-seats-census-2021-i08-v1-9672c20529b2`. Compressed reform runtime:
  **992,943 bytes**, decoded JSON **13,870,007 bytes**. Held-out preference fit
  passed; no generated defaults were edited by hand.
- `data:reform:validate` completed its calculations but refused to replace an
  existing immutable validation-v2 report. Added an optional `--output` directory
  and reran into `data/derived/r26-21-acceptance/`, preserving the previous file.
  The new run passed **7 preference profiles × 100 complete legal fixtures** and
  **7 chamber profiles/benchmarks × 1000 replicates**, with conserved entitlements
  and no non-computable conditioning fixtures. Comparing the two reports found
  17 differing scalar summaries, all under conditional `candidateChanges`;
  other summaries matched. The earlier report is not asserted to reproduce the
  current implementation. The new report is separate evidence; no model, legal
  behavior or acceptance threshold was changed to match the older counts.
- `npm run acceptance:reform`: **PASS**, **856 unit tests**, **21 root browser
  tests** and **21 `/elezioni` browser tests**, with **zero skips**. Preparation
  and application type checks had zero diagnostics. Static builds and browser
  suites ran sequentially. Production reform simulation 1 completed with
  **41 stages and 588 filled domestic seats**. Detailed JSON reports and input
  digests are under `data/derived/r26-21-acceptance/`; the
  [compact acceptance record](../test/fixtures/benchmarks/reform_application_acceptance.json)
  preserves the passed checks, artifact identities and scope in the repository.

Source refresh on **20 October 2026**:

- Reopened the [controlling GU act](https://www.gazzettaufficiale.it/eli/id/2026/10/09/26G00201/sg),
  its operative article and the [Camera legislative record](https://www.camera.it/leg19/126?idDocumento=2822-B&leg=19).
  The correction labels had no linked correction; targeted official-source
  searches for `26G00201`, errata/rettifica and reform seat decrees found no
  subsequent applicable correction or decree table. This is a dated search,
  not proof that future corrections/decrees cannot appear.
- The law's [publication warning](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticoloDefault/originario?atto.codiceRedazionale=26G00201&atto.dataPubblicazioneGazzetta=2026-10-09&atto.tipoProvvedimento=LEGGE)
  still announces republication with notes on **23 October 2026**; entry into
  force is **24 October 2026**. Both dates are later than this acceptance.
  Retained the current legal transcription and explicitly provisional I08
  census-based allocation. Recheck that republication and any new decree before
  a later release. This acceptance does not claim official-decree support has
  been exercised, whole-chamber foreign-seat support, supplementary elections,
  nationally calibrated individual-candidate forecasts or universal computability.

Production performance on Windows x64 / Node 24.20.0 / Chromium 148.0.7778.96 /
Intel i7-14700KF / 63.79 GiB physical RAM, algorithm `politiche-reform-2026-v1`,
the regenerated versions above and seed `r26-default-v1`:

| Requested | Elapsed, 4 workers | Generation total | Scrutiny total | Presentation total | Storage/coordinator total | Complete | Non-computable |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 10 | 4.873 s | 5.275 s | 2.137 s | 0.042 s | 1.663 s | 8 | 2 |
| 100 | 22.970 s | 49.219 s | 21.064 s | 0.452 s | 6.429 s | 68 | 32 |
| 1000 | 197.351 s | 482.152 s | 199.361 s | 4.116 s | 47.237 s | 698 | 302 |

- Ran `benchmark:reform` with `REFORM_BENCHMARK_WORKERS=4` and
  `REFORM_BENCHMARK_REPORT=../test/fixtures/benchmarks/browser_reform_acceptance.json`:
  **all three sizes passed**, about 3.9 minutes including setup/teardown. This
  is a new four-worker acceptance sweep, not a repeat of the previous full
  nine-case matrix, which remains preserved in its original report. Browser
  regressions still prove complete normalized archive equivalence at 1/2/4
  workers. No R performance comparison or stop rule was applied.
- Controlled browser resource hints were 8 logical processors / 16 GiB,
  maximum 5 workers, selected/actual 4. Each case started a fresh coordinator
  and pool, including production loading/verification/preparation in elapsed time.
  Worker phase totals overlap and are neither additive elapsed time nor measured
  process CPU time. The largest periodic unforced combined worker JavaScript
  heap observation was **844.70 MiB**; the largest individual compute-worker
  observation was **255.81 MiB**. These are sampled maxima, not absolute peaks,
  retained-after-GC heaps or browser process RAM. The existing conservative
  1536 MiB/worker memory budget was retained.
- The [new benchmark report](../test/fixtures/benchmarks/browser_reform_acceptance.json)
  records every exact reason separately at **10, 100 and 1000** runs, alongside
  the executed scenario, phase timings and memory samples. At 1000 requested
  and processed runs, there were **1390 error occurrences in 302 simulations**
  and **2613 warning occurrences in 914 simulations**, including **two shared
  warning contexts**; informational context had **three occurrences**. There
  were **468 bonuses**. No run was rerolled or removed from the denominator.

Errors by exact stage/reason at 1000 requested runs (each listed occurrence is
in a distinct simulation for that row; downstream rows and chambers can overlap):

| Code/stage | Chamber | Reason | Occurrences / affected runs | Frequency / requested |
| --- | --- | --- | ---: | ---: |
| R26_N18_NON_COMPUTABLE | Camera | impossible-residual | 7 | 0.7% |
| R26_N18_NON_COMPUTABLE | Camera | no-eligible-donor-remainder | 151 | 15.1% |
| R26_N18_NON_COMPUTABLE | Camera | no-eligible-recipient-remainder | 1 | 0.1% |
| R26_N21_NON_COMPUTABLE | Senato | no-eligible-donor-remainder | 59 | 5.9% |
| R26_N22_NON_COMPUTABLE | Senato | alternative-allocation-path | 57 | 5.7% |
| R26_N23_NON_COMPUTABLE | Camera | ordinary-allocation-non-computable | 159 | 15.9% |
| R26_N23_NON_COMPUTABLE | Senato | ordinary-allocation-non-computable | 59 | 5.9% |
| R26_N24_NON_COMPUTABLE | Camera | ordinary-allocation-non-computable | 159 | 15.9% |
| R26_N24_NON_COMPUTABLE | Senato | ordinary-allocation-non-computable | 59 | 5.9% |
| R26_N25_NON_COMPUTABLE | Camera | prerequisite-non-computable | 159 | 15.9% |
| R26_N26_NON_COMPUTABLE | Camera | prerequisite-non-computable | 159 | 15.9% |
| R26_N27_NON_COMPUTABLE | Senato | prerequisite-non-computable | 59 | 5.9% |
| R26_N28_NON_COMPUTABLE | Joint | stage-prerequisite-non-computable | 216 | 21.6% |
| R26_RUN_ERROR | Generation/run | Supply a validated seven-slot slate and its college figures | 86 | 8.6% |

Warnings, independently counted:

- `I03_DRAW`: **690 occurrences**, **55 affected runs / 5.5%**.
- `I07_CROSS_REGION_RECOVERY`: **93 occurrences** across **42 distinct exact
  route/disposition reasons**. Every individual reason's occurrences, affected
  runs and frequency are recorded separately in the linked JSON; same-list,
  same-college and coalition recovery, retained versus released, and source/
  destination territories are not merged. Frequencies range from **0.1% to 0.5%**
  per exact reason. Their union must not be inferred by summing them.
- `I08_PROVISIONAL_SEATS`: **1828 occurrences**, **914 affected runs / 91.4%**.
  Both chambers can issue the notice in one run. Generation failures do not
  reach scrutiny and therefore do not emit it.
- `R26_CONDITIONAL_CANDIDATES` and `R26_SEAT_TABLE_3` (Rome boundary-continuity
  assumption): **one shared context occurrence each**, no simulation frequency.
  Informational seat/geography/model notes are recorded separately, not counted
  as warnings. These known model/legal boundaries remain visible in acceptance.

Final safeguards and completion:

- Temporarily moved only the three generated production bundles aside, with
  checked workspace paths and `finally` restoration. The reform suite passed
  **626 tests**, with exactly **one explicit production-bundle skip**. This
  demonstrates that constructed law/model fixtures do not require those files.
  The production acceptance command then failed at its required-data preflight,
  before any suite could turn missing data into an accepted skip. Restored all
  three bundles and reran the full no-skip application gate successfully.
- Diagnostic frequencies require a native per-simulation ledger. Imported
  archives may contain older all-run aggregation with only a first simulation
  ID; the query explicitly rejects them rather than inventing affected-run
  counts. Browser coverage verifies this rejection after archive import.
- R26-21 is complete as **local application/performance acceptance with
  provisional I08 census tables and conditional preference/chamber models**.
  The non-computable branches above are retained limitations, not successful
  allocations. The final hosting/publication, municipal and remaining
  data-preparation steps remain open. No commit, push or deployment was performed.

## 2026-10-20: Compact 1000-run diagnostics CSV by code

Exported [the compact CSV](../test/fixtures/benchmarks/browser_reform_1000_diagnostics_by_code.csv)
with `severity,code,occurrences,simulations`: **10 error codes and 5 warning
codes**, merging all reasons of each code. Occurrences are additive; affected
simulations require a distinct union. The original reason-only report lacked
the IDs needed to obtain those unions safely.

Added tested code-level counters to the existing streaming diagnostic query and
replayed the same production scenario/seed at 1000 runs with four workers.
The replay passed and matched the original overview and every reason's counts
exactly. Enriched the original report with `codes` summaries and explicit replay
provenance, preserving its original timings. The replay's own elapsed time was
217.130 s, recorded separately under `data/derived/r26-21-acceptance/`.
For example, N23 and N24 each have **218 occurrences in 216 distinct simulations**;
I07 cross-region recovery has **93 occurrences in 55 simulations**. Session-wide
context warnings have one occurrence and zero per-run simulation IDs. No legal
logic, source data or draws changed. Focused counter tests and the production
browser replay passed.


## 2026-10-20: Territorial eligibility review and provisional I12

Investigated the benchmark's `R26_RUN_ERROR` with message `Supply a validated
seven-slot slate and its college figures`. Replay of simulation 19 showed
successful generation/validation, followed by an N20 cap transfer to a
special-only Valle d'Aosta group with zero votes and no ordinary slate in
Lombardia. N23 then threw for its missing college slate. The old report's
"Generation/run" category did not establish a generation failure.

Official-source findings and decision:

- Reopened the promulgated GU amendment articles 1 and 2, including
  C18-bis(2-bis), C31(4-bis), S1(4-ter), S9(2), S11(3-bis), the territorial
  allocation rules and S16-bis(1-ter)(b). Filing/ballot/proclamation structure
  supports territorial presence; special-only groups need not file elsewhere.
  Found no decisive official instruction expressly authorizing or excluding
  an absent-list recipient in the precise cap-remainder case. Apply Luca's
  explicitly authorized **provisional I12-v1**. Exact source links, evidence
  versus inference, and the dated search limits are in the
  [I12 source review](politics-reform-2026-specification.md#territorial-eligibility-and-zero-remainders-i12).
- Targeted official GU/Camera/Senato/Interior Ministry/judicial searches did
  not establish either proposed rule that zero votes produce no remainder,
  or that a zero remainder cannot receive a remainder seat. Applied **neither**
  change; no provisional numerical exclusion was authorized.

Implementation:

- Added one filed-slate coverage helper. A list needs an ordinary filed slate
  in the Camera constituency/Senate region; a coalition needs at least one
  filed member, and internal list splits use each member's own coverage.
  National admission, special votes and national bonus eligibility are retained.
- Camera N15/N16/N17 matrices carry explicit cell eligibility. Absent cells
  cannot receive initial or corrective seats; their absence is distinct from
  a filed zero-vote cell. Senate N19 supplies explicit regional subject
  eligibility; N20 filters both unused-remainder and regional-vote recipients,
  and N21 restricts coalition-list remainder eligibility. Keep zero targets
  in complete communications. A region with no permissible recipient produces
  `no-eligible-recipient` before any unpaired seat change.
- Composed results retain `I12-v1`; chambers with admitted lists absent from
  ordinary territories emit `I12_TERRITORIAL_ELIGIBILITY`. Candidate exhaustion,
  I07/I10/I11 recovery, special allocation and partial college coverage inside
  an otherwise filed territory keep their existing semantics.
- Added ten constructed regressions, including an admitted special-only Senate
  group through full 588-seat scrutiny, coalition/standalone Camera corrections,
  no-recipient failure and both filed-zero-vote and positive-vote/zero-remainder
  recipients. Two initial failures concerned an old bundled-seat fixture's
  stale slate college IDs and an exact-object expectation missing the new
  eligibility field; corrected those fixtures. A new synthetic coalition fixture
  initially had only integer-seat donors and correctly blocked; revised its
  inputs to exercise remainder transfers. Type checking also caught two
  readonly fixture assignments, replaced with immutable copies.

Verification:

- `npm run check:data`: pass. `npm run check`: zero errors/warnings.
- `npm test`: **866 tests, 61 files, zero skips**, including the legacy politics
  and regional suites. Production bundles were present.
- `npm run build`: pass. `npm run test:e2e`: **21 passed at root**; repeat with
  `BASE_PATH=/elezioni`: **21 passed**. Build and browser suites ran sequentially;
  no deployment was performed. Color-environment/plugin timing notices did
  not affect successful exits.
- Direct replay of recorded benchmark simulation **19**, unchanged scenario,
  data and seed: **complete, 588 seats, zero non-computable stages**, no ordinary
  entitlement for the special-only group, and the explicit I12 warning.
- Replayed **all 1000 recorded simulation IDs** through generation and composed
  scrutiny in four isolated Node workers, using the recorded scenario,
  `r26-default-v1` seed and digest-verified production bundle. Outcome:
  **823 complete, 0 partial, 177 structured non-computable, 0 exceptions**
  (hence no `R26_RUN_ERROR`, versus 86 in the historical browser report).
  All 1000 runs remained in the denominator. Other legal-stage blockers remain;
  their revised counts are separate evidence, not conclusions about their legal
  cause. There were 1000 explicit I12 warning occurrences. Reproducible runner
  and exact summary are under `data/derived/r26-i12-review-20261020/`
  (`replay.mjs`, `replay-summary.json`), including baseline-report and runner
  digests. This is a deterministic legal-output replay, not a browser benchmark
  or a new latency/memory acceptance measurement.

The historical browser benchmark report remains unchanged. Revised legal output
must not be described as reproducing its old frequencies or timing measurements.

## 2026-10-20: N28 diagnostic investigation and ordinary N18 donor restriction

`R26_N28_NON_COMPUTABLE` is the presentation of the composed N28 stage, not
an independent statutory operation. `scrutiny.ts` marks the result non-computable
when any renewed chamber has no recovery result or a non-computable recovery.
Its `stage-prerequisite-non-computable` reason propagates earlier blockers.
Unfilled/unresolved seats with computable recovery instead produce `partial`.
The historical browser report has 216 N28 occurrences; the previously recorded
post-I12 direct replay has 177. Neither count establishes the legal cause.

Replayed the recorded scenario and seed `r26-default-v1`, digest-verifying the
production snapshot. Current simulation 4 reproduces N18 Camera
`no-eligible-donor-remainder`, followed by N23/N24 ordinary-allocation blockers,
N25/N26 prerequisite blockers and joint N28. Senate recovery completes 196
seats. No bonus is awarded. In Basilicata (`camera:territory:22`), the single
college `camera:P:170001` has four seats and communicated list targets of one
each for M5S, Azione–Italia Viva, AVS and Forza Italia. Their included votes are
88,446 / 45,804 / 28,436 / 7,630: quota 170,316 / 4 = 42,579. Integer seats
are 2/1/0/0; the remainder goes to AVS, producing 2/1/1/0. M5S must lose one
seat and Forza Italia gain one. The code rejects M5S because neither seat was
awarded through a remainder.

Reopened the [controlling article 1(23)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1).
C83-bis(1)(b)(3) ranks donor colleges by the smallest decimal part without
requiring a remainder-awarded donor seat. C83-bis(1-bis)(c), the separate cap
group rule, expressly requires seats obtained through decimal parts. The
ordinary `independent-colleges` predicate imports this additional restriction.
This revises the earlier assumption that ordinary integer-seat donors must
block: this simulation demonstrates an implementation/interpretation restriction,
not an unavoidable failure of literal ordinary b(3). It does not classify every
N18/N21 blocker or settle repeated-removal semantics.

Verification was investigative, with no scrutiny-source change. The runner
under `data/derived/r26-n28-review-20261020/inspect.mjs` saves the input/result
and `observed-summary.json`. Its `--counterfactual` mode bundles an in-memory
change to only the ordinary donor predicate, permitting positive-seat donors
with defined decimals. Simulation 4 then completes all 588 domestic seats,
including Basilicata targets 1/1/1/1; `counterfactual-summary.json` records it.
This is a narrow causal experiment, not an implemented or generally validated
correction, nor a new benchmark. No migration checklist item was completed.

## 2026-10-20: Approved I13 integer-seat donor fallback and legal-gap register

Luca clarified that the strict C83-bis(1)(b)(3) reading requires
remainder-awarded donor seats and classifies their exhaustion as a legal gap.
This supersedes the preceding investigation's classification of the ordinary
restriction as an implementation defect. The separate cap-group rule expressly
has the same restriction. He authorized a warned loose reading for both when
strict donor exhaustion prevents completion, subject to downstream checks.
Reopened article 1(23) of the controlling GU act; no claim is made that the
workaround represents a binding official interpretation or observed office practice.

Implemented provisional I13-v1 only for Camera N18: ordinary independent list
corrections, the b(3) list rule imported inside cap groups, and cap-group
corrections. Exhaust strict remainder donors first, then rank positive-seat
donors by ascending existing decimal. Repeated integer removals are bounded by
the surplus and remaining seats; tied ranks use the existing seeded I03 draw.
Each fallback removal emits `I13_INTEGER_SEAT_REMOVAL`, severity `warning`,
with simulation/chamber and college/subject context. Applied policy metadata
records I13-v1, and exported list-allocation transfers mark I13. Cap-group
movements retain their detailed matrix trace rather than fictitious candidate
seat IDs. No Senate or Camera constituency donor rule changed.

Integer removals preserve original integer/provisional allocation evidence and
the decimal's exact value and used/unused/consumed state. Thus recovery does
not mistake integer-seat removal for consumption of an unused decimal or a
second remainder donation. Strict donations still consume once. Every operation
reduces an excess by one without negative seats; cap-group recipients stay in
the same college and all communicated list/group totals remain binding.
Recipient exhaustion and numerical failures keep their existing blockers.

Created the [legal-gap register](politics-reform-2026-legal-gaps.md), linked from
the transcription, specification, code reading map and active migration plan.
LG01 records ordinary donor exhaustion, LG02 cap-group donor exhaustion, and
LG03 the C3 capacity/upstream constituency-total conflict. LG03 remains blocked
as `constituency-capacity-target-mismatch`; no table resizing or cross-college
cap-group workaround was authorized. The specification and code reading map
now distinguish strict law, the approved workaround and unresolved cases.

Verification:

- Focused constructed/production regressions: **64 passed**. Cover Basilicata's
  exact 42,579 quota, strict-donor priority, repeated integer removals, unchanged
  unused/consumed decimal evidence, cap-group same-college conservation through
  internal list allocation and recovery communications, I13 transfer exports,
  seeded permutation/JSON replay and unchanged LG03/Senate/constituency blockers.
- Production simulation 4 completes **392 Camera + 196 Senate seats**, all 588
  candidate identities unique, with I13 warnings and applied policy metadata.
- `npm run check:data`: pass; `npm run check`: zero errors/warnings.
  `npm test`: **61 files / 872 passed, no skips**. The first full run hit the
  production test's old five-second timeout after adding the second replay;
  set that test's explicit timeout to 30 seconds and the rerun passed.
- `npm run build`: pass. After build completion, `ACCEPTANCE_PREBUILT=1 npm run
  test:e2e`: **21 passed**. No route/component or base-path logic changed.
- Browser production benchmark, 100 simulations / four workers: **passed**,
  25.564 s elapsed, 96 complete / four non-computable. Diagnostics include the
  applied warning: 22 integer removals in 17 simulations. Report:
  `data/derived/r26-i13-review-20261020/browser-100.json`.
  A direct Node replay ran concurrently, so this is browser execution evidence,
  not an isolated latency comparison or replacement acceptance sweep.
- Replayed all **1000 recorded IDs**, unchanged scenario/seed and digest-verified
  production bundle, in four isolated Node workers: **977 complete, 0 partial,
  23 non-computable, 0 exceptions**. The post-I12/pre-I13 replay recorded
  823 complete / 177 non-computable; the net improvement is 154 complete results.
  Recorded **180 I13 removals in 155 simulations**: 130 ordinary independent
  removals and 50 internal cap-list removals. No cap-group fallback was needed
  in this sample; that branch is proved by constructed regressions, including
  downstream internal list allocation and recovery communications.
- Remaining root blockers: Camera N18 has four
  `no-eligible-recipient-remainder` and eight `impossible-residual` results;
  Senate N21 retains ten donor-exhaustion and one recipient-exhaustion result.
  N28 propagates those 23 failures. New reachable recipient blockers do not
  establish a new legal conclusion; no automatic workaround was added for them.
  Exact summary and reproducible runner are under
  `data/derived/r26-i13-review-20261020/` (`replay-summary.json`, `replay.mjs`).

Historical benchmark and post-I12 evidence were preserved. This change adds no
new migration phase, public deployment, commit or publication.

## 2026-10-21: I14 repeated initial Camera list remainder rankings

Luca authorized interpreting C83-bis(1)(b)(2)'s descending decimal ranking as
repeatable when a first pass cannot fill the college. Rechecked the
[promulgated Camera amendment, article 1(23)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1):
b(2) does not expressly prescribe a single traversal; b(3) expressly refers to
unused recipient decimals, and (1-bis)(d) imports the list rule within cap groups.
The earlier diagnosis of benchmark 165 as intrinsically defective depended on
the single-pass reading. I14 is a provisional interpretation, not a new legal
gap; the legal-gap register is unchanged. See the
[operative contract](politics-reform-2026-specification.md#initial-camera-list-remainder-passes-i14).

The compatibility review found no need to redesign inputs, result schemas,
workers, storage, exports or recovery. Existing `provisionalSeats - integerSeats`
counts initial remainder awards; `remainderSeat` records whether any were given.
One local correction change was necessary: a donor decimal that supplied several
initial seats remains `used` after partial removal and becomes `consumed` only
after the last such seat is removed. Transfer traces mark these removals with
I14; I13 integer fallback still starts only after remainder-awarded donors are
exhausted. Recovery already distinguishes unused from already-used evidence and
needs no multiplicity field or recomputed quotient.

Implementation preserves the simultaneous first pass. Additional whole-matrix
passes reuse established rankings and tie outcomes, excluding lists that reached
their constituency target at the pass boundary. Only N18 list matrices may
repeat, including internal cap lists; cap-group indices, Senate stages and
correction-recipient eligibility keep their previous contracts. Empty eligible
pools still block. Each additional college pass emits warning
`I14_REPEATED_REMAINDER_PASS`; composed metadata records `I14-v1`.

Verification on Windows x64, Node **24.20.0**, from `web/`:

- `npm run check` and `npm run check:data`: zero diagnostics.
- `npm run test`: **61 files / 881 tests passed**, **13.51 s**. Coverage includes
  two and more passes, newly reached targets, stable ties/permutations, empty
  pools, multiple donor removals before I13, internal cap-list allocation,
  recovery communications, metadata and JSON serialization.
- `npm run build`: static build passed, **11.68 s** reported; then
  `ACCEPTANCE_PREBUILT=1 npm run test:e2e -- --reporter=line`: **21 Chromium tests
  passed**, **30.3 s**. Build and e2e ran sequentially; plugin-timing/color notices
  were informational.
- Compared all **1,000 recorded benchmark IDs** against an isolated pre-I14
  matrix graph from commit `5925ef42e58e05c0183c17caf0f6e6009869aedf`, using the
  same generated inputs, scenario/seed, digest-verified production snapshot
  `r26-generation-v1-c5c4be9fafb5` and model `r26-models-v1-f71a72020aaa`.
  Results changed from **977 complete / 23 non-computable** to **985 complete /
  15 non-computable**, with **zero partials or exceptions**. Full serialized
  result objects for the other **992 IDs are identical**, including warnings,
  transfers, draws and recovery evidence. This is a direct Node comparison,
  not a browser latency benchmark.
- All eight previous N18 `impossible-residual` IDs (**165, 349, 435, 437, 488,
  511, 567, 673**) complete with **588 domestic seats** and one I14 warning each.
  In 165, Veneto 1's +Europa receives its third seat in the second pass;
  its original decimal reaches recovery marked `used`. The existing composed
  ledger checks preserve seat and elected-candidate uniqueness. Remaining root
  blockers are four Camera recipient-exhaustion and eleven Senate failures.
- `REFORM_BENCHMARK_COUNTS=200 REFORM_BENCHMARK_WORKERS=1,4
  REFORM_BENCHMARK_REPORT=../data/derived/r26-i14-review-20261021/browser-200.json
  npm run benchmark:reform`: **passed**, **181.46 s** with one compute worker
  and **44.69 s** with four. Both runs store **195 complete / 5 non-computable**
  results, including 165's I14 warning, with identical result tables, row counts,
  warning counts and diagnostics. The preceding Node comparison had finished
  before these browser runs. These measurements do not replace the historical
  full acceptance sweep or establish a pre/post performance comparison.

The reproducible comparison and exact summary are in
`data/derived/r26-i14-review-20261021/` (`replay.mjs`, `replay-summary.json`).
Historical reports and the previous investigation artifacts remain unchanged.
No commit, deployment or publication was performed.

## 2026-10-21: N21 donor-exhaustion investigation

Replayed recorded benchmark IDs from 1 until the first current Senate
`no-eligible-donor-remainder`: simulation **31**, seed `r26-default-v1`,
digest-verified snapshot `r26-generation-v1-c5c4be9fafb5`, model
`r26-models-v1-f71a72020aaa`. This is an investigative Node replay, not a new
browser benchmark or a remeasurement of the historical 59 occurrences. The
existing post-I14 1,000-ID comparison records ten current donor-exhaustion
failures; this investigation independently checks one of them.

No bonus is awarded. Campania has 18 ordinary seats. Regional subject allocation
uses integer quota 168,720; coalition splitting yields M5S/+Europa/Azione-IV/
FdI/FI targets **11/3/1/2/1**. PD, AVS and Lega receive zero seats, so S17(4)(a)
excludes their combined 157,332 votes from the college denominators. The two
colleges have capacities 10/8, included votes 1,672,542/1,207,087 and integer
quotas **167,254/150,885**. M5S receives **7+5 integer seats**, already exceeding
its regional target. Reached-target exclusion prevents further remainder seats
but cannot undo this integer surplus. Initial complete college allocations are
M5S/+Europa/Azione-IV/FdI/FI **7/1/0/1/1** and **5/1/0/1/1**.

The first correction removes FI's remainder seat in `senato:P:150002`
(decimal about 0.468734) and gives it to Azione-IV in `senato:P:150001`
(about 0.429138). M5S remains at 12 instead of 11; +Europa remains at two
instead of three. Neither M5S cell was awarded a remainder seat, so the donor
pool is empty. Both +Europa recipient decimals remain unused. No draw occurs.
N21 blocks, followed by Senate N23/N24/N27 and joint N28 prerequisite failures.

Reopened the [controlling Senate amendment, article 2(10)](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1).
S17(4)(c) has the same relevant donor wording as C83-bis(1)(b)(3). Under the
strict remainder-awarded-seat reading adopted for Camera LG01, this is the same
kind of legal gap, not an arithmetic defect in this replay. The ordinary list
wording remains interpretive: reading it as ranking donor colleges regardless
of seat origin would instead make the predicate an implementation restriction.
Do not confuse that alternative with the more explicit S17(5)(c) cap-group rule.
I13 remains expressly scoped to Camera; no Senate workaround was applied and
no new gap-register entry was added during this analysis.

Evidence under `data/derived/r26-n21-review-20261021/`: reproducible `inspect.mjs`,
full `simulation-31.json`, scoped `summary.json`, and `verify.mjs` with independent
BigInt quota/floor/decimal assertions, initial capacity checks, exact transfer
identities and donor/recipient pool checks (`arithmetic-verification.json`).
All assertions passed. No scrutiny implementation changed.

## 2026-10-21: extend I13 to Senate N21/N22

Luca authorized extending I13 to Senate and updating LG01/LG02. The shared
matrix now enables the existing warned fallback for Senate N21 college lists
and N22 cap groups/internal lists. Strict remainder-awarded donors retain
priority; only their exhaustion enables positive-seat donors ranked by ascending
existing decimal, with warned seeded I03 draws for equal ranks. The same I13-v1
warning, transfer and metadata contracts apply. Integer removal preserves the
donor decimal and unused/used/consumed state. Senate list transfers retain their
paired corrections and regional targets; cap-group transfers stay within the
same college and preserve its capacity. Recipient eligibility, Senate regional
and cap stages, Camera constituency rules and Camera-only I14 are unchanged.

Expanded LG01 to S17(4)(c) and its S17(5)(d) import, with simulation 31 as the
worked Senate example. Expanded LG02 to S17(5)(c), backed by constructed Senate
group corrections through internal list allocation/recovery communication.
Updated the operative specification, code reading map and migration-plan
boundary. LG03 remains unresolved. Existing warning-driven metadata and located
transfer exports already support Senate, so no schema or UI change was needed.

Verification:

- `npm run check`: zero errors/warnings; `npm run check:data`: passed.
- `npm test`: **61 files / 885 passed, no skips**. Covers strict-donor priority,
  unused and consumed donor-state preservation, repeated cap-group integer
  removals, nonnegative seats, college capacities, seeded draws, permutation/JSON
  replay, N22 internal list fallback, recovery evidence and located N21 exports.
  A first tiny-vote repeated-removal fixture encountered an unrelated internal
  list arithmetic blocker; replaced its vote figures with a case that verifies
  repeated removals and full internal allocation without relaxing that blocker.
- Production simulation **31** completes **588 domestic seats**, all elected
  identities unique, with a Senate I13 warning, metadata and located transfer.
- `npm run build`: passed; then prebuilt `npm run test:e2e -- --reporter=line`:
  **21 Chromium tests passed**. Build plugin-timing/color notices were informational.
- Compared **all 1,000 recorded IDs** before/after the extension, using the same
  generated inputs and digest-verified snapshot/model/seed. The saved pre-change
  matrix forms an isolated baseline module graph. Results improve from
  **985 complete / 15 non-computable** to **994 complete / 6 non-computable**,
  with zero partials/exceptions. Full serialized results for **990 IDs are
  identical**. Nine IDs complete with 588 seats and one Senate I13 removal each:
  **31, 173, 350, 593, 610, 664, 940, 970, 980**. ID **242** changes from donor
  exhaustion to recipient exhaustion before any fallback transfer; no recipient
  workaround was added. The remaining root blockers are four Camera and two
  Senate recipient-exhaustion cases. No donor-exhaustion blocker remains in this
  sample. Senate cap-group fallback is verified by constructed tests rather
  than claimed as observed in this production sample.
- Production browser check, **100 simulations / four compute workers**:
  **97 complete / 3 non-computable**, all three recipient-exhaustion cases;
  **23 I13 removals in 18 simulations** across both chambers. The Node replay
  ran concurrently, so the 25.802 s browser elapsed time is execution evidence,
  not an isolated performance comparison.

Reproducible baseline/comparison and exact reports are under
`data/derived/r26-i13-senate-review-20261021/` (`baseline-territorial-matrix.ts`,
`replay.mjs`, `replay-summary.json`, `browser-100.json`). Prior investigation
artifacts and historical benchmark reports remain unchanged. No commit,
deployment or publication was performed.

## 2026-10-21: correct N22 stage failure reporting

Luca approved the bounded N22 reporting correction. `scrutiny.ts` previously
passed `alternative-allocation-path` regardless of whether N22 was inactive or
its selected cap allocation had failed. The stage recorder retained that fixed
reason on failure, and presentation/export correctly propagated the wrong text.

N22 now records `not-run / regional-allocation-non-computable` when regional
allocation fails before path selection, and reserves `not-run /
alternative-allocation-path` for a completed regional allocation using another
path. On the selected cap path it records the actual college status, failure
reason and available affected region. Successful N22 records have no reason.
The existing N21 aggregate stage and all allocation/recovery algorithms remain
unchanged; this adds no legal interpretation or gap-register entry.

Verification: extended existing composition tests cover no bonus, bonus without
cap correction, successful cap correction and failed regional prerequisites.
The digest-verified production regression for simulation **242**, seed
`r26-default-v1`, confirms the active N22 minority-list correction still fails
with `no-eligible-recipient-remainder`, now reporting affected region **08**.
Its stage table and error message carry the correct reason, and JSON result
export/import preserves both the reason and region. Historical benchmark
artifacts were not rewritten; no new 1,000-run benchmark is claimed.

`npm run check`: zero errors/warnings. `npm test`: **61 files / 885 passed,
no skips**. `npm run build`: static build passed; plugin-timing notices were
informational. No commit, deployment or publication was performed.

## 2026-10-21: LG03 disjoined Camera cap-group correction (I15)

Luca authorized a provisional solution for `constituency-capacity-target-mismatch`:
apply C83-bis(1-bis)(c) disjoined when the constituency's upstream entitlement
differs from its C3 college total. The controlling article 1(23) text was
rechecked against the Gazzetta Ufficiale on 21 October 2026. Its literal rule
pairs removal with reassignment in the same college; it does not expressly
authorize disjoining or additions from used decimals. I15-v1 therefore records
the approved simulator policy, without claiming established office practice.

Initial group indices/quotas retain C3 capacities. For mismatched Camera N18
cap-group matrices only, remove surplus group seats by ascending used decimal,
retaining I13 integer donor fallback after strict donors exhaust. Independently
fill deficits by descending unused decimal, then one pass over used decimals,
including those used by an earlier addition. Reuse each used decimal at most
once; recheck deficits after each seat. Exhausted pools and undefined arithmetic
remain non-computable. Ties retain seeded I03 resolution. Balanced Camera group
matrices, Senate rules and ordinary/internal list recipient rules are unchanged.

Final college totals may differ from C3, while every upstream group and
constituency/national list target stays binding. Detailed matrices retain
original capacity and final seats; corrected group counts become capacities for
the existing internal list split. Proclamation, multiple elections and recovery
need no algorithm change. Recovery still consumes final list remainders, never
group indices. No decree/endowment input is rewritten and no wire schema
version changes. `I15_DISJOINED_GROUP_CORRECTION` warns once per affected
constituency, survives later failure and supplies applied `I15-v1` metadata.
Disjoined group traces have one null endpoint and I15 metadata; I13 removals
retain I13 metadata. They remain group evidence rather than fictitious
candidate-seat transfers. The specification, legal-gap register, core README
and active plans now describe this boundary.

Verification:

- `npm run check:data` passed; `npm run check` found zero errors/warnings.
- `npm test`: **62 files / 894 passed, no skips**. New tests cover both mismatch
  directions, exact donor/recipient ordering, unused-before-used addition,
  bounded reuse/exhaustion, I13 interaction, deterministic I03 permutations,
  scope guards, JSON traces, internal list/recovery communications and national
  conservation. A tie fixture was corrected to guarantee an actual correction
  tie instead of depending on the initial statutory draw outcome.
- `npm run build` passed; plugin-timing notices were informational.
- `ACCEPTANCE_PREBUILT=1 npm run test:e2e`: **21 passed**.
- Replayed the existing full constructed N18 diagnostic input, not an
  unconditioned simulation: it previously failed LG03, now completes with
  **588 seats / 588 distinct elected candidates**, no non-computable stage and
  both chambers' recovery `complete / all-filled`. National allocation and
  the full upstream constituency-list communication remain byte-identical;
  all final college-list totals match those targets. I15 warns for Veneto 1
  (**11 -> 12**) and Puglia (**21 -> 20**). JSON roundtrip and deterministic
  replay match. Evidence: `data/derived/r26-i15-review-20261021/constructed-summary.json`.

After those tests, ran **1,000 paired simulations** directly in Node against
both an isolated pre-I15 module graph and the changed implementation, using the
saved acceptance scenario, seed `r26-default-v1`, snapshot
`r26-generation-v1-c5c4be9fafb5` and model `r26-models-v1-f71a72020aaa`.
Compressed/uncompressed production bundle SHA-256 checks passed. Saved acceptance
report SHA-256: `bf12f43b8019326203cfb46d6366aa35145d8e0fb37c781cad5f2d0ff4b19373`.
This is a correctness replay, not a new browser performance benchmark.

Both versions produce **994 complete / 0 partial / 6 non-computable**, with
**zero exceptions** and **all 1,000 full serialized results identical**.
Every complete result has 588 seats and 588 distinct elected candidates.
There are **zero LG03/I15 cases in this frequency sample**; the constructed
diagnostic above provides applicability evidence, not a measured failure rate.

Remaining root failures (first failing stage in each chamber):

| Chamber / stage | Reason | Simulations | Frequency / 1,000 | IDs |
| --- | --- | ---: | ---: | --- |
| Camera N18 | `no-eligible-recipient-remainder` | 4 | 0.4% | 42, 80, 598, 792 |
| Senate N21 aggregate / N22 cap path | `no-eligible-recipient-remainder` | 2 | 0.2% | 49, 242 |

Presentation error codes include propagation and overlap; they represent six
failed simulations, not the sum of the rows:

| Code | Occurrences | Affected simulations | Frequency / 1,000 |
| --- | ---: | ---: | ---: |
| `R26_N18_NON_COMPUTABLE` | 4 | 4 | 0.4% |
| `R26_N21_NON_COMPUTABLE` | 2 | 2 | 0.2% |
| `R26_N22_NON_COMPUTABLE` | 2 | 2 | 0.2% |
| `R26_N23_NON_COMPUTABLE` | 6 | 6 | 0.6% |
| `R26_N24_NON_COMPUTABLE` | 6 | 6 | 0.6% |
| `R26_N25_NON_COMPUTABLE` | 4 | 4 | 0.4% |
| `R26_N26_NON_COMPUTABLE` | 4 | 4 | 0.4% |
| `R26_N27_NON_COMPUTABLE` | 2 | 2 | 0.2% |
| `R26_N28_NON_COMPUTABLE` | 6 | 6 | 0.6% |

Simulation warnings, excluding informational notes and session context:

| Code | Occurrences | Affected simulations | Frequency / 1,000 |
| --- | ---: | ---: | ---: |
| `I03_DRAW` | 1 | 1 | 0.1% |
| `I07_CROSS_REGION_RECOVERY` | 99 | 58 | 5.8% |
| `I08_PROVISIONAL_SEATS` | 2,000 | 1,000 | 100% |
| `I12_TERRITORIAL_ELIGIBILITY` | 1,000 | 1,000 | 100% |
| `I13_INTEGER_SEAT_REMOVAL` | 189 | 164 | 16.4% |
| `I14_REPEATED_REMAINDER_PASS` | 8 | 8 | 0.8% |
| `I15_DISJOINED_GROUP_CORRECTION` | 0 | 0 | 0% |

All 1,000 simulations have warnings. Session-only warning context is
`R26_CONDITIONAL_CANDIDATES` and `R26_SEAT_TABLE_3`, one occurrence each and no
simulation frequency. Informational notes remain separate in the complete CSV.

Reproduction source, pre-I15 source copies, exact ID sets and reports are in
`data/derived/r26-i15-review-20261021/`: `replay.mjs`, `report.mjs`,
`replay-summary.json`, `diagnostics-by-code.csv`, `report.md` and the constructed
summary. Generated artifacts are intentionally ignored; no historical benchmark
was overwritten. No commit, deployment or publication was performed.

## 2026-10-21: N18/N21/N22 recipient-exhaustion investigation

Replayed the six remaining recorded failures, Camera **42, 80, 598, 792** and
Senate **49, 242**, with the current implementation and the saved acceptance
scenario. Every run reproduces `no-eligible-recipient-remainder` in the
correction phase. Both Senate roots are internal minority-list allocation on
the active N22 cap path; N21 also reports the aggregate failure. This was a
six-ID diagnostic replay, not a new 1,000-run frequency or browser benchmark.

In every case the deficient list has used every college decimal and still
needs one seat. Its upstream target exceeds the sum of its integer seats plus
one decimal award per college by exactly one: Camera 42/792 PD **2 > 0+1**;
80 PD **4 > 1+2**; 598 PD **5 > 2+2**; Senate 49 PD **4 > 0+3**; 242 M5S
**4 > 1+2**. This impossibility under the single-use recipient reading exists
before correction ordering is chosen. Initial colleges are full in every
failing matrix. I13 can supply donors in 42/792/242, but cannot provide an
unused recipient; I14's initial pass and I15's mismatched cap-group exception
do not cover this condition.

Rechecked promulgated article 1(23), C83-bis(1)(b)(3)/(1-bis)(d), and article
2(10), S17(4)(c)/(5)(d), against the Gazzetta Ufficiale. Both list-correction
rules require an unused recipient decimal without prescribing an exhaustion
fallback. Added **LG04**, with worked Camera 42 and Senate 242 chains and the
boundary of this strict-reading classification. In 242, N19's coalition/M5S/PD
allocation 10/1/0 becomes 6/4/1 through four N20 cap movements; the subsequent
college split cannot give M5S its fourth seat using each decimal once.
No recipient policy or scrutiny implementation changed. The plan retains both
items as open, linking the completed investigation to the pending decision.

Evidence: `data/derived/r26-recipient-review-20261021/inspect.mjs`,
`overview.json`, `summary-{id}.json` and full `simulation-{id}.json` inputs/results.
Compressed and uncompressed SHA-256 checks pass for snapshot
`r26-generation-v1-c5c4be9fafb5`, model `r26-models-v1-f71a72020aaa`, seed
`r26-default-v1`; the saved acceptance report hash remains
`bf12f43b8019326203cfb46d6366aa35145d8e0fb37c781cad5f2d0ff4b19373`.
Independent BigInt checks verify each failing matrix's vote-based quota,
quotient integer/decimal parts and full initial college capacities. Assertions
replay every correction transfer and remainder-state transition, verify the
empty recipient pools and the single-use upper bounds, and confirm identical
full results on a second scrutiny invocation. All checks passed for all six IDs.
No historical benchmark was overwritten; no commit or publication was made.

## 2026-10-21: implement LG04 recipient reuse and unlimited I15 passes

Luca confirmed LG04 and authorized reusing the greatest used recipient decimals
after unused decimals exhaust, without a limit on reuses, including removal of
I15's one-pass limit. Implemented **I16-v1** for Camera N18 and Senate N21/N22
ordinary/internal cap-list correction and updated mismatched Camera cap-group
correction to **I15-v2**. Each descending used-decimal pass visits an eligible
cell once, then restarts if deficits remain; targets are rechecked after every
seat. Unused recipients retain priority even over greater used decimals.
Absent, consumed and undefined decimals remain ineligible.

No conflict was found with allocation or recovery contracts. Each addition
reduces the finite total deficit, and additions stop at each target. A list
receiving correction seats therefore cannot become a donor later in that
matrix. Retain exact decimal values and `used -> used` state without adding
a multiplicity counter or changing the initial integer/provisional evidence.
I13 donor removal, I14 initial passes, balanced group correction and all
constituency/regional/recovery algorithms retain their prior scopes.

Each reused list seat emits `I16_REUSED_RECIPIENT_REMAINDER` with severity
`warning`, list/college/context, pass and seat counts; composed metadata records
I16-v1. I15 retains its per-constituency warning, now describing unlimited
descending passes and recording I15-v2. A transfer combining I13/I14 donor
fallback with I16 reuse preserves its primary donor `interpretationId` and
adds optional `interpretationIds` containing both policies. Located transfer
exports retain that field; existing JSON/CSV presentation needs no special
case or input/result schema-version change. Updated the specification, legal
gap register, scoped README and both active plans.

Verification:

- `npm run check`: zero errors/warnings; `npm run check:data`: passed.
- `npm test`: **62 files / 901 passed, no skips**. Includes unlimited reuse in
  independent and paired N18/N21/N22 corrections, unused-before-used priority,
  repeated descending passes across colleges, immediate target exclusion,
  genuinely empty recipient pools, I13/I16 compound transfers, recovery states,
  seeded I03 permutation/JSON replay and I15 reuse beyond one pass.
- Production regressions for **42, 49, 80, 242, 598, 792** all complete.
  Simulation 242 verifies I13/I16 annotations, final used recipient recovery
  evidence, warning/stage presentation and JSON export/import preservation.
- `npm run build`: passed; plugin-timing notices were informational.
  Prebuilt `ACCEPTANCE_PREBUILT=1 npm run test:e2e -- --reporter=line`:
  **21 Chromium tests passed**. These are functional checks, not performance
  measurements; the direct comparison also ran during part of verification.
- Replayed **all 1,000 saved acceptance IDs** against the changed implementation
  and an isolated pre-I16 module graph. Compressed/uncompressed production
  SHA-256 checks passed; snapshot `r26-generation-v1-c5c4be9fafb5`, model
  `r26-models-v1-f71a72020aaa`, seed `r26-default-v1`, saved acceptance report
  SHA-256 `bf12f43b8019326203cfb46d6366aa35145d8e0fb37c781cad5f2d0ff4b19373`.
  Results improve from **994 complete / 6 non-computable** to **1,000 complete /
  0 non-computable**, with zero partials or exceptions. Full serialized results
  for the other **994 IDs are identical**. Every complete result has 588 seats
  and 588 distinct elected candidates. All changed runs retain byte-equivalent
  upstream national/regional/list communications and satisfy every final matrix
  and territorial list target. I16 emits **seven warnings in six simulations**:
  simulation 49 also encounters and resolves a second minority-list exhaustion
  in Lazio after resolving Lombardia. This is a direct Node correctness replay,
  not a new browser frequency/performance benchmark.
- The existing constructed LG03 full input still completes with 588 distinct
  elected candidates and both recoveries `complete / all-filled`, now recording
  I15-v2. Full deterministic replay and JSON roundtrip assertions pass. The
  constructed group tests separately demonstrate the newly unlimited I15 reuse.

Reproducible baseline/comparison, changed inputs/results and per-run summaries:
`data/derived/r26-i16-review-20261021/` (`replay.mjs`, baseline source copies,
`replay-summary.json`, `verify-constructed.mjs`, `constructed-summary.json`).
Previous investigation artifacts and historical benchmarks remain unchanged.
Closed the listed error investigations in `MIGRATION_PLAN.md`; the publication
gate remains separate. No commit, deployment or publication was performed.

## 2026-10-22 — Reform scenario editor alignment

Implemented the scenario-editor item in `MIGRATION_PLAN.md`, with Luca's decisions:
persist presentation metadata; common editing as the initial workflow; explicit
advanced Camera/Senato scopes; supplied data stays JSON; expose the existing bonus
planner and compatible templates; defer legacy conversion UI; retain mean mode.
The result-table/plot alignment item remains open.

`ReformScenarioEditor` coordinates typed immutable edits and separate subject,
reference, historical-mapping, filing, bonus-link and candidate sections. Lists
and searches, filing/template tables and preview tables use bounded slices;
advanced panels mount only when opened. Extracted the earlier editor's percentage
input for both workflows and reused its grouped editing/search conventions.
Chamber overrides retain precedence after common edits. Structural changes show
dependent-removal counts before application; supplied ballot data requires JSON
reconciliation before structural edits. Invalid drafts do not replace the last
valid saved scenario. Stale JSON drafts cannot overwrite newer form edits.

Reform scenario v2 adds scenario title, list colors and coalition labels/colors.
The strict parser/export path retains valid v1 reform scenarios verbatim; only
editable drafts upgrade explicitly with deterministic presentation defaults.
Existing result sessions keep their executed scenario, and presentation-only
upgrades do not trigger a false changed-scenario notice. Metadata changes leave
legal inputs, generation output and RNG streams identical. No legacy conversion
wizard or alternative share model was added.

Added a compact editor catalog generated/fingerprinted alongside production
defaults, containing municipality/geography labels and historical list metadata
without municipal vote matrices. Regenerated through the existing Node pipeline:
snapshot `r26-generation-v1-c5c4be9fafb5` and model
`r26-models-v1-f71a72020aaa` remained unchanged; defaults are
`r26-defaults-v2-45ca4adeaaa8` and editor catalog is
`r26-editor-v1-72979a99ca58`. The preparation check verifies the editor projection
against the checksum-verified snapshot. Generated artifacts were not hand-edited.

An independent browser worker prepares references and one deterministic generated
input. It previews actual ordinary/bonus/special placements, including bonus
ordinary-prefix affiliations, resolved share references, weighted abstention and
normalization warnings. Templates pin compatible identities without allowing a
new bonus composition/order. Previews invalidate on edits and remain explicitly
conditional on simulation 1: they do not run scrutiny or establish computability
for all simulations.

Verification: `npm run acceptance:reform` passed in this checkout with all required
production bundles: checksum/catalog checks and a complete 41-stage/588-filled-seat
vertical slice; preparation/application type checks (zero Svelte errors/warnings);
909 unit tests, zero skipped; 25 browser tests at `/` and 25 at `/elezioni`, zero
skips/retries; both static builds and final root-build restoration. The eight
new unit tests cover v1/v2 scenario/result archives, unchanged generation, common
and chamber edits, filing/mapping dependencies and bonus-prefix templates. Four
new browser tests cover persistence/export, explicit chamber controls, dependency
review, invalid-draft retention, v1 imports, JSON supplied data, production worker
preview/template compatibility and mobile overflow. Existing result replay,
workers, missing-data handling and legacy/regional paths passed unchanged.
Acceptance evidence: `data/derived/r26-21-acceptance/report.json` and its unit/browser
reports (generated, untracked). `git diff --check` passed. Build chunk-size/plugin
timing notices remain informational; the politics page bundle is about 182 kB
gzip. No new performance claim, commit, deployment or publication is implied.

Browser-test development exposed a selector issue: nested select-label text
includes option text for `getByLabel(..., exact: true)`, even though its accessible
combobox name is correct. Use `getByRole('combobox', { name, exact: true })` for
these controls. Svelte read proxies also require copying JSON trees at editing
boundaries rather than passing proxies to `structuredClone` or worker messages.
The final browser/type/acceptance checks include these corrections.

Operational details and task-oriented code map:
[`politics-reform-2026-editor.md`](politics-reform-2026-editor.md).

## 2026-10-22 — Row-local presence and editor-local chamber controls

Replaced the global common/Camera/Senato switch following Luca's revised UI
request. Coalition and list rows now have Settings2 icon buttons with titles,
expanded-state semantics, hidden details and badges. Coalition details declare
chamber presence; list details combine presence and protected-minority controls.
Name/membership edits affect existing occurrences without resurrecting absent
lists. Removing presence previews dependent removals; restored lists remain
unfiled. The last list occurrence is removed through the trash action.

Coalition declarations persist in optional v2 presentation `chambers`; old files
infer them from member lists, preserving archive/default compatibility. Populated
coalitions present in both chambers acquire reciprocal automatic links; empty
coalition declarations remain editable until membership makes a legal link
possible. Disabling coalition presence unassigns local members. Re-enabling it
restores matching member identities already present in that chamber. Automatic
same-coalition links are identified in the link editor instead of offering an
inconsistent manual unlink action.

References have their own common/Camera/Senato selector; historical mappings have
an independent selector. Coverage is always chamber-specific, with an optional
chamber filter alongside region and college kind, row checkboxes and filtered
bulk actions. Candidate placement already has its own chamber selector. Shared
row sizing and trash controls remain in place.

Added unit regressions for presence restoration, non-resurrection on name edits,
empty declarations, automatic links and serialization, plus browser coverage for
row toggles/badges, protection and independent reference/history/coverage scopes.
Updated the editor guide/application/plan for the revised workflow. A Windows
text-write encoding issue was caught by the production build and corrected to
UTF-8; line endings were normalized after diff checking. Final verification is
recorded below.

Final verification: `npm run acceptance:reform` passed with 911 unit tests and
26 browser tests at each of `/` and `/elezioni`, zero skips/retries; production
checksum/vertical-slice and preparation/application type checks passed (zero
Svelte errors/warnings). Static builds passed and the root build was restored.
`git diff --check` passed. No commit or deployment was performed.

## 2026-10-22 — Reform result tables and plots alignment

Implemented the approved alignment at `MIGRATION_PLAN.md:209`. Reform results
now offer paired mean-elected/valid-vote bars and summary tables, percentage
boxplots, chamber/list and chamber/supporting-subject scatter panels, national
list spinograms, and a searchable college spinogram with final-election and
original-entitlement measures. The pre-2026 adapter uses the extracted shared
renderer and colour helpers; its existing visible charts remain available.
Broader routing/election-kind generalization remains a separate plan item.

New presentation rows retain final personal affiliation and compact national
and college observations before generated inputs/stage objects are released.
List counts include every domestic seat category by final affiliation; coalition
counts preserve legal ownership and never invent member-list bonus quotas.
Independent special winners have an explicit unaffiliated bucket. Local counts
use actual election locations, including bonus recoveries, while original
entitlements remain a separately labelled measure. No legal algorithm, model,
RNG stream, generated snapshot/default or production data was changed.

Per-chamber unknowns remain null. Complete means require full requested
coverage; confirmed elected totals divided by requested simulations are labelled
lower bounds otherwise. Boxplots describe available percentages, scatter panels
report missing pairs, and spinograms retain grey unknown-outcome cells plus
missing-percentage counts. Preferences and candidate outcomes remain conditional.
Calculations and quantile/bin boundaries preserve precision; labels alone round.

The worker builds disposable on-disk series/value indexes and supplies at most
256 observations per request. Canvas draws every available scatter/outlier point
without one DOM node per observation. Panels mount when opened and query near
the viewport; local series indexes are built only when selected. Exact type-7
quantiles use indexed ranks. Canonical summaries need only subject counters,
so cache quota exhaustion cannot interrupt an otherwise complete election.
Owner checks, cache invalidation, failed-build cleanup and deletion after active
queries protect canonical sessions and prevent surviving derived samples.

Schema-v2 JSON.gz/CSV stays generic and streaming. Canonical presentation tables
include capabilities, observations and list summaries; derivative caches never
alter archive content. Older reform archives use only saved evidence, explicitly
limit non-reconstructible affiliations/local percentages, and never rerun draws.
The operational definitions, compatibility limits and scoped code/check map live
in [`politics-reform-2026-results.md`](politics-reform-2026-results.md).

Verification: `npm run acceptance:reform` passed with 915 unit tests and 28 browser
tests at each of `/` and `/elezioni`, zero skips/retries. Production checksums
and the 41-stage/588-filled-seat vertical slice, preparation/application types
(zero errors/warnings), static builds and root-build restoration passed.
The new 1,201-observation browser fixture verifies exact disk quantiles, unknown
outcomes, bounded pages, every chart family, enlargement, local measure/search,
old archives, ownership, cache-quota isolation, rebuild and scoped cleanup.
An asynchronous import/index needs an explicit readiness wait in concurrent
browser suites; exact select lookup uses the combobox role because nested option
text affects `getByLabel`. A precision assertion uses the original arithmetic
value rather than rounding the stored observation to a decimal literal.

The separate `npm run test:results` production check passed: 1,000 stored
simulations, all 1,000 observations drawn in the checked scatter panel, boxplot
and spinogram/enlargement queries, paginated legal detail, and complete streamed
gzip import/re-export with the same content hash. Computation took 114.892 s,
the exercised chart queries 11.830 s, initial export 79.475 s and import 76.728 s
on this Windows/Chromium checkout. The archive contains 2,969,183,961 decoded
bytes / 128,631,873 compressed bytes. Sampled post-GC retained heap peaked at
11.03 MiB in the UI and 20.47 MiB in the coordinator worker, below the existing
32/256 MiB acceptance limits. These are retained JS heaps, not peak process RAM.
There were no page errors. The report and compressed archive are retained under
`data/derived/r26-21-acceptance/results-alignment-1000.json[.gz]`; the archive's
decoded-content SHA-256 is
`2ce0f5db46b706b808eeae935465c652dfce6b45abeeb1f259f0d8977c3c560c`.

The college search/select is constrained to its container for long list/college
labels on mobile; a 390-pixel browser assertion checks its right edge and the
statistics container's overflow. The final acceptance replay passed with the
same 915 unit tests and 28 browser tests at each base, now including these mobile
assertions. The root build was restored and `git diff --check` passed. Build
chunk-size/plugin-timing notices remain informational. No commit, deployment or
publication was performed.
