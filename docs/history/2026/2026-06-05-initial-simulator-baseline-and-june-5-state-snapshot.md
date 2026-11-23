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
