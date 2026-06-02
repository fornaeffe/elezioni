# Agent Notes

This repository is an R simulator for Italian elections at multiple administrative
levels: national/political elections, municipal elections, and Emilia-Romagna
regional elections.

## Migration Principles

- Keep the project clean, clear, and scalable.
- Prefer more work today for a clean, scalable migration over more maintenance
  work tomorrow.
- Architecture may change completely, but business logic must stay the same
  unless Luca explicitly approves a correction.
- Scrutiny functions (`scrutinio...`) are the highest-risk code. Translate them
  with special care and prove equivalence with tests using identical inputs.
- Scrutiny code must stay coherent with the applicable election law. The relevant
  law text should be reported in comments close to the translated logic.
- If the current implementation appears inconsistent with the law text, report
  the suspected mismatch before treating it as intended behavior.

## Current Project Shape

- Main R code is under `R/`.
- The largest and most sensitive files are:
  - `R/politiche/scrutinio.R` (~2448 lines)
  - `R/Emilia-Romagna/scrutinio_ER.R` (~798 lines)
  - `R/comunali/scrutinio_comunali.R` (~366 lines)
  - `R/caricamento_dati.R` (~694 lines)
- Example workflows live in the `.qmd` files at the repository root.
- Scenario inputs are Excel files under `scenari/`.
- `dati/dati.RData` is actually read with `readRDS()` by `carica_dati()`, despite
  the `.RData` extension. Do not use `load()` for that cache file.
- `dati/debug_scrutinio.RData` is a useful politics scrutiny fixture. It contains
  prepared inputs and a stored output for 10 simulations.

## Baseline Timings Measured Locally

Measured on 2026-06-01 with R 4.5.1 at
`C:\Program Files\R\R-4.5.1\bin\Rscript.exe`, 28 detected cores.

- `simula_comunali("Bologna", ..., 1000)`: about 3.0 seconds elapsed.
- Emilia-Romagna regional workflow, 1000 simulations: about 6.8 seconds elapsed.
- `simula_politiche(..., 10)`: about 7.1 seconds elapsed.
- `simula_politiche(..., 100)`: about 20.8 seconds elapsed.

Politics 100-simulation phase breakdown:

- Load filtered cached data: ~0.4 s, ~47 MB object.
- `calcola_collegi`: ~0.1 s, ~3 MB object.
- `calcola_parametri_input`: ~3.2 s, ~8 MB object.
- `genera_candidati`: ~3.3 s, ~33 MB object.
- `genera_voti_politiche`: ~2.4 s, ~25 MB object.
- `esegui_scrutini_politiche`: ~9.3 s, ~37 MB output object.

The 10-simulation debug scrutiny fixture round-trips exactly against the current
`esegui_scrutini_politiche()` output, including the known warning-producing
simulation 4.

## Scrutiny Translation Rules

- Build golden-master tests before translating:
  - Load `dati/debug_scrutinio.RData`.
  - Run current R scrutiny functions.
  - Serialize normalized outputs to stable fixtures.
  - Compare translated outputs column-by-column, including row ordering where it
    is legally meaningful.
- Make all tie-break behavior explicit and reproducible. The current code uses a
  mix of deterministic ordering and random draws for legal "sorteggio" cases.
- Preserve warnings and exceptional cases as test expectations until Luca decides
  whether they are bugs.
- Pay special attention to:
  - thresholds and coalition/list admission rules;
  - D'Hondt and Hare-Niemeyer allocations;
  - uninominal/plurinominal seat reconciliation;
  - candidate exhaustion and subentro behavior;
  - pluricandidature resolution;
  - ballot/runoff handling in municipal elections.

## Known Issues Or Review Points

- `R/politiche/simula.R` states the politics simulation needs at least 16 GB RAM.
  Any browser implementation must avoid materializing large row objects and
  should use typed arrays, chunking, or worker-side columnar data.
- `R/politiche/esegui_scrutini_politiche.R` previously returned
  `candidati_pluri_sim_` with a trailing underscore. This has been renamed to
  `candidati_pluri_sim`; keep the explicit name in typed migration code.
- Municipal simulation currently copies list votes into mayor and runoff vote
  columns and has `TODO simulare ballottaggio`; this is a business-rule gap, not
  just a typing issue.
- Municipal candidate election within lists is marked `TODO !`; current outputs
  allocate seats to lists/coalitions but do not model individual councilor
  candidate ranking.
- Several law comments mention `sorteggio`, but not every politics tie-break path
  appears to use an explicit random draw. Preserve current behavior for golden
  tests first, then review legally with Luca.
- In `R/politiche/scrutinio.R`, the remainder-vote attribution for
  candidate-only uninominal votes says remainders should be ordered descending,
  but the current `order()` call provides six `decreasing` values for five keys.
  R ignores the final `TRUE`, so `RESTO` is effectively sorted ascending. The
  TypeScript port preserves this for parity and marks it `TODO(law-review)`.
- In the politics subentro block, `subentro(livello = "pluri", coal = TRUE)`
  and `subentro(livello = "circ", coal = TRUE)` describe same-coalition
  searches in messages, but the non-national plurinominal-candidate path calls
  `cerca_accettori(i)` without passing `livello` or `coal`. The TypeScript port
  preserves this for parity and marks it `TODO(law-review)`.
- During politics pluricandidature resolution, R leaves `CIFRA_PERCENTUALE` on
  `candidati_pluri`; later `merge()` calls then use it as an implicit join key.
  Uninominal candidates ripescati after that point have `NA` and can fail to
  receive `ELETTI`. This appears accidental but is preserved for parity and
  marked `TODO(law-review)`.
- Politics code has explicit FIX/TODO notes around Valle d'Aosta and
  Trentino-Alto Adige/Senate handling. Treat these as migration review points.

## Migration Architecture Notes

- The front-end option should not be a direct row-object port of `data.table`
  logic. Use a small typed domain model, columnar/typed-array data where needed,
  Web Workers for simulations, and deterministic seeded RNG.
- Svelte pages and components should use Svelte 5 runes-mode best practices:
  prefer `$state` for local mutable state, `$derived` for computed values, and
  avoid updating state inside `$effect` unless there is no clearer lifecycle or
  event-driven alternative.
- A SvelteKit static app can work if it keeps the legal scrutiny core separate
  from UI components and uses workerized computation. It gives the best
  distribution/usability story.
- The web UI should become the preferred way to create and edit scenarios.
  Excel scenarios do not need to remain a first-class migrated input; they carry
  too much malformed-data and typechecking risk for the long-term app.
- Results UI should prioritize user-facing election outputs. Diagnostic tables
  such as `Generated pipeline runs` should be hidden or collapsed by default and
  reachable through an expandable/debug affordance.
- Scenario editor basics belong in this migration: list/coalition editing,
  global list percentage overrides, typed JSON save/load, automatic local
  storage persistence, reset to defaults, and validation before worker runs.
- Scenario defaults should come first from bundled defaults for election kind
  and territory; if absent, use the most voted lists from the last same-kind
  election in the same territory; if past coalition data is unavailable, each
  list defaults to its own coalition.
- Past-to-future list correspondences, location-specific percentage overrides,
  fixed-versus-mean percentage modes, and candidate templates need typed schema
  support and generator hooks. Defer large/rich UI for these advanced settings
  until production previous-election data packaging and the basic scenario model
  are stable.
- The scenario model should allow users to enter only some global percentages;
  unspecified future-list percentages should be recalculated from previous
  election results and list correspondences, preserving the current R model's
  intent.
- Keep scrutiny algorithms modular and swappable. The same normalized data and
  scenario should eventually be runnable through different scrutiny algorithm
  implementations behind a stable interface, for comparison or law-review
  experiments.
- Once at least two scrutiny algorithms exist for the same election kind, the UI
  should support selecting/comparing them. Before then, preserve the architecture
  hook without adding premature visible complexity.
- Data preparation should be migrated after the rest of the simulator. At that
  point, reassess typed Python versus Node/TypeScript. The data-preparation
  system should remain a periodic devops/GitHub Actions pipeline and stay
  agnostic about election kind, producing previous-election data bundles for
  election-specific vote generation pipelines to consume.
- `web/src/lib/politics/scrutiny.ts` is currently allowed to grow while golden
  parity stages are still being discovered. Split it into stage-focused modules
  once the tested boundaries are clear enough that the refactor lowers risk.
- A Python pipeline can use stricter runtime and static validation (`pydantic` or
  dataclasses plus `mypy`/`pyright`) and columnar performance libraries
  (`polars`, `pyarrow`, optionally `numpy`). It is the safer pure-performance
  path, but loses the install-free browser advantage unless paired with a server
  or packaged desktop workflow.
- Prefer a staged migration:
  1. Extract and freeze R golden fixtures.
  2. Define typed schemas for all input/intermediate/output tables.
  3. Port allocation primitives and scrutiny functions with law comments.
  4. Add equivalence tests.
  5. Add simulation generation and UI/reporting last.

## Current Migration State

- `MIGRATION_PLAN.md` is the living migration tracker and should be updated after
  each implementation phase.
- The R politics scrutiny output key has been renamed from
  `candidati_pluri_sim_` to `candidati_pluri_sim`; the debug scrutiny fixture
  still matches exactly after this rename.
- `scripts/export_politics_golden.R` exports the direct politics scrutiny JSON
  fixture used by the TypeScript port. Fixture schema v7 stores R-produced
  trace tables for uninominal election, uninominal list-vote attribution,
  plurinominal/circumscription figures, uninominal candidate percentages,
  circumscription totals, national list/coalition figures, threshold flags, and
  subject-level plus internal list-in-coalition Camera/Senato circumscription
  seat allocation, plus pre-subentro plurinominal seat allocation.
- `scripts/export_politics_worker_snapshot.mjs` derives
  `web/static/data/v1/politics-debug-scrutiny.json` from the golden fixture.
  This compact browser snapshot contains direct scrutiny inputs and contexts
  only, not traces or expected outputs. It is a bridge for worker execution and
  benchmarking until scenario-to-vote generation is ported.
- `scripts/export_politics_adapter_fixture.R` exports
  `test/fixtures/politiche/generated_adapter.json`, the R-style generated
  politics vote/candidate tables consumed by `esegui_scrutini_politiche()`.
  `web/src/lib/politics/adapter.ts` converts those tables into the exact direct
  scrutiny context and per-simulation inputs from the R golden fixture. This
  freezes the deterministic boundary before porting random vote/candidate
  generation.
- `scripts/export_politics_vote_preparation_fixture.R` exports
  `test/fixtures/politiche/vote_preparation.json` for the deterministic
  `prepara_dts()` boundary. `web/src/lib/politics/vote-preparation.ts` ports
  that table preparation logic. The fixture uses debug rows reconstructed from
  prepared list votes plus synthetic `astensione` and invalid-list rows, so it
  protects join/filter behavior but is not a golden fixture for the random
  `genera_voti()` draw itself.
- `scripts/export_vote_generation_fixture.R` exports
  `test/fixtures/core/vote_generation.json`, a small synthetic trace for the
  generic `R/generazione_voti.R::genera_voti()` math. It includes R-produced
  normal draws, and `web/src/lib/core/vote-generation.ts` injects those draws in
  tests to prove formula and row-order parity. Browser production runs use the
  TypeScript seeded normal sampler, not R's RNG stream.
- `scripts/export_politics_vote_generation_fixture.R` exports
  `test/fixtures/politiche/vote_generation.json`, a small synthetic trace for
  `R/politiche/genera_voti.R::genera_voti_politiche()` orchestration. The
  TypeScript port in `web/src/lib/politics/vote-generation.ts` combines the
  generic vote generator with Camera/Senato uninominal aggregation and
  `preparePoliticsVoteTables()`.
- `scripts/export_politics_candidate_generation_fixture.R` exports
  `test/fixtures/politiche/candidate_generation.json`, a small synthetic trace
  for `R/politiche/genera_candidati.R`. The TypeScript port in
  `web/src/lib/politics/candidate-generation.ts` replays R-recorded `sample()`
  outputs in tests and uses a seeded TypeScript sampler in browser runs. The R
  default date for generated candidates is captured from the fixture because
  `as.POSIXct("2000-01-01")` is local-timezone dependent.
- `scripts/export_politics_pipeline_fixture.R` exports
  `test/fixtures/politiche/pipeline.json`, a synthetic composed pipeline trace.
  `web/src/lib/politics/pipeline.ts` combines candidate generation, vote
  generation, `prepara_dts()` parity logic, and direct scrutiny input adaptation
  into the same snapshot shape used by the worker bridge.
- `scripts/export_politics_pipeline_source.R` exports
  `test/fixtures/politiche/pipeline_source_debug.json`, a compact real
  generation-source snapshot derived from `dati/debug_scrutinio.RData`.
  `web/src/lib/politics/pipeline-source.test.ts` builds one seeded browser
  simulation from this source and runs Camera/Senato scrutiny as a smoke test.
  The same script can export
  `web/static/data/v1/politics-pipeline-source-debug.json`, which is now a
  legacy bridge/test artifact.
- `scripts/export_politics_static_snapshot.mjs` derives
  `web/static/data/v1/politics-static-debug.json` from the pipeline-source
  bridge. This remains a fallback/test artifact.
- `scripts/export_politics_static_snapshot.R` exports
  `web/static/data/v1/politics-static.json` from the current R preparation
  path, using `dati/dati.RData` and `scenari/politiche_2027.xlsx`. This is the
  preferred worker input shape: reusable politics data is split from a
  `default_scenario`, then converted back into the internal
  `PoliticsPipelineSource` by `web/src/lib/politics/static-snapshot.ts`.
- `scripts/benchmark_r_workflows.R` reruns R baseline workflows.
- `web/playwright.benchmark.config.ts` and
  `web/tests/benchmarks/politics-worker.spec.ts` benchmark the generated
  politics worker path in Chromium for 10, 100, and 1000 simulations. Run with
  `cd web; npm run benchmark:politics`.
- `web/` contains the initial static SvelteKit app scaffold, strict TypeScript
  setup, worker API types, seeded RNG, allocation primitives, unit tests, and a
  Playwright smoke test.
- `web/src/lib/scenario/politics.ts` owns the current politics scenario JSON
  contract. Schema v2 covers basic scenario metadata, coalitions, lists, colors,
  global starting shares, and `shareOverride`, plus validation and JSON
  parse/serialize helpers. Old schema-v1 JSON without `shareOverride` remains
  accepted and defaults those flags to `false`.
- `web/src/routes/+page.svelte` uses one `scenarioDraft` object for the basic
  web-native editor: scenario metadata, coalition editing, list/share editing,
  validation, reset, JSON import/export, and automatic localStorage persistence.
  Build plain cloned scenario snapshots before posting to the worker. Its
  results panel prioritizes primary summary tables and keeps diagnostic tables
  such as `Generated pipeline runs` behind a details toggle by default.
- `web/src/lib/politics/scenario-projection.ts` is the current boundary between
  the web-native politics scenario and the generated worker source. It matches
  scenario lists to the static snapshot by exact list name, removes source lists
  that are not present in the scenario, applies only explicit global share
  overrides, recalculates non-overridden matched lists proportionally from
  source data, projects matched list coalitions, and warns about unmatched
  scenario lists or placeholder coalition candidates.
- New/unmatched scenario lists cannot yet be simulated by the static-snapshot
  worker path. They are ignored with a warning until list-correspondence
  defaults and future-list generation semantics are implemented.
- Advanced scenario features remain deferred: past-to-future list
  correspondences UI, location-specific percentage overrides, fixed-versus-mean
  modes, and candidate templates/editors. Keep schema/generator hooks ready, but
  do not build large UI for these before production previous-election data
  packaging is stable.
- The TypeScript politics scrutiny port now matches the R golden fixture for
  direct scrutiny output on the debug fixture: uninominal candidate election,
  candidate-only vote attribution to lists, plurinominal/circumscription
  aggregates, uninominal candidate percentages, circumscription totals,
  national list/coalition figures, 1%/3%/10% threshold flags, Camera national
  proportional allocation, Camera/Senato subject-level circumscription seat
  allocation with Camera flipper reconciliation, internal list allocation with
  the second Camera flipper reconciliation, and Camera/Senato plurinominal
  allocation with reconciliation back to circumscription list seats, candidate
  availability, subentro, pluricandidature resolution, and final
  `liste_pluri`/`candidati_uni`/`candidati_pluri` outputs.
- The worker now runs the composed generated politics pipeline on
  `web/static/data/v1/politics-static.json`, then scrutinizes the generated
  Camera/Senato simulations. It falls back to
  `web/static/data/v1/politics-static-debug.json` only if the production bridge
  snapshot is absent. The direct scrutiny and pipeline-source bridge snapshots
  are still useful for tests, but they are no longer the UI worker path.
- The first browser scenario projection matches edited list shares by exact list
  name, preserves the source abstention row, rescales political list
  probabilities into the source model, and recomputes `LOGIT_P`. This is a
  bridge behavior, not the final scenario import/editor contract.
- The generated worker path currently runs in 50-simulation chunks and is capped
  at 1000 simulations. Revisit this if the UI needs larger runs or after
  columnar packaging.
- The generated-worker browser performance gate passed on 2026-06-02 after the
  R-exported production static bridge: Chromium ran 10 politics simulations in
  1.357 s, 100 in 13.194 s, and 1000 in 123.229 s
  (`test/fixtures/benchmarks/browser_politics_worker.json`). Fresh
  same-machine full R politics baselines were 6.20 s for 10 simulations, 19.58 s
  for 100, and 151.96 s for 1000
  (`test/fixtures/benchmarks/r_baseline_politics_10_compare.json`,
  `test/fixtures/benchmarks/r_baseline_politics_100_compare.json`, and
  `test/fixtures/benchmarks/r_baseline_politics_1000_compare.json`). Repeat this
  gate after the future data-preparation migration or major snapshot/schema
  changes.
- In the Camera internal list-in-coalition flipper, the R code decrements the
  recipient list's `SEGGI_ECCEDENTI_CONTATORE` after giving it a seat. This
  appears counterintuitive but is preserved in TypeScript for parity and marked
  `TODO(law-review)`.
- Plurinominal allocation also preserves R stable ordering for equal decimal
  remainders/equal figures where the law comments mention `sorteggio`; this is
  marked `TODO(law-review)` in TypeScript.
- Candidate generation preserves the R edge where an `NA` uninominal candidate
  can be replayed into plurinominal candidate slots when list assignment samples
  an empty right-join row. This is visible in the synthetic pipeline fixture and
  may deserve business review before production data is exposed broadly.
- When posting Svelte `$state` data to workers, derive or build plain snapshots
  first. Svelte proxies are not structured-clone safe.
