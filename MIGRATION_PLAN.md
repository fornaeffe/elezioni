# SvelteKit Migration Plan

Last updated: 2026-06-02

## Summary

The migration target is a SvelteKit static TypeScript app under `web/`, with
simulation and scrutiny running in browser workers. The first migrated workflow
is `politiche`, because it carries the largest legal and performance risk.

The first browser version will use:

- bundled, versioned historical/electoral data snapshots;
- an in-app scenario editor;
- deterministic seeded simulations;
- golden-master tests against the current R implementation;
- a performance gate against the local R baseline.

If the optimized browser politics workflow is 10x or more slower than the same
R baseline on the same machine, pause and present Python fallback options.

## Status

| Phase | Status | Notes |
| --- | --- | --- |
| Migration tracking | Done | `AGENTS.md` and this file are present. |
| R safety fixes | Done | `candidati_pluri_sim_` renamed to `candidati_pluri_sim`; debug scrutiny still matches. |
| Golden-master fixtures | Done | `scripts/export_politics_golden.R` exports schema v7 JSON with direct inputs, final outputs, warnings, and scrutiny trace tables. |
| R benchmarks | Done | `scripts/benchmark_r_workflows.R` added; quick baseline JSON generated. |
| SvelteKit app scaffold | Done | `web/` created with strict TS, static adapter, Vitest, Playwright. |
| TypeScript core | Started | Worker API types, seeded RNG, allocation primitives, fixture loader tests, and unit tests added. |
| Politics scrutiny port | Started | Direct politics scrutiny output matches R for the debug fixture; the worker now scrutinizes generated TypeScript politics simulations. |
| Vote generation | Started | Generic `genera_voti()` math and politics `genera_voti_politiche()` orchestration are ported with R-draw fixture parity and seeded browser normals. |
| Candidate generation | Started | Politics `genera_candidati()` is ported with R `sample()` replay fixture parity and seeded browser sampling. |
| Composed politics pipeline | Started | Candidate generation, vote generation, vote preparation, and direct-scrutiny input adaptation are composed into tested synthetic, real-source, and browser-worker paths. |
| Generated politics input adapter | Started | R-style generated-table fixture and TypeScript adapter now rebuild the exact direct scrutiny inputs/context. `prepara_dts()` vote preparation is also ported. |
| Performance gate | Passed for current politics worker path | Chromium generated-worker benchmark on the production-shaped debug static snapshot is below fresh full R politics baselines for 10, 100, and 1000 simulations; repeat after full production data packaging. |

## Decisions

- Target: SvelteKit-first, not Python-first.
- First vertical slice: politics.
- Scenario UX: the web app is the preferred long-term way to create and edit
  scenarios. Excel scenarios can be abandoned rather than kept as a first-class
  input, because malformed workbook risk and typechecking cost are too high.
- Data source: bundled static snapshots.
- Data-preparation pipeline migration is planned, but only after the rest of
  the simulator migration. When that phase starts, reassess whether typed
  Python or Node/TypeScript is the better fit.
- Data preparation should remain a devops/GitHub Actions workflow, run
  periodically, and stay agnostic about election kind: it should produce a
  bundle of previous-election data consumed by election-specific vote generation
  pipelines.
- Compatibility: business/legal behavior must match R unless Luca approves a correction.
- R TODOs: leave for later unless a fix is trivial and helps migration safety.
- `sorteggio`: implement only when straightforward; otherwise add `TODO(law-review)`.
- Scrutiny implementations should stay modular and swappable behind a stable
  interface, so the same data/scenario can be compared across algorithm
  variants during law review.
- The UI should expose multiple scrutiny algorithms for a given election kind
  once at least two algorithms exist for that same kind. Until then, keep the
  interface/design ready but avoid premature UI complexity.
- Delay splitting `web/src/lib/politics/scrutiny.ts` until the golden-tested
  stage boundaries are clear enough that the refactor reduces risk.

## Scenario Editor Scope

The migrated app should use a web-native scenario model as the primary user
workflow. Excel scenarios can be abandoned. The scenario editor should separate
basic settings, visible immediately, from advanced settings behind an explicit
expander/button.

### Implement During This Migration

- **Basic list/coalition editor**: users can choose the lists present at the
  next election and their coalitions. This is part of the core migration
  because every workflow depends on it.
- **Default list/coalition generation**:
  - first use defaults bundled for the election kind and territory;
  - if absent, use the most voted lists from the last election of the same kind
    in the same territory;
  - if past coalitions are unavailable, default each list to its own coalition.
- **Web-native scenario save/load**: scenarios should be serializable as typed
  JSON, saved/loaded from the UI, and automatically persisted to local storage.
  Add a reset action that restores the bundled/default scenario.
- **Global list percentage editor**: users can specify national/territory-wide
  percentages for any subset of future lists. Unspecified list percentages
  should be recalculated from previous election results and list
  correspondences, preserving the current model's intent.
- **Mean versus fixed percentage mode, at least globally**: the scenario model
  should support whether an entered percentage is a stochastic mean using
  historical variability, or a fixed value. The first UI can expose this as an
  advanced option after the basic global percentage editor is stable.
- **Scenario validation**: validate duplicate list names, missing coalitions,
  invalid percentages, impossible total shares, missing default data, and
  references to unknown past/future lists before posting to the worker.

### Keep Architecture Ready, Defer Full UI

- **Past-to-future list correspondences**: this is essential for default
  percentage calculation and should be present in the scenario/data model.
  However, a rich correspondence matrix editor can wait. Start with bundled
  defaults and homonymous-list fallback; expose a compact advanced editor only
  after production previous-election data packaging is available.
- **Location-specific percentages**: keep typed support for per-location list
  overrides, but defer the full advanced UI. This can become large and hard to
  validate; implementing it before production data packaging would risk
  building the wrong interface.
- **Per-location fixed versus mean mode**: reserve the schema and generator
  hooks, but defer full UI until location overrides are implemented.
- **Candidate names/templates**: keep typed candidate-template support and the
  current generated-candidate fallback. Defer a large candidate editor until the
  politics scenario model and production data snapshot are stable. When added,
  it should allow partial candidate entry for some or all candidate slots and
  leave unspecified slots generated as today.

### Out Of Scope For The Migration Slice

- **Excel scenario import/export as a first-class path**: not worth carrying
  forward because malformed workbook risk and typechecking cost are high.
- **Full data-preparation migration**: defer until the simulator workflows are
  migrated. The future data-preparation pipeline should remain a periodic
  devops/GitHub Actions process and produce election-kind-agnostic historical
  data bundles.

## Local R Baselines

Measured on 2026-06-01 with R 4.5.1 at
`C:\Program Files\R\R-4.5.1\bin\Rscript.exe`, 28 detected cores.

| Workflow | Simulations | Elapsed |
| --- | ---: | ---: |
| Municipal Bologna | 1000 | ~3.0 s |
| Emilia-Romagna regional | 1000 | ~6.8 s |
| Politics | 10 | ~7.1 s |
| Politics | 100 | ~20.8 s |

Politics 100-simulation phase breakdown:

| Phase | Elapsed | Output size |
| --- | ---: | ---: |
| Load filtered cached data | ~0.4 s | ~47 MB |
| `calcola_collegi` | ~0.1 s | ~3 MB |
| `calcola_parametri_input` | ~3.2 s | ~8 MB |
| `carica_candidati` | ~0.1 s | ~0.3 MB |
| `genera_candidati` | ~3.3 s | ~33 MB |
| `genera_voti_politiche` | ~2.4 s | ~25 MB |
| `esegui_scrutini_politiche` | ~9.3 s | ~37 MB |

Fresh comparison baselines run on 2026-06-01/02 after the generated worker path
was wired:

| Workflow | Simulations | Elapsed | Fixture |
| --- | ---: | ---: | --- |
| R full politics workflow | 10 | 6.20 s | `test/fixtures/benchmarks/r_baseline_politics_10_compare.json` |
| R full politics workflow | 100 | 19.58 s | `test/fixtures/benchmarks/r_baseline_politics_100_compare.json` |
| R full politics workflow | 1000 | 151.96 s | `test/fixtures/benchmarks/r_baseline_politics_1000_compare.json` |
| Chromium generated politics worker | 10 | 1.216 s | `test/fixtures/benchmarks/browser_politics_worker.json` |
| Chromium generated politics worker | 100 | 12.220 s | `test/fixtures/benchmarks/browser_politics_worker.json` |
| Chromium generated politics worker | 1000 | 131.616 s | `test/fixtures/benchmarks/browser_politics_worker.json` |

Current gate result: pass for this production-shaped debug static snapshot
worker path. The 1000-simulation browser run is about 0.87x the fresh full R
workflow elapsed time, far below the 10x stop threshold. Repeat this gate after
full production data packaging.

## Implementation Checklist

1. Create the living plan and keep it current.
2. Apply the R naming safety fix and verify the debug scrutiny fixture still
   round-trips.
3. Add scripts that export normalized R golden fixtures to JSON.
4. Add scripts that reproduce R benchmark baselines.
5. Scaffold `web/` as a static SvelteKit TypeScript app.
6. Define shared simulation, warning, scenario, and table types.
7. Implement deterministic RNG and allocation primitives.
8. Add TypeScript unit tests for allocations and tie-breaks.
9. Add a worker smoke path with progress and warnings.
10. Port politics scrutiny against golden fixtures.
11. Add politics scenario editor and result presentation.
12. Run browser performance tests and decide whether to continue in TypeScript.
13. Add chunked worker execution and repeat the politics benchmark at 1000
    simulations.
14. Replace the debug-source bridge with production static data snapshots.
15. Mature the web-native scenario editor and scenario JSON contract.
16. Add scrutiny-algorithm selection in the UI when a second same-election-kind
    algorithm exists.
17. Migrate regional and municipal workflows.
18. Migrate data preparation as a periodic, election-kind-agnostic devops
    pipeline after the simulator migration.

## Risk Log

- Politics scrutiny is large and law-sensitive. Mitigation: golden-master tests
  before porting and law comments beside translated code.
- Browser memory may be the limiting factor. Mitigation: columnar data,
  chunked worker execution, and no direct `data.table` row-object port.
- R partial matching previously hid `candidati_pluri_sim_`. Mitigation:
  explicit R rename is done and typed names must stay consistent in TypeScript.
- Municipal runoff and individual councilor TODOs are business gaps. Mitigation:
  keep visible TODOs and defer until after the first politics slice.
- Excel scenarios are not a strategic target for the migrated app. Mitigation:
  design a typed web-native scenario model instead of spending migration effort
  on robust workbook ingestion/typechecking.
- Data preparation has different constraints from browser simulation.
  Mitigation: defer its migration until simulator workflows are migrated, then
  choose typed Python or Node/TypeScript based on the pipeline shape and CI needs.
- Some law-commented `sorteggio` paths may not be implemented explicitly.
  Mitigation: preserve current output first, then add `TODO(law-review)` or
  straightforward deterministic seeded draws where safe.
- `web/src/lib/politics/scrutiny.ts` is growing while the port advances.
  Mitigation: keep stage functions explicit now, then split into focused modules
  once the politics scrutiny boundary is stable enough for a low-risk refactor.
- The R remainder ordering for candidate-only vote attribution appears
  inconsistent with the nearby law comment: the comment says highest remainders,
  while the current `order()` call sorts `RESTO` ascending because an extra
  `decreasing` flag is ignored. Mitigation: TypeScript preserves R behavior for
  golden parity and marks the code `TODO(law-review)`.
- The R subentro block has two parity-sensitive behaviors that look accidental:
  non-national same-coalition subentro calls do not pass the `coal`/`livello`
  flags to `cerca_accettori()`, and later candidate merges implicitly include
  `CIFRA_PERCENTUALE` as a join key. Mitigation: TypeScript preserves both and
  marks them `TODO(law-review)`.

## Completed Work

- Created `AGENTS.md` with durable migration principles and repo notes.
- Measured local R baselines and confirmed the politics debug scrutiny fixture
  round-trips exactly against the current R scrutiny output.
- Created `MIGRATION_PLAN.md` as the living migration control document.
- Renamed the R politics scrutiny output key from `candidati_pluri_sim_` to
  `candidati_pluri_sim` and verified equality against the stored debug output.
- Added `scripts/export_politics_golden.R` and exported the first direct scrutiny
  JSON fixture at `test/fixtures/politiche/debug_scrutinio.json`.
- Added `scripts/benchmark_r_workflows.R` and generated a quick local benchmark
  result at `test/fixtures/benchmarks/r_baseline_quick.json`.
- Added the `web/` SvelteKit static app scaffold with strict TypeScript,
  worker API types, a worker smoke path, a compact scenario editor, seeded RNG,
  allocation primitives, Vitest unit tests, and a Playwright smoke test.

## Latest Verification

Run on 2026-06-02:

- R debug scrutiny equality check after rename: passed for Camera and Senato.
- `Rscript scripts/export_politics_golden.R`: passed.
- `node scripts/export_politics_worker_snapshot.mjs`: passed.
- `Rscript scripts/export_politics_pipeline_source.R`: passed.
- `Rscript scripts/export_politics_pipeline_source.R web/static/data/v1/politics-pipeline-source-debug.json`: passed.
- `node scripts/export_politics_static_snapshot.mjs`: passed.
- `Rscript scripts/benchmark_r_workflows.R --politics-sims=10 --municipal-sims=10 --regional-sims=10 --output=test/fixtures/benchmarks/r_baseline_quick.json`: passed.
- `Rscript scripts/benchmark_r_workflows.R --politics-sims=10 --municipal-sims=1 --regional-sims=1 --output=test/fixtures/benchmarks/r_baseline_politics_10_compare.json`: passed.
- `Rscript scripts/benchmark_r_workflows.R --politics-sims=100 --municipal-sims=1 --regional-sims=1 --output=test/fixtures/benchmarks/r_baseline_politics_100_compare.json`: passed.
- `Rscript scripts/benchmark_r_workflows.R --politics-sims=1000 --municipal-sims=1 --regional-sims=1 --output=test/fixtures/benchmarks/r_baseline_politics_1000_compare.json`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 111 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.
- `cd web; npm run benchmark:politics`: passed, 10 simulations in 1.216 s,
  100 simulations in 12.220 s, and 1000 simulations in 131.616 s.

## Current Caveats

- The TypeScript politics scrutiny core matches direct R scrutiny output for the
  10-simulation debug fixture. The browser worker now consumes
  `web/static/data/v1/politics-static-debug.json`, converts its default
  scenario plus reusable data into the internal pipeline source, and runs
  scrutiny on generated Camera/Senato simulations.
- The exported politics fixture is about 68 MB because it contains direct
  scrutiny inputs, R trace tables, and expected outputs for 10 simulations.
- The generated adapter fixture is about 8.1 MB and covers deterministic
  `esegui_scrutini_politiche()` input preparation, not random generation.
- The vote-preparation fixture is about 8.7 MB and covers deterministic
  `prepara_dts()` joins/filters. Its source list votes are reconstructed from
  the debug prepared rows plus synthetic `astensione` and invalid-list rows, so
  it does not claim golden parity for the upstream random `genera_voti()` draw.
- The generic vote-generation fixture is tiny and synthetic. It injects
  R-produced normal draws into the TypeScript generator, proving formula and
  row-order parity without requiring the browser RNG to reproduce R's RNG
  stream.
- The politics vote-generation fixture is also synthetic. It proves
  `genera_voti_politiche()` orchestration, including base-data joins,
  Camera/Senato uninominal aggregation, and the handoff into
  `preparePoliticsVoteTables()`.
- The candidate-generation fixture is synthetic and replays R-recorded
  `sample()` outputs. The generated-candidate default birthdate is read from
  the fixture because R's `as.POSIXct("2000-01-01")` depends on the local
  timezone; on this machine it serializes as `1999-12-31T23:00:00Z`.
- The composed pipeline fixture is synthetic. It validates module composition
  and direct-scrutiny input adaptation, but it is not a substitute for the full
  historical data snapshot or the politics performance gate.
- The real debug generation-source fixture is about 12 MB. It validates a
  one-simulation generated pipeline plus scrutiny smoke path from realistic
  source tables and is still bundled under `web/static/data/v1/` as a legacy
  bridge/test artifact.
- The production-shaped debug static snapshot is about 12 MB and is now the
  worker input. It separates reusable data from `default_scenario`, but it is
  still derived from the debug source and is not the final historical data
  bundle.
- The first worker scenario projection matches edited shares by exact list
  name, preserves the source abstention row, rescales political list
  probabilities into the source model, and recomputes `LOGIT_P`. This keeps the
  browser workflow usable but is not the final scenario import/editor contract.
- The generated worker path currently runs in 50-simulation chunks and is
  capped at 1000 simulations. This is enough for the current benchmark gate but
  should be revisited after production data packaging.
- The generated-worker browser benchmark passes the 10x performance gate for
  10, 100, and 1000 politics simulations. This is still not a final production
  gate because the worker uses a debug-derived static snapshot.
- The browser direct-scrutiny bridge snapshot is about 8.7 MB and contains only
  direct scrutiny inputs/context, not golden traces or expected outputs. It is
  still useful for tests/benchmarks but is no longer the UI worker path.
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
