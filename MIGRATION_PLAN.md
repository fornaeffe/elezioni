# SvelteKit Migration Plan

Last updated: 2026-06-01

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
| Politics scrutiny port | Started | Direct politics scrutiny output now matches R for the debug fixture and runs in the worker through a compact snapshot bridge. |
| Generated politics input adapter | Started | R-style generated-table fixture and TypeScript adapter now rebuild the exact direct scrutiny inputs/context. `prepara_dts()` vote preparation is also ported; random vote/candidate generation is still pending. |
| Performance gate | Pending | Compare browser politics run to fresh R baselines. |

## Decisions

- Target: SvelteKit-first, not Python-first.
- First vertical slice: politics.
- Scenario UX: in-app editor first.
- Data source: bundled static snapshots.
- Compatibility: business/legal behavior must match R unless Luca approves a correction.
- R TODOs: leave for later unless a fix is trivial and helps migration safety.
- `sorteggio`: implement only when straightforward; otherwise add `TODO(law-review)`.
- Scrutiny implementations should stay modular and swappable behind a stable
  interface, so the same data/scenario can be compared across algorithm
  variants during law review.
- Delay splitting `web/src/lib/politics/scrutiny.ts` until the golden-tested
  stage boundaries are clear enough that the refactor reduces risk.

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

## Risk Log

- Politics scrutiny is large and law-sensitive. Mitigation: golden-master tests
  before porting and law comments beside translated code.
- Browser memory may be the limiting factor. Mitigation: columnar data,
  chunked worker execution, and no direct `data.table` row-object port.
- R partial matching previously hid `candidati_pluri_sim_`. Mitigation:
  explicit R rename is done and typed names must stay consistent in TypeScript.
- Municipal runoff and individual councilor TODOs are business gaps. Mitigation:
  keep visible TODOs and defer until after the first politics slice.
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

Run on 2026-06-01:

- R debug scrutiny equality check after rename: passed for Camera and Senato.
- `Rscript scripts/export_politics_golden.R`: passed.
- `node scripts/export_politics_worker_snapshot.mjs`: passed.
- `Rscript scripts/benchmark_r_workflows.R --politics-sims=10 --municipal-sims=10 --regional-sims=10 --output=test/fixtures/benchmarks/r_baseline_quick.json`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 87 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## Current Caveats

- The TypeScript politics scrutiny core matches direct R scrutiny output for the
  10-simulation debug fixture and is wired into the browser worker through a
  compact debug snapshot. The generated-table adapter can now rebuild those
  direct inputs from the R-style generated vote/candidate tables, but the worker
  still does not consume the scenario editor because random scenario-to-vote
  generation is not ported yet; the worker returns
  `POLITICS_SCENARIO_GENERATOR_PENDING` intentionally.
- The exported politics fixture is about 68 MB because it contains direct
  scrutiny inputs, R trace tables, and expected outputs for 10 simulations.
- The generated adapter fixture is about 8.1 MB and covers deterministic
  `esegui_scrutini_politiche()` input preparation, not random generation.
- The vote-preparation fixture is about 8.7 MB and covers deterministic
  `prepara_dts()` joins/filters. Its source list votes are reconstructed from
  the debug prepared rows plus synthetic `astensione` and invalid-list rows, so
  it does not claim golden parity for the upstream random `genera_voti()` draw.
- The browser direct-scrutiny bridge snapshot is about 8.7 MB and contains only
  direct scrutiny inputs/context, not golden traces or expected outputs.
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
