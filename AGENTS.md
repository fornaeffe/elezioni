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
- Keep scrutiny algorithms modular and swappable. The same normalized data and
  scenario should eventually be runnable through different scrutiny algorithm
  implementations behind a stable interface, for comparison or law-review
  experiments.
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
- `scripts/benchmark_r_workflows.R` reruns R baseline workflows.
- `web/` contains the initial static SvelteKit app scaffold, strict TypeScript
  setup, worker API types, seeded RNG, allocation primitives, unit tests, and a
  Playwright smoke test.
- The TypeScript politics scrutiny port now matches the R golden fixture through
  pre-subentro plurinominal seat allocation: uninominal candidate election,
  candidate-only vote attribution to lists, plurinominal/circumscription
  aggregates, uninominal candidate percentages, circumscription totals,
  national list/coalition figures, 1%/3%/10% threshold flags, Camera national
  proportional allocation, Camera/Senato subject-level circumscription seat
  allocation with Camera flipper reconciliation, internal list allocation with
  the second Camera flipper reconciliation, and Camera/Senato plurinominal
  allocation with reconciliation back to circumscription list seats. The later
  elected plurinominal candidates, pluricandidature, and subentro logic are
  still pending; the worker currently returns a deliberate
  `POLITICS_SCRUTINY_NOT_PORTED` warning.
- In the Camera internal list-in-coalition flipper, the R code decrements the
  recipient list's `SEGGI_ECCEDENTI_CONTATORE` after giving it a seat. This
  appears counterintuitive but is preserved in TypeScript for parity and marked
  `TODO(law-review)`.
- Plurinominal allocation also preserves R stable ordering for equal decimal
  remainders/equal figures where the law comments mention `sorteggio`; this is
  marked `TODO(law-review)` in TypeScript.
- When posting Svelte `$state` data to workers, derive or build plain snapshots
  first. Svelte proxies are not structured-clone safe.
