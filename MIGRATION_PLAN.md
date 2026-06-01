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
| Golden-master fixtures | Done | `scripts/export_politics_golden.R` exports `test/fixtures/politiche/debug_scrutinio.json`. |
| R benchmarks | Done | `scripts/benchmark_r_workflows.R` added; quick baseline JSON generated. |
| SvelteKit app scaffold | Done | `web/` created with strict TS, static adapter, Vitest, Playwright. |
| TypeScript core | Started | Worker API types, seeded RNG, allocation primitives, and unit tests added. |
| Politics scrutiny port | Pending | Port after fixture export and core primitives are stable. |
| Performance gate | Pending | Compare browser politics run to fresh R baselines. |

## Decisions

- Target: SvelteKit-first, not Python-first.
- First vertical slice: politics.
- Scenario UX: in-app editor first.
- Data source: bundled static snapshots.
- Compatibility: business/legal behavior must match R unless Luca approves a correction.
- R TODOs: leave for later unless a fix is trivial and helps migration safety.
- `sorteggio`: implement only when straightforward; otherwise add `TODO(law-review)`.

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
- `Rscript scripts/benchmark_r_workflows.R --politics-sims=10 --municipal-sims=10 --regional-sims=10 --output=test/fixtures/benchmarks/r_baseline_quick.json`: passed.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm test`: passed, 6 unit tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

## Current Caveats

- The TypeScript politics scrutiny core is not ported yet. The current worker
  returns `POLITICS_SCRUTINY_NOT_PORTED` intentionally.
- The exported politics fixture is about 31 MB because it contains direct
  scrutiny inputs and expected outputs for 10 simulations.
- `npm audit` reports 3 low-severity findings through SvelteKit's transitive
  `cookie` dependency. The suggested automatic fix is a semver-major downgrade
  to obsolete SvelteKit packages, so it has not been applied.
