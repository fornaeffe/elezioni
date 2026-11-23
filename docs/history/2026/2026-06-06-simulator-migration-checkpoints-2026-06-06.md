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
