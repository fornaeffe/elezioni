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
