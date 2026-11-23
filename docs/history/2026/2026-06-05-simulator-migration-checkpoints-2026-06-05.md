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
