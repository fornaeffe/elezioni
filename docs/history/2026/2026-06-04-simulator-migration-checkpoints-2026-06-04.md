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
