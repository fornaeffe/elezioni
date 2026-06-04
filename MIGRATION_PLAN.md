# SvelteKit Migration Plan

Last updated: 2026-06-04

## Purpose

This file is the migration source of truth. The sections before the checkpoint
log describe the current plan, status, decisions, and risks. The dated
checkpoints below are historical notes and should not be used as the primary
task list.

Target architecture: a SvelteKit static TypeScript app under `web/`, with
simulation and scrutiny running in browser workers. The first migrated workflow
is `politiche`, because it has the largest legal and performance risk.

The browser path remains preferred as long as the optimized politics workflow is
not 10x or more slower than the same full R baseline on the same machine. If a
future browser benchmark reaches that stop threshold after one optimization
pass, pause and present Python fallback options before continuing.

## Current State

| Area | State | Source of truth |
| --- | --- | --- |
| Migration tracking | Done | `AGENTS.md` and this plan are present. |
| R safety fixes | Done | `candidati_pluri_sim_` renamed to `candidati_pluri_sim`; debug scrutiny still matches. |
| Politics golden fixtures | Done for current politics slice | `scripts/export_politics_golden.R`, `test/fixtures/politiche/debug_scrutinio.json`. |
| R and browser benchmarks | Done for current politics slice | `scripts/benchmark_r_workflows.R`, `web/tests/benchmarks/politics-worker.spec.ts`, benchmark JSON under `test/fixtures/benchmarks/`. |
| SvelteKit app scaffold | Done | `web/` with strict TypeScript, static adapter, Vitest, Playwright. |
| Shared TypeScript core | Usable, still growing | Worker API types, seeded RNG, allocation primitives, scenario types, warning/result contracts. |
| Politics scrutiny | R-parity direct fixture passes | `web/src/lib/politics/scrutiny.ts`; registry id `politiche-r-parity-v1`. Split only when boundaries are clearer. |
| Politics generation pipeline | Current browser path working | Candidate generation, vote generation, vote preparation, direct-scrutiny adaptation, worker chunking. |
| Production static politics snapshot | Bridge done | `scripts/export_politics_static_snapshot.R` writes `web/static/data/v1/politics-static.json`. |
| Politics scenario defaults | Bridge done | `scripts/export_politics_scenario_defaults.mjs` writes `web/src/lib/scenario/politics-defaults.generated.ts`. |
| Scenario editor | Basic workflow working | List/coalition/share editor, JSON save/load, localStorage, reset, validation. Advanced controls remain. |
| Politics browser vertical slice | Stabilized | `web/src/lib/politics/vertical-slice.test.ts` guards snapshot -> scenario -> projection -> generation -> scrutiny. |
| Performance gate | Passed | Chromium worker on the R-exported production static snapshot is below fresh full R politics baselines for 10, 100, and 1000 simulations. |
| Regional and municipal workflows | Not started | Migrate after politics browser workflow is stable enough. |
| Data-preparation migration | Deferred | Reassess typed Python vs Node/TypeScript after simulator workflows are migrated. |

## Next Work

This ordered list is the active implementation plan. Update it whenever a step
reveals a cleaner order or a new blocker.

1. Keep the politics browser vertical slice green while scenario semantics grow.
   Every change to defaults, projection, generation, worker behavior, or the
   scrutiny registry should preserve or deliberately update
   `web/src/lib/politics/vertical-slice.test.ts`.
2. Expose global `mean`/`fixed` share mode in the advanced scenario UI. The
   projection boundary already honors `fixed` globally by setting
   `SIGMA_GLOBAL = 0` for active political list rows.
3. Add a compact one-to-one correspondence view/editor for advanced scenario
   users. Support only the semantics already implemented by projection: one
   current source-model list reused by one future scenario list. Keep bundled
   correspondences visible as defaults/metadata without warning spam.
4. Decide and implement richer correspondence semantics only after the UI makes
   the business meaning explicit. Multi-source aggregation and split factors
   need clear rules for local deltas, variability, abstention, and candidate
   templates before they should affect simulations.
5. Add candidate-template support to the scenario model and worker pipeline,
   allowing user-provided names for selected slots while preserving generated
   candidates for unspecified places.
6. Improve politics result presentation with legally meaningful summaries,
   charts, exports, and clearer warning/detail separation. Keep diagnostic
   tables such as `Generated pipeline runs` collapsed by default.
7. Refactor politics scrutiny only when it lowers risk. The likely target is
   stage-focused modules behind the existing scrutiny algorithm registry, but
   do not split during active parity discovery just for size alone.
8. Add a second politics scrutiny algorithm only for a concrete law-review or
   comparison need. Expose UI selection/comparison only after at least two real
   same-election-kind algorithms exist.
9. Repeat the politics browser performance gate after major scenario, data,
   generation, or scrutiny changes.
10. Migrate the Emilia-Romagna regional workflow: first add R golden fixtures
    and benchmarks, then port allocation, generation, scrutiny, worker, UI, and
    browser tests into the same architecture.
11. Migrate the municipal workflow: first add R golden fixtures and benchmarks,
    preserve current behavior, and keep known runoff/councilor-candidate
    business TODOs explicit for later law review.
12. Generalize the web app across election kinds: routing, snapshot selection,
    scenario defaults, worker dispatch, result components, validation, and
    shared UI patterns.
13. Migrate data preparation after simulator workflows are migrated. Reassess
    typed Python versus Node/TypeScript then; keep it a periodic,
    election-kind-agnostic devops/GitHub Actions pipeline producing static
    previous-election bundles.
14. Retire temporary bridge artifacts and update user/developer documentation
    once migrated workflows no longer depend on R-exported intermediary
    snapshots.

## Completed Foundations

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
- Generated the default web politics scenario from that static snapshot,
  including bundled list correspondences.
- Built the basic scenario editor: metadata, list/coalition/share editing,
  validation, reset, JSON import/export, localStorage autosave, and plain
  worker-safe scenario snapshots.
- Added scenario projection from the web-native scenario into the current
  source model, including explicit share overrides, proportional recalculation
  for non-overridden matched lists, homonymous matching, safe one-to-one
  declared correspondences, projection result rows, and warnings.
- Reworked results so primary user-facing summaries appear before diagnostics,
  with generated pipeline details collapsed by default.
- Added the production politics vertical-slice guard.
- Passed the current browser performance gate for 10, 100, and 1000 politics
  simulations.

## Durable Decisions

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
- Location-specific percentages, per-location fixed/mean modes, rich
  correspondence matrices, and candidate editors are advanced features. Keep
  schema/generator hooks ready, but build the large UI only when the underlying
  data contract is stable.
- Scrutiny algorithms must remain modular and swappable. Keep the registry hook
  even while there is only one registered politics algorithm.
- Results UI should prioritize election outputs and keep diagnostic pipeline
  tables behind an explicit detail/debug affordance.
- Data preparation should be migrated last. It should remain a periodic
  devops/GitHub Actions workflow and stay agnostic about election kind.

## Verification Snapshot

Latest full web verification on 2026-06-04:

- `cd web; npx vitest run src/lib/scenario/politics.test.ts src/lib/politics/vertical-slice.test.ts`: passed, 10 tests.
- `cd web; npx vitest run src/lib/politics/scenario-projection.test.ts`: passed, 7 tests.
- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 132 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

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

## Current Caveats And Risks

- `web/src/lib/politics/scrutiny.ts` is large. Keep it stable while parity is
  still being discovered; split it into stage-focused modules only when the
  tested boundaries are clear.
- Several politics `sorteggio` paths preserve R stable ordering where law
  comments may imply random draws. These are marked `TODO(law-review)` where
  found and should be reviewed with Luca before business correction.
- The R candidate-only vote attribution appears inconsistent with its law
  comment: the comment says highest remainders, while current R effectively
  sorts `RESTO` ascending because an extra `decreasing` flag is ignored.
  TypeScript preserves R behavior for parity.
- The R subentro block has parity-sensitive behaviors that look accidental:
  some same-coalition subentro paths do not pass `coal`/`livello`, and later
  candidate merges implicitly include `CIFRA_PERCENTUALE` as a join key.
  TypeScript preserves both for parity.
- The production static snapshot is still an R-exported bridge, not the final
  migrated data-preparation pipeline. Regenerate dependent generated defaults
  when `web/static/data/v1/politics-static.json` changes.
- Bundled list correspondences in the generated default are durable metadata for
  defaults and future advanced editing. The current projection warns only for
  unused manual correspondences.
- Declared correspondence projection supports only the safe one-to-one bridge
  case. Multi-source aggregation and split factors remain deferred until their
  business semantics are explicit.
- New/unmatched scenario lists are ignored by the current static-snapshot
  worker path unless they reuse one current source-model list through a declared
  correspondence. The worker reports this as a warning.
- `globalShareMode = "fixed"` is currently honored globally by setting
  `SIGMA_GLOBAL = 0` for active political list rows. Municipality-level
  variation is unchanged until location-specific semantics are designed.
- Placeholder uninominal candidates may be generated when a matched scenario
  list uses a coalition absent from the current candidate template. This keeps
  the browser workflow runnable but needs final scenario/data semantics.
- The generated worker path currently runs in 50-simulation chunks and is
  capped at 1000 simulations. Revisit after columnar packaging or if larger UI
  runs are needed.
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

## 2026-06-03 Checkpoint 26

Completed in the scrutiny-algorithm registry pass:

- Added optional `scrutinyAlgorithmId` to `SimulationRequest` and
  `SimulationBenchmark`.
- Added `web/src/lib/politics/scrutiny-algorithms.ts`, registering
  `politiche-r-parity-v1` as the default politics scrutiny algorithm.
- Rewired the generated politics worker to resolve the algorithm through the
  registry instead of importing `runPoliticsScrutiny()` directly.
- Recorded the selected algorithm in benchmark metadata and added a fallback
  warning for unknown requested algorithm IDs.
- Added registry tests proving the default algorithm is registered, unknown IDs
  fall back explicitly, and the default registry path calls the same
  implementation as the direct R-parity scrutiny function.
- Kept the UI selector deferred: there is still only one real politics
  algorithm, so exposing a visible choice would add noise without user value.

Verification:

- `cd web; npm run check`: passed with 0 warnings.
- `cd web; npm run test`: passed, 125 tests.
- `cd web; npm run build`: passed.
- `cd web; npm run test:e2e`: passed, 1 Playwright test.

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
