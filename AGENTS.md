# Agent Instructions

This repository contains an R simulator for Italian national, municipal, and
Emilia-Romagna regional elections, plus an in-progress static SvelteKit and
TypeScript migration under `web/`.

Use [`MIGRATION_PLAN.md`](MIGRATION_PLAN.md) as the active task list. Record
completed work, rationale, verification, problems, and workarounds in
[`docs/migration-history.md`](docs/migration-history.md). Keep this file limited
to stable instructions that apply across migration steps.

## Working Principles

- Keep the project clean, clear, and scalable. Prefer a sound long-term design
  over a shortcut that creates recurring maintenance.
- Change architecture freely where useful, but preserve business and legal
  behavior unless Luca explicitly approves a correction.
- Treat scrutiny functions (`scrutinio...`) as the highest-risk code. Freeze R
  behavior in golden-master tests before translating or refactoring it.
- Compare translated scrutiny output column by column, including row order when
  legally meaningful. Preserve warnings and exceptional cases as expectations
  until Luca decides whether they are defects.
- Make tie-breaking explicit and reproducible. Preserve current behavior for
  parity first, even where the law mentions `sorteggio`, then mark and report
  suspected mismatches for legal review.
- Keep the applicable election-law text in comments close to translated
  scrutiny logic. Report a suspected mismatch before treating it as intended
  behavior or correcting it.
- Give special scrutiny to admission thresholds, coalition and list rules,
  D'Hondt and Hare-Niemeyer allocations, uninominal/plurinominal seat
  reconciliation, candidate exhaustion and subentro, pluricandidature
  resolution, and municipal ballot/runoff handling.
- Update the plan when a step is completed or reveals a safer order or a new
  blocker. Put evidence and narrative in history, not in the plan.

## Repository Map And Data Safety

- Main R code lives under `R/`. The most sensitive implementations are
  `R/politiche/scrutinio.R`, `R/Emilia-Romagna/scrutinio_ER.R`,
  `R/comunali/scrutinio_comunali.R`, and `R/caricamento_dati.R`.
- Root `.qmd` files contain example workflows; Excel scenario inputs are under
  `scenari/`.
- `dati/dati.RData` is an RDS file despite its extension. Read it with
  `readRDS()`, as `carica_dati()` does; do not use `load()`.
- `dati/debug_scrutinio.RData` is the politics scrutiny fixture with prepared
  inputs and stored output for ten simulations, including the known
  warning-producing simulation 4.
- Large generated politics and regional JSON artifacts are intentionally
  untracked. Regenerate them from the scripts when needed, and make dependent
  tests skip clearly when they are absent. Keep small generated TypeScript
  defaults and catalogs tracked.
- Do not edit generated defaults by hand. After regenerating a static snapshot,
  run its corresponding scenario-default and catalog generators.

## Migration Architecture

- Keep the browser as the primary target while it satisfies the performance
  gate in the migration plan. Use a static SvelteKit app, browser workers,
  deterministic seeded RNG, chunking, and columnar or typed-array structures
  where scale requires them; do not directly port `data.table` row objects.
- Keep the legal scrutiny core separate from UI components and worker
  orchestration. Keep algorithms modular and selectable through stable
  registries, even while only one implementation exists.
- Add an algorithm selector or comparison UI only after two real algorithms
  exist for the same election kind.
- Use Svelte 5 runes-mode practices: `$state` for local mutable state,
  `$derived` for computed state, and `$effect` only when lifecycle or
  event-driven code is not clearer.
- Convert Svelte proxy state to plain cloned snapshots before `postMessage()`;
  proxies are not structured-clone safe.
- Keep election result aggregation in tested TypeScript presentation helpers,
  not in route components or worker orchestration. Derive chart structures from
  those result-table contracts.
- Keep generic export logic in `web/src/lib/core/result-export.ts`. Structured
  result exports must include both scenario and worker result; CSV remains
  table-oriented for spreadsheet review.
- Use `ScrutinyWarning.severity` consistently: `info` for normal run metadata,
  `warning` for issues that may affect interpretation, and `error` for failed
  execution paths. Present informational notes separately from warnings and
  errors.
- Prefer user-facing election results. Keep diagnostic pipeline tables hidden
  or collapsed behind an explicit debug/details affordance.
- Allow `web/src/lib/politics/scrutiny.ts` to remain large while parity stages
  are still being discovered. Split it only after tested stage boundaries are
  stable enough to reduce risk.
- Migrate data preparation after the simulator workflows. Keep it a periodic,
  election-kind-agnostic devops or GitHub Actions pipeline that produces static
  previous-election bundles; reassess typed Python versus Node/TypeScript then.

## Scenario Semantics

- Make the web UI the preferred scenario editor. Excel scenarios do not need to
  remain a first-class migrated interface.
- Choose defaults first from bundled data for the election kind and territory.
  Otherwise use the most-voted lists from the latest same-kind election in the
  same territory; if coalition history is unavailable, put each list in its own
  coalition.
- Display list shares as percentages of valid votes, excluding abstention.
  Internally use fractions of electors, which sum to one only with
  `astensione`. Expose abstention separately rather than as an ordinary list.
- Let users override only some global list shares. Convert overrides to elector
  fractions using the active abstention fraction, hold abstention fixed, and
  distribute the remaining fraction among non-overridden lists according to
  projected previous-election results. If every list is overridden and the
  valid-vote total differs from 100%, normalize to 100% and warn.
- In mean mode, treat an overridden global share as the current
  `P_{l,t-1}` and set its parameter date to the projection date, so uncertainty
  continues to drift from today to election day.
- Do not expose fixed percentage modes until separately designed and tested. A
  future global fixed mode must bypass random vote generation and produce one
  deterministic vote distribution and scrutiny output. A future per-percentage
  mode may zero selected uncertainty only after its interaction with local
  variation is defined.
- For local overrides, use the same valid-vote-to-elector conversion and
  normalization rules with local historical fractions. Keep local abstention
  fixed, then recompute local deltas and parameter dates. Apply broader region
  overrides before province and municipality overrides so the most specific
  value wins.
- Project historical correspondences per municipality and election. Sum many
  original lists mapped to one future list; split one original list among future
  lists using normalized factors; count original abstention and unmapped
  original-list votes as `astensione`.
- Treat bundled correspondences as default metadata. Keep validation strict for
  user-authored correspondence rows without producing warnings merely because a
  bundled row became irrelevant after a list was removed or renamed.
- Candidate templates pin selected slots without replacing generation.
  Uninominal templates target chamber, coalition, and `UNI_COD`; plurinominal
  templates target chamber, list, `PLURI_COD`, candidate number, and minority
  flag. Apply `candidateName` to `CANDIDATO_ID` and optional `birthDate` to
  `DATA_NASCITA`; warn and otherwise ignore unmatched templates.

## Known Business And Legal Review Boundaries

- Keep the renamed politics output key explicit as `candidati_pluri_sim`; do
  not reintroduce the former trailing-underscore name.
- Municipal simulation currently copies list votes into mayor and runoff vote
  columns and does not simulate the runoff. Municipal councilor seats are
  allocated to lists or coalitions without ranking individual candidates.
  Preserve these gaps during parity migration and keep them explicit for later
  business review.
- Preserve the politics R behavior marked `TODO(law-review)`, including the
  candidate-only remainder ordering, same-coalition subentro argument behavior,
  implicit `CIFRA_PERCENTUALE` merge key, internal Camera flipper counter update,
  and stable ordering in some legal tie cases. Consult migration history for the
  exact observations and rationale.
- Treat the existing Valle d'Aosta and Trentino-Alto Adige/Senate FIX/TODO notes
  as review points.
- Preserve the candidate-generation edge where an `NA` uninominal candidate can
  be sampled into plurinominal slots from an empty right-join row until a
  business correction is approved.
- The R correspondence join uses `nomatch = NULL`; incomplete workbooks can drop
  unmapped original-list votes. Do not copy that fragility into the web path:
  map them explicitly to `astensione`.

## Verification And Generated Artifacts

- Keep vertical-slice tests green when changing defaults, projections,
  generation, worker behavior, scrutiny registries, or static snapshot schemas.
- Run focused tests while developing, then the relevant full TypeScript check,
  unit tests, production build, and Playwright tests before declaring a phase
  complete. Run the relevant R fixture exporters and benchmarks when changing
  parity boundaries or performance-sensitive code.
- Run `cd web; npm run build` and `cd web; npm run test:e2e` sequentially. They
  both touch SvelteKit build/prerender output and can race on hashed assets.
- The generated worker currently uses 50-simulation chunks and caps runs at
  1000. Revisit this only when larger runs are required or columnar packaging
  changes the trade-off.
- Record commands, counts, timing, environment, and outcomes in
  `docs/migration-history.md`; do not turn this file into a status log.
