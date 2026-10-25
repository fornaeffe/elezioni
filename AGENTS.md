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
- Keep the applicable election-law text in comments close to translated
  scrutiny logic.
- Update the plan when a step is completed or reveals a safer order or a new
  blocker. Put evidence and narrative in history, not in the plan.

## R repository Map And Data Safety

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
  development tests skip clearly when they are absent. Release acceptance must
  require every production bundle and its dependent checks; missing data is a
  failure, not an accepted skip or a debug-data fallback.
- Do not edit generated defaults by hand. After regenerating a static snapshot,
  run its corresponding scenario-default and catalog generators.
- New preparation sources belong in `data/sources/`, derived artifacts in
  `data/derived/`; follow [`data/README.md`](data/README.md) and its pinned source
  manifest. `dati/` is legacy: remove it only after the whole data-preparation
  migration is complete and its legacy consumers have migrated.
- Keep population reconstruction assumptions explicit and conserve the legal
  municipal census total. Missing fine geography may use labelled estimates;
  it must not silently become zero or be mistaken for an official seat decree.

## Web app architecture

- Keep the browser as the primary target. Use a static SvelteKit app, browser workers,
  deterministic seeded RNG, chunking, and columnar or typed-array structures
  where scale requires them.
- Target GitHub Pages hosting with GitHub Actions for Node/TypeScript data
  preparation, validation and static builds. Keep navigation, worker and data
  URLs compatible with a configurable project base path. Use the final
  publication gate in `MIGRATION_PLAN.md` before enabling public deployment.
- Keep the legal scrutiny core separate from UI components and worker
  orchestration. Keep algorithms modular and selectable through stable
  registries, even while only one implementation exists.
- Add an algorithm selector or comparison UI only after two real algorithms
  exist for the same election kind.
- Use Svelte 5 runes-mode practices: `$state` for local mutable state,
  `$derived` for computed state, and `$effect` only when lifecycle or
  event-driven code is not clearer.
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

## Other general instructions

- Use $...$ and $$...$$ syntax for markdown math formulas.

