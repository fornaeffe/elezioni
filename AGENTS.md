# Agent Instructions

This repository contains a static SvelteKit and TypeScript simulator under
`web/` for Italian national and Emilia-Romagna regional elections. Municipal
simulation remains deferred. Production preparation uses Node/TypeScript;
the retired R implementation is available only in Git history.

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

## Repository Map And Data Safety

- Application code and Node preparation live under `web/`; run npm commands
  there. Use [web workspaces](docs/web-workspaces.md) for task entry points and
  [production preparation](docs/data-preparation.md) for complete regeneration.
- Frozen JSON regression inputs/outputs under `test/fixtures/` retain historical
  R parity and approved legal failures. Preserve their checksum manifests;
  they are test evidence, never runtime or production-preparation inputs.
- Large generated politics and regional JSON artifacts are intentionally
  untracked. Regenerate them from the scripts when needed, and make dependent
  development tests skip clearly when they are absent. Release acceptance must
  require every production bundle and its dependent checks; missing data is a
  failure, not an accepted skip or a debug-data fallback.
- Do not edit generated defaults by hand. After regenerating a static snapshot,
  run its corresponding scenario-default and catalog generators.
- New preparation sources belong in `data/sources/`, derived artifacts in
  `data/derived/`; follow [`data/README.md`](data/README.md) and its pinned source
  manifest. Historical implementation references in migration history are not
  current commands; consult the pinned pre-retirement revision there if needed.
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

- The vote generation algorithm should:
  - generate a plausible distribution of votes for each list in each municipality at the next political election with these properties (that must hold not only nationwide but also for each college), in this priority order:
    - limit generation of wildly extreme, non plausible results
    - the real results should have a sufficient likelihood in the generated distributions
    - the expected value of the generated distribution should be close to the real results, and preferably non sistematically biased
  - use the current membership table architecture to link components to historical and future lists
  - be applicable onto the actual italian political context
  - use information from past elections and polls
  - keep the computational time under 60 minutes in browser on this machine for fitting and 1000 simulations (vote generation + scrutiny), using an adequate number of workers.
- The scrutiny algorithms should follow the law as strictly as possible, adopting documented provisional fallback to overcome situations
  where a strict law application would prevent scrutiny completion. Ideally, given the same input of a real election (e.g. lists, votes, candidates...) it should return the real outcome (elected candidates).

## Other general instructions

- Use $...$ and $$...$$ syntax for markdown math formulas.

