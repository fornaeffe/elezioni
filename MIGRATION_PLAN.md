# SvelteKit Migration Plan

This is the active, directive-only migration plan. Follow the steps in order
unless a step reveals a safer or cleaner dependency order. Mark a step done
only after its implementation and relevant verification are complete. Record
what changed, why, problems encountered, workarounds, and measured results in
[`docs/migration-history.md`](docs/migration-history.md).

The target is a static SvelteKit TypeScript app under `web/`, with simulation
and scrutiny running in browser workers. Migrate `politiche` first, then
Emilia-Romagna regional elections, then municipal elections, and migrate data
preparation last.

## Plan

- [x] Establish migration guidance, a living plan, and a detailed history.
- [x] Measure repeatable R baselines and add browser benchmark coverage.
- [x] Freeze politics R behavior in golden fixtures before translation.
- [x] Scaffold the static SvelteKit app with strict TypeScript, tests, worker
  contracts, deterministic seeded RNG, and allocation primitives.
- [x] Port the politics scrutiny path and prove direct Camera and Senato parity
  against the debug golden fixture.
- [x] Put politics scrutiny behind a stable algorithm registry, using
  `politiche-r-parity-v1` as the default implementation.
- [x] Port politics candidate generation, vote generation, vote preparation,
  direct-scrutiny adaptation, chunked worker execution, and progress reporting.
- [x] Export the production politics static snapshot and generate the default
  web scenario from it.
- [x] Add raw historical municipal list votes, municipality metadata, and named
  college metadata to the politics snapshot contract.
- [x] Port correspondence-based parameter preparation, including normalized
  splits, many-to-one aggregation, and explicit fallback of unmapped historical
  list votes to `astensione`.
- [x] Add the web-native politics scenario editor with list, coalition, global
  valid-vote share, abstention, historical correspondence, local override, and
  candidate-template controls; keep JSON persistence and validation typed.
- [x] Add politics result presentation, charts, severity-aware messages, and
  reproducible compressed JSON plus table-oriented CSV export.
- [x] Guard the production politics path with a vertical-slice test from static
  snapshot through projection, generation, and scrutiny.
- [x] Pass the politics browser performance gate for 10, 100, and 1000
  simulations against same-machine full R baselines.
- [x] Migrate the Emilia-Romagna regional workflow with golden fixtures, static
  data, scenario defaults, generation, scrutiny, worker dispatch, UI, tests,
  and benchmarks.
- [ ] Keep completed politics and regional vertical slices green while shared
  scenario, worker, result, and routing infrastructure evolves.
- [ ] Preserve mean-mode global and local share semantics; do not expose fixed
  modes until their interaction with geographic variation and uncertainty has
  a dedicated design and test pass.
- [ ] Refactor the large politics scrutiny implementation into stage-focused
  modules only when stable tested boundaries make the change lower-risk.
- [ ] Add another politics scrutiny algorithm only for a concrete law-review or
  comparison need; expose algorithm selection only when two real algorithms for
  the same election kind exist.
- [ ] Repeat the politics performance gate after major changes to scenarios,
  schemas, snapshots, generation, scrutiny, or data preparation. If, after one
  optimization pass, the browser workflow is at least 10 times slower than the
  same full R workflow on the same machine, pause and present Python fallback
  options before continuing.
- [ ] Migrate the municipal workflow: create R golden fixtures and benchmarks
  first, preserve current behavior, and keep the runoff and councilor-candidate
  gaps explicit for later business and legal review.
- [ ] Generalize routing, snapshot selection, scenario defaults, validation,
  worker dispatch, result components, and shared UI patterns across election
  kinds.
- [ ] Migrate data preparation only after simulator workflows are migrated.
  Reassess typed Python versus Node/TypeScript, and keep the result a periodic,
  election-kind-agnostic devops/GitHub Actions pipeline that produces static
  previous-election bundles.
- [ ] Retire temporary bridge artifacts and update user and developer
  documentation when migrated workflows no longer depend on R-exported
  intermediary snapshots.
