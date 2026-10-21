# SvelteKit Migration Plan

This is the active, directive-only migration plan. Follow the steps in order
unless a step reveals a safer or cleaner dependency order. Mark a step done
only after its implementation and relevant verification are complete. Record
what changed, why, problems encountered, workarounds, and measured results in
[`docs/migration-history.md`](docs/migration-history.md).

The target is a static SvelteKit TypeScript app under `web/`, with simulation
and scrutiny running in browser workers. Migrate `politiche` first, then
Emilia-Romagna regional elections, then municipal elections. Shared source
acquisition and geography/census preparation migrate with reform R26-07;
historical election importers and remaining data preparation follow their consumers.
The desired hosting target for the whole migration is GitHub Actions plus
GitHub Pages on GitHub Free: Actions prepares data and builds the static site;
simulation and scrutiny remain in the browser. Reassess the completed workload
at the final publication gate below. Luca merges the completed migration branch
into `main`; public deployment follows that merge and a successful hosting gate.

## Working principles


- Treat scrutiny functions (`scrutinio...`) as the highest-risk code. Freeze R
  behavior in golden-master tests before translating or refactoring it.
- Compare translated scrutiny output column by column, including row order when
  legally meaningful. Preserve warnings and exceptional cases as expectations
  until Luca decides whether they are defects.
- Make tie-breaking explicit and reproducible. Preserve current behavior for
  parity first, even where the law mentions `sorteggio`, then mark and report
  suspected mismatches for legal review.
- Give special scrutiny to admission thresholds, coalition and list rules,
  D'Hondt and Hare-Niemeyer allocations, uninominal/plurinominal seat
  reconciliation, candidate exhaustion and subentro, pluricandidature
  resolution, and municipal ballot/runoff handling.
- Use the Node/TypeScript acquisition and geography/census foundation in
  [`data/README.md`](data/README.md). Migrate remaining historical importers as
  their simulator/model inputs are implemented. Keep all preparation a
  non-interactive Node/TypeScript pipeline, reusable locally and in GitHub
  Actions, producing versioned static bundles across election kinds. See the
  [preparation delivery contract](data/README.md#github-actions-delivery-contract).

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
  complete. Run relevant R fixture exporters when changing parity boundaries,
  and browser benchmarks when changing performance-sensitive code.
- Run `cd web; npm run build` and `cd web; npm run test:e2e` sequentially. They
  both touch SvelteKit build/prerender output and can race on hashed assets.
- Legacy politics and regional generation use 50-simulation chunks. Legacy
  politics retains its 1000-run plot limit. Reform uses bounded per-simulation
  compute workers and local result storage without an arbitrary run cap.
- Record commands, counts, timing, environment, and outcomes in
  `docs/migration-history.md`; do not turn this file into a status log.

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
- [x] **2026 politics reform** Follow the step-by-step
  [implementation plan](docs/politics-reform-2026-plan.md), using the
  [normative specification](docs/politics-reform-2026-specification.md),
  [current scoped transcription](docs/politics-reform-2026.md) and the
  [modelling decisions](docs/politics-reform-2026-modelling.md). Keep the old
  scrutiny algorithm as a selectable alternative. Apply the approved provisional
  I07 Senate recovery policy, provisional I10 coalition-college recovery reading,
  I11 exclusion of zero-territorial-entitlement lists from recovery,
  provisional I12 exclusion of absent lists from ordinary territorial allocation,
  I13 warned Camera ordinary/cap integer-seat donor fallback,
  provisional I14 repeated initial Camera list remainder passes,
  and approved R26-02/03/04 models: attributed list
  totals, historically informed synthetic preferences and paired chamber votes
  with common anchors and explicit chamber precedence. Maintain the
  [legal-gap register](docs/politics-reform-2026-legal-gaps.md) as gaps are verified.
  R26-05 contracts and
  validation, R26-06 numerical primitives, R26-07 versioned geography/seat
  tables, R26-08 electoral figures/rankings, R26-09 admission/preliminary
  special winners, R26-10 special territories, R26-11 joint bonus decision and
  R26-12 Camera national allocation/cap, R26-13 Camera territorial allocation and
  R26-14 Senato regional allocation/cap, R26-15 Senato list and college
  allocation, R26-16 proclamations and multiple elections, R26-17 exhaustion
  and replacements, R26-18 composition and registry, R26-19 scenarios,
  historical preparation and generation, and R26-20 worker/UI/explanation/export
  integration are complete, including bounded local result storage, lazy detail
  queries and streaming archive import/export. R26-20a bounded parallel execution
  and resource-based worker choice, and R26-21 local application/performance
  acceptance are complete, using explicitly provisional I08 census tables.
  Preserve conditional preference outcomes, explicit chamber-covariance assumptions and non-computable legal
  paths when presenting simulation probabilities. Source refresh, required-data
  checks and repeatable local acceptance follow the
  [R26-21 procedure](docs/politics-reform-2026-acceptance.md); whole-migration
  publication remains the final gate below.
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
- [ ] Repeat browser performance measurements after major changes to scenarios,
  schemas, snapshots, generation, scrutiny, or data preparation. R retirement is
  decided: assess browser latency, phase costs and memory directly, without an
  R comparison or a 10x stop rule.
- [ ] Migrate the municipal workflow: create R golden fixtures and benchmarks
  first, preserve current behavior, and keep the runoff and councilor-candidate
  gaps explicit for later business and legal review.
- [ ] Generalize routing, snapshot selection, scenario defaults, validation,
  worker dispatch, result components, and shared UI patterns across election
  kinds. Preserve configurable project-base navigation and worker/data loading
  for GitHub Pages, including direct page loads and refreshes.
- [ ] Finish data-preparation migration on the shared R26-07 acquisition,
  geography and census foundation. Migrate historical election importers and
  static previous-election bundles; use R26-19 for required paired Senate history
  and calibration. Deliver all production bundles/defaults from a clean clone
  through the Node/TypeScript pipeline, with cache misses supported and required
  data checks blocking release. Remove legacy `dati/` only after the whole
  data-preparation migration is complete and its consumers have migrated.
- [ ] Retire temporary bridge artifacts and update user and developer
  documentation when migrated workflows no longer depend on R-exported
  intermediary snapshots.
- [ ] **Final hosting reassessment and publication.** After all preceding
  implementation and acceptance work, reassess GitHub Actions plus GitHub Pages
  against current GitHub Free eligibility, runner time/RAM/disk, artifact/cache
  retention, site size, bandwidth and deployment limits. Measure the complete
  cold-cache Node preparation/check/build pipeline on a standard hosted Linux
  runner, warm-cache replay, and actual production payload/transfer sizes across
  every migrated election kind. Validate project-base routes, direct loads,
  refreshes, workers, required bundles, simulation and exports. If feasible,
  implement/verify the preparation, validation, build and Pages deployment
  workflow with bounded retention and production-only output; keep public
  deployment disabled until Luca merges the migration branch into `main`.
  Then publish the passing `main` revision, smoke-test the public site and
  record the URL, commit, data versions, measurements and outcome in history.
  Leave this step open until publication and deployed checks succeed. If the
  target fails, record the concrete blocker and reassess free alternatives
  before selecting a replacement; do not mark hosting or migration complete.

## After completion of the web migration

Start these additions after all simulator workflows and data preparation have
completed the web migration; they do not expand the current domestic reform
implementation or municipal parity scope.

- [ ] Add foreign-seat simulation under the applicable foreign-voting law,
  including its current territorial divisions, seat endowments, votes,
  preferences and candidate proclamations. Keep its inputs and calculations
  separate from domestic admission, bonus and caps, then compose accurately
  labelled whole-chamber results.
- [ ] Review historical Camera/Senato coverage delivered under R26-19 and extend
  it where additional comparable elections/geographies improve the evidence.
  Paired Senate-history preparation is part of the current R26-04 decision's
  implementation, not a deferred duplicate model. Preserve the distinction
  between historical 25+ Senate electorates and the 18+ baseline, align turnout,
  geography and list coverage, and version any expanded calibration evidence.
- [ ] Model away-from-home voter distribution using sourced or explicitly
  assumed residence-to-temporary-domicile flows, eligibility and turnout.
  Attribute votes to the legally applicable destination college, conserve
  electors/votes without double counting, and test geographic and joint-bonus
  sensitivity. Distinguish voting-population movement from legal census data
  used for seat apportionment.
