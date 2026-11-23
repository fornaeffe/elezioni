# Legacy Workflow Contracts and Verification

These retained contracts apply to the legacy election workflows and their parity work. For the common politics and reform workflows, follow the linked model-specific contracts. Historical R behavior is not an authorization to change legal behavior.

## Working principles


- Treat scrutiny functions (`scrutinio...`) as the highest-risk code. Preserve
  frozen historical behavior in golden-master tests before refactoring it.
  Consult the isolated pre-retirement revision for any deferred translation.
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
  [`data/README.md`](../data/README.md). Migrate remaining historical importers as
  their simulator/model inputs are implemented. Keep all preparation a
  non-interactive Node/TypeScript pipeline, reusable locally and in GitHub
  Actions, producing versioned static bundles across election kinds. See the
  [preparation delivery contract](../data/README.md#github-actions-delivery-contract).

## Scenario Semantics

These rules describe the existing election workflows. For the new 2026 reform
vote-share engine, the maintained [model](vote-generation-fast-model.md) and
[scenario contracts](vote-generation-fast-contracts.md) supersede factor-based
historical splits, unmapped-to-abstention inference and the old uncertainty
construction. Keep the other workflows' semantics intact.

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
- In original projection paths, project historical correspondences per municipality
  and election. Sum many
  original lists mapped to one future list; split one original list among future
  lists using normalized factors; count original abstention and unmapped
  original-list votes as `astensione`. Reform v5 uses the distinct model-specific
  [choice-set policies](vote-generation-unified-workflow.md#model-boundary-and-restored-semantics).
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
  complete. Preserve frozen regression fixtures when changing parity boundaries;
  new legal behavior requires reviewed expectations. Run browser benchmarks
  when changing performance-sensitive code.
- Run `cd web; npm run build` and `cd web; npm run test:e2e` sequentially. They
  both touch SvelteKit build/prerender output and can race on hashed assets.
- Legacy politics and regional generation use 50-simulation chunks. Legacy
  politics retains its 1000-run plot limit. Reform uses bounded per-simulation
  compute workers and local result storage without an arbitrary run cap.
- Record commands, counts, timing, environment, and outcomes in
  [project history](history/README.md); do not turn the task list into a status log.

