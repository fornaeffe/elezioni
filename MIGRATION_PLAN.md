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

Current reform vote-generation contracts are [M01 2.0.0](docs/models/M01.md)
and [M02 2.0.0](docs/models/M02.md), with M02 as default. Model-law versions,
numerical policies, source/scenario identities and implementation hashes remain
separate. Completed tasks and their verification are recorded in migration history.

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
  [`data/README.md`](data/README.md). Migrate remaining historical importers as
  their simulator/model inputs are implemented. Keep all preparation a
  non-interactive Node/TypeScript pipeline, reusable locally and in GitHub
  Actions, producing versioned static bundles across election kinds. See the
  [preparation delivery contract](data/README.md#github-actions-delivery-contract).

## Scenario Semantics

These rules describe the existing election workflows. For the new 2026 reform
vote-share engine, the approved [production plan](docs/vote-generation-production-plan.md#model-and-scenario-contracts)
supersedes factor-based historical splits, unmapped-to-abstention inference and
the old uncertainty construction. Keep the other workflows' semantics intact.

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
  [choice-set policies](docs/vote-generation-unified-workflow.md#model-boundary-and-restored-semantics).
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
  `docs/migration-history.md`; do not turn this file into a status log.

## Plan

- [x] Adopt the [desired default scenario](docs\politics-reform-2026-default-scenario.md) for all politics workspaces.
- [x] Generalize routing, snapshot selection, scenario defaults, validation,
  worker dispatch, result components, and shared UI patterns across election
  kinds. Preserve configurable project-base navigation and worker/data loading
  for GitHub Pages, including direct page loads and refreshes. Use the
  [workspace task map](docs/web-workspaces.md) for implementation entry points.
- [x] Finish data-preparation migration on the shared R26-07 acquisition,
  geography and census foundation. Produce the shared workspace descriptors and
  defaults through their existing compiler contracts. Migrate historical election importers and
  static previous-election bundles; use R26-19 for required paired Senate history
  and calibration. Deliver all production bundles/defaults from a clean clone
  through the Node/TypeScript pipeline, with cache misses supported and required
  data checks blocking release. Adopt the definitive 2022 geography and shared
  legal-census reference where applicable; apply the Bolzano overlay only to
  reform. Use the [complete production pipeline](docs/data-preparation.md).
- [x] Check if production acceptance tests can be pruned and/or made faster.
- [x] Retire temporary bridge artifacts and update user and developer
  documentation when migrated workflows no longer depend on R-exported
  intermediary snapshots, including the retained R scripts and `dati/`.
- [ ] Merge the politics workspaces in a single workspace: switching generator
  and/or scrutiny algorithm should be as simple as choosing an option in a select.
  Read the [guidelines](docs\common-politics-scenario\guidelines.md).
  - [x] CP-01 — Inventory and preserve behavior: [field/control/asset mapping](docs/common-politics-scenario/inventory.md)
    and [reproducible baselines](docs/common-politics-scenario/baselines.md).
  - [x] CP-02 — Establish the shared domain contract: [domain API and boundaries](docs/common-politics-scenario/contract.md).
  - [x] CP-03 — Adapt existing execution paths: [execution API and adapters](docs/common-politics-scenario/execution.md).
  - [x] CP-04 — Replace authored-default construction, including approved colors: [authored input and packaging](docs/common-politics-scenario/defaults.md).
  - [x] CP-05 — Enforce new-format files and draft isolation: [strict file API and draft boundary](docs/common-politics-scenario/files.md).
  - [x] CP-06 — Build the common editor within the approved D14 perimeter: [editor components and review route](docs/common-politics-scenario/editor.md).
  - [x] CP-07 — Unify lifecycle, persistence and results: [controller, caches, durable sessions and resources](docs/common-politics-scenario/lifecycle.md).
  - [ ] CP-08 — Verify the supported pairs and complete merger acceptance.
    Retarget superseded workspace browser suites before the full production gate;
    measure national adapter resources and retain the agreed campaign-size restriction.
- [x] Change default scenario colors to colors that reflects the real parties
  political colors (or colors with which polling houses usually represent parties).
- [x] In "Dotazioni dei seggi per collegio e liste del premio" do now show territory ids,
  only names.
- [x] Bonus winner, if any, is the same for both chambers: do not offer a chamber selector
  for "vincitori del premio" barplot.
- [ ] Move application status to a small fixed, collapsible box near the top
  right corner of the window, or other noninvasive but easily reachable location.
- [ ] Assess if parallelization will improve import/export performances.
  Add progress indication, and parallelization if it will improve performances.
- [x] Color cells of election probability table with a more saturated color the higher
  the probability is (with the list/coalition color). Change font color to achieve adequate contrast.
- [ ] Add a selector for the election probability table metric:
  - current metric (default)
  - current metric, but conditional to the presence in the college slate of at least one candidate also present in a bonus slate
  - current metric, but conditional to the absence in the college slate of any candidate also present in a bonus slate
  The two latter metrics should also state, for each row, the denominator (the number of simulations over which the probability is computed).
- [ ] **Additional model drafts evaluation**. Do it in research\vote-generation 
  folder. For each model, follow the steps until completion or until evidence of
  model unusability.
  - [ ] Evaluate model draft [M04](docs\model-drafts\M04v0.0.1.md)
    - [ ] Check model internal consistency and completeness
    - [ ] Define remaining model details
    - [ ] Choose a research fitting framework/library/software
    - [ ] Fit the model with progressively complex synthetic fixtures, then with
      real data and default scenario, optimizing priors and fitting parameters;
      assess if model refactor will improve performace without changing the 
      underlying principles.
    - [ ] Generate, fit and optimize model variants (e.g. regional variation)
  - [ ] Evaluate model draft [M03](docs\model-drafts\M03v0.0.1.md)
    - [ ] Check model internal consistency and completeness
    - [ ] Define remaining model details
    - [ ] Choose a research fitting framework/library/software
    - [ ] Fit the model with progressively complex synthetic fixtures, then with
      real data and default scenario, optimizing priors and fitting parameters;
      assess if model refactor will improve performace without changing the 
      underlying principles.
    - [ ] Generate, fit and optimize model variants (e.g. regional variation)
  - [ ] If at least one of the two model completed evaluation successfully, 
    test the predictive capacity of successful model variants on the same metrics
    of the [previous research comparison](research\vote-generation\docs\vote-generation-legacy-comparison-results.md).
    Use all available Camera, Senato 18+ and European elections, and polls.
    Focus mainly on Error30, Coverage90, MAE and WIS, averaged across all folds,
    on college, constituency and whole election scales. Target a Error30 < 1%,
    Coverage90 > 80%, MAE < 5 pp, WIS < 0.05. 
  - [ ] If any draft model fail this test,
    check if its predictive capacity can be optimized, if yes optimize and
    repeat the test.
  - [ ] If at least one model passes the test:
    - [ ] check if it can be made running in browser. If browser fitting is 
      feasible, plan for a whole browser computation;
      otherwise plan for a once-in-a-while data-preparation fitting and a prefit
      only browser simulation. Raise the computational limit to 12 hours for 
      fitting + simulation,
      30 minutes for simulation with default prefit, on this machine, with 
      default scenario and an appropriate number of workers. Optimize for 
      performance as much as possible without noticeably changing generated 
      distributions, even if the computational time is under the limit.
    - [ ] substitute in production M01, M02
      and legacy previous-law and regional generators with the
      successful model set, adapting the scenario editor accordingly.
  - [ ] If neither model passes the test, use M01 and M02 generators also for 
    previous law politics and regional. Create
    adapted versions of M01 and M02 if necessary, reusing common logic.
  - [ ] Clean the repo removing any regenerable artifact no longer needed in 
    production.
- [ ] Align politics previous-law and regional contracts, editor, worker 
  behavior and results 
  presentation to reform counterparts: only unavoidable differences due to 
  different law should be accepted. This should resolve the previous-law Camera flipper dead end exposed by the shared
  pre-preparation-migration default (seed `politiche-2027`, 1,000 requested runs,
  simulation 584): it should be treated as uncomputable run, and the rest of
  simulations should proceed unperturbed. See
  [workspace boundaries](docs/web-workspaces.md#defaults-and-compatibility) and
  the 2 November workspace entry in migration history. Note that if the generator
  has changed since the failure discovery, the same seed may not generate the 
  failure any more.
- [ ] Correct meaningful local support for future filed lists without historical
  candidature/support, including Sud chiama Nord in Molise/Basilicata. Separate
  historical absence from future filing zeros; choose an explicit local estimate
  or scenario reference before implementation. I17 prevents the observed
  corrective zero-vote award without correcting generation; keep other zero-quota
  blockers. Luca rejected an automatic sole-college pass. See the
  [generation diagnosis](docs/politics-reform-2026-generation.md#filed-lists-without-local-history).
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
- [ ] Add poll editing to remaining election workflows. For common politics,
  follow the [D14 editor tasks below](#deferred-common-politics-editors-d14);
  the current reform editor predates that merger's approved deferrals.
- [ ] Let the user enter fixed vote counts and perform a single scrutiny (e.g. applying a new scrutiny algorithm to a previous election result, possibly adapted; or setting national fixed shares and choosing a deterministic algorithm to calculate the
local votes)
- [ ] Evaluate modelling of the $0$ component from poll's "will abstain" answers.
- [ ] Evaluate explicit modelling of party cessation date (i.e. a ceased party
  is not included in poll's "other" component). Discuss the date to which priors
  should refer to, for parties ceased at the reference date.
- [ ] Implement the municipal workflow using the pre-retirement revision pinned
  in migration history as the historical reference. Freeze municipal golden
  fixtures and benchmarks in an isolated historical checkout before translation;
  preserve behavior and keep runoff and councilor-candidate gaps explicit for
  later business and legal review. The active checkout does not require R.
- [ ] Add short-term forecasts of the **current election's final scrutiny result**
  from incomplete incoming returns. Keep this separate from forecasting subsequent
  elections and from the historical-result availability fallback. Preserve timestamped
  section/municipality snapshots, reporting completeness and denominator consistency;
  model geographically uneven reporting, missing returns and revisions without
  treating missing counts as zero. Validate uncertainty and final-result predictions
  on chronological counting snapshots before exposing a dedicated live-count workflow.

### Deferred common politics editors (D14)

Implement these after the web migration, following the approved
[D14 editor perimeter](docs/common-politics-scenario/guidelines.md#history-and-advanced-controls-d14).
The merger must preserve, validate and execute their scenario settings without
requiring these UI editors. Keep model-specific meanings and inactive values;
use structured controls without an embedded raw JSON editor.

- [ ] Add historical-source/poll editors: supported poll additions and edits,
  selection and exclusions with reasons, citation/method/availability metadata,
  and scenario-owned observations without modifying bundled originals. New
  historical election datasets remain a data-preparation task.
- [ ] Add model-specific historical-selection controls: show M01's automatic
  latest eligible Camera/Senato pair and eligible Camera polls; expose M02's
  selected historical elections, paired sources and supported poll contexts.
  Do not turn M01's automatic policy into an arbitrary source selector.
- [ ] Add supplied-vote/preference editors under the applicable law, organized
  by chamber, college and list/candidate. Cover every retained representation,
  validity evidence and provenance; show which generated inputs are replaced,
  restore generation on removal, and preserve missing versus explicit zero.
- [ ] Add model-setting/profile editors: retained M02 forecast submethods,
  scale, regional profile correlation and reference mode; chamber-dependence
  and reform preference profiles; M01 partial named observations with
  add/edit/remove/enable controls, distinct from complete M02 observations.
  Select registered profiles rather than adding a general profile-definition
  editor; keep version identities read-only.

The pooling editor remains in CP-06 because the current default uses pooling.
Move that editor to this post-migration list only if CP-01 finds that the
authored default no longer uses it; do not create a duplicate implementation task.
