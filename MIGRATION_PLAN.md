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

- [x] Add the approved unique, fixed **Altre liste** category across legacy
  politics, reform politics and Emilia-Romagna editors, generation, scrutiny,
  persistence and exports. Follow the [category contract](docs/other-list.md);
  require candidate-free valid-vote accounting and unconditional seat exclusion.

- [x] Implement [reference-based bonus candidate selection](docs/politics-reform-2026-candidates.md),
  with supplied positions/declarations, legal ordinary reuse, explicit archive
  compatibility and regenerated production defaults; leave territorial-support
  estimation corrections to parameter preparation.
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
  I13 warned integer-seat donor fallback in Camera N16 constituency list and
  Camera/Senate college corrections,
  provisional I14 repeated initial Camera list remainder passes,
  I15-v2 disjoined Camera cap-group corrections, I16-v1 repeated used-decimal
  list correction recipients after unused decimals exhaust,
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
- [x] Investigate errors: if they represent genuine gaps in the regulatory texts,
  describe the gap in `docs\politics-reform-2026-legal-gaps.md`, let Luca decide
  a provisional workaround, and apply that workaround with a warning for affected
  simulations. If they instead are situation fully covered by the regulatory
  texts and the errors come from our implementation, correct the implementation
  following the regulatory text. Investigated errors and approved resolutions:
  - [x] LG03—college capacity versus upstream territorial entitlement: apply
  warned I15-v2 disjoined Camera cap-group corrections; retain initial C3 quotas
  and upstream entitlements. See the legal-gap register for scope and limits.
  - [x] N18 `no-eligible-recipient-remainder`: LG04 handled by warned I16-v1
    repeated descending used-decimal passes after unused recipients exhaust.
  - [x] N21/N22 `no-eligible-recipient-remainder`: apply the same I16-v1 to
    ordinary/internal cap lists; preserve upstream targets and recovery states.
  - [x] LG05 N16 integer-only surplus donors: extend warned I13 while retaining
    ordered surplus lists and existing recipient rules; do not extend I16.
  - [x] LG06 observed zero-vote cap-group chain: apply warned I17 only to
    Camera cap-path N16 corrective recipients with zero constituency votes;
    search other recipients and retain explicit exhaustion. Other zero-vote
    seat cases require separate review.
- [ ] Correct meaningful local support for future filed lists without historical
  candidature/support, including Sud chiama Nord in Molise/Basilicata. Separate
  historical absence from future filing zeros; choose an explicit local estimate
  or scenario reference before implementation. I17 prevents the observed
  corrective zero-vote award without correcting generation; keep other zero-quota
  blockers. Luca rejected an automatic sole-college pass. See the
  [generation diagnosis](docs/politics-reform-2026-generation.md#filed-lists-without-local-history).
- [x] Align pre-2026 reform and 2026 reform UI:
  - [x] adapt the pre-2026 reform scenario editor UI for the 2026 reform,
    with row-local chamber presence, editor-local reference/history scopes,
    persisted presentation metadata, filings, bonus links and compatible
    candidate/planner previews. Supplied data remains JSON; legacy conversion
    UI is deferred. See the [editor guide](docs/politics-reform-2026-editor.md).
  - [x] Reconcile partial reference shares as supplied constraints, retaining
    unspecified local proportions and disconnected special-ballot capacity;
    preserve turnout, filing zeros, chamber derivation and scope precedence.
  - [x] adapt tables and plots from pre-2026 to 2026 reform, where applicable, especially:
    - mean elected candidates per list, paired with mean percentages over valid votes
      (if possible, show all elected candidates, both ordinary and bonus, where pre-2026
      plot showed only candidates elected in plurinominal colleges).
    - simulated percentages boxplots
    - elected vs percentage xy plot, one plot per chamber * list and one
      plot per chamber * coalition
    - elected vs percentage spinograms, one plot per chamber * list and a
      selectable chamber * list * college plot, as in pre-2026 plots
  Shared chart rendering, bounded statistical queries, explicit coverage and
  archive compatibility are implemented. See the
  [results guide](docs/politics-reform-2026-results.md) for counting definitions
  and saved-archive limits; verification is in migration history.
  - [x] Present global conditions first, then message frequencies by severity/code,
    then affected simulation IDs on demand. Preserve exact context, requested
    denominators and explicit legacy-archive limits. See the
    [messages guide](docs/politics-reform-2026-messages.md).
- [x] **Poll source preparation and earlier forecasting research.** Follow the
  [poll preparation guide](docs/polls-preparation.md). Prioritize simulation
  computability: fill missing mappings, splits and territorial support with
  explicit, versioned estimates/proxies and retain their provenance.
  - [x] Import and reconciliation pilot: maintain an authoritative JSON archive
    with government survey references for optional manual audits (no tracked
    PDFs or document checksums),
    retain reviewed source/question metadata and normalized observations,
    reproduce preparation offline, reuse verified historical correspondences,
    and test mass-conserving reconciliation including estimated fallbacks.
  - [x] Poll-informed references: combine historical baselines and compatible
    polls, preserve user overrides and chamber/geographic semantics, and add
    reference-date measurement uncertainty separately from future movement.
  - [x] Historical calibration pipeline: retain matched past-election polls and
    official results, fit partially pooled house/election-error candidates, and
    compare chronological held-out forecasts and intervals, including original
    national uncertainty and separately fitted floors for history/poll-only
    benchmarks. Follow the
    [calibration guide](docs/polls-calibration.md); retain explicit rejection of
    corrections that lack support.

  Earlier fitted corrections remain rejected; these completed source/research
  steps do not establish calibrated forecasts. Production vote/poll fitting and
  forecast acceptance now belong to FV-01 through FV-08 below. Preserve the
  reviewed source JSON and promote its required readers before research archival.

- [ ] **Fast vote-share model in production: 2026 reform only.** Follow the
  [browser production plan](docs/vote-generation-production-plan.md). Periodic
  Node/TypeScript preparation must deliver compact editable history and a default
  prefitted model; the browser reuses it for scenario composition and can refit
  after poll, membership or reference-date edits. Earlier research evidence is
  in [history](docs/migration-history.md#2026-10-27--resumed-party-level-research-with-a-fast-share-model)
  and the [approved report](docs/vote-generation-fast-research.md).
  - [x] Select the fast `kind-common-conservative` model for this implementation.
    Original joint-state/Stan research gates no longer block the selected model.
  - [x] **FV-01:** Freeze production scenario/model/reuse contracts, numerical and
    acceptance protocols; preserve the research inventory/evidence. Read the
    [maintained model](docs/vote-generation-fast-model.md),
    [scenario/data contract](docs/vote-generation-fast-contracts.md),
    [reference/chamber rules](docs/vote-generation-fast-references.md),
    [validation protocol](docs/vote-generation-fast-validation.md) or
    [evidence index](research/vote-generation/README.md) for the specific task.
  - [x] **FV-02:** Port fitting/profile estimation to one browser-safe TypeScript
    engine; pass independent reduced numerical and browser-computability gates.
    Use the [engine guide](web/src/lib/politics/reform-2026/vote-share/README.md).
  - [x] **FV-03:** Produce compact source-backed historical/poll records supporting
    editable memberships/dates, with complete reform geography and Senate inputs.
    Use the [source preparation guide](docs/vote-generation-fast-data.md).
  - [x] **FV-04:** Implement fast generation, reference interventions, physical-cell
    filing/projection and paired Camera/Senato integration through existing scrutiny.
    Use the [generation and Senate adapter guide](docs/vote-generation-fast-generation.md).
  - [x] **FV-05:** Prefit the explicit default scenario and package matching
    versioned data/defaults/fit/catalog through clean-clone local/CI commands.
    Use the [candidate release guide](docs/vote-generation-fast-release.md).
  - [x] **FV-06:** Add cancellable browser refitting, semantic invalidation,
    compatible-state installation and bounded worker memory/ownership.
    Use the [browser fitting guide](docs/vote-generation-fast-fitting.md).
  - [x] **FV-07:** Deliver component/list editing, reference controls and advanced
    poll/membership/date editing with demand loading and ready/error states.
    Defer background prefetch/ready notifications and persistent cache management.
    Use the [candidate workspace guide](docs/vote-generation-fast-workspace.md).
  - [ ] **FV-08:** Pass full-list/geographic/paired/seat, browser/export and resource
    acceptance, then switch reform defaults. Keep public deployment at the final gate.
    Acceptance remains open. Preserve the earlier restricted-filing capacity
    failures as historical evidence; use the [FV-08 guide](docs/vote-generation-fast-acceptance.md)
    and the authorized replacement comparison below for the current candidate.
    Accept routing of existing v1-v3 reform archives to their original generator
    when the v4 workspace becomes the installed default; do not convert their model.
    Resolve maximum-envelope refit latency or approve measured support limits;
    reduced FV-02 acceptance does not accept the complete production envelope.
    - [x] **Authorized replacement comparison (30 October):** first install Luca's
      24-list future choice set; then implement/test feasible candidates against
      historical elections and polls, prioritizing plausible municipality/college
      tails, coverage and mean accuracy, with a 30-minute browser budget. Use the
      [active research execution guide](docs/vote-generation-feasible-research.md).
    - [ ] Compare the selected candidate with the installed R-derived generator.
      Freeze reviewed correspondences, new-list support and reference inputs;
      separate future-scenario checks from historical accuracy. Follow the
      [legacy comparison assessment](docs/vote-generation-legacy-comparison-plan.md).
  - [ ] **FV-09:** Execute the [research archive/cleanup plan](docs/vote-generation-research-archive-plan.md)
    after dependency separation and acceptance; retain curated evidence and remove
    only audited, regenerable outputs no longer consumed.
- [ ] Adopt the [desired default scenario](docs\politics-reform-2026-default-scenario.md) for all politics workspaces.
- [x] **Refine UI**
  - [x] Restore the bundled reform scenario with an explicit confirmation,
    preserving calculated results and discarding pending editor drafts.
  - [x] Show one plot at a time per chart family and offer a selector:
    - For mean elected and mean valid votes percentage plot, offer a control to switch between camera and senate
    - The same for simulated percentage per lists boxplots
    - For elected vs percentage xy-plots, offer controls for
      - lists or coalitions/single lists
      - Camera or Senato
      - list (or coalition/single list)

      With these controls, it will be unnecessary a separate section for coalition xy-plots
    - The same as above for spinograms
  - [x] Add an election probability table: the user selects list and chamber,
    the UI will plot a table with one college per row and seven columns (one per list position) plus college name column. The list position columns represents, from left to right:
    - the election probability for _capolista_
    - the election probability for the first candidate in decreasing preference order
    - the election probability for the second candidate in decreasing preference order
    - the election probability for the third candidate in decreasing preference order
    ...and so on
    Election probability includes:
    - a candidate elected because a seat is assigned to this college
    - a candidate elected because a recovery selected this college
    Election probability does not include a candidate elected in another college,
    but also present in this list.
  - [x] UI state (idle/loading/calculating...) should be presented in a collapsible
    compact fixed overlay near a corner of the window.
  - [x] UI messages (e.g. scenario editor errors) should be presented as dismissable
    toasts. Toasts should be dedicated to interaction messages: result's infos, warnings
    and errors should stay in the result section.
  - [x] Extend the current mean elected / mean valid votes plots for coalitions/single lists: offer a selector to switch between lists view and coalitions/single lists view.
  - [x] Extend the current simulate percentages boxplots for coalitions/single lists,
  with selector as above.
  - [x] add a bonus winner plot: a barplot with a bar for each coalition
    or autonomous list representing the percentage of simulations in which that coalition
    won the bonus. Aside the bar there should be the percentage, as in existing mean valid
    votes plots.
  - [x] change the college selection for the spinogram of elected in a single college:
    make it a three-part selector: one for the chamber, one for the college, one for the list. When possible, changing the college should not change the list (unless, e.g.,
    the list is not present in the new college).
  - [x] Filter the single-college spinogram by all simulations, bonus awarded,
    or bonus not awarded; display the plotted and selected simulation counts.
  - [x] Offer collapsed advanced controls for an independent college/list share
    on the X axis; keep the college plot outside any nested collapsible section.
  - [x] move the single simulation seats plot in a collapsible section
  - [x] better describe the meaning of columns in table of elected and shares
- [x] add the table of seat endowment per each college with and without bonus
    and per each constituency's bonus slate.
- [ ] edit the single college spinogram to let the user choose between a single
    college and a whole constituency.
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
- [ ] Add poll editing to remaining election workflows; reform poll editing is
  included now in FV-07 and does not wait for this post-migration extension.
- [ ] Let the user enter fixed vote counts and perform a single scrutiny (e.g. applying a new scrutiny algorithm to a previous election result, possibly adapted; or setting national fixed shares and choosing a deterministic algorithm to calculate the
local votes)
- [ ] Evaluate modelling of the $0$ component from poll's "will abstain" answers.
- [ ] Evaluate explicit modelling of party cessation date (i.e. a ceased party
  is not included in poll's "other" component). Discuss the date to which priors
  should refer to, for parties ceased at the reference date.
- [ ] Migrate the municipal workflow: create R golden fixtures and benchmarks
  first, preserve current behavior, and keep the runoff and councilor-candidate
  gaps explicit for later business and legal review.
- [ ] Add short-term forecasts of the **current election's final scrutiny result**
  from incomplete incoming returns. Keep this separate from forecasting subsequent
  elections and from the historical-result availability fallback. Preserve timestamped
  section/municipality snapshots, reporting completeness and denominator consistency;
  model geographically uneven reporting, missing returns and revisions without
  treating missing counts as zero. Validate uncertainty and final-result predictions
  on chronological counting snapshots before exposing a dedicated live-count workflow.
