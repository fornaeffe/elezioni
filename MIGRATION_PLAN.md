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
- [ ] **Poll-informed vote-share preparation and forecasting.** Follow the
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

  Forecast acceptance remains open: expand Camera holdouts, reassess historical
  prior strength and validate intervals in the full generator before enabling
  corrections or treating opt-in poll scenarios as calibrated forecasts.
  Use the [uncertainty study](docs/polls-uncertainty.md) for tested joint endpoint
  candidates. The [revised drift proposal](docs/vote-generation-drift-model.md)
  instead derives national results from joint municipal elector outcomes;
  sampled-endpoint reconciliation is not its invariant. Retain filing,
  denominator, override and chamber semantics.

- [ ] **Vote-generation redesign: electorate-wide latent national/local drift.**
  Follow the [current formulation and staged comparisons](docs/vote-generation-drift-model.md)
  and the authorized [fast model alternative](docs/vote-generation-fast-research.md).
  Preserve editable, source-specific correspondence defaults and the observed
  count layer; keep state/process uncertainty distinct from polling measurement.
  - [x] Complete the [earlier endpoint comparison](docs/vote-generation-research.md)
    and six-category sensitivity. Luca has not selected those models for adoption.
  - [x] Correct the drift formulation and record the accepted observation/polling
    requirements, comparison hierarchy and confirmed combined non-valid baseline.
  - [x] Implement the earlier fixed-outcome research fit/backtest path using compatible
    Camera/European elections and polls. Complete stages 1–6 and both separate
    stage-7 branches, retaining the conditional stage-5 trigger. See the
    [measured comparison and limitations](docs/vote-generation-drift-results.md)
    and [reproduction protocol](docs/vote-generation-drift-testing.md).
  - [x] Approve the party-level correspondence revision: scenario-owned complete
    memberships, grouped list observations, choice-set redistribution and
    aggregate reference-share constraints; record it in the current model contract.
  - [x] Specify weakly informative reference-date priors: exchangeable centered
    political contrasts, expected non-valid share 0.5, backward historical walks
    and prior reset/refitting whenever the scenario reference date changes.
  - [x] Add the research-only membership/count kernel and reduced stage-0
    posterior-validation harness, separately from production generation.
  - [x] Require cutoff-available distinguishing information for requested
    components; recheck the restricted class, alternative estimation and
    reduced Stan reference. Record the [follow-up](docs/vote-generation-party-drift-restricted.md).
  - [x] Prepare a minimal provisional source-backed joint-list membership table
    for review and the authorized admissible chronological research panels.
  - [ ] Pass the original joint-state model's real-data stage-0 posterior gate
    before its full-data fitting and stages 1–7. Retain the 30-minute limit for approximate alternatives;
    the authorized final offline Stan fallback has that time gate waived.
    Research resumed on 27 October with authority to simplify the model while
    preserving forecast quality and historical/joint/new-list scenario support.
  - [x] Implement and validate the fast bounded reference-share research kernel,
    mean-preserving forecast dynamics and scenario composition prototype.
  - [x] Complete the frozen fast candidate's corrected predictive replays and
    complete compatible municipal benchmark; record approximation, source-fix,
    pooled-outcome, mean-error and sparse-event limits in its research report.
  - [ ] Review predictive quality, the provisional membership table and fuller
    list/source coverage before selecting a production design. The fast candidate
    is computationally practical; universal calibration and unbiased means are
    not established by the six-election research panel.
  - [ ] Implement the versioned membership/scenario contract and validated grouped
    fitting/posterior reuse and invalidation for the selected engine. Offline Stan
    requires preparation after semantic edits; it does not supply browser refitting.
    Revalidate split uncertainty, prior symmetry/participation calibration,
    historical-window invariance, reference-date invalidation, poll Other and
    hard/soft reference inputs; repeat the staged comparisons and measure
    fitting accuracy, memory and latency before runtime adoption. Browser fitting
    requires its own acceptance if retained as an implementation candidate.
  - [ ] After revalidation and selection, implement the approved runtime design with full-list
    split uncertainty, dated geography/electorates, compatible overrides,
    filing support and paired-chamber dependence. Derive national aggregates
    from municipal counts; do not force them to softmax of the national factor.
  - [ ] Validate full-list/municipal/regional and seat distributions before changing
    defaults; keep sparse-event, new-party and long-horizon limitations visible.

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
- [ ] Let the user add polls
- [ ] Let the user enter fixed vote counts and perform a single scrutiny (e.g. applying a new scrutiny algorithm to a previous election result, possibly adapted; or setting national fixed shares and choosing a deterministic algorithm to calculate the
local votes)
- [ ] Evaluate modelling of the $0$ component from poll's "will abstain" answers.
- [ ] Evaluate explicit modelling of party cessation date (i.e. a ceased party
  is not included in poll's "other" component). Discuss the date to which priors
  should refer to, for parties ceased at the reference date.
- [ ] Migrate the municipal workflow: create R golden fixtures and benchmarks
  first, preserve current behavior, and keep the runoff and councilor-candidate
  gaps explicit for later business and legal review.
