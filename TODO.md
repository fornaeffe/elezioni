# Project TODO

This is the active task list. Record completed work and evidence according to
[project history](docs/history/README.md). Order work by dependencies and update
tasks when the evidence changes; detailed execution plans belong near their work.

Keep in mind that links with a line number might point to the wrong line if, in the 
meantime, the linked file has been edited and the link has not been updated.

## Future work

- [ ] **UI polishing**
  - [ ] Review `$effect()` usage: check if the same (or better) behavior
    could be achieved with `$state()` and `$derived()` or other Svelte 5
    idiomatic patterns. If yes, change the code accordingly.
    - [ ] Result queries for general messages, affected simulation IDs and
      context, and election probabilities.
    - [ ] Navigation from a diagnostic to a simulation in the result tables
      and the reform seat chart.
    - [ ] Shared application status for regional operations and errors,
      politics execution and errors, and editor loading.
    - [ ] Regional draft persistence in `draft.svelte.ts`.
  - [ ] Rewrite models' names and descriptions in a more undersantable manner.
  - [ ] Rethink membership editor, component pooling editor and component reference
    editor:
    - [ ] Add short explanatory text under the first collapsible section of this
      group, describing the meaning of components and membership.
    - [ ] Separate future lists memberships editor from historical sources 
      memberships editor.
    - [ ] Change ["Rimuovi componente" button](web\src\lib\politics\editor\MembershipsEditor.svelte:48)
      into a compact icon button, moving the current text to `title` attribute.
    - [ ] Change ["Conferma aggiunta" button](web\src\lib\politics\editor\MembershipsEditor.svelte:58)
      into a compact icon button placed just right to the select, moving the 
      current text to `title` attribute. 
    - [ ] Change [membership lock checkbox](web\src\lib\politics\editor\MembershipsEditor.svelte:41)
      with a lock icon switch placed between Camera and Senato boxes, without
      visible text but with "Collega Camera e Senato" as `title`.
    - [ ] Change historical sources membership editor into a collapsible tree 
      view with levels:
        - source (in chronological order, with a visually clear distinction 
          between elections and polls)
        - option
    - [ ] Change ["Rimuovi membro" button](web\src\lib\politics\editor\MembershipsEditor.svelte:75)
      into a compact icon button, moving the current text to `title` attribute.
    - [ ] Rethink the presentation of 
      ["Provenienza"](web\src\lib\politics\editor\MembershipsEditor.svelte:85),
      making it more understandable.
    - [ ] In pooled component editor, list a pool's components one per row 
      instead of inline, and add, if possible, a button to remove one component.
  - [ ] In pooled component editor, open the cancelable new pool form in the
      shared dialog through a button.
  - [ ] Rename different components with same name
  - [ ] Review historical memberships
  - [ ] In "Ipotesi senza storico e riferimenti dei componenti" do not show
    controls for settings that are ignored, i.e. if a list has historical members,
    do not show controls for that list's new component without history. If 
    even component's references are ignored when a list has historical members,
    do not list that list in this section's selector at all.
  - [ ] Remove the collapsible "Dettagli" section from general warnings, that 
    currently contains only the warning code. Move the warning code at the end 
    of the warning description, in parentheses.
  - [ ] Do not make information messages collapsible. Move the info code at the
    end of the warning description, in parentheses, like warnings.
  - [ ] In "Riepilogo dei messaggi", under "Messaggio" column, 
    show only the error/warning/info title, with a collapsible (default collapsed)
    description of the message. The code should be at the end of the description,
    in parentheses.
  - [ ] Replace "COMMON_CONTRAST_FALLBACK" label with a more meaningful label.
  - [ ] Assess if "Dotazioni territoriali provvisorie" (`I08_PROVISIONAL_SEATS`)
    warning shoud stay both
    in general warnings and in per-simulation warnings, as currently implemented,
    or if it could stay only in general warnings.
  - [ ] `I12_TERRITORIAL_ELIGIBILITY` should be treated as a general information
    describing the adopted interpretation,
    non a per-simulation warning.
  - [ ] When clicking on "Simulazioni interessate" button, the "Simulazioni 
    interessate" section opens below the table, not necessarily in view of the 
    user. It is thus not obvious for the user that something happened. Rethink
    the behavior of this button and the location of "Simulazioni interessate"
    section.
  - [ ] Rethink the "Chiudi elenco delle simulazioni" button location and appearance
  - [ ] Move the "Scarica tutti gli ID filtrati CSV" button in the same line of
    "Filtra simulazioni", if width allows, and render it as an icon button with
    a meaningful `title`.
  - [ ] Rethink interested simulation "Messaggi di" and "Risultato di" buttons:
    "Messaggi di" should expand the simulation row with details, instead of 
    showing details at the bottom of the list (possibly out of view),
    "Risultato di" should also scroll the page to bring inspected simulation's
    results' section in view, with a "Torna su" button available (while still 
    meaningful).
  - [ ] Details opened clicking on "Messaggi di ..." button (or expanding the 
    affected simulation row, in the previous TODO is implemented) should be made
    more readable: develop a solution to translate syntaxes like 
    `same-list-other-territory: senato:territory:09 -> senato:territory:11; retained`
    or `["senato","ordinary","senato:P:090001","fdi",5]`
    into human-readable meaningful phrases.
  - [ ] When results are loading, "Vincitori del premio" shows "Nessuna 
    assegnazione del premio disponibile" as if there aren't any data available.
    If this message only represent a placeholder while data are loading, it should
    be substituted with an appropriate skeleton / loading placeholder. If it 
    represents both a real no data state and a loading state, it should stay for
    the no data state but the loading state should show an appropriate skeleton /
    loading placeholder. Check if this behavior is replicated in any other section,
    and correct it in the same way.
  - [ ] Rethink and rewrite these unclear terms and phrases, better explaining their meaning:
    - [ ] "Seleziona componenti interi etc..." (`web\src\lib\politics\editor\AdvancedEditor.svelte:38`)
    - [ ] "simulazioni conservate"
    - [ ] "Eletti per affiliazione personale finale; coalizioni per titolarità del seggio."
    - [ ] "Identità e recuperi restano condizionati ai dati/modelli delle preferenze."
    - [ ] `web\src\lib\politics\reform-2026\ReformTerritorialSpinogram.svelte:185-188`
    - [ ] `web\src\lib\politics\reform-2026\ReformTerritorialSpinogram.svelte:191-193`
  - [ ] In `web\src\lib\politics\PoliticsSummaryTable.svelte`:
    - [ ] remove `'Mean elected': 'Eletti medi'` column, and rename "Eletti confermati medi"
      in "Eletti medi"
    - [ ] Add a greater than or equal sign in front of values where the denominator
      includes unresolved simulations.
  - [ ] Uniform the style of result section filter: adopt the compact same-row
    of "Eletti medi e percentuali sui voti validi" also for "Percentuali per lista",
    "Probabilità di elezione", "Eletti per collegio o circoscrizione". Keep spanning
    over multiple rows when the width cannot accomodate all fields.
  - [ ] Invert colors in spinograms: darker colors = more candidates elected.
  - [ ] Make the "Percentuali per lista" boxplots expands its labels on large 
    screen, instead of clipping labels while leaving blank space on the right
  - [ ] Add web documentation or links to markdown documentation on GitHub.
- [ ] Some results (particularly "Probabilità di elezione") take a long time to
  load on some occasions. Investigate the results loading performances and check
  if there are any avoidable slowdowns, of if some cases need unavoidable long
  loading (more than 5 seconds as a rule of thumb) add a progress indicator.
- [ ] Add a majority result section: where the user could compose a majority 
  grouping lists, and the app should show the percentage of simulations in which
  that group owns the majority of seats, and what is the median seat count for 
  the selected group. Evaluate the feasibilty and logic of a list of suggested
  majorities, with their respective probability of reach the majority and median
  seat count.
- [ ] Add a per-fixed-candidate result section, showing for each fixed candidate
  the election probability, global and for each of its slots (if filed in more 
  than one slot).
- [ ] **Additional model drafts evaluation**. Do it in research\vote-generation 
  folder. For each model, follow the steps until completion or until evidence of
  model unusability.
  - [ ] **Deferred M06 research:** specify and validate its grouped-history endpoint
    law, missing/new-component support and full-list accuracy before browser acceptance.
  - [ ] Evaluate model draft [M08](docs/model-drafts/M08v0.0.1.md)
    - [x] Check model internal consistency and completeness ([review](research/vote-generation/m08-review.md))
    - [ ] Define remaining model details
    - [ ] Choose a research fitting framework/library/software
    - [ ] Fit the model with progressively complex synthetic fixtures, then with
      real data and default scenario, optimizing priors and fitting parameters;
      assess if model refactor will improve performace without changing the 
      underlying principles.
    - [ ] Generate, fit and optimize model variants (e.g. regional variation)
  - [ ] If the model completed evaluation successfully, 
    test the predictive capacity of successful model variants on the same metrics
    of the [previous research comparison](research/vote-generation/campaign-2026-11/protocol.md).
    Target performances comparable to M01 2.0.0 and M02 3.0.0. 
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
    - [ ] add the model to production alongside current accepted models, 
      adapting the pipeline and editor accordingly.
- [ ] Review Git-tracked sources and generated artifacts, recording their sizes,
  provenance and reproducibility. Define which belong in Git, which should be
  downloaded or regenerated, and how to preserve historical evidence and exact
  dependencies needed by saved scenarios. Document checksum/version review,
  mutable upstream endpoints, cache retention and clean-clone preparation.
  Apply resulting tracking-policy changes only after that review. See the
  17 November source-update entry in migration history.
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
  failure any more. Make regional result storage follow Common Politics' behavior.
  Then reassess the `result.storage` guard in `GeneralMessages`; if no problems
  arise, keep only the `messages-general` path.
- [ ] Correct meaningful local support for future filed lists without historical
  candidature/support, including Sud chiama Nord in Molise/Basilicata. Separate
  historical absence from future filing zeros; choose an explicit local estimate
  or scenario reference before implementation. I17 prevents the observed
  corrective zero-vote award without correcting generation; keep other zero-quota
  blockers. Luca rejected an automatic sole-college pass. See the
  [generation diagnosis](docs/politics-reform-2026-generation.md#filed-lists-without-local-history).
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

Follow the approved
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

The pooling editor remains in CP-06; do not create a duplicate implementation task.


