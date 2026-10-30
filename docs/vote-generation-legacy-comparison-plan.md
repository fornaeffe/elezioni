# Production legacy comparison: feasibility and decisions

Assessment: 30 October 2026. Compare the selected filing-aware candidate with
the **installed reform generator** in `paired-vote-generation.ts`, the TypeScript
implementation of the R-derived approach with paired-chamber extensions.
This is a benchmark plan; generator/defaults and correspondences are unchanged.
The earlier `legacy-global-beta-v1` comparator is a fast-model predecessor,
**not this production legacy generator**.

## Feasibility and verified interfaces

Use the unchanged `preparePairedVoteReferences()` and
`generatePairedMunicipalVotes()` functions through an isolated replay adapter.
For future paired/legal runs, reuse `prepareReformGeneration()` and
`generateReformSimulation()`. The older `generation-baseline.ts` adapter provides
precedent, but its four fixed categories and reference clocks do not implement
this full-list comparison.

An in-memory translation of the new 24-list definitions, filing, minority flags
and bonus links passes legacy scenario validation with legacy-format presentation
metadata. No municipality has different future filing masks across its colleges.
This removes one obvious capacity concern, but does not prove every generated
run. The structural probe has no correspondence assumptions and is not a
completed forecast scenario.

The actual installed default has schema 2, 22 Camera/25 Senate lists, a 2022
baseline, no supplied share overrides and **no active poll references**. The
engine supports optional schema-3 poll-derived reference points and measurement
errors. Its national/regional/local logit scales are assumed constants,
0.02/0.01/0.01 per square-root day, not fitted historical movement parameters.

Small Node 24.20.0 probes of that installed default completed three full paired
municipal draws in 278.6/270.9/268.1 ms after 438.6 ms preparation. A separate
full default smoke took 781.3 ms preparation, 645.0 ms generation/assembly/
preferences and 384.3 ms scrutiny, with all 588 seats filled. These are
**existing-default Node probes**, not new-24-list timings, browser measurements
or proof of the 30-minute budget. No computational blocker was found; run a
reduced browser pilot before the complete 512-run comparison.

## Two test panels

The preceding candidate tests already distinguish:

1. **Future scenario:** the exact requested 24 lists, coalitions, Camera filings,
   corresponding whole Senate regions and minority declarations. Compare loading,
   preparation, 512 paired draws, municipal/college distributions, conservation,
   filing zeros, failures, seats and browser time. There is no observed next-
   election truth for predictive accuracy scoring.
2. **Historical accuracy:** Camera 2013/2018/2022 at 730/365/90/15 days, with
   512 draws, the same municipalities, college crosswalks, actual historical
   observable groups, metrics and equal-unit weights. Reuse the full 2018/2022
   short-horizon domains and their stated omissions. A literal 2026 ballot does
   not supply historical truth for its new parties or unobserved internal splits.
   Coarsen actual reported groups; never manufacture truth by splitting them.

Historical folds need cutoff-specific snapshots from the existing original-data
sources. The current production snapshot is fixed at September 2022 and cannot
predict earlier elections. Use the latest eligible previous Camera result for
the faithful Camera baseline; importing the new model's fitted geographic
profiles would change that procedure. Match the earlier tests' conditional
electorate/geography choices for scoring. Operational baseline-electorate
forecasts would be an additional experiment. Retain the same Camera accuracy /
Senate functional distinction as the preceding study.

## Decisions before numerical comparison

| Decision | Consequence and recommendation |
| --- | --- |
| Polls and information | Use the installed **no-poll** procedure as the primary legacy reference, as requested. The selected candidate retains its polls. This compares complete procedures, not only transition laws. Optionally add a candidate-without-polls ablation or a separately labelled legacy run using common cutoff reference points. Borrowing poll-informed candidate points is not a poll-free forecast. |
| Joint historical lists | Freeze reviewed factors before scoring. Azione–Italia Viva 2022 splits between two future lists/coalitions; Noi Moderati–UDC also separates. Impegno Civico–CD, ISP/DSP and grouped residuals require explicit treatment. Legacy weights are deterministic assumptions, not the candidate's uncertain decomposition. With insufficient evidence, use a predeclared sensitivity instead of tuning factors against held-out results. |
| New or unobserved support | Exact zeros remain zero under legacy generation. FN and Ora need reference support or a declared donor proxy to receive positive forecasts. Progetto Civico/Avanti has no separately identified 2022 share. Team K cross-chamber support, STF and broad VdA tickets need explicit treatment. Choose a faithful historical-only baseline retaining these limits, a scenario-assisted baseline with reviewed point references/proxies, or report both separately. |
| Unmapped support and turnout | Preserve production semantics in the primary baseline: unmapped history and mapped votes outside future filing become combined non-valid support. The candidate redistributes unassigned historical-rest over offered political choices, with separate non-valid inputs. Report valid-vote and elector-denominator scores plus excluded historical mass. Matching turnout through extra interventions is a separate controlled variant. |

Straightforward identity links include FdI, FI, Lega, M5S and the reviewed PD
group. Wider historical tickets remain labelled proxies when used as future
party support. Ordinary list IDs and special ballot-group IDs can share a
printed label: key the actual matrix by event, chamber and source identity.
Do not mechanically assign all votes of a group containing historical-rest to
its remaining named party.

Legacy factors are normalized across **all mappings of a historical row before
filing is applied**. A lone factor of 0.3 sends 100% of that row to its future
list, not 30%. It cannot by itself represent a discarded unidentified remainder;
do not add a fictitious future list to conceal that limitation. Share overrides
are point values: the candidate's reference SDs and joint uncertainty cannot be
copied unchanged. A sampling wrapper would be a separately labelled variant.

## Dates and interpretation

Without polls or supplied references, drift starts at the previous **election
date**. Changing `projectionDate` alone does not refresh support or shorten the
interval. With the same previous Camera baseline, all four cutoff cases have
the same legacy predictive law, apart from Monte Carlo differences. Report this
property; do not silently reset clocks to manufacture distinct horizons.

Manual share overrides reset the affected list's national **and territorial**
shock dates to `projectionDate`. The poll-reference path retains historical
territorial age and adds measurement error. Thus a point override intended to
match means also changes uncertainty timing.

Every choice must use cutoff-available information. For example, a historical-
only 2013 test cannot infer positive M5S support from its later outcome when the
eligible prior ballot has no such support. Retain that baseline limitation.

## Implementation order after choices are fixed

1. Produce and review exact future/per-fold matrix and reference tables, including
   split ratios, unsupported lists, proxies and lost valid mass.
2. Add an isolated adapter and independent projection/clock/zero/conservation
   checks. Preserve production scales, RNG, integer apportionment and scrutiny;
   do not add multinomial noise to the legacy model.
3. Run reduced numerical/browser pilots, then matched historical/full-domain
   panels with the existing scores and whole-draw simulation uncertainty.
   Retain failures and avoid post-result factor tuning.
4. Measure the actual 24-list paired browser workload through scrutiny. Legacy
   has reference preparation rather than a fitting step equivalent to the new
   model's refit; report both preparation and complete execution time.
5. Retain compact source-pinned evidence. Public dispatch and research archival
   remain separate migration steps.
