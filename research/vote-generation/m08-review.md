# M08 0.0.1 consistency and pipeline review

The draft defines a valid Camera draw and a valid conditional Senato draw once
the inputs, parameters and college intersections are supplied. Both draws can
feed the current domestic scrutiny pipeline through a new generator adapter.
The priority before fitting is to specify the observation denominators and
geography, resolve how national election uncertainty survives aggregation, and
decide which chamber effects the model should reproduce. The proposals below
are for the next draft. [M08 0.0.1](../../docs/model-drafts/M08v0.0.1.md)

## Equations and forecast behavior

- **The count equations conserve electorates.** Each offered ballot defines a
  probability partition. Electorate-weighted aggregation preserves the sum of
  one. Within a Camera college, every positive-electorate intersection has the
  same filed lists and positive baseline probabilities for those lists, so the
  raking margins are feasible after removing zero rows and columns. Define the
  allocation as the unique fit to those margins on positive support, with an
  explicit convergence tolerance and failure diagnostic. Keep the fitted
  intersection counts fractional for transfer and retain the original integer
  college counts. Give every active zero-electorate college an explicit zero
  row. A political ballot with no offered lists is degenerate at outcome $0$.
- **The chamber link has coherent endpoints.** Full matching gives the
  smoothed Camera shares on the Senato electorate weights. Disjoint political
  ballots give the Senato baseline and standalone dispersion conditional on
  shared support. Intermediate weights give a normalized mean and a positive
  concentration. The one-elector smoothing keeps a filed list eligible after
  a realized zero. Differences in ballot grouping lower the link for the entire
  ballot, including turnout. The transfer leaves the split of a grouped Camera
  list to the fitted component baseline. A split unobserved elsewhere in the
  data remains prior-driven.
- **Partial links can shift Senato means.** Normalizing the transferred ratios
  is nonlinear. For example, with one Camera elector, equal support for $0$ and
  two political lists in each chamber, and exactly one political list shared,
  the Senato-only list has baseline share $1/3$ and expected linked share
  $43/126$. Its transferred share before blending is $2/7$ when Camera draws
  either common outcome and $1/2$ when it draws the Camera-only list. Averaging
  these and blending at $w=1/3$ yields a difference of $1/126$, about 0.79
  percentage points in this toy case. Unequal electorate weights and raking
  across college boundaries add another potential source of mean shifts.
  Retain the transfer for the first comparison, measure signed gaps by ballot
  overlap and support, and recenter its predictive mean only if the shifts
  are material and unsupported by data.
- **Filing restrictions also change expected turnout.** Removing a political
  component from the softmax increases the probability of outcome $0$, even
  with unchanged turnout preference. For the main candidate, give non-valid
  support its own baseline logit and normalize the political composition over
  the offered components. Under the linked Senato draw, transfer and blend the
  non-valid probability separately from the composition conditional on valid
  voting, then recombine them for the DM draw. Start the turnout link with the
  draft's political overlap weight and compare against standalone turnout
  when filings differ. Keep the draft's single softmax as a sensitivity for
  the implicit turnout response.
- **National uncertainty needs a shared election-level term.** Under the
  natural conditional-independence interpretation of the college draws, national
  share variance conditional on support is $\sum_u a_u^2 V_u$, with electorate
  weights $a_u$ and college share variances $V_u$. For 100 equally weighted
  colleges of 100,000 electors, a 20% share and concentration 600 give about
  1.64 percentage points of conditional SD per college and 0.16 nationally.
  A shared posterior draw also contributes uncertainty, but more municipal rows
  can tighten it without adding independent evidence about election-to-election
  movement. Further, a sum of independent municipal DMs is generally not a
  college DM with the same concentration. Use one shared national log-ratio
  shock per election, applied before ballot restriction in both the historical
  likelihood and forecast. Calibrate the remaining local dispersion at a
  declared spatial scale. Keep independent college draws as a benchmark and
  add regional shocks only if held-out errors warrant them. The draft's lag
  formula changes marginal uncertainty and supplies no time covariance. If
  sensitivity to recency remains a problem, compare an explicit shared
  temporal process as a separate variant. [Comparison protocol](campaign-2026-11/protocol.md#scores-and-comparison-order)

## Observation and fitting decisions

| Decision | Proposed specification |
| --- | --- |
| Dates and eligible sources | Separate the information cutoff from the forecast date. Require election observations and their availability dates, and poll dates, fieldwork end and publication, to be at or before the cutoff. Use days divided by a versioned year length for $\Delta_i$. Start with Camera history and Camera polls. The authored default selects four Camera elections, four European elections and 45 polls, of which 38 are Camera polls. Record the excluded European inputs for the Camera-only benchmark. A mixed-history variant should give European elections and polls an explicit context effect. [Authored default](../../data/politics-default-scenario.json#L142) |
| Election counts and ballot coverage | Use integer observed list totals and $y_0=E-\sum_\ell y_\ell$. Include attributed valid candidate-only ballots where the source provides a defensible reconstruction, and carry its provenance. Otherwise coarsen to observable ticket groups or exclude and report the electorate. Derive offered sets from ballots or source metadata, preserving filed zeros. Reject impossible positive counts outside a choice's support. Missing places, unmapped choices, and candidate-only valid votes cannot enter outcome $0$. [Source contract](../../web/src/lib/politics/reform-2026/vote-share/source-types.ts) · [Comparison data policy](campaign-2026-11/protocol.md#common-data-boundary) |
| Poll denominator and count reconstruction | Define a question-specific answer set. For a conditional political table, divide each electorate-weighted political answer mass by the sum of the political masses in that question; omit outcome $0$ from that likelihood. Include $0$ in a complete elector-base question only when the published category matches abstention plus blank/invalid voting. Undecided and nonresponse do not establish that outcome. Prefer actual question counts. Otherwise reconstruct with stable largest-remainder rounding from published shares and a declared question base, or a rounded effective-base proxy with an audit flag. None of the 45 selected bundled questions has a recorded question base. Retain the originals, rounding and provenance, including positive shares rounded to a zero count. Use one comparable observation per shared respondent base initially, or model joint survey errors explicitly. [Source contract](../../web/src/lib/politics/reform-2026/vote-share/source-types.ts#L21) |
| Scenario references | Group compatible rows by date, geography, chamber channel and denominator. Use valid-vote fractions for political questions and a separate binomial/beta-binomial observation for non-valid fractions of electors. Treat 1,000 as a nominal pseudo-count and set a separate fixed reference concentration so evidence strength is explicit. Encode positive political remainder as Other only if the declared pre-filing offered complement is nonempty. Validate overlaps and sums, retain explicit zeros, use common references once, and report common/chamber conflicts according to explicit precedence. Declare whether list and component references and their SD fields are supported. Start with list/component means and no SD input. Opt references into M08 explicitly through `appliesTo`. [Scenario contract](../../docs/common-politics-scenario/contract.md#ownership-and-encoding) |
| Membership and support | Compile binary memberships from the existing source/question/choice and future filing tables. Build $P$ from eligible sources, effective future memberships and opted-in references. Reuse whole-component pooling only if it preserves each observed partition, or merge affected choices while preserving their observed totals and recording the coarsening. Diagnose grouped components that never separate and locations with no informative history. Give new components and weakly identified splits proper priors and carry their uncertainty into generation. A finite posterior alone does not establish identification. [Membership input normalization](../../web/src/lib/politics/execution/inputs.ts#L12) |
| Logit constraints and priors | Under the draft's softmax, fix the outcome-$0$ score and local deviation to zero and impose $\sum_q a_q\delta_{j,q}=0$ for each political contrast, using pinned reference electorate weights. With an independent turnout logit, apply the same geographic centering and identify political contrasts separately. Interpret national scores as baseline log odds. Compute reported national shares by aggregating actual geography and ballot probabilities. Use proper Gaussian contrast priors with a shared local scale and proper positive scale priors. Calibrate the 50% non-valid prior expectation in the declared all-offered baseline, and calibrate local and lag dispersion with explicit probability-scale prior-predictive quantiles. Signatures are a filing requirement and provide no vote-share bound. State the initial numeric priors before a fixture fit. |
| Concentration, lag and posterior prediction | Specify the likelihood dependence at each level. Separate election-scale, local, poll and reference variation. One lag gives information about $k/(1+\Delta/\tau)$; even with multiple lags, a few independent elections weakly identify a time scale. Start with a fixed or strongly regularized $\tau$, with sensitivity runs. Specify a positive excess parameter for $\kappa_{\mathrm{link}}-k_{\mathrm{election}}$ on compatible forecast scales. Each simulation selects one joint posterior state for both chambers and all colleges, then draws new forecast shocks and counts. This preserves spatial and component dependence learned in fitting. |
| Comparable chamber calibration | Use matched integer returns at the chosen likelihood scale. Enter the Camera counts once, and condition Senato on those counts, aggregating Camera observations when necessary. Anchor both chamber means for the pair in that source's shared election state so forecast lag does not become within-election noise. The 2022 pair is the only bundled same-age calibration source; it informs spatial residuals within that election. Exclude age-25+ Senato from primary calibration. Test signed gaps on common footprints, and keep any 2022 reconstruction explicitly in-sample. If signed discrepancies persist, compare a regularized chamber bias before adding time-varying chamber effects. [Chamber evidence](campaign-2026-11/results.md#proposed-chamber-criterion-and-results) |

## Geography and pipeline integration

**Compile a versioned joint crosswalk.** Intersect the two future physical
college partitions and historical source units within each source scope. Use
separate historical and future electorate margins and conserve each known
total. Keep historical votes at observed resolution. Fill missing within-unit
electorate weights from labelled census/crosswalk estimates where defensible.
Coarsen affected observations or exclude and report them when there is no
defensible match. Scope lookups should support partial colleges, including
municipal and provincial references. The statistical cell carries $s_{j,q}$.
Source-specific fragments can contribute exposure weights to that cell without
requiring one fully materialized cross-product of all historical geographies.

The pinned Rosatellum geography has 8,213 matching physical unit keys across
chambers, including parts in the three municipalities split in both chambers.
It can supply the future intersection keys directly. The pinned reform asset
stores chamber-specific college margins and has no municipality split across
multiple physical colleges in both chambers. Its joint weights can therefore
be reconstructed by pairing each split-side part with the other chamber's sole
college. A future municipality split on both sides would need a shared finer
crosswalk or an explicit reconstruction assumption. Derived Camera TAA
proportional rows are outside the physical partition. [Rosatellum preparation](../../web/scripts/data-preparation/workspace-geography.ts#L100) · [Reform preparation](../../web/scripts/data-preparation/normalize.ts#L127)

**Emit the existing integer college contract.** A generator should return one
`CollegeCounts` row per renewed physical college, with its integer electorate
and integer filed list totals. Outcome $0$ is the difference between electorate
and total valid votes. The current law coverage resolves actual plurinominal
filings under Rosatellum, statutory uninominal exceptions, and reform special
ballots. For Camera-only renewal, use the Camera draw. For Senato-only renewal,
use a standalone Senato draw with its own electorate and filings. Document
that this can differ in distribution from Senato in a joint election. The
existing assembly can then handle candidates, reform preferences and supplied
votes, and derived Camera TAA results. Supply deterministic chamber-specific
central support for candidate/reference preparation. Reform's bonus slates
require an explicitly dated reference support input; if the draft still has a
single forecast endpoint, settle how the reference-date support is estimated
and record its basis when preparing the slate. The fractional Camera raking
table stays within the link and does not enter scrutiny. [Count contract](../../web/src/lib/politics/generation/types.ts) · [Law adapters](../../docs/common-politics-scenario/execution.md#law-adapters) · [Bonus preparation](../../web/src/lib/politics/reform-2026/reference-bonus-plan.ts#L62)

**Add M08-specific preparation and worker support if research succeeds.** The
current scenario and worker dispatch recognize M01/M02. A new registered pair,
versioned compiled input and fitted-state contract, and seeded generation
adapter are needed. Key fit reuse on the evidence, date, memberships, priors,
electorates and crosswalks actually consumed. Preserve deterministic results by
simulation ID and report prior-driven splits, exclusions and crosswalk
estimates. A fit can theoretically be prepared outside the browser and passed
to the existing worker boundary. Browser fitting and resource limits need
measurement. [Pair registry](../../web/src/lib/politics/scenario/registry.ts#L5) · [Worker boundary](../../web/src/lib/politics/execution/compute-inputs.ts#L20)

## Next research step

Settle the observation and geography contracts and the initial variant in a
new model draft. Start fixtures with one college and known parameters, then
test crossing colleges, a merged/split ballot, conditional poll denominators,
zero counts and electorates, and a prior-driven list. Check integer college
margins, predictive recovery, signed transfer shifts and national variance
against the independent-college benchmark. On real data, use matched domestic
footprints and compare national and territorial WIS and coverage, signed
Camera–Senato gap scores, and implementation cost. Run a Camera-only history
comparison on common inputs before comparing the mixed-history variant with
M01 2.0.0 and M02 3.0.0. The review used equation checks and read-only inspection
of the pinned default metadata and geography. It provides no fit, predictive
comparison or runtime measurement. [Campaign protocol](campaign-2026-11/protocol.md)
