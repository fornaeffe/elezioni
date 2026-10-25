# Party-level vote-generation drift model

This is the current model contract, consolidating the
[original correspondence proposal](correspondence-model.md) and the decisions
below. Use the [historical research report](vote-generation-drift-results.md)
and [testing protocol](vote-generation-drift-testing.md) only for the earlier
fixed-outcome experiments; their fitted parameters and calibration do not
automatically carry over to this party-level model.

## Decision status — 25 October 2026

Luca approves party-level latent support, list-level observations, a combined
non-valid outcome, and scenario ownership of the complete membership table.
The scenario may revise historical decompositions as well as the future ballot.
Historical source counts remain unchanged. No voter-movement studies, voter-flow
matrices or origin–destination parameters enter this model.

Availability uses the choice set defined below: removing a component from the
softmax denominator redistributes its probability proportionally across all
remaining outcomes, including non-valid voting. This replaces the earlier
exit-to-non-valid projection assumption.

The planned production workflow estimates the scenario-specific model in browser
workers after semantic membership edits. A matching precomputed default fit may
be served to avoid that computation. Browser fitting, its latency and posterior
accuracy have not yet been implemented or verified. This specification changes
neither the existing generator nor legal scrutiny defaults. Use already downloaded
compatible polls for the next research round.

## Political components and scenario-owned memberships

Let $P$ contain every stable political component needed to represent the
historical observations and the scenario ballot. A component need not previously
have stood alone. Include scenario-defined new components and retain an explicit
unresolved political component where the sources cannot support a finer identity.
An opaque joint group may remain atomic when neither observations nor the future
scenario distinguish its members. A requested future split does justify historical
latent components even without separate past observations; retain their uncertainty.
Party identity and its available statistical evidence are separate concepts.

Identity rules:

- A rename retains the same identity.
- A joint electoral list groups its constituent identities.
- An organizational merger retains historical components beneath the new list
  label; it does not introduce an additional independent support component for
  the same voters.
- A scenario-created split component can already belong to historical joint
  lists, even though it will first stand separately in the future. This is a
  retrospective decomposition, not necessarily a historical party birth.

The scenario owns the complete effective identity, availability and membership
specification for every modeled election and poll question, plus the future
ballot. Preparation supplies versioned defaults and original observations.
Storage may use a pinned default plus overrides, but the effective table must be
fully reproducible, inspectable and included in scenario exports.

Index membership by observation/question or ballot, date, election kind/chamber
and geography. Let $c$ denote a municipality or, where its ballots differ, the
municipality–college part used for generation. Let $A_{c,e}\subseteq P$ be the
available components for election $e$, and $L_{c,e}$ its lists. The member sets
$G_{l,c,e}$ must form a partition:

$$
G_{l,c,e}\ne\varnothing,\qquad
\bigcup_{l\in L_{c,e}}G_{l,c,e}=A_{c,e},\qquad
G_{l,c,e}\cap G_{m,c,e}=\varnothing\quad(l\ne m).
$$

A component belongs to zero or one list on each local ballot. In an incidence
matrix $M_{l,j,c,e}$, membership entries are zero or one; available-component
columns sum to one. There are no partition coefficients in this table.
A coalition of distinct lists does not turn those lists into one joint list.

When an editor creates a historical component split, update the relevant
historical election and poll memberships explicitly. Preserve each original
source count once; the corresponding likelihood now observes a sum of component
probabilities. Never duplicate a source count or replace it with fixed fractional
pseudo-counts. Keep all original observations instead of successively overwriting
them through intervening merged lists.

## Party probabilities, list probabilities and observed counts

For $j\in\{0\}\cup P$, define latent log scores

$$
s_{j,c,k,t}=\eta_{j,n,t}+\delta_{j,c,t}+\gamma_{j,k}.
$$

Here $k$ denotes the election-kind context; chamber-specific states/gaps follow
the paired-chamber contract below. Outcome $0$ means abstention plus blank/invalid
voting in the reconciled modeled electorate. It is not a political component.

Use $\pi$ for component probabilities and $p$ for list probabilities. For an
election at $t_e$, put

$$
Z_{c,e}=\exp(s_{0,c,k(e),t_e})
+\sum_{h\in A_{c,e}}\exp(s_{h,c,k(e),t_e}),
$$

$$
\pi_{j,c,e}=
\begin{cases}
\exp(s_{j,c,k(e),t_e})/Z_{c,e}, & j\in\{0\}\cup A_{c,e},\\
0, & j\in P\setminus A_{c,e}.
\end{cases}
$$

Aggregate into the actual ballot lists:

$$
p_{l,c,e}=\sum_{j\in G_{l,c,e}}\pi_{j,c,e},
\qquad p_{0,c,e}=\pi_{0,c,e},
\qquad p_{0,c,e}+\sum_{l\in L_{c,e}}p_{l,c,e}=1.
$$

Grouping a fixed available set leaves $p_0$ unchanged. Changing availability does
not: at unchanged scores, removing a component with probability $a$ divides each
remaining probability by $1-a$. For example, A = 20%, B = 50%, non-valid = 30%
becomes B = 62.5%, non-valid = 37.5% when A is excluded. This approved
choice-set substitution rule is not a measured voter-flow statement. Joining
the same components under one label introduces no additional alliance effect
by itself.

Let $E_{c,e}$ be electors, $V_{c,e}$ valid attributed votes and $y_{l,c,e}$ the
observed list counts. After reconciling valid candidate-only attribution,

$$
V_{c,e}=\sum_{l\in L_{c,e}}y_{l,c,e},
\qquad y_{0,c,e}=E_{c,e}-V_{c,e},
$$

$$
\left(y_{0,c,e},(y_{l,c,e})_{l\in L_{c,e}}\right)
\mid E_{c,e},\boldsymbol p_{c,e}
\sim \operatorname{Multinomial}
\left(E_{c,e},\left(p_{0,c,e},(p_{l,c,e})_{l\in L_{c,e}}\right)\right).
$$

This includes observed non-valid counts and allows observed zeros with finite
latent scores. Historical valid votes remain valid observations even when their
party is unavailable on the future ballot. Incomplete extraction, unresolved
memberships and survey nonresponse never become historical outcome $0$.
Retain political residuals or report source mismatches; audit inconsistent
turnout denominators rather than silently correcting them.

Outcome $0$ stays outside candidatures, coalitions, thresholds and seat allocation.
A poll's Other answer is defined below and is not an ineligible synthetic party.
An optional pooled minor-list scenario feature is a separate contract: preserve
valid-vote totals and relevant coalition identities, and do not let a pooled
group qualify as one fictitious list.

Future $E_{c,t}$ remains an input or a separate demographic forecast. Align
electorates and territorial coverage. The model predicts valid-vote participation,
not separate abstention and ballot-validity processes. A shared softmax couples
participation with political scores; keeping $s_0$ fixed does not fix $p_0$.

## Identified party dynamics, births and split uncertainty

Use a fixed component basis for a fit, with $J=1+|P|$. To avoid confusing the
party set with a projection matrix, write
$C_J=I-\boldsymbol1\boldsymbol1^\top/J$. Store centered scores and implement
inference in $J-1$ orthonormal contrasts. Availability changes the observation
normalizer, not the meaning of the retained component coordinates.

For consecutive state times, use elapsed time in years:

$$
\boldsymbol\eta_n(t+\Delta)
=\boldsymbol\eta_n(t)+\sqrt{\Delta}\,C_JD_\eta\boldsymbol\epsilon_{\eta,t},
$$

$$
\boldsymbol\delta_c(t+\Delta)
=\boldsymbol\delta_c(t)+\sqrt{\Delta}\,C_JD_{\delta,c}
\boldsymbol\epsilon_{\delta,c,t}.
$$

Here $D_\eta=\operatorname{diag}(\sigma_{\eta,j})$ and
$D_{\delta,c}=\operatorname{diag}(\sigma_{\delta,j,c})$, including $j=0$.
The scales describe independent innovations before centering. For a contrast
matrix $H$, national innovation covariance is
$H^\top D_\eta^2H\,\Delta$. National innovations are shared across municipalities.
Baseline innovations are independent across components, municipalities, national/
local processes and disjoint time intervals before centering. Overlapping
intervals use increments of the same process.
Apply these equations to initialized component trajectories. An existing but
unoffered component may drift unobserved; omission from a poll does not reset
its state. Do not extrapolate a genuine component's trajectory before birth.

Use regularized $\gamma_{j,k}$ effects with Camera as reference, including outcome
$0$, when the adjusted-kind model is selected. Proper initial priors and covariance
assumptions also identify the national/local decomposition. Do not force the
municipal mean of local innovations to zero without carrying the changed
covariance into the national process.

Within an observed joint list C, the split is already implied by the scores:

$$
\rho_{j\mid C,c,e}
=\frac{\exp(s_{j,c,k(e),t_e})}
{\sum_{h\in G_{C,c,e}}\exp(s_{h,c,k(e),t_e})}.
$$

Do not add independent split coefficients. Joint counts identify the aggregate,
not every internal contrast. Separate-party elections and ordinary party-intention
polls available at the cutoff may inform that contrast through the temporal model.
Otherwise exchangeable proper priors give equal expected contributions with
uncertainty. Preserve shared national split uncertainty and pool weakly informed
local differences; independent municipal splits must not manufacture national
certainty through averaging. Proper priors do not turn prior-driven parameters
into data-identified estimates. No voter-movement information is used.

For a genuine historical birth, introduce a proper initial distribution at its
first relevant modeled support date; do not accumulate unobserved Brownian noise
from an arbitrary earlier dataset start. Components already existing at the
window start receive initial priors there; a known birth within the window
starts its trajectory at that date. If the birth date is unavailable, use the
first supported appearance as an explicit initialization convention. Use a
weak, finite Gaussian prior in gauge-fixed initial score-contrast coordinates,
with location/scale pooled from comparable components using cutoff-available
evidence and proper hyperpriors. Use neutral,
zero-centered, regularized geographic and kind-effect priors unless supported
information justifies a different centre. In insufficient data, use versioned
proper fallback scales rather than an improper flat prior. Condition on the first
real observation once; do not require a user-supplied historical share or invent
pseudo-votes. Carry initial uncertainty into forecasts. Prior scales need
recalibration on the new party basis.

A retrospectively introduced component within an old joint list starts with that
joint history and an uncertain decomposition; its first separate future label is
not a reason to discard those observations or reset its history.

Party aggregation induces list dynamics. In particular, a joint-list log score
contains a log-sum-exp of component scores and generally is not itself a
constant-volatility Brownian process. This is an intended consequence of defining
the process on parties. Earlier list-level scales and calibration do not transfer
automatically.

## Poll choice sets, Other and the observation link

For each poll question $q$, assume that its reported political answers are the
offered political choice set. This is the approved observation assumption, not a
statement that unnamed parties have ceased to exist. Distinguish questions in one
survey and retain their shared dependence. Exclude undecided, abstention and other
nonresponse answers from the political-intention denominator; they do not observe
outcome $0$.

Retain fieldwork measurement time $t_q$ (midpoint or an explicit fieldwork-average
model), publication/fieldwork-end availability at the forecast cutoff, kind/chamber,
geographic coverage $S_q$, and original answer groups. Without a political Other
answer, $A_{c,q}$ is the union of the reported political groups. The question then
informs relative support within that offered set. Do not invent an unreported
residual or treat exclusion from that question as permanent party absence.

Where an answer means other political parties, let $B_{c,q}$ be the modeled
components existing and geographically relevant in that question's context.
Its membership is

$$
G_{\mathrm{Other},c,q}
=B_{c,q}\setminus
\bigcup_{a\in\mathcal R_q}G_{a,c,q},
$$

where $\mathcal R_q$ contains the explicitly named political answers. Then
$A_{c,q}=B_{c,q}$ and the named groups plus Other partition that set. Newly created
historical split components can belong to an old named group; a genuinely not-yet-
existing party is outside $B_{c,q}$. Scope/date these distinctions in the scenario.

Other contains neither nonresponse nor non-valid voting. It is a grouping of
components, not an additional duplicate component. Positive reported Other with
an empty modeled complement is a membership/source mismatch; represent unresolved
political support explicitly instead of assigning it to $0$. Preserve source
normalization/rounding and audit unexplained gaps or overlapping answer groups.

A small Other observation constrains the combined unreported support and hence
each nonnegative contribution at that date/context, subject to observation error.
It does not identify the internal split, provide a universal future upper bound,
or prevent later drift. Proper priors and volatility pooling remain necessary.

Apply the component softmax using the question's available set, then sum component
probabilities into answer probabilities $p_{a,c,q}$. The aggregate link is

$$
r^{\mathrm{agg}}_{a,q}
=\frac{\sum_{c\in S_q}E_{c,t_q}p_{a,c,q}}
{\sum_{c\in S_q}E_{c,t_q}(1-\pi_{0,c,q})}.
$$

Equivalently, weight municipal conditional political shares by expected valid
votes $E_{c,t_q}(1-\pi_{0,c,q})$. Elector weights alone are generally incorrect.
The non-valid score cancels from a municipal conditional political share; poll
preferences do not directly identify participation, though heterogeneous valid
weights and state correlations can provide indirect information.

Keep shared polling error on a compatible component-contrast basis. One coherent
implementation tilts municipal conditional political probabilities with a shared
Gaussian contrast $b_{g(q),j}$ before grouping, retaining the unperturbed expected
valid-vote weights:

$$
r^{(b)}_{j,c,q}
=\frac{\pi_{j,c,q}\exp(b_{g(q),j})}
{\sum_{h\in A_{c,q}}\pi_{h,c,q}\exp(b_{g(q),h})},
$$

$$
\widetilde r_{a,q}
=\frac{\sum_{c\in S_q}E_{c,t_q}(1-\pi_{0,c,q})
\sum_{j\in G_{a,c,q}}r^{(b)}_{j,c,q}}
{\sum_{c\in S_q}E_{c,t_q}(1-\pi_{0,c,q})}.
$$

At $b=0$ this reduces to the aggregate link. Shared-error grouping and covariance
regularization are fixed in the fitting policy. Apply sample-dependent uncertainty
in the observed answer-contrast space, using the reported or explicitly estimated
effective decided sample, rounding and appropriate handling of printed zeros.
Do not fabricate respondent counts. Any additional independent poll-error floor
belongs here. The shared bias and sampling noise remain observation quantities:
condition latent states on polls, but add no new poll-error draw when generating
election votes.

The literal national-index link remains a comparison diagnostic on a common
choice set: normalize the political $\eta_{j,n}(t)+\gamma_{j,k}$ components and
group them into the reported answers. The aggregate link is the reference
operator for scenario shares and handles geographic availability explicitly.

## Scenario reference shares and new components

Use the same aggregate operator for polls and scenario shares, with each input's
own date, geography and ballot context. For a component or list group $G$, scope
$S$, reference date $t$ and declared scenario ballot $b$, define

$$
F_{G,S,b,t}(\boldsymbol s)
=\frac{\sum_{c\in S}E_{c,t}\sum_{j\in G_{c,b}}\pi_{j,c,b,t}}
{\sum_{c\in S}E_{c,t}(1-\pi_{0,c,b,t})}.
$$

The default context is the modeled future ballot evaluated at the input's
reference date. Polls retain their own question contexts. This permits direct
reuse of the link without pretending that scenario inputs are surveys. The same
availability-softmax construction applies to any such context $b$ and date $t$.

Reference targets explicitly distinguish component identities from ballot lists;
matching display labels do not make them interchangeable after a split.
Every genuinely brand-new scenario party requires a national valid-voter share, reference
date (default: scenario reference date) and uncertainty (default: zero).
Geographically scoped overrides are optional. Use this contract for all
scenario-defined national shares, not only new components. A joint-list total
constrains the sum of its members and does not determine their internal split.
A new component record representing support inside historical joint lists is
a retrospective decomposition, not automatically a genuinely new party: use
that grouped history and proper priors, with optional reference targets.

Store uncertainty with explicit units; use standard deviation in percentage
points of valid-voter share at the input boundary. Zero means the exact latent
reference-support constraint

$$
F_{G,S,b,t}(\boldsymbol s)=a.
$$

Positive uncertainty supplies soft reference information through a bounded
logistic-normal reference factor, calibrated numerically to the entered mean
and share-scale standard deviation. Its uncertainty describes the
reference information; other evidence and priors can shift posterior moments.
Use a coherent joint simplex/contrast specification for exhaustive groups and
reject infeasible moments (for one interior share, $0<u^2<a(1-a)$ with
$u$ expressed as a fraction). Do not interpret a percentage-point uncertainty as a
raw logit standard deviation.

These are scenario constraints, with no synthetic poll sample, shared poll-bias
term or automatic polling-error floor. A hard constraint fixes aggregate latent
support at that date/context; municipal probabilities remain uncertain and
future drift and multinomial count variation remain active. It does not fix
future realized national vote counts.

Resolve supplied-data, chamber and geographic override precedence before building
the active constraint set. Validate hard constraints jointly with filing support,
normalization and any scoped overrides; reject or explain incompatible active
targets rather than silently relaxing them. Record superseded targets explicitly.
An interior available component needs an interior share with finite scores;
exact endpoints require structurally compatible support or an explicit fixed-
support rule, not an undisclosed epsilon substitution.

Translate targets by solving the aggregate-to-latent problem. Never assign a
national input directly to $\eta_{j,n}$ or uniformly to every municipality.
A common reference-score shift for a group provides a useful scalar root solve
when all other quantities are fixed and the target is feasible. A fitted posterior
must retain the dependence and uncertainty implied by its hard/soft constraints;
independent post-fit rescaling is not a substitute for conditioning.

For new components use proper, pooled defaults for municipal differences,
volatility and cross-chamber behavior, with a neutral geographic prior unless
an explicit supported pattern is supplied. A national reference value alone
does not estimate those quantities. Initialize genuine new support at its relevant
date; distinguish it from a retrospective decomposition of an old joint list.
Preserve real observation dates and no-look-ahead rules in backtests.

Political share inputs are among valid voters. Any non-valid override remains
an elector share and uses its own aggregate operator,

$$
F_{0,S,b,t}=\frac{\sum_{c\in S}E_{c,t}\pi_{0,c,b,t}}
{\sum_{c\in S}E_{c,t}}.
$$

It follows the same declared hard/soft and precedence rules, without conditioning
non-valid support on valid voting or claiming separate turnout identification.

## Browser estimation, default fits and invalidation

The browser is the production fitting target after scenario editing. Run fitting
in cancellable workers with progress, deterministic seeds, numerical diagnostics
and versioned outputs; keep editing/rendering responsive. Benchmark real party
counts and geographic scale before claiming acceptable fitting latency.

Preparation may publish a precomputed default fit. It must include the posterior
state/uncertainty representation and innovation-parameter uncertainty needed for
generation, not just point estimates. A browser may reuse it only when the
complete fitting fingerprint matches: source data and cutoff, identities,
historical/question/future memberships and availability, kind/chamber/geography,
birth/initial priors, fitting policy/version and effective reference constraints.
A reproducible fit also records its inference algorithm, tolerances and seed;
changing only a forecast Monte Carlo seed does not redefine the fitted model.
A pinned-default-plus-overrides representation must resolve to this same
effective fingerprint.

A semantic membership edit, including adding a retrospective split component,
invalidates the old fit. Tell the user that editing memberships requires a new
parameter estimation and may take substantial time; make the stale/estimating/
ready/failed state visible. Never silently generate from the old parameter bundle
or convert it with the edited table. Pure presentation renames that preserve
identity and membership need not invalidate inference. Warm starts and verified
dependency-based reuse may reduce work without treating an incompatible fit as
valid.

Implement this as a new versioned scenario/fitting contract. The current
past-list/future-list factor schema and generator are legacy behavior, not the
membership model. Preserve archived scenarios/results through explicit adapters;
do not reinterpret old factors as memberships or claim browser fitting already
exists.

## Volatility comparisons and chamber compatibility

Compare common $\sigma_\eta$ with partially pooled component-specific
$\sigma_{\eta,j}$. Local candidates are:

| Local structure | Parameterization |
| --- | --- |
| Common | $\sigma_{\delta,j,c}=s_\delta$ |
| Component-specific | $\log\sigma_{\delta,j,c}=\mu_\delta+u_j$ |
| Municipality-specific | $\log\sigma_{\delta,j,c}=\mu_\delta+v_c$ |
| Separable component + municipality | $\log\sigma_{\delta,j,c}=\mu_\delta+u_j+v_c$ |

Center and shrink log-scale effects. Include the non-valid outcome and report
whether improvements concern participation or political support. Begin with
pooled scales where component evidence is weak; a standalone appearance alone
does not identify volatility, local geography or kind effects. Do not add
unrestricted component-by-municipality interactions in this comparison round.

The [paired-chamber decision](politics-reform-2026-modelling.md#camera-senato-votes-r26-04)
applies to component contrasts including $0$. Couple national/local innovations
on that basis, preserve each chamber's marginal variance budget and historical-gap
regularization, then aggregate through each chamber's ballot memberships.
Keep distinct electorates, filings and override precedence. Do not duplicate a
chamber gap already represented elsewhere when adding kind effects. Camera/
European observations alone do not identify Camera/Senato divergence. A combined
non-valid outcome cannot enforce separate abstention/invalid-ballot identities.

## Desired staged research plan

Revalidate the party/grouped-observation implementation before extending the
earlier comparison. Its results remain evidence about the old outcome basis.

| Stage | Compare | Hold fixed / limit |
| --- | --- | --- |
| 0 | Validate party memberships, choice sets, births, split uncertainty and reference constraints | Count conservation, observed zeros, Other complements and reduced-problem posterior checks |
| 1 | Literal versus aggregate polling link | Common national/local volatility; compatible question grouping |
| 2 | Direct Camera/European pooling versus regularized kind effects | Selected link; common volatilities |
| 3 | Common versus partially pooled component-specific national volatility | Common local volatility |
| 4 | Common, component-specific and municipality-specific local volatility | Selected national structure |
| 5 | Separable component + municipality local volatility if both help | No unrestricted interaction grid |
| 6 | Recheck the alternative national structure at the winning local structure | One limited interaction check |
| 7a | Regional innovations versus the selected simpler model | Split the local variance budget |
| 7b | Local mean reversion versus the selected simpler model | One pooled rate around persistent historical geography |

Run both stage-7 branches separately, without automatically combining them or
crossing every earlier variant. Retain the all-common and current-generator
benchmarks. Regional innovations replace part of the municipal residual budget,
rather than adding unconstrained marginal variance.

For reversion compare Brownian local differences with

$$
d\boldsymbol\delta_c
=-\kappa(\boldsymbol\delta_c-\overline{\boldsymbol\delta}_c)\,dt
+C_JD_{\delta,c}\,d\boldsymbol W_c,\qquad \kappa\ge0.
$$

Estimate the persistent centre only from training data; reversion toward zero
would erase geography. Include non-valid voting in the same declared construction.

Use cutoff-available Camera/European counts and polls, consistent time laws at
730/365/90/15-day horizons, and chronological development/confirmation separation.
Count elections as independent validation events, and report kinds separately.
Membership choices, priors and default hyperparameters must not use held-out
results. No new polling acquisition is required for this round.

## Estimation, generation and runtime boundary

The model remains latent Gaussian with non-Gaussian observations. Joint-list
counts constrain sums of component probabilities; their likelihood can be
non-concave in the component scores and weakly identify internal contrasts.
The existing direct-outcome count IRLS and local log-ratio moment estimator
cannot be reused unchanged. Never treat inferred component counts as measured
data when estimating volatility or posterior covariance.

Use proper priors/pooling and a sparse Laplace or suitable nonlinear latent-
Gaussian fitting approach, checking modes and validating against sampling on
reduced problems, especially inseparable components and newly introduced splits.
Optimization aids such as latent allocations do not replace uncertainty from the
original grouped likelihood. Exact Kalman marginalization is only a Gaussian
approximation diagnostic. Browser implementation requires measured numerical
accuracy, memory and execution behavior before runtime acceptance.

At generation draw latent component states, aggregate into future-list
probabilities and draw multinomial counts including $0$. Pass list counts to
the legal scrutiny boundary; party-level fitting alone requires no legal-rule
change. Compute national results from municipal counts. Neither national valid
shares nor elector shares need equal softmax of the national latent factor.
Do not impose an independent national endpoint or post-draw IPF.

Retain plausibility, coverage/width/proper scores and predictive-mean accuracy
in Luca's priority order. Check integer/elector conservation, aggregate consistency,
hard-reference constraints at their own dates, split uncertainty and calibration
of political and non-valid outcomes. Full-list, override, paired-chamber, seat and
browser-fitting acceptance remain required before changing runtime defaults.

## References

- [Stan multinomial/logit reference](https://mc-stan.org/docs/functions-reference/multivariate_discrete_distributions.html):
  count observations on a simplex.
- [Stan multi-logit identification](https://mc-stan.org/docs/stan-users-guide/regression.html#identifiability):
  reference/centered parameterizations.
- [Kristensen et al., TMB](https://arxiv.org/abs/1509.00660):
  sparse latent-variable fitting by Laplace approximation.
