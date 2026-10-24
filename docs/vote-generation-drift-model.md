# Revised vote-generation drift model

Use this document for the current mathematical proposal, implementation
requirements and staged research plan. The [previous comparison](vote-generation-research.md)
and its [methods](vote-generation-methods.md) retain their measured historical
results; their endpoint-reconciliation architecture is not the selected design.

## Decision status — 24 October 2026

Luca accepts the observed-count layer, corrected identification/time/innovation
conventions, the staged comparison hierarchy and these polling requirements:
separate sample-dependent uncertainty; an error component shared by polls;
fieldwork measurement dates distinct from publication availability; and polling
noise confined to the polling likelihood. Both stage-7 variants below belong
to the planned research.

Luca subsequently authorized research execution and confirmed the combined
non-valid outcome as its baseline. The
[testing protocol and execution record](vote-generation-drift-testing.md)
contains implementation details, diagnostics and measured comparisons.
Research selection does not activate a runtime model or change production
defaults. Use the already downloaded compatible polls; do not acquire more
polling data for this research round.

## Electorate outcomes and observed counts

For municipality \(c\), election kind \(k\) and time \(t\), let \(L_{c,k,t}\)
be the political lists present on that ballot. Add outcome \(0\): an elector
without a valid vote attributable to a political list in the modeled scope.
Its intended meaning is abstention plus blank/invalid ballots, after applicable
valid candidate-only vote attribution has been reconciled.

Let \(E_{c,e}\) be electors, \(V_{c,e}\) actual valid attributed votes and
\(y_{l,c,e}\) the corresponding observed list counts. Then

\[
y_{0,c,e}=E_{c,e}-V_{c,e},\qquad
V_{c,e}=\sum_{l\in L_{c,k(e),t_e}}y_{l,c,e}.
\]

Incomplete list extraction, an unmapped party or undecided survey respondents
must not become outcome \(0\). Retain missing political support through an
explicit observed residual/group or report the unresolved source mismatch.
Conserve all valid attributed votes before deriving the non-valid complement.
Source turnout-denominator anomalies require an audit, not silent correction.
The legacy scenario fallback for unmapped history is a separate, versioned
semantics issue; this document does not modify that runtime behavior.

For \(j\in \{0\}\cup L_{c,k,t}\), define latent log scores

\[
s_{j,c,k,t}=\eta_{j,n,t}+\delta_{j,c,t}+\gamma_{j,k},
\qquad
\pi_{j,c,k,t}
=\frac{\exp(s_{j,c,k,t})}
{\sum_{h\in\{0\}\cup L_{c,k,t}}\exp(s_{h,c,k,t})}.
\]

Here \(\gamma_{j,k}=0\) under direct election-kind pooling. The adjusted variant
uses regularized kind effects with Camera as the reference, including an effect
for outcome \(0\). Center those effect vectors too. This allows kind-related
participation differences as well
as political differences without assuming that Camera and European votes
cannot inform the same process.

Latent probabilities and observed elector shares are different quantities:

\[
\boldsymbol y_{c,e}\mid E_{c,e},\boldsymbol\pi_{c,k(e),t_e}
\sim \operatorname{Multinomial}(E_{c,e},\boldsymbol\pi_{c,k(e),t_e}),
\qquad
\widehat p_{j,c,e}=y_{j,c,e}/E_{c,e}.
\]

This observation layer permits observed zeros with finite latent scores and
conserves the electorate. Outcome \(0\) is a statistical outcome, not a political
list: exclude it from candidatures, thresholds, coalitions and seat allocation,
and expose it separately in the UI. The combined model predicts
valid-vote participation \(1-\pi_0\), not actual voter turnout separately from
ballot validity.

Future \(E_{c,t}\) remains an input or a separate demographic forecast. Adding
outcome \(0\) does not estimate the size or geography of the future electorate.
Keep denominators and territorial coverage aligned for each election/chamber.

One softmax also couples participation with political scores: keeping the
non-valid score unchanged does not keep its probability unchanged when political
scores move. This is a feature of the proposed joint model to evaluate, not a
mathematical reason to require a separate turnout process. The count-layer
factorization into valid/non-valid totals and conditional party counts is
equivalent; independently specifying Gaussian dynamics for a valid-rate logit
and party scores would generally define a different process model.

## Identified dynamics and initial states

Use a stable outcome basis for each fitted model, with \(J\) outcomes including
\(0\). Let \(P=I-\boldsymbol1\boldsymbol1^\top/J\). Store centered scores and
implement inference in \(J-1\) orthonormal contrasts. Scores are multinomial
log scores, not componentwise binary logits of observed shares.

For consecutive observation/state times, use elapsed time in years:

\[
\boldsymbol\eta_n(t+\Delta)
=\boldsymbol\eta_n(t)
+\sqrt{\Delta}\,P D_\eta\boldsymbol\epsilon_{\eta,t},
\]

\[
\boldsymbol\delta_c(t+\Delta)
=\boldsymbol\delta_c(t)
+\sqrt{\Delta}\,P D_{\delta,c}\boldsymbol\epsilon_{\delta,c,t},
\]

where \(D_\eta=\operatorname{diag}(\sigma_{\eta,j})\) and
\(D_{\delta,c}=\operatorname{diag}(\sigma_{\delta,j,c})\).
The scales refer to independent innovations before score centering. For a
contrast matrix \(H\), their identifiable national covariance is
\(H^\top D_\eta^2H\,\Delta\), not independent per-reference-list noise.

Before projection, standard-normal innovations are independent across outcomes,
municipalities and disjoint time intervals, and between national and local
components in the baseline. The national vector is shared by all municipalities.
Overlapping intervals must use consistent increments from the same process,
not fresh unrelated endpoint draws. Correlated chamber and regional variants
modify the stated independence explicitly.

Give national and municipal initial contrasts proper, regularized distributions,
whose centres and geographic donor information use only data available before
the forecast cutoff. Propagate their posterior uncertainty along with future
innovations and volatility uncertainty.

Category centering removes the arbitrary common score level. Separating a
national effect from a common shift of all local effects additionally relies
on the observation link, initial priors and covariance assumptions. Do not
silently force the municipal mean of every local innovation to zero; that would
change the independent-local-drift model unless the resulting covariance is
carried into the national/local decomposition.

## Polling links conditional on a valid political vote

Define the municipal conditional party share

\[
r_{l,c,k,t}
=\frac{\pi_{l,c,k,t}}{1-\pi_{0,c,k,t}}
=\frac{\exp(s_{l,c,k,t})}
{\sum_{h\in L_{c,k,t}}\exp(s_{h,c,k,t})}.
\]

The non-valid score cancels from this municipal conditional share. Thus a
party-preference-only poll does not directly measure the absolute participation
contrast or the non-valid fraction. Historical elector/valid counts inform
that part of the model; polling-derived state correlations can provide indirect
information, but survey nonresponse is not an observation of outcome \(0\).

Stage 1 compares these two links, with other model choices held fixed:

| Link | Centre of the poll's political log scores |
| --- | --- |
| Literal latent-index link | The political components of \(\boldsymbol\eta_n(t)+\boldsymbol\gamma_k\), excluding outcome \(0\) and re-centering over political outcomes |
| Aggregate-support link | Centered logs of the electorate model's aggregate conditional valid-vote shares below |

For a poll covering municipalities \(S_q\), the aggregate link is

\[
r^{\mathrm{agg}}_{l,q}
=
\frac{\sum_{c\in S_q}E_{c,t}\pi_{l,c,k,t}}
{\sum_{c\in S_q}E_{c,t}(1-\pi_{0,c,k,t})}.
\]

Equivalently, aggregate municipal \(r_{l,c,k,t}\) with weights proportional to
\(E_{c,t}(1-\pi_{0,c,k,t})\). Elector weights alone are generally incorrect for
conditional party support. Heterogeneous participation changes these weights,
so national polls can constrain participation patterns indirectly without
directly observing the total non-valid rate.

For a common full political panel, let \(\boldsymbol m_q\) be the chosen
centered poll-score centre and write the logistic-normal observation layer as

\[
\boldsymbol h_q=\boldsymbol m_q+\boldsymbol b_{g(q)}+\boldsymbol e_q,\qquad
\boldsymbol b_g\sim N(\boldsymbol0,B),\quad
\boldsymbol e_q\sim N(\boldsymbol0,R_q),\qquad
\boldsymbol p_q^{\mathrm{poll}}=\operatorname{softmax}(\boldsymbol h_q).
\]

Covariances operate in political contrast space; no non-valid survey category
is fabricated. \(R_q\) accounts for sample-dependent uncertainty using the
reported or explicitly estimated effective decided sample. Shared campaign or
survey-period errors \(b_g\) keep additional polls from eliminating common
polling uncertainty. Group definitions and covariance regularization must be
fixed before fitting. Any additional independent polling-error floor belongs
in this likelihood, not in political drift.

Map grouped/partial questions to sums of underlying political probabilities
before computing their likelihood; define shared errors on a compatible stable
basis rather than equating different questions' contrast coordinates. Respect
rounding and unavailable decided counts without inventing respondent counts.
Deduplicate derived archive views and retain same-survey dependence. Conditional
ballot alternatives must not be treated as independent generic observations.

Use fieldwork periods for measurement time (an explicit midpoint approximation
or a fieldwork-average observation); use publication and fieldwork-end dates
to determine availability at the forecast cutoff. Do not refresh a stale poll
to the cutoff. Vote-intention polls condition on declared support, which is an
approximation to future valid voting; retain that mismatch in the polling
assumptions/error model. Forecast election votes from latent political states
and the count layer, without a new poll-noise draw.

## Volatility comparisons and chamber compatibility

National comparisons are common \(\sigma_\eta\) versus partially pooled
outcome-specific \(\sigma_{\eta,j}\). Local candidates are:

| Local structure | Parameterization |
| --- | --- |
| Common | \(\sigma_{\delta,j,c}=s_\delta\) |
| Outcome-specific | \(\log\sigma_{\delta,j,c}=\mu_\delta+u_j\) |
| Municipality-specific | \(\log\sigma_{\delta,j,c}=\mu_\delta+v_c\) |
| Separable outcome + municipality | \(\log\sigma_{\delta,j,c}=\mu_\delta+u_j+v_c\) |

Center the log-scale effects and shrink them toward zero. Outcome-specific
effects include the non-valid outcome; report whether improvement is mainly
participation-related or political. Common scales are a baseline assumption,
not a claim that non-valid voting is equally volatile. Do not add unrestricted
outcome-by-municipality interactions in this comparison round.

The [paired-chamber decision](politics-reform-2026-modelling.md#camera-senato-votes-r26-04)
remains applicable in contrast coordinates. Include outcome \(0\) in the
joint chamber covariance, preserve each chamber's marginal variance budget,
retain historical gap regularization, distinct electorates/filings and existing
override precedence. Camera/European observations do not by themselves identify
Camera/Senato temporal divergence. A combined non-valid category cannot enforce
separate cross-chamber abstention/invalid-ballot identities without modeling
those components.

## Desired staged research plan

| Stage | Compare | Hold fixed / limit |
| --- | --- | --- |
| 0 | Review the electorate-wide combined-outcome formulation above | Record denominator, observation and identification requirements before new backtests |
| 1 | Literal versus aggregate polling link | Common national and local volatility |
| 2 | Direct Camera/European pooling versus regularized kind effects | Selected polling link; common volatilities |
| 3 | Common versus partially pooled outcome-specific national volatility | Common local volatility |
| 4 | Common, outcome-specific and municipality-specific local volatility | Selected national structure |
| 5 | Separable outcome + municipality local volatility if both effects help | No unrestricted interaction grid |
| 6 | Recheck the alternative national structure at the winning local structure | One limited interaction check |
| 7a | Regional innovation component versus selected simpler model | Split the local variance budget; test spatial dependence |
| 7b | Mean-reverting local differences versus selected simpler model | One pooled reversion rate around persistent historical geography |

Both 7a and 7b are planned comparisons, run separately; do not automatically
cross them with every earlier variant or combine them. Keep the all-common
drift model and the current production generator as benchmarks. Initial
identification, count observations and the accepted polling requirements are
common requirements, not extra branches of the comparison grid.

For 7a, decompose a local score difference into a shared regional process and
a municipal residual, allocating the existing marginal local variance budget
between them instead of simply adding more variance. For 7b, compare the
Brownian baseline with

\[
d\boldsymbol\delta_c
=-\kappa(\boldsymbol\delta_c-\overline{\boldsymbol\delta}_c)\,dt
+P D_{\delta,c}\,d\boldsymbol W_c,\qquad \kappa\geq0.
\]

Use one pooled reversion rate and a regularized persistent geographic centre
estimated only from training data. Reversion toward zero would erase geography
and is not the proposed variant. The non-valid outcome follows the same declared
regional/reversion construction in this comparison.

Fit each forecast using all compatible downloaded Camera/European election
and poll evidence available at its historical cutoff. Use the same time-law
across 730/365/90/15-day horizons, not separate horizon-tuned volatility
parameters. Count whole elections as independent validation events; report
Camera and European outcomes separately. Distinguish exploratory model
selection from an untouched test. No new poll downloads are required now.

Keep source-election correspondence defaults editable, dated and reproducible.
Mappings define observed groups/donors and uncertain splits, not measured voter
flows. Never use held-out municipality votes to choose a split or initialize a
new list; never map an unobserved/unmapped political list to non-valid voting
merely to complete a source table.

## Estimation and evaluation

The accepted multinomial count likelihood makes this a latent-Gaussian model
with a non-Gaussian observation layer. The aggregate poll link is nonlinear too.
Use sparse Laplace estimation as the first scalable fitting candidate, validate
against Bayesian sampling on reduced problems, and propagate state/scale
uncertainty. Exact Kalman marginalization applies only to a positive-share
Gaussian log-ratio approximation with linear observations; it is a useful
diagnostic, not an exact solver for the accepted count model.

At generation, draw future latent states and counts jointly over outcomes
including \(0\). Compute national elector shares by aggregating municipality
counts, and national political valid-vote shares by conditioning the resulting
valid totals. Neither is required to be softmax of the national latent factor.
Do not impose a sampled national endpoint or post-draw IPF to such a vector.
Reference-date user overrides need the correct aggregate-to-latent conversion;
future movement stays active.

Retain plausibility, coverage/width/proper scores and predictive-mean accuracy
in Luca's priority order. Check support, integer/elector conservation and
consistency of national summaries with municipal counts. Replace the former
sampled-margin matching diagnostic: it is not an invariant of this model.
Score non-valid/valid participation as well as political shares. Separate
abstention and ballot-validity outputs would require a later decomposition.

## References

- [Stan multinomial/logit reference](https://mc-stan.org/docs/functions-reference/multivariate_discrete_distributions.html):
  count observations on a simplex.
- [Stan multi-logit identification](https://mc-stan.org/docs/stan-users-guide/regression.html#identifiability):
  reference/centered parameterizations.
- [Kristensen et al., TMB](https://arxiv.org/abs/1509.00660):
  sparse latent-variable fitting by Laplace approximation.
