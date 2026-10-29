# Fast vote-share model: maintained numerical contract

Status: **selected for the 2026 reform workflow; FV-02 numerical port implemented**.
See the [engine entry point](../web/src/lib/politics/reform-2026/vote-share/README.md)
for reduced acceptance and remaining performance limits. Production integration
and default selection remain FV-03 through FV-08.
This document owns the selected `kind-common-conservative` mathematical model.
The equations below are extracted unchanged from the approved
[research report](vote-generation-fast-research.md). Research timing, predictive
results, failures and reproduction evidence remain in that historical report.

Read only the relevant companion contract:

- [Scenario, assets and identity](vote-generation-fast-contracts.md).
- [Reference interventions and paired-chamber boundary](vote-generation-fast-references.md).
- [Frozen numerical and production acceptance protocol](vote-generation-fast-validation.md).
- [Implementation sequence](vote-generation-production-plan.md).

The derivation below has Camera/European reference contexts. The production
Camera/Senato extension is a separately versioned contract; no European fit is
relabeled Senate. Gaussian share information, empirical-Bayes geography,
three fixed covariance updates and the forward Beta transition are deliberate
approximations. This is not the earlier grouped-count latent-state posterior.
Only arithmetic, storage and execution may change during the port; changing
these probability laws or fixed conventions needs a recorded modelling decision.

## National reference estimation

### Reading the model and its notation

The calculation has three stages. First, historical elections and polls estimate
country support **at a reference date**. Second, a stochastic transition moves
that support to the future election. Third, municipal profiles distribute each
country draw geographically, and integer votes are drawn. The reference fit does
not contain one latent variable for every municipality at every historical date.
That reduction is the main source of its speed.

The equations below describe the implemented `kind-common-conservative`
candidate. Alternatives are identified where they differ. Time is measured in
years, using 365.25 days per year; shares are fractions, so 0.01 means one
percentage point. A superscript $\top$ denotes transpose, $\operatorname{diag}(x)$
the diagonal matrix with diagonal $x$, and $\mathbf1$ a vector of ones of the
required length.

| Symbol | Meaning |
| --- | --- |
| $t_\star$, $T$, $\Delta=T-t_\star$ | Reference/cutoff date, future election date and forecast horizon |
| $j=1,\ldots,K$ | Identifiable political components; a component can itself be an indivisible historical joint unit |
| $j=0$ | Combined non-valid outcome: every elector not casting a valid list vote |
| $k\in\{\mathrm C,\mathrm E\}$ | Camera or European election context |
| $q_k=(q_{k1},\ldots,q_{kK})^\top$ | Country political support at $t_\star$, conditional on valid political voting; $q_{kj}\ge0$, $\mathbf1^\top q_k=1$ |
| $a_k$ | Country non-valid probability at $t_\star$; it is a fraction of electors, not of valid votes |
| $v_k=(a_k,1-a_k)^\top$ | Two-outcome representation used to fit non-valid versus valid support |
| $b=(b_0,\ldots,b_K)^\top$ | One future country draw on the elector denominator, including non-valid support |
| $i$, $g$ | Historical information record and one reported answer/list group in that record |
| $c$, $r(c)$ | Municipality and its region |
| $E_{ce}$, $V_{ce}$ | Electors and valid list votes in municipality $c$ at historical election $e$ |
| $\widehat\sigma$, $\widehat\sigma_0$ | Estimated country political and non-valid movement scales |
| $d_{cj}$, $s_{cj}$ | Estimated municipal log-profile and its predictive uncertainty SD at $t_\star$ |

Only cutoff-distinguishable components enter the fitted basis. An unresolved
historical joint unit remains indivisible; adding genuinely new support later
requires reference information. A proper prior can make an unidentified
posterior finite, but that alone does not make the scenario admissible.

Without a change in available components, reference elector probabilities are

$$
b_{k,0}=a_k,\qquad b_{k,j}=(1-a_k)q_{kj},\quad j\ge1.
$$

Thus a party with 20% of valid support and a 30% non-valid rate has probability
$0.7\times0.2=0.14$ on the elector denominator. Poll nonresponse is not an
observation of $a_k$: the political fit uses decided political answers, while
the non-valid fit uses election elector/count totals.

### What a grouped observation says

Record $i$ has $G_i$ reported political groups and a vector of observed shares
$r_i=(r_{i1},\ldots,r_{iG_i})^\top$, summing to one on that record's political
choice set. For an election, these are original grouped counts divided by the
corresponding national valid-vote total. For a poll, they are the prepared
political-answer proportions. Source groups are coarsened where necessary to
retain a disjoint, observable partition; internal component counts are never
invented.

Define a $G_i\times K$ **membership/incidence matrix** $S_i$ and an offered-set
row $u_i$ by

$$
(S_i)_{gj}=\mathbf1\{j\text{ belongs to reported group }g\},\qquad
(u_i)_j=\mathbf1\{j\text{ is represented in this record's choices}\}.
$$

Each row $S_{ig}$ selects the components in one group. Since the groups are
disjoint and exhaust the represented choices, $u_i=\mathbf1^\top S_i$.
An explicit Other answer represents its underlying component complement, so
those components occur both in its group row and in $u_i$; they are not absent
merely because they are unnamed.

The predicted share of group $g$, conditional on that offered set, is

$$
\pi_{ig}(q_{k(i)})=
\frac{S_{ig}q_{k(i)}}{u_iq_{k(i)}}.
$$

For example, with components $(A,B,C,D)$ and choices $(A+B,C)$,
$u_i=(1,1,1,0)$ and $S_{i,A+B}=(1,1,0,0)$. At
$q=(0.20,0.10,0.30,0.40)^\top$, the joint list has conditional share
$(0.20+0.10)/(0.20+0.10+0.30)=0.50$. This ratio compares support only among
represented political choices; it does not treat $D$ as an observed zero.

The equality $\pi_{ig}=r_{ig}$ can be rearranged as
$(S_{ig}-r_{ig}u_i)q_{k(i)}=0$. Actual observations have uncertainty, so the
model makes that expression approximately, rather than exactly, zero. Because
the answer residuals sum to zero, only $G_i-1$ are independent. Let $H_i$ be a
Helmert contrast matrix satisfying

$$
H_i\mathbf1=0,\qquad H_iH_i^\top=I_{G_i-1},\qquad
A_i=H_i(S_i-r_i u_i).
$$

Here $r_i u_i$ is an outer product. The residual vector is
$A_iq_{k(i)}$, with $G_i-1$ coordinates. Contrasts remove a redundant equation;
they do not change the group memberships or observed shares.

Stack the two reference contexts into
$x=(q_{\mathrm C}^\top,q_{\mathrm E}^\top)^\top$.
For record $i$, $X_i$ places $A_i$ in its context's block and zeros in the other.
Stacking all $X_i$ gives $X$. The Gaussian **share-information approximation** is

$$
0\mid x,\widehat C\ \sim\ \mathcal N(Xx,\widehat C),\qquad
L(x)\propto\exp\left\{-\frac12(Xx)^\top\widehat C^{-1}(Xx)\right\}.
$$

This is an estimating-residual likelihood approximation, not an exact
multinomial likelihood for the historical counts. The matrix $\widehat C$ is
constructed from training information, then held fixed during posterior
sampling. In particular, its determinant is then constant in $x$.

### Observation uncertainty and correlated historical movement

Several records should not count as independent, equally precise readings of
current support. A poll has sampling and shared survey error; an old election
has little count noise but may be far from current support. The covariance
matrix represents these different uncertainties.

For record $i$, let $n_i$ be its sample denominator: the national valid total for
an election, or the already prepared effective political sample size for a poll.
The effective sample includes the upstream sample/design-effect policy; it is
not automatically the survey's advertised total sample. Define

$$
\widetilde r_{ig}=
\begin{cases}
(y_{ig}+0.5)/(n_i+0.5G_i),&\text{election},\\
r_{ig},&\text{poll},
\end{cases}
\qquad
B_i=H_i\left[
\frac{\operatorname{diag}(\widetilde r_i)-\widetilde r_i\widetilde r_i^\top}{n_i}
+\frac{\omega_i^2}{12}I_{G_i}
\right]H_i^\top.
$$

The half-count adjustment stabilizes the **variance calculation only**; the
observed shares in $A_i$ remain $y_{ig}/n_i$, including actual zeros. The rounding
width is $\omega_i=0$ for elections and 0.001 for polls, corresponding to a
0.1-percentage-point rounding interval. The current poll preparation path still
rejects printed-zero political answers rather than pretending they were
positive measurements.

Let $\bar q$ be the current linearization point and
$m_i=\max(10^{-5},u_i\bar q)$ its offered political mass. Multiplication by
$m_i^2$ converts share-answer covariance $B_i$ to the scale of $A_iq$: the latter
is the answer-share discrepancy multiplied by offered mass.

The remaining covariance is built using two maps from component perturbations
to share perturbations:

$$
J_b=\operatorname{diag}(\bar q)-\bar q\bar q^\top,\qquad
J_p=\operatorname{diag}(s)-\bar q s^\top,
\qquad s_j=\max(\bar q_j,h_j).
$$

Both maps have columns summing to zero, so their perturbations conserve total
political probability. $J_b$ is the usual softmax derivative. $J_p$ uses a
historical scale bound $h_j$: the largest separately observed share of $j$ if
one exists, otherwise the smallest observed group share containing $j$, with
a $10^{-4}$ floor. Consequently, a party's small fitted current mean does not
erase uncertainty about its decline from a larger historical share. A joint
group's bound is a covariance convention, not an allocation of its votes.

Write $\tau_i=\max(0,t_\star-t_i)$ for record age and $\ell(i)$ for a poll's
declared half-year/election-kind error group. For elections, $\ell(i)$ is absent.
The $(i,l)$ block of the residual covariance is

$$
\begin{aligned}
\widehat C_{il}={}&
\widehat\sigma^2\min(\tau_i,\tau_l)
 A_iJ_pJ_p^\top A_l^\top\\
&+0.12^2\mathbf1\{i,l\text{ are polls with }\ell(i)=\ell(l)\}
 A_iJ_bJ_b^\top A_l^\top\\
&+\mathbf1\{i=l\}\,m_i^2B_i.
\end{aligned}
$$

A diagonal $10^{-16}I$ is added to the stacked matrix as arithmetic-noise
regularization. The terms have distinct meanings:

- **Movement:** records of ages $\tau_i$ and $\tau_l$ share the first
  $\min(\tau_i,\tau_l)$ years of a backward Brownian covariance approximation.
  More old records do not provide independent replications of the same movement.
- **Survey error:** polls in the same declared period/context share a component
  error with fixed scale 0.12. Increasing their sample sizes cannot remove it.
  These are period/context groups, not separately fitted pollster effects.
- **Sampling/rounding:** only a record's own diagonal block receives $m_i^2B_i$.

The movement covariance is shared across both context blocks; the Camera and
European reference means are distinct, but their historical movement errors
are not assumed independent. This linearized backward covariance is a fitting
approximation. The forward forecast below uses a different, explicitly defined
probability transition, rather than claiming one exact continuous-time process
underlies both calculations.

### Estimating the country movement scales

For consecutive historical elections of the **same kind**, retain political
components observed separately at both elections with actual valid shares at
least 0.005. Use half-count-smoothed shares $z_{ej}$ for these scale statistics.
For transition $e\to f$, with duration $d_{ef}$, calculate

$$
R_{ef}=\operatorname{median}_{j<h}
\frac{\left[\log(z_{fj}/z_{fh})-\log(z_{ej}/z_{eh})\right]^2}{2d_{ef}}.
$$

Pairs provide one median per eligible transition, not thousands of independent
country observations. If $M$ transitions contribute,

$$
\widehat\sigma=
\operatorname{clip}_{[0.12,0.70]}
\sqrt{\frac{4(0.35)^2+\sum R_{ef}}{4+M}}.
$$

The four default-weight units regularize a short history toward scale 0.35;
they are not fictitious elections or component counts. For non-valid fractions
$a_e^{\mathrm{obs}}=N_{e0}/E_e$, use every same-kind transition's statistic

$$
R^0_{ef}=\frac{
[\operatorname{logit}(a_f^{\mathrm{obs}})-\operatorname{logit}(a_e^{\mathrm{obs}})]^2
}{d_{ef}},\qquad
\widehat\sigma_0=
\operatorname{clip}_{[0.05,0.50]}
\sqrt{\frac{4(0.15)^2+\sum R^0_{ef}}{4+M_0}}.
$$

Here $\operatorname{logit}(p)=\log[p/(1-p)]$ and clipping restricts a value to
the stated interval. These scales are empirical estimates with finite defaults,
not sampled hyperparameters from a complete Bayesian historical-state model.
The 0.005 threshold applies only to political scale calibration; it does not
discard those components or their votes from the reference-information fit.

### Prior, context differences and the bounded posterior

Let $u_K=\mathbf1/K$ denote the uniform political simplex point; it is unrelated
to an observation's offered-set row $u_i$. The reference regularization is

$$
p(x)\propto
\exp\left\{-\frac{4}{2}\sum_k\|q_k-u_K\|^2
-\frac12\sum_j\frac{(q_{\mathrm E,j}-q_{\mathrm C,j})^2}{(0.5s_j)^2}\right\}
\prod_k\mathbf1\{q_k\ge0,\ \mathbf1^\top q_k=1\}.
$$

The first term is a weak, proper Gaussian penalty centered at equal component
shares, restricted to the simplex. The second shrinks context differences
toward zero, more strongly for components with smaller historical scales.
The factor 0.5 is a fixed scale convention, not an exactly fitted logit SD.
The pooled-kind alternative instead fits one common $q$ and has no difference
penalty.

If $D=(-I_K,\ I_K)$ and $W=\operatorname{diag}[(0.5s_j)^{-2}]$, the posterior is

$$
\begin{gathered}
Q=X^\top\widehat C^{-1}X+4I_{2K}+D^\top WD,\qquad
b_q=4(u_K^\top,u_K^\top)^\top,\\
p(x\mid\mathcal D,\widehat\theta)
\propto\exp\{-\tfrac12x^\top Qx+b_q^\top x\}
\prod_k\mathbf1\{q_k\ge0,\ \mathbf1^\top q_k=1\}.
\end{gathered}
$$

$\mathcal D$ denotes cutoff-available information; $\widehat\theta$ collects
fixed/estimated covariance conventions. The symbol $b_q$ is a precision-equation
linear term, not the future elector-probability vector $b$.

Since the covariance depends on $\bar q$, construction uses exactly three
feasible generalized-least-squares updates. Initialize uniformly; walk through
training elections, replace each observed group's total by its smoothed share
while retaining the previous within-group proportions, retain unsupported
coordinates, and normalize. Floor the resulting coordinates at $10^{-4}$ and
normalize again. This only initializes a numerical approximation: these internal
proportions never become observed component counts.

At each of three updates, construct $\widehat C,Q,b_q$ at the current $\bar q$,
find the mode on the product of simplexes, then form the next $\bar q$ from the
two context modes' average, floored at $10^{-4}$ and normalized. The final
sampler uses the third update's $Q,b_q$; it does not continuously recalculate
covariances as posterior draws change.

### Non-valid support and posterior sampling

The non-valid fit is separate from the political fit. For historical election
$i$, let $p_i=N_{i0}/E_i$ and let $\bar a$ be the latest training election's
non-valid fraction. Its residual is

$$
[(1-p_i),-p_i]v_{k(i)}=a_{k(i)}-p_i,
$$

with covariance

$$
C^0_{il}=\widehat\sigma_0^2\bar a^2(1-\bar a)^2\min(\tau_i,\tau_l)
+\mathbf1\{i=l\}\frac{p_i(1-p_i)}{E_i}+10^{-16}\mathbf1\{i=l\}.
$$

Use the same Gaussian/simplex construction with center $(0.5,0.5)$ and precision
4, and a context-difference penalty on both coordinates with scale
$0.5\bar a(1-\bar a)$. This yields a posterior on two two-outcome simplexes.
The political and non-valid reference posteriors are sampled independently;
their dependence in elector outcomes is introduced by multiplication by
$1-a_k$ and by the later choice-set/projection operations. This factorization
is another approximation relative to a joint elector-count historical model.

Both posterior targets are proper, bounded and log-concave. The native sampler
moves from $x$ to $x+zd$ along probability-transfer directions $d$, preserving
every simplex sum. Conditional on the other coordinates,

$$
z\mid x\sim\mathcal N\left(
\frac{d^\top(b_q-Qx)}{d^\top Qd},\ \frac1{d^\top Qd}
\right)
\quad\text{truncated to }\{z:x+zd\ge0\}.
$$

Updates include transfers within one context and simultaneous transfers across
contexts. Four chains are checked with rank-normalized split/folded R-hat,
bulk ESS and tail ESS; gates are R-hat <= 1.05, bulk ESS >= 400 and tail ESS >= 200.
Retained draws extend from 2,000 to 4,000/8,000 per chain when needed.
Diagnostics follow the [authors' definitions](https://avehtari.github.io/rhat_ess/rhat_ess.html);
truncated-normal sampling uses [Robert's rejection/tail proposals](https://citeseerx.ist.psu.edu/document?doi=bb1edc1cf04a42dc4f2945e440e37ff531976594&repid=rep1&type=pdf).

These updates sample the specified **conditional bounded Gaussian target**.
They do not make the Gaussian share approximation exact for historical counts,
or integrate uncertainty in the fitted covariance matrix. Numerical convergence
and predictive calibration remain separate checks.

## Forecast uncertainty and predictive means

### Drawing reference support and movement scales

For the target context $k$, draw $q_k$ and $v_k$ from their reference posteriors.
Each predictive draw also receives independent scale multipliers:

$$
\sigma^{(s)}=\operatorname{clip}_{[0.06,1]}
 [\widehat\sigma\exp(0.2Z_s)],\qquad
\sigma_0^{(s)}=\operatorname{clip}_{[0.025,0.70]}
 [\widehat\sigma_0\exp(0.2Z^0_s)],\qquad Z_s,Z_s^0\sim\mathcal N(0,1).
$$

This declared lognormal mixture propagates uncertainty about movement magnitude;
it is not a fitted hyperparameter posterior. It changes dispersion without
introducing a directional trend. Before clipping, the estimated scale is the
mixture's median, rather than its mean. The following transition is applied separately
to the political simplex $q_k$ and the two-outcome simplex $v_k$.

### The finite-step Beta transition

Define $\mathcal T_h(p;\sigma)$ for an input simplex $p$, horizon $h\ge0$ and
movement scale $\sigma$. The scale is common across components in the selected
candidate; the scenario prototype can supply component-specific values.

For $h>0$, take $M=\lceil h/0.25\rceil$ steps of length $\delta=h/M$.
At every step and for every draw, independently sample a uniform permutation
$\pi$ of the $L$ simplex outcomes. Given the **old**, pre-step probabilities,
define the conditional fractions

$$
R_\ell=1-\sum_{m<\ell}p_{\pi_m},\qquad
\mu_\ell=p_{\pi_\ell}/R_\ell,\quad \ell=1,\ldots,L-1.
$$

These fractions describe how much of the remaining probability each outcome
receives. A zero remaining mass is assigned conditional fraction zero; its
outcome necessarily receives zero. Draw independent conditional sticks

$$
\begin{aligned}
\nu_\ell&=\sigma_{\pi_\ell}^2\delta\,\mu_\ell(1-\mu_\ell),\\
\kappa_\ell&=\max\left(2,\frac1{\max(\nu_\ell,10^{-15})}-1\right),\\
B_\ell&\sim\operatorname{Beta}
 (\mu_\ell\kappa_\ell,(1-\mu_\ell)\kappa_\ell),\\
p'_{\pi_\ell}&=B_\ell\prod_{m<\ell}(1-B_m),\qquad
p'_{\pi_L}=\prod_{m<L}(1-B_m).
\end{aligned}
$$

Thus probabilities stay nonnegative and sum to one. An exact conditional
fraction of zero/one, or a zero scale for that stick, is retained exactly;
$h=0$ or an all-zero scale vector returns the input unchanged. The code clips
interior means at $10^{-12}$ and $1-10^{-12}$ for numerical stability.
With component-specific scales, fixing one stick's fraction does not generally
fix that component's absolute share: earlier sticks can change the remainder.

The quantity $\nu_\ell$ is a **concentration-control quantity**, not the Beta
variance itself. The actual conditional variance is

$$
\operatorname{Var}(B_\ell\mid p,\pi)
=\frac{\mu_\ell(1-\mu_\ell)}{\kappa_\ell+1}
\approx \sigma_{\pi_\ell}^2\delta
 [\mu_\ell(1-\mu_\ell)]^2,
$$

where the approximation applies when numerical and concentration floors are
inactive. This gives roughly logit-scaled small-step variation for each stick;
$\sigma$ is not a percentage-point SD of each component. The floor
$\kappa\ge2$ limits very diffuse steps. Randomizing the order removes a fixed
privilege for the first or last component, while retaining simplex dependence.
This is the implemented finite-step kernel, not an assertion that the resulting
vector has a Dirichlet distribution or follows an exact logit Brownian process.
The fitted movement scale calibrates this forward kernel operationally; the
backward Gaussian approximation and forward Beta kernel do not have identical
covariances on every political contrast.

### What mean preservation does and does not imply

Before arithmetic clipping, $E[B_\ell\mid p,\pi]=\mu_\ell$. Conditional stick
independence then gives

$$
E[p'_{\pi_\ell}\mid p,\pi]
=\mu_\ell\prod_{m<\ell}(1-\mu_m)=p_{\pi_\ell}.
$$

Taking expectations over permutations, steps and scale mixtures preserves this:
$E[\mathcal T_h(p;\sigma)\mid p]=p$. Consequently the country forecast mean,
before choice-set changes, remains the posterior mean of reference support.
There is no automatic Jensen shift from adding zero-centered noise on logits
and then applying softmax. Arithmetic floors introduce only a numerical
qualification; the independent large-draw fixture checks the implemented means.

This is a **conditional martingale property**, not a claim that the reference
estimate predicts an unseen election without bias. The model has uncertainty
about future movement but no fitted directional momentum. It can miss a real
trend, a poll bias or a newly formed list. Nor does this property establish
unbiased municipal means after nonlinear geographic allocation.

### Election-realization discrepancy

Even shortly before an election, support inferred from polls may not equal the
realized result. A separate political discrepancy scale $\widehat\eta$ is
estimated using **training elections only**. For each eligible election, take
the latest compatible same-kind poll, published and fielded by election day,
whose representative date is at most 45 days earlier. Coarsen the poll and
election to common observable groups and normalize on their common support.

For two such groups $g,h$ with both actual shares $z$ and reported shares $r$
at least 0.005, define

$$
\begin{aligned}
e_{e,gh}&=\log(z_{eg}/z_{eh})-\log(r_{eg}/r_{eh}),\\
U_e&=\operatorname{median}_{g<h}
\max\left(0,\frac{e_{e,gh}^2}{2}
-\frac{1/r_{eg}+1/r_{eh}}{2n_e}
-\widehat\sigma^2 d_e-0.12^2\right),\\
\widehat\eta&=\operatorname{clip}_{[0.10,0.50]}
\sqrt{\frac{4(0.25)^2+\sum_eU_e}{4+M_\eta}}.
\end{aligned}
$$

$d_e$ is the poll-to-election gap in years, $n_e$ the poll's effective sample,
and $M_\eta$ the number of contributing elections. Subtractions account for
sampling, elapsed movement and already modeled poll error; the remainder is
pooled toward default scale 0.25. This is a regularized discrepancy statistic,
not an exact variance decomposition of a fully joint fitted model.

The selected candidate uses multiplier 1.5; the less conservative alternative
uses 1. Its final country draw is

$$
\begin{aligned}
q^{\mathrm{move}}&=\mathcal T_\Delta(q_k;\sigma^{(s)}),&
q^T&=\mathcal T_1(q^{\mathrm{move}};1.5\widehat\eta),\\
v^T&=\mathcal T_\Delta(v_k;\sigma_0^{(s)}),&
b&=(v^T_0,\ v^T_1q^T_1,\ldots,v^T_1q^T_K)^\top.
\end{aligned}
$$

The subscript 1 on the discrepancy transition is a **unit exposure convention**
for its variance, not an extra calendar year. It remains present at a zero
calendar horizon when enabled, and preserves conditional political means.
There is no corresponding extra non-valid discrepancy step in this candidate.
It represents election uncertainty, not a new survey-error draw applied to votes.

## Municipal predictive module

### Historical geographic features

The country draw determines how much support exists overall. Municipal features
describe where that support tends to be stronger or weaker. They are estimated
from historical observed outcomes, rather than fitting a second independent
likelihood to the same counts.

For a historical municipality/election, write $N_{ce0}=E_{ce}-V_{ce}$ for its
combined non-valid count and $y_{ceg}$ for an observed political group's count.
Let $a_e^{\mathrm{obs}}$ be the national non-valid fraction and $z_{eg}$ the
national valid share of that **same observable group**. The features and their
approximate measurement-noise variances are

$$
\begin{aligned}
\widetilde a_{ce}&=(N_{ce0}+0.5)/(E_{ce}+1),\\
f_{ce0}&=\operatorname{logit}(\widetilde a_{ce})
-\operatorname{logit}(a_e^{\mathrm{obs}}),&
R_{ce0}&=\frac1{N_{ce0}+0.5}+\frac1{V_{ce}+0.5},\\
f_{ceg}&=\log\frac{(y_{ceg}+0.5)/(V_{ce}+0.5G_{ce})}{z_{eg}},&
R_{ceg}&=\frac1{y_{ceg}+0.5}.
\end{aligned}
$$

$G_{ce}$ is the number of observed political groups in that municipality.
An offset near $\log 2$ for a political group means its local valid-share
feature is about twice the national one. An offset near zero means no observed
relative advantage. The non-valid feature instead compares local and national
non-valid **odds**. These are different feature definitions, though both later
enter the municipal log-score vector.

The half-counts stabilize log features and noise estimates only. They do not
alter source counts or become fractional count observations. A feature is
skipped if national coarsening leaves no exact matching group baseline.
National baseline probabilities in these logarithms have a $10^{-10}$
arithmetic floor; this does not replace a zero source count by observed support.

For a component $j$, use its separately observed feature history when any is
available. Otherwise use its most recent observed joint group's feature as a
pooled fallback, marked with extra uncertainty. This applies one group feature
to its members; it does not estimate their separate historical vote counts.
If even that feature is unavailable, use a regional/national fallback.

### Regional shrinkage and local movement estimates

For each usable feature history $\mathcal E_{cj}$, construct an initial average

$$
w_e=2^{-(t_\star-t_e)/5}
\begin{cases}2,&k(e)=k(T),\\1,&\text{otherwise},\end{cases}
\qquad
\bar f_{cj}=\frac{\sum_{e\in\mathcal E_{cj}}w_e f_{cej}}{\sum w_e}.
$$

The weights have a five-year half-life and double the weight of the target kind.
Let $I_{cj}$ be average exposure over those entries: electors for non-valid
features, valid voters for political features. The regional center $\gamma_{rj}$
averages available $\bar f_{cj}$ with weights $\sqrt{I_{cj}+1}$, or is zero if
none exist. Initialize

$$
m_{cj}^{\mathrm{init}}=
\frac{I_{cj}}{I_{cj}+500}\bar f_{cj}
+\frac{500}{I_{cj}+500}\gamma_{r(c)j}.
$$

Without a local feature, take $m_{cj}^{\mathrm{init}}=\gamma_{r(c)j}$.
The constant 500 controls shrinkage of sparse local histories; it is not an
added block of observed voters.
For this initialization, a municipality uses its latest available training
region, or target metadata if no training row exists. Future generation uses
the target region map. Historical municipality IDs can contribute to profile
construction even when they are absent from the future active geography.

Estimate local movement from consecutive chronological training elections,
including transitions between kinds. For component $j$ and transition $e\to f$,
use municipalities with separately observed features at both ends. With at
least three such municipalities, calculate

$$
L_{ef,j}=\max\left(0,
\frac{\operatorname{Var}_c(f_{cfj}-f_{cej})
-\operatorname{Mean}_c(R_{cej}+R_{cfj})}{t_f-t_e}\right).
$$

The variance is the sample variance across municipalities. Removing national
levels through the features and subtracting approximate count noise targets
changes in geographic patterns, rather than country movement or small-count
noise. It remains a moment estimator, not a fitted latent transition posterior.
Because transitions between kinds are included, it can absorb kind-dependent
geographic change as well as temporal change.

Define the common annual local variance and component filter scales by

$$
\begin{aligned}
L_*&=\operatorname{clip}_{[0.03^2,0.8^2]}
 \operatorname{median}_{e,f,j}L_{ef,j},\\
(\tau_j^{\mathrm{fit}})^2&=
\operatorname{clip}_{[0.03^2,0.8^2]}
\frac{4L_*+\sum_{ef}L_{ef,j}}{4+M_j}.
\end{aligned}
$$

Use $L_*=0.25^2$ if no transition statistic exists. $M_j$ counts contributing
transitions, not municipalities. The four regularization units pull sparse
component estimates toward the common value.

### Filtering each profile to the reference date

Start with mean $m=m_{cj}^{\mathrm{init}}$ and variance $P=1$. Process its
eligible feature entries chronologically. Before an entry after the first,
propagate variance by elapsed time; the first uses $P^-=1$. For each entry,

$$
\begin{aligned}
P^-&=P+(\tau_j^{\mathrm{fit}})^2(t_e-t_{\mathrm{previous}}),\\
\mathcal R_{cej}&=R_{cej}+0.15^2\mathbf1\{k(e)\ne k(T)\},\\
G_{cej}&=P^-/(P^-+\mathcal R_{cej}),\\
m&\leftarrow m+G_{cej}(f_{cej}-m),\qquad
P\leftarrow(1-G_{cej})P^-.
\end{aligned}
$$

The gain $G_{cej}$ is large when the feature is precise relative to current
profile uncertainty. The 0.15 term weakens borrowing across election kinds.
After the last entry, propagate $P$ to $t_\star$ with the same annual variance.
The returned profile parameters are

$$
d_{cj}=m,\qquad
s_{cj}^2=P+
\begin{cases}
0.10^2,&\text{separate feature history exists},\\
0.45^2,&\text{pooled or missing-history fallback}.
\end{cases}
$$

Without any local entry, retain the fallback mean and $P=1$; do not create zero
historical counts. The regional initialization reuses the feature history that
the filter subsequently processes. These are empirical-Bayes predictive
features, not independent-prior-plus-likelihood Bayesian posterior moments.
Their uncertainties must therefore be assessed by backtesting.

The **selected common-local candidate** uses $\tau_j^{\mathrm{future}}=\sqrt{L_*}$
for every future outcome, including non-valid. The component-local alternative
uses $\tau_j^{\mathrm{future}}=\tau_j^{\mathrm{fit}}$. In both, profile filtering
and its returned $s_{cj}$ still use the component filter scales just defined.

### From a country draw to municipal probabilities

Future electorates $E_c$, regions and component availability are conditional
scenario inputs. Future observed votes are used only to score a backtest, not
to fit $q,a,d,s$ or movement scales. In the research replay these conditional
inputs come from the held-out election's geography/ballot metadata.

Let $I_{cj}$ indicate whether component $j$ is offered in municipality $c$;
$I_{c0}=1$. Components absent everywhere are removed from the country draw:

$$
I_j=\max_cI_{cj},\qquad
\widetilde b_j=\frac{I_jb_j}{\sum_{h=0}^KI_hb_h}.
$$

This retains the approved choice-set substitution rule: renormalize all
remaining outcomes, **including non-valid support**. It is not a redistribution
restricted to valid political votes.

For each predictive draw, sample independent standard normals $Z_L$,
$\xi_{cj}$, $\epsilon_{cj}$ and $\zeta_{rj}$. The single $Z_L$ is shared across
all municipalities/components in that draw, representing uncertain local
movement magnitude. $\zeta_{rj}$ is shared by municipalities of region $r$;
$\xi$ and $\epsilon$ are municipality-specific. Set $\rho=0.15$ and define

$$
\begin{aligned}
\ell_{cj}={}&\log\widetilde b_j+d_{cj}+s_{cj}\xi_{cj}\\
&+\tau_j^{\mathrm{future}}\exp(0.2Z_L)\sqrt\Delta
 \left[\sqrt{1-\rho}\,\epsilon_{cj}+\sqrt\rho\,\zeta_{r(c)j}\right],\\
p^{(0)}_{cj}={}&
\frac{I_{cj}\exp(\ell_{cj})}{\sum_{h=0}^KI_{ch}\exp(\ell_{ch})}.
\end{aligned}
$$

Absent outcomes have log score $-\infty$. A zero country target is retained as
zero by the following projection; tiny log floors only support arithmetic.
The terms separate uncertainty in the **reference geographic profile**
($s\xi$, present even at $\Delta=0$) from **future local movement** (proportional
to $\sqrt\Delta$). The 15% regional fraction splits one movement variance budget,
rather than adding another full variance on top. Before softmax/projection,
conditional on $Z_L$, same-region municipalities share movement covariance
$\rho\Delta\tau_j^2\exp(0.4Z_L)$ for outcome $j$.
The option without regional noise sets $\rho=0$. Softmax and the next projection
introduce dependence across outcomes and municipalities even when the underlying
normal draws are otherwise independent.

### Country-margin projection and integer list votes

The raw probabilities $p^{(0)}$ generally do not have country mean
$\widetilde b$: historical profiles, nonlinear noise and local availability
change their aggregate. For each country draw, choose common outcome offsets
$\lambda_j$ such that

$$
p_{cj}=\frac{I_{cj}\exp(\ell_{cj}+\lambda_j)}{
\sum_hI_{ch}\exp(\ell_{ch}+\lambda_h)},\qquad
\sum_cw_cp_{cj}=\widetilde b_j,\qquad
w_c=E_c/\sum_dE_d.
$$

An outcome with zero target can have offset $-\infty$. Otherwise the offsets
are only identifiable up to an additive constant, which does not
affect probabilities. The implemented raking iteration starts from $p^{(0)}$,
computes $m_j^{(n)}=\sum_cw_cp_{cj}^{(n)}$, multiplies each column by its
target/current ratio, then normalizes each row:

$$
p_{cj}^{(n+1)}=
\frac{p_{cj}^{(n)}\widetilde b_j/m_j^{(n)}}{
\sum_h p_{ch}^{(n)}\widetilde b_h/m_h^{(n)}}.
$$

Structural zeros stay zero. The implementation handles zero-target columns
explicitly and stops when the maximum margin discrepancy is at most $10^{-9}$,
with at most 120 iterations. An unavailable positive target, an empty municipal
choice set or non-convergence is an error, not permission to alter the target
or fabricate local support. Geographic filing restrictions can make a requested
country composition infeasible.

**This is conditional projection of probabilities before count generation**, an
explicit change from the earlier model's prohibition on endpoint reconciliation.
It imposes the country draw as an expected elector-share margin, while retaining
relative geographic patterns as adjusted by common outcome offsets. It does
not force realized integer country totals to equal an independently drawn count.

Finally, conditionally independently across municipalities, draw

$$
(N_{c0},N_{c1},\ldots,N_{cK})\mid p_c
\sim\operatorname{Multinomial}(E_c,p_c).
$$

For a scenario list with component membership $\mathcal J_{cL}$,

$$
Y_{cL}=\sum_{j\in\mathcal J_{cL}}N_{cj},\qquad
N_j^{\mathrm{country}}=\sum_cN_{cj}.
$$

Every draw has integer counts and $N_{c0}+\sum_LY_{cL}=E_c$ when lists partition
the offered components. Grouping the same components into a joint list simply
sums their existing counts. Country results are always municipal sums, with

$$
E\left[\frac{\sum_cN_{cj}}{\sum_cE_c}\ \middle|\ \widetilde b\right]
=\widetilde b_j.
$$

Count noise remains around that margin.
The displayed expectation is exact for the ideal projection; the implemented
probability margin is accurate to the stated $10^{-9}$ tolerance.
Municipal valid-list shares instead use denominator $E_c-N_{c0}$, so they must
not be confused with elector shares.
The conditional mean of one municipality is $E_c p_{cj}$; averaging over profile
noise and projection is nonlinear and need not equal a softmax of mean scores.
Thus neither country martingale dynamics nor count conservation proves unbiased
municipal predictions.

The resulting hierarchy separates reference-posterior uncertainty, future
country movement, political election discrepancy, geographic profile
uncertainty, regional/local movement and integer sampling. It remains a modular
empirical-Bayes predictive model, not a fully joint Bayesian posterior over all
historical country and municipal states. Its substantive approximations require
the predictive checks reported below.

