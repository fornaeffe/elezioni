# Candidate definitions for the chamber campaign

Use the [protocol](protocol.md) for the data boundary, folds, scores and execution;
use [results](results.md) for measurements. These definitions are research laws,
not additions to the browser registry. Current [M01](../../../docs/models/M01.md)
and [M02](../../../docs/models/M02.md) specifications remain authoritative.

## Why the current dependence construction needs testing

For current M02, two Gaussian-copula uniforms select two row indices from the
same stored empirical posterior. Each chamber has the correct marginal row
distribution, but the joint distribution depends on the order of the stored
rows. MCMC chain/iteration order is not a vote-share quantile function.
Consequently a latent index correlation of 0.85 does not imply a political
posterior correlation of 0.85. Reordering entire rows leaves the empirical
posterior unchanged and can nevertheless change inter-chamber dependence.
`code/diagnose.ts` measures this on the actual released fitted state, including
the effect of sorting complete rows by FdI support without changing any row.

This is separate from profile noise and future movement. Current M02 also draws
two profile fields and two bounded mass-pair trajectories. Even rho=1 does not
guarantee a fixed chamber contrast: different starting support changes mass-ranked
pairings and subsequent nonlinear paths. Increasing a single correlation setting
is therefore not a complete statistical specification of a narrow chamber gap.

## M02-shared research-v1

Draw one full M02 Camera probability field, including its posterior row, profile
uncertainty, national/regional/local movement, discrepancy and scale uncertainty.
Let its non-valid probability be $a_c$ and conditional political vector $q_c$.
From the paired-history adapter retain national contrasts $d_j$, non-valid
log-odds contrast $d_0$ and local profile differences $b_{cj}$. Put
$g_{cj}=d_j+b_{cj}$, including outcome zero, then define

$$
a_{Sc}=\operatorname{logistic}(\operatorname{logit}a_c+g_{c0}),\qquad
q_{Scj}=\frac{q_{cj}\exp(g_{cj})}{\sum_kq_{ck}\exp(g_{ck})}.
$$

Both chambers retain their own multinomial ballot counts conditional on the
probabilities and forecast electorate. Camera's probability law and seeded
counts are exactly those of the current M02 research adapter. There is no second
latent national state, no second profile-uncertainty field and no second future
movement simulation. This reduces work and the opportunity for arbitrary chamber
decorrelation; it does not shrink Camera's uncertainty to fix a joint problem.

The simplification assumes a fixed descriptive chamber contrast. It does **not**
estimate uncertainty in a future change of that contrast. When earlier contrast
patterns transfer poorly, its intervals can undercover. Neither a good in-sample
2022 reconstruction nor the Camera marginal proves a calibrated joint forecast.
Research here also assumes the same six families are filed throughout the domain.
Different future physical filings require a deliberate common unrestricted field
and separate conditioning; that production design is not implemented here.

## M05/M06/M07 research-v1

Let $K=6$, $H$ the $(K-1)\times K$ orthonormal Helmert basis and
$x=(H\log q,\operatorname{logit}v)$, where $v$ is valid votes/electors.
Historical transformed shares use the declared 0.005 floor followed by
renormalization. It is a proxy for the endpoint model, never an added observed
count or a structural filing rule. The state has dimension six.

The central national vector uses the latest eligible complete poll. Within each
reported group, allocate its *forecast support* in proportion to the latest
historical family shares; an empty historical group receives equal proportions.
This is a conditional point-prediction assumption, not historical count recovery.
Without a poll retain the baseline national vector. Valid-rate center uses the
latest eligible same-kind event, with a latest-event fallback.

For $n$ earlier completed forecast errors $e_b$, rebuilt at the target fold's
own horizon $h$, use uncentered endpoint error covariance

$$
C_N=\frac{\operatorname{diag}(v_h,\ldots,v_h,0.2^2)+\sum_be_be_b^\top}{n+1},
\qquad v_h=0.2^2+0.35^2h/365.25,\qquad \nu=n+3.
$$

One completed election supplies one error vector. The zero error center is
retained, so persistent bias contributes to uncertainty rather than being fitted
away. Geographic residuals subtract the national change, then the valid-weighted
regional change. With transition length $T_b\ge0.25$ years:

$$
C_R=\frac{0.12^2I+\sum_b\sum_r r_{br}r_{br}^\top/(R_bT_b)}{n+1},\qquad
C_L=\frac{0.18^2I+\sum_b\sum_c l_{bc}l_{bc}^\top/(M_bT_b)}{n+1}.
$$

Forecast spatial exposure is the baseline-to-election age $A\ge0.25$ years.
M05 draws one national $N(0,C_N)$ innovation, independent regional
$N(0,AC_R)$ and local $N(0,AC_L)$ innovations. It preserves cross-family and
share/valid-rate covariance inside each vector.

M06 multiplies each such vector by an independent
$\sqrt{(\nu-2)/\chi^2_\nu}$, sharing that scalar among all coordinates of
that vector. It has the same covariance as M05 and heavier joint tails; this is
not a robust Student-t likelihood fit.

M07 retains Gaussian national innovation. Profiles pool eligible municipal states
with an eight-year half-life after removing their event national state and adding
the latest baseline national state. One completed transition and one sign are
sampled for the entire country. Its regional/local residual field is scaled by
$\sqrt{A/T_b}$ and supplemented by 0.35 times the Gaussian spatial innovation.
Missing residuals keep the regularizing noise. With no available transition,
retain the full Gaussian field. Sparse observed transitions and sign symmetry
are assumptions, not independent bootstrap elections.

For all three, shift local valid-rate logits to the drawn national valid rate,
then rake local political shares using **forecast electors times sampled valid
rates** to the drawn national political vector (tolerance $10^{-8}$).
Draw municipal multinomial counts. When comparing chambers, apply the same
paired-contrast pushforward as M02-shared before the independent Senate count
draw. No European coordinate is relabelled a Senate state. Priors and transition
budgets are inherited conventions, without fitting new constants to 2022.
