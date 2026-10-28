# Fast vote generation: reference and chamber contract

FV-01 revision **1**, 28 October 2026. These are interventions applied to the
accepted reference state, not additional historical observations or poll rows.
Implementation/independent numerical acceptance belongs to FV-04. The numerical
historical model remains the [selected model](vote-generation-fast-model.md).

## Approved turnout clarification

Luca explicitly chose on 28 October 2026: **keep turnout fixed for list-reference
edits; changing turnout is a separate input**. This preserves the installed
reform behavior and deliberately differs from `fast_scenario.py`, whose free
elector-weight renormalization can change non-valid support. Use reference policy
`fixed-turnout-reference-v1`; do not describe that prototype as production parity.

For one reference draw, write $b=(a,(1-a)q)$, with non-valid elector probability
$a$ and valid-political simplex $q$. Apply political references to $q$, obtaining
$q'$, then return $b'=(a,(1-a)q')$. At the local boundary keep each cell's existing
non-valid probability fixed unless it has an explicit non-valid intervention.
This does not remove posterior uncertainty in $a$, future participation movement,
profile uncertainty or multinomial count noise.

**Choice-set removal is a different operation.** First remove globally absent
components and renormalize all remaining elector outcomes, including non-valid,
as in the approved model. Then apply political references holding the resulting
non-valid state fixed. Adding a referenced new component allocates valid support
and does not itself change turnout. Regional structural zeros still need the
global probability projection; no unavailable outcome receives support.

## Disjoint groups and exact references

At one scope/channel, reference subjects select disjoint component groups $G_g$.
Within a group preserve the base draw's component proportions. A joint list
without a reference simply sums existing counts. A new component has a declared
fallback and required reference; it supplies no historical component counts.
An intervened all-zero fitted group is invalid positive support unless an
explicit new/missing-history support policy supplies a labelled prior.

Let exact group references total $F$, uncertain groups have sampled shares
$R_g$, and free fitted components have total base valid mass $q_U$.

$$
q'_j=R_g\frac{q_j}{\sum_{h\in G_g}q_h}\quad(j\in G_g),\qquad
q'_j=\left(1-\sum_gR_g\right)\frac{q_j}{q_U}\quad(j\in U).
$$

Exact references have $R_g=m_g$ in every reference draw. Preserve the free
components' ratios, not their absolute masses. If $U$ is empty, all group shares
must sum to one in every draw. If $U$ is nonempty but has no supported mass and
the remaining requested mass is positive, fail; never seed arbitrary epsilon.
Exact zero support is retained, not treated as an omitted reference.

Example with $a=0.30$, $q=(0.20,0.30,0.50)$ and exact A=40%:
$q'=(0.40,0.225,0.375)$ and
$b'=(0.30,0.28,0.1575,0.2625)$. Joint B+C has 60% valid support;
the non-valid outcome remains 30%. Adding new N=10% with no other intervention
instead gives $q'=(0.18,0.27,0.45,0.10)$ and unchanged $a$.

## Multiple uncertain references

The production family is a **joint calibrated logistic-normal simplex**, not
independent percentage draws followed by clipping/normalization. This extends
the prototype's single uncertain logistic-normal reference while retaining its
fraction/percentage-point SD semantics. The chosen default dependence is induced
by the simplex; there is no user-entered arbitrary covariance matrix in v1.

After exact references, let $U_*=1-F$. Form categories from the $M$ uncertain
groups and, when present, one free residual group. Draw Gaussian log weights
$Z_g=\mu_g+s_g\epsilon_g$, independent standard normal $\epsilon_g$, and set

$$
R_g=U_*\frac{\exp Z_g}{\sum_h\exp Z_h}.
$$

Calibrate the uncertain categories to $E[R_g]=m_g$ and
$\operatorname{SD}(R_g)=d_g/100$ **after** the joint transform. With a free residual,
fix its Gaussian weight to zero; it absorbs the unused mass and has no separately
specified moments. Without a free residual, center $\mu$ to remove its additive
non-identifiability. Among calibrated diagonal-score solutions use minimum
$\sum_gs_g^2$, then minimum $\sum_g\mu_g^2$, with stable-ID tie resolution;
numerical calibration/solver tolerances are versioned and part of fitted state.
All hard categories have already been eliminated and are not Gaussian weights.

For one uncertain group and a free residual this is exactly
$U_*\operatorname{logistic}(\mu+s\epsilon)$, the prototype's family with the
appropriate upper mass. Two fully specified uncertain groups necessarily have
equal SDs and complementary means. Never promise independently variable SDs
for every member of a fully specified simplex. Means must lie within support,
sum to at most $U_*$, and SDs must obey at least
$0\le d_g/100<\sqrt{m_g(U_*-m_g)}$ for interior continuous references. Boundary
means require zero SD. These necessary conditions are not proof of simultaneous
logistic-normal representability.

Require independent integration/moment checks within $10^{-6}$ share units for
each requested mean and SD, with integration refinement below $10^{-7}$.
If moments cannot be represented/calibrated, return
`REFERENCE_MOMENTS_UNSUPPORTED`; expose the offending groups/moments. Do not
silently reduce SD, shift means, pretend the fit is accepted or limit the editor
to one uncertain list. Numerical work in FV-04 must demonstrate multiple-uncertain
partial and fully specified examples before acceptance. A different distribution
or dependence policy requires a new recorded reference-model decision/version.

Examples for contract tests:

- A=20% exact, B mean=30%, SD=2 pp, free C: B lies in $(0,0.8)$, C=0.8-B,
  A remains exactly 0.2. Its calibration is on mean 0.3 and SD 0.02, not 2.
- A mean=30%, SD=2 pp and B mean=40%, SD=3 pp with free C: calibrate jointly;
  C=1-A-B is nonnegative in every draw. Do not sample A/B independently on
  $(0,1)$ and repair cases whose sum exceeds one.
- Fully specified A/B with means 0.4/0.6 and SDs 2/2 pp is complementary;
  SDs 2/3 pp is inconsistent and must fail.
- A+B and B+C references overlap: reject `REFERENCE_OVERLAP`; do not invent a
  within-B solution. Duplicate new-component/list references must be collapsed
  only when they declare the identical subject/value; contradictory rows fail.

If every active list has an explicit **central** reference and central means
do not sum to one, retain the installed normalization convention: divide means
and their SDs by that positive deterministic total and record the before/after
values in a warning. Zero total is an error. This is input normalization before
joint calibration; never normalize each stochastic draw after calibration.
Partial overfull central references and incompatible exact sums fail explicitly.

## Common/chamber precedence and scoped interventions

Retain the installed common-anchor convention. For a group with compatible
baseline valid shares $p_C,p_S$, use

$$
\delta=0.5\,g\operatorname{clip}_{[-1,1]}
 [\operatorname{logit}(p_S)-\operatorname{logit}(p_C)],\qquad
r_C=\operatorname{logistic}(\operatorname{logit}r-\delta/2),\quad
r_S=\operatorname{logistic}(\operatorname{logit}r+\delta/2).
$$

$g$ is the declared historical-gap sensitivity multiplier. Endpoint anchors
0/1 remain exact; an absent positive baseline needs an explicitly labelled
contrast fallback, not a manufactured logit count. Use prepared compatible
reference baseline means at that scope, not future truth. Common mean/SD inputs
describe the anchor distribution **before** this nonlinear map; derived chamber
means/SDs need not numerically equal them. The same sampled common anchor drives
both derived references. An explicit chamber reference replaces only that
chamber's derived reference and uncertainty, retaining the other derived value.

| Same-scope inputs | Camera | Senato |
| --- | --- | --- |
| None | Compatible fitted baseline | Compatible fitted baseline |
| Common only | Derived from common anchor | Derived from same anchor |
| Common + Camera | Explicit Camera | Common-derived Senato |
| Common + Senato | Common-derived Camera | Explicit Senato |
| Both chamber-specific, common optional | Explicit Camera | Explicit Senato |
| Only one chamber-specific | Explicit for that chamber | Other chamber retains baseline |

Apply reference resolution in national -> region -> province -> municipality
order. At each scope resolve explicit chamber over common-derived over baseline.
More specific overrides replace the affected cells for the referenced subjects;
they do not freeze all other lists. Preserve each affected cell's non-valid
probability and relative free-list support. Validate aggregate targets using
elector-weighted **valid-vote denominators** and actual filing/capacity.

Example: two equal-valid-population regions start at A=0.4 after a national
reference. A later region-1 A=0.8 reference yields final A=0.6 nationally; the
national 0.4 is superseded for those cells. Trace this, rather than reimposing
the earlier national margin or claiming both incompatible equalities hold.
For the same reason, derive the post-intervention country reference margin by
aggregating the effective local reference probabilities. Recenter the geographic
profile representation on that margin before applying future country/local
movement. The no-local-intervention case retains the model's original country
margin. Interventions never become election-day fixed outputs.

Derived or uncertain chamber/scoped targets must pass the same joint simplex,
moment and geographic checks. If nonlinear common transforms plus explicit
chamber targets cannot coexist, fail with a reference conflict; do not clip,
silently renormalize partial targets or resample failed simulations. Individual
infeasible projection/count/legal paths remain errors with requested-run
denominators intact. Forecast movement may change a zero-SD reference later.

Non-valid interventions use elector weights, not valid-vote weights. They
preserve within-cell political relative shares before political references are
applied, and may specify exact or calibrated uncertain targets separately.
They never reinterpret poll nonresponse as turnout. Exact aggregate non-valid
support need not make every local non-valid probability identical.

## Paired-chamber extension boundary

The C/E historical fit and the future C/S joint simulation are different
objects. FV-04 must supply a versioned Senate reference adapter/fit, a paired
component identity/contrast table, compatible 18+ electorates and explicit
historical-age/geography assumptions. A fitted-state envelope must identify
whether each chamber uses direct observations or a labelled contrast adapter.
One paired 2022 election cannot estimate general future forecast-error covariance.

Freeze the coupling interface: input is accepted marginal reference/profile
states plus a declared dependence profile; output has paired simulation IDs and
the same individual chamber marginal laws/variance budgets. Coupling can share
random innovations/quantiles but cannot add a second unbudgeted shock or reuse
European draws as Senate. A shared seed is not evidence of dependence accuracy.
Exact coupling mechanics remain FV-04's separately tested implementation, not
an FV-01 claim of a validated Senate model.

Test common-only, either/both explicit references, single-chamber renewal,
zero/doubled gaps and weaker/central/stronger dependence. Freeze baseline
R26 profiles as **assumptions**, not measured correlations: effective shock
correlations 0.50/0.85/0.98, obtained from the existing shared/residual variance
budget. Validate marginal preservation and paired bonus/seat sensitivity. Do
not change legal bonus eligibility, domestic/foreign scope, candidate or
preference rules to accommodate a generated distribution.
