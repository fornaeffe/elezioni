## 2026-10-28 — Formalize the fast research model description

Expanded the three model-definition sections of
[the fast research report](../../../research/vote-generation/docs/vote-generation-fast-research.md) at Luca's request.
Added a notation/denominator guide and worked grouped-choice example; defined
the observation matrices, residual covariance, movement-scale estimators,
bounded Gaussian priors/posteriors, three construction updates and separate
non-valid fit. Defined the finite-step Beta transition, its actual variance and
conditional mean preservation, scale mixtures and election-discrepancy estimate.
The municipal section now specifies historical features, regional shrinkage,
local scale estimation/filtering, fallback uncertainty, regional/local random
scores, country-margin raking and integer list/count aggregation.

Checked equations against `fast_party_model.py`, `run_fast.py`,
`modular_geography.py` and the native conditional sampler. Explicitly distinguish
fitting approximations from forecast dynamics, common future local scales from
component filter scales, and a martingale property from empirical unbiasedness.
This documents the existing implementation; no model/code/data or numerical
results were changed and no fitting runs were needed. The scenario boundary,
evidence and reproduction sections remain unchanged. Markdown math/link and
whitespace checks passed. Research/adoption status is unchanged.
