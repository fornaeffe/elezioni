# Feasible municipal vote fields

This document describes the replacement candidates authorized on 30 October
2026. Use [the research guide](vote-generation-feasible-research.md) for the
comparison and its measured limits. Estimation and original grouped counts still
follow [the fitted model contract](vote-generation-fast-model.md). The former
global Beta generator remains an explicit comparator.

## Reference distribution

For a posterior draw, add the fitted municipal log profile and its Gaussian error
to the log national component probabilities. Normalize over components actually
filed in that municipality, including the separate non-valid category. Unfiled
components have exactly zero probability. National totals are weighted sums of
these feasible local probabilities; they are not an additional imposed margin.
This changes the meaning of a fitted national coordinate: conditional local
normalization can change its aggregate share. The rejected country-calibrated
reference experiment measures that tradeoff.

The regional profile variant uses

$$z_{ij}=\sqrt{\rho}\,Z_{r(i),j}+\sqrt{1-\rho}\,Z_{i,j}.$$

All terms are standard Normal before multiplying by the existing profile SD.
The municipal marginal variance stays unchanged. The regional term prevents
shared geographic error from disappearing when municipalities are aggregated.
The covariance share is an explicit validated modelling assumption, not a fitted
correlation estimate. Municipality noise is shared across physical splits of the
same municipality. The existing Camera/Senate latent Gaussian coupling applies
to both error terms; it is not a claim about vote-count correlation.

Scenario reference-share edits operate afterwards, with more specific geographic
scopes overriding broader scopes. Political references preserve turnout; turnout
has its own inputs. References for unobserved parties and borrowed regional
profiles remain assumptions, not additional election or poll observations.

## Bounded pair transfers

At each step of at most a quarter-year, pair the offered political components.
For a pair with current elector probabilities $p_j,p_k$, transfer

$$\delta=\frac{p_jp_k}{p_j+p_k}
\tanh\!\left(\frac{s_j-s_k}{\sqrt{2}}\right),
\qquad(p'_j,p'_k)=(p_j+\delta,p_k-\delta).$$

The transfer conserves mass and cannot cross zero. Conditional on the current
field and the selected matching, the Gaussian shock difference is symmetric, so
the expected transfer is zero. The mass-weighted variant chooses a random order
using independent exponential priorities divided by current component mass,
within each filing pattern, then pairs neighbours. This makes major components
more likely to be paired together. Matching uses the previous state and is
independent of the new shock, preserving the zero-mean argument. The uniform
matching variant is retained for comparison; numerous tiny components can
otherwise suppress major-party movement.

Future shocks combine common national component noise with regional and municipal
geographic noise. The geographic innovation budget uses 15% regional and 85%
municipal variance. The scale multiplier affects both parts. A separate
election-discrepancy phase adds common political uncertainty with unit exposure;
it adds no turnout shock. Historical scale estimates are reused, but their old
Beta interpretation is not asserted for this different transition law.

Turnout/non-valid movement uses a separate bounded transfer
$a'=a+a(1-a)\tanh(s_0)$ and rescales all political elector probabilities together.
Its shock is independent of the political shocks. Each municipality's political
valid-vote proportions and elector probabilities retain their conditional means.
Aggregated valid-vote ratios can still change their means when random turnout
changes the relative weights of municipalities; empirical college mean errors
therefore remain part of the comparison.
Multinomial sampling then produces integer municipal counts. Physical population
splits, final list attribution and legal scrutiny retain their existing contracts.

## Alternatives and limits

- Filing-stratum Beta evolves aggregates separately within identical choice sets,
  then projects local probabilities to feasible stratum margins. It preserves
  more of the earlier transition but has heavier tails and may change municipal
  means through projection. It remains a tested alternative.
- A log-ratio Gaussian field would also stay on the simplex, but its nonlinear
  normalization can change means and requires a separate calibration study. It
  is not implemented as an additional accepted candidate.
- Bounded transfers limit each step, not the cumulative change over a long
  horizon. They do not guarantee realistic future political developments.
- A zero-mean future transition does not establish an unbiased reference fit.
  Grouped observations do not identify arbitrary internal party splits. Known
  future filing and fixed historical electorates are conditional assumptions in
  the retrospective tests, not forecasts of filing or demographic change.
- Regional covariance is a simple geographic model. It does not estimate
  within-region distance, province/college residual structure, national polling
  bias, party splits/mergers or novel-party support. Report college coverage,
  group-specific mean errors and long-horizon failures alongside national scores.
- Empirical coverage across a few reused elections is descriptive. It cannot
  certify a universal nominal coverage rate or an unseen-election holdout result.
