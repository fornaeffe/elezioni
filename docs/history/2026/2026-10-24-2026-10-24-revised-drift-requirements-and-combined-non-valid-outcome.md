## 2026-10-24 — Revised drift requirements and combined non-valid outcome

Luca did not select the previous endpoint models and proposed national/local
Gaussian drift in multinomial log scores. After review, he accepts a count
observation layer for observed zeros; corrected identification, initial-state,
time and innovation conventions; the staged comparison hierarchy; and four
polling requirements: separate sample-dependent uncertainty, shared polling
error, fieldwork measurement versus publication availability, and polling noise
confined to its observation likelihood. Both regional innovations and local
mean reversion are retained as planned stage-7 comparisons, not optional
alternatives to be dropped without testing.

Added docs/vote-generation-drift-model.md as the focused current specification.
Replaced the raw draft in the historical methods guide with a direct pointer,
marked the completed comparison's recommendation as historical/not selected,
and updated the active plan without changing the old measured results.

The working electorate-wide proposal adds outcome 0 for electors without a
valid attributed political vote: observed count equals electors minus actual
valid votes after source reconciliation. It uses a joint multinomial over
political outcomes plus outcome 0. This can represent combined abstention and
blank/invalid voting without separate processes; future electorate size still
requires an input/forecast. It cannot identify abstention and ballot validity
separately, and neither survey undecided/nonresponse nor unmapped political
votes become outcome 0.

Documented both pending polling links conditional on a valid political vote.
The aggregate link divides summed elector-weighted list probabilities by
summed elector-weighted valid probabilities, equivalently weighting municipal
conditional preferences by expected valid votes. The non-valid score cancels
from a municipality's conditional party shares; party-only polls do not
directly identify the total non-valid fraction, though heterogeneous valid
weights and state correlations allow indirect information. Outcome 0 remains
in volatility/chamber comparisons while staying outside political lists and
legal seat allocation. Global-softmax coupling of participation and political
scores is a model assumption to inspect, not proof of political independence.

The joint drift/count proposal does not impose an independently sampled national
margin. National summaries must agree with municipal counts, rather than be
IPF-forced to softmax of the national factor. Sparse Laplace fitting is the first
scalable candidate for the accepted non-Gaussian count layer; exact Kalman
methods apply only to a Gaussian log-ratio approximation with linear observations.

Verification in this documentation-only step: mathematical conditioning and
multinomial accounting identities checked; relative-document links and diff
whitespace checked. No new polls were downloaded, parameters fitted, backtests
run, production code/data changed, commits created or model activation performed.
The combined-outcome formulation is documented for this abstention review;
future research execution remains a subsequent task.
