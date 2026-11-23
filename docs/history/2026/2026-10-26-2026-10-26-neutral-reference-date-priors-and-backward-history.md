## 2026-10-26 — Neutral reference-date priors and backward history

Updated `docs/vote-generation-drift-model.md` with Luca's approved prior policy.
Political contrasts have exchangeable, zero-centered proper priors. Calibrate
the non-valid score offset to expected elector share 0.5 in the declared
reference context, including municipal aggregation and covariance uncertainty;
this is not a hard participation constraint. Use finite, weakly informative
scales, prior predictive checks and sensitivity analysis. Informative evidence
can override the prior, while unidentified contrasts remain prior-sensitive.

Anchor national/local states at the scenario reference date, construct history
with backward Gaussian increments, and forecast forward from that date.
Birth dates bound historical support rather than supplying another independent
initial prior. The specification distinguishes backward construction from the
earlier forward-process independence assumptions and requires normalized
transitions so extra unobserved history does not change retained marginals.

Moving the reference date resets the neutral prior and requires a new fit even
if the available observations do not change. Default precomputed fits carry a
fixed reference date; cache fingerprints and visible invalidation must include
that date and the prior calibration policy. Earlier posteriors may initialize
computation, but do not become evidence or the next prior. Updated the active
plan and stage-0 validation requirements; implementation remains pending.

Verification: standard-library numerical integration checks the illustrated
Gaussian offset calibration, exchangeable expected shares and gauge invariance;
Gaussian covariance checks confirm empty-window extension invariance and the
distinction between resetting and propagating a prior. Used standard-library
checks because local Python has no NumPy. Checked Markdown links, math
delimiters, UTF-8 and diff whitespace. Preserved the pre-existing edit removing
the obsolete exit-projection sentence. No runtime code, fitted parameters or
historical research results changed; no backtest or browser fitting was run.
