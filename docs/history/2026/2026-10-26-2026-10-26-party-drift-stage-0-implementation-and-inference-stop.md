## 2026-10-26 — Party drift stage-0 implementation and inference stop

Implemented a separate offline research path in
`web/scripts/data-preparation/party-drift-*.ts`: versioned count/membership
validation and semantic fingerprints; exact original grouped multinomial
likelihood/gradient/observed Hessian; availability, Other and expected-valid-vote
aggregate operators; conditional-scale neutral reference-prior calibration and
backward/forward Gaussian construction; multiple-start reduced Laplace fitting,
exact-likelihood elliptical slice sampling, chain diagnostics and independent
aggregate/split-coordinate quadrature. Added focused unit tests and
`data:forecast:party:validate` / `data:forecast:party:profile` commands. No source
counts are split or converted to inferred component observations.

Stopped at the requested preliminary gate. The separate-component posterior
passes, but five inseparable-list fixtures fail the prespecified Laplace
accuracy limits. A balanced 40% joint list has exact posterior mean 39.98%
versus Laplace 52.38%; across fixtures the largest component mean discrepancy
is 11.53 percentage points and largest 90% width ratio 2.47. All 54 starts reach
gradient tolerance; 53 have positive-definite curvature and one symmetric
saddle is excluded. Reference maximum split R-hat is 1.0073 and minimum ESS 684.
Independent fine/coarse quadrature agrees to below 3e-13 in means and with
sampling to 0.075 percentage points. This isolates a curved grouped-posterior
approximation problem rather than a routine optimizer failure.

The reduced validation takes 7.29 seconds on the i7-14700KF/64 GiB machine.
Original-data census and 256/1,024/4,096-row kernel probes cover 63,426 usable
municipality/election observations. Extrapolated countrywide count-only
likelihood scans take 0.081 seconds. Direct reference-sampler expansion projects
to 2.7 hours serial even with one proposal per iteration, or 14–18 hours using
observed reduced proposal counts, before latent-state/poll/hyperparameter costs.
This exceeds the 30-minute threshold; no countrywide fit was launched. Faster
sparse inference has no accepted total-time estimate while its accuracy is open.

Recommend retaining the probabilistic model while investigating explicit
non-Gaussian integration of weak split directions, with sparse approximations
only in validated directions. Did not tighten priors, collapse requested
identities or select an alternative engine automatically. The decision, exact
measurements, artifact paths, commands and remaining scope are in
`docs/vote-generation-party-drift-stage0.md`; the active plan now requires
resolving this blocker before full-data fitting or stages 1–7. Complete scenario
and browser contracts, joint scale fitting, hard/soft constraint posterior
conditioning, grouped poll/temporal posterior checks and prior sensitivity
remain open. Earlier fixed-outcome results are unchanged.

Verification: `npm run check:data` passes; all 1,018 tests in 77 files pass,
including 16 new research tests. The separate research validation command saves
its evidence and exits nonzero for the measured accuracy failure; a passing
unit suite does not approve the approximation. No UI/browser acceptance was
run for these offline-only scripts. Production generator, defaults and source
data are unchanged; stages 1–7, full-data fitting, commit and publication were
not performed.
