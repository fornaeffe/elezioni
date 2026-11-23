## 2026-10-26 — Restricted components, offline Stan and user-requested pause

Added Luca's admissible-component restriction to the model and research-v2
contract: collective distinguishing information must be available at each
cutoff; neutral priors alone do not authorize a requested split. Implemented
share-scale soft-reference calibration and exact aggregate hard conditioning.
All eight restricted reference samplers pass their diagnostics. Laplace passes
seven fixtures but misses the broad soft-reference within-list mean by 7.75 pp
(largest elector component error 4.35 pp). Exact-density Student-t importance
correction passes the one-city case but fails ESS/weight gates at eight cities.
The authorized final offline Stan fallback passes reduced soft/hard references;
its 30-minute gate is waived. Detailed measurements and reproduction are in
`docs/vote-generation-party-drift-restricted.md`.

Prepared the source-backed provisional membership proposal for Luca's later
review: 189 source-label rows, 107 statistical identities, original counts
conserved. Restored full answer lists from two already cached 2024 survey PDFs
only in a research sidecar, without duplicate surveys or production-source
changes. The full-list cutoff audit leaves 23/24 cases unresolved. Luca chose
admissible research panels; implemented conservative cutoff-specific pooling,
original-basis Other complements and source-preserving outcome coarsening. The
first 24-panel structural preflight passes with 5–28 components. Final geography
preparation also retains future-only municipalities as missing-history units.

Implemented a conditional-scale real-data Stan pilot with original grouped
counts, backward Gaussian priors, common volatility inference and literal polls.
Independent NumPy density/gradient checks pass. A 47-parameter real case completes
in 1.13 seconds with R-hat 1.0045, minimum ESS 1,266 and no divergence/depth hits.
Its rough city-linear full-size estimate is 75.8 minutes; this is not an estimate
or convergence result for the larger later models. Corrected the initial metric's
array serialization after two explicitly unusable interrupted engineering runs.

**Luca requested pausing on 26 October 2026 at 16:10 Europe/Rome.** Stopped all
four verified active chain processes and preserved the checkpoint under
`data/derived/party-drift/stan-real-673eff932932/paused-attempt.json`. That larger
16-municipality/4,762-parameter pilot reached warmup iteration 100 in all chains;
its 349-second interrupted run supplies no accepted posterior or convergence
failure. No countrywide fitting, stages 1–7, production changes, commit or
publication occurred. Resume with fresh warmup from the saved prepared inputs.
Focused verification reached 23 passing research tests and a passing preparation
type check; the full suite was not rerun after the follow-up's final edits.
