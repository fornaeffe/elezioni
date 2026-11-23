## 2026-10-30 — Filing-aware replacement models and requested future default

Luca authorized implementing and comparing replacement forecast laws, retaining
only the municipality/college plausibility, actual-result support and mean
accuracy priorities, membership architecture, Italian context, history/poll
inputs and a complete browser computation below 30 minutes. Earlier Beta and
hard national-margin properties remain historical comparator contracts. This
continues FV-08 research; it does not activate the ordinary reform default.

Before predictive comparisons, authored and prepared Luca's 24-list default in
both chambers. Ora!/DSP use the requested 14 Camera constituencies and, following
Luca's confirmation, the nine corresponding entire Senate regions. All other
ordinary/TAA/VdA filings, coalitions and five minority declarations follow the
scenario request. Historical counts remain grouped and unchanged. The fit has
28 observed-supported coordinates; 34 components are available in the future
scenario, including historical-rest, which is not filed by default. Six future
components use explicit reference support. New-party, provincial, cross-chamber
and whole-alliance VdA proxies are labelled assumptions, not invented observed
party splits. Exact identities and primary-source links are retained beside the
authored scenario. See the [results and default assumptions](../../../research/vote-generation/docs/vote-generation-feasible-results.md).

Implemented three browser-safe forecast fields: filing-stratum Beta, uniform
bounded pair transfers and mass-weighted bounded pair transfers. Feasible local
probabilities generate national totals, avoiding unsupported national future
margin requests. Symmetric bounded transfers conserve mass, retain structural
zeros and preserve conditional municipal means. A separate regional reference
profile term reallocates existing variance without increasing municipal marginal
variance. Existing reference interventions, fixed turnout for list edits,
physical reconstruction, count generation and legal scrutiny remain in use.
National/local list uncertainty controls also reach the new kernels. Model,
scale and regional covariance are serialized and included in generation keys.
Equations and qualifications are in the [model guide](../../../research/vote-generation/docs/vote-generation-feasible-model.md).

Ran 134 candidate/case comparisons, each with 512 predictive draws, using cutoff
eligible historical elections and polls: Camera 2013/2018 development, reused
Camera 2022 validation, 730/365/90/15-day horizons, and full retained 2018/2022
short-horizon municipal domains. They are variants of three elections, not 134
independent elections or a fresh holdout. Original grouped observations define
observable truth; current-college reconstructions use labelled population splits.
The full retained domains have 7,883/7,830 municipalities and 52 current Camera
colleges; VdA and nine unmatched 2022 municipalities limit geographic validation.
No Senate predictive calibration is claimed from the functional paired tests.

Sequential selection chose mass-weighted pairs, scale 1.5 and regional profile
variance share 0.25. Protocols and selections are tracked in `data/filing-aware-*`.
Country-calibrated references worsened development accuracy/interval scores and
were rejected. Uniform matching had insufficient major-party variability when
many tiny components were present. Regional profile covariance improved college
coverage without adding municipal marginal variance. The selected model is a
compromise: full 2022 municipal tail-error probability falls from 1.384% to
1.114%, while college 90% coverage is 80.11% versus global Beta's 79.78% and
69.90% for mass-weighted pairs without the regional reference term. Full 2018
college coverage is 84.29%; its small college-tail difference is unresolved by
simulation error. Neither nominal coverage nor absence of systematic bias is
established; named-group and long-horizon errors remain explicit.

The actual single-worker Chromium 148.0.7778.96 computation on the i7-14700KF
completed **512/512 paired Camera/Senate generations and legal scrutinies in
1,583.0 seconds (26.38 minutes)**, including loading and a real national/profile/
paired refit after changing the reference date to 20 October. Every request filled
all 588 modelled domestic seats; technical and non-computable failures were zero.
Refitting took 6.75 seconds, generation 1,413.28 seconds and scrutiny 160.34 seconds.
The existing memory policy permits one consumer for the larger packet and rejects
an unsafe second reservation. This is the measured default draw budget, not the
maximum edited envelope or full-campaign storage/export acceptance.

Curated comparison, original report summaries, group errors, joint-draw tail-rate
MCSE/paired intervals and the complete browser ledger are under
`test/fixtures/benchmarks/vote_share_feasible_*`. Raw country predictive arrays
stay ignored and stream through gzip below V8's single-string limit. An initial
2022 output attempt was interrupted before reporting; its identified orphan was
stopped and the completed repeat retained identical inputs, parameters and seeds.
Pooled-group summaries were corrected from identical saved draws to predeclared
equal-group-within-unit/equal-unit weighting, without a statistical rerun. The
curator checks the reconstructed joint-draw rates against report values.

Repeated profile lookups and temporary shock vectors were removed from the hot
path. Independent zero-scale, conservation, exact-support, deterministic-stream,
moment, component-granularity and covariance fixtures pass. The uniform-pair
64-draw hash remains unchanged through optimization, and resolved profile caching
retains identical full-default draws in the fixed three-draw comparison. Default
integration tests caught malformed UTF-8 labels and an expected-ID sort error;
these were corrected before the full browser run. Browser harness action/HTTP
transport mistakes were corrected explicitly. Older ten-share/two-consumer test
fixtures were adapted to the expanded basis and actual memory policy; no runtime
validation or memory limit was weakened. A five-second unit timeout under heavy
parallel validation passed both its targeted repeat and the full bounded-worker
suite with the unchanged timeout.

Verification completed so far: **105 unit files / 1,232 tests**, data and browser
harness TypeScript checks, zero Svelte diagnostics, static candidate release
verification, **7 Chromium release/refit/worker checks**, and **8 root-path
Chromium/Firefox workspace checks**. Required assets are present. The FV-01
preservation check still verifies all **61 evidence files / 1,433 inventory
entries**. No research source or artifact was moved or deleted. FV-09 inventory
now includes this comparison while preserving the selected runtime modules.
The corrected export/replay checks pass at both `/` and `/elezioni`: **16
Chromium/Firefox functional workspace checks** in total. The nested check
exposed fractional `estimatedWorkerMemoryMiB` metadata, rejected by the existing
result importer. The producer now rounds its reported MiB estimate up, without
changing reservation budgets or forecast code. Tests now parse the actual export
and reject result-service errors, preventing stale stored results from masking
a failed import. An initial nested memory assertion measured 589.64 MiB and
failed; that engineering result is retained. Corrected samples measured 420.85
MiB root and 444.69 MiB project-base. The authorized comparison records the older
512 MiB threshold diagnostically; default FV-08 test mode still enforces it.
These samples are not an unsampled RSS bound or a full-campaign resource gate.
The complete 1,232-test suite passed again after the metadata correction.
Root/project-base releases retain the same catalog/generation fingerprint, and
the standard root build was restored with fast dispatch opt-in. FV-08's wider
resource/export/public-default gates remain open. No commit or deployment was
performed.
