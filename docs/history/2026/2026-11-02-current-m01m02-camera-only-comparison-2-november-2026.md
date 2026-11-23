## Current M01/M02 Camera-only comparison — 2 November 2026

Completed Luca's requested comparison of the updated models after explicit
approval of a test-only Camera preparation exception. The
[results and reproduction guide](../../../research/vote-generation/docs/current-camera-comparison-results.md) retain
the same historical observable-group scores, conditional electorates/filing,
2013/2018/2022 cutoffs, reduced panels and full short-horizon domains as the
previous comparison. No older model was rerun. Production model contracts,
generated defaults, sources, routing and legal scrutiny remain unchanged.

The isolated `current-camera-models.ts` adapter invokes current M01 conditional
pooling, local completion, original-group poll reconciliation and component-shock
kernels, and current M02 reference fitting, original-offered profiles,
unrestricted raking and bounded mass-pair kernels. Historical M01 uses the latest
eligible Camera baseline without a paired Senate prerequisite; M02 omits Senate
adaptation. Original counts remain integers for M02; M01's explicit original-list
composition fractions remain unrounded. The frozen cutoff-specific research
bases are retained for this experiment; they are not the current 34-component
future default or an invariant marginalization of its nonlinear laws.

Recorded all M01 joint/pooled composition assumptions, existing reviewed weights,
equal-weight fallbacks and mandatory named-poll anchors. The 2022 Impegno Civico
residual zero is an explicit consequence of the already reviewed Centro
Democratico 100% composition, not an inferred zero for missing data. Target vote
counts are excluded from the preparation boundary; only conditional electorate,
region and filed identities enter. Physical-cell electorates are apportioned
before generation, with shared municipal profile/shock streams and final filing
conditioning. Fine-crosswalk failures remain in municipal/evaluated-area scores.

An initial draft adapter exposed a source-contract problem: the old export has
merged national groups without original offered-set strata. Discarded those draft
scores, reconstructed strata from pinned original municipal counts, checked exact
elector/non-valid totals against each frozen aggregate, enabled the corrected
original-offered profile policy, and restarted the historical comparison. Final
models use independent stream namespaces; no forecast parameters or legal rules
were tuned against the results. The full workload was split across separate
processes; the sequential driver was stopped after the two completed 2018 cases
to avoid duplicating the separately running 2022 cases.

Final historical evidence comprises **28 model/case rows**, each with **512
draws**, no generation exceptions, accepted numerical diagnostics and exact
elector conservation. Full domains contain 7,883/7,830 municipalities, with
100/nine unmatched fine crosswalks, 52 colleges and 27 constituencies. The M01
missing-baseline audits retain 94/40 regional fallbacks. All requested distribution
scores, multi-level coverage, widths, mean errors, WIS/CRPS, finite-band near-truth
frequencies, tail frequencies and elector-denominator summaries are retained;
group bias and whole-draw Monte Carlo errors are curated separately.

Full municipal Error30/Coverage90/mean MAE are respectively:

- 2018 M01: **0.78% / 49.48% / 5.91 pp**; M02: **2.36% / 89.17% / 5.50 pp**.
- 2022 M01: **0.44% / 57.95% / 3.80 pp**; M02: **1.28% / 83.75% / 3.95 pp**.

M01's lower tails accompany severe undercoverage; M02 has better coverage/WIS
but is not universally better in mean accuracy. Current M02 has worse full-domain
municipal Error30/MAE than the frozen older selected candidate's point results.
Those old figures were reused without rerunning it; changed preparation and
forecast semantics prevent attributing the difference to one isolated law change.
These fixed, reused cases do not accept general predictive calibration or change
the production-default decision.

The separate Chromium **148.0.7778.96** campaign used the actual current 24-list
default, Camera only, a loaded prefit, production generation/candidatures and
legal scrutiny in **four dedicated workers**. Removed Senate-specific references
from that Camera-only scenario; leaving them in initially caused the correct
non-renewed-chamber preparation rejection before any simulation attempt. Final
campaigns each attempted **1,000** draws: all returned complete results with
**392 filled seats and 392 distinct candidates**, zero Senate seats, exceptions,
non-computable results or unfilled seats. Wall times were **9.77 minutes M01**
and **6.45 minutes M02**. Worker phase sums include all attempts and are not
additive wall phases. These narrower, contended workloads are not an isolated
speed ranking, a cold-refit benchmark, paired acceptance or UI/export acceptance.

Maintained evidence is
`test/fixtures/benchmarks/current_camera_comparison.json` and
`current_camera_details.json.gz`; raw reports/inputs/scores remain under
`data/derived/current-camera-comparison/`. Curated details retain group bias,
college/constituency/area scores and joint tail draws rather than all regenerable
municipal rows. Code, source, old-receipt and raw-report hashes bind the results.

Verification passed: **seven focused adapter checks**, `npm run check:data`,
the complete case/source/diagnostic curation gates, and original-count/unique-raw-
identity audits across all six training elections. Final artifact/link checks
and `git diff --check` passed. Old benchmark/model files are unchanged; no
publication, deployment or commit was performed.
