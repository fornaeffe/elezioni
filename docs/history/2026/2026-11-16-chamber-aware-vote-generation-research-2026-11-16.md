## Chamber-aware vote-generation research (2026-11-16)

Reopened `research/vote-generation/` with a separate current-code campaign,
task-oriented index, protocol, defining equations, generated comparison tables
and a curated source/code/hash receipt. The archived original-layout scripts,
FV-01/FV-09 evidence, production generators/defaults and protected model drafts
remain unchanged. Research M05/M06/M07 identify Joint Gaussian, covariance-matched
Joint Student-t and spatial transition bootstrap, adapted to six whole-ticket
families without fabricating historical within-ticket counts.

Completed 40 cases/11,776 draws: Camera 2018/2022, 15/365-day mixed-history folds,
Camera-only sensitivities, paired 2022 and explicit in-sample reconstruction
ablations. College/constituency/national scores include Error30, Coverage90, MAE,
WIS and signed chamber-difference scores on identical reconstructed footprints.
The 2018 Senate contrast sensitivity is labelled age-confounded; common-18+
eligibility is waived only in research preparation. Reconstruction uses 2022
contrasts and is not a held-out predictive success. All generated rows conserve
electors with valid nonnegative integer counts.

Actual released posterior diagnostics establish that correlated MCMC row indices
do not correlate political posterior values as intended. Reordering whole rows
changes that dependence without changing the empirical posterior. In the
2022-contrast reconstruction, national difference RMS ratios fall from 62.09
(current M02) to 18.75 (row synchronization), 1.59 (full coupling), and 1.00
(one common M02 field). The latter preserves Camera's seeded law and improves
reconstruction coverage, but a fixed 2018 contrast undercovers 2022 differences.
M01 leads the primary marginal Error30/near-nominal-coverage comparison;
M05/M06 improve territorial MAE/WIS with more extreme errors. No replacement was
adopted. Full-component/filing/reference and browser acceptance remain directive
work in the plan; a changed common-field probability law requires MAJOR versioning.

Six 512-draw paired resource cases ran sequentially in isolated Node processes;
remaining accuracy jobs used bounded parallelism, with their timings excluded
from speed ranking. Generation measured 207/837/734/609/616/611 ms per draw for
M01/M02/shared-M02/M05/M06/M07, respectively, with 513–609 MiB peak whole-process
RSS on the six-family panel. These exclude browser/worker/scrutiny acceptance.
Resume checks retained completed cases only under identical transitive code/
lockfile identities. Standalone ESM and explicit Node type roots resolved the
research directory's package/type boundary; probability apportionment wrappers
were corrected to supply the active helper's required integer weights before
accepted runs.

Verification from `web/`: the campaign `tsc` check and native test runner passed
5/5 tests, including active-M02 integer parity at draws 1/2/7, Camera
marginal preservation, deterministic endpoints and cutoff leakage checks;
`npm run check:data` passed; the research-import-boundary Vitest passed 1/1;
`npm run test:research:archive` verified 212 archive files and 61 retained evidence
files. Receipt curation also verifies exact 512-draw Camera score parity and a
single source/code identity across all cases. See the
[findings and acceptance boundary](../../../research/vote-generation/campaign-2026-11/results.md).
