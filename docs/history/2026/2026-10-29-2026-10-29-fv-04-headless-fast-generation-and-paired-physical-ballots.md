## 2026-10-29 — FV-04: headless fast generation and paired physical ballots

Implemented the [generation boundary](../../vote-generation-fast-generation.md) for the
2026 reform and completed FV-04. It consumes accepted C/E reference chains,
municipal profiles, explicit future component/list compositions, physical filing
and dated conditional electorates. It applies finite-step Beta movement, the
selected scale mixtures, reference-profile uncertainty, regional/local noise,
the conservative 1.5 election discrepancy, global projection and integer
multinomial counts. The original FV-02 numerical kernels and frozen research
extraction are unchanged; `index.ts` only gains additive public exports.

Political interventions hold each cell's turnout fixed; separate non-valid
references use elector denominators. Implemented joint uncertain partial/full
references, deterministic full-centre normalization with before/after trace,
common/explicit chamber precedence and national-to-municipal shadowing. Effective
local probabilities determine the country reference before future movement.
List identity remains distinct when the same component files under different
lists in disjoint territories. New support requires explicit references and
labelled profile/local/national uncertainty; Other retains its fixed legal
identity. Missing/unfiled support is not filled with epsilon.

The logistic-normal calibrator uses positive Laplace factorization and verifies
moments/refinement against independent direct normal tensor integration on reduced
fixtures. An initial coarse outer quadrature missed the frozen tolerance; raised
its integration order rather than relaxing acceptance. Closed families compare
deterministic starting solutions by the contract objectives. Numerical v1 exposes
a resource bound of 16 simultaneous uncertain categories; arbitrary moment
combinations are not promised representable. Impossible/overlapping targets fail.

Physical electorates are apportioned before sampling, preserving municipal
integers. Transportation feasibility detects joint filing capacities; residual
cycles identify saturated support faces before globally raking. All passes use
the same country target, tolerance `1e-9` and 120-update limit. No target repair
or reroll occurs. Camera TAA is one set of physical special ballots with one
derived proportional view; VdA/TAA remain in both chamber boundaries.

Added `compilePairedHistory` and `paired-18plus-logratio-adapter-v1`. The adapter
uses original same-date Camera/Senate 2022 counts with equal common-18+
denominators, reviewed whole-group correspondences, clipped national/local
contrasts and explicit shrinkage/fallback assumptions. Every current target is
eligible: 7,904 included, no paired denominator exclusions. Senate 2018 25+ is
rejected from the adapter. Senate draws push forward Camera coordinates; European
coordinates are never relabelled. Latent Gaussian-copula correlations
0.50/0.85/0.98 share one existing marginal innovation budget, without adding old
paired shocks. These remain assumptions, not measured vote/seat correlations or
an empirically calibrated direct Senate fit.

Extracted shared final-count assembly from the installed generator and narrowed
its candidate/bonus contracts. New central physical reference support drives
bonus candidature planning. The existing people, special/ordinary candidate,
supplied-count, preference-budget, Other and legal-tie behavior remains shared.
Early strict-input checks caught statistical list fields crossing the legal
boundary; the adapter now passes only legal list fields. Byte comparison against
the previous `HEAD` implementation passed 12 joint/single and bonus-policy cases,
including complete generated inputs and traces. The temporary comparison module
was removed. Legal scrutiny algorithms were not edited.

Verification and measurements are pinned in
[the benchmark snapshot](../../../test/fixtures/benchmarks/vote_share_generation.json),
including LF-normalized source hashes and environment. Regenerable full reports
stay under ignored `data/derived/fv04-generation/`:

- The frozen 120,000 two-year transition draws have maximum mean error
  **0.00008578**, within four MCSE, and zero >80% events in this fixture.
- Four partial/closed uncertain-reference fixtures pass independent integration
  and 50,000 draws for each of `fv04-reference-0/1/2`; requested means/SDs agree
  within four MCSE. Maximum calibration moment error is about `3.7e-11`; maximum
  refinement difference about `1.01e-10`. Direct integration also passes the
  separate frozen `1e-6` / `1e-7` limits.
- Copula/Beta marginal mean and variance checks pass for all dependence profiles,
  including adaptive Gamma rejection. The 32 synthetic legal runs per profile
  retain all requested IDs: 15/13/20 non-computable runs out of 32 for weaker/
  central/stronger dependence. Bonus and seat summaries explicitly distinguish
  computable runs. Tiny synthetic electorates can trigger statutory truncated-
  quota `initial-overallocation`; that outcome is tested and preserved. This is
  a mechanics/sensitivity screen, not production predictive acceptance.
- Before full processing, the source-backed 128-target profile pilot took
  **15.7 ms**, projecting about 0.97 seconds for 7,904 targets. The explicit
  two-observable-pool/no-poll country check then fit in **111 ms**, prepared full
  profiles in **692 ms**, prepared the paired generator in **248 ms** and generated
  one pair in **650 ms**. All **15,810 physical cells** conserve **46,120,143**
  conditional electors per chamber; legal scrutiny has no non-computable stage.
  Global projection residual is `2.16e-10` within seven updates. The actual
  reference/election dates are 2026-10-19 / 2027-09-25; electorates are explicitly
  held at the 2022 vintage. This test basis is not a proposed political default.
- Initial country generation took about 7.6 seconds because projection passes
  rebuilt identical random scores. Flat per-chamber Float64 score caching and
  lazy private-stream creation reduced it to 0.65 seconds without changing
  complete inputs/traces/audits in four byte-comparison cases. Scores are built
  in bounded chunks, retained for one chamber/pass and included in workspace
  estimates; no simulations × geography × components tensor is allocated.
- Node process lifetime peak RSS for the full source/fit/generation/legal check
  was **434,068 KiB** (about 424 MiB). This includes original history/compilation,
  temporary objects and legal results, not just numerical buffers. It is not a
  browser memory or concurrent-worker acceptance claim.
- The final full unit suite passes **87 files / 1,162 tests**. `check` reports
  zero errors/warnings and `check:data` passes. Chromium **148.0.7778.96** test
  workers at `/` and `/elezioni` produce byte-identical Node legal inputs across
  chunk sizes and simulation ordering, and matching calibrated parameters.
  Both static builds pass; the root build was restored. Existing bundle-size
  and plugin-timing advisories remain. FV-01 verifies 1,433 original inventory
  entries and 61 retained evidence files; documentation links and whitespace
  were checked.

Installed parsers/defaults/catalogs/dispatch/editor remain unchanged. FV-05 must
select an admissible explicit default basis and package verified state; FV-06/07
own browser estimation/revisions/editor integration. FV-08 still owns default
predictive, 1,000-run production seat, full input-envelope, retention and combined
worker-memory acceptance. A feasible final reference does not repair an earlier
infeasible baseline projection. No predictive coverage/latency claim for arbitrary
128-component states follows from this reduced-basis boundary check. FV-09
archive/deletion work, public deployment and commits were not performed.
