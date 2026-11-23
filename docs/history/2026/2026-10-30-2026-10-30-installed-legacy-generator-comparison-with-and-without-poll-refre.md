## 2026-10-30 — Installed legacy generator comparison with and without poll refresh

Completed the authorized comparison against the actual installed R-derived
`paired-vote-generation.ts` engine. It is distinct from the earlier fast-model
comparator named `legacy-global-beta-v1`. The two procedures preserve production
transition scales, seeded streams, integer reconstruction and legal scrutiny.
Both use explicit eligible poll points for otherwise unsupported new lists;
the no-refresh procedure omits general polling updates. The poll procedure uses
simple latest-publication points through the existing poll-reference interface,
not the separate production multi-house polling estimator. All assumptions,
mixed-wave normalization, source dates and reference-clock limits are recorded
in the [results](../../../research/vote-generation/docs/vote-generation-legacy-comparison-results.md) and
[execution guide](../../../research/vote-generation/docs/vote-generation-legacy-comparison-plan.md).

Historical Camera 2013/2018/2022 use four source cutoffs and 512 draws, with the
same reduced truth/observable groups and full 2018/2022 short-horizon domains as
the selected candidate. There are 28 case/variant comparisons and 25 distinct
effective input sets: the three later no-refresh 2018 cutoffs reuse the first
forecast. Every historical draw conserves electors exactly. Snapshots use
original preceding Camera list counts and a labelled regional fallback for
missing/invalid municipality history. The candidate's original compact-source
geography supplies the same scoring crosswalk; its fitted parameters/profiles
are not copied. The full municipal domains contain 7,883/7,830 observations,
with 100/nine omitted only from area scores because no unique current-college
crosswalk is available, and 94/40 regional historical fallbacks. College and
constituency scores cover 52/27 units. Joint truth is never internally split.

Reviewed deterministic matrix assumptions include Azione/IV 56/44,
Noi Moderati/UDC 75/25 and LeU 40/50/10. Unrecognized original lists remain
unmapped, hence non-valid support. Poll availability/publication gates exclude
post-cutoff information. A source review rejected reading SWG's joint
LibDem/Ora answer as separately observed shares; Ora uses the separate eligible
8 October Youtrend point instead. New named-ticket audits link exact TEAM K
identity and split the three explicitly named Senate TAA joint candidate groups.
An explicit national-poll proxy supplies positive FN support in TAA, which the
ordinary poll refresh excludes. Special-region positivity is checked before
benchmarking. VdA whole-alliance references remain labelled scenario proxies.

Technical pilots exposed two replay problems, corrected without changing
production code: historical TAA filing containers had incorrectly entered the
ordinary poll domain, and incomplete future regional reference vectors left
no support for a free remainder. The first correction leaves the no-poll law
unchanged, verified independently, so its completed forecasts were reused.
Future input audits also caught opaque named joint tickets that required manual
recognized-party splits. Preliminary browser pilots were stopped and restarted
after those input corrections; their partial timings are not the final evidence.
Historical accuracy inputs/weights were not tuned to held-out results.

The selected candidate improves full 2018 tails, mean errors and distribution
scores. Full 2022 is mixed: the poll-informed legacy procedure has lower
municipal mean error (3.23 versus 3.35 pp) and higher college coverage/lower
college mean error, while the selected candidate has fewer college errors above
30 pp. Whole-draw Monte Carlo intervals and per-group signed biases are retained;
these reused elections do not establish future calibration or isolate transition
laws from differences in information/reference construction.

Final Chromium 148.0.7778.96 runs attempted 1,000 paired simulations per
procedure. No-refresh took 19.49 minutes: 998 complete 588-seat results and two
exceptions. Poll-refresh took 21.30 minutes: 981 complete results, 18 exceptions
and one returned non-computable 399-seat result. Both measured workloads pass
the current 60-minute time budget, but both browser reliability gates fail.
The exception reproduces in Node (including poll-informed draw 23 and
no-refresh draw 131): recovery eligibility omits a nationally unadmitted TAA
coalition member already awarded a proportional seat. An isolated diagnostic
build changed only the error message to inspect the rejected row; production
sources/benchmark bundles were unchanged. Poll-informed draw 135 separately
returns Senate Sicilia `N21/impossible-residual`. Added both investigations to
the plan; no legal rule or fallback was changed to force completion.

Verification: 33 focused adapter/existing production-generation tests pass;
`npm run check:data` and formatting checks pass. Both complete browser commands
return a failing gate and retain all attempted IDs/results. Compact evidence is
in `test/fixtures/benchmarks/vote_share_legacy_comparison.json` and
`vote_share_legacy_details.json.gz`; full regenerable inputs/scores remain in
`data/derived/legacy-comparison/`. Extended the later FV-09 archive inventory,
without executing moves/deletions. The benchmark is excluded from ordinary E2E
discovery and has its own explicit configuration/budget. Production defaults,
generators, scrutiny, public dispatch and deployment are unchanged.
