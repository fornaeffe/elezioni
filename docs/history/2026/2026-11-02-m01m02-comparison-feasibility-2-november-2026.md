## M01/M02 comparison feasibility — 2 November 2026

Assessed Luca's request for the previous comparison's metrics using only the
updated M01/M02 models. The [focused assessment](../../../research/vote-generation/docs/model-comparison-feasibility.md)
records the blocked historical scope, available future operational path and
required evaluation-design decision. No old model was rerun and no predictive
or 1,000-draw browser campaign was started.

Added `scripts/acceptance/model-comparison-feasibility.ts` under `web/`, which
loads and validates the installed release/source and reads the frozen comparison
receipt solely to recover its target/cutoff combinations. It invokes current
preparation guards without fitting, generating votes or running scrutiny. The
[new receipt](../../../test/fixtures/benchmarks/model_comparison_feasibility.json)
pins source/catalog identities, old-receipt hash and probe implementation hashes.

All 12 original target/cutoff combinations fail M01's common-18+ pair selection.
Both supplied pair choices fail M02's paired compilation at each cutoff (24
rejections). Senate 2018 is age-25+ audit information; the only common-18+ pair
is 2022, available from 26 September, after the latest requested cutoff of
10 September 2022. The full 2018/2022 cases use the same blocked short-horizon
cutoffs. A positive control at 19 October 2026 compiles 7,904 matched municipal
rows. No source-loading failure or missing generated bundle caused these results.

Using the 2022 pair retrospectively would leak held-out outcomes. A Camera-only
renewal alone does not avoid M02's current paired-refit compilation; M01's current
baseline contract also explicitly requires the pair. The plan therefore records
the need to agree a Camera-only historical evaluation/preparation design before
implementing a current-model scoring adapter. Original metric definitions and
scoring helpers can be reused without rerunning the old procedures. The old
historical execution script still invokes its old list-based kernel and its
former browser configuration is absent; these cannot be treated as current-model
test entry points.

Verification: the direct feasibility probe and `npm run check:data` passed.
The positive control compiles observations only; it is not new predictive or
future scrutiny acceptance. No model law, default, historical source, production
routing or legal scrutiny changed.
