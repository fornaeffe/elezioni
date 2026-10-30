# Filing-aware vote forecast comparison

## Active scope

On 30 October 2026 Luca authorized implementing and testing replacement models.
The fixed priorities are: plausible tails, sufficient probability for actual
results, then accurate and preferably unbiased means, at municipality and college
as well as national level. Keep scenario-owned historical/future memberships,
past election and poll inputs, actual Italian filing, and a complete browser
computation below 30 minutes on the i7-14700KF machine. Earlier Beta-martingale,
hard national-margin and 30-second targets are comparison properties, not immutable
requirements for this research. Original FV-01/FV-08 evidence stays historical.
Public deployment and research archival remain separate migration gates.

## Current implementation

The requested 24-list candidate default is prepared. The selected research
candidate is `bounded-mass-pair-field-v2`, innovation scale 1.5, reference-profile
regional variance share 0.25. This is a tested compromise rather than dominance
on every score. Use [the equations and limits](vote-generation-feasible-model.md)
for kernel work and [the results](vote-generation-feasible-results.md) for the
comparison, default assumptions and measured browser budget. FV-08 default
activation remains separate.

## Execution order

1. Replace the candidate default choice set using
   `data/fast-share-future-scenario.json`, before predictive comparisons. Preserve
   counts as original groups; link recognized identities, and label unsupported
   new-party/internal splits explicitly. Luca confirmed Senate filing throughout
   the corresponding whole regions for restricted Camera constituencies.
2. Implement browser-safe coherent forecast fields: conditional territorial
   evolution and feasible bounded transfers, with the earlier global Beta model
   as a labelled comparator. Consider a common-factor log-ratio field as the
   practical alternative when its geographic/national bias can be assessed.
3. Run independent reduced numerical/conservation/moment fixtures and a reduced
   browser timing gate before full-country computation. Do not use a fitted mode
   as proof of posterior uncertainty or silently split grouped observations.
4. Freeze candidates/metrics/cutoffs/seeds before predictive inspection. Replay
   earlier election/poll vintages into subsequent political elections; compare
   observable list groups only, with both municipality and legal-college scores.
   Keep earlier research panels separately labelled; they are reused evidence.
5. Use development cases to select, then assess later elections without tuning
   on their results. Report tails first, coverage/width/proper interval scores
   second, mean/bias errors third, with sparse/new-party and long-horizon results.
6. Exercise the complete new future choice set, both chambers, structural zeros,
   regional minorities and partially filed lists. Measure actual browser fitting
   and generation on this machine; report draw budget and all failed requests.

## Reading and evidence

- Scenario correspondence and assumption records live beside the authored default.
- Shared estimation/data contracts: `vote-generation-fast-data.md`,
  `vote-generation-fast-model.md`, and the `vote-share/` engine README.
- Prior failed campaign: `vote-generation-fast-acceptance.md` and
  `test/fixtures/benchmarks/vote_share_acceptance.json`.
- New raw outputs: `data/derived/filing-aware-research/`; retain curated reports
  under `test/fixtures/benchmarks/`, with code/input hashes and declared cutoffs.

## Reproduction

Run Node 24 commands from `web/`. Prepare sources and the complete candidate
using the [release guide](vote-generation-fast-release.md), with
`REFORM_FAST_CANDIDATE=1` for the candidate application build. Do not edit
fingerprinted model files during release or browser verification.

`npx tsx --tsconfig tsconfig.json scripts/acceptance/filing-aware-research.ts MODE`
runs the comparison. Modes are `development`, `validation`, and `full FOLDER`;
prefix `reference-`, `mass-` or `regional-` for the three subsequent declared
experiments. Authored protocols are `data/filing-aware-*.json`;
`data/filing-aware-selections.json` preserves the sequential selections and their
development-report hashes. Full folders are `modular-1d5b31b17cfb` (2018/15d)
and `modular-939576fd812f` (2022/15d), generated using
`scripts/data-preparation/party-drift-modular-export-cli.ts camera-YEAR 15 10000 main`.
Historical reduced source panels and pins remain in the FV-01 evidence archive.

Run `scripts/acceptance/filing-aware-curate.ts` with the same `tsx` command to
retain compact reports and joint-draw simulation errors. Raw country-sized
predictive arrays stream through gzip; they are ignored regenerable artifacts.
The earlier pooled-group summaries were corrected from identical saved draws
to the declared equal-unit weighting, without rerunning or selecting new draws.

For the actual browser budget, run
`npx playwright test --config playwright.filing-aware.config.ts` with
`ACCEPTANCE_PREBUILT=1`, `FILING_PRODUCTION=1`,
`FILING_MODEL=bounded-mass-pair-field-v2`, `FILING_SCALE=1.5`,
`FILING_DRAWS=512`. It loads the prepared default, changes the reference date,
performs a real national/profile/paired refit, then executes all requested paired
generations and legal scrutinies. It retains failures and fails the 30-minute
gate. Without `FILING_PRODUCTION`, it runs the reduced synthetic numerical/browser
gate; `FILING_REPLAY_SOURCE` selects a browser retrospective input. Explicitly
type-check this harness with `npx tsc -p tsconfig.filing-aware.json`.

Run the workspace suite with `FILING_RESEARCH_COMPARISON=1` to retain functional
and memory reports for this authorized comparison separately from original
FV-08 acceptance. The older 512 MiB threshold is recorded as a diagnostic in
this mode, since it is not one of Luca's fixed replacement targets. With the
flag absent, the original memory assertion remains enforced. Test both `/` and
`/elezioni` builds. Full-campaign result-storage/export resource acceptance is
still an open FV-08 gate, not established by these single-run UI checks.

The browser evidence curator is `scripts/acceptance/filing-aware-finalize.ts`.
It requires the complete 512-run report plus both functional workspace reports.
Set the release command's `--report` path to
`../data/derived/filing-aware-research/selected-release.json`.
It checks the current generation fingerprint, preserves prior failed
engineering records and writes the tracked browser ledger/summary.

This is authorized model development. No additional approval is needed merely
to change the previous statistical kernel within these targets. Historical
scrutiny rules, observed counts and source availability still remain intact.
