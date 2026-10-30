# FV-08 acceptance: reform vote generation

## Status and next action

**30 October replacement campaign:** Luca authorized changing the statistical
kernel within the [new fixed goals](vote-generation-feasible-research.md).
The feasible candidate/default and current results are documented there. The
capacity failures and proposed next investigation below record the **earlier
global-Beta candidate**, not the current feasible field. No further permission
is needed to conduct that authorized comparison. The original screen remains
retained evidence; it has not been overwritten or reclassified as passing.

**Default selection is blocked.** The installed v1-v3 generator remains the
normal reform default; `REFORM_FAST_CANDIDATE=1` still selects the v4 candidate
for development. FV-08 is not complete, and FV-09 archival must not start.

Use the [frozen validation protocol](vote-generation-fast-validation.md) for
acceptance thresholds and the [history](migration-history.md) for measured
results. This guide covers only FV-08 execution and the open model decision.

The full-country generation/legal preflight exposes infeasible forecasts for
restricted-filing components: **35 capacity errors among 1,000 requested draws**;
the other **965 runs complete with all 588 seats filled**. These counts describe
the retained earlier candidate screen, not final browser-pipeline acceptance;
diagnostic repeats using that same original kernel reproduced the targets below.
For frozen seed `fv08-production-33`, the seven
Valle d'Aosta components jointly request an elector share of
`0.002570668634721578`, but their physical filing contains only
`0.0021289396262279586` of national electors. Seed `fv08-production-45` requests
`0.02028188884312478` against a joint capacity of `0.019713577210721138`.
These are transport-capacity violations, rather than projection convergence or
floating-point errors. The generator rejects them before legal scrutiny.

The extra default components are anchored by regional reference shares, but
their future fallback innovations in that generator act on national shares. A Beta
transition can move a territorially restricted group above the total mass of
all cells where it is filed. An individually feasible share is insufficient:
overlapping groups can also violate their joint capacity. The reduced research
domain did not establish acceptance for these expanded special ballots.

Do not clip targets, increase projection tolerance, reroll seeds, drop failed
runs, or reduce the declared forecast horizon to conceal this result. The frozen
protocol explicitly blocks selection for an unexplained concentration defect.

## Earlier proposed model decision

The recommended next investigation is a **support-aware transition for
restricted-filing components**. It would model their support conditionally within
their actual filing domain, while retaining explicit turnout, exact structural
zeros and a coherent joint Camera/Senate draw. National totals would be derived
from that feasible field. Ordinary nationwide fitted components, user reference
interventions, regional anchors and the treatment of pooled historical identities
must be specified together; moving a denominator alone is not a complete fix.

This was the investigation proposed before Luca authorized the
[implemented replacement comparison](vote-generation-feasible-research.md).
Its original follow-up was to specify equations, shared-domain/overlapping-filing
behavior, interactions and reference semantics, then add independent fixtures and repeat the frozen failing seeds
without replacing their earlier ledger entries, and rerun the whole FV-08 panel.
Conditioning global draws on feasibility is another model choice; it changes
their marginal distributions and cannot be introduced as an invisible retry.

## Reproduction

Run from `web/` with Node 24 and the pinned dependencies. Required inputs must
exist; these scripts fail rather than use debug bundles. Preparation commands
are in the [release guide](vote-generation-fast-release.md).

```powershell
npm run data:reform:shares:release
npx tsx --tsconfig tsconfig.node.json scripts/acceptance/fast-share-production.ts 10
npx tsx --tsconfig tsconfig.node.json scripts/acceptance/fast-share-production.ts 1000
npx tsx --tsconfig tsconfig.node.json scripts/acceptance/fast-share-production.ts capacity
npx tsx --tsconfig tsconfig.node.json scripts/acceptance/fast-share-resources.ts 128/512 6
npx tsx --tsconfig tsconfig.node.json scripts/acceptance/fast-share-replay-cli.ts 24
$env:REFORM_FAST_CANDIDATE = '1'
npm run test:reform:shares:ui
```

Repeat preparation/build/browser checks with `$env:BASE_PATH = '/elezioni'` for
the project-base build; reset both environment variables before restoring the
standard build. Do not edit fingerprinted model sources while preparation or
acceptance is running. A release binds its implementation and fitted assets.

Raw reports are ignored under `data/derived/fv08-acceptance/`; retain curated
review evidence under `test/fixtures/benchmarks/` with
`scripts/acceptance/fast-share-curate.ts` after both browser reports and the
complete 1,000-draw ledger exist. That command retains this failed screen and
cannot declare a passing release. These commands use the selected local catalog;
reproducing the original failure requires its retained original scenario/kernel.
Current feasible-kernel production diagnostics are written separately under
`data/derived/filing-aware-research/production/`, by catalog ID. The `capacity` command is an explicitly
labelled diagnostic repeat of failures 33/45 and exits unsuccessfully while
those targets remain infeasible. Production seeds are
`fv08-production-0` through `fv08-production-999`. The preflight records every
requested draw and its error or legal result. It is a **Node generation/legal
screen**, not the required browser storage/plot/export acceptance or a matched
comparison with the installed generator.

The replay CLI uses the original 16 development and 8 corrected confirmation
cases, 128-unit domains and 512 predictive draws. It writes point comparisons
and raw predictive score inputs. Its `future` availability variant reproduces
the research choice-set order for diagnosis; `reference` uses the approved
production order. Neither output accepts port parity without the frozen
1,000-replicate chain-block simulation MCSE and separate election-cluster
sensitivity. Do not treat municipalities or horizons as independent elections.

## Remaining release gates

Apply the [authorized replacement goals and measured draw budget](vote-generation-feasible-results.md)
when updating this older acceptance campaign. Its 30-second/512-MiB/1,000-run
figures are not additional immutable statistical targets for the replacement.
The ordinary production switch still needs coherent storage/export, legal and
resource verification for the declared supported workload.

- The feasible replacement now completes the requested full-default, special-
  territory and Senate computation without future target repair. Retain its
  coverage/bias limitations and the earlier failed global-Beta screen.
- Complete the original-domain predictive comparison with simulation MCSE,
  regional and observable-group scores; then run the compatible full-country
  benchmark and expanded historical comparisons against the installed generator.
- Run the declared paired-browser campaign, consistent with the supported
  30-minute computation budget, through legal scrutiny, result storage, plots,
  export and import, retaining requested denominators and
  reporting admission/42%/bonus/caps, seats, candidates and failures.
- Complete default and maximum-envelope browser resource measurements, five warm
  plus one cold repetition, actual 4x worker throttling, cancellation feedback,
  simultaneous multiworker memory and constrained-memory single-worker behavior.
  Synthetic Node timing and sampled Chromium heap alone do not close this gate.
- Complete current-source root/project-base release checks, all supported browser
  targets and source-only cold Linux/offline-warm preparation. Preserve required
  data checks and fixed-input legal regressions; no required skips.
- The Windows WebKit 26.4 test target has no `navigator.storage`, so its
  disk-backed result-export checks fail. The capability guard now preserves the
  intended explanatory error instead of throwing a property-access exception.
  Chromium and Firefox export checks pass. Validate an OPFS-enabled WebKit
  target or provide a separately reviewed bounded export design; do not accept
  this failed target as a skip or infer Safari support from the other browsers.
- Only after all gates pass, change the reform default dispatch. Public
  deployment remains the final migration publication gate.
