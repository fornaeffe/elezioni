# M01/M02 comparison feasibility — 2 November 2026

The initial assessment found the historical comparison **blocked under the
unmodified production contracts**. Luca subsequently approved a test-only
Camera preparation exception. See the
[current Camera-only comparison](current-camera-comparison-results.md) for its
scope, execution and results. The restrictions and receipt below document the
initial assessment; they do not prohibit the approved research exception.
The previous comparison and its receipts remain unchanged.

## Requested metrics and test scope

Reuse the [previous comparison](vote-generation-legacy-comparison-results.md)'s
observable-group scoring, denominators, geographic domains and weighting:

| Part | Required workload | Feasibility |
| --- | --- | --- |
| Historical reduced panels | Camera 2013/2018/2022; 730/365/90/15-day cutoffs; 512 draws per model/case | All 12 target/cutoff combinations blocked |
| Historical full domains | Camera 2018/2022 at 15 days; 512 draws per model/case | Same blocked short-horizon cutoffs |
| Future operational checks | Requested 24-list ballot; 1,000 paired generation/scrutiny attempts per model | Current paired sources and browser workflow exist; campaign not run in this assessment |

Historical metrics include Error30, Coverage90, predictive-mean MAE, WIS, CRPS,
interval widths, near-truth frequency, extreme tails and group bias, at municipal,
college, constituency and evaluated-area levels. Whole-draw Monte Carlo tail
errors require actual joint draws. Future timing, exceptions, non-computability
and filled-seat counts cannot substitute for historical predictive scores.

## Blocking source requirement

[M01 1.0.0](models/M01.md#notation-observations-and-scope) requires the latest
cutoff-eligible same-date Camera/Senato pair with common age-18+ electorates.
[M02 1.0.0](models/M02.md#4-senate-reference-and-chamber-dependence) requires that
paired history for its Senate adaptation. Its supported refitting service also
compiles the pair when preparing a Camera-only renewal; changing `renewal` alone
does not remove this preparation dependency.

The installed source bundle contains:

| Pair | Source availability | Senate population | Eligibility |
| --- | --- | --- | --- |
| 2018 | 5 March 2018 | Age 25+, `age-audit` | Rejected by both current paired contracts |
| 2022 | 26 September 2022 | Age 18+, `paired-adaptation` | Eligible only after all requested historical cutoffs |

There are no earlier Senate events in the bundle. The latest requested cutoff
is 10 September 2022. Reusing the 2022 pair for these forecasts would leak
held-out results; relabelling the 2018 age-25+ electorate would change the source
meaning. Neither is a valid workaround. Current 2026 fitted states and default
reference anchors likewise cannot serve as historical cutoff-owned inputs.

Direct probes of current implementation boundaries confirm:

- M01 `latestLegacyPair()` rejects every one of the 12 cutoffs with
  `No eligible common-18+ Camera/Senato pair at this reference date`.
- M02 `compilePairedHistory()` rejects both supplied pair selections at every
  cutoff with `PAIRED_SOURCE_UNAVAILABLE` (24 rejected pair/cutoff attempts).
- A positive control at the installed reference date, 19 October 2026, accepts
  the 2022 pair and compiles 7,904 matched municipalities. Source loading and
  integrity checks pass; the historical failures concern eligibility.

The [source-pinned receipt](../test/fixtures/benchmarks/model_comparison_feasibility.json)
records all cutoffs, event metadata, implementation hashes and the positive
control. Reproduce from `web/`:

```powershell
node --import tsx scripts/acceptance/model-comparison-feasibility.ts
```

This command writes the feasibility receipt and executes no model draws or fits.

## Evaluation tooling and next decision

The historical `legacy-comparison.ts` still calls the old list-based generator;
it is not an M01/M02 evaluator. Its former browser configuration is absent.
`legacy-comparison-report.ts` combines frozen older procedures, so it must not
be used to label those results M01/M02. The scoring helpers remain reusable.
The current future workflow is `playwright.unified-share.config.ts` with
`unified-share.spec.ts`, which selects only M01 and M02.

Luca approved a Camera-only research design: M01 uses the latest eligible Camera
baseline without a Senate prerequisite; M02 fits Camera/European history and
uses only Camera coordinates/profiles. Production contracts remain unchanged.
The [research adapter and results](current-camera-comparison-results.md) retain
cutoff-owned evidence, explicit M01 compositions and mandatory references, current
physical-cell generation and the old observable-group scoring contracts. New
evidence is separate from frozen old-model results; no old-model rerun is needed.

A future-only performance/reliability campaign can instead use the current
paired baseline, but it answers only the operational part of the request.
No model law, source, default, scrutiny rule or production routing was changed.
