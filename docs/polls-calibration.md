# Historical poll calibration

Use this guide for substep 3 of the poll work in
[MIGRATION_PLAN.md](../MIGRATION_PLAN.md). For current-poll import and opt-in
reference scenarios, see [poll preparation](polls-preparation.md). Measured scores
and verification are in the
[calibration history](migration-history.md#2026-10-23--historical-poll-calibration-pipeline-substep-3).

For the later weighted-poll uncertainty estimators, retests and regional/local
integration proposal, use [poll uncertainty research](polls-uncertainty.md).

From `web/`, with no network, raw government caches, R or Python:

```powershell
npm run data:polls:calibration:check
npm run data:polls:calibration:generate
```

`check` validates the authoritative historical poll JSON, result-table pins and
event pairings, then fits and evaluates the candidates without writing files.
Poll-survey references support optional manual government-site audits; PDFs and
their checksums are not inputs. Official election-result pins are separate and
remain unchanged.
`generate` writes an immutable `poll-calibration-v2-<hash>.json` report under
`data/derived/polls/`. The reference-scenario command also runs this audit,
writes its report, and embeds its version and rejection reasons in scenario
assumptions. It preserves the active v1 shares and uncertainty. It refuses a
receipt asserting accepted calibration until runtime integration is implemented.

## Selected evidence

The separate `history/` archive contains reviewed JSON for 16 government-submitted
surveys, with government references in its `manifest.json`. The six independent
events are:

| Election | Last publication cutoff | Pollsters |
| --- | --- | --- |
| Camera, 24–25 February 2013 | 8 February | SWG, Demos, Tecnè |
| European, 25 May 2014 | 9 May | SWG, Demos |
| Camera, 4 March 2018 | 16 February | SWG, Demos, Tecnè |
| European, 26 May 2019 | 10 May | SWG, Demos, Tecnè |
| Camera, 25 September 2022 | 9 September | SWG, Demos, Tecnè |
| European, 8–9 June 2024 | 24 May | SWG, Tecnè |

Every selected question has fieldwork and publication at/before its cutoff.
Forecast horizons run to the first voting day. Archive insertion dates can
differ from publication: record the survey's publication date in the manifest.
Exclude local and post-election surveys even when the archive date filter returns them.
The 2014 Tecnè political-party table was excluded because it did not establish
a European ballot. SWG 2018 and Tecnè 2019 report ranges: their explicit
midpoints are labelled estimates, not additional confidence intervals. Demos
decision-certainty questions are not treated as vote-table nonresponse.

The [Interior Ministry historical-data service](https://www.interno.gov.it/it/temi/elezioni-e-referendum/dato-storico-elezioni)
provides the eight pinned Camera/European result archives in
`elections-manifest.json`. Camera 2008 and European 2009 supply first-event
historical baselines; subsequent baselines use the previous same-kind election.
An empty M5S mapping in 2008/2009 means it was absent from those ballots, not
that a missing result was estimated as zero. Baselines are frozen historical
results available now, rather than reconstructed contemporary download vintages.

`results.json` retains source/member hashes, full national list totals,
denominators, and the comparable panel. Camera includes domestic proportional
lists, excluding Valle d'Aosta and overseas; European data includes domestic
Valle d'Aosta and the separate 2024 out-of-town-voter member, excluding overseas.
The national adult poll frame approximates these scopes. Camera 2018 uses the
government's attributed national list totals. Camera 2022 reconstructs
candidate-only attribution at the original uninominal-college level using the
existing historical importer; direct list votes alone are not the target.
These are the pinned open-data snapshots, not a claim to match every later
proclamation or live results release.

The comparable partition is **PD, M5S, Lega, all other lists**. Each of the three
parties is separately observed in every selected survey. Other is a changing
complement: do not transfer its bias to a specific new party or infer a missing
FdI value from a grouped answer. European translations deliberately retain this
panel only; full remaining-party tables can be manually consulted on the government
site using manifest references. Camera
translations retain the fuller tables. The six events are a purposive sample,
not a complete poll census; selection and source availability can affect results.

## Estimation and acceptance

Expanding chronological folds train on earlier elections only: 2013 trains
the 2014 forecast; 2013–2014 train 2018; and so on, through 2024. Neither the
held-out result nor future results enter a fold's fitted parameters. There are
five holdouts overall, including only two Camera holdouts. Scores give each
election equal weight, then each of the four panel categories equal weight;
parties and multiple houses are not additional independent elections.

Compare previous-election shares, recency/sample-weighted polls alone, the
actual v1 historical/poll blend, pooled bias correction, additional uncertainty,
and their combination. Four additional benchmarks attach original national
uncertainty or original national uncertainty plus a separately fitted error floor
to previous-election shares and weighted polls alone. The original point-only
rows remain for continuity.

For these four rows, original uncertainty is `0.02 × sqrt(elapsed days)` on the
logit scale, with no polling measurement variance, historical pseudo-sample
variance or v1 shared-error allowance. Historical elapsed time starts at the
previous election's date, parsed from its pinned source ID. Weighted polls are
references supplied at the cutoff, so their elapsed time runs from the cutoff
to election day. This reproduces the original **national component** within the
same four-category surrogate; it does not reproduce regional/local shocks,
elector-share/turnout normalization or chamber coupling of the full simulator.

Each benchmark's additional floor is estimated from its own earlier-event logit
errors, after subtracting its own original movement variance, and shrunk with
three prior election equivalents. No fitted bias correction is added to these
four rows. Exact zero/one reference shares stay structural boundaries, as in the
original generator: noise cannot invent a missing party. Boundary observations
are excluded from logit-floor fitting; per-category usable-event counts and zero
fallback floors are reported. This matters for M5S before its first election.
These benchmark rows are diagnostic and do not expand automatic correction
acceptance to new scenario/runtime policies.

Relative house deviations are centered within elections
and shrunk toward zero with three prior election equivalents. Aggregate bias
is fitted after house adjustment, with the same shrinkage. This is a regularized
method-of-moments candidate, not an identified causal house-effect model.

The extra logit variance uses election-level residual error, retaining separate
fits for the uncorrected and bias-corrected means, and subtracting v1
sampling/prior/error variance and assumed national
movement (`0.02² × days`) before adding a nonnegative floor. This avoids adding
the entire observed blackout error a second time as measurement noise. It
cannot separately identify late movement, shared response bias and electorate
mismatch. The report preserves all fitted effects, training IDs and assumptions.

Intervals use 4,000 deterministic joint logistic-normal draws per holdout and
candidate, followed by share normalization. Report pointwise 90% coverage,
proper interval score (width plus missed-tail penalty), and MAE, overall and
by election kind. They are a national valid-share surrogate: they do not validate
municipal geography, turnout coupling, chamber differences, arbitrary scenario
lists or simultaneous coverage in the actual generator.

Corrections are **withheld**. Acceptance requires at least three chronological
Camera holdouts, improved interval score, at least 80% observed Camera coverage,
and verified full-generator interval coverage. Mean corrections also require
a 5% Camera MAE improvement; uncertainty-only correction must not worsen MAE.
European observations add diagnostics and partially pooled training evidence;
they cannot supply the missing Camera holdouts. The uncertainty-only candidate
passes Camera metric thresholds but lacks sufficient holdouts and runtime
validation; mean corrections fail metric thresholds. V1 itself undercovers this
panel. Keep poll-informed scenarios opt-in
and explicitly uncalibrated. Before forecast acceptance, expand same-kind
evidence, revisit historical-prior strength and validate full-generator intervals.
Sparse/new lists retain explicit proxies and uncertainty assumptions so that
simulation remains computable.

## Reproduce or extend official results

The reviewed poll JSON and compact result table stay in Git; optional poll PDFs
remain ignored. Large downloaded election ZIPs stay ignored under
`data/sources/poll-history/`. To verify the frozen target table
against all pinned government originals:

```powershell
npm run data:polls:calibration:download
npm run data:polls:calibration:verify-results
```

Downloads verify hashes and reuse valid caches. A changed remote archive fails
instead of silently refreshing a target. Import only the named scrutiny/list
members; candidate-preference files repeat list totals and must not be summed.
`freeze-results` is an explicit maintainer command for a reviewed manifest change;
it rewrites only `results.json` and `results-pin.json`. Review the regenerated
diff, run `verify-results`, then normal calibration checks. Normal check/generate
commands never download or rewrite tracked inputs. The historical selection,
including compact targets and metadata, has its own enforced 1 MiB budget.

