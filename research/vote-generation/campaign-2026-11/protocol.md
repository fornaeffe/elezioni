# Chamber-aware model comparison, November 2026

This is an active research campaign. It does not install a generator or change
M01/M02 2.0.0, scenario defaults, legal scrutiny or frozen FV-01/FV-09 evidence.
Start with [results](results.md) for findings and limitations, or the
[candidate equations](models.md) for the probability laws; use this file to
understand or rerun the experiment. Old experiments remain in the [archive](../README.md).

## Questions and candidates

1. Does current M02 generate much larger Camera–Senato differences than 2022?
2. Can a single common latent field remove unnecessary independent chamber
   uncertainty while preserving Camera's marginal forecast law?
3. How do M01, M02 and the simpler endpoint alternatives trade extreme errors,
   calibration, point accuracy, WIS, time and memory?

| ID | Law used in this campaign |
| --- | --- |
| M01 2.0.0 | Active `generation/m01.ts` and `m01-polls.ts`, unchanged |
| M02 2.0.0 | Active reference/profile fitting, reference noise, bounded mass-pair field and multinomial kernels, with a small research orchestration adapter verified against `prepareM02Votes` |
| M02-shared research-v1 | One complete M02 Camera probability field; Senate is its deterministic paired-contrast pushforward, then its own count draw. No second posterior selection, profile noise, movement-scale draw or bounded-pair trajectory |
| M05 research-v1 | Joint Gaussian endpoint and spatial innovations |
| M06 research-v1 | Covariance-matched joint Student-t innovations; one scale per vector |
| M07 research-v1 | Gaussian endpoint plus a signed, whole-country historical spatial transition block and regularizing Gaussian noise |

M05–M07 adapt the [earlier methods](../docs/vote-generation-methods.md) from four
to six observable families and use a shared chamber field plus the same descriptive
paired contrast as M02-shared. Their national innovation budget is an endpoint
budget, without adding M02 movement/discrepancy or another poll-error draw.
Their central polling point uses the latest eligible complete grouped question,
with within-group proportions inherited from the historical baseline, rather
than the earlier four-category weighted polling average. This is an explicit
grouped-evidence adaptation, aligned with M01's latest-publication convention;
it is not an unchanged replay of the earlier experiment. Endpoint counts now use
the same municipal multinomial layer as M02 instead of deterministic rounding.
M01 retains its own specified seeded integer-apportionment law. M02 retains
separate Camera/European reference-fitting contexts. The endpoint models pool
completed Camera/European errors and spatial transitions, retain a same-kind
valid-rate center, and have no separate geographic kind intercept. Their mixed
spatial budget can therefore include election-kind changes; the Camera-only
sensitivity matters. European coordinates are never identified as Senate states.
M02-shared changes a probability law and would require a new MAJOR version if
adopted. It is not M02 2.0.1. `M02-row-sync` and `M02-rho1` are diagnostic
ablations, not additional production candidates.

## Common data boundary

The current hash-validated compact source, its pinned municipal counts, source
memberships, effective poll samples and dates are the only acquisition inputs.
No archived script with broken original-layout imports is executed in place.
Every result records the source ID/versions, seed, input hash and eligible IDs.
Membership reviews, current municipal crosswalks and source corrections are a
present-day pinned reconstruction. This is a retrospective temporal backtest,
not an as-published historical data-vintage experiment. The targets have already
been examined in earlier research; they are not untouched confirmation data.

The panel is FdI, FI/PdL, Lega, M5S, PD and changing Other. An **entire observed
ticket** with exactly one of these five named component identities belongs to
that family. Thus PD+Art1 is an observed PD-family ticket, not a fabricated
PD/Art1 split. Tickets with none belong to Other; two named identities fail.
This is a reduced electoral-family experiment, not validation of all future
list/component identities. M02 keeps unoffered historical families absent from
the observation mask. Endpoint log-ratio coordinates instead use the old,
explicit 0.005 share floor as a modelling proxy. Poll groups are coarsened
without splitting a joint observation; M01 retains its specified equal-composition
assumption for otherwise unspecified grouped polling answers.

Exclude Valle d'Aosta and Trentino-Alto Adige from every event: their special
ballots do not supply the same list decomposition across chambers. Exclude
unmatched places, ambiguous target mappings and invalid denominators, recording
their electorates. No missing place becomes a zero. Municipal candidate-only
attribution is the existing conserved-margin reconstruction, not an observation
of the voter's chosen list.

Each forecast uses the latest eligible baseline electorate, intersected with
matched target geography; target vote counts and target electorates are withheld
from generation. Missing M01 Camera local history uses original regional pooled
counts. Both chamber forecasts use the same conditional electorate. This does
not reconstruct historic demographic change.

College and constituency results aggregate to the **current 2022/census
footprints** using municipal population shares, including splits. They are
reconstructed footprints for older elections, not their historical colleges.
Camera and Senate are projected onto each identical footprint before subtraction;
subtracting differently bounded native colleges would confound politics with
geography. Reports include both Camera and Senate footprints. National means
the matched domestic domain, not a claim about overseas or excluded regions.
Municipal simulated counts are allocated fractionally to those footprints.
This retains the forecast geography but omits the extra count noise of independently
sampling physical submunicipal ballot cells. The active-M02 parity check uses
one physical cell per municipality. These are share-forecast comparisons on
reconstructed footprints, not a full split-ballot or legal-assembly acceptance.

## Temporal folds and Senate evidence

Primary Camera folds: 2018 and 2022, at horizons 15 and 365 days, using mixed
Camera/European history, 256 draws. Camera-only history at 15 days for both
targets is a sensitivity. Every history event must precede the cutoff and be
available by it. Poll date, fieldwork end and publication must all be eligible;
retain at most 180-day-old polls within 35 days of the newest eligible release.
Training endpoint errors are rebuilt at the same earlier horizon; each completed
election contributes one vector, not one vector per municipality or poll.

Production M01/M02 require a pre-cutoff common-18+ chamber pair. The bundled
history has **only 2022**; pretending these kernels can perform an unchanged
pre-2022 production replay would be false. This research explicitly waives only
that preparation requirement for the Camera forecasts. It calls the same current
laws after supplying a Camera baseline; no age waiver enters production.

The paired 2022 forecast uses 512 draws and a separate, explicitly **age-confounded
2018 sensitivity**: 2018 Senate electors were 25+, Camera 18+. All outcomes are
rescaled jointly to equal conditional electorates before the existing descriptive
contrast adapter. This preserves observed proportions, not age comparability.
No 2022 Senate outcome trains this out-of-time comparison. Its contrast errors
cannot be interpreted purely as model error or future 18+ calibration.

A separate 2022-contrast reconstruction uses the held-out year's paired
differences and is deliberately **in-sample**. It diagnoses whether independent
chamber simulation overwhelms even a supplied small paired contrast. It cannot
select a predictive winner. Row synchronization and rho=1 ablations accompany it.
One same-age pair cannot identify temporal contrast innovation or certify a
future bonus-discordance probability. No tuning or nominal 90% acceptance band
is learned from thousands of municipalities as though they were independent
elections.

## Scores and comparison order

All shares use valid votes. For model samples $p^{(s)}_{aj}$ and observed
$y_{aj}$, average categories equally inside each area, then areas equally
inside each fold; folds are reported individually and receive equal weights
in summaries. Do not pool municipality/college counts into an election sample.

1. **error30:** fraction of draws with $|p^{(s)}_{aj}-y_{aj}|>0.30$; this is a
   30-percentage-point error, not a 30% relative error. Lower is better.
2. **Coverage90:** fraction of truths inside central 90% predictive intervals.
   Assess distance from 90% together with width and WIS; 100% is not the goal.
3. **MAE:** absolute error of the predictive mean, averaged over areas/families.
4. **WIS:** median plus central 50%, 80%, 90%, 95% intervals:

$$
\operatorname{WIS}(F,y)=\frac{0.5|m-y|+\sum_{\alpha\in\{.5,.2,.1,.05\}}(\alpha/2)\operatorname{IS}_{\alpha}}{4.5},
$$

$$
\operatorname{IS}_{\alpha}=(u-l)+\frac2\alpha[(l-y)_++(y-u)_+].
$$

For the proposed **chamber-contrast criterion**, score the signed difference
$\delta_{aj}=p_{S,aj}-p_{C,aj}$ against the observed difference with exactly
the same Coverage90, MAE and WIS. Primary selection uses **contrast WIS with
contrast coverage**, which penalizes excessive width and collapsed uncertainty.
Report the diagnostic magnitude ratio as well:

$$
R_{\Delta}=\frac{S^{-1}\sum_s\sqrt{(AJ)^{-1}\sum_{a,j}(\delta^{(s)}_{aj})^2}}
{\sqrt{(AJ)^{-1}\sum_{a,j}(y_{S,aj}-y_{C,aj})^2}}.
$$

Also report the observed RMS percentile in the joint simulated RMS distribution
and largest absolute gap. A very large ratio plus percentile near zero diagnoses
overdispersion. The ratio is **not** a proper scoring rule or a standalone tuning
objective; zero observed RMS makes it undefined. Comparing signed deltas avoids
rewarding a reversed chamber effect with the same magnitude.

All rows must have nonnegative integer outcomes conserving electors. These checks
are necessary but do not establish political plausibility. With 256/512 draws,
rare error30 estimates have finite Monte Carlo resolution. National coverage has
only six correlated family outcomes per election; do not report a precise
election-level calibration confidence interval from them.

## Resource protocol and reproduction

Run npm/Node commands from `web/`, with the pinned lockfile and Node 24:

```powershell
npx tsc -p ../research/vote-generation/campaign-2026-11/tsconfig.json
node --import tsx --test ../research/vote-generation/campaign-2026-11/code/verification.test.ts
node --import tsx ../research/vote-generation/campaign-2026-11/code/campaign.ts
```

Single case:

```powershell
node --import tsx ../research/vote-generation/campaign-2026-11/code/run.ts --model=M02 --target=camera-2022 --horizon=15 --regime=mixed --draws=512 --mode=2018-age-audit
```

Use `--limit=160 --draws=8` for a computability pilot.
Default generated destination: `data/derived/vote-generation-campaign-2026-11/`.
Reports/details are regenerable; the curated summary and code/source manifest
live beside this protocol. Never overwrite frozen receipts.

Run the six 512-draw paired cases in separate **sequential, isolated** processes
for speed/RSS comparison. Run remaining accuracy folds in batches of at most
eight processes, bounded also by half the logical CPU count and one GiB available
RAM per process; their timings are explicitly not speed benchmarks. Report source
loading, preparation, generation and score aggregation separately; high-water RSS
includes them all. A fit-plus-1,000-generation estimate scales measured generation time;
it is not a measured 1,000-draw browser+scrutiny acceptance. M02-shared's Camera
marginal is identical to M02 by construction and verified seeded parity, so reuse
its historical Camera scores explicitly; benchmark the paired paths separately.
No result here establishes the under-60-minute browser+scrutiny requirement.
`campaign.ts --resume` reuses only passed ledger entries with the exact current
transitive code/lockfile identity. A partial case is rerun. The initial sequential
driver was replaced after the first timing cases; completed matching receipts are
retained, and resumed versus new work is explicit in the execution ledger.
