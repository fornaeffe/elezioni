# Restricted party-drift research

Research checkpoint, 26 October 2026. The identifiable-component restriction
improves several reduced fits but does **not** make a single Gaussian posterior
approximation reliable throughout the admitted class. A broad, informative soft
share still produces substantial approximation error. A reasonable importance
correction passes that smallest case and fails its increasing-geography test.
The authorized final fallback, offline Stan, passes the reduced reference checks.
Production generation and its defaults remain unchanged.

**Paused at Luca's explicit request, 26 October 2026, 16:10 Europe/Rome.** All
four active Stan chains were stopped. No background fitting remains. The larger
real-data pilot had reached warmup iteration 100 in every chain; no posterior
or convergence result is accepted from that interrupted attempt. Resume from
the saved inputs with fresh warmup, not from an accepted partial posterior.

Use the [model contract](vote-generation-drift-model.md) for the probability
model and the [membership proposal](vote-generation-party-memberships.md) for
source-label review. The [original stage-0 report](vote-generation-party-drift-stage0.md)
preserves the earlier unrestricted experiment; its stop does not describe the
new authorization or the new results.

## Scope and decisions

- Require collective distinguishing information at the forecast cutoff. Proper
  priors alone do not authorize a requested split. Structural resolution is a
  necessary screen, not posterior concentration or municipal identification.
- Test separate historical observations, a separate poll, an exact national
  reference and soft national references, including deliberately weak evidence.
- After the approximation failures, use Stan as the final reference engine in
  preparation. Luca waived the 30-minute gate for this fallback. Diagnostics and
  feasibility still require measurement; a faster invalid fit is not accepted.
- Luca selected **an admissible research panel** for chronological comparisons.
  Original lists are retained in provenance. Where identities must pool, score
  explicitly aggregated research outcomes; claim no internal calibration.
- Neither this selection nor the provisional membership table approves a
  production generator or implements arbitrary scenario refitting in a browser.

## Restricted posterior checks

Eight fixtures use the original grouped multinomial likelihood and proper,
reference-anchored Gaussian states. No inferred component counts become data.
Conditional reference SDs are 1.5 national and 0.5 local. The expected non-valid
reference share is calibrated to 0.5. These fixed scales are a research
conditioning convention, **not** the complete pooled-scale hyperprior model.

Soft inputs are specified in percentage points of valid-voter share.
`party-drift-reference.ts` calibrates the logistic-normal share mean and SD and
includes its density Jacobian. Exact aggregate shares eliminate one Gaussian
coordinate and retain the induced prior and constraint Jacobian. They do not
shift posterior draws after fitting. A national constraint is not imposed
separately on every municipality.

Reference sampling uses four elliptical-slice chains. The separate-history
fixture needed 48,000 retained draws per chain; the other fixtures use 12,000.
All eight finally meet R-hat <= 1.05 and ESS >= 400. The fixed comparison limits
are absolute probability-mean error <= 2.5 pp and central-90% width ratio
0.65–1.5. Historical negative-control quadrature is retained separately.

| Distinguishing evidence | Municipalities | Laplace mean error, maximum pp | Laplace accuracy |
| --- | ---: | ---: | --- |
| Separate historical counts | 1 | 0.58 | Pass |
| Separate poll with shared component bias | 1 | 0.18 | Pass |
| Exact 20% national reference | 1 | 0.29 | Pass |
| Soft 20% reference, SD 1 pp | 1 | 0.54 | Pass |
| Soft 20% reference, SD 5 pp | 1 | 1.37 | Pass |
| Soft 20% reference, SD 15 pp | 1 | **7.75** | **Fail** |
| Exact national reference | 2 | 0.27 | Pass |
| Soft national reference, SD 5 pp | 2 | 0.85 | Pass |

The complete run takes 230.49 seconds. Artifact:
`data/derived/party-drift/restricted-party-drift-67975df6508a/summary.json`.
This is a dense reduced accuracy experiment, not a sparse countrywide fitter.
The broad-reference maximum is a within-joint share error; its largest elector
component-share error is 4.35 pp. Both exceed the prespecified 2.5 pp limit.

### Importance correction and its limit

An exact-target Student-t mixture importance correction uses 32,768 proposals
and two seeds. On the failed one-city fixture it passes the posterior checks;
original-weight ESS is 1,487/3,128, maximum normalized weight 0.0116/0.0111,
and elapsed time 0.106/0.104 seconds. Diagnostics are measured before resampling.

The independent increasing-geography experiment fails already at eight
municipalities (27 latent coordinates): ESS 174/596 against a minimum of 1,000;
maximum weight 0.0475/0.0235 against a maximum of 0.02. It therefore stops before
32/60 municipalities. Do not extrapolate the attractive one-city timing to
countrywide fitting. Artifact:
`data/derived/party-drift/importance-geography-465f924ff57e/summary.json`.

### Offline Stan reference

CmdStan 2.37.0, CmdStanPy 1.3.0 and Python 3.12.12 run in WSL Ubuntu. Dependencies
and compiled models live outside the repository; requirements and model sources
are tracked under `web/scripts/data-preparation/stan/`. Model and prepared-data
hashes are checked. Four chains use 1,000 warmup and 2,000 retained draws each.

| Fixture | Sampling seconds | Maximum R-hat | Minimum ESS | Divergences / depth hits |
| --- | ---: | ---: | ---: | --- |
| Broad soft reference, one municipality | 0.17 | 1.0016 | 2,854 | 0 / 0 |
| Broad soft reference, eight municipalities | 1.43 | 1.0023 | 2,243 | 0 / 0 |
| Exact reference, one municipality | 0.18 | 1.0015 | 3,173 | 0 / 0 |

All BFMI values exceed 0.94. One-city soft and hard outputs also pass comparison
against the independent elliptical-slice reference; the exact reference residual
is zero to recorded precision. The eight-city result has Stan diagnostics but
no independent full-dimensional posterior accuracy comparison. First compilation
took 10.29 seconds, separately from sampling.

Recorded results are under
`data/derived/party-drift/stan-reduced-20cd28f9931f/stan-summary.json`.
The export command now discovers a matching independent reference artifact
instead of hard-coding that artifact's directory. Re-exporting changes its own
fingerprint; the mathematical Stan source is unchanged.

## Memberships, information cutoffs and research panels

The provisional table has **189 source-list/election rows and 107 statistical
identities** across eight elections, 2008–2024. Most identities are atomic rare
source lists; this is not 107 independently fitted contemporary parties.
`memberships-policy.json` limits decomposition to obvious joint lists and the
continuities needed to represent them. It keeps undocumented partners opaque.
The review document lists assumptions and sources; it is not an official
numerical genealogy. In particular, PCL/SCR is not confused with PdAC, and
endorsements are not automatically new components.

Validation covers 63,426 usable municipal-election cells, 239,088,120 valid votes
and 383,282,848 cumulative electors, conserving each original count once. The
common-domain full-history membership matrix has rank 107/107. That fact alone
does not establish chronological resolution or volatility identification.

An audit of the originally proposed full-list 24 holdouts finds **23 unresolved
at their cutoff**, even under an optimistic complement screen. The sole passing
case is European 2024 at 15 days. Two already cached 2024 government survey PDFs
contain full answer lists; the research sidecar restores those same questions,
rather than adding duplicate surveys. Original poll sources and production
bundles are unchanged. Publication and fieldwork-end dates still gate use.

`party-drift-panel.ts` constructs the authorized alternative:

1. Start from PD, M5S, Lega, FI/PDL and FdI where supported; close their explicit
   joint-list memberships, including the conditional future ballot.
2. Pool unsupported or unrequested source identities in an opaque political
   research residual. A future ballot is context, not distinguishing evidence.
3. Collapse remaining unresolved contrasts before fitting. Coarsen intersecting
   source outcomes by summing their original counts once; preserve source labels.
4. Construct each poll's Other complement on the original available basis
   **before** pooling, then combine overlapping research answers. Positive Other
   never becomes non-valid voting or an invented component share.
5. Check actual projected observations again for the real-data sample. Retain
   missing municipal histories as missing; conditional future geography may
   contain municipalities without training observations.

All 24 panels pass the preliminary structural screen, retaining 5–28 political
components. Counts conserve electors. The recorded first preflight is
`data/derived/party-drift/admissible-panels-e3e2bf7592e9/panels.json`;
later preparation also includes target-only municipalities as missing-history
units, so its fingerprint and municipality totals differ. No held-out vote
shares choose a panel or initialize a training fit. No full-list or internal
pooled-component coverage claim follows from this construction.

## Real-data computability gate

The current real-data pilot uses 16 municipalities, seven training elections and 33
polls for European 2024 at 15 days: 28 political components, 107 original count
cells and 4,762 parameters. It is conditional on fixed reference scales and uses
the literal poll link while jointly estimating common national/local innovation
scales with proper log-normal priors. It is a computability experiment before
the larger aggregate-link and pooled-reference-scale models.

`real-party-drift.stan` uses list-total logits and within-list log ratios to
avoid the count-induced curved ridge. The change to component log odds has a
constant Jacobian; original counts remain grouped. The local historical Gaussian
process is evaluated with normalized reference-anchored backward covariance
`0.5^2 + sigma_local^2 min(r-ti,r-tj)`; unused coordinates are proper auxiliaries,
not pre-birth observations. National innovations remain shared across cities.
The runner records convergence, BFMI, process RSS, CSV bytes and a clearly marked
rough city-linear extrapolation, which cannot certify full-data convergence.

The pilot is **paused by user request** at
`data/derived/party-drift/stan-real-673eff932932/`. `paused-attempt.json`
distinguishes that decision from the automatic error artifact produced by the
interrupted subprocesses. Four chains reached warmup iteration 100 during a
349-second incomplete run; this is not a measured total fitting time. Initial
gradient evaluations took 0.00636–0.00651 seconds. No countrywide fit or
stage-1–7 result is accepted by this checkpoint.

Before that run, a smaller real Camera-2013/730-day case (two municipalities,
two elections, one poll, 47 parameters) completed in 1.13 seconds: maximum
R-hat 1.0045, minimum ESS 1,266, zero divergences/depth hits, minimum BFMI 0.846.
Artifact: `data/derived/party-drift/stan-real-34e108ab6108/stan-summary.json`.
A gross city-linear estimate for that small model is 75.8 minutes for 8,051
municipalities. It excludes the much larger later models and cannot certify
their convergence or memory. The Stan time gate is waived; the research was
paused because Luca requested it, not because this estimate exceeded 30 minutes.

An independent NumPy implementation checks real-model density differences to
`1.46e-11` and selected finite-difference gradients to `3.31e-7`, including the
internal unconstrained array/matrix order used by the metric. Results are saved
in the same small-case directory. Two earlier larger attempts were interrupted
for engineering changes and remain explicitly unusable: the first lacked scale
preconditioning and produced overflow rejections during warmup; the second's
initial metric incorrectly followed CSV order. The corrected implementation
uses array-element order, then matrix column/row order. No probability-model
restriction was tightened to obtain a passing small fit.

Joint reference-scale hyperpriors/prior sensitivity, aggregate
poll temporal bridges, the staged alternatives, forward forecast scoring,
paired-chamber/seat validation and browser execution remain pending.

## Reproduction

From `web/` (pinned electoral/poll sources are required):

```powershell
npm run check:data
npm run test -- scripts/data-preparation/party-drift.test.ts
npm run data:forecast:party:restricted
npm run data:forecast:party:importance # expected nonzero: geography gate fails
npm run data:forecast:party:memberships
npm run data:forecast:party:cutoffs
npm run data:forecast:party:panels
npm run data:forecast:party:stan:prepare
npm run data:forecast:party:real:prepare -- 16 european-2024 15
```

The original `party:validate` remains the historical negative-control experiment
and intentionally rejects its Laplace candidate. Do not confuse that command
with the restricted suite. Generated data/chain CSVs remain ignored under
`data/derived/party-drift/`.

Install `stan/requirements.txt` into an isolated Python environment and provision
CmdStan 2.37.0 with a C++ toolchain. Run `stan/run_restricted.py <export-directory>`
or `stan/run_real.py <export-directory>`, supplying `--cmdstan <installation>` and
`--cache <external-model-cache>`. In this session these run through `wsl -d Ubuntu`
with `/home/forna/.cache/elezioni-research/python-env/bin/python`. The manifest
pins data, code, policy and independent reduced-reference hashes. A clean clone
must regenerate ignored artifacts in dependency order.

At the pause, the focused research suite had 23 passing tests and preparation
type checking had passed. The complete repository suite was not rerun after
this follow-up's final edits. No further tests or fits should run until Luca
resumes the work. On resumption, first run `check:data` and the focused suite,
recreate the final panel preflight (including future-only municipalities), and
finish the real-data gate before claiming a staged backtest result.
