## 2026-10-17 R26-04: approved paired chamber vote model

Luca approved **shared political shocks plus historical chamber contrasts and
correlated residuals**, refined so a supplied common national/local share anchors
model-derived chamber references and explicit per-chamber shares take precedence.
Recorded the [model, input precedence and validation contract](../../politics-reform-2026-modelling.md#camera-senato-votes-r26-04)
and marked the decision step complete. R26-01/01a/02/03/04 are now complete as
documentary/decision steps; R26-05 contracts are next. No parameters were fitted,
generator implemented or production data imported.

### Decision and reference precedence

The model combines shared national/territorial movement with regularized
historical contrasts and chamber-specific national, regional and local variation.
Its joint covariance must preserve the intended marginal uncertainty; adding
unbudgeted Senate noise to an existing Camera draw is not the selected method.
Numerical residual scales and dependence parameters require fitting or explicitly
labelled assumptions, with weaker/stronger dependence and gap sensitivity.

A supplied common share is a shared starting anchor from which the model derives
distinct Camera and Senato references. An explicit reference replaces the
modelled reference adjustment for that chamber at that scope. When only one
chamber is explicitly supplied, the other still derives from the common anchor,
or from its historical/model baseline if no common input exists. Both explicit
references override both modelled adjustments. Correlated election-day
uncertainty remains active in mean mode; explicit reference values are not
fixed simulation results.

Across geographic scopes retain the established broader-to-more-specific
resolution. Within the same scope, per-chamber input wins over common-derived
references, which win over model defaults. This preserves local specificity,
partial-share rules, abstention handling, valid-vote/elector conversion and
normalization/warnings. Trace the common anchor, supplied/derived references,
scope and reconciliation rather than interpreting a shared input as Camera-only.

| Alternative | Decision and rationale |
| --- | --- |
| Identical territorial votes | Retain as a benchmark, not the production model: omits chamber behaviour and can couple joint-bonus events too tightly. |
| Independent chamber generation | Not selected: discards the common political environment. |
| Separate historical baselines with identical shocks | Not selected as the full model: treats the chamber gap's uncertainty as absent. |
| Shared shocks, historical contrasts, correlated residuals | Selected: represents common movement and chamber differences, with explicit transfer/dependence uncertainty. |
| Supplied chamber-specific inputs | Retained with precedence over model-derived references at the same scope. |

### Official source audit and exploratory comparison

The decision review found no Senate election history in the existing
`dati/dati.RData` bundle. Its historical elections were Camera 2018/2022,
European 2019/2024 and regional 2020/2024. The existing
[vote generator](../../../web/src/lib/politics/vote-generation.ts) draws one territorial
list table and aggregates it into each chamber's geography. Downstream list
coverage/filtering can still produce differences; this is not a claim that all
final legacy chamber figures are identical.

Official archives were downloaded on 17 October 2026 to the operating-system
temporary directory `elezioni-r26-04`, outside the repository. The Ministry's
[2022 catalogue entry](https://www.dati.gov.it/node/view-dataset/dataset?id=caca3eec-122a-4668-a75b-3a2426ce3ae6)
identifies municipal Camera and Senato result files. The historical download
endpoint redirected successfully to the DAIT archive; direct ZIP retrieval
worked even where browser page retrieval returned access errors.

| Archive | Bytes | SHA-256 |
| --- | --- | --- |
| [Camera 2022](https://dait.interno.gov.it/documenti/opendata/camera/camera-20220925.zip) | 1,216,091 | `dd8ede158ea3f7b0aae65024135a27e34ab9e959728df95f0e977cd56edf88ff` |
| [Senato 2022](https://dait.interno.gov.it/documenti/opendata/senato/senato-20220925.zip) | 1,181,512 | `ffaf0296cbb3702d1e0d13a9b7f88b6370f2ea2930b44d8b80a57c5fcc759c05` |
| [Senato 2018](https://dait.interno.gov.it/documenti/opendata/senato/senato-20180304.zip) | 1,314,885 | `ebee153d2546a99a1e14682a070f05f6fa6008f53ac13a658506e0a9a3d87920` |

The 2022 ordinary tables are `camera2022_Italia_LivComune.csv` (117,591 rows,
including Camera TAA rows) and `Senato_Italia_LivComune.csv` (117,510 rows).
They contain municipality, electoral colleges, electorate/turnout, direct list
votes and repeated candidate totals. Senate Valle d'Aosta/TAA results and turnout
tables are separate; both ZIPs also contain foreign results, which were not
incorporated into this domestic assessment. The 2018 archive contains municipal
and aggregate Senate results; availability was verified, not age-adjusted fitting.

Exploratory Python comparison, reproducible from the fingerprinted ZIPs:

- Read the two 2022 ordinary CSVs as UTF-8 with BOM handling and semicolon
  delimiter. Normalize Camera `CIRC-REG` to region by removing a trailing
  constituency number 1-4; exclude regions starting `TRENTINO` or `VALLE` in both
  ordinary tables. Match on region and municipality name, then sum `VOTILISTA`
  by that key and list label. All **7,548** resulting keys match across chambers.
  This name-based join is exploratory; verify ISTAT identities, ambiguities,
  geography and correspondence before production fitting.
- On that common ordinary footprint, summed raw direct list votes are
  **26,600,019 Camera** and **26,601,426 Senato**. These are raw-field diagnostics,
  not final attributed national figures or a completed calibration dataset.
- For illustration, FdI's raw share differs by about **-0.122 percentage points**
  (Senato minus Camera), while the unweighted municipality difference's
  nearest-index 10th/90th percentiles are about **-2.083/+1.941 points**. List coverage, vote
  volume, candidate effects and attribution must be reconciled before interpreting
  that spread as chamber behaviour or future residual variance.

The [Ministry's age-change explanation](https://www.interno.gov.it/it/notizie/18enni-chiamati-voto-prima-volta-eleggere-senato-repubblica)
identifies 2022's common 18+ electorate after constitutional law 1/2021. Its
[2018 election instructions](https://www.interno.gov.it/it/notizie/elezioni-2018-come-vota-corpo-elettorale-tessera-elettorale)
state the former Camera 18+/Senato 25+ bases. Therefore 2022 is the primary paired
baseline reviewed here; older differences confound electorate age and chamber
behaviour. Scaling by population alone cannot recover unobserved age-specific
voting choices.

One comparable election's municipality pairs do not supply repeated independent
national forecasting errors. Geographic share correlation must not be reported
as a calibrated future-shock correlation. The reform also changes the old
uninominal candidate/ballot context, limiting transfer of 2022 contrasts. No
numerical future correlation, mean gap or residual variance was estimated here.

### Downstream requirements and scope

Use `paired-chamber-votes-v1`, separately versioned parameter/profiles and sources,
age/geography/coverage assumptions, seeds and supplied/common/model-derived
reference provenance. R26-05 must reserve those contracts. R26-19 must acquire
and prepare paired Senate history, reconcile R26-02 attribution, fit or explicitly
label covariance/contrast assumptions, implement the precedence table and scope
rules, preserve marginal uncertainty and validate paired sensitivity/replay.
R26-20 must report reference provenance and joint-bonus dependence assumptions.

Required fixtures cover common-only, one/both explicit chamber references,
chamber-only input without a common anchor, partial lists, conflicting scopes,
mean-mode uncertainty, normalization/abstention and coverage. Keep constructed
legal fixtures for different/linked/unlinked winners, one chamber at/below 42%,
single-chamber renewal and incomplete joint inputs independent of model fitting.
Separate Monte Carlo uncertainty within profiles from dependence sensitivity;
do not claim a universal direction of bias or measured certainty for assumptions.

The root plan and specification now include paired Senate-history preparation in
the current R26-19 scope. Post-migration work reviews/extends historical coverage
instead of planning a duplicate model; approval of R26-04 is not delivery of that
data pipeline. Updated dependent contracts, generation and reporting directives.
R26-02/03 contracts and the statutory transcription remain unchanged; all
application implementation/acceptance steps remain open.

Verification of this documentation update:

- All 77 local file/section links across the root plan, four reform documents
  and this entry resolved. Confirmed 22 ordered steps with only R26-01, R26-01a
  and R26-02/03/04 complete; R26-05 depends on those approved decisions. All
  N01-N29 and I01-I09 entries remain present.
- Checked common-only, one/both chamber references and chamber-only input
  without a common anchor are documented, together with same-scope precedence,
  geographic specificity and continuing mean-mode uncertainty. No obsolete
  pending-R26-04 directive remains in the active documents.
- R26-02/03 contracts and the statutory transcription are unchanged. Exactly
  five Markdown files changed. Rechecked all three temporary source ZIPs'
  fingerprints and readable directory entries; no source data were imported
  into the repository or fitted in this update.
- `git diff --check` passed. Application tests/build/browser checks and
  stochastic calibration experiments were not run for this documentation-only
  update. The contract/precedence checks above do not claim runtime behavior.
