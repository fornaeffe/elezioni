## 2026-10-17 R26-03: approved historical-pattern preference model

Luca approved fitting a **synthetic preference model informed by historical
patterns**, with supplied valid counts taking precedence. Recorded the active
[model, calibration and output contract](../../politics-reform-2026-modelling.md#preferences-r26-03)
and marked the decision step complete. R26-04 remains the next discussion.
This approval selects the model family and validation obligations; no parameters
were fitted, generator implemented or source data changed.

### Decision and alternatives

The approved generator separates effective preference intensity, candidate
concentration and territorial variation. It uses partial pooling when evidence
is sparse, shared underlying strength across a person's candidatures, and local
variation. New candidates without individual evidence receive exchangeable
synthetic strengths informed by the historical distribution. Historical names
and their measured support are not automatically mapped onto new people.

Generate counts of valid combinations of zero to three distinct eligible
preferences, excluding capolista and applying gender constraints. The resulting
candidate counts have a constructive realizability trace. The central profile
uses maximum entropy over valid combinations subject to selected intensity and
candidate-strength constraints; this is an explicit modelling convention.
Aggregate source counts do not identify individual ballot combinations or the
zero/one/two/three-preference mix. Alternative multiplicity/pairing assumptions,
especially the regional source's unobserved third preference, require sensitivity
checks. Preference-list ties still use statutory presentation order.

The convention preserves R26-02: attributed totals support effective preference
intensity without reconstruction of bonus-only counts. The zero-preference
category is unspecified composition. When direct/bonus-only components are
explicit, only direct list votes supply the preference budget. Supplied valid
preference counts take precedence; missing counts are never silently zero.

| Alternative | Decision and rationale |
| --- | --- |
| Historically informed synthetic preferences | Selected: supports complete candidate-dependent scrutiny while learning concentration/territorial patterns and exposing transfer assumptions. |
| Supplied counts only | Retained as the preferred input when available; insufficient by itself for automated future-election generation. |
| Defer preferences and restrict outputs | Honest fallback for incomplete inputs, but not the chosen primary model; candidate resolution/recovery cannot be claimed complete. |
| Uniform preferences or independent random rankings | Useful diagnostic baselines; discard observed concentration and shared candidate strength. |
| Copy historical candidate profiles directly | Not selected: list lengths, candidate identities, campaigns, protected capolista and electoral systems differ. |

### Preference source audit and evidence

The assessment located preference data already downloaded locally. Repeated the
read-only cache checks when recording approval, using `readRDS()` with R 4.5.1;
the `.RData` suffix does not make these workspace images:

| Source | Audited coverage/content | SHA-256 |
| --- | --- | --- |
| Raw ER 2024 preferences (`dati/raw/regionali 2024Regionali_EmiliaRomagna_2024_Preferenze.csv.RData`; no longer retained) | 325,133 candidate-section rows; 4,529 distinct municipality/section keys, 330 municipalities, nine constituencies, 11 list labels and 547 candidate slots in 97 list-constituency pairs. No duplicate candidate-section keys were found in the assessment. | `0518457791af919962d448a3e0604cb49679e22421266db0083e5749ee739631` |
| Matching raw scrutiny (`dati/raw/regionali 2024Regionali_EmiliaRomagna_2024_Scrutini.csv.RData`; no longer retained) | 49,819 rows; summed list votes 1,494,375. Two blank-list zero-vote placeholders have no matching preference slate; do not interpret them as missing positive support. | `a4fbc2f7c1e3309a287e0ef7d567911670d45aef5d5f40b2c959dd2a02ec01d6` |
| Prepared bundle (`dati/dati.RData`; no longer retained), `preferenze` | 22,050 municipal candidate-preference rows, all ER regional 2024; lower resolution than the raw section cache. | `1d5b7fbde447bdacf54574ac638e8a004dcc637cf0c4d46ed6e332f5f2526fdf` |

Reproduce the concentration diagnostic by aggregating raw `PREFERENZE` over
`CIRCOSCRIZIONE, LISTA, NUM_CANDIDATO`, then dividing each list-constituency's
maximum candidate count by its preference total. Across the 97 pairs, the
unweighted minimum/median/maximum leading share is **18.3% / 38.6% / 81.3%**.
Lists have three to eleven candidates; these summaries are diagnostics, not
national parameters. The 547 entries identify candidatures, not 547 distinct
people or independent statistical observations.

The cache sums to **644,626 preference marks**, or **0.4313683 marks per list
vote**. That is not a measured fraction of voters expressing preferences: the
source allows two. The [regional metadata and voting rules](https://statistica.regione.emilia-romagna.it/dati/rappresentazioni-grafiche-dati/visualizzazione-elezioni-regionali-2024-emilia-romagna-risultati/visualizzazione-elezioni-regionali-2024-emilia-romagna-risultati-metadati)
confirm the section/municipality scope and double preference with different
genders. The raw candidate fields include list position and names, but no gender;
join verified rosters before gender-based calibration, rather than inferring it.

The later [regional publication, table 4.1, PDF page 74](https://statistica.regione.emilia-romagna.it/studi-analisi/2025/elezioni-regionali-2024-emilia-romagna-pubblicazioni/documenti-allegati/pubblicazione_elet_2024_web.pdf/@@download/file)
reports **644,089** marks, 537 fewer than the cache. Its reading notes distinguish
official electoral-office records from ministerial election-night data, and
table 4.1 specifically identifies its preference totals as ministerial. This
establishes a version/reconciliation requirement, not the cause of every
difference or a completed correction. Preserve source status and provenance.

Additional sources checked during the assessment, not added to the repository:

- [Bologna municipal 2021, PD candidate preferences](https://opendata.comune.bologna.it/explore/dataset/elezioni-amministrative-2021-preferenze-lista-partito-democratico-lepore-sindaco/):
  the public API returned 445 section records with candidate columns. Other
  municipal lists were located in the same catalogue.
- [Bologna European 2024 candidate preferences](https://opendata.comune.bologna.it/explore/dataset/elezioni-europee-2024-preferenze-ai-candidati-suddivisi-per-sezione/):
  the public API returned 73,870 records with section, list, candidate number/name
  and valid preference votes. European data can inform three-preference
  sensitivity, but do not resolve joint-ballot identification from marginals.
- [Bari European 2024, official Puglia catalogue](https://dati.puglia.it/ckan/dataset/elezioni-europee-2024):
  verified catalogue metadata/resources for candidate preferences and list votes
  by section. File contents were not validated in this assessment.

Some browser fetches failed while direct public-API requests succeeded. The
Ministry's European ZIP URL returned content that was not a ZIP; no national
preference extract was claimed or imported from it. Geographic expansion,
cleaning, source reconciliation and calibration remain R26-19 work. A complete
regional cache and large section row count do not establish national coverage
or an equally large independent sample.

### Downstream requirements and supported claims

Use model ID `preferences-empirical-synthetic-v1` with separately versioned fitted
parameters/profiles, sources, seeds and supplied/generated provenance. Before
production integration, reconcile totals/rosters, adjust list length and the
capolista exclusion, validate held-out provinces and later elections/regions,
and run paired sensitivity with votes/candidatures fixed. Account for repeated
candidates and clustered sections. Declare diagnostic/acceptance criteria and
seed budgets before inspecting stochastic results.

Propagate alternative profiles through multiple elections, bonus precedence,
candidate exhaustion and recovery. Separate vote-based seat entitlements from
final filled seats and candidate/recovery outcomes conditional on preferences.
Synthetic candidate probabilities are conditional scenario outputs; profile
sensitivity is not a calibrated confidence interval or a forecast for real names.
New data, actual reform results, candidate evidence or material validation
failures can justify revisiting the model.

Updated R26-05 contracts, R26-16 fixture independence, R26-19 fitting/generation,
R26-20 reporting and the specification's scope references. The selected model
does not settle Camera-Senato vote dependence, change legal ranking rules or
simulate invalid-ballot adjudication. R26-04 remains pending; all application
implementation and acceptance steps remain open.

Verification of this documentation update:

- All 76 local file/section links across the root plan, four reform documents
  and this entry resolved. Confirmed 22 ordered steps with only R26-01, R26-01a,
  R26-02 and R26-03 complete, and all N01-N29/I01-I09 entries retained.
- The R26-02 contract, pending R26-04 discussion section and statutory
  transcription are unchanged. Exactly five Markdown files changed.
- Read-only R checks reproduced the cache coverage, preference/list-vote sums
  and leading-share diagnostics. All three recorded source hashes remained
  unchanged. No fitting, stochastic model experiment or application test ran.
- `git diff --check` passed. Application build/browser checks were not run for
  this documentation-only update; calibration and implementation acceptance
  remain assigned to their later steps.
