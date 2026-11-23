## 2026-10-19: R26-19 scenarios, historical preparation and generation

Implemented the approved R26-02/03/04 generation boundary after verifying the
existing R26-18 composition and registry. The production selector/worker remains
unavailable for the reform until R26-20. The
[generation guide](../../politics-reform-2026-generation.md) owns the task map, commands,
contracts and replay instructions; the separate
[protocol](../../politics-reform-2026-generation-protocol.md) owns diagnostic gates and
replication budgets.

### Preparation and evidence

- Added a pinned eight-source historical/model manifest and non-interactive
  `data:reform:download`, `data:reform:generate`, `data:reform:check` and
  `data:reform:validate` commands on the existing Node/TypeScript acquisition,
  checksumming, geography and immutable-output foundation. Preparation reads no
  legacy R cache and performs no fitting/downloads in the browser.
- The 2022 Camera/Senato domestic sources independently resolve to all **7,904
  municipalities** and **46,120,143 electors** in each chamber. The source list
  fields are direct votes. Old-law uninominal attribution adds **1,027,135**
  candidate-only Camera ballots to **27,069,655** direct ordinary list votes,
  giving **28,096,790**. Senate ordinary figures are **26,601,426 + 963,877 =
  27,565,303**. Separate special archives contribute **53,746 / 557,313** valid
  ballots. Municipal reconstruction conserves both each list's historical
  uninominal total and each municipality's candidate-only margin. Independent
  per-list rounding was replaced with controlled row/column reconciliation.
- Geography joins reject ambiguity/missing rows. Explicit aliases cover changed
  and bilingual names; original college membership resolves ambiguous same-region
  names. Reform submunicipal support uses labelled census-weight estimates.
  Special candidate ballot groups retain their own identities, without invented
  member-list decomposition. Source candidate links supply historical coalition
  metadata for defaults. Older Senate 2018 data are acquired and audited but
  excluded from the central fit because of the 25+ electorate.
- Reconciled ER 2024's **547 roster slots / 532 recorded people**, with verified
  gender and birth metadata. List numbers differ between releases, so joins use
  exact names to establish list identity, then verify filed slots. This also
  resolves the two Rimini spelling/name variants without inferring gender.
  The later regional publication has **644,089** marks against the ministerial
  cache's **644,626**, with identical **1,494,375** list votes. The later coherent
  preference/list release is used for fitting; the **-537** difference and the
  unidentified causes remain in the audit.
- Expanded preferences to all five European 2024 municipal workbooks, with
  recorded roster gender/birth fields and **15,588,458** marks. The flat XLSX
  reader iterates rows without a million-cell object graph, supports only the
  pinned source encoding and rejects formulas/unsupported cells. Complete
  roster and list-budget/footprint checks pass. The fit uses **93,659 positive
  ballot observations**, with **zero invalid-budget/roster exclusions**; known
  zero-ballot observations remain in source reconciliation rather than becoming
  invented intensity observations.
- Equal-election weighting, partial pooling, roster concentration and separate
  within-person local dispersion yield baseline effective intensity
  **0.5501554059**, log-strength sigma **1.5971061015** and local sigma
  **0.3954103197**. Twenty-nine province/region hold-outs exclude all training
  observations sharing the held-out candidate identities. Intensity MAE
  **0.1957805028** and pooled leading-share MAE **0.0592449574** pass the
  predeclared 0.25 transfer-diagnostic gates. Zero/tie and dispersion errors are
  also retained by fold. These are distributional transfer diagnostics, not
  calibration of future named-candidate forecasts.

### Runtime contracts and corrections

- Added strict versioned reform scenario/snapshot/model contracts, independent
  fitted-parameter/profile/default identities, typed persistence and explicit
  opt-in legacy conversion. The legacy parser rejects reform/unknown-algorithm
  envelopes and unsupported future versions instead of silently restoring old-law
  defaults. Existing legacy scenarios, inputs, generator and R-parity behavior
  remain available. Unreconciled manual historical mappings and gender-less old
  candidate templates require explicit resolution during conversion.
- Correspondences normalize splits per historical list, aggregate many-to-one
  mappings and send unmapped support to abstention. Common anchors and explicit
  chamber references follow the approved same-scope precedence, then broader to
  narrower geography. IPF preserves local abstention and filed-support zeros;
  incompatible targets fail explicitly. Dates retain election-day mean-mode
  uncertainty. References, scope, normalizations, source/model versions and seeds
  are transferable and reproducible.
- Vote shocks split a marginal variance budget among shared and chamber-specific
  components rather than adding Senate noise. Future daily scales and joint
  correlations are explicit assumptions; one 2022 paired election cannot identify
  future national forecast covariance. Weaker/central/stronger dependence, gap
  sensitivity and identical/independent benchmarks remain distinguishable.
- Candidate generation constructs seven alternating-gender positions, ordinary
  bonus prefixes, coalition bonus affiliations, special links and compatible
  same-list multiple candidacies, respecting filed templates. Generated identities
  are distinct across chambers; shared ordinary identities retain their underlying
  preference strength. Supplied preferences or explicit missing counts take
  precedence. Supplied component inputs use only direct votes for preferences;
  Camera TAA certificates partition the actual special ballot budget once.
- Synthetic preferences use maximum entropy over valid zero-to-three-name
  combinations and aggregate binomial/multinomial sampling, with constructive
  certificates. Capolista is excluded and multiple preferences contain both
  genders. The central persistence share is derived from the fitted variance
  components. Review corrected a confound in the initial implementation:
  persistence now partitions a fixed fitted total variance instead of also
  changing marginal concentration. A dedicated 1,000-seed variance check guards it.
- Entity-addressed streams isolate shared/chamber vote components, candidates,
  person/local preference strengths, combination counts and legal draws. R26-18's
  default attribution stream is now isolated from downstream legal draws too.
  Complete representation-equivalence checks cover 30/42/45/50/60 majority cases,
  special ballots and a tied coalition attribution with identical downstream
  results. This establishes tested arithmetic/representation equivalence, not
  empirical absence of all unobserved behavioral bias.

### Reproduction, limits and verification

- Accepted catalog versions: snapshot **`r26-generation-v1-c5c4be9fafb5`**, models
  **`r26-models-v1-f71a72020aaa`**, preference parameters
  **`r26-preferences-fit-v1-cf0024e3b8e3`**, paired parameters
  **`r26-paired-2022-assumptions-v1-31896d7dffaa`**, defaults
  **`r26-defaults-v1-f204e831aad3`**. Runtime size is **992,943 compressed bytes /
  13,870,007 uncompressed bytes**. The generated catalog records both hashes and
  the defaults fingerprint. Only the current runtime gzip is present in static
  assets; exploratory versions were removed from runtime/source publication paths.
- Acquired all geography/model sources into an initially empty
  `data/derived/r26-clean-sources` directory, then generated into a separate empty
  output directory with the same commands. This checks cache-miss reproduction
  locally; it is not a hosted Linux/Actions measurement or a fresh `npm ci` clone.
- The first artificial twenty-region preference-conditioning experiment hit
  the already documented N18 capacity blocker. Its 700 non-complete runs were
  rejected as evidence of complete legal propagation, without changing the law
  or rescaling endowments. The corrected conditioning uses the independently
  worked complete election fixture, and its TAA budget is the actual special
  ballots rather than the redundant proportional view. Detailed failed/final
  experiments remain separate in `data/derived/`.
- Browser execution initially exposed Node 24's JSON import-attribute requirement
  in the test runner; added `with { type: 'json' }` to the tracked seat import.
  No UI route or application algorithm selector was activated.
- N18 non-computability remains a legal/input boundary for some generated
  scenarios. R26-20 must report it and retain the requested-run denominator,
  without rerolling or silently dropping failures. Synthetic probabilities are
  conditional; pairing/triple behavior and future chamber covariance remain
  assumptions. Whole-application acceptance, performance and publication remain
  R26-21 and the final migration gate.

Final verification on Windows x64, Node **24.20.0**:

- Empty-cache acquisition/generation produced **byte-identical snapshot, model
  and default JSON** against normal preparation. The actual production check
  verifies gzip/uncompressed/default fingerprints and completes all **41 legal
  stage entries**, filling **588/588 domestic seats**, with no unfilled,
  unresolved or non-computable output for the default seed.
- `data:reform:validate` on the final model ran **100 paired full legal runs for
  each of seven preference profiles**. All **700** runs filled 588 seats, with
  zero entitlement changes and zero non-computable stages. Compared with central,
  mean changed candidate identities were **5.25 concentrated, 6.20 dispersed,
  178.71 weak persistence, 6.97 strong persistence, 4.43 no triples and 4.51 enhanced
  triples**. Mean recovery activity varied from 97.50 to 97.54 operations. These
  are conditional worked-fixture diagnostics, not real-candidate probabilities.
- Paired vote diagnostics used **1,000 seeds per profile/benchmark**. Measured
  normalized correlations were **0.4989 / 0.8497 / 0.9802** for assumed
  **0.50 / 0.85 / 0.98** dependence. Marginal variance/mean gates and integer
  conservation passed. In the constructed near-42% example, conditional joint-42
  frequencies were **0.283 / 0.317 / 0.322**, with Monte Carlo standard errors
  about **0.014–0.015**. Zero/doubled gap frequencies were **0.415 / 0.173**;
  identical/independent benchmarks were **0.502 / 0.254**. This is model sensitivity,
  not a measured national bonus forecast or a confidence interval. The final
  versioned detailed report is
  `data/derived/r26-models-v1-f71a72020aaa-validation-v2.json`.
- `npm run check:data` and `npm run check` passed with **zero diagnostics**.
  `npm run test` passed **815 tests in 52 files**, without skips, in **7.83 s**;
  this includes old-law golden, registry and production-data checks. The static
  build passed in about **4.06 s**. Build and e2e ran sequentially.
- Final Chromium suite passed **10/10**, including Node/browser-worker replay of
  generated scenarios, direct registry execution and I10/I11 recovery evidence.
  An earlier full browser run had a single landing-heading timeout, which passed
  in isolation and on subsequent full runs; no unrelated UI change was made.
- R26-19 is complete within these explicit model/legal limits. R26-20/21 remain
  open. No commit, deployment, hosted-runner acceptance or whole-migration
  completion is claimed.
