# R26-19 scenarios, preparation and generation

Use this guide for generation work. Legal scrutiny stays in the stage modules;
the [modelling decisions](politics-reform-2026-modelling.md) define semantics.
Use the [validation protocol](politics-reform-2026-generation-protocol.md) for
diagnostics and [migration history](migration-history.md#2026-10-19-r26-19-scenarios-historical-preparation-and-generation)
for measured evidence, versions and limitations. For worker/UI/export use the [application guide](politics-reform-2026-application.md).

## Task map

| Task | Implementation |
| --- | --- |
| Input/model/scenario contracts | `web/src/lib/politics/reform-2026/generation-types.ts` |
| Strict validation, serialization, defaults, explicit legacy conversion | `generation-scenario.ts` in the same directory |
| Prepare references, then generate one paired simulation | `generation.ts` |
| Correspondences, common anchors, geographic overrides and paired shocks | `paired-vote-generation.ts` |
| Filed seven-slot lists, bonus prefixes, templates and compatible reuse | `reform-candidate-generation.ts` |
| Valid combinations, maximum entropy and shared person strength | `preference-generation.ts` |
| Entity-addressed streams and aggregate integer sampling | `generation-math.ts` |
| Pinned acquisition and offline preparation | `web/scripts/data-preparation/reform-cli.ts`, `reform-models.ts` |
| Historical attribution and geography reconciliation | `reform-history.ts` in the preparation directory |
| Preference source joins, pooling and held-out diagnostics | `reform-preferences.ts`, `ministry-workbook.ts` |
| Paired sensitivity experiments | `reform-validation.ts` |

## Clean-clone preparation

Requires Node 24 and npm. From `web/`:

```powershell
npm ci
npm run data:download
npm run data:reform:download
npm run data:reform:generate -- --publish
npm run data:reform:check
npm run data:reform:validate
```

Geography uses the existing [shared manifest and workflow](../data/README.md).
Historical/model sources use
[`data/reform-model-source-manifest.json`](../data/reform-model-source-manifest.json).
Downloads verify existing caches and fetched bytes; changed upstream bytes fail.
Generation/fitting are offline and read no R caches. `--sources DIR` and
`--output DIR` support empty-cache reproduction. Raw inputs remain in
`data/sources/`; detailed reconciliation, fits and experiment reports remain in
`data/derived/`. No downloads or fitting occur in the browser.

Without `--publish`, generation only writes immutable derived versions. With it,
the command also produces the ignored gzip runtime snapshot under
`web/static/data/reform-2026/`, tracked compact models/defaults under
`web/src/lib/politics/reform-2026/data/`, and the generated catalog pointer.
Snapshot, model, parameters, profiles and defaults have explicit identities.
The catalog pins compressed/uncompressed hashes and the defaults fingerprint.
Regenerate defaults through this command; never edit them by hand.

`data:reform:check` requires the actual production bundle, verifies checksums,
validates its defaults/models and runs generation through composed scrutiny.
It fails on missing data or non-computable stages. Development unit tests may
clearly skip the production-data test if the ignored bundle is absent; the
release command never substitutes a debug snapshot or accepts that skip.
Whole-application acceptance and performance remain R26-21.

## Source semantics

- 2022 Camera/Senato use the common 18+ electorate. Reconstruct candidate-only
  attribution at the **historical uninominal** level under C77/S16 as amended by
  [law 165/2017](https://www.normattiva.it/uri-res/N2Ls?urn:nir:stato:legge:2017-11-03;165=).
  Municipal reconstruction preserves both list/college and municipal ballot
  margins. Its seeded rounding is a preparation assumption, not an official
  municipal attribution. The fixed namespaces are
  `r26-history-attribution-v1` and `r26-history-municipal-reconstruction-v1`.
- Join municipalities through the pinned geographic catalog, explicit aliases
  and college membership when names are ambiguous. Split-municipality support
  is estimated from census weights, preserving municipal electors and list votes.
- VdA and Senate TAA archives contain candidate ballot-group totals without a
  recoverable member-list decomposition. Preserve these groups as distinct
  special-only scenario references. Do not fabricate their component votes.
  The Camera TAA proportional view uses the same special ballots once.
- ER fitting uses the later coherent regional municipal preference/section-vote
  publication. Link recorded gender/birth rosters through verified names and
  filed slots, accounting for changed list numbers and two spelling variants.
  Retain the 537-mark difference from the ministerial version in the audit.
- European 2024 municipal workbooks cover five constituencies and include
  recorded gender/birth metadata. Check complete rosters, list budgets and
  geographic footprints. Source zero-vote observations remain known zeros;
  only positive-ballot observations enter intensity fitting.
- Older Senate history is acquired/audited with its former 25+ electorate and
  excluded from central fitting. A single paired election does not estimate
  future national forecast-error covariance. Shock scales, dependence and
  historical-gap shrinkage are labelled assumptions with sensitivity profiles.

## Runtime use and replay

```ts
const prepared = prepareReformGeneration(scenario, snapshot);
const { input, trace } = generateReformSimulation(prepared, simulationId);
const result = runPoliticsReformScrutiny(input);
```

All three boundaries are browser-worker compatible. Reuse prepared references
across simulation IDs/chunks. No global RNG or Node dependency enters runtime
generation. Persist the full scenario, snapshot/model/catalog identities, input
provenance, trace and worker result through the R26-20 export contract. Resolve
the catalog's relative data path against the configured project base.

Generated list votes are final attributed totals with unspecified ballot
composition. Supplied component votes remain explicit and are attributed once
by the legal core. Preferences use direct votes for component inputs and effective
intensity for final totals. Their combination certificate includes the empty
subset, excludes capolista and enforces mixed genders for multiple preferences.
Supplied counts or explicit missing preferences take precedence. Override Camera
TAA special ballots, rather than its redundant proportional view.

Scenario shares are valid-vote fractions. Correspondence factors normalize per
historical list; many-to-one aggregation and unmapped-to-abstention semantics are
preserved. Common/per-chamber references resolve within each scope; national,
region, province and municipality scopes apply in that order. Geographic
reconciliation preserves local abstention and structural filing zeros. Infeasible
support is an error. Fully supplied inconsistent totals are normalized with a
trace/warning. Supplied mean references use the projection date and retain
election-day uncertainty.

Defaults use the eight leading ordinary lists, retained special-territory lists
and ballot groups, and historical candidate-link coalition metadata. Ordinary
filing and reciprocal bonus links are explicit editable future assumptions.
Retaining a special-territory list does not automatically assert protected
minority status. Bonus affiliations come from filed ordinary prefixes; they are
not proportional coalition seat quotas. Templates can pin names, birth dates,
gender, availability and compatible shared identities. Special templates can
declare independent affiliation. Conflicting or unmatched templates fail.

The reform parser rejects unknown versions/models. The old parser rejects reform
envelopes instead of restoring legacy defaults. `convertLegacyScenarioToReform`
requires an explicit list mapping and source algorithm, and records conversion
assumptions. Legacy candidate templates without gender and unreconciled manual
historical mappings require separate resolution. The old snapshot, algorithm,
generator and random sequence remain separate.

## Interpretation limits

Fitted preference intensity/concentration/local dispersion inform exchangeable
synthetic candidates; historical popularity is not assigned to a new name.
Persistence partitions a fixed fitted variance budget, so its sensitivity does
not silently alter marginal concentration. Valid combinations use a central
maximum-entropy convention; aggregate observations do not identify pairings or
triple-preference behavior. Candidate outcomes are conditional model results,
and profile spread is not a calibrated confidence interval.

N18's cap-group geographic-capacity blocker and ordinary arithmetic/no-progress
blockers remain explicit. Ordinary C83-bis(1)(b)(3) independently corrects list
surpluses/deficits against the communicated targets, retaining the decree-based
initial quotas. Never rescale legal endowments, reroll a failed simulation or
omit it from the requested-run
denominator. The application distinguishes non-computability, vote-based
entitlements, filled/unfilled seats and preference-dependent recovery. Both
algorithms are available in the worker/UI; R26-21 acceptance remains separate.
