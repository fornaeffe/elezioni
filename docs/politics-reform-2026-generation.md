# R26-19 scenarios, preparation and generation

This guide describes the installed generator. Its approved replacement is
specified by the [fast-model production plan](vote-generation-production-plan.md).
Use that plan for FV implementation, browser refitting and research cleanup;
update this guide at cutover. Existing commands below do not yet build the new
fast-model production assets.

Use this guide for generation work. Legal scrutiny stays in the stage modules;
the [modelling decisions](politics-reform-2026-modelling.md) define semantics.
Use the [validation protocol](politics-reform-2026-generation-protocol.md) for
diagnostics and [migration history](migration-history.md#2026-10-19-r26-19-scenarios-historical-preparation-and-generation)
for measured evidence, versions and limitations. For worker/UI/export use the [application guide](politics-reform-2026-application.md).
For common/chamber editing, v1/v2 presentation metadata compatibility and planner
previews, use the [scenario editor guide](politics-reform-2026-editor.md).

## Task map

| Task | Implementation |
| --- | --- |
| Input/model/scenario contracts | `web/src/lib/politics/reform-2026/generation-types.ts` |
| Strict validation, serialization, defaults, explicit legacy conversion | `generation-scenario.ts` in the same directory |
| Prepare references, then generate one paired simulation | `generation.ts` |
| Correspondences, common anchors, geographic overrides and paired shocks | `paired-vote-generation.ts` |
| Bonus reference allocations, supplied candidates, witness prefixes and ordinary reuse | [Candidate generation guide](politics-reform-2026-candidates.md) |
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
For whole-application acceptance and performance use the
[R26-21 procedure](politics-reform-2026-acceptance.md).

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
support is an error. Partial overrides fix only supplied references (including
chamber references derived from common anchors); unspecified lists retain their
local relative proportions while their aggregate shares adjust to the remaining
geographic capacity. They do not receive proportional national targets: a
disconnected special ballot cannot exchange votes with ordinary lists. A
reference above the valid-vote capacity of its filed geography reports the
requested share and maximum available share. Fully supplied inconsistent totals
are normalized with a trace/warning. Supplied mean references use the projection
date and retain election-day uncertainty.

Defaults use the eight leading ordinary lists, retained special-territory lists
and ballot groups, and historical candidate-link coalition metadata. Ordinary
filing and reciprocal bonus links are explicit editable future assumptions.
Retaining a special-territory list does not automatically assert protected
minority status. New defaults select bonus candidatures proportionally to frozen
reference support, constrained by supplied people, ordinary prefixes and gender;
see the [candidate policy](politics-reform-2026-candidates.md) for rounding,
explicit bonus declarations, multiple ordinary candidacies and archive replay.
These candidate affiliations are not legal coalition-member seat entitlements.
Templates can pin names, birth dates,
gender, availability and compatible shared identities. Special templates can
declare independent affiliation. Conflicting or unmatched templates fail.

The reform parser rejects unknown versions/models. The old parser rejects reform
envelopes instead of restoring legacy defaults. `convertLegacyScenarioToReform`
requires an explicit list mapping and source algorithm, and records conversion
assumptions. Legacy candidate templates without gender and unreconciled manual
historical mappings require separate resolution. The old snapshot, algorithm,
generator and random sequence remain separate.

## Filed lists without local history

Future filing enables a list to receive votes; it does not currently supply a
meaningful territorial prior where its historical correspondence has no rows.
Historical absence and an observed zero both project to zero local support.
For a positive explicit national reference, `reconcileReferences` seeds such
filed cells with `1e-12` and fits the aggregate multiplicatively. This numerical
seed is not a territorial extrapolation model. Existing positive support can
absorb the national adjustment while the other municipalities remain negligible.
Log-odds drift and integer municipal apportionment can therefore still produce
zero votes throughout a future filed region. College aggregation conserves
those municipal votes; it does not create the missing support.

The 26 October 2026 manual scenario maps `lista-3` (Sud chiama Nord) only to its
2022 namesake and supplies a 1.4% common national reference, with no local or
poll-derived proxy. Both pinned ministerial archives have no Sud chiama Nord
row in any of Molise's 136 municipalities; Basilicata also has no rows.
Future Camera/Senato slates are filed in both regions. The resolved Camera
national anchor is 1.313881%, but prepared Molise valid-vote shares remain about
$1.70\times10^{-12}$ to $2.98\times10^{-12}$. Simulation 311's drifted normalized
Molise weights represent about 0.000471 votes in total (largest municipality
0.000107); integer apportionment awards zero in every municipality before
college aggregation. This is a missing territorial-support model, rather than
a rejected filing or lost college votes. A national aggregate reference alone
does not identify a new local profile.

Luca requested reconstruction, not an automatic support change. A correction
must specify meaningful local reference shares or an explicit, sourced/labelled
territorial proxy where historical support is absent; a larger arbitrary
epsilon is not a reviewed estimate. Keep future unfiled cells at zero, preserve
turnout and reference precedence, and test national/local reconciliation. No
support-model or scenario correction has yet been selected or applied.
The N18 positive-seat zero-vote blocker remains; the sole-college bypass was
explicitly rejected. The later approved I17 legal workaround excludes zero-vote
recipients only in Camera cap-path N16 corrective transfers, preventing the
observed eight failures without changing this generation model. Other zero-vote
seat cases retain their existing treatment pending separate review.
Detailed input/source/replay evidence is in migration
history and `data/derived/r26-manual-tests/scn-molise-trace.json`.

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
initial quotas. Apply scoped I14 initial list ranking passes when one pass
cannot fill a college; warnings identify provisional repeated-pass outcomes.
Empty pools and other unchanged no-progress conditions remain non-computable.
Never rescale legal endowments, reroll a failed simulation or omit it from the
requested-run denominator. The application distinguishes non-computability, vote-based
entitlements, filled/unfilled seats and preference-dependent recovery. Both
algorithms are available in the worker/UI; application acceptance is documented
in the [R26-21 procedure](politics-reform-2026-acceptance.md).
