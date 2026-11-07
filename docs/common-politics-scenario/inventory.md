# CP-01 — Behavior and input inventory

Baseline: 7 November 2026, source revision
`a700d19b04272f39fad17ddd3ceb9e48048ea623`. This inventories the existing
implementation against the approved [target](guidelines.md); it does not define
the CP-02 schema or implement the merger. Target paths below use the guidelines'
provisional vocabulary. They specify ownership, not an old-file converter.

| Implementing | Read |
| --- | --- |
| Types, parsing, edit operations (CP-02) | Field maps below; [candidate identities](#candidate-identities-and-law-boundaries) |
| Generation and legal adapters (CP-03) | [Model differences](#generation-boundaries), [comparison evidence](baselines.md) |
| Default packaging (CP-04) | [Defaults and assets](#defaults-and-assets), [observed inventory](default-inventory.json) |
| Files, editor, lifecycle (CP-05–07) | [Editor perimeter](#editor-perimeter-d14), [runtime ownership](#runtime-and-file-ownership) |

## Source entry points

Paths in this section are relative to `web/`. The
[workspace map](../web-workspaces.md#task-map) remains the navigation guide.

| Boundary read | Authoritative entry points |
| --- | --- |
| Routing and combinations | `src/routes/politics/+page.svelte`, `src/lib/workspaces/registry.ts`, `import-routing.ts`, `navigation.ts`, `handoff.ts` |
| Original inputs, validation and controls | `src/lib/core/types.ts`; `src/lib/scenario/politics.ts`, `politics-tickets.ts`, `politics-candidate-slots.ts`; `src/lib/workspaces/LegacyPoliticsWorkspace.svelte` |
| Released v5 inputs and validation | `src/lib/politics/reform-2026/vote-share/{release,forecast-types,correspondences,source-types,source-compiler,scenario}.ts`; `src/lib/politics/reform-2026/{input,generation-types}.ts` |
| Reform editor/lifecycle | `src/lib/politics/reform-2026/ReformWorkspace.svelte`; `vote-share/{FastReformWorkspace,FastScenarioEditor}.svelte`, `{editor-model,fit-client,fit-service,checkpoint}.ts` |
| Generation and legal assembly | `src/lib/politics/{scenario-projection,parameter-preparation,candidate-generation,vote-generation,adapter,simulation-scrutiny,scrutiny}.ts`; `src/lib/politics/reform-2026/{generation,assembly,reform-candidate-generation,legacy-candidate-generation,paired-vote-generation}.ts`; `src/lib/politics/reform-2026/vote-share/{unified-generation,legacy-adapter,legacy-polls,generation,reference-plans}.ts` |
| Defaults/assets | `scripts/workspaces/{defaults,generate}.ts`; `scripts/data-preparation/{fast-share-default,fast-share-future-default,fast-share-release,workspace-snapshots}.ts`; `src/lib/workspaces/{data,snapshots.generated}.ts` |
| Results/persistence | `src/lib/workspaces/{controller.svelte,draft.svelte,result-access}.ts`; `src/lib/core/{result-export,result-store,json-stream,stored-diagnostics}.ts`; `src/lib/workers/{simulation.worker,reform-compute,reform-pool}.ts`; `src/lib/results/` |

## Original `Scenario` field map

Every declared field in `core/types.ts`'s politics `Scenario` and its nested
records is covered here. Regional uses of the same type remain outside the merger.
Percentages below mean 0–100 in the old scenario; the proposed contract uses
explicit fractions. Preserve absence and explicit zero independently.

| Existing field/control | Target ownership and treatment |
| --- | --- |
| `id`, `name`, `electionDate` | Shared scenario identity/name/date; new default gets D04's identity. No name-based relational keys |
| `defaultSource.{kind,electionKind,territory,dataVersion,snapshotId,notes}` | `provenance` plus exact `data` pins; election kind is explicit contract identity. Preserve assumptions/citations, not an old-schema wrapper |
| `globalShareMode = mean` | Mean semantics retained under M01/M02; no fixed-percentage mode |
| `abstentionShare`, `abstentionOverride` | Optional national/common `nonValidReferences`, elector denominator, percentage-to-fraction conversion. False means no override even if a stored display value is nonzero. Old M00 residual calculation is retired |
| `coalitions[].{id,name,color}` | Shared `subjects.coalitions`; derive singleton electoral tickets only inside Rosatellum assembly |
| `lists[].{id,name,color,kind}` | Shared list identity/presentation and residual `other` kind; `Altre liste` keeps its reserved ID/name/grey color and no candidates |
| `lists[].coalition` | Old value is a coalition **name** or null; target is a stable coalition ID or null. Renaming cannot break relationships |
| `lists[].minorityRegion` | Preserve Rosatellum minority declaration in the shared declaration interpreted under its law. Do not equate it blindly to reform's legal-reference-bearing declaration |
| `lists[].filing.{camera,senato}` | Rosatellum law block with chamber/geography namespace. Missing means historical unrestricted behavior in old files; explicit `[]` means filed nowhere. New format requires complete law blocks; no implicit law-switch conversion |
| `lists[].{startingShare,shareOverride}` | Active values become national/common list references of valid votes; false keeps the override absent. CP-04 adopts reform baseline for equivalent defaults; no M00 projected-mean compatibility requirement |
| `listCorrespondences[].{id,futureList,pastElection,pastDate,pastList,factor,source}` | Retire M00 arbitrary correspondence-factor controls and execution. Retain useful source identity/provenance through historical memberships and M01 normalized source compositions, with binary chamber future memberships. No automatic conversion/import of old rows |
| `localShareOverrides[].{id,scope,locationCode,list,startingShare}` | Shared local references with explicit namespace, stable subject ID, common channel and fraction of valid votes. Model-specific precedence is retained; do not invent chamber-specific information absent in this field |
| `candidateGeneration.uninominalToPlurinominalShare` | Rosatellum law-specific reuse parameter, retained verbatim |
| `candidateGeneration.plurinominalCandidacyCountShares` | Rosatellum five probabilities for 1–5 filings; retain all five, normalization and existing sampler |
| `candidateTemplates[].{id,ramo,kind}` | Declared placement ID, chamber and Rosatellum uninominal/plurinominal role |
| `candidateTemplates[].{coalition,uninominalCode,list,plurinominalCode,candidateNumber,minority}` | Rosatellum placement owner/location/ordered position/minority role; null/irrelevant role fields never become empty IDs or slot zero |
| `candidateTemplates[].{candidateName,birthDate}` | Shared declared candidate identity and biography under D09. Old name was written into `CANDIDATO_ID`; old template ID identifies a pin, not necessarily a person. Nullable date and absent gender need the new creation rules, not inferred gender or automatic identity merging |

Original execution settings are outside `Scenario`: `simulations`, `seed`,
`electionDate`, `kind`, `dataVersion`, `scrutinyAlgorithmId` in `SimulationRequest`.
Move authored count/seed/worker preference into `execution`, exact dependencies
into `data`, and selected scrutiny into `selection`. The old runtime's implicit
projection/current date must not replace the new persisted `referenceDate`.

## Released `ReleasedScenario` v5 field map

This includes fields inherited through `FastGenerationScenario` and
`LegalGenerationSettings`, including advanced inputs currently accessible only
through JSON. Old v1–v4 reform archive fields are test evidence, not additional
new-format import obligations.

| Existing field/control | Target ownership and treatment |
| --- | --- |
| `kind`, `schemaVersion`, `algorithmId` | New common format identity plus `selection.scrutiny` exact ID/version; independently versioned model/law blocks |
| `electionDate`, `referenceDate` | Shared dates; retain any distinct model parameter/projection date at its own boundary |
| `seed`, `fitSeed` | Separate visible simulation/fitting seeds in `execution` |
| `renewal` | Shared joint/Camera/Senato renewal; retain declarations for inactive chamber |
| `generation.{model,modelVersion}` | Selected exact generator/version **per law** in `selection.generatorsByLaw`; M01 and M02 both 2.0.0 at baseline |
| `generation.compositions[choice].{fractions,provenance,proxy}` | M01 model block: normalized historical source compositions, including zero weights and explicitly labelled opaque-source proxies. Not fractional future membership or observed internal counts |
| `generation.supplementalPolls[].{id,date,available,end,context,groups,sample,source,enabled}`; `groups[].{label,members,share}` | M01-only partial named observations; retain identity, source/availability, enable state and group structure. Never turn into complete M02 observations or double count |
| `models.{statistical,numerical,references,chambers,rng,generationImplementationId}` | Exact separate probability/numerical/reference/chamber/RNG/implementation identities in versioned model settings/data and executed provenance. Keep hashes distinct from probability-law versions |
| `models.{forecast,forecastScale,profileRegionCorrelation,forecastReference}` | M02 model block; retain registered forecast method, scale, regional correlation and conditional/country-calibrated reference mode, including inactive values |
| `data.{sourceBundleId,fittedStateId,profileStateId,geographyVersion,seatTableVersion,preferenceModelVersion}` | Exact asset/manifest pins. Source/geography/seat/preference dependencies are authored; cached fit/profile identities belong to separately versioned reusable preparation/checkpoint records, not embedded arrays |
| `historySelection.{electionIds,pairedCamera,pairedSenato}` | `history`; preserve M02 selection. M01 executes automatic latest eligible same-date common-18+ pair, not arbitrary M02 history selection |
| `sourceMemberships[choice].{members,provenance}` | Source/question/choice-scoped historical membership edits with provenance; bundled originals remain immutable |
| `componentMap[originalId]` | Whole-component pooling declarations in `history`; retain aliases/provenance separately from genuine pooling. Many original IDs mapped to `historical-rest` are genuine pooling |
| `polls.{selected,added,excluded}` | Selected poll IDs; scenario-owned additions; exclusions with reasons. `added` keeps every `SourcePoll` field listed below |
| `components[].{id,kind,provenance}` | Historical catalogue IDs remain pinned assets; scenario-owned `newComponents` keep stable identity and support assumptions; presentation label/support does not establish empirical identification |
| `components[].fallback.{geographicLogProfile,profileSd,localInnovationSd,nationalInnovationSd}` | Model-specific new-component fallback assumptions and their provenance. Retain even when inactive; not measured support or a replacement for a reference |
| `lists.{camera,senato}[].{id,name,kind,coalitionId,protectedMinority}` | Shared list subjects. Existing chamber variants can differ: use distinct IDs for differing name/affiliation/minority, never pick one chamber silently. `protectedMinority.{regionId,legalReference}` retains its legal meaning |
| `lists[ch][].componentIds` | Explicit binary future membership sets per chamber plus linked-editing metadata; shared component catalogue is not duplicated |
| `lists[ch][].{nationalUncertaintyMultiplier,localUncertaintyMultiplier}` | M02 model settings keyed by list and chamber; dimensionless, absent effective default 1. M01 stores but ignores these settings |
| `coverage[].{chamber,collegeId,listIds}` | Reform law filings with physical-ballot geography; TAA proportional view is derived, not a second ballot. Empty filing is explicit; no list-presence boolean in target |
| `references[].{id,scope,channel,subject,mean,sdPercentagePoints}` | Shared political reference declarations: national/region/province/municipality; common/Camera/Senato; list/component stable ID; mean fraction of valid votes, SD in percentage points |
| `nonValidReferences[].{id,scope,channel,mean,sdPercentagePoints}` | Shared non-valid references with **elector** denominator. Preserve explicit SD zero; missing SD uses D06's persisted default/origin policy when applicable, not zero |
| `candidateGeneration.{reuseProbability,maximumOrdinaryCandidacies,bonusPolicy}` | Reform law settings; retain independent reuse/max-filings policy and `reference-proportional-v1` bonus policy. Not substitutes for Rosatellum parameters |
| `candidateTemplates[].{chamber,role,locationId,ownerId,position,affiliationListId,candidate}` | Reform placements with ordered ordinary/bonus/special roles; identity extracted into shared `candidates`. Null special affiliation means independent; removing one pin does not remove other placements |
| `suppliedVotes[].{chamber,collegeId,votes}` | Law-specific supplied legal input. Preserve ordinary components/direct list + bonus-only subject votes versus attributed list totals; special candidate-only votes/totals versus attributed totals. Keep provenance and counts, including zero |
| `suppliedPreferences[].{chamber,collegeId,listId,preferences}` | Law-specific legal input: explicit `missing.reason` versus complete `counts[{candidateId,count}]`; supplied provenance; validity `adjudicated.reference` or constructive `combinations.ballots[{candidateIds,count}]`. Missing is not zero; all non-head candidates and zero counts remain represented |
| `bonusLinks[].{camera,senato,cameraDeclares,senatoDeclares}` | Reform typed subject endpoints and existing reciprocal declaration evidence. D11 target serializes explicit reciprocal pairs once; derive automatic links/status, retain ineligible declarations, reject overlapping endpoints. Do not carry directional booleans into an inconsistent target pair. Single-chamber execution emits no joint links |
| `preferenceProfileId`, `chamberProfileId` | Registered reform preference/chamber-dependence profile selections with pinned parameters/versions; no general profile-definition editor |
| `presentation.{name,coalitions,lists,components}` | Shared display name/colors and component labels; coalition `chambers` retains declared scope; list entries currently have chamber/id/name/color; component entries have id/name/support. Shared subjects remove redundant display copies without erasing distinct identities |

`polls.added` preserves `SourcePoll`'s `id`, `surveyId`, `context`, `date`,
`fieldworkStart`, `fieldworkEnd`, `availableDate`, `citation`, `sampleSize`,
`questionSampleSize` (nullable), `decidedFraction`, `effectiveSample`,
`designEffect`, `effectiveSamplePolicy`, `errorGroup`, `roundingWidth`,
`original[{label,percentage,kind}]`, `normalized[{label,members,share,other}]`,
`otherDefinition`, `status`, `reasons` and `method`. Poll-option “other” is the
offered complement, distinct from the future residual list and non-valid votes.

## Candidate identities and law boundaries

Reform embedded `Candidate` fields are `id`, `name`, `birthDate`, `gender`,
`availability` (available or unavailable with reason) and
`externalCandidacies[{chamber,kind: domestic|foreign}]`. Move biography and
availability to shared identities; keep external declarations under their law.
Preserve explicit reuse by ID; equal names never merge people. New declarations
require name and selected gender; a missing birth date resolves once to January
1 of election year minus 50, persisted and labelled assumed. Editing the election
date later must not recompute it. No old-template importer is required.

Generated candidates remain runtime objects. Keep Rosatellum's uninominal reuse,
1–5 filing distribution, sampled ordering and documented empty-right-join/NA
edge; keep reform's ordinary/bonus/special generation, gender/age constraints,
reuse maximum, reference-based bonus composition, independent special candidates
and availability/cross-chamber/foreign incompatibilities. A single-chamber run
still validates applicable external declarations. Inactive pins survive filing
and renewal changes; only effective law placements enter legal validation.

## Editor perimeter (D14)

“Deferred” here means **editor only**. The field must remain validatable,
executable and serializable during the merger, including inactive values.

| Capability | Delivery |
| --- | --- |
| Scenario name/date, law, renewal, per-law generator selection, both seeds and random-seed buttons, count/workers | CP-06/07; automatic model preparation and explicit cancellation; no load-default-model button |
| Coalition/list name/color, null coalition, minority, residual category, add/remove | CP-06; shared subjects, no chamber-specific shared-name fields |
| Chamber memberships and linked/unlinked edits; historical source membership and M01 normalized compositions | CP-06; retain explicit assumptions and component conflict checks |
| Whole-component pooling | **CP-06**: observed default still uses it (154 map entries, 126 to `historical-rest`) |
| New-component fallback and list/component references | CP-06; active model semantics and fallback provenance visible |
| Optional basic “Non valido / astensione” mean under lists | CP-06; visible by default; elector denominator explained |
| Advanced uncertainty, national/chamber references and common/chamber local trees | CP-06; D06 defaults, explicit/default origin, complete-family feasibility, stored inactive values |
| Electorate data | CP-06 read-only advanced view; source assumptions and common-18+ policy visible |
| Filing trees; candidate identities, manual slot trees, generation controls; reform bonus links | CP-06; D09/D11, tri-state filing, inactive pins/links, no silent replacement |
| Historical-source/poll add/edit/select/exclude with metadata; model-specific history selection | Post-migration D14 tasks in migration plan; preserve execution policies now |
| Supplied votes/preferences in every retained representation, validity evidence/provenance | Post-migration editor; preserve supplied-input execution now |
| M02 method/scale/correlation/reference mode; model/profile selections; M01 partial named observations | Post-migration editor; retained typed blocks now; read-only exact version identities |
| Embedded raw JSON editor | Retire; structured controls and validated scenario file import/export replace it |
| M00 arbitrary correspondence factors, M00 turnout/unmapped/poll/uncertainty controls | Retire with M00; do not add inactive M00 model block or preserve old distribution law |

## Defaults and assets

[default-inventory.json](default-inventory.json) is the observed, checksummed
pre-merger report. It records both compiled defaults, authored source hashes,
release pointer/asset descriptors, exact list filings/references and model pins.
Reproduce it with the read-only command in [baselines](baselines.md).
It is evidence, never a production default or preparation input.

| Difference at baseline | CP-04 treatment |
| --- | --- |
| Route opens previous law; reform M02 is a separate draft | New default reform/M02/joint; keep per-law M01/M02 saved selections |
| Existing choice-set ID `reform-future-choice-set-20261030-v1`; previous default adds `politics-` | New authored `data/politics-default-scenario.json`, ID `politics-default-v1`, same approved display name |
| Both compile 24 lists/two political coalitions; old compiler uses names, historical projected means and reviewed generic poll display shares | Keep stable shared IDs; choose reform M02 equivalents; do not retain two independent default compilers choosing values |
| M00 non-valid display 55.18%, override false; reform has four explicit non-valid references | Preserve reference absence versus display value; use reform declarations with their chamber values/SDs |
| Previous default has 108 correspondence rows and shared Camera/common local anchors; reform has 34 components, 53 source compositions, two partial M01 polls, 17 political references | Preserve model-specific inputs; no conversion of arbitrary M00 factors, collapse of distinct chamber references, or loss of SDs |
| Previous UI seed `politiche-2027`; reform `fast-share-default-v1` with fit seed `fast-share-default-fit-v1` | Author reform seeds; keep independent visible fields |
| Both election date 2027-09-25; reform reference cutoff 2026-10-19 | Author both dates; remove wall-clock dependence from new authored meaning |
| M00 candidate reuse 0.35 and `[0.875,0.0625,0.025,0.0125,0.025]`; reform reuse 0.2/max 5/reference-proportional bonus | Keep both separate law blocks |
| M02 bounded-mass-pair-field-v2, scale 1.5, region correlation 0.25; reference mode/multipliers implicit | Preserve settings, materialize `conditional` and multipliers 1; use D06 only for new/edited missing uncertainty |
| Existing colors are generated/palette-dependent | Apply D04's approved shared colors in CP-04; color task remains open until then |
| Original 10 runs, legacy 1,000 cap; reform count/worker UI preferences outside scenario | Author 10 and explicit auto workers; durable execution/no Rosatellum cap comes in CP-07 |

Asset dependencies that cannot be reduced to `dataVersion`:

| Asset | Ownership / preservation |
| --- | --- |
| `data/fast-share-future-scenario.json`, `fast-share-default-policy.json`, reviewed legacy comparison polls/compositions and source membership policy | Current authored inputs/provenance; consolidate choices in CP-04, retain sourced assumptions. Pooling is derived using source inventory, so inspecting the authored list JSON alone is insufficient |
| `data/{source-manifest,workspace-source-manifest,fast-share-source-manifest,reform-model-source-manifest}.json`, acquired history/polls/geography/census | Pinned preparation sources; follow `data/README.md`. Bundled originals and source availability remain immutable |
| `static/data/v1/politics-static.json`, descriptor and generated defaults/catalogues | Rosatellum history/electorate/definitive geography; regenerate via complete pipeline, not hand edits |
| `static/data/reform-2026/fast-share/candidate.json` → catalogue → source/chunks, fit, profiles, runtime, scenario | Exact compressed/decoded checksums and versions; runtime has seat/model parameters, geography and conditional electorates; no latest-bundle substitution |
| Reform seat tables/model assets and Senate paired history | Keep legal census totals, labelled reconstruction, preference/chamber parameters and common-18+ adaptation. Bolzano overlay applies to reform only |
| Fit/profile numerical matrices and portable states | Reusable runtime/checkpoint evidence, separately versioned; scenario remains meaningful without a cached fit |
| `test/fixtures/` legal/R inputs, outputs and manifests | Frozen regression evidence only; never runtime/preparation inputs, never regenerate expectations for this merger |

## Generation boundaries

M00→M01 deliberately changes turnout, unmapped support, poll/reference dates and
uncertainty. M00 uses the original shared logistic municipal/list field,
arbitrary correspondence weights and unmapped-to-non-valid projection. Its
national/local mean override mechanics do not become an M01 compatibility mode.
Do not require M00 distribution parity or infer predictive improvement.

Retain M01's latest eligible same-date common-18+ Camera/Senato pair, original
source compositions, binary future sets, latest-publication eligible Camera named
polls (including partial observations), list shocks and list/singleton-component
references. Preserve historical turnout while conditioning retained political
support; reference SDs and M02 multipliers remain inactive. Missing municipal
history uses labelled regional proportions, not zero.

Retain M02's original grouped likelihood and identifiable component basis,
selected history/paired adaptation, physical filed-set conditioning of the
all-elector simplex, calibrated reference uncertainty and registered forecast
methods. Removed/unfiled support is not M00 abstention. Preserve conditional
reference behavior; do not add raking/country calibration by default. The model
policy audit and restored-model tests are listed in [baselines](baselines.md).

Rosatellum+M01 does not yet execute. CP-03 must extract statistical output from
the current reform-assembled return and validate its own geography/electorate,
special ballot, candidate and legal assembly. Use retained M01 statistical
evidence plus fixed-input Rosatellum legal/candidature evidence; do not pretend
this inventory establishes an already working third pair or a version decision.
Rosatellum+M02 remains out of scope. Assess M01 version impact against model
specification separately from adapter/RNG/implementation changes.

## Runtime and file ownership

| Existing behavior/data | Merger boundary |
| --- | --- |
| Law select mounts separate workspaces/drafts; original `elezioni:web:politics-scenario:v1` and reform `elezioni:politics-reform-2026:fast-draft:v1` | CP-05 new common draft key; ignore old keys untouched. CP-06 switching preserves shared/inactive blocks and per-law choices |
| Old JSON/gzip scenarios, old reform workflow routing, schema-bearing result/checkpoint files | CP-05 rejects old politics formats clearly; no compatibility reader/converter. Keep regional routing and historical legal fixtures independent |
| Count/worker preferences outside scenario; model-specific local-storage worker key | Requested count/seeds/auto or fixed workers belong to scenario; effective workers/memory/timings belong to execution |
| `SimulationRequest.fast.{packets,file}`, accepted fit, revisions, source loading, progress, cancellation, result owner/ID/base path | Runtime/checkpoint/session only; no worker/storage handles in authored JSON. Retain delayed-message rejection and stage-dependent fit reuse |
| Immutable `resultScenario`, stale fingerprint; stored session selection independent of draft | CP-07 shared session list and immutable executed identity; viewing/importing a result does not replace draft |
| Tables, per-simulation rows, warning code/severity/chamber/reason, simulation membership, interpretation/law/draw context | Result envelope; separate info/warnings/errors. Preserve non-computable simulations and requested denominators, never silently reroll or erase them |
| Durable committed prefix, interruption before run 1, quota failure, OPFS/IndexedDB fallbacks, streaming JSON/gzip/CSV exports and checkpoint bytes | CP-07 reuse bounded services for all three pairs; completed/requested coverage and executed provenance roundtrip, restore exact fit without network; CSV remains table-oriented |
| Legacy fatal Camera flipper path | Fixed legal failure remains unchanged. Future common orchestration must isolate uncomputable runs without modifying legal allocation; do not claim current legacy 1,000-run success |

The [baseline guide](baselines.md) supplies executable cases and the limits of
each comparison. CP-02–08 remain implementation work.
