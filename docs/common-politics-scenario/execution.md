# CP-03 — Common politics execution

Use `web/src/lib/politics/execution/index.ts` for the new scenario's execution API.
It supports exactly Rosatellum/M01, reform/M01 and reform/M02. Read the
[domain contract](contract.md) for authored fields and the [baselines](baselines.md)
for independent statistical, candidature and fixed-input legal evidence.

| Task | Entry point under `web/src/lib/` |
| --- | --- |
| Prepare, generate votes, assemble legal input, execute scrutiny | `politics/execution/prepare.ts` |
| Read exact decoded asset bytes and reject incompatible inputs | `politics/execution/assets.ts` |
| Normalize reference units, source encoding, memberships and model inputs | `politics/execution/inputs.ts` |
| M01 original-source compilation, polls, municipal shocks, college reconstruction | `politics/generation/m01-compiler.ts`, `m01-polls.ts`, `m01.ts`, `m01-counts.ts` |
| M02 physical vote field, references and integer counts | `politics/generation/m02.ts` |
| Law-specific filing and candidature assembly | `politics/execution/rosatellum.ts`, `reform.ts` |
| Validated common compute packet and per-run result tables | `workers/politics-compute.ts` |
| Actual compute worker, bounded pull transport and ordered pool | `workers/reform-compute.worker.ts`, `reform-compute.ts`, `reform-pool.ts` |

## API and ownership

`preparePoliticsExecution(value, readers, options)` parses and detaches the common
scenario, loads only its active dependencies, validates the complete document and
active semantics, then prepares the selected statistical model and law adapter.
`options` supports cancellation, cooperative progress and an optional numerical
workspace limit. Missing assets, unsupported versions/pairs and invalid active
inputs fail explicitly. The original-column chunk reader retains the existing
compressed/decoded integrity checks.

`readers.implementationId` identifies the actual transitive execution code hash.
Browser callers use the exported `commonPoliticsImplementation`, supplied by the
Vite build; Node callers compute the same hash with `implementationKey` on
`web/src/lib/workers/politics-compute.ts`. It is separate from model versions and
stage keys and must also participate in any prepared-state/checkpoint reuse.

The prepared object exposes:

- `generateVotes(id)`: physical college elector/list counts and model trace;
- `generate(id)`: the same votes plus a discriminated `PoliticsSimulationRequest`;
- `run(id)`: generation/assembly followed by the existing scrutiny registry;
- `keys`: separate preparation, generation and legal-assembly identities.

No M00 generator exists at this boundary. M01 does not construct a reform legal
scenario to run Rosatellum. The shared statistical implementations consume only
vote settings, original observations, chamber parameters and geographic context.
The previous reform entry points are thin compatibility facades over those same
implementations, preserving the CP-01 comparison recipe and its recorded hashes.
They are not converters for old authored files.

Preparation/generation keys exclude subject names/colors, inactive-law settings,
inactive-model settings and candidate controls. M01 does not consume reference
SDs or the fitting seed. Its explicitly saved `M01.parameterDate` supplies the
existing projection-date input; it remains distinct from the evidence cutoff.
M02 uses its existing source compiler, fitting steppers, geographic profiles and
common-18+ Senate adapter. It preserves the selected forecast policy and reference
SDs, including explicit zero. Candidate changes affect legal assembly without
changing the statistical preparation identity.

## Decoded asset contracts

CP-04 will author/package these inputs; CP-03 does not replace production defaults.
Every top-level asset is decoded UTF-8 JSON, verified against its exact scenario
pin before parsing. Production retrieval/decompression belongs to `readers.asset`.
Original-column chunks use `readers.sourceChunk`.

| Scenario pin | Decoded value |
| --- | --- |
| `data.historical` | Existing `CompactSource`; its content identity must equal the pin's version |
| `data.catalogue` | `ExecutionCatalogue`: component/source/question/choice/poll identities, namespaced law locations and explicit parent-child relationships, including inactive declarations |
| `data.referenceGeography` | Region/province/municipality scope catalogue |
| `data.electorate` | `ExecutionElectorate` v1: `kind: politics-electorate`, explicit `minimumAge: 18`, dated conditional municipal `targets` with provenance |
| Active law's `geography` | `SourceGeography` with physical cells only, matching target population totals and the exact pinned geography version |
| Active law's `seats` | Existing reform `SeatTableBundle`, or `RosatellumSeats` v1 |
| Active law/model's sole model pin | `GenerationParameters` v1: exact model/version, existing chamber/preference parameter bundle and explicit Rosatellum chamber-profile ID |

Source memberships and M01 compositions retain composite domain identities.
The original-column representation has globally unique choice IDs: event choices
use the pinned `SourceChoice.id`; poll choices use `pollChoiceId(pollId, label)`.
The adapter checks the owning source and rejects ambiguous flattening. Poll
membership edits produce a runtime view of the original poll, retaining its
observed shares and metadata; they do not overwrite the source asset. Pooling
remains whole-component mapping, separate from M01's fractional compositions.

## Law adapters

Rosatellum uses an explicit physical-ballot crosswalk to chamber, constituency,
uninominal code, proportional code and region. Validate this against the complete
legal grid and 392/196 domestic seat totals. Preserve the previous-law geography;
the reform-only Bolzano overlay is not a Rosatellum input. Equal codes in different
chambers/laws do not establish a relationship.

Ordinary list filings use actual plurinominal colleges. VdA in both chambers and
Senate TAA have no proportional college: use their actual `uninominal` filing
locations and retain null proportional codes plus the zero-seat legal context
rows. The adapter rejects uninominal filing elsewhere. This completes CP-02's
filing vocabulary without fabricating a proportional college.

Rosatellum reuses `generatePoliticsCandidates`, including its uni-to-pluri reuse,
five-bin candidature-count distribution and retained sampling edges. Shares come
from prepared M01 central references separately for each chamber. Electoral
singleton tickets are internal, distinct from political coalition declarations.
Pins retain declared IDs/biography; internal pin tokens and a collision-free
generated namespace prevent accidental identity merging. Active pins are checked
for slot, affiliation, availability, age and incompatible uses. The relevant
[Rosatellum candidature text](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticoloDefault/originario?atto.codiceRedazionale=17G00175&atto.dataPubblicazioneGazzetta=2017-11-11&atto.tipoProvvedimento=LEGGE)
is identified next to the adapter checks. The generator's existing policy and
the fixed-input scrutiny implementation are preserved.

Reform reuses the existing reference-based bonus plan, candidature/preferences
generation, supplied-input contracts and final scrutiny-input validator. TAA's
proportional totals remain a derived view of its physical ballots. Assembly uses
`materializeBonusLinks`, so automatic and eligible explicit links are emitted once;
retained competing declarations are excluded, empty counterparts fail separate
participation validation and single renewal emits no joint links.

## Model-version assessment and comparison

M01 remains **2.0.0** for both laws. The extracted arithmetic retains its specified
conditional electorate, latest eligible history, compositions, turnout-preserving
filing, fallback, poll, reference, list-shock and reconstruction contracts.
Rosatellum supplies a different legal/geographic context to those existing inputs;
it adds no statistical capability or probability-law parameter. This is an
implementation/adapter change, rather than a MINOR model capability or MAJOR law
change. The new adapter has its own `rosatellum-m01-v1` identity. Scrutiny remains
at the existing algorithm IDs and explicit metadata versions; implementation
hashes and preparation identities change independently.

M00's turnout, unmapped-support, poll and uncertainty behavior is intentionally
replaced by M01 under D01. Rosatellum draws and generated IDs therefore have no
M00 parity requirement. The new implementation is deterministic by simulation ID
and independent of worker assignment. Retained reform facades still reproduce
CP-01's exact record; common-format tests compare ballots, candidature and legal
results separately from changed source/parameter provenance. The restricted M01
capacity blocker and M02 tiny-electorate `initial-overallocation` remain visible;
there is no reroll, scaling repair or regenerated legal expectation.

## Worker and later-step boundary

`PoliticsComputePacket` carries the immutable scenario, exact asset/chunk bytes
and all three expected stage keys plus the actual implementation ID. The worker
checks that ID against its own compiled build. `init-politics` repeats parsing, checksums,
supported-pair, model preparation and legal validation inside the actual compute
worker; mismatching keys fail. It uses the existing one-run-at-a-time, pull-driven
result encoding and ordered pool. Legal/generation failures retain their original
simulation IDs and error diagnostics. This path introduces no 1,000-run cap.

CP-04 still owns authored defaults and production packaging. CP-05 owns new
scenario/result readers and draft isolation. CP-06/07 will connect the editor and
common controller/session lifecycle, including prepared-state reuse across compute
workers; this initial packet path repeats model preparation per initialized worker.
The current old-format workspace UI stays transitional until those steps. Its M00
path and plot ceiling are not capabilities of the new common executor. CP-08 owns
the three full 1,000-run/performance campaigns and final merger acceptance.

Verification commands and measured outcomes belong in
[migration history](../migration-history.md#cp-03--adapt-existing-execution-paths-2026-11-07).
