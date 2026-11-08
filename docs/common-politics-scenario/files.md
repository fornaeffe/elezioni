# Common politics files and drafts (CP-05)

Use this guide for file validation, routing, checkpoint binding and draft storage.
For authored fields use [the domain contract](contract.md); execution and model
preparation use [the execution guide](execution.md). Editor/session wiring belongs
to CP-06/07.

| Task | Entry point under `web/src/lib/` |
| --- | --- |
| Scenario JSON / deterministic gzip | `politics/scenario/serialization.ts` |
| Strict streamed scenario file reader and metadata inspection | `politics/files/metadata.ts` |
| Result/archive schemas and readonly types | `politics/scenario/envelopes.ts` |
| Result/archive readers, writers and staging sink | Reexports from `core/result-export.ts`; implementation in `politics/files/results.ts` |
| Exact checkpoint binding and portable numeric-state validation | `politics/files/checkpoint.ts` |
| Shared draft key and validated storage reads/writes | `politics/files/draft.ts` |
| External election routing and transitional editor guard | `workspaces/import-routing.ts` |
| Contract examples and rejection/roundtrip checks | `politics/files/files.test.ts` |

## Formats and identity

Scenario files retain `politics-scenario` / schema 1. Result documents use
`politics-result` / schema 1; portable archives use `politics-result-archive` /
schema 1. These versions are independent of model versions and numeric-state
payload versions. No import fills missing settings or changes an unsupported
model/version/pair.

A result contains the complete detached `executedScenario`, `execution`,
`session` and columnar worker `result`. Inactive law/model settings and both
saved generator selections remain in the executed scenario. Provenance binds
the active generator, scrutiny and adapter/version, exact active assets, numerical
and RNG policies, implementation SHA-256, both seeds, requested/actual workers,
stage keys, start time and elapsed time. The worker benchmark must agree.
Adapter metadata version 1.0.0 labels the CP-03 adapters without changing them.

The session has a stable ID, complete/interrupted state and explicit simulation
IDs with complete/partial/failed outcomes. IDs are unique and within the requested
denominator. `completedSimulations` counts processed simulations, including failed
attempts; it equals the session evidence length. Complete sessions account for
every requested simulation; interrupted sessions may contain zero attempts.
Diagnostics retain severity, scope, occurrence count, chamber/context/reason and
simulation membership. Simulation-specific diagnostics must reference an attempted
ID. Generic table cells preserve zero, empty strings, booleans and null separately.
Rows must match unique, nonempty column descriptors.

An archive adds `exportedAt` and `portableFit`. M02 requires its frozen numeric
checkpoint; M01 uses null. A standalone result can be viewed without reusable fit
state and is not a substitute for a portable M02 archive.

## Streaming and publication

`inspectPoliticsFile` reads routing identity and selection while skipping other
values incrementally. Writers place routing headers before result details. The
inspector cancels the stream as soon as the required identity is present; it can
inspect an incomplete archive header and is **not** a file-validity check.
Full readers reject duplicate object keys, unknown fields, invalid/trailing JSON,
unsupported versions, inconsistent identities, coverage and malformed rows.
JSON and JSON.gz use the same readers after streaming decompression.

`streamPoliticsArchive` emits a header, then one row/diagnostic at a time. Large
stored results supply descriptors with empty resident rows and a
`PoliticsArchiveRows` source; pipe encoded chunks through `CompressionStream`
for gzip transport. This does not materialize a complete archive or reread stored
tables into memory. CSV remains table-oriented through the existing generic CSV
helpers.

`importPoliticsArchiveStream` / `importPoliticsArchiveFile` require a
`PoliticsArchiveSink` with **separate staging storage**. Row and warning callbacks
must never publish current results or edit the draft. Column descriptors can
arrive after rows because JSON field order is unrestricted. `commit(header)` is
called only after EOF, complete envelope/checkpoint validation and diagnostic
membership checks; it must publish atomically. `abort()` discards staging on any
failure, including a failed commit. The returned/committed header contains table
descriptors with empty row arrays and empty diagnostics; those payloads belong to
the sink. Session evidence remains in the header. CP-07 supplies durable session
ownership and the concrete storage sink.

## Checkpoints and drafts

`politics-checkpoint` schema 1 is a new common identity wrapper around the
unchanged `state-codec` v1 `fast-share-fit` and `fast-share-profiles` payloads.
It includes a canonical SHA-256 of the **entire** common scenario, exact active
dependencies, generator/version, fitting seed, numerical version, preparation key
and execution implementation hash. `implementationSha256` stores the raw checksum
part of the executor's `implementation-v1-<sha256>` identity. Numeric-state metadata
must include that checksum alongside its own fitting/profile implementation parents.
Even an inactive or presentation-only edit
invalidates exact file-checkpoint reuse. It cannot restore a draft.

Creation and validation verify both states, descriptor/content hashes, byte
counts, finite typed arrays, declared dimensions, accepted fit diagnostics and
sampler policy, dates, metadata parents and profile bases. Portable assets are
limited to 64 MiB, carried in base64 chunks of at most 64 KiB so no fitted array
becomes an oversized JSON token; missing, duplicate, unused or corrupt assets fail. Old
scenario/catalog/runtime states and former `politics-reform-session` files are
not accepted. Execution must additionally check the restored component and
municipality basis against its loaded exact source/geography assets before use.

`readPoliticsDraft` and `writePoliticsDraft` access only
`elezioni:politics:common-draft:v1`. Reads return a detached valid scenario or
throw; writes finish validation/serialization before touching storage. They never
inspect, combine, remove or overwrite former politics keys. The common editor
will own when to restore/save this draft; file APIs do not select result sessions
or implicitly replace a draft.

## Transitional browser boundary

The CP-04 projected editors remain until CP-06/07. They no longer read or write
old politics draft keys or restore scenario-bearing checkpoints. External file
entry points reject all old politics schemas before routing or changing current
draft/results. Valid common files can be read through the APIs above; the
transitional editor guard reports that loading requires the new editor instead
of feeding them to an incompatible editor or destroying the current screen.
Common draft persistence and browser import/export/session integration start in
CP-06/07; transitional edits are currently memory-only. Existing old internal
projection/export helpers remain for those editors and isolated regression
evidence until CP-08 retirement; they are not the common file API.

Independent Emilia-Romagna file routing, formats and draft persistence remain
unchanged. Frozen historical regression fixtures and their checksum manifests
remain test evidence, never runtime import formats.
