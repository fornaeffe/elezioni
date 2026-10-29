# Fast-share candidate preparation (FV-05)

This guide covers periodic Node preparation for the **2026 reform only**.
The candidate is packaged and checked separately from the installed defaults;
[FV-06](vote-generation-fast-fitting.md) supplies browser estimation infrastructure,
FV-07 owns editing/integration, FV-08 owns predictive acceptance and
activation, and the final migration gate owns public deployment.

| Task | Entry point |
| --- | --- |
| Reproduce or inspect the candidate | Commands below |
| Review default selection and assumptions | [Policy](../data/fast-share-default-policy.json), default section below |
| Change raw history or poll correspondence | [Source preparation](vote-generation-fast-data.md) |
| Read a candidate in a worker | [release.ts](../web/src/lib/politics/reform-2026/vote-share/release.ts) and transport section below |
| Change numerical or generation behavior | [Engine guide](../web/src/lib/politics/reform-2026/vote-share/README.md), [generation guide](vote-generation-fast-generation.md) |
| Review measured evidence | [Snapshot](../test/fixtures/benchmarks/vote_share_release.json), [history](migration-history.md#2026-10-29--fv-05-reproducible-prefitted-candidate-release) |

## Reproduce locally or in CI

Requires Node **24**, npm and the tracked source policies. From `web/`:

```powershell
npm ci
npm run data:reform:shares:release -- --download
```

This one command verifies/downloads pinned originals, prepares compact editable
history and unchanged seat/preference inputs, fits the explicit default and both
chamber profiles, verifies every candidate asset, runs `check:data` and `check`,
builds the static app and verifies its copied candidate assets. It selects
`candidate.json` only after success. It neither changes the application selector
nor publishes a site. On Linux, run the same commands with Node 24; Linux runner
timing has not yet been measured.

Omit `--download` for offline replay from verified original caches. Downloads
fetch missing pins only; changed upstream bytes fail. No R, Python, research
directory, research export, ignored debug fixture or manually prepared snapshot
is required. The standalone Node tsconfig allows preparation before the first
SvelteKit build. Other election workflows still require their own preparation;
this candidate command does not replace whole-application release acceptance.

```powershell
# Package/check without an app build, or check an already selected package:
npm run data:reform:shares:release:prepare
npm run data:reform:shares:release:check
# Build for a GitHub Pages project path:
npm run data:reform:shares:release -- --base /elezioni
# Reproduce HEAD plus the current diff in an isolated source-only checkout:
npm run acceptance:reform:shares:candidate
# Browser checks against the final static build (Chromium installed separately):
npm run test:reform:shares:release
```

The isolated acceptance command starts with empty original/numerical/output
caches, installs dependencies, downloads pinned sources, runs cold and warm
releases, compares every required byte and exercises missing/corrupted assets.
It retains logs under ignored `data/derived/fv05-acceptance/`. Required release
checks fail when any asset is absent, including advanced-refit chunks. The
separate browser development test skips clearly if no candidate was prepared;
that skip is not release acceptance.

Default directories, relative to the repository, are:

| Location | Contents |
| --- | --- |
| `data/sources/` | Verified original caches and tracked reviewed JSON |
| `data/derived/fast-share-cache/` | Content-addressed source/legal/fit/profile caches |
| `data/derived/fast-share-release/` | Complete immutable candidate plus selected pointer |
| `web/static/data/reform-2026/fast-share/` | Staged candidate assets only |
| `web/build/data/reform-2026/fast-share/` | Verified final static release copy |

Options `--sources`, `--cache`, `--output`, `--report` accept alternate paths.
Read-only checks accept `--catalog PATH/candidate.json`. Static release staging
uses the configured repository asset root; alternate output-only packaging uses
`release:prepare`. Old content-addressed files remain available; this step does
not prune them. Archive cleanup remains FV-09.

## Explicit candidate default

The policy fixes reference date **2026-10-19**, election date **2027-09-25**,
fit seed `fast-share-default-fit-v1` and simulation seed `fast-share-default-v1`.
It uses all eight Camera/European historical events, **35 selected polls** and
paired Camera/Senate 2022 common-18+ counts. Senate 2018 remains a 25+ audit asset.
The six excluded poll questions have explicit reasons: three conditional-ballot
exclusions and three with multiple distinct residual answers unsupported by the
single offered-complement observation policy. All 41 original questions remain
in the compact source for review and future admissible selections.

The installed filing/list choice set is retained: 22 Camera and 25 Senate lists,
covering 7,904 municipalities and 15,810 physical cells. Only eight Camera lists
have ordinary nationwide filing. The fitted basis therefore has **10 coordinates**:
nine main political components, including separate Azione/Italia Viva, plus an
explicit unfiled `historical-rest`. Current joint ballot groups remain indivisible;
other historical/poll identities pool whole into the residual. The approved
choice-set operation removes unfiled support before forecasting; it does not
assign that support to an invented current party.

The scenario contains **35 future components** in total. Its 25 restricted or
opaque special-ballot components are **reference-supported**, using the existing
new-component mechanism. Here `kind: new` means outside the fitted basis, not
that the political party was recently founded. These components have explicit
paired-2022 valid-share anchors in regions 02/04, conditional on current physical
filing and original regional valid denominators. Separate regional non-valid
anchors retain the original baseline. Their geographic log-profile is 0, profile
SD 0.45, local innovation SD 0.25 and national innovation SD 0.35, all declared
fallback assumptions. Their original historical counts remain in the pooled
likelihood and source bundle; there is no fabricated historical split or zero.
These fallbacks require predictive/sensitivity review at FV-08; this package
does not establish empirically fitted trajectories for those 25 components.

Municipal electorates are held at the matched Camera-2022 common-18+ counts for
both chambers: **46,120,143** electors each. This is a conditional scenario,
not a demographic forecast. Label matching and the complete poll inventory are
checked; an upstream default/source change requires explicit policy review.

## Immutable states, reuse and transport

The catalog binds source, default scenario, fitted state, both profiles, runtime
geography/electorates, legal seat/preference versions and generation implementation.
The portable schema stores metadata in canonical JSON and Float64/Uint8/Uint32
numeric columns in declared shapes and little-endian order. It retains all
accepted four-chain joint-context political and separate non-valid draws, without
quantization or subsampling. Deterministic gzip uses a zero timestamp; descriptors
pin compressed and decoded sizes and SHA-256 hashes. No clock or local path
enters an artifact identity.

Source/legal caches bind reviewed source parents and transitive implementation
content. Fit/profile caches additionally bind compiled semantic inputs, fit seed,
solver or paired inputs/reference date as applicable. Implementation keys include
local TS/JSON dependencies, the lockfile and standalone Node compiler configuration.
Metadata/provenance changes re-envelope compatible arrays without pretending the
old artifact has the new identity. A corrupt present cache fails; it is not a
cache miss. Immutable existing bytes cannot be overwritten with different content.

`loadCandidate(pointer, read, options)` checks state identities, semantic bindings,
numeric shapes and model versions before preparing generation. Supply the
expected generation implementation ID. Ordinary loading needs no source metadata
or history chunk and calls neither fitting function. `loadCandidateSource`
verifies original source metadata on demand. Browser estimation/revision ownership
is not implemented by these synchronous readers.

Disk readers return raw `Uint8Array` bytes and must pass exact compressed hashes.
Fetch readers return `{ transport: 'http', body: Uint8Array }`: servers may deliver
the gzip file unchanged or transparently decode `Content-Encoding: gzip`. The
reader verifies the matching compressed **or decoded** size/hash in that explicit
HTTP mode; corrupt bodies still fail. It never accepts uncompressed disk/cache
data as a fallback. Browser transfer reports distinguish server-reported encoded
`Content-Length` from the decoded response-body length.

The package's generation budget includes pointer, catalog, scenario, fit,
profiles and runtime chunks. Advanced-refit budget includes the source descriptor
and all ten original history chunks. Neither budget includes application code or
other simulators. The measured candidate is about **4.20 MiB** generation and
**4.30 MiB** additional refit data. Full browser refit latency, aggregate worker
memory, multi-browser support and production predictive/seat acceptance remain
FV-06/08 gates.
