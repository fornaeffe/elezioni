# Fast-share source preparation (FV-03)

This is the active original-data pipeline for the 2026 reform model. It produces
immutable refit records; it does not select or install a production scenario,
component basis, Senate adapter or default fit. Read the
[scenario/data contract](vote-generation-fast-contracts.md) for those boundaries.

| Task | Read/run |
| --- | --- |
| Acquire pinned originals | `data:reform:shares:download`; [manifest composition](../data/fast-share-source-manifest.json) |
| Normalize history/geography | [fast-share-history.ts](../web/scripts/data-preparation/fast-share-history.ts), existing reviewed old-college attribution and census preparation |
| Correspondences and citations | [source policy](../data/sources/fast-share/memberships-policy.json), [citations](../data/sources/fast-share/membership-sources.json) |
| Restore/normalize polls | [fast-share-polls.ts](../web/scripts/data-preparation/fast-share-polls.ts), [reviewed restorations](../data/sources/fast-share/poll-refinements.json) |
| Encode/read chunks | [portable types](../web/src/lib/politics/reform-2026/vote-share/source-types.ts), [codec](../web/src/lib/politics/reform-2026/vote-share/source-codec.ts) |
| Change selected history/memberships/date | [compiler](../web/src/lib/politics/reform-2026/vote-share/source-compiler.ts) |
| Numerical estimation | [FV-02 engine guide](../web/src/lib/politics/reform-2026/vote-share/README.md) |
| Prefit/package an explicit default without activating it | [FV-05 candidate release](vote-generation-fast-release.md) |
| Recorded acceptance/resources | [Original FV-03 benchmark](../test/fixtures/benchmarks/vote_share_sources.json), [availability-policy update](../test/fixtures/benchmarks/vote_share_availability.json), [history](migration-history.md#2026-10-29--fv-03-compact-editable-production-history) |

## Commands and ownership

Requires Node 24/npm, without R, Python, WSL, research exports or previous derived
artifacts. From `web/`:

```powershell
npm ci
npm run data:reform:shares:download
npm run data:reform:shares:prepare
npm run data:reform:shares:check -- --asset ../data/derived/fast-share/SOURCE_ID.asset.json --report ../data/derived/fv03-acceptance/node-check.json
```

Replace `SOURCE_ID` with the full content ID printed by preparation. There is no
mutable latest pointer. `download -- --sources DIR` uses an alternate empty cache;
`prepare -- --sources DIR --output DIR` reproduces offline from that verified
cache. Existing wrong source bytes fail instead of being silently replaced.
Preparation checks all original archives and all encoded row/count round trips.
`check` requires every chunk and full target coverage; missing/corrupt data fails.
Outputs default to ignored `data/derived/fast-share/`. No command in this step
publishes to `static/`, changes generated defaults or enables deployment.

Optional national prefitting is explicit:

```powershell
npm run data:reform:shares:prefit -- --asset SOURCE_ID.asset.json --request selection.json --seed my-fit-seed --output output-directory
```

`selection.json` implements `SourceSelection`: reference date, election/poll IDs,
component basis, optional whole-component pooling and complete edited membership
rows with provenance, conditional target codes/electorates, and target context.
It must provide every required selected source correspondence and distinguish
the requested fitted basis. No default is inferred. The command writes accepted
joint C/E political and non-valid chains using FV-02; it does not fit Senate or
install physical-cell profiles. FV-04/05 own production adaptation and prefitting.

## Source content and explicit limits

- Retain Camera 2008/2013/2018/2022, European 2009/2014/2019/2024 and Senate
  2018/2022. Senate 2018 carries `minimumAge: 25`, `role: age-audit`; 2022 is the
  paired 18+ baseline. Neither is accepted by the C/E numerical compiler.
- Every source municipal choice remains present, including VdA, TAA, unmatched
  places, unlocated fuori-sede, observed zeros and invalid denominators. Original
  direct list counts accompany the reviewed integer attribution of candidate-only
  ballots. Opaque special-group counts are never decomposed into party estimates.
- Known availability dates and citations in the source manifest's
  `resultAvailability.knownDates` take precedence. Otherwise assume availability
  **by the end of the first civil day after the final polling day**. The manifest
  records the second polling day for Camera 2008/2013 explicitly. This is a
  labelled retrospective fallback, not a verified publication timestamp or a claim
  that the complete archived snapshot was available at the start of that day.
  Event and availability dates must both precede/equal the inclusive daily cutoff.
  Poll publication/end/representative days
  must also qualify. Moving the reference date always changes the numerical keys.
  Incomplete live returns are a separate post-migration current-election
  final-count forecasting task; they are not supported as complete historical
  observations for forecasting subsequent elections.
- Same-place matches use the reviewed source-name resolver and target codes;
  mergers and ambiguous identities remain labelled unmatched. Target records
  retain all 7,904 municipalities and 15,810 physical membership cells across
  86 colleges. Population sums agree per municipality/chamber. Camera TAA's
  overlapping proportional view is excluded from physical cells and derived later.
- All 41 poll questions retain original and normalized answers, sample bases,
  effective-sample/design-effect policy, shared-survey/error groups, rounding,
  citations, dates and exclusion reasons. The three conditional questions remain
  excluded. Reviewed early FDI/FI repairs replace the coarse view of the same
  question. No PDF is a required preparation input. Named contemporary identities
  without established histories use distinct `poll-only-*` atoms; no geographic
  proxy or ancestral split is imported from the installed generator.
- Promoted correspondences retain their provisional review status and citation
  dictionary in the source envelope. They are complete source proposals, not an
  automatically admissible default basis. The compiler rejects unresolved fitted
  contrasts, competing raw memberships, unsupported opaque splits, duplicate
  surveys, unavailable sources and incomplete component mappings.

The compiler retains unmatched resident observations nationally and for labelled
historical profile information. Invalid-denominator observations remain in the
source/national totals, with explicit local exclusions. Unlocated fuori-sede
observations remain in the source but cannot supply resident-municipal denominators.
Profiles use the common observable national partition by summing whole groups;
no joint count is allocated. Missing target history stays absent and uses FV-02's
labelled fallback. Poll Other is formed on the original dated basis **before**
pooling named/unnamed support. Election turnout is never inferred from nonresponse.

## Chunk layout and identity

The `fast-share-source` envelope and each event use separately compressed,
content-addressed assets, with exact compressed/decoded sizes and SHA-256 pins.
The small `.asset.json` descriptor identifies the envelope; the envelope contains
event descriptors, dictionaries, full source memberships and physical geography.
No PDF, workbook remainder, geometry shape or held-out future truth is shipped.

Each event is one bounded little-endian uint32 chunk, described by explicit
column offsets/lengths: unit, electors, voters, flags, choice offsets, choice
indices, attributed counts and original direct counts. All integer conversions
are range-checked. A present choice with count zero is an observed offered zero;
an absent municipality row is missing history. Flags distinguish invalid
denominators, unmatched geography, missing voters and unlocated records. The
missing-voter flag makes its numeric placeholder non-observational. Decoding
checks hashes, sizes, shape, offsets, indices, masks and national conservation.

Chunks can be read/released sequentially. Explicit selection produces separate
national/profile input keys using canonical finite JSON and SHA-256 domain
separators. Presentation/citation-only corrections change provenance/envelope
identities but preserve numerical keys. Edited memberships/date or numeric
source content change the appropriate numerical keys. Bytes are deterministic
across cold/warm preparation; LF attributes pin the promoted JSON on Windows/Linux.

## Acceptance and remaining integration

Run the source tests and reduced worker test without real data:

```powershell
npx vitest run src/lib/politics/reform-2026/vote-share/source.test.ts
npx playwright test tests/e2e/fast-share-source.spec.ts --workers=1
```

For the additional required full-source streaming test, set
`FV03_SOURCE_ASSET` to the generated `.asset.json` path and run the browser command
at both `BASE_PATH=''` and `BASE_PATH='/elezioni'`. A specified missing asset fails;
there is no debug fallback. Reports record one first-worker run and five warm
runs, Node/browser key equality and full count validation. Local intercepted
asset delivery measures decode/validation computability, not internet throughput.

The measured bundle is about 4.30 MiB compressed / 18.91 MiB decoded. The largest
live event numeric allocation is about 4.29 MiB, excluding metadata, hash copies
and JS row objects. The separate Node decode report measures process RSS and
inflate/column/row-boundary heap/buffer use; it is not a browser peak-process
measurement. Browser aggregate memory, maximum-envelope refit latency, full
default admissibility, Senate coupling and predictive acceptance remain later
gates. This event-chunk format is the FV-03 measured baseline; FV-05/08 must
remeasure combined decoded data, retained fits/profiles and simultaneous workers.

Active dependencies are the `fast-share-*` preparation modules, this codec and
compiler, `data/sources/fast-share/`, the reviewed poll JSON, pinned source
manifests, and existing geography/attribution readers. Research originals stay
unchanged until the audited FV-09 archive/cleanup step.
