# Geography and census preparation

Use this workflow for R26-07 geography and seat tables. Historical vote/preference
importers and R26-19 statistical calibration are separate future consumers.
`dati/` remains the legacy R data directory; remove it only after the whole
data-preparation migration is complete and its consumers have migrated.

## Regenerate in another clone

Requires Node 24 and npm; R and Python are not runtime dependencies. From `web/`:

```powershell
npm ci
npm run data:download
npm run data:generate
```

- `source-manifest.json` pins original URLs, request headers, sizes and SHA-256.
- `sources/` caches downloaded originals; `derived/` holds normalized catalogs,
  extraction audits and complete seat-table artifacts. Both directories are ignored.
- The download command verifies existing files and fetches missing files only.
  It never silently accepts changed bytes. Total pinned downloads are about 145 MB;
  the government geography ZIP is about 98 MB despite its page's smaller size label.
- Generation is offline. It reads no R cache, scenario workbook or legacy `dati/`
  file. Output identities include content hashes, without timestamps or local paths.
- Repeating generation must preserve existing versioned files byte for byte.
  Different inputs/estimation assumptions produce a different version.

Optional locations are relative to the command's working directory:

```powershell
npm run data:download -- --sources ../data/derived/clean-sources
npm run data:generate -- --sources ../data/derived/clean-sources --output ../data/derived/check
npm run data:download -- --only camera-a2
npm run data:import -- camera-a2 C:/Downloads/a2-camera-pluri-unico.xlsx
```

Manual imports must match the same manifest. For an upstream revision, review the
new source, pin a new manifest/method version, regenerate and validate. Do not
overwrite an earlier accepted table or bypass a checksum failure.

## Inputs and geographic resolution

The manifest's reference is fixed to the **2021 legal population**, DPR 20 January
2023, GU 53 of 3 March 2023, SO 10. Recheck its electoral applicability before
generating a new release; annual population estimates do not replace it automatically.
[Istat distinguishes the electoral legal population from annual census releases](https://www.istat.it/notizia/popolazione-censuaria/).

| Task | Source / implementation |
| --- | --- |
| Acquire/cache/import pinned sources | [`sources.ts`](../web/scripts/data-preparation/sources.ts) |
| Read CSV/TSV, XLSX, ZIP members and DBF attributes | [`formats.ts`](../web/scripts/data-preparation/formats.ts) |
| Reconcile the real catalog and audit the original tables | [`normalize.ts`](../web/scripts/data-preparation/normalize.ts) |
| Calculate/validate seat tables and adapt legal inputs | [`endowments.ts`](../web/src/lib/politics/reform-2026/endowments.ts) |
| Reconstruct incomplete population parts | [`population.ts`](../web/src/lib/politics/reform-2026/population.ts) |

The [government's definitive geography downloads](https://www.riformeistituzionali.gov.it/it/i-nuovi-collegi-elettorali/)
include presentation-oriented A1/A2/B1/B2 spreadsheets and a geography archive
updated to 2022. The latter's `UT_Collegi2020.dbf` is the primary machine-readable
membership source. Only attributes are needed, not GIS polygon operations. Its
bytes require Windows-1252 despite its UTF-8 `.cpg` declaration.

The XLSX parser explicitly carries constituency/college continuations, ignores
capoluogo summary rows and resets at section headers. It extracts 147/49 Camera
and 74/26 Senate colleges; A2/B2 college aggregations are checked against the DBF.
The 2022 archive already accounts for Misiliscemi, Montecopiolo and Sassofeltrio.
Do not restore the proposal CSV's manual Lazio patches or its obsolete membership.

Bolzano U04/U05/U06 is overlaid from the reform's statutory annexes. Annex 4's
semicolon lists and annex 5's bilingual tables must independently resolve to the
same 116 municipal codes. The post-census Montagna rename has an explicit alias.

The ordinary Camera geography divides Roma among three plurinominal colleges.
Istat's [2021 urban-zone population data](https://www.istat.it/notizia/basi-territoriali-e-variabili-censuarie/)
cover all 155 zones and sum to the legal municipal population, 2,749,031. The
code-based zone crosswalk assumes boundary continuity with the 2022 electoral
units; it has not been spatially verified. The artifact carries an Italian warning.

If `submunicipal-geography-2021.zip` is unavailable, generation remains usable:
available observations take precedence, missing residue uses historical weights,
and zero information uses equal weights. Inconsistent fine totals are rescaled
to the legal municipal total. Integer reconstruction preserves that total exactly;
the method, input weights, estimates and seeded ties are recorded. A different
source-availability state produces different catalog/table versions. Corrupt
downloaded files still fail checksum validation rather than becoming estimates.

The manifest also caches municipal-change records and 2021 section-population
files for subsequent preparation work. They are not consumed by this generator
and are not claimed among its used source hashes.

## Publish and consume

`npm run data:generate -- --publish` additionally writes an immutable, tracked
artifact under `web/src/lib/politics/reform-2026/data/`. Register a new accepted
version in [`bundled-data.ts`](../web/src/lib/politics/reform-2026/bundled-data.ts).
Normal generation does not publish or change defaults.

`getBundledReformSeatTables()` works without raw archives/network access and
returns an owned copy. `chamberGeography()` adapts its validated tables to the
R26-05 input boundary; caller-provided votes/candidates remain separate.
`seatTableMessages()` provides Italian informational/provisional and population
warnings. Population warnings also survive legal-input/result serialization in
the additive optional `SeatTableSource.populationNotes` field. R26-20 must render
these messages when the reform application path is integrated.

The compact artifact contains the census/geography/source versions, populations,
both capacity branches, exact apportionment traces and seed. Preparation draws
use simulation ID 0; later composition must expose their I03 warning/trace for
each affected run. Geography identities are `(geography version, chamber, ID)`.

No applicable reform allocation decree was verified for this release. The
published tables therefore use **provisional I08-v1**, never an official-decree
label. A verified decree can replace the capacities through `SeatTableBundle`
and `validateSeatTableBundle()` using `method: 'official-decree'`, the exact
reference, a new version, `seed: null` and empty I08 traces/draws/warnings. Validate
the complete catalog, special structures, minima and totals; retain old artifacts
for replay. Do not silently force a conflicting decree into the Camera 314 pool.

## Checks

From `web/`, run `npm run check:data`, `npm run check`, and `npm run test`.
Preparation tests cover cached/manual/HTTP acquisition, immutable output, source
formats, full municipal coverage and explicit missing-data estimation. Real-source
tests skip clearly when archives are absent; reform law/table tests always run
using the tracked artifact. Build and browser checks remain sequential:
`npm run build`, then `npm run test:e2e`.
