# Production data preparation

Use this guide to reproduce all installed workspaces. For individual geography,
poll or model tasks, use the task map in [data/README.md](../data/README.md).
Requires **Node 24 and npm**, without R, Python, scenario workbooks, legacy
`dati/` caches or research exports. Preparation uses `tsconfig.node.json` and
does not require a pre-existing SvelteKit build or generated configuration.

## Complete workflow

From `web/`:

```powershell
npm ci
npm run data:prepare -- --download
npm run data:check
```

Preparation verifies/acquires pinned originals and generates geography/seat
artifacts, previous-law politics and Emilia-Romagna snapshots, R26-19
preference/assembly inputs, M01/M02 source/default/fit assets, then every
workspace default, descriptor and municipality/candidate catalog. It runs
production integrity and scrutiny checks before reporting success. It does not
build or publish a website.

After acquisition, omit `--download` for offline regeneration. Missing/corrupt
required sources fail; changed archive bytes require a reviewed manifest update.
The complete command requires the pinned fine Roma geography. The specialized
geography generator retains its documented missing-input estimation option.

Optional directories are relative to the working directory:

```powershell
npm run data:prepare -- --download --sources ../data/derived/clean-sources --cache ../data/derived/clean-fit --output ../data/derived/clean-production
npm run data:check -- --sources ../data/derived/clean-sources --output ../data/derived/clean-production
```

`--sources` relocates downloaded originals; reviewed repository sources are
copied with checksum verification. `--cache` relocates optional model caches;
a missing cache is supported. `--output` relocates immutable intermediates and
reports. Runtime data/defaults are installed in the current checkout; use a
separate checkout for an isolated complete preparation.

`prepare-report.json` and `check-report.json` are written under the selected
output directory (`data/derived/production/` by default). Receipt hashes identify
snapshots, generated defaults/catalogs and the selected reform candidate.
Timing/platform information is outside artifact identities.

## Authoritative inputs

Implementation entry points under `web/scripts/data-preparation/`:

| Task | Module |
| --- | --- |
| Complete command and blocking verification | `pipeline-cli.ts` |
| Original law-neutral DBF/census | `definitive-geography.ts` |
| Previous-law geography, populations and endowments | `workspace-geography.ts` |
| Historical import and dated municipal reconciliation | `workspace-history.ts` |
| Politics/ER snapshots and authored-reference compilation | `workspace-snapshots.ts` |

Existing default/catalog compilers are mapped in [web workspaces](web-workspaces.md).

| Input | Source |
| --- | --- |
| Definitive geography and legal census | `data/source-manifest.json` |
| Camera/European historical archives | Existing election manifest under `data/sources/polls/history/` and `data/reform-model-source-manifest.json` |
| Senate pairing and preference calibration | Existing R26-19 manifest/builders |
| Regional 2020 and dated municipal crosswalks | `data/workspace-source-manifest.json` |
| Shared authored politics choices | `data/fast-share-future-scenario.json` and its existing compiler |
| Earlier politics reference choices and ER defaults | `data/workspace-defaults.json` |

`workspace-defaults.json` contains authored lists, coalitions and correspondences
transcribed from the retired workbooks, with workbook hashes for provenance.
It contains no fitted coefficients or generated observations. The earlier
politics choices remain a reference scenario; the visible politics default
still comes from the shared authored compiler. Excel is not a Node pipeline input.
Original workbooks are available in the pre-retirement Git revision pinned in
[migration history](migration-history.md#2026-11-02--r-and-temporary-bridge-retirement).

ISTAT's historical API changes the ordering of rows with the same municipal
code between identical requests. Its crosswalks are therefore reviewed,
Git-tracked compressed JSON snapshots under `data/sources/geography/`.
Canonicalization sorts complete records and keys without losing fields or rows.
The manifest records the original URL/response hash, canonical JSON hash and
exact compressed-file hash/size. Preparation reads these pinned repository
snapshots offline, including with an empty alternate source directory. Updating
the observations requires a reviewed snapshot/manifest change; live API bytes
never replace them implicitly.

Only consumed originals are acquired by the complete pipeline. The older shared
manifest's unused section-population and municipal-change archives remain
available through its specialized acquisition command.

## Geography and history

Current runtime targets use **7,904 municipalities and the 2021 legal census**
with definitive 2022 electoral geography. Previous-law preparation includes all
**147 Camera and 74 Senate uninominal colleges**. Established constituency IDs
remain readable by saved scenarios; physical-unit IDs explicitly identify
definitive DBF records.

`definitive-geography.ts` reads original memberships. Only the reform builder
applies the Bolzano overlay. Previous-law seat endowments retain that workflow's
apportionment formula and seeded ties with revised populations; reform I08
capacities are not reused. Calculated capacities remain labelled provisional,
not official decree tables.

The 2020 spreadsheets predate the Montecopiolo/Sassofeltrio transfer. Those two
superseded memberships per chamber are audited, with the 2022 DBF authoritative.
Other spreadsheet/DBF discrepancies fail.

Dated official crosswalks aggregate earlier municipalities into the census
reference. Reverse reconstruction after a merger uses labelled census weights
and integer allocation, preserving each source choice total and electorate.
Election-specific dates prevent later mergers from contaminating earlier
elections. Trapani/Misiliscemi before the 2021 split uses the same explicit
estimation convention. Unknown/ambiguous joins fail; there is no fuzzy assignment.

Previous-law/ER views retain direct list counts and their established
`astensione` residual convention. Reform candidate-only attribution remains
separate. Inconsistent source denominators retain signed residuals and audit
entries, without inventing electors or deleting votes. Camera-2022 VdA uses its
separate official candidate/group and turnout files without inventing a
member-party decomposition.

Montecopiolo/Sassofeltrio have no ER-2020 observation because they were outside
that election's territory. Rows remain absent, rather than becoming zeros.
All current ER municipalities have later observations. Population/electorate
reconstruction conserves municipal totals exactly. Fine-geography continuity
and historical reconstruction remain estimates in scenario/result provenance.

## Checks and packaging

`data:check` regenerates previous-law/ER snapshots for byte comparison and checks
all small generated outputs, required reform assets, identities, checksums and
the production scrutiny slice. It never downloads or repairs inputs.
The [application acceptance](politics-reform-2026-acceptance.md) gate includes it,
then types, all unit tests, the full `/elezioni` browser matrix and focused root
URL/worker checks, with one sequential build per path. Missing required data
cannot pass through skipped tests.

Temporary debug bridges and their exporters have been removed from runtime
assets; builds use Vite directly. Archives/audits stay outside runtime assets.
Frozen compressed golden outputs under `test/fixtures/` support TypeScript regression in fresh
clones; they are checksum-pinned and never production preparation inputs or
browser fallbacks. The exact pre-migration Camera simulation-584 failure is
retained independently of revised datasets.

R scripts, legacy caches, Quarto workflows and Excel scenarios are retired.
To change a frozen regression, review and version the expectation; the active
checkout cannot regenerate it with R. Public deployment and hosted-runner/free-tier
acceptance remain the final migration gate.
